%%%---------------------------------------------------------------------------
%%% @doc
%%%   Log handler for {@link error_logger} writing flat files with term
%%%   formatting. This handler writes all events (without regard to whether it
%%%   was `*_msg' or `*_report') to a file formatted as Erlang term.
%%% @end
%%%---------------------------------------------------------------------------

-module(haircut_file_term_h).

-behaviour(gen_event).

%%% gen_event callbacks
-export([init/1, terminate/2]).
-export([handle_event/2, handle_call/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------
%%% types

-record(state, {file, fh}).

%%%---------------------------------------------------------------------------
%%% gen_event callbacks

%%----------------------------------------------------------
%% initialization and cleanup {{{

%% @private
%% @doc Initialize {@link gen_event} state.

init(File) ->
  FileABS = filename:absname(File),
  case file:open(FileABS, [append, raw]) of
    {ok, FH} ->
      {ok, #state{file = FileABS, fh = FH}};
    {error, Reason} ->
      {error, Reason}
  end.

%% @private
%% @doc Clean up {@link gen_event} state.

terminate(_Reason, _State = #state{fh = FH}) ->
  file:close(FH).

%% }}}
%%----------------------------------------------------------
%% communication {{{

%% @private
%% @doc Handle {@link gen_event:notify/2}.

handle_event(Event, State = #state{fh = FH}) ->
  {MS,S,US} = now(),
  Time = io_lib:format("~B.~6..0B", [MS * 1000000 + S,US]),
  EventLine = format_event(Event),
  file:write(FH, [Time, "\t", EventLine, "\n"]),
  {ok, State}.

format_event({Level, Data} = _Event) when is_atom(Level) ->
  % 2G columns should be \infty, term should fit in this width
  [atom_to_list(Level), "\t", io_lib:print(Data, 1, 16#ffffffff, -1)];
format_event(Event) ->
  format_event({unknown, Event}).

%% @private
%% @doc Handle {@link gen_event:call/3}.

handle_call(reopen = _Request, State = #state{file = File, fh = FH}) ->
  file:close(FH), % remember to close the old handle before possibly dying
  {ok, NewFH} = file:open(File, [append, raw]), % die on any problem
  {ok, ok, State#state{fh = NewFH}};

handle_call(_Request, State) ->
  {ok, {error, unknown}, State}.

%% @private
%% @doc Handle incoming messages.

handle_info(_Msg, State) ->
  {ok, State}.

%% }}}
%%----------------------------------------------------------
%% code change {{{

%% @private
%% @doc Handle code change.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
