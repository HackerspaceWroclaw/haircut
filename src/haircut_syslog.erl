%%%---------------------------------------------------------------------------
%%% @doc
%%%   Syslog connector.
%%%
%%% @TODO Move this to Indira.
%%% @end
%%%---------------------------------------------------------------------------

-module(haircut_syslog).

-behaviour(gen_server).

%%% public API
-export([start_link/0]).

%%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------

-record(state, {
  path = "/dev/log",
  handle
}).

%%%---------------------------------------------------------------------------
%%% public API {{{

%% @doc Start syslog connector process.

-spec start_link() ->
  {ok, pid()} | {error, term()}.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% }}}
%%%---------------------------------------------------------------------------
%%% gen_server callbacks

%%----------------------------------------------------------
%% initialization and cleanup {{{

%% @private
%% @doc Initialize {@link gen_server} state.

init(_Args) ->
  {ok, #state{}}.

%% @private
%% @doc Clean up {@link gen_server} state.

terminate(_Reason, _State = #state{handle = undefined}) ->
  ok;
terminate(_Reason, _State = #state{handle = Syslog}) ->
  indira_syslog:close(Syslog).

%% }}}
%%----------------------------------------------------------
%% communication {{{

%% @private
%% @doc Handle {@link gen_server:call/2}.

handle_call(_Request, _From, State) ->
  {reply, {error, unknown}, State}.

%% @private
%% @doc Handle {@link gen_server:cast/2}.

handle_cast(_Request, State) ->
  {noreply, State}.

%% @private
%% @doc Handle incoming messages.

handle_info({syslog, _Ident, _Facility, _Priority, _Data} = Msg,
            State = #state{path = Path, handle = undefined}) ->
  case indira_syslog:open_local(Path) of
    {ok, Syslog} ->
      handle_info(Msg, State#state{handle = Syslog});
    {error, _Reason} ->
      {noreply, State}
  end;

handle_info({syslog, Ident, Facility, Priority, Data} = _Msg,
            State = #state{handle = Syslog}) ->
  % 2G columns should be \infty, term should fit in this width
  DataLine = io_lib:print(Data, 1, 16#ffffffff, -1),
  Line = indira_syslog:format(Facility, Priority, Ident, DataLine),
  case indira_syslog:send(Syslog, Line) of
    ok ->
      {noreply, State};
    {error, _Reason} ->
      indira_syslog:close(Syslog),
      NewSyslog = try_reconnect_and_send(State#state.path, Line),
      {noreply, State#state{handle = NewSyslog}}
  end;

handle_info(_Msg, State) ->
  {noreply, State}.

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
%%% helper functions

try_reconnect_and_send(Path, Line) ->
  case indira_syslog:open_local(Path) of
    {ok, Syslog} ->
      indira_syslog:send(Syslog, Line), % this may fail, but we won't care
      Syslog;
    {error, _Reason} ->
      undefined
  end.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
