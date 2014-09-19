%%%---------------------------------------------------------------------------
%%% @doc
%%%   Log handler for {@link error_logger} writing to syslog with term
%%%   formatting. This handler writes all events (without regard to whether it
%%%   was `*_msg' or `*_report') to a file formatted as Erlang term.
%%% @end
%%%---------------------------------------------------------------------------

-module(haircut_syslog_term_h).

-behaviour(gen_event).

%%% gen_event callbacks
-export([init/1, terminate/2]).
-export([handle_event/2, handle_call/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------
%%% types

-record(state, {}).

%%%---------------------------------------------------------------------------
%%% gen_event callbacks

%%----------------------------------------------------------
%% initialization and cleanup {{{

%% @private
%% @doc Initialize {@link gen_event} state.

init(_Args) ->
  {ok, #state{}}.

%% @private
%% @doc Clean up {@link gen_event} state.

terminate(_Reason, _State) ->
  ok.

%% }}}
%%----------------------------------------------------------
%% communication {{{

%% @private
%% @doc Handle {@link gen_event:notify/2}.

handle_event({Level, Message} = _Event, State) ->
  try_syslog(Level, Message),
  {ok, State};

handle_event(Event, State) ->
  try_syslog(unknown, Event),
  {ok, State}.

%% @private
%% @doc Handle {@link gen_event:call/3}.

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

try_syslog(Level, Message) ->
  SyslogPriority = case Level of
    info     -> info;
    error    -> err;
    critical -> crit;
    _        -> warning
  end,
  try
    indira_syslog ! {syslog, haircut, daemon, SyslogPriority, Message}
  catch
    error:badarg -> ignore
  end.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
