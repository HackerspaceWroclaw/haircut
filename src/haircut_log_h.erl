%%%---------------------------------------------------------------------------
%%% @doc
%%%   Log handler for {@link error_logger}.
%%%   This is temporary module intended to work out how logs should be
%%%   formatted and stored and what log types are to be handled by Indira.
%%% @end
%%%---------------------------------------------------------------------------

-module(haircut_log_h).

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

%% log entry emitted when can't connect to epmd starts with:
%%   Protocol: "inet_tcp": register error: ...
handle_event({info_msg, _GLead, {_Pid, _Format, _Data}} = _Event, State) ->
  {ok, State}; % ignore

%% log entry emitted when a gen_server crashes
handle_event({error_msg, _GLead, {_Pid, _Format, _Data}} = _Event, State) ->
  {ok, State}; % ignore

%% info-level: application started/stopped, child started, Indira INFO
handle_event({info_report, _GLead, {_Pid, Type, Report}} = _Event, State) ->
  case {Type,Report} of
    {progress, [{application, App}, {started_at, _AppNode}]} ->
      % application started
      io:fwrite(". application ~s started~n", [App]);
    {progress, [{supervisor, {_SupPid, _SupName}}, {started, _Child}]} ->
      % child started
      % TODO: what is `SupName' when supervisor is not a registered process?
      % TODO: log some operational details, like child's name, PID and MFA
      ignore;
    {std_info, [{application, App}, {exited, _Reason}, {type, _StartType}]} ->
      % application stopped
      io:fwrite(". application ~s stopped~n", [App]);
    {std_info, [{indira_info, MsgType} | Context]} ->
      % Indira INFO messages
      io:fwrite(". indira[~p]: ~1024p~n", [MsgType, Context]);
    {_,_} ->
      % TODO: haircut's own logs
      % TODO: warnings (+W i)
      ignore
  end,
  {ok, State};

%% error-level: crash reports, child start problems, Indira CRITICAL
handle_event({error_report, _GLead, {_Pid, Type, Report}} = _Event, State) ->
  case {Type,Report} of
    {crash_report, [CrashProps, _EmptyList]} ->
      % gen_server (or supervisor) failed to start, gen_server crashed
      % _EmptyList: at least it is expected it's `[]'
      % NOTE: this does not include processes that got `exit(P,Reason)'
      format_child_crash_report(CrashProps);
    {supervisor_report, [{supervisor, {_SupPid, _SupName}},
                          {errorContext, child_terminated},
                          {reason, _Reason}, {offender, _ChildProps}]} ->
      % similar to crash report above, but cleaner MFA specification and is
      % generated even for processes that got exit signal
      % TODO: log these details
      ignore;
    {std_error, [{indira_error, MsgType} | Context]} ->
      io:fwrite("!! indira[~p]: ~1024p~n", [MsgType, Context]);
    {_,_} ->
      % TODO: haircut's own logs
      % TODO: warnings (no +W flag)
      ignore
  end,
  {ok, State};

%% warning-level: Indira ERROR
handle_event({warning_report, _GLead, {_Pid, Type, Report}} = _Event, State) ->
  case {Type,Report} of
    {std_warning, [{indira_error, MsgType} | Context]} ->
      io:fwrite("! indira[~p]: ~1024p~n", [MsgType, Context]);
    {_,_} ->
      ignore
  end,
  {ok, State};

%% any other message: ignore
handle_event(_Event, State) ->
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
%% helper functions

format_child_crash_report(Details) ->
  Pid = proplists:get_value(pid, Details),
  RegName = proplists:get_value(registered_name, Details),
  {Mod,_Func,_Args} = proplists:get_value(initial_call, Details),
  % TODO: format this value to something readable
  % the value seems to be:
  % {exit, {Reason :: term(), StackTrace :: [tuple()]}}
  % how about throw?
  CrashInfo = proplists:get_value(error_info, Details),
  % NOTE: the idea that `Mod' (initial call module) is the process' meaning is
  % just a guess; supervisor has better information about this (process' name,
  % for instance)
  case RegName of
    undefined ->
      io:fwrite("! ~s (~p) crashed: ~1024p~n", [Mod, Pid, CrashInfo]);
    _ ->
      io:fwrite("! ~s (~p ~p) crashed: ~1024p~n",
                [Mod, Pid, RegName, CrashInfo])
  end,
  ok.

%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
