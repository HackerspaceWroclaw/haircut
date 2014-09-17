%%%---------------------------------------------------------------------------
%%% @doc
%%%   Log handler for {@link error_logger}.
%%%   This is temporary module intended to work out how logs should be
%%%   formatted and stored and what log types are to be handled by Indira.
%%%
%%% @TODO Send events with stack trace somewhere to log detailed details.
%%% @TODO Split this module to collect operational logs separately from usage
%%%   (operational logs will go to Indira in the future).
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
      oplog(info, "application started", [{application, App}]);
    {progress, [{supervisor, {_SupPid, _SupName}}, {started, _Child}]} ->
      % child started
      % TODO: what is `SupName' when supervisor is not a registered process?
      % TODO: log some operational details, like child's name, PID and MFA
      ignore;
    {std_info, [{application, App}, {exited, stopped}, {type, _StartType}]} ->
      % application stopped
      oplog(info, "application stopped", [{application, App}]);
    {std_info, [{application, App}, {exited, Reason}, {type, _StartType}]} ->
      % application stopped unexpectedly
      oplog(error, "application crashed",
            [{application, App}, {reason, normalize_reason(Reason)}]);
    {std_info, [{indira_info, MsgType} | Context]} ->
      % Indira INFO messages
      oplog(info, MsgType, [{context, Context}]);
    {_,_} ->
      % TODO: haircut's own logs
      % TODO: warnings (+W i)
      io:fwrite("# ignored info type=~p: ~1024p~n", [Type, Report]),
      ignore
  end,
  {ok, State};

%% error-level: crash reports, child start problems, Indira CRITICAL
handle_event({error_report, _GLead, {Pid, Type, Report}} = _Event, State) ->
  case {Type,Report} of
    {crash_report, [_CrashProps, _EmptyList]} ->
      % gen_server (or supervisor) failed to start, gen_server crashed
      % _EmptyList: at least it is expected it's `[]'
      % NOTE: this does not include processes that got `exit(P,Reason)'
      ignore;
    {supervisor_report, [{supervisor, {_SupPid, _SupName} = SupId},
                          {errorContext, start_error},
                          {reason, Reason}, {offender, ChildProps}]} ->
      oplog(error, "process start error",
            [{reason, normalize_reason(Reason)},
              {supervisor, supervisor_info(Pid, SupId)},
              {child, child_info(ChildProps)}]);
    {supervisor_report, [{supervisor, {_SupPid, _SupName} = SupId},
                          {errorContext, child_terminated},
                          {reason, Reason}, {offender, ChildProps}]} ->
      % similar to crash report above, but cleaner MFA specification and is
      % generated even for processes that got exit signal
      TrueReason = normalize_reason(Reason),
      {Level, Message} = case TrueReason of
        normal   -> {info,  "process stopped"};
        shutdown -> {info,  "process shut down"};
        _        -> {error, "process crashed"}
      end,
      oplog(Level, Message,
            [{reason, TrueReason},
              {supervisor, supervisor_info(Pid, SupId)},
              {child, child_info(ChildProps)}]);
    {std_error, [{indira_error, MsgType} | Context]} ->
      oplog(critical, MsgType, [{context, Context}]);
    {_,_} ->
      % TODO: haircut's own logs
      % TODO: warnings (no +W flag)
      io:fwrite("# ignored error type=~p: ~1024p~n", [Type, Report]),
      ignore
  end,
  {ok, State};

%% warning-level: Indira ERROR
handle_event({warning_report, _GLead, {_Pid, Type, Report}} = _Event, State) ->
  case {Type,Report} of
    {std_warning, [{indira_error, MsgType} | Context]} ->
      oplog(error, MsgType, [{context, Context}]);
    {_,_} ->
      io:fwrite("# ignored warning type=~p: ~1024p~n", [Type, Report]),
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

%%%---------------------------------------------------------------------------

%% @doc Send operational log.
%%
%% @spec oplog(atom(), atom() | string() | binary(), list()) ->
%%   atom()

oplog(Level, Event, Context) when is_list(Event) ->
  oplog(Level, list_to_binary(Event), Context);

oplog(Level, Event, Context) when is_binary(Event); is_atom(Event) ->
  try
    Message = {Level, [{event, Event} | Context]},
    gen_event:notify(haircut_log_operational, Message)
  catch
    error:badarg -> ignore
  end.

%%%---------------------------------------------------------------------------
%%% helper functions {{{

supervisor_info(Pid, {local, SupName} = _SupId) ->
  Info = [
    {name, SupName},
    {message_origin, pid_to_binary(Pid)}
  ],
  Info;

supervisor_info(Pid, {SupPid, SupName} = _SupId) when is_pid(SupPid) ->
  Info = [
    {pid, pid_to_binary(SupPid)},
    {name, SupName},
    {message_origin, pid_to_binary(Pid)}
  ],
  Info.

%%----------------------------------------------------------

child_info(ChildProps) ->
  collect_child_info(ChildProps).

%%----------------------------------------------------------

collect_child_info([] = _ChildProps) ->
  [];

collect_child_info([{pid, undefined} | RestInfo]) ->
  [{pid, undefined} | collect_child_info(RestInfo)];
collect_child_info([{pid, Pid} | RestInfo]) when is_pid(Pid) ->
  [{pid, pid_to_binary(Pid)} | collect_child_info(RestInfo)];

collect_child_info([{name, undefined} | RestInfo]) ->
  collect_child_info(RestInfo);
collect_child_info([{name, Name} | RestInfo]) ->
  [{name, Name} | collect_child_info(RestInfo)];

collect_child_info([{child_type, Type} | RestInfo]) ->
  [{type, Type} | collect_child_info(RestInfo)];

collect_child_info([_Info | RestInfo]) ->
  collect_child_info(RestInfo).

%%----------------------------------------------------------

pid_to_binary(Pid) when is_pid(Pid) ->
  list_to_binary(pid_to_list(Pid)).

%%----------------------------------------------------------

normalize_reason({{TrueReason, _Value}, Stack} = _Reason)
when is_list(Stack) ->
  % `{badmatch,V}', `{case_clause,V}', `{try_clause,V}', ...
  TrueReason;

normalize_reason({undef, [{MissM,MissF,MissArgs} | _] = _Stack} = _Reason) ->
  % undefined function
  % TODO: FuncName = <<
  %   (atom_to_binary(MissM, utf8))/binary, ":",
  %   (atom_to_binary(MissF, utf8))/binary, "/",
  %   (list_to_binary(integer_to_list(length(MissArgs))))/binary
  % >>
  {undef, {MissM, MissF, length(MissArgs)}};

normalize_reason({TrueReason, Stack} = _Reason) when is_list(Stack) ->
  % process died (with stack trace)
  TrueReason;

normalize_reason({'EXIT', TrueReason} = _Reason) ->
  % `catch(exit(...))'
  TrueReason;

normalize_reason(Reason) ->
  Reason.

%%----------------------------------------------------------

%%% }}}
%%%---------------------------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
