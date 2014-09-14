%%%---------------------------------------------------------------------------
%%% @doc
%%%   Log handler for {@link error_logger}.
%%%   This is temporary module intended to work out how logs should be
%%%   formatted and stored and what log types are to be handled by Indira.
%%%
%%% @TODO Send events with stack trace somewhere to log detailed details.
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
    {std_info, [{application, App}, {exited, stopped}, {type, _StartType}]} ->
      % application stopped
      io:fwrite(". application ~s stopped~n", [App]);
    {std_info, [{application, App}, {exited, Reason}, {type, _StartType}]} ->
      % application stopped unexpectedly
      io:fwrite("!! application ~s crashed (~1024p)~n", [App, Reason]);
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
      % similar to crash report above, but cleaner MFA specification and is
      % generated even for processes that got exit signal
      format_supervisor_start_error_report({SupId, Pid}, Reason, ChildProps);
    {supervisor_report, [{supervisor, {_SupPid, _SupName} = SupId},
                          {errorContext, child_terminated},
                          {reason, Reason}, {offender, ChildProps}]} ->
      % similar to crash report above, but cleaner MFA specification and is
      % generated even for processes that got exit signal
      format_supervisor_crash_report({SupId, Pid}, Reason, ChildProps);
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

%%%---------------------------------------------------------------------------
%%% helper functions {{{

%%----------------------------------------------------------

format_supervisor_crash_report(Supervisor, shutdown = _Reason, ChildProps) ->
  % child was ordered to stop
  Pid = proplists:get_value(pid, ChildProps),
  Name = proplists:get_value(name, ChildProps),
  ChildType = proplists:get_value(child_type, ChildProps),
  case {Name,ChildType} of
    {undefined, _} ->
      % simple_one_for_one, swarm worker (swarm supervisor? unlikely)
      io:fwrite("! swarm ~p (~p under ~1024p) shut down~n",
                [Pid, ChildType, format_supervisor(Supervisor)]);
    {_, supervisor} ->
      io:fwrite("! supervisor ~p (pid ~p) shut down~n", [Name, Pid]);
    {_, worker} ->
      io:fwrite("! ~p (pid ~p) shut down~n", [Name, Pid])
  end,
  ok;

format_supervisor_crash_report(Supervisor, normal = _Reason, ChildProps) ->
  % child stopped
  Pid = proplists:get_value(pid, ChildProps),
  Name = proplists:get_value(name, ChildProps),
  ChildType = proplists:get_value(child_type, ChildProps),
  case {Name,ChildType} of
    {undefined, _} ->
      % simple_one_for_one, swarm worker (swarm supervisor? unlikely)
      io:fwrite(". swarm ~p (~p under ~1024p) stopped normally~n",
                [Pid, ChildType, format_supervisor(Supervisor)]);
    {_, supervisor} ->
      io:fwrite(". supervisor ~p (pid ~p) stopped normally~n", [Name, Pid]);
    {_, worker} ->
      io:fwrite(". ~p (pid ~p) stopped normally~n", [Name, Pid])
  end,
  ok;

format_supervisor_crash_report(Supervisor, Reason, ChildProps) ->
  % child died
  Pid = proplists:get_value(pid, ChildProps),
  Name = proplists:get_value(name, ChildProps),
  ChildType = proplists:get_value(child_type, ChildProps),
  TrueReason = format_exit_reason(Reason),
  case {Name,ChildType} of
    {undefined, _} ->
      % simple_one_for_one, swarm worker (swarm supervisor? unlikely)
      io:fwrite("! swarm ~p (~p under ~1024p) crashed: ~1024p~n",
                [Pid, ChildType, format_supervisor(Supervisor), TrueReason]);
    {_, supervisor} ->
      io:fwrite("! supervisor ~p (pid ~p) crashed: ~1024p~n",
                [Name, Pid, TrueReason]);
    {_, worker} ->
      io:fwrite("! ~p (pid ~p) crashed: ~1024p~n", [Name, Pid, TrueReason])
  end,
  ok.

%%----------------------------------------------------------

format_supervisor_start_error_report(Supervisor, Reason, ChildProps) ->
  Name = proplists:get_value(name, ChildProps),
  ChildType = proplists:get_value(child_type, ChildProps),
  TrueReason = format_exit_reason(Reason),
  case {Name,ChildType} of
    {undefined, _} ->
      % simple_one_for_one, swarm worker (swarm supervisor? unlikely)
      io:fwrite("! swarm ~p (under ~1024p) didn't start: ~1024p~n",
                [ChildType, format_supervisor(Supervisor), TrueReason]);
    {_, supervisor} ->
      io:fwrite("! supervisor ~p didn't start: ~p~n",
                [Name, TrueReason]);
    {_, worker} ->
      io:fwrite("! ~p didn't start: ~p~n", [Name, TrueReason])
  end,
  ok.

%%----------------------------------------------------------

format_supervisor({{local, Name}, _Pid} = _Supervisor) ->
  Name;

format_supervisor({{Pid, Name}, Pid} = _Supervisor) ->
  {Name, Pid};

format_supervisor({{_SomePid, Name}, Pid} = _Supervisor) ->
  {Name, Pid}.

%%----------------------------------------------------------

format_exit_reason({{TrueReason, _Value}, Stack} = _Reason)
when is_list(Stack) ->
  % `{badmatch,V}', `{case_clause,V}', `{try_clause,V}', ...
  TrueReason;

format_exit_reason({undef, [{MissM,MissF,MissArgs} | _] = _Stack} = _Reason) ->
  % undefined function
  {undef, {MissM, MissF, length(MissArgs)}};

format_exit_reason({TrueReason, Stack} = _Reason) when is_list(Stack) ->
  % process died (with stack trace)
  TrueReason;

format_exit_reason({'EXIT', TrueReason} = _Reason) ->
  % `catch(exit(...))'
  TrueReason;

format_exit_reason(Reason) ->
  Reason.

%%----------------------------------------------------------

%%% }}}
%%%---------------------------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
