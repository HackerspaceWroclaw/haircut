%%%---------------------------------------------------------------------------
%%% @doc
%%%   Restarter for {@link haircut_bot}. The process expects to be a sibling
%%%   of {@link haircut_bot} processes (supervisors are ignored, only workers
%%%   count here) and talks with its supervisor about them.
%%% @end
%%%---------------------------------------------------------------------------

-module(haircut_bot_restarter).

-behaviour(gen_server).

%%% public API
-export([start_link/0]).

%%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------

-record(state, {parent, init}).
-define(MONITOR_TABLE, nabla). % TODO: change the table name
-define(RESTART_INTERVAL, 10000).

%%%---------------------------------------------------------------------------
%%% public API {{{

%% @doc Start haircut bot restarter.

-spec start_link() ->
  {ok, pid()} | {error, term()}.

start_link() ->
  Parent = self(),
  gen_server:start_link(?MODULE, [Parent], []).

%%% }}}
%%%---------------------------------------------------------------------------
%%% gen_server callbacks

%%----------------------------------------------------------
%% initialization and cleanup {{{

%% @private
%% @doc Initialize {@link gen_server} state.

init([Parent] = _Args) ->
  ets:new(?MONITOR_TABLE, [set, named_table]),
  {ok, #state{parent = Parent, init = true}, 0}.

%% @private
%% @doc Clean up {@link gen_server} state.

terminate(_Reason, _State) ->
  ets:delete(?MONITOR_TABLE),
  ok.

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

% retrieve siblings' pids and monitor them
handle_info(timeout = _Msg, State = #state{parent = Parent, init = true}) ->
  Siblings = [
    {Name, Pid} ||
    {Name, Pid, worker, _Modules} <- supervisor:which_children(Parent), 
    Pid /= self()
  ],
  MonitorRefs = monitor_all(Siblings),
  ets:insert(?MONITOR_TABLE, MonitorRefs),
  {noreply, State#state{init = false}};

% bot went down, setup a restart timer for it
handle_info({'DOWN', Ref, process, Pid, _Info} = _Msg, State) ->
  case ets:lookup(?MONITOR_TABLE, Ref) of
    [] ->
      % not a thing that was intentionally monitored (?!?)
      ignore;
    [{Ref, Name}] ->
      ets:delete(?MONITOR_TABLE, Ref),
      case Pid of
        undefined ->
          ok;
        _ when is_pid(Pid) ->
          Report = [{bot, Name}, {restart_after, ?RESTART_INTERVAL}],
          log_info("bot went down", Report)
      end,
      erlang:send_after(?RESTART_INTERVAL, self(), {restart, Name})
  end,
  {noreply, State};

% timer expired, restart the sibling
handle_info({restart, SiblingName} = _Msg, State = #state{parent = Parent}) ->
  log_info("restart timer fired", [{bot, SiblingName}]),
  case supervisor:restart_child(Parent, SiblingName) of
    {ok, undefined = Pid} ->
      log_info("bot not started: network problem", [{bot, SiblingName}]),
      % pretend the sibling started and try to monitor it
      MonitorRefs = monitor_all([{SiblingName, Pid}]),
      ets:insert(?MONITOR_TABLE, MonitorRefs);
    {ok, Pid} ->
      log_info("bot started successfully", [{bot, SiblingName}, {pid, Pid}]),
      MonitorRefs = monitor_all([{SiblingName, Pid}]),
      ets:insert(?MONITOR_TABLE, MonitorRefs);
    {error, running} ->
      log_info("bot already started", [{bot, SiblingName}]),
      Siblings = [
        {Name, Pid} ||
        {Name, Pid, worker, _Modules} <- supervisor:which_children(Parent), 
        Pid /= self(), Name == SiblingName
      ],
      MonitorRefs = monitor_all(Siblings),
      ets:insert(?MONITOR_TABLE, MonitorRefs);
    {error, Reason} ->
      log_error("some error", [{bot, SiblingName}, {error, Reason}]),
      'TODO' % stop
  end,
  {noreply, State};

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
%% support functions {{{

monitor_all([] = _Processes) ->
  [];

monitor_all([{Name, Pid} | Rest] = _Processes) ->
  case Pid of
    undefined ->
      % send a fake DOWN message about missing child
      MonitorRef = make_ref(),
      self() ! {'DOWN', MonitorRef, process, undefined, undefined};
    _ when is_pid(Pid) ->
      MonitorRef = monitor(process, Pid)
  end,
  Record = {MonitorRef, Name},
  [Record | monitor_all(Rest)].


log_info(Message, Report) ->
  error_logger:info_report(haircut_restarter, [{msg, Message} | Report]).

%log_warn(Message, Report) ->
%  error_logger:warning_report(haircut_restarter, [{msg, Message} | Report]).

log_error(Message, Report) ->
  error_logger:error_report(haircut_restarter, [{msg, Message} | Report]).

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
