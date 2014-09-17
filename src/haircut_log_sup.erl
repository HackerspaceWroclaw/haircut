%%%---------------------------------------------------------------------------
%%% @doc
%%%   Logger supervisor.
%%% @end
%%%---------------------------------------------------------------------------

-module(haircut_log_sup).

-behaviour(supervisor).

%%% public API
-export([start_link/0]).

%%% supervisor callbacks
-export([init/1]).

%%%---------------------------------------------------------------------------
%%% public API

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%---------------------------------------------------------------------------
%%% supervisor callbacks

init([] = _Args) ->
  Children = [
    {haircut_syslog,
      {haircut_syslog, start_link, []},
      permanent, 5000, worker, [haircut_syslog]},
    {haircut_log_activity,
      {haircut_log_activity, start_link, []},
      permanent, 5000, worker, dynamic},
    {haircut_log_operational,
      {haircut_log_operational, start_link, []},
      permanent, 5000, worker, dynamic}
  ],
  {ok, {
    {one_for_one, 5, 10},
    Children
  }}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
