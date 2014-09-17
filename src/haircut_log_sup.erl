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
  % TODO: haircut_log_syslog, but some time later it will get moved to Indira
  Children = [
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
