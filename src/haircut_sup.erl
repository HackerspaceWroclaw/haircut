%%%---------------------------------------------------------------------------
%%% @doc
%%%   Application's root supervisor.
%%% @end
%%%---------------------------------------------------------------------------

-module(haircut_sup).

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
    {haircut_log_sup,
      {haircut_log_sup, start_link, []},
      permanent, 5000, supervisor, [haircut_log_sup]},
    {haircut_commander,
      {haircut_commander, start_link, []},
      permanent, 5000, worker, [haircut_commander]},
    {haircut_bot_sup,
      {haircut_bot_sup, start_link, []},
      permanent, 5000, supervisor, [haircut_bot_sup]}
  ],
  {ok, {
    {one_for_one, 5, 10},
    Children
  }}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
