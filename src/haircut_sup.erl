%%%---------------------------------------------------------------------------
%%% @doc
%%%   Bot's root supervisor.
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
    {haircut_commander,
      {haircut_commander, start_link, []},
      permanent, 5000, worker, [haircut_commander]}
  ],
  {ok, {
    {one_for_one, 5, 10},
    Children
  }}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
