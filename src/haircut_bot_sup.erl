%%%---------------------------------------------------------------------------
%%% @doc
%%%   Bot's and bot restarter's supervisor.
%%% @end
%%%---------------------------------------------------------------------------

-module(haircut_bot_sup).

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
    {haircut_bot,
      {haircut_bot, start_link, []},
      permanent, 5000, worker, [haircut_bot]},
    {haircut_bot_restarter,
      {haircut_bot_restarter, start_link, []},
      permanent, 5000, worker, [haircut_bot_restarter]}
  ],
  {ok, {
    {one_for_one, 5, 10},
    Children
  }}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
