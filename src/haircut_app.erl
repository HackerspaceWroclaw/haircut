%%%---------------------------------------------------------------------------
%%% @doc
%%%   Bot's application entry point.
%%% @end
%%%---------------------------------------------------------------------------

-module(haircut_app).

-behaviour(application).

%%% application callbacks
-export([start/2, stop/1]).

%%%---------------------------------------------------------------------------

-type state() :: term().

%%%---------------------------------------------------------------------------
%%% application callbacks

-spec start(normal, term()) ->
  {ok, pid()} | {ok, pid(), state()} | {error, term()}.

start(_StartType, _StartArgs) ->
  haircut_sup:start_link().

-spec stop(state()) ->
  any().

stop(_State) ->
  ok.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
