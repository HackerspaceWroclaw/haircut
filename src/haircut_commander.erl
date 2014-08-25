%%%---------------------------------------------------------------------------
%%% @doc
%%%   Controller process.
%%% @end
%%%---------------------------------------------------------------------------

-module(haircut_commander).

-behaviour(gen_server).

%%% public API
-export([start_link/0]).

%%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------

-record(state, {}).

%%%---------------------------------------------------------------------------
%%% public API

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%---------------------------------------------------------------------------
%%% gen_server callbacks

%%----------------------------------------------------------
%% initialization and cleanup {{{

%% @private
%% @doc Initialize {@link gen_ealirc} state.

init([] = _Args) ->
  {ok, #state{}}.

%% @private
%% @doc Clean up {@link gen_ealirc} state.

terminate(_Reason, _State) ->
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

handle_info({command, ReplyTo, ChannelID, {<<"shutdown">>, _}} = _Message,
            State) ->
  ReplyTo ! {result, ChannelID, ok},
  init:stop(),
  {noreply, State};

handle_info({command, ReplyTo, ChannelID, Command} = _Message, State) ->
  io:fwrite("got command ~p~n", [Command]),
  Reply = [{error, <<"unsupported command">>}],
  ReplyTo ! {result, ChannelID, Reply},
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

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
