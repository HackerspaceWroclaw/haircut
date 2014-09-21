%%%---------------------------------------------------------------------------
%%% @doc
%%%   IRC bot body.
%%% @end
%%%---------------------------------------------------------------------------

-module(haircut_bot).

-behaviour(gen_ealirc).

%%% public API
-export([start_link/0, start_link/6]).

%%% gen_ealirc callbacks
-export([init/1, terminate/2]).
-export([connected/2, disconnected/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, handle_message/4]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------

-define(RECONNECT_INTERVAL, 10000). % reconnect after 10s
-record(state, {
  nick :: ealirc:nick(),
  user :: {string(), string()},
  channels :: [ealirc:channel()]
}).

%%%---------------------------------------------------------------------------
%%% public API {{{

%% @doc Start haircut bot process, taking configuration from application
%%   environment.

-spec start_link() ->
  {ok, pid()} | {error, term()}.

start_link() ->
  {ok, Server} = application:get_env(server),
  {ok, Port} = application:get_env(port),
  {ok, Nick} = application:get_env(nick),
  {ok, {User, FullName}} = application:get_env(user),
  {ok, Channels} = application:get_env(channels),
  start_link(Server, Port, Nick, User, FullName, Channels).

%% @doc Start haircut bot process.
%%
%%   When `User' is set to `env', the value of `os:getenv("USER")' is used
%%   here.
%%
%%   When `Nick' is set to `user', the value of `User' (after reading `$USER')
%%   is used as a nick.

-spec start_link(inet:hostname() | inet:ip_address(), integer(),
                 user | string(), env | string(), string(),
                 [ealirc:channel()]) ->
  {ok, pid()} | {error, term()}.

start_link(Server, Port, Nick, env = _User, FullName, Channels) ->
  EnvUser = os:getenv("USER"),
  start_link(Server, Port, Nick, EnvUser, FullName, Channels);

start_link(Server, Port, user = _Nick, User, FullName, Channels)
when is_list(User) ->
  start_link(Server, Port, User, User, FullName, Channels);

start_link(Server, Port, Nick, User, FullName, Channels) ->
  Args = [Nick, User, FullName, Channels],
  gen_ealirc:start_link({local, ?MODULE}, ?MODULE, Args, {Server, Port}, []).

%%% }}}
%%%---------------------------------------------------------------------------
%%% gen_ealirc callbacks

%%----------------------------------------------------------
%% initialization and cleanup {{{

%% @private
%% @doc Initialize {@link gen_ealirc} state.

init([Nick, User, FullName, Channels] = _Args) ->
  State = #state{
    nick = Nick,
    user = {User, FullName},
    channels = Channels
  },
  {ok, State}.

%% @private
%% @doc Clean up {@link gen_ealirc} state.

terminate(_Reason, _State) ->
  ok.

%% @private
%% @doc Configure IRC connection.

connected(_Socket, State = #state{nick = Nick, user = {User, FullName},
                                  channels = Channels}) ->
  error_logger:info_report(bot, [{event, connected}]),
  % TODO: `Nick' could be already in use
  gen_ealirc:nick(self(), Nick),
  gen_ealirc:user(self(), User, none, FullName),
  gen_ealirc:join(self(), Channels),
  {ok, State}.

%% @private
%% @doc Handle the case when the connection was lost.

disconnected({connect,_} = _Reason, State) ->
  error_logger:info_report(bot, [{event, no_connection}]),
  {reconnect, ?RECONNECT_INTERVAL, State};

disconnected(_Reason, State) ->
  error_logger:info_report(bot, [{event, connection_lost}]),
  {reconnect, ?RECONNECT_INTERVAL, State}.

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

handle_info(_Msg, State) ->
  {noreply, State}.

%% @private
%% @doc Handle incoming IRC messages.

handle_message(Prefix, "PING" = _Command, Args, State = #state{nick = Nick}) ->
  {ok, PongCmd} = ealirc_proto:pong(Nick),
  gen_ealirc:quote(self(), PongCmd),
  case {Prefix,Args} of
    {none,[From | _]} ->
      io:fwrite("<~s> PING from ~s~n", [Nick, From]);
    {_,[From | _]} ->
      io:fwrite("<~s> PING from ~s (~p)~n", [Nick, From, Prefix])
  end,
  {noreply, State};

handle_message({user, Nick, _, _} = _Prefix,
               "NICK"             = _Command,
               [NewNick]          = _Args,
               State              = #state{nick = Nick}) ->
  io:fwrite("<~s> Changing nickname from ~s to ~s~n", [Nick, Nick, NewNick]),
  {noreply, State#state{nick = NewNick}};

handle_message({user, Nick, _, _} = _Prefix,
               "PRIVMSG"          = _Command,
               [MsgTarget, "!" ++ Request] = _Args,
               State) ->
  [ReqCmd | _] = string:tokens(Request, " "),
  Reply = "sorry, command " ++ ReqCmd ++ " is not implemented yet",
  case MsgTarget of
    "#" ++ _ -> % other channel indicators: "+", "!", "&"
      gen_ealirc:privmsg(self(), MsgTarget, Nick ++ ": " ++ Reply);
    _ ->
      gen_ealirc:privmsg(self(), Nick, Reply)
  end,
  {noreply, State};

handle_message({user, Nick, _, _} = _Prefix,
               "MODE"             = _Command,
               [Channel, "+o", SelfNick] = _Args,
               State              = #state{nick = SelfNick}) ->
  gen_ealirc:privmsg(self(), Channel, [Nick ++ ": thank you"]),
  {noreply, State};

handle_message({user, Nick, _, _} = _Prefix,
               "MODE"             = _Command,
               [Channel, "-o", SelfNick] = _Args,
               State              = #state{nick = SelfNick}) ->
  gen_ealirc:privmsg(self(), Channel, [Nick ++ ": you bastard!"]),
  {noreply, State};

handle_message(Prefix, Command, Args, State = #state{nick = Nick}) ->
  io:fwrite("<~s> [~p] ~p ~1024p~n", [Nick, Prefix, Command, Args]),
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
