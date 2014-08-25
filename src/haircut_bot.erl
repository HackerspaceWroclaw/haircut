%%%---------------------------------------------------------------------------
%%% @doc
%%%   IRC bot body.
%%% @end
%%%---------------------------------------------------------------------------

-module(haircut_bot).

-behaviour(gen_ealirc).

%%% public API
-export([start_link/0, start_link/5]).

%%% gen_ealirc callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, handle_message/4]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------

-record(state, {nick}).

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
  start_link(Server, Port, Nick, User, FullName).

%% @doc Start haircut bot process.
%%
%%   When `User' is set to `env', the value of `os:getenv("USER")' is used
%%   here.
%%
%%   When `Nick' is set to `user', the value of `User' (after reading `$USER')
%%   is used as a nick.

-spec start_link(inet:hostname() | inet:ip_address(), integer(),
                 user | string(), env | string(), string()) ->
  {ok, pid()} | {error, term()}.

start_link(Server, Port, Nick, env = _User, FullName) ->
  EnvUser = os:getenv("USER"),
  start_link(Server, Port, Nick, EnvUser, FullName);

start_link(Server, Port, user = _Nick, User, FullName) when is_list(User) ->
  start_link(Server, Port, User, User, FullName);

start_link(Server, Port, Nick, User, FullName) ->
  Args = [Nick, User, FullName],
  gen_ealirc:connect_link(Server, Port, {local, ?MODULE}, ?MODULE, Args, []).

%%% }}}
%%%---------------------------------------------------------------------------
%%% gen_ealirc callbacks

%%----------------------------------------------------------
%% initialization and cleanup {{{

%% @private
%% @doc Initialize {@link gen_ealirc} state.

init([Nick, User, FullName] = _Args) ->
  gen_ealirc:nick(self(), Nick),
  gen_ealirc:user(self(), User, none, FullName),
  % TODO: `Nick' could be already in use
  {ok, #state{nick = Nick}}.

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
