%%%---------------------------------------------------------------------------
%%% @doc
%%%   Logger for activity (talks, presence and issued !commands).
%%% @end
%%%---------------------------------------------------------------------------

-module(haircut_log_activity).

%%% public API
-export([start_link/0]).

%%%---------------------------------------------------------------------------
%%% public API

start_link() ->
  case gen_event:start_link({local, ?MODULE}) of
    {ok, Pid} ->
      {ok, LogSink} = application:get_env(activity_log),
      add_handlers(Pid, LogSink),
      {ok, Pid};
    {error, Reason} ->
      {error, Reason}
  end.

%%%---------------------------------------------------------------------------

add_handlers(_Pid, none = _LogSink) ->
  ok;
add_handlers(Pid, LogSink) when is_list(LogSink) ->
  gen_event:add_handler(Pid, haircut_file_term_h, LogSink).

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
