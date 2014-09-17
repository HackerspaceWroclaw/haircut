%%%---------------------------------------------------------------------------
%%% @doc
%%%   Logger for operational events.
%%% @end
%%%---------------------------------------------------------------------------

-module(haircut_log_operational).

%%% public API
-export([start_link/0]).

%%%---------------------------------------------------------------------------
%%% public API

start_link() ->
  case gen_event:start_link({local, ?MODULE}) of
    {ok, Pid} ->
      {ok, LogSink} = application:get_env(op_log),
      add_handlers(Pid, LogSink),
      {ok, Pid};
    {error, Reason} ->
      {error, Reason}
  end.

%%%---------------------------------------------------------------------------

add_handlers(_Pid, none = _LogSink) ->
  ok;
add_handlers(_Pid, syslog = _LogSink) ->
  'TODO';
add_handlers(Pid, LogSink) when is_list(LogSink) ->
  gen_event:add_handler(Pid, haircut_file_term_h, LogSink).

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
