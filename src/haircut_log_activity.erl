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
  % TODO: unhardcode this
  File = "run/activity.log",
  case gen_event:start_link({local, ?MODULE}) of
    {ok, Pid} ->
      gen_event:add_handler(Pid, haircut_file_term_h, File),
      {ok, Pid};
    {error, Reason} ->
      {error, Reason}
  end.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
