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
  % TODO: unhardcode this
  File = "run/operational.log",
  case gen_event:start_link({local, ?MODULE}) of
    {ok, Pid} ->
      gen_event:add_handler(Pid, haircut_file_term_h, File),
      {ok, Pid};
    {error, Reason} ->
      {error, Reason}
  end.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
