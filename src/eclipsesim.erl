%%% -*- erlang-indent-level: 2 -*-
%%%
%%% DG Eclipse simulator

-module(eclipsesim).

-export([ main/1
        , format_error/1
        ]).

%% Command-line interface ======================================================

-spec main([string()]) -> no_return().
main(_Args) ->
  halt(0).

%% Format errors ===============================================================

-spec format_error(term()) -> io_lib:chars().
format_error({Module, Reason} = Error) when is_atom(Module) ->
  case erlang:function_exported(Module, format_error, 1) of
    true ->
      try Module:format_error(Reason)
      catch _:_ -> default_format_error(Error)
      end;
    false -> default_format_error(Error)
  end;
format_error(Error) -> default_format_error(Error).

default_format_error(Error) ->
  io_lib:format("~tp", [Error]).
