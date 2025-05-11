%%% -*- erlang-indent-level: 2 -*-
%%%
%%% DG Eclipse memory simulation
%%%
%%% We simulate a 32K x 16 bits RAM initialized to all bits zero, stored in an ETS table.
%%%
%%% TODO:
%%% - allow the loader to mark regions as write-protected
%%% - emulate the MMPU paging unit and a larger physical RAM

-module(memory).

-export([ init/0
        , get_word/1
        , set_word/2
        ]).

-export_type([ address/0
             ]).

-define(UINT15_MAX, ((1 bsl 15) - 1)).
-type address() :: 0..?UINT15_MAX.
-type word() :: 0..((1 bsl 16) - 1).

-define(ETS, ?MODULE).

%% API =========================================================================

-spec init() -> ok.
init() ->
  ets:new(?ETS, [named_table, public]),
  ok.

-spec get_word(address()) -> word().
%% TODO: for OTP >= 26 use ets:lookup_element/4
get_word(Address) ->
  case ets:lookup(?ETS, Address) of
    [{_Address, Word}] -> Word;
    [] -> 0
  end.

-spec set_word(address(), word()) -> ok.
set_word(Address, Word) ->
  ets:insert(?ETS, {Address, Word}),
  ok.
