%%% -*- erlang-indent-level: 2 -*-
%%%
%%% Support for radix 50 encoded symbols in DG relocatable binary files.
%%%
%%% Notes:
%%% - the "50" in "radix 50" is itself in octal, so it's "radix 40" in decimal
%%% - symbols are limited to 5 characters, and are padded with NULs if necessary
%%% - characters may be 0-9, A-Z, ".", "?", or NUL
%%%
%%% References:
%%% 093-000080-05_Extended_Relocatable_Loaders_Mar78.pdf, pages A-1 to A-2
%%% https://github.com/napobear/dpa/blob/master/nova/radix50.c

-module(radix50).

-export([ from_string/1
        , to_string/2
        ]).

-export_type([ word/0
             ]).

-type word() :: 0..((1 bsl 16) - 1).

-define(RADIX, 8#50).

%% API =========================================================================

-spec from_string(string()) -> {ok, {word(), word()}} | {error, any()}.
from_string(A) ->
  from_string(A, _I = 4, _B = erlang:make_tuple(5, 0)).

from_string([Ai | A], I, B) when I >= 0 ->
  case from_ch(Ai) of
    error -> {error, {invalid_character, Ai}};
    Bi -> from_string(A, I-1, setelement(1+I, B, Bi))
  end;
from_string([_ | _], _I, _B) ->
  {error, symbol_too_long};
from_string([], _I, B) ->
  N1 = (element(1+4, B) * ?RADIX + element(1+3, B)) * ?RADIX + element(1+2, B),
  N2 = element(1+1, B) * ?RADIX + element(1+0, B),
  {ok, {N1, N2 bsl 5}}. % upshift N2 to make room for 5-bit symbol type

-spec to_string(word(), word()) -> {ok, string()} | {error, any()}.
to_string(N1, N2) ->
  N2b = N2 bsr 5,
  B4 = N2b rem ?RADIX,
  B3 = N2b div ?RADIX,
  B2 = N1 rem ?RADIX,
  N1b = N1 div ?RADIX,
  B1 = N1b rem ?RADIX,
  B0 = N1b div ?RADIX,
  to_string_2([B4, B3, B2, B1, B0]).

%% strip trailing padding NULs
to_string_2([0 | Bs]) -> to_string_2(Bs);
to_string_2(Bs) -> to_string_3(Bs, []).

%% decode payload and reverse
to_string_3([], As) ->
  {ok, As};
to_string_3([B | Bs], As) ->
  case to_ch(B) of
    error -> {error, invalid_encoding};
    A -> to_string_3(Bs, [A | As])
  end.

%% Internal ====================================================================

-spec from_ch(char()) -> byte() | error.
from_ch(C) ->
  if C >= $0, C =< $9 -> 8#1 + (C - $0);
     C >= $A, C =< $Z -> 8#13 + (C - $A);
     C >= $a, C =< $z -> 8#13 + (C - $a);
     C =:= $. -> 8#45;
     C =:= $? -> 8#46;
     true -> error
  end.

-spec to_ch(byte()) -> char() | error.
to_ch(B) ->
  if B >= 8#47 -> error;
     B =:= 8#46 -> $?;
     B =:= 8#45 -> $.;
     B >= 8#13 -> $A + (B - 8#13);
     B >= 8#1 -> $0 + (B - 8#1);
     true -> error
  end.

%% Unit tests ==================================================================

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

roundtrip_test() ->
  {ok, {N1a, N2a}} = from_string("FIE42"),
  ?assertEqual({ok, "FIE42"}, to_string(N1a, N2a)),
  {ok, {N1b, N2b}} = from_string("FIE"),
  ?assertEqual({ok, "FIE"}, to_string(N1b, N2b)).

errors_test() ->
  ?assertEqual({error, symbol_too_long}, from_string("QWERTY")),
  ?assertEqual({error, {invalid_character, $-}}, from_string("A-B")),
  ?assertEqual({error, invalid_encoding}, to_string(0, ?RADIX bsl 5)).

title_test() ->
  %% 093-000080-05, page A-13, TITLE block containing encoded "ROOT"
  W6 = 8#000663,
  W7 = 8#012226,
  %% all words in the .RB file are byte-swapped
  N1 = bswap(W6),
  N2 = bswap(W7),
  %%
  ?assertEqual({ok, "ROOT"}, to_string(N1, N2)).

bswap(W) ->
  ((W bsr 8) band 255) bor ((W band 255) bsl 8).

-endif.
