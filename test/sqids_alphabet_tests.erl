-module(sqids_alphabet_tests).
-include_lib("eunit/include/eunit.hrl").
-import(sqids, [new/0, new/1, encode/2, decode/2]).

-compile([export_all]).

simple_test() ->
    Sqids = new(#{
            alphabet => <<"0123456789abcdef">>
        }),
    Numbers = [1, 2, 3],
    Id = <<"489158">>,
    ?assertEqual(Id, encode(Numbers, Sqids)),
    ?assertEqual(Numbers, decode(Id, Sqids)),
    ok.

short_alphabet_test() ->
    Sqids = new(#{
            alphabet => <<"abc">>
        }),
    Numbers = [1, 2, 3],
    ?assertEqual(Numbers, decode(encode(Numbers, Sqids), Sqids)),
    ok.

long_alphabet_test() ->
    Sqids = new(#{
            alphabet =>
                <<  "abcdefghijklmnopqrstuvwxyz"
                  , "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                  , "0123456789!@#$%^&*()-_+|{}"
                  , "[];:\\'\"/?.>,<`~"
                >>
        }),
    Numbers = [1, 2, 3],
    ?assertEqual(Numbers, decode(encode(Numbers, Sqids), Sqids)),
    ok.

multibyte_characters_test() ->
    Reason = 'Alphabet cannot contain multibyte characters',
    ?assertError(Reason, new(#{
            alphabet => <<"ë1092">>
        })),
    ok.

repeating_alphabet_characters_test() ->
    Reason = 'Alphabet must contain unique characters',
    ?assertError(Reason, new(#{
            alphabet => <<"aabcdefg">>
        })),
    ok.

too_short_of_an_alphabet_test() ->
    Reason = 'Alphabet length must be at least 3',
    ?assertError(Reason, new(#{
            alphabet => <<"ab">>
        })),
    ok.

