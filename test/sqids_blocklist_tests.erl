-module(sqids_blocklist_tests).
-include_lib("eunit/include/eunit.hrl").
-import(sqids, [new/0, new/1, encode/2, decode/2]).

-compile([export_all]).

'if no custom blocklist param, use the default blocklist _test' () ->
    Sqids = new(),
    ?assertEqual([4572721], decode(<<"aho1e">>, Sqids)),
    ?assertEqual(<<"JExTR">>, encode([4572721], Sqids)),
    ok.

'if an empty blocklist param passed, don\'t use any blocklist _test' () ->
    Sqids = new(#{
            blocklist => []
        }),
    ?assertEqual([4572721], decode(<<"aho1e">>, Sqids)),
    ?assertEqual(<<"aho1e">>, encode([4572721], Sqids)),
    ok.

'if a non-empty blocklist param passed, use only that _test' () ->
    Sqids = new(#{
            blocklist => [
                    <<"ArUO">> % originally encoded [100000]
                ]
        }),
    
    % make sure we don't use the default blocklist
    ?assertEqual([4572721], decode(<<"aho1e">>, Sqids)),
    ?assertEqual(<<"aho1e">>, encode([4572721], Sqids)),
    
    % make sure we are using the passed blocklist
    ?assertEqual([100000], decode(<<"ArUO">>, Sqids)),
    ?assertEqual(<<"QyG4">>, encode([100000], Sqids)),
    ?assertEqual([100000], decode(<<"QyG4">>, Sqids)),
    ok.

blocklist_test() ->
    Sqids = new(#{
            blocklist => [
                    <<"JSwXFaosAN">> % normal result of 1st encoding, let's block that word on purpose
                  , <<"OCjV9JK64o">> % result of 2nd encoding
                  , <<"rBHf">> % result of 3rd encoding is `4rBHfOiqd3`, let's block a substring
                  , <<"79SM">> % result of 4th encoding is `dyhgw479SM`, let's block the postfix
                  , <<"7tE6">> % result of 4th encoding is `7tE6jdAHLe`, let's block the prefix
                ]
        }),
    ?assertEqual(<<"1aYeB7bRUt">>, encode([1_000_000, 2_000_000], Sqids)),
    ?assertEqual([1_000_000, 2_000_000], decode(<<"1aYeB7bRUt">>, Sqids)),
    ok.

'decoding blocklist words should still work _test' () ->
    Sqids = new(#{
            blocklist => [
                    <<"86Rf07">>
                  , <<"se8ojk">>
                  , <<"ARsz1p">>
                  , <<"Q8AI49">>
                  , <<"5sQRZO">>
                ]
        }),
    ?assertEqual([1, 2, 3], decode(<<"86Rf07">>, Sqids)),
    ?assertEqual([1, 2, 3], decode(<<"se8ojk">>, Sqids)),
    ?assertEqual([1, 2, 3], decode(<<"ARsz1p">>, Sqids)),
    ?assertEqual([1, 2, 3], decode(<<"Q8AI49">>, Sqids)),
    ?assertEqual([1, 2, 3], decode(<<"5sQRZO">>, Sqids)),
    ok.

'match against a short blocklist word _test' () ->
    Sqids = new(#{
            blocklist => [<<"pnd">>]
        }),
    ?assertEqual([1000], decode(encode([1000], Sqids), Sqids)),
    ok.

'blocklist filtering in constructor _test' () ->
    Sqids = new(#{
            alphabet  => <<"ABCDEFGHIJKLMNOPQRSTUVWXYZ">>
          % lowercase blocklist in only-uppercase alphabet
          , blocklist => [<<"sxnzkl">>]
        }),
    Id = encode([1, 2, 3], Sqids),
    Numbers = decode(Id, Sqids),
    % without blocklist, would've been "SXNZKL"
    ?assertEqual(<<"IBSHOZ">>, Id), 
    ?assertEqual([1, 2, 3], Numbers),
    ok.

'max encoding attempts _test' () ->
    Alphabet  = <<"abc">>,
    MinLength = 3,
    Blocklist = [<<"cab">>, <<"abc">>, <<"bca">>],
    Sqids = new(#{
            alphabet   => Alphabet
          , min_length => MinLength
          , blocklist  => Blocklist
        }),
    ?assertEqual(MinLength, size(Alphabet)),
    ?assertEqual(MinLength, length(Blocklist)),
    Reason = 'Reached max attempts to re-generate the ID',
    ?assertError(Reason, encode([0], Sqids)),
    ok.

