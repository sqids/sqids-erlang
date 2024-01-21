-module(sqids_encoding_tests).
-include_lib("eunit/include/eunit.hrl").
-import(sqids, [new/0, new/1, encode/2, decode/2]).

-compile([export_all]).

simple_test() ->
    Sqids = new(),
    Numbers = [1, 2, 3],
    Id = <<"86Rf07">>,
    ?assertEqual(Id, encode(Numbers, Sqids)),
    ?assertEqual(Numbers, decode(Id, Sqids)),
    ok.

different_inputs_test() ->
    Sqids = new(),
    LargeNum = round(1.0e+308) * round(1.0e+308),
    Numbers = [0, 0, 0, 1, 2, 3, 100, 1_000, 100_000, 1_000_000, LargeNum],
    assert(Sqids, Numbers),
    ok.

incremental_numbers_test() ->
    Sqids = new(),
    assert(Sqids, <<"bM">>, [0]),
    assert(Sqids, <<"Uk">>, [1]),
    assert(Sqids, <<"gb">>, [2]),
    assert(Sqids, <<"Ef">>, [3]),
    assert(Sqids, <<"Vq">>, [4]),
    assert(Sqids, <<"uw">>, [5]),
    assert(Sqids, <<"OI">>, [6]),
    assert(Sqids, <<"AX">>, [7]),
    assert(Sqids, <<"p6">>, [8]),
    assert(Sqids, <<"nJ">>, [9]),
    ok.

'incremental numbers, same index 0 _test' () ->
    Sqids = new(),
    assert(Sqids, <<"SvIz">>, [0, 0]),
    assert(Sqids, <<"n3qa">>, [0, 1]),
    assert(Sqids, <<"tryF">>, [0, 2]),
    assert(Sqids, <<"eg6q">>, [0, 3]),
    assert(Sqids, <<"rSCF">>, [0, 4]),
    assert(Sqids, <<"sR8x">>, [0, 5]),
    assert(Sqids, <<"uY2M">>, [0, 6]),
    assert(Sqids, <<"74dI">>, [0, 7]),
    assert(Sqids, <<"30WX">>, [0, 8]),
    assert(Sqids, <<"moxr">>, [0, 9]),
    ok.

'incremental numbers, same index 1 _test' () ->
    Sqids = new(),
    assert(Sqids, <<"SvIz">>, [0, 0]),
    assert(Sqids, <<"nWqP">>, [1, 0]),
    assert(Sqids, <<"tSyw">>, [2, 0]),
    assert(Sqids, <<"eX68">>, [3, 0]),
    assert(Sqids, <<"rxCY">>, [4, 0]),
    assert(Sqids, <<"sV8a">>, [5, 0]),
    assert(Sqids, <<"uf2K">>, [6, 0]),
    assert(Sqids, <<"7Cdk">>, [7, 0]),
    assert(Sqids, <<"3aWP">>, [8, 0]),
    assert(Sqids, <<"m2xn">>, [9, 0]),
    ok.

multi_input_test() ->
    Sqids = new(),
    Numbers = lists:seq(0, 99),
    ?assertEqual(0, lists:nth(1, Numbers)),
    ?assertEqual(99, lists:last(Numbers)),
    assert(Sqids, Numbers),
    ok.

'encoding no numbers _test' () ->
    Sqids = new(),
    ?assertEqual(<<"">>, encode([], Sqids)),
    ok.

'decoding empty string _test' () ->
    Sqids = new(),
    ?assertEqual([], decode(<<"*">>, Sqids)),
    ok.

'decoding an ID with an invalid character _test' () ->
    Sqids = new(),
    ?assertEqual([], decode(<<"*">>, Sqids)),
    ok.


%%% internal functions %%%
assert(Arg1, Sqids) when is_map(Sqids) ->
    assert(Sqids, Arg1);
assert(Sqids, Numbers) when is_list(Numbers) ->
    ?assertEqual(Numbers, decode(encode(Numbers, Sqids), Sqids)),
    ok;
assert(Sqids, Id) when is_binary(Id) ->
    ?assertEqual(Id, encode(decode(Id, Sqids), Sqids)),
    ok.

assert(Arg1, Sqids, Arg2) when is_map(Sqids) ->
    assert(Sqids, Arg1, Arg2);
assert(Arg1, Arg2, Sqids) when is_map(Sqids) ->
    assert(Sqids, Arg1, Arg2);
assert(Sqids, Numbers, Id) when is_list(Numbers), is_binary(Id) ->
    assert(Sqids, Id, Numbers);
assert(Sqids, Id, Numbers) ->
    ?assertEqual(Numbers, decode(Id, Sqids)),
    ?assertEqual(Id, encode(Numbers, Sqids)),
    ok.

assert_functions_test() ->
    Sqids = new(),
    Numbers = [1, 2, 3],
    Id = <<"86Rf07">>,
    % assert function args can be in any order.
    assert(Sqids, Numbers),
    assert(Sqids, Id),
    assert(Numbers, Sqids),
    assert(Id, Sqids),
    assert(Sqids, Id, Numbers),
    assert(Sqids, Numbers, Id),
    assert(Id, Sqids, Numbers),
    assert(Numbers, Sqids, Id),
    assert(Id, Numbers, Sqids),
    assert(Numbers, Id, Sqids),
    ok.

