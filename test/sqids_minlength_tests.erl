-module(sqids_minlength_tests).
-include_lib("eunit/include/eunit.hrl").
-import(sqids, [new/0, new/1, encode/2, decode/2]).

-compile([export_all]).

simple_test() ->
    Sqids = new(#{
            min_length => size(default_options(alphabet))
        }),
    Numbers = [1, 2, 3],
    Id = <<"86Rf07xd4zBmiJXQG6otHEbew02c3PWsUOLZxADhCpKj7aVFv9I8RquYrNlSTM">>,
    ?assertEqual(Id, encode(Numbers, Sqids)),
    ?assertEqual(Numbers, decode(Id, Sqids)),
    ok.

incremental_test() ->
    Numbers = [1, 2, 3],
    Size = size(default_options(alphabet)),
    Map = #{
            6 => <<"86Rf07">>
          , 7 => <<"86Rf07x">>
          , 8 => <<"86Rf07xd">>
          , 9 => <<"86Rf07xd4">>
          , 10 => <<"86Rf07xd4z">>
          , 11 => <<"86Rf07xd4zB">>
          , 12 => <<"86Rf07xd4zBm">>
          , 13 => <<"86Rf07xd4zBmi">>
          , Size + 0 => <<"86Rf07xd4zBmiJXQG6otHEbew02c3PWsUOLZxADhCpKj7aVFv9I8RquYrNlSTM">>
          , Size + 1 => <<"86Rf07xd4zBmiJXQG6otHEbew02c3PWsUOLZxADhCpKj7aVFv9I8RquYrNlSTMy">>
          , Size + 2 => <<"86Rf07xd4zBmiJXQG6otHEbew02c3PWsUOLZxADhCpKj7aVFv9I8RquYrNlSTMyf">>
          , Size + 3 => <<"86Rf07xd4zBmiJXQG6otHEbew02c3PWsUOLZxADhCpKj7aVFv9I8RquYrNlSTMyf1">>
        },
    maps:foreach(fun(MinLength, Id) ->
            Sqids = new(#{min_length=>MinLength}),
            ?assertEqual(Id, encode(Numbers, Sqids)),
            ?assertEqual(MinLength, size(encode(Numbers, Sqids))),
            ?assertEqual(Numbers, decode(Id, Sqids))
        end, Map),
    ok.

incremental_numbers_test() ->
    Sqids = new(#{
            min_length => size(default_options(alphabet))
        }),
    Ids = #{
            <<"SvIzsqYMyQwI3GWgJAe17URxX8V924Co0DaTZLtFjHriEn5bPhcSkfmvOslpBu">> => [0, 0]
          , <<"n3qafPOLKdfHpuNw3M61r95svbeJGk7aAEgYn4WlSjXURmF8IDqZBy0CT2VxQc">> => [0, 1]
          , <<"tryFJbWcFMiYPg8sASm51uIV93GXTnvRzyfLleh06CpodJD42B7OraKtkQNxUZ">> => [0, 2]
          , <<"eg6ql0A3XmvPoCzMlB6DraNGcWSIy5VR8iYup2Qk4tjZFKe1hbwfgHdUTsnLqE">> => [0, 3]
          , <<"rSCFlp0rB2inEljaRdxKt7FkIbODSf8wYgTsZM1HL9JzN35cyoqueUvVWCm4hX">> => [0, 4]
          , <<"sR8xjC8WQkOwo74PnglH1YFdTI0eaf56RGVSitzbjuZ3shNUXBrqLxEJyAmKv2">> => [0, 5]
          , <<"uY2MYFqCLpgx5XQcjdtZK286AwWV7IBGEfuS9yTmbJvkzoUPeYRHr4iDs3naN0">> => [0, 6]
          , <<"74dID7X28VLQhBlnGmjZrec5wTA1fqpWtK4YkaoEIM9SRNiC3gUJH0OFvsPDdy">> => [0, 7]
          , <<"30WXpesPhgKiEI5RHTY7xbB1GnytJvXOl2p0AcUjdF6waZDo9Qk8VLzMuWrqCS">> => [0, 8]
          , <<"moxr3HqLAK0GsTND6jowfZz3SUx7cQ8aC54Pl1RbIvFXmEJuBMYVeW9yrdOtin">> => [0, 9]
        },
    maps:foreach(fun(Id, Numbers) ->
            ?assertEqual(Id, encode(Numbers, Sqids)),
            ?assertEqual(Numbers, decode(Id, Sqids))
        end, Ids),
    ok.

min_lengths_test() ->
    LargeNum = round(1.0e+308) * round(1.0e+308),
    MinLengthList = [0, 1, 5, 10, size(default_options(alphabet))],
    NumbersList = [
            [0]
          , [0, 0, 0, 0, 0]
          , [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
          , [100, 200, 300]
          , [1_000, 2_000, 3_000]
          , [1_000_000]
          , [LargeNum]
        ],
    lists:foreach(fun(MinLength)->lists:foreach(fun(Numbers)->
            Sqids = new(#{min_length=>MinLength}),
            Id = encode(Numbers, Sqids),
            ?assert(size(Id) >= MinLength),
            ?assertEqual(Numbers, decode(Id, Sqids))
        end, NumbersList)end, MinLengthList),
    ok.

default_options(Key) ->
    maps:get(Key, sqids:default_options()).

