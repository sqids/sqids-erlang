-module(sqids).

-export([
        new/0
      , new/1
      , default_options/0
      , encode/2
      , decode/2
    ]).

-export_type([
        options/0
      , str/0
      , blocklist/0
      , sqids/0
    ]).

% String representations supported in this module is/are:
%   - non multibyte binary format
-type str() :: unicode:latin1_binary().

-type char_() :: <<_:8>>.

-type blocklist() :: lists:list(str()).

-type options() :: #{
        alphabet   => str()
      , min_length => non_neg_integer()
      , blocklist  => blocklist()
    }.

-opaque sqids() :: #{
       '?MODULE'   := ?MODULE
      , alphabet   := str()
      , min_length := non_neg_integer()
      , blocklist  := blocklist()
      , n          := non_neg_integer()
    }.

-spec default_options() -> options().
default_options() ->
    #{  alphabet   =>
            <<"abcdefghijklmnopqrstuvwxyz",
              "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
              "0123456789">>
      , min_length => 0
      , blocklist  => sqids_blocklist:get()
    }.

-spec new() -> sqids().
new() ->
    new(#{}).

-spec new(options()) -> sqids().
new(Options0) when is_map(Options0)->
    Options = maps:merge(default_options(), Options0),
    case Options of
        #{  alphabet := Alphabet
          , min_length := MinLength
          , blocklist := Blocklist
        } when is_binary(Alphabet)
             , is_integer(MinLength)
             , MinLength >= 0 % no upper limit.
             , is_list(Blocklist) -> ok;
        _ ->
            erlang:error(badarg, [Options0])
    end,
    BinAlphabet = maps:get(alphabet, Options),
    ListAlphabet = binary_to_list(BinAlphabet),
    Is7Bit = unicode:bin_is_7bit(BinAlphabet),
    case {size(BinAlphabet), Is7Bit} of
        {Size, true} when Size < 3 ->
            Reason0 = 'Alphabet length must be at least 3',
            erlang:error(Reason0, [Options0]);
        {_Size, true} ->
            ok;
        _ ->
            Reason0 = 'Alphabet cannot contain multibyte characters',
            erlang:error(Reason0, [Options0])
    end,
    SetsOpt = [{version, 2}],
    SetAlphabet = sets:from_list(ListAlphabet, SetsOpt),
    case {size(BinAlphabet), sets:size(SetAlphabet)} of
        {SetSize, SetSize} ->
            ok;
        _ ->
            Reason1 = 'Alphabet must contain unique characters',
            erlang:error(Reason1, [Options0])
    end,
    AlphabetLowercased = string:casefold(BinAlphabet),
    AlphabetCharSet = str_to_char_set(AlphabetLowercased),
    FilteredBlocklist = lists:filtermap(fun
        (Word) when size(Word) >= 3->
            WordLowercased = string:casefold(Word),
            WordChars = str_to_char_list(WordLowercased),
            try
                lists:foreach(fun
                    (C) ->
                        case sets:is_element(C, AlphabetCharSet) of
                            true -> ok;
                            false -> throw({?MODULE, break})
                        end
                    end, WordChars)
            of
                ok ->
                    {true, WordLowercased}
            catch
                throw:{?MODULE, break} ->
                    false
            end;
        (_) ->
            false
        end, maps:get(blocklist, Options)),
    UniqueFilteredBlocklist = sets:to_list(
            sets:from_list(FilteredBlocklist, SetsOpt)
        ),
    #{ '?MODULE'   => ?MODULE
      , alphabet   => shuffle(BinAlphabet)
      , min_length => maps:get(min_length, Options)
      , blocklist  => UniqueFilteredBlocklist
      , n          => size(BinAlphabet)
    } ;
new(Options0) ->
    erlang:error(badarg, [Options0]).


-spec encode([non_neg_integer()], sqids()) -> str().
encode([], #{'?MODULE':=?MODULE}) ->
    <<>>;
encode(Numbers, Sqids=#{'?MODULE':=?MODULE}) ->
    lists:foreach(fun
        (Num) when is_integer(Num) andalso Num >= 0 ->
            ok;
        (_) ->
            erlang:error(badarg, [Numbers, Sqids])
        end, Numbers),
    encode_numbers(Numbers, 0, Sqids);
encode(Arg1, Arg2) ->
    erlang:error(badarg, [Arg1, Arg2]).

-spec encode_numbers(
        [non_neg_integer()], non_neg_integer(), sqids()
    ) -> str().
encode_numbers(Num, Inc, #{n:=N}=Sqids) when Inc > N ->
    Reason = 'Reached max attempts to re-generate the ID',
    erlang:error(Reason, [Num, Inc, Sqids]);
encode_numbers(Numbers, Increment, Sqids) ->
    This = fun(Key) -> maps:get(Key, Sqids) end,
    {_i, Offset0} = lists:foldl(fun(V, {I, A})->
            Next = binary:at(This(alphabet), V rem This(n)) + I + A,
            {I+1, Next}
        end, {0, length(Numbers)}, Numbers),
    Offset1 = Offset0 rem This(n),
    Offset = (Offset1 + Increment) rem This(n),
    <<SliceLeft:Offset/binary, SliceRight/binary>> = This(alphabet),
    Alphabet0 = <<SliceRight/binary, SliceLeft/binary>>,
    Prefix = binary:at(Alphabet0, 0),
    Alphabet1 = list_to_binary(lists:reverse(binary_to_list(Alphabet0))),
    {RevCharList0, Alphabet2} = encode_input_array(
            Numbers, [Prefix], Alphabet1
        ),
    Id = case This(min_length) of
        MinLength when MinLength > length(RevCharList0) ->
            Separator = binary:at(Alphabet2, 0),
            RevCharList1 = [Separator|RevCharList0],
            Id0 = list_to_binary(lists:reverse(RevCharList1)),
            id_padding(Id0, This(min_length), Alphabet2);
        _ ->
            list_to_binary(lists:reverse(RevCharList0))
    end,
    case is_blocked_id(Id, This(blocklist)) of
        false ->
            Id;
        _ ->
            encode_numbers(Numbers, Increment+1, Sqids)
    end.

-spec encode_input_array(
        [non_neg_integer(), ...], [non_neg_integer(), ...], str()
    ) -> {[char_(), ...], str()}.
encode_input_array([Num], Id0, Alphabet) ->
    <<_:1/binary, AlphabetWithoutSeparator/binary>> = Alphabet,
    Id1 = [to_id(Num, AlphabetWithoutSeparator)|Id0],
    {Id1, Alphabet};
encode_input_array([Num|Numbers], Id0, Alphabet) ->
    <<Separator:1/binary, AlphabetWithoutSeparator/binary>> = Alphabet,
    Id1 = [to_id(Num, AlphabetWithoutSeparator)|Id0],
    Id2 = [Separator|Id1],
    encode_input_array(Numbers, Id2, shuffle(Alphabet)).

id_padding(Id0, MinLength, Alphabet0) when MinLength - size(Id0) > 0 ->
    Alphabet = shuffle(Alphabet0),
    Size = min(MinLength-size(Id0), size(Alphabet)),
    <<Padding:Size/binary, _/binary>> = Alphabet,
    Id = <<Id0/binary, Padding/binary>>,
    id_padding(Id, MinLength, Alphabet);
id_padding(Id, _MinLength, _Alphabet) ->
    Id.

-spec decode(str(), sqids()) -> [non_neg_integer()].
decode(<<>>, #{'?MODULE':=?MODULE}) ->
    [];
decode(Id, Sqids=#{'?MODULE':=?MODULE}) when is_binary(Id) ->
    try
        decode_(Id, Sqids)
    of
        Ret -> Ret
    catch
        {?MODULE, return, Ret} -> Ret
    end;
decode(Arg1, Arg2) ->
    erlang:error(badarg, [Arg1, Arg2]).

-spec decode_(str(), sqids()) -> [non_neg_integer()].
decode_(Id0, Sqids) ->
    This = fun(Key) -> maps:get(Key, Sqids) end,
    lists:foreach(fun(C) ->
            case sets:is_element(C, str_to_char_set(This(alphabet))) of
                true -> ok;
                _ -> throw({?MODULE, return, []})
            end
        end, str_to_char_list(Id0)),
    <<Prefix:1/binary, Id1/binary>> = Id0,
    {Offset, _} = binary:match(This(alphabet), Prefix),
    <<SliceLeft:Offset/binary, SliceRight/binary>> = This(alphabet),
    Alphabet0 = <<SliceRight/binary, SliceLeft/binary>>,
    Alphabet1 = list_to_binary(lists:reverse(binary_to_list(Alphabet0))),
    decode_([Id1], Alphabet1, []).

-spec decode_(MaybeId, str(), sqids()) -> [non_neg_integer()]
    when MaybeId :: nil() | nonempty_list(str()). % [] or [<<...>>]
decode_([], _, Ret) ->
    lists:reverse(Ret);
decode_([Id0], Alphabet0, Ret0) ->
    <<Separator:1/binary, AlphabetWithoutSeparator/binary>> = Alphabet0,
    Chunks = binary:split(Id0, Separator),
    case Chunks of
        [<<>>|_] ->
            lists:reverse(Ret0);
        [Chunk|Id1] ->
            Ret1 = [to_number(Chunk, AlphabetWithoutSeparator)|Ret0],
            Alphabet1 = case length(Chunks) of
                ChunksLength when ChunksLength > 1 ->
                    shuffle(Alphabet0);
                _ ->
                    Alphabet0
            end,
            decode_(Id1, Alphabet1, Ret1)
    end.

-spec shuffle(str()) -> str().
shuffle(Alphabet) ->
    shuffle_(0, size(Alphabet)-1, Alphabet).

-spec shuffle_(non_neg_integer(), non_neg_integer(), str()) -> str().
shuffle_(_, 0, Alphabet) ->
    Alphabet;
shuffle_(I, J, Alphabet) ->
    N = size(Alphabet),
    R = (I * J + binary:at(Alphabet, I) + binary:at(Alphabet, J)) rem N,
    shuffle_(I+1, J-1, swap(I, R, Alphabet)).

-spec swap(non_neg_integer(), non_neg_integer(), str()) -> str().
swap(X, X, BinaryString) ->
    BinaryString;
swap(Y, X, BinaryString) when Y > X ->
    swap(X, Y, BinaryString);
swap(X, Y, BinaryString) when (
        is_integer(X) andalso X >= 0 andalso
        is_integer(Y) andalso Y >= 0 andalso
        is_binary(BinaryString)
    ) ->
    PrefixSize   = X,
    LeftSize     = 1,
    InterfixSize = Y - X -1,
    RightSize    = 1,
    <<  Prefix:PrefixSize/binary
      , Left:LeftSize/binary
      , Interfix:InterfixSize/binary
      , Right:RightSize/binary
      , Suffix/binary
    >> = BinaryString,
    <<  Prefix/binary
      , Right/binary
      , Interfix/binary
      , Left/binary
      , Suffix/binary
    >>;
swap(Arg1, Arg2, Arg3) ->
    erlang:error(badarg, [Arg1, Arg2, Arg3]).

-spec to_id(non_neg_integer(), str()) -> char_().
to_id(Num, Alphabet) ->
    to_id_(Num, Alphabet, <<>>).

-spec to_id_(non_neg_integer(), str(), str()) -> str().
to_id_(0, _, Id) when  size(Id) > 0 ->
    Id;
to_id_(Num0, Alphabet, Id0) ->
    Char = binary:at(Alphabet, Num0 rem size(Alphabet)),
    Id1 = <<Char/integer, Id0/binary>>,
    Num1 = Num0 div size(Alphabet),
    to_id_(Num1, Alphabet, Id1).

-spec to_number(str(), str()) -> non_neg_integer().
to_number(Id, Alphabet) ->
    lists:foldl(fun(V, A) ->
            {Index, _} = binary:match(Alphabet, <<V/integer>>),
            A * size(Alphabet) + Index
        end, 0, binary_to_list(Id)).

-spec is_blocked_id(str(), blocklist()) -> boolean().
is_blocked_id(Id0, Blocklist) ->
    Id = string:casefold(Id0),
    try
        lists:foreach(fun(Word) ->
                case is_blocked_id_(Word, Id) of
                    true -> throw({?MODULE, return, true});
                    false -> ok
                end
            end, Blocklist)
    of
        _ ->
            false
    catch
        throw:{?MODULE, return, true} ->
            true
    end.

-spec is_blocked_id_(Word::str(), Id::str()) -> boolean().
is_blocked_id_(Word, Id) when size(Word) =< 3 orelse size(Id) =< 3 ->
    (Word =:= Id);
is_blocked_id_(Word, Id) ->
    case re:run(Word, <<"\\d">>) of
        nomatch ->
            case binary:match(Id, Word) of
                nomatch -> false;
                _ -> true
            end;
        _ ->
            RevWord = reverse_str(Word),
            W = size(Word),
            case {Id, reverse_str(Id)} of
                {<<Word:W/binary, _/binary>>, _} -> true;
                {_, <<RevWord:W/binary, _/binary>>} -> true;
                _ -> false
            end
    end.

-spec str_to_char_set(str()) -> sets:set(char_()).
str_to_char_set(Str) ->
    List = str_to_char_list(Str),
    sets:from_list(List, [{version, 2}]).

-spec str_to_char_list(str()) -> lists:list(char_()).
str_to_char_list(Str) ->
    lists:map(fun(Char) ->
            <<Char/integer>>
        end, binary_to_list(Str)).

-spec reverse_str(str()) -> str().
reverse_str(Str) ->
    reverse_str_(Str, <<>>).

-spec reverse_str_(str(), str()) -> str().
reverse_str_(<<>>, Str) -> Str;
reverse_str_(<<Head:1/binary, Tail/binary>>, Str) ->
    reverse_str_(Tail, <<Head/binary, Str/binary>>).

