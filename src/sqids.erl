-module(sqids).

-export([
        new/0
      , new/1
      , default_options/0
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

-type blocklist() :: sets:set(str()).

-type options() :: #{
        alphabet   => str()
      , min_length => non_neg_integer()
      , blocklist  => blocklist()
    }.

-opaque sqids() :: #{
        alphabet   := str()
      , min_length := non_neg_integer()
      , blocklist  := blocklist()
    }.

-spec default_options() -> options().
default_options() ->
    #{  alphabet   =>
            <<"abcdefghijklmnopqrstuvwxyz",
              "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
              "0123456789">>
      , min_length => 0
      , blocklist  => sqids_blocklist:get()
    } .

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
        ->
            case {sets:is_set(Blocklist), is_map(Blocklist)} of
                {true, true} -> ok;
                _ ->
                    erlang:error(badarg, [Options0])
            end;
        _ ->
            erlang:error(badarg, [Options0])
    end,
    BinAlphabet = maps:get(alphabet, Options),
    ListAlphabet = unicode:characters_to_list(BinAlphabet),
    case {size(BinAlphabet), length(ListAlphabet)} of
        {Size, Size} when Size < 3 ->
            Reason0 = 'Alphabet length must be at least 3',
            erlang:error(Reason0, [Options0]);
        {Size, Size} ->
            ok;
        _ ->
            Reason0 = 'Alphabet cannot contain multibyte characters',
            erlang:error(Reason0, [Options0])
    end,
    SetAlphabet = sets:from_list(ListAlphabet, [{version, 2}]),
    case {size(BinAlphabet), sets:size(SetAlphabet)} of
        {SetSize, SetSize} ->
            ok;
        _ ->
            Reason1 = 'Alphabet must contain unique characters',
            erlang:error(Reason1, [Options0])
    end,
    AlphabetLowercased = string:casefold(BinAlphabet),
    AlphabetCharSet = str_to_char_set(AlphabetLowercased),
    FilteredBlocklist = maps:fold(fun
        (Word, [], Acc) when size(Word) >= 3->
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
                    Acc#{WordLowercased => []}
            catch
                throw:{?MODULE, break} ->
                    Acc
            end;
        (_, [], Acc) ->
            Acc;
        (_, _, _) ->
            erlang:error(badarg, [Options0])
        end, #{}, maps:get(blocklist, Options)),
    #{  alphabet   => shuffle(BinAlphabet)
      , min_length => maps:get(min_length, Options)
      , blocklist  => FilteredBlocklist
    } ;
new(Options0) ->
    erlang:error(badarg, [Options0]).


-spec shuffle(str()) -> str().
shuffle(Alphabet) ->
    % TODO
    Alphabet.

-spec str_to_char_set(str()) -> sets:set(char_()).
str_to_char_set(Str) ->
    List = str_to_char_list(Str),
    sets:from_list(List, [{version, 2}]).

-spec str_to_char_list(str()) -> lists:list(char_()).
str_to_char_list(Str) ->
    lists:map(fun(Char) ->
            <<Char/integer>>
        end, binary_to_list(Str)).

