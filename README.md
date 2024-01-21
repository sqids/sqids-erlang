# [Sqids Erlang](https://sqids.org/erlang)

Sqids (pronounced "squids") is a small library that lets you generate YouTube-looking IDs from numbers. It's good for link shortening, fast & URL-safe ID generation and decoding back into numbers for quicker database lookups.

## Getting started

Add sqids as a dependency to rebar.config
<!-- FIXME: 0.1.0 is unreleased. -->
```erlang
{deps,
    [
        {sqids, {git, "https://github.com/sqids/sqids-erlang.git",  {tag, "0.1.0"}}}
    ]
}.
```
Supported Erlang versions are 
  - Erlang/OTP 24
  - Erlang/OTP 25
  - Erlang/OTP 26

## Examples

```erlang
1> Sqids = sqids:new(),
1> Id = sqids:encode([1, 2, 3], Sqids).
<<"86Rf07">>
2> sqids:decode(Id, Sqids).
[1,2,3]
3> MySqids = sqids:new(#{alphabet=><<"ABC123">>, min_length=>10, blocklist=>[]}),
3> MyId = sqids:encode([0], MySqids).
<<"A13C2B31AC">>
4> sqids:decode(MyId, MySqids).
[0]
```

## License

[MIT](LICENSE)
