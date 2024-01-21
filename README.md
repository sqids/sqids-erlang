# [Sqids Erlang](https://sqids.org/erlang)

[Sqids](https://sqids.org/erlang) (*pronounced "squids"*) is a small library that lets you **generate unique IDs from numbers**. It's good for link shortening, fast & URL-safe ID generation and decoding back into numbers for quicker database lookups.

Features:

- **Encode multiple numbers** - generate short IDs from one or several non-negative numbers
- **Quick decoding** - easily decode IDs back into numbers
- **Unique IDs** - generate unique IDs by shuffling the alphabet once
- **ID padding** - provide minimum length to make IDs more uniform
- **URL safe** - auto-generated IDs do not contain common profanity
- **Randomized output** - Sequential input provides nonconsecutive IDs
- **Many implementations** - Support for [40+ programming languages](https://sqids.org/)

## 🧰 Use-cases

Good for:

- Generating IDs for public URLs (eg: link shortening)
- Generating IDs for internal systems (eg: event tracking)
- Decoding for quicker database lookups (eg: by primary keys)

Not good for:

- Sensitive data (this is not an encryption library)
- User IDs (can be decoded revealing user count)

## 🚀 Getting started

Add sqids as a dependency to rebar.config
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

## 👩‍💻 Examples

Simple encode & decode:

```erlang
1> Sqids = sqids:new(),
1> Id = sqids:encode([1, 2, 3], Sqids). % "86Rf07"
2> Numbers = sqids:decode(Id, Sqids). % [1,2,3]
```

> **Note**
> 🚧 Because of the algorithm's design, **multiple IDs can decode back into the same sequence of numbers**. If it's important to your design that IDs are canonical, you have to manually re-encode decoded numbers and check that the generated ID matches.

Enforce a *minimum* length for IDs:

```erlang
1> Sqids = new(#{ min_length=>10 }),
1> Id = sqids:encode([1, 2, 3], Sqids). % "86Rf07xd4z"
2> Numbers = sqids:decode(Id, Sqids). % [1,2,3]
```

Randomize IDs by providing a custom alphabet:

```erlang
1> Sqids = new(#{ alphabet=><<"k3G7QAe51FCsPW92uEOyq4Bg6Sp8YzVTmnU0liwDdHXLajZrfxNhobJIRcMvKt">> }),
1> Id = sqids:encode([1, 2, 3], Sqids). % "XRKUdQ"
2> Numbers = sqids:decode(Id, Sqids). % [1,2,3]
```

Prevent specific words from appearing anywhere in the auto-generated IDs:

```erlang
1> Sqids = new(#{ blocklist => [ <<"86Rf07">> ] }),
1> Id = sqids:encode([1, 2, 3], Sqids). % "se8ojk"
2> Numbers = sqids:decode(Id, Sqids). % [1,2,3]
```

## License

[MIT](LICENSE)
