# ByteParsers

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Build Status](https://travis-ci.org/tpapp/ByteParsers.jl.svg?branch=master)](https://travis-ci.org/tpapp/ByteParsers.jl)
[![Coverage Status](https://coveralls.io/repos/tpapp/ByteParsers.jl/badge.svg?branch=master&service=github)](https://coveralls.io/github/tpapp/ByteParsers.jl?branch=master)
[![codecov.io](http://codecov.io/github/tpapp/ByteParsers.jl/coverage.svg?branch=master)](http://codecov.io/github/tpapp/ByteParsers.jl?branch=master)

# What

Parse delimited fields in `Vector{UInt8}` representation of
strings. This library is an **experiment** which I found useful in my
own work. *The API may change at any point without notice or
deprecations.* See the docstrings and the unit tests for usage
information.

# Why

Benchmarks suggest that this can be significantly faster than working
with `String`s, and most delimiters (eg ';', ',') and parsed
representations (eg digits, decimal dot) don't require working with
`UTF8` in any case.

# Alternatives

- [TextParse.jl](https://github.com/JuliaComputing/TextParse.jl),
  which is also fast and much more versatile and polished

- `Base.DateFormat` which is now amazingly fast, thanks to the great work in 
[#18000](https://github.com/JuliaLang/julia/pull/18000), [#15888](https://github.com/JuliaLang/julia/issues/15888), [19545](https://github.com/JuliaLang/julia/pull/19545)

# Comparisons

As of Oct 30, 2017, running on my Dell XPS 13 laptop with a `Intel(R) Core(TM) i7-6560U CPU @ 2.20GHz`. [(source)](benchmark/benchmark_comparison.jl)

| parsing                                        | ByteParsers | TextParse |
|:-----------------------------------------------|------------:|----------:|
| `"123456789;"` as `Int`                        |       15 ns |     21 ns |
| `"19800101;"` as `Date`                        |       29 ns |     65 ns |
| `"foobar;"` as a `String` (view or equivalent) |       19 ns |     25 ns |
| `"foobar;"` skipped until separator `;`        |        6 ns |       n/a |
