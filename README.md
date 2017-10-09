# ByteParsers

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Build Status](https://travis-ci.org/tpapp/ByteParsers.jl.svg?branch=master)](https://travis-ci.org/tpapp/ByteParsers.jl)
[![Coverage Status](https://coveralls.io/repos/tpapp/ByteParsers.jl/badge.svg?branch=master&service=github)](https://coveralls.io/github/tpapp/ByteParsers.jl?branch=master)
[![codecov.io](http://codecov.io/github/tpapp/ByteParsers.jl/coverage.svg?branch=master)](http://codecov.io/github/tpapp/ByteParsers.jl?branch=master)

Parse delimited fields in `Vector{UInt8}` representation of
strings. Benchmarks suggest that this can be significantly faster than
working with `String`s, and most delimiters (eg ';', ',') and parsed
representations (eg digits, decimal dot) don't require working with
`UTF8` in any case.

This library is an experiment which I found useful in my own work. See
the docstrings and the unit tests for usage information.
