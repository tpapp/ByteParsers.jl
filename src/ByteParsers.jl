__precompile__()
module ByteParsers

using Base.Dates: daysinmonth, UTD, totaldays

import Base: getindex, isequal, length, show, unsafe_get

using ArgCheck
using DocStringExtensions
using MacroTools
using Parameters: @unpack

export
    # generic
    parsedtype,
    parsenext,
    MaybeParsed,
    isparsed,
    getpos,
    # parsers
    PosInteger,
    PosFixedInteger,
    DateYYYYMMDD,
    Skip,
    ViewBytes,
    Line


# generic interface

const ByteVector = Vector{UInt8}

"""
A parser that parses to `MaybeParsed{T}` values.

Supports [`parsedtype`](@ref) and [`parsenext`](@ref) methods.
"""
abstract type AbstractParser{T} end

"""
    parsedtype(parser)

Return type of a parser, see [`parsenext`](@ref).

See [`Line`](@ref) for the semantics of skipping types which parse into `Void`.
"""
parsedtype(::AbstractParser{T}) where {T} = T

"""
    parsenext(parser, str, pos, sep)::MaybeParsed{parsedtype(parser)}

Parse `str` starting at `pos` using `parser`. Variable-length fields are parsed
until terminated by `sep`. Return a [`MaybeParsed`](@ref) object, the position
is that of the *next* character after parsing.
"""
function parsenext end

function parsenext(parser, str::AbstractString, pos, sep::Char)
    @argcheck Int(sep) < 0x80
    parsenext(parser, convert(ByteVector, str), pos, UInt8(sep))
end

"""
    MaybeParsed(pos, [value])

When `pos > 0`, it is the position of the character after parsing, and `value`
holds the value. When `pos < 0`, it wraps the error code (see
[`pos_to_error`](@ref)).
"""
struct MaybeParsed{T}
    pos::Int
    value::T
    MaybeParsed{T}(pos) where {T} = new{T}(pos)
    MaybeParsed{T}(pos, value::T) where {T} = new{T}(pos, value)
    MaybeParsed(pos, value::T) where {T} = new{T}(pos, value)
end

@inline unsafe_get(x::MaybeParsed) = x.value

"""
    $SIGNATURES

When [`isparsed(x)`](@ref), return the position of the *next* byte after parsing
the value that was returned as `x`.

Otherwise, return the position of the byte where parsing failed (may be outside
the length of the string for end of line errors).
"""
@inline getpos(x::MaybeParsed) = abs(x.pos)

function isequal(x::MaybeParsed{Tx}, y::MaybeParsed{Ty}) where {Tx,Ty}
    if isbits(Tx) && isbits(Ty)
        (x.pos == y.pos) & ifelse(x.pos > 0, isequal(x.value, y.value), true)
    else
        x.pos == y.pos && (x.pos > 0 ? isequal(x.value, y.value) : true)
    end
end

"""
    $SIGNATURES

Convert a position to an error code.
"""
@inline pos_to_error(pos::Int) = -pos

"""
    $SIGNATURES

Test if the object has been parsed (otherwise, it contains an error).
"""
@inline isparsed(x::MaybeParsed) = x.pos > 0

"""
    pos, value = @checkpos x [label]

Macro for checking a [`MaybeParsed`](@ref) object (eg returned by
[`parsenext`](@ref). If there is an error, `@goto label` is invoked, otherwise
the position and the parsed value are extracted into `pos` and `value`.
"""
macro checkpos(ex, label = :error)
    @capture ex (pos_, value_) = rhs_
    quote
        maybeparsed = $(esc(rhs))
        $(esc(pos)) = maybeparsed.pos
        if $(esc(pos)) < 0
            @goto $label
        else
            $(esc(value)) = maybeparsed.value
        end
    end
end


# integer parsing

struct PosInteger{T <: Integer, S <: Integer} <: AbstractParser{T} end

"""
    $SIGNATURES

Parse a positive integer until a delimiter as the first type. The second type is
used for the internal accumulation of digits, which is *not checked for
overflows*, but conversions to `T` are.
"""
function PosInteger(T::Type{<:Integer} = Int, S::Type{<:Integer} = Int64)
    if !(promote_type(T, S) ≡ S)
        warn("Using narrower type for parsing than return value.")
    end
    PosInteger{T, S}()
end

show(io::IO, parser::PosInteger{T, S}) where {T, S} =
     print(io, "Parse positive integer into $T (accumulating in $S)")

function parsenext(parser::PosInteger{T,S}, str::ByteVector, start::Int,
                   sep::UInt8) where {T,S}
    n = zero(S)
    z = UInt8('0')
    pos = start
    len = length(str)
    @inbounds while true
        chr = str[pos]
        if chr == sep
            if pos == start
                pos = pos_to_error(pos) # empty field
            else
                pos += 1
            end
            break
        end
        maybe_digit = chr - z
        if 0 ≤ maybe_digit ≤ 9
            n = n*10 + maybe_digit
            pos += 1
        else
            pos = pos_to_error(pos) # invalid character
            break
        end
        if pos > len            # EOL without separator
            pos = pos_to_error(pos)
            break
        end
    end
    MaybeParsed(pos, T(n))
end

struct PosFixedInteger{T <: Integer} <: AbstractParser{T}
    width::Int
    function PosFixedInteger{T}(width::Int) where {T}
        @argcheck width ≥ 1
        new{T}(width)
    end
end

show(io::IO, parser::PosFixedInteger{T}) where T =
     print(io, "Parse positive integer of $(parser.width) digits into $T")

"""
    $SIGNATURES

Parse a positive integer of *fixed width* as type `T`.

!!! NOTE
    Does not check for overflows, make sure `T` has enough digits.
"""
PosFixedInteger(width, T::Type{<: Integer} = Int) =
    PosFixedInteger{T}(width)

function parsenext(parser::PosFixedInteger{T}, str::ByteVector, start,
                   sep) where {T}
    n = zero(T)
    z = UInt8('0')
    pos = start
    stop = start + parser.width - 1
    len = length(str)
    stop > len && return MaybeParsed{Int}(pos_to_error(len+1))
    @inbounds while pos ≤ stop
        chr = str[pos]
        maybe_digit = chr - z
        if 0 ≤ maybe_digit ≤ 9
            n = n*10 + maybe_digit
            pos += 1
        else
            pos = pos_to_error(pos)
            break
        end
    end
    MaybeParsed(pos, n)
end

"""
    DateYYYYMMDD()

Parse a date in YYYYMMDD format.
"""
struct DateYYYYMMDD <: AbstractParser{Date} end

show(io::IO, ::DateYYYYMMDD) = print(io, "Parse date in YYYYMMDD format")

function parsenext(parser::DateYYYYMMDD, str::ByteVector, pos, sep)
    len = length(str)
    len ≥ pos + 8 || return MaybeParsed{Date}(pos_to_error(pos + 9)) # not enough characters
    @checkpos (pos, year) = parsenext(PosFixedInteger(4), str, pos, sep)
    @checkpos (pos, month) = parsenext(PosFixedInteger(2), str, pos, sep)
    1 ≤ month ≤ 12 || (pos -= 2; @goto error)
    @checkpos (pos, day) = parsenext(PosFixedInteger(2), str, pos, sep)
    1 ≤ day ≤ daysinmonth(year, month) || (pos -= 2; @goto error)
    @inbounds str[pos] == sep || @goto error
    pos += 1
    return MaybeParsed(pos, Date(UTD(totaldays(year, month, day))))
    @label error
    MaybeParsed{Date}(pos_to_error(pos))
end


# skip and view fields

"""
    Skip()

Skip a delimited field.
"""
struct Skip <: AbstractParser{Void} end

show(io::IO, ::Skip) = print(io, "skip field (parsed as `nothing`)")

function parsenext(parser::Skip, str::ByteVector, pos, sep::UInt8)
    len = length(str)
    @inbounds while pos ≤ len
        str[pos] == sep && return MaybeParsed(pos + 1, nothing)
        pos += 1
    end
    MaybeParsed{Void}(pos_to_error(pos))
end

"""
    ViewBytes()

Return the delimited field as a view (`SubArray`) into the parsed vector,
without the delimiter.
"""
struct ViewBytes <: AbstractParser{SubArray{UInt8,1,Array{UInt8,1},
                                            Tuple{UnitRange{Int64}},
                                            true}}
end

show(io::IO, ::ViewBytes) = print(io, "return field as bytes (SubArray)")

function parsenext(parser::ViewBytes, str::ByteVector, start, sep::UInt8)
    pos = start
    @checkpos (pos, value) = parsenext(Skip(), str, pos, sep)
    return MaybeParsed(pos, @view str[start:(pos-2)])
    @label error
    MaybeParsed{parsedtype(parser)}(pos)
end


# lines

"""
    Line(parsers...)

Parse a string using `parsers`, separated by the separator given to
`parsenext`. Parsed values are returned as a tuple, wrapped in a `MaybeParsed`,
except for parsers which have `parsedtype(parser) == Void`, which are skipped.

# implementation

The return type is calculated and saved in parameter `S`, while the indices of
parsers for which the values are kept are in `K`. For example, for

```julia
Line(PosInteger(), Skip(), DateYYYYMMDD())
 ```

`S == Tuple{Int, Date}` and `K == (1, 3)`.
"""
struct Line{T <: Tuple, S, K} <: AbstractParser{S}
    parsers::T
end

function show(io::IO, parser::Line)
    print(io, "parse line as the following:")
    for p in parser.parsers
        print(io, "\n    ")
        print(io, p)
    end
end

function Line{T <: Tuple}(parsers::T)
    parsedtypes = map(parsedtype, parsers)
    keep(T) = T ≠ Void
    S = Tuple{Iterators.filter(keep, parsedtypes)...}
    K = tuple(find(keep, parsedtypes)...)
    Line{T, S, K}(parsers)
end

Line(parsers::AbstractParser...) = Line(parsers)

const LineN{N, S, K} = Line{T, S, K} where T <: NTuple{N, Any}

function _tuplefor(itr, ex)
    vars = [ Base.Cartesian.inlineanonymous(ex, i) for i in itr ]
    Expr(:escape, Expr(:tuple, vars...))
end

getindex(line::Line{T, S, K}, i) where {T,S,K} = line.parsers[K[i]]

length(line::Line{T, S, K}) where {T,S,K} = length(K)

"""
    @tuplefor itr expr

Generates a tuple using `Base.Cartesian.inlineanonymous`, using elements in
`itr` (cf `Base.Cartesian.@ntuple`, which is a special case of this for `itr =
1:N`). For example,

```julia @tuplefor (1,2,9) k ``` would generate ```julia (k_1, k_2, k_9) ```

!!! NOTE

    internal use, not exported.
"""
macro tuplefor(itr, ex)
    _tuplefor(itr, ex)
end

@generated function parsenext(line::LineN{N, S, K}, str::ByteVector,
                              pos, sep) where {N, S, K}
    quote
        R = MaybeParsed{S}
        Base.@nexprs $N j -> begin
            @checkpos (pos, value_j) = parsenext(line.parsers[j], str, pos, sep)
        end
        return R(pos, @tuplefor $K value)
        @label error
        R(pos)
    end
end

end # module
