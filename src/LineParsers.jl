module LineParsers

using MacroTools
using ArgCheck

export
    # generic
    parsedtype,
    parsenext,
    MaybeParsed,
    # parsers
    PositiveInteger,
    PositiveFixedInteger,
    DateYYYYMMDD,
    Skip,
    Line

######################################################################
# generic interface
######################################################################

const ByteVector = Vector{UInt8}

"A parser that parses to `MaybeParsed{T}` values."
abstract type AbstractParser{T} end

"""
    parsedtype(parser)

Return type of a parser, see [`parsenext`](@ref).
"""
parsedtype(::AbstractParser{T}) where {T} = T

"""
    parsenext(parser, str, pos, sep)::MaybeParsed{parsedtype(parser)}

Parse `str` starting at `pos` using `parser`. Variable-length fields are parsed
until terminated by `sep`. Return a [`MaybeParsed`](@ref) object.
"""
function parsenext end

"""
    MaybeParsed(pos, [value])

When `pos > 0`, it is the position of the character after parsing, and `value`
holds the value. When `pos < 0`, it wraps the error code.
"""
struct MaybeParsed{T}
    pos::Int
    value::T
    MaybeParsed{T}(pos) where {T} = new{T}(pos)
    MaybeParsed{T}(pos, value::T) where {T} = new{T}(pos, value)
    MaybeParsed(pos, value::T) where {T} = new{T}(pos, value)
end

@inline pos_to_error(pos, errorcode = 0) = -pos

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

######################################################################
# integer parsing
######################################################################

struct PositiveInteger{T <: Integer} <: AbstractParser{T} end

PositiveInteger(T::Type{<:Integer} = Int) = PositiveInteger{T}()

function parsenext(parser::PositiveInteger{T}, str::ByteVector, start, sep::C) where {T, C}
    n = zero(T)
    z = C('0')
    pos = start
    len = length(str)
    @inbounds while pos ≤ len
        chr = str[pos]
        if chr == sep
            if pos == start
                pos == pos_to_error(pos)
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
            pos = pos_to_error(pos)
            break
        end
    end
    MaybeParsed(pos, n)
end

struct PositiveFixedInteger{T <: Integer} <: AbstractParser{T}
    width::Int
    function PositiveFixedInteger{T}(width::Int) where {T}
        @argcheck width ≥ 1
        new{T}(width)
    end
end

PositiveFixedInteger(width, T::Type{<: Integer} = Int) =
    PositiveFixedInteger{T}(width)

function parsenext(parser::PositiveFixedInteger{T}, str::ByteVector, start,
                   sep::C) where {T, C}
    n = zero(T)
    z = C('0')
    pos = start
    stop = start + parser.width - 1
    @assert stop ≤ length(str)
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

struct DateYYYYMMDD <: AbstractParser{Date} end

function parsenext(parser::DateYYYYMMDD, str::ByteVector, pos, sep)
    @checkpos (pos, year) = parsenext(PositiveFixedInteger(4), str, pos, sep)
    @checkpos (pos, month) = parsenext(PositiveFixedInteger(2), str, pos, sep)
    @checkpos (pos, day) = parsenext(PositiveFixedInteger(2), str, pos, sep)
    str[pos] == sep || @goto error
    pos += 1
    return MaybeParsed(pos, Date(year, month, day))
    @label error
    MaybeParsed{Date}(pos)
end

######################################################################
# skip and view fields
######################################################################

struct Skip <: AbstractParser{Void} end

function parsenext(parser::Skip, str::ByteVector, pos, sep::UInt8)
    len = length(str)
    @inbounds while pos ≤ len
        str[pos] == sep && return MaybeParsed(pos + 1, nothing)
        pos += 1
    end
    MaybeParsed{Void}(pos_to_error(pos))
end

######################################################################
# lines
######################################################################

struct Line{T <: Tuple, S} <: AbstractParser{S}
    parsers::T
end

Line{T <: Tuple}(parsers::T) = Line{T, Tuple{map(parsedtype, parsers)...}}(parsers)

const LineN{N, S} = Line{T, S} where T <: NTuple{N, Any}

@generated function parsenext(line::LineN{N, S}, str::ByteVector, pos, sep) where {N, S}
    quote
        R = MaybeParsed{S}
        Base.@nexprs $N j -> begin
            @checkpos (pos, value_j) = parsenext(line.parsers[j], str, pos, sep)
        end
        return R(pos, Base.@ntuple $N value)
        @label error
        R(pos)
    end
end

end # module
