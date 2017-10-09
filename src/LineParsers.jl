module LineParsers

using MacroTools
using ArgCheck

using Base.Dates: daysinmonth, UTD, totaldays

export
    # generic
    parsedtype,
    parsenext,
    MaybeParsed,
    isparsed,
    # parsers
    PositiveInteger,
    PositiveFixedInteger,
    DateYYYYMMDD,
    Skip,
    ViewBytes,
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
until terminated by `sep`. Return a [`MaybeParsed`](@ref) object, the position
is that of the next character after parsing.
"""
function parsenext end

function parsenext(parser, str::AbstractString, pos, sep::Char)
    @argcheck Int(sep) < 0x80
    parsenext(parser, convert(ByteVector, str), pos, UInt8(sep))
end

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

@inline Base.unsafe_get(x::MaybeParsed) = x.value

function Base.isequal(x::MaybeParsed{Tx}, y::MaybeParsed{Ty}) where {Tx,Ty}
    if isbits(Tx) && isbits(Ty)
        (x.pos == y.pos) & ifelse(x.pos > 0, isequal(x.value, y.value), true)
    else
        x.pos == y.pos && (x.pos > 0 ? isequal(x.value, y.value) : true)
    end
end

@inline pos_to_error(pos) = -pos

@inline isparsed(x::MaybeParsed) = x.pos > 0

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

function parsenext(parser::PositiveInteger{T}, str::ByteVector, start::Int,
                   sep::UInt8) where {T}
    n = zero(T)
    z = UInt8('0')
    pos = start
    len = length(str)
    @inbounds while true
        chr = str[pos]
        if chr == sep
            if pos == start
                pos == pos_to_error(pos) # empty field
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
        if pos > len           # EOL without separator
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

function parsenext(parser::PositiveFixedInteger{T}, str::ByteVector, start, sep) where {T}
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

struct DateYYYYMMDD <: AbstractParser{Date} end

function parsenext(parser::DateYYYYMMDD, str::ByteVector, pos, sep)
    len = length(str)
    len ≥ pos + 8 || return MaybeParsed{Date}(pos_to_error(pos + 9)) # not enough characters
    @checkpos (pos, year) = parsenext(PositiveFixedInteger(4), str, pos, sep)
    @checkpos (pos, month) = parsenext(PositiveFixedInteger(2), str, pos, sep)
    1 ≤ month ≤ 12 || (pos -= 2; @goto error)
    @checkpos (pos, day) = parsenext(PositiveFixedInteger(2), str, pos, sep)
    1 ≤ day ≤ daysinmonth(year, month) || (pos -= 2; @goto error)
    @inbounds str[pos] == sep || @goto error
    pos += 1
    return MaybeParsed(pos, Date(UTD(totaldays(year, month, day))))
    @label error
    MaybeParsed{Date}(pos_to_error(pos))
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

struct ViewBytes <: AbstractParser{SubArray{UInt8,1,Array{UInt8,1},
                                            Tuple{UnitRange{Int64}},
                                            true}}
end

function parsenext(parser::ViewBytes, str::ByteVector, start, sep::UInt8)
    pos = start
    @checkpos (pos, value) = parsenext(Skip(), str, pos, sep)
    return MaybeParsed(pos, @view str[start:(pos-2)])
    @label error
    MaybeParsed{parsedtype(parser)}(pos)
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
