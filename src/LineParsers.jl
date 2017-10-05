module LineParsers

using ArgCheck
using Parameters

export
    ByteVector,
    MaybeParsed,
    isparsed,
    INVALID, EOL, EMPTY,
    parsefield,
    FixedWidth,
    DateYYYYMMDD,
    SkipField,
    ViewField

######################################################################
# general interface
######################################################################
struct ParserException <: Exception end

const ByteVector = Vector{UInt8}

struct MaybeParsed{T,S <: Signed}
    pos::S
    value::T
    MaybeParsed{T}(pos::S) where {T,S} = new{T,S}(pos)
    MaybeParsed(pos::S, value::T) where {T,S} = new{T,S}(pos, value)
end

function Base.isequal(x::MaybeParsed{Tx,Sx}, y::MaybeParsed{Ty,Sy}) where {Tx,Sx,Ty,Sy}
    if isbits(Tx) && isbits(Ty)
        (x.pos == y.pos) & ifelse(x.pos > 0, isequal(x.value, y.value), true)
    else
        x.pos == y.pos && (x.pos > 0 ? isequal(x.value, y.value) : true)
    end
end

isparsed(pos::Int) = pos > 0

isparsed(mp::MaybeParsed) = isparsed(mp.pos) > 0

pos_value(mp::MaybeParsed) = mp.pos, mp.value

MaybeParsed{T}(mp::MaybeParsed{T}) where {T} = mp

function MaybeParsed{T}(mp::MaybeParsed) where {T}
    @unpack pos, value = mp
    if pos > 0
        MaybeParsed{T}(pos, convert(T, value))
    else
        MaybeParsed{T}(pos)
    end
end

"Invalid character."
const INVALID = -1

"End of line."
const EOL = -2

"Empty field."
const EMPTY = -3

"""
    parsefield(str, start, parser, separator, [len = length(string)])

Parse a field in `str::ByteVector`, starting at byte `start`, using `parser`,
which determines the type of the result. `separator` is either a separator which
delimits fields, or the number of characters to parse.

Parsing stops at byte `len`, which defaults to the length of `str`.

The result is returned as a `MaybeParsed{T}` object, with either position (or an
error code), and the value. `parser` determines `T`.
"""
function parsefield end

function parsefield(str::ByteVector, start, T, sep::Char, args...)
    @argcheck Int(sep) ≤ 0x80
    parsefield(str, start, T, UInt8(sep), args...)
end

######################################################################
# integer parsing
######################################################################

"""
    parsefield(str, start, ::Type{<: Integer}, separator, [len])

Parse an integer of the given type until `separator`.

!!! warning

    Does not check overflow; if you have too many digits, this can be a problem.
"""
function parsefield(str::ByteVector, start, ::Type{T}, sep::UInt8,
                    len = length(str)) where {T <: Integer}
    n = zero(T)
    z = UInt8('0')
    pos = start
    @inbounds while pos ≤ len
        chr = str[pos]
        chr == sep && return MaybeParsed(pos == start ? EMPTY : pos, n)
        maybe_digit = chr - z
        if 0 ≤ maybe_digit ≤ 9
            n = n*10 + maybe_digit
            pos += 1
        else
            return MaybeParsed{Int}(INVALID)
        end
    end
    MaybeParsed{Int}(EOL)
end

"""
    FixedWidth(n)

Stops parsing after reading the given number of characters.
"""
struct FixedWidth
    width::Int
end

"""
    parsefield(str, start, ::Type{<: Integer}, width::Int, [len])

Parse an integer of the given type using `width` digits.

!!! warning

    Does not check overflow; if you have too many digits, this can be a problem.

    Does not verify that any kind of separator follows the digits.
"""
function parsefield(str::ByteVector, start, ::Type{T}, width::FixedWidth,
                    len = length(str)) where {T <: Integer}
    n = 0
    z = UInt8('0')
    pos = start
    stop = start + width.width
    @argcheck stop ≤ len+1 "Too short for fixed width parsing."
    @inbounds while pos < stop
        maybe_digit = str[pos] - z
        if 0 ≤ maybe_digit ≤ 9
            n = n*10 + maybe_digit
            pos += 1
        else
            return MaybeParsed{Int}(INVALID)
        end
    end
    MaybeParsed(pos, n)
end

struct DateYYYYMMDD{strict} end

"""
    parsefield(str, start, ::Type{DateYYYYMMDD}, strict = true)

Parse dates of the form "yyyymmdd".

When `strict = false`, zero days are replaced by 1 (a peculiarity of the
dataset).
"""
function parsefield(str::ByteVector, start, ::Type{DateYYYYMMDD{strict}},
                    sep::UInt8, len = length(str)) where {strict}
    stop = start+8
    len ≥ stop || return MaybeParsed{Date}(EOL)

    pos, y = pos_value(parsefield(str, start, Int, FixedWidth(4)))
    isparsed(pos) || return MaybeParsed{Date}(pos)

    pos, m = pos_value(parsefield(str, pos, Int, FixedWidth(2)))
    isparsed(pos) || return MaybeParsed{Date}(pos)

    pos, d = pos_value(parsefield(str, pos, Int, FixedWidth(2)))
    isparsed(pos) || return MaybeParsed{Date}(pos)

    str[pos] == sep || @goto invalid

    d == 0 && (strict ? @goto(invalid) : (d = 1))

    (m ≤ 12 && d ≤ Base.Dates.daysinmonth(y, m)) || @goto invalid

    return MaybeParsed(pos, Date(y, m, d))

    @label invalid
    MaybeParsed{Date}(INVALID)
end

"""
    SkipField()

Skip the field, returning `nothing`.
"""
struct SkipField end

function parsefield(str::ByteVector, start, ::SkipField, sep::UInt8, len = length(str))
    pos = start
    @inbounds while pos ≤ len
        str[pos] == sep && return MaybeParsed(pos, nothing)
        pos += 1
    end
    MaybeParsed{Void}(EOL)
end

"""
    ViewField

Return the contents of the field as a `SubArray` (view).
"""
struct ViewField end

function parsefield(str::ByteVector, start, ::ViewField, sep::UInt8, len = length(str))
    pos, _ = pos_value(parsefield(str, start, SkipField(), sep, len))
    isparsed(pos) ? MaybeParsed(pos, @view(str[start:(pos-1)])) : MaybeParsed(pos)
end

end # module
