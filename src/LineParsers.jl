module LineParsers

using ArgCheck
using Parameters

export
    ByteVector,
    MaybeParsed,
    isparsed,
    INVALID, EOL, EMPTY,
    parsefield,
    parsedtype,
    FixedWidth,
    DateYYYYMMDD,
    SkipField,
    ViewField,
    parseline

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

Base.unsafe_get(mp::MaybeParsed) = mp.value

pos_value(mp::MaybeParsed) = mp.pos, mp.value

MaybeParsed{T}(mp::MaybeParsed{T}) where {T} = mp

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

The result is returned as a `MaybeParsed{T}` object, with either position at the
last character parsed (or an error code), and the value. `parser` determines
`T`.
"""
function parsefield end

function _safe_char2uint8(c::Char)
    @argcheck Int(c) ≤ 0x80
    UInt8(c)
end

parsefield(str::ByteVector, start, T, sep::Char, args...) =
    parsefield(str, start, T, _safe_char2uint8(sep), args...)

_value_type_parameter(::Type{MaybeParsed{T,S}}) where {T,S} = T
_value_type_parameter(T::S) where {S <: MaybeParsed} =
    _value_type_parameter(S)

"""
    parsedtype(str, start, parser, separator)

Return a type `T` such that `parsefield` called with the same parameters returns
a value of type `MaybeParsed{T,S}`.

!!! note

    Relies on type inference unless overridden.
"""
function parsedtype(str::Tstr, start::Tstart, ::Type{T}, sep::Tsep) where
    {Tstr, Tstart, T, Tsep}
    T_ = Base._return_type(parsefield, Tuple{Tstr, Tstart, Type{T}, Tsep})
    _value_type_parameter(T_)
end

"""

   parsefield(::Void, pos, parser, sep)

Return `MaybeParsed{T}(pos)`, where `T` matcheds the type parameter of the value
returned by the same call with a `ByteVector` as the first parameter.

The function is useful for writing type stable code when parsing the parsed has
given up on a set of fields.
"""
function parsefield(::Void, pos::P, ::Type{T}, sep::S) where {P, T, S}
    MaybeParsed{_value_type_parameter(T_)}(pos)
end

######################################################################
# integer parsing
######################################################################

"""
    parsefield(str, start, ::Type{<: Integer}, separator, [len])

Parse an integer of the given type until `separator`.

!!! warning

    Does not check overflow; if you have too many digits, this can be a
    problem. Make sure you use a wide enough type.
"""
function parsefield(str::ByteVector, start, ::Type{T}, sep::UInt8,
                    len = length(str)) where {T <: Integer}
    n = zero(T)
    z = UInt8('0')
    pos = start
    @inbounds while pos ≤ len
        chr = str[pos]
        chr == sep && return MaybeParsed(pos == start ? EMPTY : pos + 1, n)
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
    stop ≤ len + 1 || return MaybeParsed{Int}(EOL)
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

    return MaybeParsed(pos + 1, Date(y, m, d))

    @label invalid
    MaybeParsed{Date}(INVALID)
end

"""
    SkipField()

Skip the field, returning `nothing`.
"""
struct SkipField end

function parsefield(str::ByteVector, start, ::Type{SkipField}, sep::UInt8,
                    len = length(str))
    pos = start
    @inbounds while pos ≤ len
        str[pos] == sep && return MaybeParsed(pos + 1, nothing)
        pos += 1
    end
    MaybeParsed{Void}(EOL)
end

"""
    ViewField

Return the contents of the field as a `SubArray` (view).
"""
struct ViewField end

function parsefield(str::ByteVector, start, ::Type{ViewField}, sep::UInt8,
                    len = length(str))
    pos, _ = pos_value(parsefield(str, start, SkipField, sep, len))
    MaybeParsed(pos, @view(str[start:(ifelse(isparsed(pos),pos-2,0))]))
end

######################################################################
# line parsing
######################################################################

"""
    parseline_(str, sep, previous_fields, field_counter)

Parse `str`, which contains fields separated by `sep`. `previous_fields` is a
`MaybeParsed(pos, values)` object where `values` are the previously parsed
fields and parsing continues from `pos + 1`. `field_counter` is the number of
fields parsed so far.

!!! NOTE
    Internal function, not part of the API.
"""
parseline_(str, sep, previous_fields, field_counter) =
    previous_fields, field_counter

function parseline_(str, sep, previous_fields, field_counter,
                    parsers_first, parsers_tail...)
    previous_pos, previous_value = pos_value(previous_fields)
    current_field = parsefield(str, previous_pos, parsers_first, sep)
    if isparsed(current_field)
        pos, value = pos_value(current_field)
        parseline_(str, sep, MaybeParsed(pos, (previous_value..., value)),
                   field_counter + 1, parsers_tail...)
    else
        pos = current_field.pos
        previous_types = _value_type_parameter(previous_fields).parameters
        current_type = _value_type_parameter(current_field)
        next_types = map(p -> parsedtype(str, pos, p, sep), parsers_tail)
        T = Tuple{previous_types..., current_type, next_types...}
        MaybeParsed{T}(pos), field_counter
    end
end

parseline(str, sep, parsers) = parseline_(str, sep, MaybeParsed(1, ()), 0,
                                          parsers...)

parseline(str::ByteVector, sep::Char, parsers) =
    parseline(str, _safe_char2uint8(sep), parsers)

end # module
