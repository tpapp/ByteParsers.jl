using ByteParsers
using ByteParsers: FixEmpty
using Base.Test
using InferenceUtilities

const sc = UInt8(';')           # semicolon, used as separator in tests

"""
   x ≅ y (type with \\cong)

A less strict comparison of MaybeParsed types: positions have to match (`==`),
values only when parsed (`isequal`). Type parameters don't have to match
otherwise. Useful for testing.
"""
≅(x, y) = isequal(x, y)

"""
    x ≂ y (type with \\eqsim)

A very strict comparison of MaybeParsed types: type parameters and positions
have to match (`==`), values only when parsed (`isequal`). Useful for testing.
"""
≂(x, y) = false

≂(x::MaybeParsed{T}, y::MaybeParsed{T}) where {T} = isequal(x, y)

@testset "MaybeParsed methods" begin
    @test MaybeParsed{Int}(-1) ≂ MaybeParsed{Int}(-1)
    @test MaybeParsed(-1, 1) ≂ MaybeParsed(-1, 2)
    @test MaybeParsed(4, 10) ≂ MaybeParsed(4, 10)
    @test !(MaybeParsed{Int}(-3) ≅ MaybeParsed{Int}(-4))
    @test !(MaybeParsed(2, 3) ≅ MaybeParsed(2, 4))
    @test !(MaybeParsed(3, 3) ≅ MaybeParsed(2, 3))
    @test unsafe_get(MaybeParsed(1, 10)) ≡ 10
    @test_throws UndefRefError unsafe_get(MaybeParsed{String}(1))
    @test getpos(MaybeParsed(5, 10)) == 5
    let mp = MaybeParsed(2,5)
        @test getpos(mp) == 2
        @test isparsed(mp)
    end
    let mp = MaybeParsed(ByteParsers.pos_to_error(7), 19)
        @test getpos(mp) == 7
        @test !isparsed(mp)
    end
end

@testset "integer parsing" begin
    @test parsenext(PosInteger(), "119;", 1, ';') ≂ MaybeParsed(5, 119)
    @test parsenext(PosInteger(), "222;xx;", 5, ';') ≂ MaybeParsed{Int}(-5)
    @test parsenext(PosFixedInteger(2), "22;77", 4, ';') ≂ MaybeParsed(6, 77)
    @test parsenext(PosInteger(), b"111", 1, sc) ≂ MaybeParsed{Int}(-4)
    @test parsenext(PosFixedInteger(3), b"11x", 1, sc) ≂ MaybeParsed{Int}(-3)
    @test parsenext(PosFixedInteger(3), b"11", 1, sc) ≂ MaybeParsed{Int}(-3)
    @test parsenext(PosInteger(), b";", 1, sc) ≂ MaybeParsed{Int}(-1)
    @test @isinferred parsenext(PosInteger(), b"11", 1, sc)
    @test @isinferred parsenext(PosFixedInteger(3), b"11", 1, sc)
    # typed
    for T ∈ [Int8, Int16, Int32, Int64, BigInt]
        @test parsenext(PosInteger(T), b"19;", 1, sc) ≂ MaybeParsed(4, T(19))
    end
    @test_throws InexactError parsenext(PosInteger(Int8), b"300", 1, sc)
    @test repr(PosInteger(Int16, Int32)) ==
        "Parse positive integer into Int16 (accumulating in Int32)"
    @test repr(PosFixedInteger(4, Int16)) ==
        "Parse positive integer of 4 digits into Int16"
end

@testset "skip or verbatim strings" begin
    @test parsenext(Skip(), b"xxx;yyyy;", 5, sc) ≂ MaybeParsed(10, nothing)
    @test parsenext(ViewBytes(), b"xxx;yyyy;", 5, sc) ≅ MaybeParsed(10, b"yyyy"[:])
    @test parsenext(Skip(), b"nosep", 1, sc) ≂ MaybeParsed{Void}(-6)
    @test parsenext(ViewBytes(), b"nosep", 1, sc) ≅ MaybeParsed{String}(-6)
    @test @isinferred parsenext(Skip(), b"something", 1, sc)
    @test @isinferred parsenext(ViewBytes(), b"something", 1, sc)
    @test repr(Skip()) == "skip field (parsed as `nothing`)"
    @test repr(ViewBytes()) == "return field as bytes (SubArray)"
end

@testset "dates" begin
    @test parsenext(DateYYYYMMDD(), b"xxx;19800101;", 5, sc) ≂
        MaybeParsed(14, Date(1980, 1, 1))
    @test parsenext(DateYYYYMMDD(), b"19800100;", 1, sc) ≂ MaybeParsed{Date}(-7)
    @test parsenext(DateYYYYMMDD(), b"19809900;", 1, sc) ≂ MaybeParsed{Date}(-5)
    @test parsenext(DateYYYYMMDD(), b"19800101", 1, sc) ≂ MaybeParsed{Date}(-10)
    @test @isinferred parsenext(DateYYYYMMDD(), b"19800101;", 1, sc)
    @test repr(DateYYYYMMDD()) == "Parse date in YYYYMMDD format"
end

@testset "fix empty values" begin
    p = FixEmpty(-1, PosInteger())
    @test parsenext(p, b";", 1, sc) ≂ MaybeParsed{Int}(2, -1)
    @test parsenext(p, b"2;", 1, sc) ≂ MaybeParsed{Int}(3, 2)
    @test parsedtype(p) ≡ Int
    @test repr(p) == "parse empty fields as -1, otherwise " * repr(PosInteger())
end

@testset "parsed types" begin
    @test parsedtype(PosInteger()) ≡ Int
    @test parsedtype(PosFixedInteger(2)) ≡ Int
    @test parsedtype(Skip()) ≡ Void
    @test parsedtype(ViewBytes()) ≡ typeof(@view b"xx"[1:1])
end

@testset "parsing line" begin
    line1 = b";1212;skipped;kept;"
    mp1 = MaybeParsed(length(line1)+1, (-1, 1212, b"kept")) # value skipped
    parser1 = Line(FixEmpty(-1, PosInteger()), PosInteger(), Skip(), ViewBytes())
    @test length(parser1) == 3
    @test parser1[1] ≅ FixEmpty(-1, PosInteger())
    @test parser1[2] == PosInteger()
    @test parser1[3] == ViewBytes()
    @test parsenext(parser1, line1, 1, sc) ≅ mp1
    @test parsenext(Line(PosInteger()), b"1bad;", 1, sc) ≅
        MaybeParsed{Tuple{Int}}(-2)
    @test @isinferred parsenext(parser1, line1, 1, sc)
    @test repr(parser1) ==
        ("parse line as the following:\n    " *
         repr(FixEmpty(-1, PosInteger())) * "\n    " *
         repr(PosInteger()) * "\n    " *
         repr(Skip()) * "\n    " *
         repr(ViewBytes()))
end
