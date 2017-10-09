using ByteParsers
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
    @test isparsed(MaybeParsed(1,1))
    @test !isparsed(MaybeParsed{Int}(-1))
    @test unsafe_get(MaybeParsed(1, 10)) ≡ 10
    @test_throws UndefRefError unsafe_get(MaybeParsed{String}(1))
end

@testset "integer parsing" begin
    @test parsenext(PositiveInteger(), "119;", 1, ';') ≂ MaybeParsed(5, 119)
    @test parsenext(PositiveInteger(), "222;xx;", 5, ';') ≂ MaybeParsed{Int}(-5)
    @test parsenext(PositiveFixedInteger(2), "22;77", 4, ';') ≂ MaybeParsed(6, 77)
    @test parsenext(PositiveInteger(), b"111", 1, sc) ≂ MaybeParsed{Int}(-4)
    @test parsenext(PositiveFixedInteger(3), b"11x", 1, sc) ≂ MaybeParsed{Int}(-3)
    @test parsenext(PositiveFixedInteger(3), b"11", 1, sc) ≂ MaybeParsed{Int}(-3)
    @test parsenext(PositiveInteger(), b";", 1, sc) ≂ MaybeParsed{Int}(-1)
    @test @isinferred parsenext(PositiveInteger(), b"11", 1, sc)
    @test @isinferred parsenext(PositiveFixedInteger(3), b"11", 1, sc)
end

@testset "skip or verbatim strings" begin
    @test parsenext(Skip(), b"xxx;yyyy;", 5, sc) ≂ MaybeParsed(10, nothing)
    @test parsenext(ViewBytes(), b"xxx;yyyy;", 5, sc) ≅ MaybeParsed(10, b"yyyy"[:])
    @test parsenext(Skip(), b"nosep", 1, sc) ≂ MaybeParsed{Void}(-6)
    @test parsenext(ViewBytes(), b"nosep", 1, sc) ≅ MaybeParsed{String}(-6)
    @test @isinferred parsenext(Skip(), b"something", 1, sc)
    @test @isinferred parsenext(ViewBytes(), b"something", 1, sc)
end

@testset "dates" begin
    @test parsenext(DateYYYYMMDD(), b"xxx;19800101;", 5, sc) ≂
        MaybeParsed(14, Date(1980, 1, 1))
    @test parsenext(DateYYYYMMDD(), b"19800100;", 1, sc) ≂ MaybeParsed{Date}(-7)
    @test parsenext(DateYYYYMMDD(), b"19809900;", 1, sc) ≂ MaybeParsed{Date}(-5)
    @test parsenext(DateYYYYMMDD(), b"19800101", 1, sc) ≂ MaybeParsed{Date}(-10)
    @test @isinferred parsenext(DateYYYYMMDD(), b"19800101;", 1, sc)
end

@testset "parsed types" begin
    @test parsedtype(PositiveInteger()) ≡ Int
    @test parsedtype(PositiveFixedInteger(2)) ≡ Int
    @test parsedtype(Skip()) ≡ Void
    @test parsedtype(ViewBytes()) ≡ typeof(@view b"xx"[1:1])
end

@testset "parsing line" begin
    line1 = b"1212;skipped;kept;"
    mp1 = MaybeParsed(length(line1)+1, (1212, b"kept")) # second value skipped
    parser1 = Line(PositiveInteger(), Skip(), ViewBytes())
    @test parsenext(parser1, line1, 1, sc) ≅ mp1
    @test parsenext(Line(PositiveInteger()), b"1bad;", 1, sc) ≅
        MaybeParsed{Tuple{Int}}(-2)
    @test @isinferred parsenext(parser1, line1, 1, sc)
end
