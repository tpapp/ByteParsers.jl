using LineParsers
using Base.Test
using InferenceUtilities

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

≂(x::MaybeParsed{T,S}, y::MaybeParsed{T,S}) where {T,S} = x ≅ y

@testset "MaybeParsed methods" begin
    @test MaybeParsed{Int}(INVALID) ≂ MaybeParsed{Int}(INVALID)
    @test MaybeParsed(INVALID, 1) ≂ MaybeParsed(INVALID, 2)
    @test MaybeParsed(4, 10) ≂ MaybeParsed(4, 10)
    @test !(MaybeParsed{Int}(INVALID) ≅ MaybeParsed{Int}(EOL))
    @test !(MaybeParsed(2, 3) ≅ MaybeParsed(2, 4))
    @test !(MaybeParsed(3, 3) ≅ MaybeParsed(2, 3))
    @test isparsed(MaybeParsed(1,1))
    @test !isparsed(MaybeParsed{Int}(EOL))
    @test unsafe_get(MaybeParsed(1, 10)) ≡ 10
    @test_throws UndefRefError unsafe_get(MaybeParsed{String}(1))
end

@testset "integer parsing" begin
    @test parsefield(b"119;", 1, Int, ';') ≂ MaybeParsed(5, 119)
    @test parsefield(b"222;xx;", 5, Int, ';') ≂ MaybeParsed{Int}(INVALID)
    @test parsefield(b"22;77", 4, Int, FixedWidth(2)) ≂ MaybeParsed(6, 77)
    @test parsefield(b"111", 1, Int, ';') ≂ MaybeParsed{Int}(EOL)
    @test parsefield(b"11x", 1, Int, FixedWidth(3)) ≂ MaybeParsed{Int}(INVALID)
    @test parsefield(b"11", 1, Int, FixedWidth(3)) ≂ MaybeParsed{Int}(EOL)
    @test @isinferred parsefield(b"11", 1, Int, ';')
    @test @isinferred parsefield(b"11", 1, Int, FixedWidth(4))
end

@testset "skip or verbatim strings" begin
    @test parsefield(b"xxx;yyyy;", 5, SkipField, ';') ≂
        MaybeParsed(10, nothing)
    @test parsefield(b"xxx;yyyy;", 5, ViewField, ';') ≅
        MaybeParsed(10, b"yyyy"[:])
    @test parsefield(b"nosep", 1, SkipField, ';') ≂ MaybeParsed{Void}(EOL)
    @test parsefield(b"nosep", 1, ViewField, ';') ≅ MaybeParsed{String}(EOL)
    @test @isinferred parsefield(b"something", 1, SkipField, UInt8(';'))
    @test @isinferred parsefield(b"something", 1, ViewField, UInt8(';'))
end

@testset "dates" begin
    @test parsefield(b"xxx;19800101;", 5, DateYYYYMMDD{true}, ';') ≂
        MaybeParsed(14, Date(1980, 1, 1))
    @test parsefield(b"xxx;19800100;", 5, DateYYYYMMDD{true}, ';') ≂
        MaybeParsed{Date}(INVALID)
    @test parsefield(b"xxx;19800100;", 5, DateYYYYMMDD{false}, ';') ≂
        MaybeParsed(14, Date(1980, 1, 1))
    @test @isinferred parsefield(b"19800101", 1, DateYYYYMMDD{true}, ';')
end

@testset "parsed types" begin
    @test parsedtype(b"", 9, Int, UInt8(';')) ≡ Int
    @test parsedtype(b"", 9, DateYYYYMMDD{true}, UInt8(';')) ≡ Date
    @test parsedtype(b"", 9, SkipField, UInt8(';')) ≡ Void
    @test parsedtype(b"", 9, ViewField, UInt8(';')) ≡ typeof(@view b"xx"[1:1])
end

@testset "parseline" begin
    line1 = b"1212;skipped;kept;"
    mp1 = MaybeParsed(length(line1)+1, (1212, b"kept"))
    @test parseline(line1, ';', Int, SkipField, ViewField) ≅ mp1
    @test parseline(b"1bad;", UInt8(';'), Int, SkipField) ≅ MaybeParsed{Tuple{Int}}(INVALID)
end
