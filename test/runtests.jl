using LineParsers
using Base.Test
using InferenceUtilities

const ≅ = isequal               # infix is more compact

@testset "MaybeParsed methods" begin
    @test MaybeParsed{Int}(INVALID) ≅ MaybeParsed{Int}(INVALID)
    @test MaybeParsed(INVALID, 1) ≅ MaybeParsed(INVALID, 2)
    @test MaybeParsed(4, 10) ≅ MaybeParsed(4, 10)
    @test !(MaybeParsed{Int}(INVALID) ≅ MaybeParsed{Int}(EOL))
    @test !(MaybeParsed(2, 3) ≅ MaybeParsed(2, 4))
    @test !(MaybeParsed(3, 3) ≅ MaybeParsed(2, 3))
    @test isparsed(MaybeParsed(1,1))
    @test !isparsed(MaybeParsed{Int}(EOL))
end

@testset "integer parsing" begin
    @test parsefield(b"119;", 1, Int, ';') ≅ MaybeParsed(4, 119)
    @test parsefield(b"222;xx;", 5, Int, ';') ≅ MaybeParsed{Int}(INVALID)
    @test parsefield(b"22;77", 4, Int, FixedWidth(2)) ≅ MaybeParsed(6, 77)
    @test parsefield(b"111", 1, Int, ';') ≅ MaybeParsed{Int}(EOL)
    @test parsefield(b"11x", 1, Int, FixedWidth(3)) ≅ MaybeParsed{Int}(INVALID)
    @test parsefield(b"11", 1, Int, FixedWidth(3)) ≅ MaybeParsed{Int}(EOL)
    @test @isinferred parsefield(b"11", 1, Int, ';')
    @test @isinferred parsefield(b"11", 1, Int, FixedWidth(4))
end

@testset "skip or verbatim strings" begin
    @test parsefield(b"xxx;yyyy;", 5, SkipField, ';') ≅
        MaybeParsed(9, nothing)
    @test parsefield(b"xxx;yyyy;", 5, ViewField, ';') ≅
        MaybeParsed(9, @view(b"yyyy"[:]))
    @test parsefield(b"nosep", 1, SkipField, ';') ≅ MaybeParsed{Void}(EOL)
    @test parsefield(b"nosep", 1, ViewField, ';') ≅ MaybeParsed{String}(EOL)
    @test @isinferred parsefield(b"something", 1, SkipField, UInt8(';'))
    @test @isinferred parsefield(b"something", 1, ViewField, UInt8(';'))
end

@testset "dates" begin
    @test parsefield(b"xxx;19800101;", 5, DateYYYYMMDD{true}, ';') ≅
        MaybeParsed(13, Date(1980, 1, 1))
    @test parsefield(b"xxx;19800100;", 5, DateYYYYMMDD{true}, ';') ≅
        MaybeParsed{Date}(INVALID)
    @test parsefield(b"xxx;19800100;", 5, DateYYYYMMDD{false}, ';') ≅
        MaybeParsed(13, Date(1980, 1, 1))
    @test @isinferred parsefield(b"19800101", 1, DateYYYYMMDD{true}, ';')
end
