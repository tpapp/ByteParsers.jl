using Compat # until https://github.com/JuliaCI/BenchmarkTools.jl/issues/83 is fixed
using BenchmarkTools
using ByteParsers
using TextParse
import TextParse: tryparsenext

sep = UInt8(';')
lopt = TextParse.LocalOpts(';', '\"', '\\', false, false)

string_and_bytes(s) = s, convert(Vector{UInt8}, s)

# parsing integers
s1, b1 = string_and_bytes("123456789;")
p1 = PositiveInteger()
t1 = TextParse.Numeric(Int)

@benchmark parsenext($p1, $b1, 1, $sep)                    # 15 ns
@benchmark tryparsenext($t1, $s1, 1, $(length(s1)), $lopt) # 21 ns

# parsing dates
s2, b2 = string_and_bytes("19800101;")
p2 = DateYYYYMMDD()
t2 = TextParse.DateTimeToken(Date, DateFormat("yyyymmdd"))

@benchmark parsenext($p2, $b2, 1, $sep)                    # 29 ns
@benchmark tryparsenext($t2, $s2, 1, $(length(s2)), $lopt) # 65 ns

# parsing strings
s3, b3 = string_and_bytes("foobar;")
p3 = ViewBytes()
t3 = TextParse.StringToken(TextParse.StrRange)

@benchmark parsenext($p3, $b3, 1, $sep)                    # 19 ns
@benchmark tryparsenext($t3, $s3, 1, $(length(s3)), $lopt) # 25 ns

# skipping string (use previous b3)

p4 = Skip()
@benchmark parsenext($p4, $b3, 1, $sep)                    # 6 ns

######################################################################
# a mess of various benchmarks below, FIXME: organize
######################################################################

str = b"1980;1990;19800101;"
sep = UInt8(';')

p1 = PositiveInteger(Int)
p2 = PositiveFixedInteger(4)
pd = DateYYYYMMDD()
r1 = Line(p1, p1)
r2 = Line(p1, p1, pd)

parsenext(p1, str, 1, sep)
parsenext(p2, str, 1, sep)
parsenext(r1, str, 1, sep)
parsenext(r2, str, 1, sep)
@code_typed parsenext(r1, str, 1, sep)

@benchmark parsenext($p1, $str, 1, $sep) # 12 ns
@benchmark parsenext($r1, $str, 1, $sep) # 19 ns
@benchmark parsenext($p2, $str, 1, $sep) # 11 ns
@benchmark parsenext($r2, $str, 1, $sep) # 44 ns
@benchmark parsenext($pd, $str, 11, $sep) # 30 ns

str2 = b"irrelevant;"
ps = Skip()
pv = ViewBytes()
@benchmark parsenext($ps, $str2, 1, $sep) # 8 ns
@benchmark parsenext($pv, $str2, 1, $sep) # 20 ns

## skipping
skiptest = Line(PositiveInteger(), Skip(), Skip(), Skip(), ViewBytes(), ViewBytes())
skipstr = b"1234567;skipped;skipped;skipped;relevant;super-relevant;"
parsenext(skiptest, skipstr, 1, sep)
@benchmark parsenext($skiptest, $skipstr, 1, $sep) # 80ns


## how does Date parsing compare?
df = DateFormat("yyyymmdd")
str2 = "19800101"
@benchmark tryparse(Date, $str2, $df) # 69 ns

parsenext(PositiveInteger(), b"111;", 1, sep)
parsenext(PositiveInteger(), b"111", 1, sep)
