Pkg.clone("https://github.com/afniedermayer/InferenceUtilities.jl") # until registered
Pkg.clone(pwd())
Pkg.build("LineParsers")
Pkg.test("LineParsers"; coverage=true)
