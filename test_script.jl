Pkg.clone("https://github.com/afniedermayer/InferenceUtilities.jl") # until registered
Pkg.clone(pwd())
Pkg.build("ByteParsers")
Pkg.test("ByteParsers"; coverage=true)
