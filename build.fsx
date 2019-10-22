#r @"paket:
source https://nuget.org/api/v2
framework netstandard2.0
nuget Fake.Core.Target
nuget Fake.DotNet.Cli
nuget Fake.DotNet.Testing.Expecto //"

#if !FAKE
#load "./.fake/build.fsx/intellisense.fsx"
#r "netstandard" // Temp fix for https://github.com/fsharp/FAKE/issues/1985
#endif


// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

open Fake
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators

// Targets
Target.create "Clean" (fun _ ->
    // Shell.cleanDirs
    //   [
    //     "src/TigerCompiler/bin/"
    //     "src/TigerCompiler/obj/"
    //     "tests/TigerCompiler.Tests/bin/"
    //     "tests/TigerCompiler.Tests/obj/"
    //   ]
    ()
)

Target.create "Build" (fun _ ->
    DotNet.exec id "build" "Tiger.sln -c Release" |> ignore
)

Target.create "RunTests" (fun _ ->
    !! "tests/**/bin/Release/**/*Tests*.dll"
    |> Testing.Expecto.run (fun p -> { p with Parallel = false } )
)

Target.create "All" ignore

// Build order
"Clean"
  ==> "Build"
  ==> "RunTests"
  ==> "All"

// start build
Target.runOrDefault "All"