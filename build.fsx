// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.Testing.Expecto

// Default target configuration
let configuration = "Release"
let vsProjProps =
#if MONO
    [ ("DefineConstants","MONO"); ("Configuration", configuration) ]
#else
    [ ("Configuration", configuration); ("Platform", "Any CPU") ]
#endif

// Filesets
let solutionFile = "Tiger.sln"
let testExecutables = !! "tests/**/bin/Release/*Tests*.exe"


// Targets
Target "Clean" (fun _ ->
    !! solutionFile |> MSBuildReleaseExt "" vsProjProps "Clean" |> ignore
)

Target "Build" (fun _ ->
    // compile all projects below src/app/
    !! solutionFile
    |> MSBuildReleaseExt "" vsProjProps "Rebuild"
    |> Log "AppBuild-Output: "
)

Target "RunTests" (fun _ ->
    testExecutables
    |> Expecto (fun p -> { p with Parallel = false } )
    |> ignore
)

Target "All" DoNothing

// Build order
"Clean"
  ==> "Build"
  ==> "RunTests"
  ==> "All"

// start build
RunTargetOrDefault "All"
