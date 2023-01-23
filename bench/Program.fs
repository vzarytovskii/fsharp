module FSharp.Benchmarks.Program

open System
open System.IO
open System.Linq
open System.Xml
open System.Collections.Immutable

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Text
open FSharp.Compiler.Tokenization

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open BenchmarkDotNet.Extensions
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Configs
open Perfolizer.Horology

open BenchmarkDotNet.Loggers
open BenchmarkDotNet.Exporters
open BenchmarkDotNet.Columns
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Exporters.Json
open BenchmarkDotNet.Reports


[<Literal>]
let FSharpCategory = "fsharp"

let (++) a b = Path.Combine(a, b)

let prepareProject projectDirName =
    let projectDir = __SOURCE_DIRECTORY__ ++ projectDirName

    let projectFile =
        projectDir
        |> Directory.GetFiles
        |> Seq.filter (fun f -> f.EndsWith ".fsproj")
        |> Seq.toList
        |> function
            | [] -> failwith $"No .fsproj file found in {projectDir}"
            | [x] -> x
            | files -> failwith $"Multiple .fsproj files found in {projectDir}: {files}"

    let fsproj = XmlDocument()
    do fsproj.Load projectFile

    let sourceFiles = [|for node in fsproj.DocumentElement.SelectNodes("//Compile") -> projectDir ++ node.Attributes["Include"].InnerText|]

    let checker = FSharpChecker.Create(projectCacheSize=300)

    let projectOptions, _diagnostics =
        checker.GetProjectOptionsFromScript("file.fs", SourceText.ofString "", assumeDotNetFramework=false)
        |> Async.RunSynchronously

    let projectOptions =
        { projectOptions with
            OtherOptions = [|
                yield! projectOptions.OtherOptions
                "--optimize+"
                "--target:library"
            |]
            UseScriptResolutionRules = false
            ProjectFileName = projectFile
            SourceFiles = sourceFiles }

    projectDir, projectOptions, checker

let parseAndTypeCheckProject (projectDir, projectOptions, checker: FSharpChecker)  =

    let result = checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously

    match result.Diagnostics |> Seq.where (fun d -> d.Severity = FSharpDiagnosticSeverity.Error) |> Seq.toList with
    | [] -> projectDir, projectOptions, checker
    | errors ->
        let errors = errors |> Seq.map (sprintf "%A") |> String.concat "\n"
        failwith $"Type checking failed {errors}"

let counter = (Seq.initInfinite id).GetEnumerator()

let typeCheckFileInProject projectDir projectOptions (checker: FSharpChecker) filename =
    let filename = projectDir ++ filename

    counter.MoveNext() |> ignore
    let count = counter.Current
    let contents = File.ReadAllText filename + $"\n// {count}" // avoid cache

    let _parseResult, checkResult = checker.ParseAndCheckFileInProject(filename, count, SourceText.ofString contents, projectOptions) |> Async.RunSynchronously

    match checkResult with
    | FSharpCheckFileAnswer.Succeeded checkFileResults ->
        match checkFileResults.Diagnostics |> Seq.where (fun d -> d.Severity = FSharpDiagnosticSeverity.Error) |> Seq.toList with
        | [] -> checkFileResults
        | errors ->
            let errors = errors |> Seq.map (sprintf "%A") |> String.concat "\n"
            failwith $"Type checking failed {errors}"
    | FSharpCheckFileAnswer.Aborted -> failwith "Type checking aborted"


[<BenchmarkCategory(FSharpCategory)>]
type FsPlusBenchmarks () =

    let projectDir, projectOptions, checker = prepareProject "FSharpPlus" |> parseAndTypeCheckProject

    [<Benchmark>]
    member this.TypeCheckControlMonoid () =
        let filename = "Control" ++ "Monoid.fs"
        typeCheckFileInProject projectDir projectOptions checker filename

    [<Benchmark>]
    member this.TypeCheckDataFree () =
        let filename = "Data" ++ "Free.fs"
        typeCheckFileInProject projectDir projectOptions checker filename


[<BenchmarkCategory(FSharpCategory)>]
type FsToolkitBenchmarks () =

    let projectDir, projectOptions, checker = prepareProject "FsToolkit.ErrorHandling"

    let sourceFiles = [for file in projectOptions.SourceFiles do
                           file, SourceText.ofString (File.ReadAllText file)]

    let parseAllFiles =
        let parsingOptions, _diagnostics = checker.GetParsingOptionsFromProjectOptions projectOptions
        [for file, contents in sourceFiles do
            checker.ParseFile(file, contents, parsingOptions, cache=false)]

    [<Benchmark>]
    member this.ParseAndTypeCheckProject() =
        parseAndTypeCheckProject (projectDir, projectOptions, checker)

    [<IterationCleanup(Target = "ParseAndTypeCheckProject")>]
    member _.TypeCheckingCleanup() =
        checker.InvalidateAll()
        checker.ClearLanguageServiceRootCachesAndCollectAndFinalizeAllTransients()

    [<Benchmark>]
    member _.ParseAllFilesInProjectSequential() =
        parseAllFiles |> Async.Sequential |> Async.RunSynchronously

    [<Benchmark>]
    member _.ParseAllFilesInProjectParallel() =
        parseAllFiles |> Async.Parallel |> Async.RunSynchronously

    // [<Benchmark>]
    // How to avoid cache?
    member _.GetTooltip() =
        let file, contents = sourceFiles |> List.find (fun (file, _) -> file.EndsWith "AsyncResult.fs")
        let _parseResults, typeCheckAnswer = checker.ParseAndCheckFileInProject(file, 0, contents, projectOptions) |> Async.RunSynchronously
        match typeCheckAnswer with
        | FSharpCheckFileAnswer.Succeeded checkFileResults ->
            let result = checkFileResults.GetToolTip(173, 47, "        values |> Async.map (Result.requireHead error)", ["Result"; "requireHead"], FSharpTokenTag.Identifier)
            // printfn $"%A{result}"
            result
        | _ -> failwith "Type checking failed"

    // [<Benchmark>]
    member _.GetAutocompleteList() =
        let file, contents = sourceFiles |> List.find (fun (file, _) -> file.EndsWith "AsyncResult.fs")
        let parseResults, typeCheckAnswer = checker.ParseAndCheckFileInProject(file, 0, contents, projectOptions) |> Async.RunSynchronously
        match typeCheckAnswer with
        | FSharpCheckFileAnswer.Succeeded checkFileResults ->
            let result = checkFileResults.GetDeclarationListInfo(Some parseResults, 9, "    let inline retn (value: 'ok) : Async<Result<'ok, 'error>> = Ok value |> Async.", PartialLongName.Empty 82, (fun () -> []))
            // Doesn't work, dunno why...
            result
        | _ -> failwith "Type checking failed"


[<EntryPoint>]
let main args =
    let assembly = typeof<FsToolkitBenchmarks>.Assembly
    let artifactsPath = DirectoryInfo(Path.GetDirectoryName(assembly.Location) ++ "BenchmarkDotNet.Artifacts")
    let job  = Job.Default
                .WithWarmupCount(1) // 1 warmup is enough for our purpose
                .WithIterationTime(TimeInterval.FromMilliseconds(25000)) // the default is 0.5s per iteration, which is slighlty too much for us
                .WithMinIterationCount(15)
                .WithMaxIterationCount(20) // we don't want to run more that 20 iterations
                .DontEnforcePowerPlan(); // make sure BDN does not try to enforce High Performance power plan on Windows
    let config = ManualConfig.CreateEmpty()
                    .WithOptions(ConfigOptions.DisableOptimizationsValidator)
                    .WithBuildTimeout(TimeSpan.FromMinutes(15)) // for slow machines
                    .AddLogger(ConsoleLogger.Default) // log output to console
                    .AddValidator(DefaultConfig.Instance.GetValidators().ToArray()) // copy default validators
                    .AddAnalyser(DefaultConfig.Instance.GetAnalysers().ToArray()) // copy default analysers
                    .AddExporter(MarkdownExporter.GitHub) // export to GitHub markdown
                    .AddColumnProvider(DefaultColumnProviders.Instance) // display default columns (method name, args etc)
                    .AddJob(job.AsDefault()) // tell BDN that this are our default settings
                    .WithArtifactsPath(artifactsPath.FullName)
                    .AddDiagnoser(MemoryDiagnoser.Default) // MemoryDiagnoser is enabled by default
                    .AddExporter(JsonExporter.Full) // make sure we export to Json
                    .AddColumn(StatisticColumn.Median, StatisticColumn.Min, StatisticColumn.Max)
                    .WithSummaryStyle(SummaryStyle.Default.WithMaxParameterColumnWidth(36)); // the default is 20 and trims too aggressively some benchmark results
    BenchmarkSwitcher
        .FromAssembly(assembly)
        .Run(args, config) |> ignore
    0
