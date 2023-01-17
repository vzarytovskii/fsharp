// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Editor.Tests

open System
open System.Collections.Generic
open NUnit.Framework
open Microsoft.CodeAnalysis.Text
open Microsoft.VisualStudio.FSharp.Editor
open FSharp.Compiler.CodeAnalysis
open FSharp.Editor.Tests.Helpers

// AppDomain helper
type Worker() =

    let filePath = "C:\\test.fsx"

    let projectOptions =
        {
            ProjectFileName = "C:\\test.fsproj"
            ProjectId = None
            SourceFiles = [| filePath |]
            ReferencedProjects = [||]
            OtherOptions = [||]
            IsIncompleteTypeCheckEnvironment = true
            UseScriptResolutionRules = true
            LoadTime = DateTime.MaxValue
            OriginalLoadReferences = []
            UnresolvedReferences = None
            Stamp = None
        }

    member _.VerifyCompletionListExactly(fileContents: string, marker: string, expected: List<string>) =
        let caretPosition = fileContents.IndexOf(marker) + marker.Length

        let document =
            RoslynTestHelpers.CreateSingleDocumentSolution(filePath, SourceText.From(fileContents), options = projectOptions)

        let expected = expected |> Seq.toList

        let actual =
            let x =
                FSharpCompletionProvider.ProvideCompletionsAsyncAux(document, caretPosition, (fun _ -> [])).Result
                

            x
            |> Seq.toList
            // sort items as Roslyn do - by `SortText`
            |> List.sortBy (fun x -> x.SortText)

        let actualNames = actual |> List.map (fun x -> x.DisplayText)

        if actualNames <> expected then
            Assert.Fail(
                sprintf
                    "Expected:\n%s,\nbut was:\n%s\nactual with sort text:\n%s"
                    (String.Join("; ", expected |> List.map (sprintf "\"%s\"")))
                    (String.Join("; ", actualNames |> List.map (sprintf "\"%s\"")))
                    (String.Join("\n", actual |> List.map (fun x -> sprintf "%s => %s" x.DisplayText x.SortText)))
            )

module FsxCompletionProviderTests =

    let getWorker () = Worker()

    [<Test>]
#if RELEASE
    [<Ignore "Fails in some CI, reproduces locally in Release mode, needs investigation">]
#endif
    let fsiShouldTriggerCompletionInFsxFile () =
        let fileContents =
            """
    fsi.
    """

        let expected =
            List<string>(
                [
                    "CommandLineArgs"
                    "EventLoop"
                    "FloatingPointFormat"
                    "FormatProvider"
                    "PrintDepth"
                    "PrintLength"
                    "PrintSize"
                    "PrintWidth"
                    "ShowDeclarationValues"
                    "ShowIEnumerable"
                    "ShowProperties"
                    "AddPrinter"
                    "AddPrintTransformer"
                    "Equals"
                    "GetHashCode"
                    "GetType"
                    "ToString"
                ]
            )

        // We execute in a seperate appdomain so that we can set BaseDirectory to a non-existent location
        getWorker().VerifyCompletionListExactly(fileContents, "fsi.", expected)
