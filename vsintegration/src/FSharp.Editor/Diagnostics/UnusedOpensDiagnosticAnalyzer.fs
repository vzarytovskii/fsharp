// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System
open System.Composition
open System.Collections.Immutable
open System.Diagnostics
open System.Threading

open Microsoft.CodeAnalysis

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Text

open Microsoft.CodeAnalysis.ExternalAccess.FSharp.Diagnostics
open System.Threading.Tasks

[<Export(typeof<IFSharpUnusedOpensDiagnosticAnalyzer>)>]
type internal UnusedOpensDiagnosticAnalyzer
    [<ImportingConstructor>]
    (
    ) =

    static member GetUnusedOpenRanges(document: Document) : Task<range list> =
        backgroundTask {
            if document.Project.IsFSharpCodeFixesUnusedOpensEnabled then
                let! sourceText = document.GetTextAsync()
                let! _, checkResults = document.GetFSharpParseAndCheckResultsAsync(nameof(UnusedOpensDiagnosticAnalyzer))
                let! unusedOpens = UnusedOpens.getUnusedOpens(checkResults, fun lineNumber -> sourceText.Lines.[Line.toZ lineNumber].ToString())
                return unusedOpens
            else
                return List.empty
        } 

    interface IFSharpUnusedOpensDiagnosticAnalyzer with

        member _.AnalyzeSemanticsAsync(descriptor, document: Document, cancellationToken: CancellationToken) =
            if document.Project.IsFSharpMiscellaneousOrMetadata && not document.IsFSharpScript then
                Task.FromResult(ImmutableArray.Empty)
            else
                backgroundTask {
                    do Trace.TraceInformation("{0:n3} (start) UnusedOpensAnalyzer", DateTime.Now.TimeOfDay.TotalSeconds)
                    let! sourceText = document.GetTextAsync(cancellationToken)
                    let! unusedOpens = UnusedOpensDiagnosticAnalyzer.GetUnusedOpenRanges(document)
            
                    return
                        unusedOpens
                        |> List.map (fun range ->
                              Diagnostic.Create(
                                 descriptor,
                                 RoslynHelpers.RangeToLocation(range, sourceText, document.FilePath)))
                        |> Seq.toImmutableArray
                }
