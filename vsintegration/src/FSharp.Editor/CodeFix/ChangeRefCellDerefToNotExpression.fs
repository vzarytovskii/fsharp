// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System.Composition
open System.Threading.Tasks

open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.CodeFixes
open FSharp.Compiler.CodeAnalysis

[<ExportCodeFixProvider(FSharpConstants.FSharpLanguageName, Name = "ChangeRefCellDerefToNotExpression"); Shared>]
type internal FSharpChangeRefCellDerefToNotExpressionCodeFixProvider
    [<ImportingConstructor>]
    (
    ) =
    inherit CodeFixProvider()
    
    let fixableDiagnosticIds = set ["FS0001"]

    let tryGetDerefTextSpan (parseResults: FSharpParseFileResults) filepath span sourceText =
        let errorRange = RoslynHelpers.TextSpanToFSharpRange(filepath, span, sourceText)
        let derefRange = parseResults.TryRangeOfRefCellDereferenceContainingPos errorRange.Start
        derefRange |> Option.bind (fun derefRange ->
           RoslynHelpers.TryFSharpRangeToTextSpan(sourceText, derefRange)
        )

    override _.FixableDiagnosticIds = Seq.toImmutableArray fixableDiagnosticIds

    override _.RegisterCodeFixesAsync context : Task =
        backgroundTask {
            let document = context.Document
            let! parseResults = document.GetFSharpParseResultsAsync(nameof(FSharpChangeRefCellDerefToNotExpressionCodeFixProvider))
            let! sourceText = context.Document.GetTextAsync(context.CancellationToken)

            let derefSpan = tryGetDerefTextSpan parseResults document.FilePath context.Span sourceText
            
            if derefSpan.IsNone then
                return ()

            let derefSpan = derefSpan.Value
            
            let title = SR.UseNotForNegation()

            let diagnostics =
                context.Diagnostics
                |> Seq.filter (fun x -> fixableDiagnosticIds |> Set.contains x.Id)
                |> Seq.toImmutableArray

            let codeFix =
                CodeFixHelpers.createTextChangeCodeFix(
                    title,
                    context,
                    (fun () -> asyncMaybe.Return [| TextChange(derefSpan, "not ") |]))

            context.RegisterCodeFix(codeFix, diagnostics)
        }