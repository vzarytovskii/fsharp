// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System.Threading
open System.ComponentModel.Composition

open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.ExternalAccess.FSharp.Navigation

open Microsoft.VisualStudio.Language.Intellisense
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Utilities
open CancellableTasks

[<AllowNullLiteral>]
type internal FSharpNavigableSymbol(item: FSharpNavigableItem, span: SnapshotSpan, gtd: GoToDefinition) =
    interface INavigableSymbol with
        member _.Navigate(_: INavigableRelationship) =
            gtd.NavigateToItem(item, CancellationToken.None)

        member _.Relationships = seq { yield PredefinedNavigableRelationships.Definition }

        member _.SymbolSpan = span

type internal FSharpNavigableSymbolSource(metadataAsSource) =
    let mutable disposed = false
    let gtd = GoToDefinition(metadataAsSource)

    interface INavigableSymbolSource with
        member _.GetNavigableSymbolAsync(triggerSpan: SnapshotSpan, cancellationToken: CancellationToken) =
            // Yes, this is a code smell. But this is how the editor API accepts what we would treat as None.
            if disposed then
                null
            else
                cancellableTask {
                    let snapshot = triggerSpan.Snapshot
                    let position = triggerSpan.Start.Position
                    let document = snapshot.GetOpenDocumentInCurrentContextWithChanges()
                    
                    let! cancellationToken = CancellableTask.getCurrentCancellationToken ()
                    
                    let! sourceText = document.GetTextAsync(cancellationToken)

                    let! gtdResult = gtd.FindDefinitionTask(document, position, cancellationToken)
                    match gtdResult with
                    | None -> return null
                    | Some (result, range) ->

                        let declarationTextSpan = RoslynHelpers.FSharpRangeToTextSpan(sourceText, range)
                        let declarationSpan = Span(declarationTextSpan.Start, declarationTextSpan.Length)
                        let symbolSpan = SnapshotSpan(snapshot, declarationSpan)
                        match result with
                            | FSharpGoToDefinitionResult.NavigableItem (navItem) ->
                                return FSharpNavigableSymbol(navItem, symbolSpan, gtd) :> INavigableSymbol

                            | FSharpGoToDefinitionResult.ExternalAssembly (targetSymbolUse, metadataReferences) ->
                                let nav =
                                    { new INavigableSymbol with
                                        member _.Navigate(_: INavigableRelationship) =
                                            // Need to new up a CTS here instead of re-using the other one, since VS
                                            // will navigate disconnected from the outer routine, leading to an
                                            // OperationCancelledException if you use the one defined outside.
                                            use ct = new CancellationTokenSource()
                                            gtd.NavigateToExternalDeclaration(targetSymbolUse, metadataReferences, ct.Token) |> ignore

                                        member _.Relationships = seq { yield PredefinedNavigableRelationships.Definition }

                                        member _.SymbolSpan = symbolSpan
                                    }

                                return nav
                }
                |> CancellableTask.start cancellationToken

        member _.Dispose() = disposed <- true

[<Export(typeof<INavigableSymbolSourceProvider>)>]
[<Name("F# Navigable Symbol Service")>]
[<ContentType(Constants.FSharpContentType)>]
[<Order>]
type internal FSharpNavigableSymbolService [<ImportingConstructor>] (metadataAsSource: FSharpMetadataAsSourceService) =

    interface INavigableSymbolSourceProvider with
        member _.TryCreateNavigableSymbolSource(_: ITextView, _: ITextBuffer) =
            new FSharpNavigableSymbolSource(metadataAsSource)
