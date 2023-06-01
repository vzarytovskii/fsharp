// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System
open System.Composition
open System.Threading
open System.Threading.Tasks

open FSharp.Compiler.Text.Range

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.ExternalAccess.FSharp.Editor
open CancellableTasks
open Microsoft.VisualStudio.Shell

[<Export(typeof<IFSharpGoToDefinitionService>)>]
[<Export(typeof<FSharpGoToDefinitionService>)>]
type internal FSharpGoToDefinitionService [<ImportingConstructor>] (metadataAsSource: FSharpMetadataAsSourceService) =

    interface IFSharpGoToDefinitionService with
        /// Invoked with Peek Definition.
        member _.FindDefinitionsAsync(document: Document, position: int, cancellationToken: CancellationToken) =
            let navigation = FSharpNavigation(metadataAsSource, document, rangeStartup)

            navigation.FindDefinitions(position, cancellationToken) |> Task.FromResult

        /// Invoked with Go to Definition.
        /// Try to navigate to the definiton of the symbol at the symbolRange in the originDocument
        member _.TryGoToDefinition(document: Document, position: int, cancellationToken: CancellationToken) =
            let gtdTask =
                cancellableTask {
                    let navigation = FSharpNavigation(metadataAsSource, document, rangeStartup)

                    return! navigation.TryGoToDefinition(position)
                }

            ThreadHelper.JoinableTaskFactory.Run(
               SR.NavigatingTo(),
               (fun _progress ct ->
                    let cts = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken, ct)
                    gtdTask cts.Token),
               TimeSpan.FromSeconds 1)