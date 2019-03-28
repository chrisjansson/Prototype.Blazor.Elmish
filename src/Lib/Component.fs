module Component

open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open Html
open Microsoft.AspNetCore.Components
open Microsoft.JSInterop
open Elmish

type ComponentImpl() =
    inherit ComponentBase()
    
    let mutable state = None
    let mutable dispatchL = None
    let mutable tsc = new TaskCompletionSource<unit>()
    
    override this.OnInit() =
        Program.mkProgram App2.init App2.update App2.view
        |> Program.withSetState this.SetState
        |> Program.runWith None this.JsInterop
        |> Async.StartImmediate
        ()
        
    member this.SetState model dispatch =
        state <- Some model
        System.Console.WriteLine(model)
        dispatchL <- Some dispatch
        base.StateHasChanged()
        tsc.Task |> Async.AwaitTask
        
    [<Inject>]
    member val JsInterop: IJSRuntime = null with get, set
    
    member this.FocusElement (id: string) =
        this.JsInterop.InvokeAsync("focusElement", id)
    
    override this.OnAfterRender() =
        if not tsc.Task.IsCompleted then
            tsc.SetResult(())

    override this.BuildRenderTree(renderTreeBuilder) =
        base.BuildRenderTree(renderTreeBuilder)
        
        let seq () =
            let mutable v = 0
            let toUse = v
            v <- v + 1
            toUse
        
        let rec render dom =
            match dom with
            | DomAst.Content s ->
                renderTreeBuilder.AddContent(seq (), s)
            | DomAst.Element (el, attributes, children) ->
                renderTreeBuilder.OpenElement(seq (), el)
                
                for attribute in attributes do
                    match attribute with
                    | DomAttribute.String (attr, v) -> renderTreeBuilder.AddAttribute(seq (), attr, v)
                    | DomAttribute.Bool (attr, v) -> renderTreeBuilder.AddAttribute(seq(), attr, v)
                    | DomAttribute.MouseEvent (evnt, handler) -> renderTreeBuilder.AddAttribute(seq(), evnt, handler)
                    | DomAttribute.ChangeEvent (evnt, handler) -> renderTreeBuilder.AddAttribute(seq(), evnt, handler)
                    | DomAttribute.KeyboardEvent (evnt, handler) -> renderTreeBuilder.AddAttribute(seq(), evnt, handler)
                    | DomAttribute.UIEvent (evnt, handler) -> renderTreeBuilder.AddAttribute(seq(), evnt, handler)
                    
                for child in children do
                        render child
                
                renderTreeBuilder.CloseElement()
        
        match (state, dispatchL) with
        | Some s, Some d -> render (App2.view s d)
        | _ -> ()
        ()
        
        
    
    
