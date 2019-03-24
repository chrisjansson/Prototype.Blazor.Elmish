module Component

open Html
open Microsoft.AspNetCore.Components
open Microsoft.AspNetCore.Components;
open Microsoft.JSInterop

type ComponentImpl() =
    inherit ComponentBase()
    
    let mutable state = None
    let mutable dispatchL = None
    
    override this.OnInit() =
        let program = Elmish.Program.mkProgram App2.init App2.update App2.view
        let program =
            { program with setState = this.SetState  }
            
        Elmish.Program.runWith None program 
        ()
        
    member this.SetState model dispatch =
        state <- Some model
        System.Console.WriteLine(model)
        dispatchL <- Some dispatch
        base.StateHasChanged()
        
    [<Inject>]
    member val JsInterop: IJSRuntime = null with get, set
    
    member this.FocusElement (id: string) =
        this.JsInterop.InvokeAsync("focusElement", id)
    
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
        
        
    
    
