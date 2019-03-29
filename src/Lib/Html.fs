module Html
open Microsoft.AspNetCore.Components
open Microsoft.AspNetCore.Components
open Microsoft.AspNetCore.Components
open System.Threading.Tasks

[<RequireQualifiedAccess>]
type DomAst =
    | Content of string
    | Element of string * DomAttribute array * (DomAst seq)
    | Component of System.Type * ((string * obj) list)

and DomAttribute =
    | String of string * string
    | Bool of string * bool
    | MouseEvent of string * System.Func<UIMouseEventArgs, Task>
    | KeyboardEvent of string * System.Func<UIKeyboardEventArgs, Task>
    | ChangeEvent of string * System.Func<UIChangeEventArgs, Task>
    | UIEvent of string * System.Func<UIEventArgs, Task>
    
open System
open Elmish

type Attr<'msg> = 
    | OnClick of 'msg
    | OnInput of (string -> 'msg)
    | Type of string
    | Value of string
    | ClassName of string
    | Placeholder of string
    | ReadOnly
    | Disabled
    | DisabledB of bool
    | AutoFocus
    | OnDoubleClick of 'msg
    | OnKeyDown of (UIKeyboardEventArgs -> 'msg)
    | Id of string
    | Name of string
    | OnBlur of 'msg
    | Style of string
    | Checked of bool
    | For of string
    | Hidden of bool
    | Href of string

let inline onClick msg = OnClick msg
let inline _type t = Type t
let inline value v = Value v
let inline onInput f = OnInput f
let autoFocus = AutoFocus
let inline className<'msg> cn: Attr<'msg> = ClassName cn
let inline placeholder text = Placeholder text
let readonly = ReadOnly
let disabled = Disabled
let onKeyDown f =  OnKeyDown f
let onBlur f =  OnBlur f
let onDoubleClick f =  OnDoubleClick f
let id s = Id s
let name s = Name s
let inline style s = Style s
let _checked b = Checked b
let _for s = For s
let hidden b = Hidden b
let href s = Href s

type Html<'msg> = Dispatch<'msg> -> DomAst

let convertToProp attr dispatch =
    let onChangeR (e: UIChangeEventArgs) f =
        printf "change"
        let value = e.Value :?> string
        let msg = f value
        dispatch msg |> Async.StartAsTask :> Task
        
    let onClick msg e =
        dispatch msg |> Async.StartAsTask :> Task

    match attr with
    | OnClick msg -> DomAttribute.MouseEvent ("onclick", System.Func<UIMouseEventArgs, Task> (onClick msg))
    | OnDoubleClick msg -> DomAttribute.MouseEvent ("ondblclick", System.Func<UIMouseEventArgs, Task> (onClick msg))
    | OnKeyDown msg -> DomAttribute.KeyboardEvent ("onkeydown", System.Func<UIKeyboardEventArgs, Task> (fun e -> (dispatch (msg e) |> Async.StartAsTask) :> Task))
    | Type t -> DomAttribute.String ("type", t)
    | Value v -> DomAttribute.String ("value", v) //v :> IHTMLProp
    | OnInput f -> DomAttribute.ChangeEvent ("oninput", System.Func<UIChangeEventArgs, Task>(fun e -> onChangeR e f)) // Props.OnChange (fun e -> onChangeR e f) :> IHTMLProp
    | ClassName cn -> DomAttribute.String ("class", cn)  
    | Placeholder text -> DomAttribute.String ("placeholder", text) 
    | ReadOnly -> DomAttribute.Bool ("readonly", true)
    | Disabled -> DomAttribute.Bool ("disabled", true) 
    | DisabledB b -> DomAttribute.Bool ("disabled", b)
    | AutoFocus -> DomAttribute.Bool ("autofocus", true)
    | Id s -> DomAttribute.String ("id", s)
    | Name s -> DomAttribute.String ("name", s)
    | OnBlur s -> DomAttribute.UIEvent ("onblur", System.Func<UIEventArgs, Task>(fun e -> dispatch s|> Async.StartAsTask :> Task))
    | Style s -> DomAttribute.String ("style", s)
    | Checked b -> DomAttribute.Bool ("checked", b)
    | For s -> DomAttribute.String ("for", s)
    | Hidden b -> DomAttribute.Bool("hidden", b)
    | Href s -> DomAttribute.String("href", s)

let convertToProps (props: Attr<_> seq) dispatch =
    Seq.map (fun p -> convertToProp p dispatch) props
    |> Array.ofSeq

let applyDispatch (elements: Html<'msg> seq) (dispatch: Dispatch<'msg>) = 
    Seq.map (fun e -> e dispatch) elements

let inline str (str: string): Html<'msg> =
    fun _ -> DomAst.Content str
    
let input (props: Attr<'msg> seq): Html<'msg> =
    fun d -> DomAst.Element ("input", convertToProps props d, [])

let div (props: Attr<'msg> seq) (children: Html<'msg> seq): Html<'msg> =
    fun d -> DomAst.Element ("div", convertToProps props d, applyDispatch children d)

let label (props: Attr<'msg> seq) (children: Html<'msg> seq): Html<'msg> =
    fun d -> DomAst.Element ("label", convertToProps props d, applyDispatch children d)
        
let h1 (props: Attr<'msg> seq) (children: Html<'msg> seq): Html<'msg> =
    fun d -> DomAst.Element ("h1", convertToProps props d, applyDispatch children d)
        
let header (props: Attr<'msg> seq) (children: Html<'msg> seq): Html<'msg> =
    fun d -> DomAst.Element ("header", convertToProps props d, applyDispatch children d)
        
let h4 (props: Attr<'msg> seq) (children: Html<'msg> seq): Html<'msg> =
    fun d -> DomAst.Element ("h4", convertToProps props d, applyDispatch children d)
     
let span (props: Attr<'msg> seq) (children: Html<'msg> seq): Html<'msg> =
    fun d -> DomAst.Element ("span", convertToProps props d, applyDispatch children d)

let button (props: Attr<'msg> seq) (children: Html<'msg> seq): Html<'msg> =
    fun d -> DomAst.Element ("button", convertToProps props d, applyDispatch children d)

let pre (props: Attr<'msg> seq) (children: Html<'msg> seq): Html<'msg> =
    fun d -> DomAst.Element ("pre", convertToProps props d, applyDispatch children d)

let fieldset (props: Attr<'msg> seq) (children: Html<'msg> seq): Html<'msg> =
    fun d -> DomAst.Element ("fieldset", convertToProps props d, applyDispatch children d)
    
let li (props: Attr<'msg> seq) (children: Html<'msg> seq): Html<'msg> =
    fun d -> DomAst.Element ("li", convertToProps props d, applyDispatch children d)

let ul (props: Attr<'msg> seq) (children: Html<'msg> seq): Html<'msg> =
    fun d -> DomAst.Element ("ul", convertToProps props d, applyDispatch children d)

let section (props: Attr<'msg> seq) (children: Html<'msg> seq): Html<'msg> =
    fun d -> DomAst.Element ("section", convertToProps props d, applyDispatch children d)

let a (props: Attr<'msg> seq) (children: Html<'msg> seq): Html<'msg> =
    fun d -> DomAst.Element ("a", convertToProps props d, applyDispatch children d)

let footer (props: Attr<'msg> seq) (children: Html<'msg> seq): Html<'msg> =
    fun d -> DomAst.Element ("footer", convertToProps props d, applyDispatch children d)

let strong (props: Attr<'msg> seq) (children: Html<'msg> seq): Html<'msg> =
    fun d -> DomAst.Element ("strong", convertToProps props d, applyDispatch children d)

let p (props: Attr<'msg> seq) (children: Html<'msg> seq): Html<'msg> =
    fun d -> DomAst.Element ("p", convertToProps props d, applyDispatch children d)

let run (html: Html<'msg>) (dispatch: Dispatch<'msg>) =
    html dispatch

let runChildren (children: Html<'msg> seq) (dispatch: Dispatch<'msg>) = applyDispatch children dispatch

let map (mapper: 'msgA -> 'msgB) (html: Html<'msgA>): Html<'msgB> =
    fun d -> html (fun m -> d (mapper m))

type LazyComponent<'model, 'msg when 'model : equality>()  =
    inherit ComponentBase()

    let mutable oldModel = Unchecked.defaultof<'model>
    
    [<Parameter>]
    member val Model = Unchecked.defaultof<'model> with get, set

    [<Parameter>]    
    member val View = Unchecked.defaultof<'model -> Html<'msg>> with get, set
    
    [<Parameter>]    
    member val Dispatch = Unchecked.defaultof<Dispatch<'msg>> with get, set

    override this.ShouldRender() =
        oldModel <> this.Model

    override this.OnAfterRender() =
        oldModel <- this.Model
            
    override this.BuildRenderTree(renderTreeBuilder) =
        printfn "render!!"
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
            | DomAst.Component (c, attributes) ->
                renderTreeBuilder.OpenComponent(seq (), c)
                
                for (n, v) in attributes do
                    renderTreeBuilder.AddAttribute(seq(), n, v)
                
                renderTreeBuilder.CloseComponent()
    
        if this.Model = Unchecked.defaultof<'model> || Object.ReferenceEquals(this.Dispatch, Unchecked.defaultof<Dispatch<'msg>>) then
            ()
        else
            render (this.View this.Model this.Dispatch)


let lazy1<'msg, 'model when 'model : equality> (view: 'model -> Html<'msg>) (model: 'model): Html<'msg> =
    let t = typeof<LazyComponent<'model, 'msg>>
    fun d -> DomAst.Component (t, [ "View", view :> obj; "Model", model :> obj; "Dispatch", d :> obj ])
    
    
let lazy2<'msg, 'm1, 'm2 when 'm1 : equality and 'm2 : equality> (view: 'm1 -> 'm2 -> Html<'msg>) (m1: 'm1) (m2: 'm2): Html<'msg> =
    let t = typeof<LazyComponent<'m1 * 'm2, 'msg>>
    let v (m1, m2) = view m1 m2
    let m = m1, m2
    fun d -> DomAst.Component (t, [ "View", v :> obj; "Model", m :> obj; "Dispatch", d :> obj ])
    
let classList (classes: (string * bool) list) =
    let visibleClasses =
        classes
        |> List.filter (fun (_, visible) -> visible)
        |> List.map fst
    
    className (System.String.Join(" ", visibleClasses))
    
module Dom =
    open Microsoft.JSInterop

    let focus (id: string) =
      let sub _ (f: IJSRuntime) =
            f.InvokeAsync("focusElement", id) |> Async.AwaitTask
      Cmd.ofSub sub