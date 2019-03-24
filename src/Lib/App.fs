module App2

// MODEL

// The full application state of our todo app.
type Model =
    {
        Entries: Entry list
        Field : string
        Uid : int
        Visibility : string
    }
and Entry =
    {
        Description : string
        Completed : bool
        Editing : bool
        Id : int
    }

let emptyModel: Model =
    {
        Entries = []
        Visibility = "All"
        Field = ""
        Uid = 0
    }
    
let newEntry desc id =
    {
        Description = desc
        Completed = false
        Editing = false
        Id = id
    }
    
open Elmish

let init maybeModel = ( Option.defaultValue emptyModel maybeModel , Cmd.none )

// UPDATE


(**  Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them
**)
type Msg =
    | NoOp
    | UpdateField of string
    | EditingEntry of int * bool
    | UpdateEntry of int * string
    | Add
    | Delete of int
    | DeleteComplete
    | Check of int * bool
    | CheckAll of bool
    | ChangeVisibility of string

// How we update our Model on a given Msg?
let update msg model =
    match msg with
    | NoOp ->
            ( model, Cmd.none )
    | Add ->
            ( { model with
                    Uid = model.Uid + 1
                    Field = ""
                    Entries =
                        if System.String.IsNullOrEmpty(model.Field) then
                            model.Entries
                        else
                            model.Entries @ [ newEntry model.Field model.Uid ]
              }
            , Cmd.none)
    | UpdateField str ->
            ( { model with Field = str } , Cmd.none )
    | EditingEntry (id, isEditing) ->
        let updateEntry t =
            if t.Id = id then
                { t with Editing = isEditing }
            else
                t

        //TODO: what happens here?
//        let focus = Dom.focus ("todo-" ++ String.fromInt id)
//                
//        ({ model with Entries = List.map updateEntry model.Entries } , Task.attempt (fun _ -> NoOp) focus)
        ({ model with Entries = List.map updateEntry model.Entries } , Cmd.none)
    | UpdateEntry (id, task) ->
        let
            updateEntry t =
                if t.Id = id then
                    { t with Description = task }
                else
                    t
        ( { model with Entries = List.map updateEntry model.Entries } , Cmd.none )
    | Delete id ->
            ( { model with Entries = List.filter (fun t -> t.Id <> id) model.Entries } , Cmd.none )
    | DeleteComplete ->
            ( { model with Entries = List.filter (fun t -> not t.Completed) model.Entries } , Cmd.none )
    | Check (id, isCompleted) ->
        let updateEntry t =
            if t.Id = id then
                { t with Completed = isCompleted }
            else
                t
        ( { model with Entries = List.map updateEntry model.Entries } , Cmd.none )
    | CheckAll isCompleted ->
        let updateEntry t =
            { t with Completed = isCompleted }
        ( { model with Entries = List.map updateEntry model.Entries } , Cmd.none )
    | ChangeVisibility visibility ->
        ( { model with Visibility = visibility } , Cmd.none )

// VIEW

open Html
open Microsoft.AspNetCore.Components

let onEnter msg nop =
    let inner (event: UIKeyboardEventArgs) =
        if event.Code = "Enter" then
            msg
        else
            nop
        
    Html.onKeyDown inner

let viewInput (task: string) =
    header [ className "header" ] [
        h1 [] [ str "todos" ]
        input [ className "new-todo"; _type "text"; placeholder "What needs to be done?"; autoFocus; value task; name "newTodo"; onInput UpdateField; onEnter Add NoOp ]
    ]
   


// VIEW CONTROLS AND FOOTER



//viewControlsCount : Int -> Html Msg
let viewControlsCount entriesLeft =
    let item_ =
        if entriesLeft = 1 then
            " item"
        else
            " items"
    span [ className "todo-count" ] [
        strong [] [ str (string entriesLeft) ]
        str (item_ + " left")
    ]

let visibilitySwap (uri: string) (visibility: string) (actualVisibility: string) =
    li [ onClick (ChangeVisibility visibility) ] [
            a [ href uri; classList [ ( "selected", visibility = actualVisibility ) ] ] [ str visibility ]
    ]

let viewControlsFilters (visibility: string) =
    ul [ className "filters" ] [
        visibilitySwap "#/" "All" visibility
        str " "
        visibilitySwap "#/active" "Active" visibility
        str " "
        visibilitySwap "#/completed" "Completed" visibility
    ]

let viewControlsClear entriesCompleted =
    button [ className "clear-completed"; hidden (entriesCompleted = 0); onClick DeleteComplete ]
        [ str ("Clear completed (" + (string entriesCompleted) + ")")
    ]

let viewControls (visibility: string) (entries: Entry list) =
    let entriesCompleted =
        List.length (List.filter (fun e -> e.Completed) entries)

    let entriesLeft =
        List.length entries - entriesCompleted
    
    footer [ className "footer"; hidden (List.isEmpty entries); ] [
        viewControlsCount entriesLeft
        viewControlsFilters visibility
        viewControlsClear entriesCompleted
    ]
    
// VIEW INDIVIDUAL ENTRIES


//viewEntry : Entry -> Html Msg
let viewEntry (todo: Entry) =
    li [ classList [ ( "completed", todo.Completed ); ( "editing", todo.Editing ) ] ] [
        div [ className "view" ] [
            input [ className "toggle"; _type "checkbox"; _checked todo.Completed; onClick (Check (todo.Id, not todo.Completed)) ]
            label [ onDoubleClick (EditingEntry (todo.Id, true)) ] [ str todo.Description ]
            button [ className "destroy"; onClick (Delete todo.Id) ] []
        ]
        input [ className "edit"; value todo.Description; name "title"; id ("todo-" + (string todo.Id)); onInput (fun s -> UpdateEntry (todo.Id, s)); onBlur (EditingEntry (todo.Id, false)); onEnter (EditingEntry (todo.Id, false)) NoOp ]
    ]

//viewKeyedEntry : Entry -> ( String, Html Msg )
let viewKeyedEntry (todo: Entry) =
//    ( string todo.Id, lazy viewEntry todo )
//    ( string todo.Id, viewEntry todo )
    viewEntry todo

    
// VIEW ALL ENTRIES


//viewEntries : String -> List Entry -> Html Msg
let viewEntries (visibility: string) (entries: Entry list) =
    let isVisible todo =
        match visibility with
        | "Completed" -> todo.Completed
        | "Active" -> not todo.Completed
        | _ -> true

    let allCompleted = List.forall (fun e -> e.Completed) entries

    let cssVisibility =
        if List.isEmpty entries then
            "hidden"
        else
            "visible"
            
    section [ className "main"; style (sprintf "visibility: %s" cssVisibility) ] [
        input [ className "toggle-all"; _type "checkbox"; name "toggle"; _checked allCompleted; onClick (CheckAll (not allCompleted)) ]
        label [ _for "toggle-all" ] [ str "Mark all as complete" ]
//        Keyed.ul [ className "todo-list" ] <| List.map viewKeyedEntry (List.filter isVisible entries)
        ul [ className "todo-list" ] <| List.map viewKeyedEntry (List.filter isVisible entries)
    ]

let infoFooter =
    footer [ className "info" ] [
        p [] [ str "Double-click to edit a todo" ]
        p [] [
            str "Written by "
            a [ href "https://github.com/evancz" ] [ str "Evan Czaplicki" ]
        ]
        p [] [
            str "Part of "
            a [ href "http://todomvc.com" ] [ str "TodoMVC" ]
        ]
    ]


//TODO: style in elm?

//view : Model -> Html Msg
let view (model: Model) =
    div [] [
        section [ className "todoapp" ] [
//todo: lazy
//                lazy viewInput model.field
//                lazy2 viewEntries model.visibility model.entries
//                lazy2 viewControls model.visibility model.entries
            
            viewInput model.Field
            viewEntries model.Visibility model.Entries
            viewControls model.Visibility model.Entries
        ]
        infoFooter
    ]
