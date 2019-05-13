# Blazor Elmish (Blemish obviosuly)
I had some Blazor fomo and want to check it out a bit. 
Here I investigated how an Elm like programming model could fit into the existing Blazor infrastructure. 

An example of an Elmish Blazor view:
        
    let viewInput (task : string) =
        header [ className "header" ] [
            h1 [] [ str "todos" ]
            input [ className "new-todo"; _type "text"; placeholder "What needs to be done?"; value task; name "newTodo"; onInput UpdateField; onEnter Add NoOp ]
        ]

The existing foundation of Elmish was used as a starting point for the Model-Update part of TEA. 
The only notable changes were to the Cmd and Subscription types to be over a reader monad so that effects could be written against Blazors JS interop.

