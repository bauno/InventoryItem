/// Aggregate framework.
[<RequireQualifiedAccess>]
module Aggregate

let paperino = 12

/// Represents an aggregate.4
type Aggregate<'TState, 'TCommand, 'TEvent> = {

    /// An initial state value.
    zero : 'TState;

    /// Applies an event to a state returning a new state.
    apply : 'TState -> 'TEvent -> 'TState;

    /// Executes a command on a state yielding an event.
    exec : 'TState -> 'TCommand -> Choice<'TEvent, string list>;
}

type Id = System.Guid

/// Creates a persistent, async command handler for an aggregate given load and commit functions.
let makeHandler (aggregate:Aggregate<'TState, 'TCommand, 'TEvent>) (load: System.Type * Id -> Async<Choice<obj seq,string list>>, commit: Id * int -> obj -> Async<Choice<unit, string list>>) =
    fun (id,version) command -> async {
        let! events = load (typeof<'TEvent>,id)
        let res = match events with
                    | Choice2Of2 s -> Choice2Of2 s
                    | Choice1Of2 es -> let events = es |> Seq.cast :> 'TEvent seq        
                                       let state = Seq.fold aggregate.apply aggregate.zero events
                                       let event = aggregate.exec state command                                       
                                       match event with
                                       | Choice1Of2 event -> let res  = event |> commit (id,version) |> Async.RunSynchronously
                                                             match res with
                                                             | Choice1Of2 x -> Choice1Of2 x                                                                               
                                                             | Choice2Of2 e -> Choice2Of2 e                                          
                                       | Choice2Of2 s -> Choice2Of2 s
        return res                
    }

/// Creates a persistent command handler for an aggregate given load and commit functions.
let makeHandlerSync (aggregate:Aggregate<'TState, 'TCommand, 'TEvent>) (load:System.Type * Id -> obj seq, commit:Id * int -> obj -> unit) =
    fun (id,version) command ->
        let events = load (typeof<'TEvent>,id) |> Seq.cast :> 'TEvent seq
        let state = Seq.fold aggregate.apply aggregate.zero events
        let result = aggregate.exec state command
        match result with
        | Choice1Of2 event  -> event |> commit (id,version)   |> Choice1Of2
        | Choice2Of2 errors -> errors |> Choice2Of2
