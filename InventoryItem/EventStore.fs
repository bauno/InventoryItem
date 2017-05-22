/// Integration with EventStore.
[<RequireQualifiedAccess>]
module EventStore

open System
open System.Net
open EventStore.ClientAPI
open Chessie.ErrorHandling

/// Creates and opens an EventStore connection.
let conn (endPoint: IPEndPoint )=
    let conn = EventStoreConnection.Create(endPoint)
    conn.ConnectAsync() |> Async.AwaitIAsyncResult |> Async.Ignore |> ignore
    conn

let streamId (category: string )(id:Guid) = category + "-" + id.ToString("N").ToLower()

let loadAsync (conn: IEventStoreConnection) (t,id)  = async {
  let streamId = streamId (t.GetType().Name) id
  let! eventsSlice = conn.ReadStreamEventsForwardAsync(streamId, 1L, 500, false)  |> Async.AwaitTask
  return eventsSlice,t;
}

let toEvent  (deserialize: Type * string * byte array -> obj) ((slice: StreamEventsSlice),t)  =
  slice.Events |> Seq.map ( fun e ->  deserialize(t, e.Event.EventType, e.Event.Data))


//conn, deserialize
let load'' (conn: IEventStoreConnection) (deserialize: Type * string * byte array -> obj) (category: string) (id:Guid) =
  let streamId (id:Guid) = category + "-" + id.ToString("N").ToLower()
  async {
    try
      let sId = streamId id
      let! slice = conn.ReadStreamEventsForwardAsync(sId, 1L, 500, false)  |> Async.AwaitTask
      let t = Type.GetType category
      let events = slice.Events |> Seq.map (fun e -> deserialize(t, e.Event.EventType, e.Event.Data))
      return! (ok events)
    with
      | :? AggregateException as ex -> return fail (sprintf "Error while reading aggregate from EventStore: %s" ex.InnerException.Message)
    }

/// Creates event store based repository.
let makeRepository
    (conn:IEventStoreConnection)
    category
    (serialize:obj -> string * byte array, deserialize: Type * string * byte array -> obj) =

    let streamId (id:Guid) = category + "-" + id.ToString("N").ToLower()

    let load (t,id) = async {
        try
            let streamId = streamId id
            let! eventsSlice = conn.ReadStreamEventsForwardAsync(streamId, 1L, 500, false)  |> Async.AwaitTask
            //printfn "Length: %d" eventsSlice.Events.Length
            return eventsSlice.Events |> Seq.map (fun e -> deserialize(t, e.Event.EventType, e.Event.Data)) |> Choice1Of2
        with
          | :? AggregateException as e ->
                return Choice2Of2 ([sprintf "Error while reading aggregate from EventStore: %s" e.InnerException.Message])
    }

    let commit (id,expectedVersion) e = async {
        let streamId = streamId id
        let eventType,data = serialize e
        let metaData = [||] : byte array
        let eventData = [EventData(Guid.NewGuid(), eventType, true, data, metaData)]
        let commitVersion = if expectedVersion = 0 then ExpectedVersion.Any else expectedVersion - 1
        try
          let! res = conn.AppendToStreamAsync(streamId, int64(commitVersion), eventData) |> Async.AwaitTask
          return Choice1Of2 ()
        with
          | :? AggregateException as e ->
                  return Choice2Of2 ([sprintf "Error while committing aggregate to EventStore: %s" e.InnerException.Message])
    }

    load,commit

/// Creates a function that returns a read model from the last event of a stream.
let makeReadModelGetter (conn:IEventStoreConnection) (deserialize:byte array -> _) =
    fun streamId -> async {
            let! eventsSlice = conn.ReadStreamEventsBackwardAsync(streamId, -1L, 1, false) |> Async.AwaitTask
            if eventsSlice.Status <> SliceReadStatus.Success then return Choice1Of2 None
            elif eventsSlice.Events.Length = 0 then return Choice1Of2 None
            else let lastEvent = eventsSlice.Events.[0]
                 if lastEvent.Event.EventNumber = 0L then return Choice1Of2 None
                 else return deserialize(lastEvent.Event.Data) |> Some |> Choice1Of2
    }
