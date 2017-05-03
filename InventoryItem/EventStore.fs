/// Integration with EventStore.
[<RequireQualifiedAccess>]
module EventStore

open System
open System.Net
open EventStore.ClientAPI

/// Creates and opens an EventStore connection.
let conn (endPoint: IPEndPoint )=
    let conn = EventStoreConnection.Create(endPoint)
    conn.ConnectAsync() |> Async.AwaitIAsyncResult |> Async.Ignore |> ignore
    conn

/// Creates event store based repository.
let makeRepository
    (conn:IEventStoreConnection)
    category
    (serialize:obj -> string * byte array, deserialize: Type * string * byte array -> obj) =

    let streamId (id:Guid) = category + "-" + id.ToString("N").ToLower()

    let load (t,id) = async {
        let streamId = streamId id
        let! eventsSlice = conn.ReadStreamEventsForwardAsync(streamId, 1L, 500, false)  |> Async.AwaitTask
        return eventsSlice.Events |> Seq.map (fun e -> deserialize(t, e.Event.EventType, e.Event.Data))
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
                  return Choice2Of2 (sprintf "Error while committing aggregate to EventStore: %s" e.InnerException.Message)
    }

    load,commit

/// Creates a function that returns a read model from the last event of a stream.
let makeReadModelGetter (conn:IEventStoreConnection) (deserialize:byte array -> _) =
    fun streamId -> async {
        let! eventsSlice = conn.ReadStreamEventsBackwardAsync(streamId, -1L, 1, false) |> Async.AwaitTask
        if eventsSlice.Status <> SliceReadStatus.Success then return None
        elif eventsSlice.Events.Length = 0 then return None
        else
            let lastEvent = eventsSlice.Events.[0]
            if lastEvent.Event.EventNumber = 0L then return None
            else return Some(deserialize(lastEvent.Event.Data))
    }
