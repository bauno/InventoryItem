module InventoryItemRop.Tests

open EventStore.ClientAPI
open EventStore.ClientAPI.Projections
open EventStore.ClientAPI.SystemData
open NUnit.Framework
open FsUnit
open Chessie.ErrorHandling

let address = System.Net.IPAddress.Parse("127.0.0.1");
let port = 1113
let endPoint = System.Net.IPEndPoint(address, port)

let conn = EventStoreConnection.Create endPoint
conn.ConnectAsync() |> Async.AwaitIAsyncResult |> Async.Ignore |> ignore

let ser, des = Serialization.serializer 

let id = System.Guid.NewGuid()

let loadSync = EventStore.loadSync'' conn des "InventoryItem"

let load = EventStore.load'' conn des "InventoryItem"

let upCast (eventSeq: obj seq) : Async<Result<InventoryItem.Event seq, string>> =
    async {
     return eventSeq |> Seq.cast :> InventoryItem.Event seq |> ok
    }

let upCastSync (eventSeq: obj seq) =    
     eventSeq |> Seq.cast :> InventoryItem.Event seq |> ok

let pippo id = 
    id
    |> loadSync
    >>= upCastSync


let topolino id = 
    let qui = loadSync id 
    let quo = bind upCastSync qui
    quo



let pluto id = 
    let qui = load id
    let input = [obj()] |> Async.singleton 
    let input1 : Result<_,string> =  input |> ok
    let quo = Async.bind upCast (input)
    quo

let basettoni id = 
    asyncTrial {
        let qui = load id
        //let quo = bi
        Zero()
    }

    