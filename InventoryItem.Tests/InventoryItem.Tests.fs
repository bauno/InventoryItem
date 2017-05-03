module InventoryItem.Tests

open EventStore.ClientAPI
open EventStore.ClientAPI.Projections
open EventStore.ClientAPI.SystemData
open NUnit.Framework
open FsUnit

let address = System.Net.IPAddress.Parse("127.0.0.1");
let port = 1113
let endPoint = System.Net.IPEndPoint(address, port)

let conn = EventStoreConnection.Create endPoint
conn.ConnectAsync() |> Async.AwaitIAsyncResult |> Async.Ignore |> ignore

let handleCommand' =
    Aggregate.makeHandler
        { zero = InventoryItem.State.Zero; apply = InventoryItem.apply; exec = InventoryItem.exec }
        (EventStore.makeRepository conn "InventoryItem" Serialization.serializer)

let handleCommand (id,v) c = handleCommand' (id,v) c |> Async.RunSynchronously

let id = System.Guid.NewGuid()


let firstChoice =
  function
  | Choice1Of2 x -> x
  | Choice2Of2 error -> Assert.Fail error


[<Test>]
[<Order(1)>]
let createInventoryItem() =
    let version = 0
    InventoryItem.Create("Pool Pump")
    |> handleCommand (id,version)
    |> firstChoice
    |> should equal ()


[<Test>]
[<Order(2)>]
let renameInventoryItem() =
    let version = 1
    InventoryItem.Rename("Cooler Pool Pump")
    |> handleCommand (id,version)
    |> firstChoice
    |> should equal ()


[<Test>]
[<Order(3)>]
let checkInItemsItem() =
    let version = 2
    InventoryItem.CheckInItems(100)
    |> handleCommand (id,version)
    |> firstChoice
    |> should equal ()

[<Test>]
[<Order(4)>]
let removeItems() =
    let version = 3
    InventoryItem.RemoveItems(37)
    |> handleCommand (id,version)
    |> firstChoice
    |> should equal ()
