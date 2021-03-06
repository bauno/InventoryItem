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

let itemRepo = (EventStore.makeRepository conn "InventoryItem" Serialization.serializer)

let handleCommand' =
    Aggregate.makeHandler
        { zero = InventoryItem.State.Zero; apply = InventoryItem.apply; exec = InventoryItem.exec }
        itemRepo

let handleCommand (id,v) c = handleCommand' (id,v) c |> Async.RunSynchronously

let id = System.Guid.NewGuid()


let firstChoice =
  function
  | Choice1Of2 x -> x
  | Choice2Of2 (errors: string list) -> Assert.Fail errors.[0]


[<Test>]
[<Order(0)>]
let initProjections() =
    let pm = new ProjectionsManager(new EventStore.ClientAPI.Common.Log.ConsoleLogger(), endPoint, System.TimeSpan.FromSeconds(10.0))
    let file p = System.IO.File.ReadAllText("./InventoryItem.Tests/Projections/" + p)
    pm.CreateContinuousAsync("FlatReadModelProjection", file "FlatReadModelProjections.js", UserCredentials("admin", "changeit")) |> ignore
    pm.CreateContinuousAsync("OverviewReadModelProjection", file "OverviewReadModelProjections.js", UserCredentials("admin", "changeit")) |> ignore
    ()



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

[<Test>]
[<Order(5)>]
let cannotRemoveMoreItemsThanIHaveCheckedIn() =
    let version = 4
    InventoryItem.RemoveItems(87)
    |> handleCommand (id,version)
    |> function
    | Choice1Of2 _ -> failwith "Wrong choice!"
    | Choice2Of2 e -> e.[0] |> should equal "Cannot check in a negative item count"

[<Test>]
[<Order(6)>]
let cannotRemoveNegativeItems() =
    let version = 4
    InventoryItem.RemoveItems(-55)
    |> handleCommand (id,version)
    |> function
    | Choice1Of2 _ -> failwith "Wrong choice!"
    | Choice2Of2 e -> e.[0] |> should equal "The item count must be positive"


[<Test>]
[<Order(7)>]
let IcanCheckInOtherItems() =
    let version = 4
    InventoryItem.CheckInItems(100)
    |> handleCommand (id,version)
    |> firstChoice
    |> should equal ()

[<Test>]
[<Order(8)>]
let andRemoveOthers() =
    let version = 5
    InventoryItem.RemoveItems(9)
    |> handleCommand (id,version)
    |> firstChoice
    |> should equal ()


[<Test>]
[<Order(99)>]
let wrongVersion() =
  let version = 99
  let streamId (id: System.Guid) = "InventoryItem-" + id.ToString("N").ToLower()
  let error = sprintf "Error while committing aggregate to EventStore: Append failed due to WrongExpectedVersion. Stream: %s, Expected version: 98, Current version: 5" (streamId id)
  InventoryItem.RemoveItems(37)
  |> handleCommand (id,version)
  |> function
  | Choice1Of2 _ -> failwith "Wrong choice!"
  | Choice2Of2 e -> e.[0] |> should equal error
