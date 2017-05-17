[<RequireQualifiedAccess>]
module InventoryItem

type State = {
    isActive : bool
    quantity: int
    Name: string
}
with static member Zero = { isActive = false; quantity = 0; Name = System.String.Empty }

type Command =
    | Create of string
    | Deactivate
    | Rename of string
    | CheckInItems of int
    | RemoveItems of int

type Event =
    | Created of string
    | Deactivated
    | Renamed of string
    | ItemsCheckedIn of int
    | ItemsRemoved of int

let apply item = function
    | Created n -> { item with State.isActive = true; Name = n }
    | Deactivated _ -> { item with State.isActive = false; }
    | Renamed nn -> {item with Name = nn}
    | ItemsCheckedIn e ->  { item with quantity = item.quantity + e }
    | ItemsRemoved e -> { item with quantity = item.quantity - e}

open Validator

module private Assert =
    let validName name = notNull ["The name must not be null."] name <* notEmptyString ["The name must not be empty"] name
    let validCount count = validator (fun c -> c > 0) ["The item count must be positive"] count
    let inactive state = validator (fun i -> not i.isActive) ["The item is already deactivated."] state

module private DomainAssert =
      let removable stateAndCount = validator (fun (state,count) ->  (state.quantity - count) > 0) ["Cannot check in a negative item count"] stateAndCount

let exec state =
    function
    | Create name        -> Assert.validName name   <?> Created(name)
    | Deactivate         -> Assert.inactive state   <?> Deactivated
    | Rename name        -> Assert.validName name   <?> Renamed(name)
    | CheckInItems count -> Assert.validCount count <?> ItemsCheckedIn(count)
    | RemoveItems count  -> Assert.validCount count <* DomainAssert.removable (state, count) <?> ItemsRemoved(count)
