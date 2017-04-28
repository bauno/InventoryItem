open System

let puree = Choice1Of2

/// Given a function in a choice and a choice value x, applies the function to the value if available,
/// otherwise propagates the second choice. (Applicative functor)
let apply f x =
    match f,x with
    | Choice1Of2 f, Choice1Of2 x   -> Choice1Of2 (f x)
    | Choice2Of2 e, Choice1Of2 x   -> Choice2Of2 e
    | Choice1Of2 f, Choice2Of2 e   -> Choice2Of2 e
    | Choice2Of2 e1, Choice2Of2 e2 -> Choice2Of2 (e1 @ e2)

let (<*>) = apply

let bind f =
  fun twoTrack ->
    match twoTrack with
    | Choice1Of2 x -> f x
    | Choice2Of2 x -> Choice2Of2 x

let inline (>>=) result f = bind f result

/// Applies the function to the choice 1 value and returns the result as a choice 1, if matched,
/// otherwise returns the original choice 2 value. (Functor)
let map f o =
    match o with
    | Choice1Of2 x -> f x |> puree
    | Choice2Of2 x -> Choice2Of2 x

let inline (<!>) f x = map f x

let inline lift2 f a b = f <!> a <*> b

/// Composes two choice types, passing the case-1 type of the right value.
let inline ( *>) a b = lift2 (fun _  z -> z) a b

/// Composes two choice types, passing the case-2 type of the left value.
let inline ( <*) a b = lift2 (fun z _ -> z) a b

/// Composes a choice type with a non choice type.
let inline (<?>) a b = lift2 (fun _ z -> z) a (Choice1Of2 b)

/// Composes a non-choice type with a choice type.
let inline (|?>) a b = lift2 (fun z _ -> z) (Choice1Of2 a) b

let pippo = (fun z _ -> z)

let pluto = (fun _ z -> z)

pippo 1 2
pluto 1 2

let validator predicate error x =
    if predicate x then Choice1Of2 x
    else Choice2Of2 error

let notNull (s: string) =
  //printfn "NotNull"
  not (isNull s)

let notEmpty (s:string) =
  //printfn "NotEmpty"
  if not (String.IsNullOrEmpty(s))
    then s.Length > 0
  else false

let NotNull = validator notNull ["String must not be null"]
let NotEmpty = validator notEmpty ["String must not be empty"]


let res = NotNull null
let res1 = NotEmpty ""

//let validName name = NotNull name <* NotEmpty name
//let validName name = lift2 (fun _ z -> z) (NotNull name) (NotEmpty name)
//let validName name = (NotNull name) <* (NotEmpty name)

//lift2 f a b = (map f a ) apply b

let mapf name = map (fun z _ -> z) (NotNull name)

let validName name = apply (map (fun z _ -> z) (NotNull name)) (NotEmpty name)

printfn "%A" (validName null)
printfn "%A" (validName "")
printfn "%A" (validName "pippo")

let validEmail (email: string) =
  let regex = System.Text.RegularExpressions.Regex(@"^\w+\@\w+\.\w{3}$")
  if String.IsNullOrEmpty(email)
    then false
  else regex.IsMatch(email)

let ValidMail =
  validator validEmail ["Invalid email"]

let validMail mail = NotNull mail <* NotEmpty mail <* ValidMail mail

let sendMail mail =
  printfn "Mail to %s sent!" mail
  mail |> puree

printfn "%A" (validMail null)
printfn "%A" (validMail "")
printfn "%A" (validMail "Pippo")
printfn "%A" (validMail "Pippo@pippo.com")

let mail = "pippo@pippo.com"
//validMail mail <?> sendMail mail
 let pipeline mail = (validMail mail) >>= (sendMail mail)
