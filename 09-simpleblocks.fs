type Val = | List of Val list 
           | NumberI  of int
           | NumberF of double
           | String of string
           | Bool of bool

type ParseReturn<'ret,'token> = 
  | Success of 'ret *('token list) 
  | Failure of string

type Parser<'ret,'token> =
  'token list -> ParseReturn<'ret,'token>

// OR combinator : first matching parser 
let (|-|) ph1 ph2 toks  = 
   match ph1 toks with  
   | Success(_) as ret ->  ret 
   | Failure(_) -> ph2 toks;

//AND combinator : sequential application
let (.--.) ph1 ph2 toks = 
   match ph1 toks with 
   | Success (x,toks2) -> 
        match ph2 toks2 with 
        | Success (y,toks3) -> Success((x,y), toks3) //we give back (x,y)
        | Failure (m) -> Failure (m) 
   | Failure (m) -> Failure (m)

//AND combinator, but ignoring the second result
let (.--) ph1 ph2 toks  = 
   match ph1 toks with 
   | Success (x,toks2) -> 
        match ph2 toks2 with 
        | Success (y,toks3) -> Success(x, toks3) //we give back (x)
        | Failure (m) -> Failure (m)
   | Failure (m) -> Failure (m)

//AND combinator, ignoring the first result
let (--.) ph1 ph2 toks  = 
   match ph1 toks with 
   | Success (_,toks2) -> 
        match ph2 toks2 with 
        | Success (y,toks3) -> Success(y, toks3) //we give back (y)
        | Failure (m) -> Failure (m) 
   | Failure (m) -> Failure (m)

//apply f to successful result
let (>>) ph f toks = 
   match ph toks with 
   | (Success(x,toks2)) -> Success(f x, toks2) 
   | Failure (m) -> Failure (m)

let empty toks = Success([],toks);
let one  ph  toks = 
    match ph toks with 
    | Success (x,toks2) -> Success([x],toks2) 
    | Failure (m) -> Failure (m);

//we have either empty, which gives back an Success(empty list of 'a), OR
//  we apply once the parser  ph, giving a result 'a, 
//          AND once repeat0plus ph, giving back a list of 'a
//          for a combined result of Success('a, List<'a>) 
//  to which we apply List.Cons to get a Success(List<'a>)
let rec repeat0plus ph toks = ((ph .--.  repeat0plus ph >> List.Cons) |-| empty  ) toks;
let rec repeat1plus ph toks = ((ph .--.  repeat0plus ph >> List.Cons) |-| one ph ) toks;

let a = "[a number is like 1234 but can also be 9.12 ]"
 
let letter (tokens:char list) =    
  match tokens with
   | (c::rest) when System.Char.IsLetter(c) ->
          Success(c, rest)
   | _ -> Failure "digit expected" 
          
let digit (tokens:char list) =    
  match tokens with
   | (c::rest) when System.Char.IsDigit(c) ->
          Success((int c - int '0'), rest)
   | _ -> Failure "digit expected"

let isChar cMatch (tokens:char list) =
  match tokens with
   | (c::rest) when c = cMatch ->
          Success(c, rest)
   | _ -> Failure (sprintf "%A expected" cMatch)

let toString (e:char list) = System.String(e |> List.toArray)

let string toks = (repeat1plus letter >> toString >> Val.String) toks 

let toInt (intList:int list) = List.fold (fun s e -> s*10+e) 0 (intList |> List.rev)
let integer toks = (repeat1plus digit >> toInt >> Val.NumberI )toks 

let toFloat (leftList, rightList) = 
    let numDigits x = floor (log10 x + 1.0)
    let integralPart = float(toInt leftList)
    let decimalPart  = float(toInt rightList)
    integralPart +  decimalPart / 10.**(numDigits decimalPart)

let float toks = 
    let aDot = isChar '.'
    (repeat1plus digit .-- aDot  .--. repeat1plus digit  >> toFloat >> Val.NumberF )toks 


