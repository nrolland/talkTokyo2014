let empty () = System.Collections.Generic.List()
let stringList = empty()
do stringList.Add("hello")
let intList = empty()
do intList.Add(1)

type List<'T> = | Empty | Cons of ('T * List<'T>)
let stringList = Empty
let stringList = Cons ("hello", stringList)
let intList = Empty
let intList = Cons (1, intList)