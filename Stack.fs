module files

type Point = { x : float; y: float; z: float; }
let mypoint = { x = 1.0; y = 1.0; z = -1.0; }

type IntListStack = {emptyStack : List<int>;
                     push : int * List<int> -> List<int>;
                     pop : List<int> -> List<int>;
                     top : List<int> -> int }

let intListStack = { emptyStack  = List.empty;
                     push  = List.Cons ;
                     pop  =  List.tail ;
                     top = List.head   }

type GenericListStack<'T> = {emptyStack : List<'T>;
                     push : 'T * List<'T> -> List<'T>;
                     pop : List<'T> -> List<'T>;
                     top : List<'T> -> 'T }

let genericListStack<'T> = {  emptyStack  = List.empty;
                              push  = List.Cons ;
                              pop  =  List.tail ;
                              top = List.head   } : GenericListStack<'T>

type GenericListStack2<'T,'Stack> = {emptyStack : 'Stack;
                                     push : 'T * 'Stack -> 'Stack;
                                     pop : 'Stack -> 'Stack;
                                     top : 'Stack -> 'T }
