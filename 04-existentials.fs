namespace existentials 

module SO = 
    type Packed = int
    type 't NumberOps = {  //forall t . f(t)
        opPack : 't -> Packed
        opUnpack : Packed -> 't
        opAdd : 't -> 't -> 't 
    }

    type ApplyNumberOps<'x> = 
        abstract Apply :  't NumberOps -> 'x  //forall t . f(t) -> x  //(x is bound)

    // ∃ 't. 't NumberOps
    type ExNumberOps =
        abstract Apply : ApplyNumberOps<'x> -> 'x   //forall x. (forall t . f(t) -> x) <=> \exists t. f(t)

    // take any 't NumberOps to an ExNumberOps
    // in some sense this is the only "proper" way to create an instance of ExNumberOps
    let wrap n = { new ExNumberOps with member __.Apply(f) = f.Apply(n) }

    let undefined<'a> : 'a = failwith "undefined"

    let unpackInt  : int -> int = undefined
    let unpackReal : int -> float = undefined
    let packInt  : int -> int = undefined
    let packReal : float -> int = undefined


    type PackedType = | PackedInt | PackedReal

    let getNumberOps (t : PackedType) =
        match t with
        | PackedInt  -> wrap { opPack = packInt ; opUnpack = unpackInt ; opAdd = (+) }
        | PackedReal -> wrap { opPack = packReal; opUnpack = unpackReal; opAdd = (+) }

    let addPacked (t : PackedType) (a : Packed) (b : Packed) =
        (getNumberOps t).Apply 
            { new ApplyNumberOps<_> with 
                member __.Apply({ opPack = pack; opUnpack = unpack; opAdd = add } as d) = 
                    d.opAdd (d.opUnpack a) (d.opUnpack b) |> d.opPack }
                
module converge =
    type StackOps<'Rep> = {                //forall t . Stack(t)
           empty   :'Rep
           isEmpty :'Rep ->  bool
           push    : (int *'Rep) -> 'Rep
           pop     :'Rep -> 'Rep
           top     :'Rep ->  int
        }

    // ∃ 't. Stack<'t>
    [<AbstractClass>]
    type ExistentialStack()=
        abstract Apply : StackClient<'x> -> 'x // forall x. (forall t . Stack(t) -> x) -> x = ∃ t. Stack(t)
    and Stack<'Rep>(e:'Rep,ops:StackOps<'Rep>)=
        inherit ExistentialStack()
        member this.witness = e
        member this.getOps  = ops
        override this.Apply(mc) = mc.Apply<'Rep>(this)
    and StackClient<'R> = interface
        abstract member Apply<'U> :  Stack<'U> -> 'R // forall U. Stack(U) -> R
    end

    module ExStack = 
        let empty (s:ExistentialStack)=
            s.Apply{ new StackClient<ExistentialStack> 
                        with member __.Apply<'Rep>(d:Stack<'Rep>) = 
                               new Stack<'Rep>(d.getOps.empty, d.getOps):> ExistentialStack  } 
        let isEmpty (s:ExistentialStack) = s.Apply{ new StackClient<bool> 
                        with member __.Apply<'Rep>(d:Stack<'Rep>) = 
                               d.getOps.isEmpty(d.witness)  } 
        let push (s:ExistentialStack)(e:int) =
            s.Apply{ new StackClient<ExistentialStack> 
                        with member __.Apply<'Rep>(d:Stack<'Rep>) = 
                               new Stack<'Rep>(d.getOps.push(e, d.witness), d.getOps):> ExistentialStack  } 
        let pop (s:ExistentialStack) =
            s.Apply{ new StackClient<ExistentialStack> 
                        with member __.Apply<'Rep>(d:Stack<'Rep>) = 
                               new Stack<'Rep>(d.getOps.pop(d.witness), d.getOps):> ExistentialStack  } 
        let top (s:ExistentialStack) =
            s.Apply{ new StackClient<int> 
                        with member __.Apply<'Rep>(d:Stack<'Rep>) = 
                               d.getOps.top(d.witness)  } 

    let listOps =  { empty = List.empty;isEmpty = List.isEmpty;push = List.Cons;pop = List.tail;top = List.head  } 
    let listStack  = new Stack<List<int>>(List.empty,listOps)  :> ExistentialStack

    let arOps =  { empty = (Array.init 100 id,-1) ;
                   isEmpty =  fun (ar,i) -> i = -1 ;
                   push = fun (e,(ar,i)) -> ar.[i+1] <- e
                                            (ar,i+1);
                   pop = fun ((ar,i)) -> (ar,i-1);
                   top = fun ((ar,i)) -> ar.[i]  } 
    let arStack  = new Stack<_>(arOps.empty,arOps) :> ExistentialStack
       
    let listStack1  = ExStack.push listStack  1  
    let listStack2  = ExStack.push listStack1 2  
    let el2         = ExStack.top listStack2
    let el1         = ExStack.top listStack1
    let listStack3  = ExStack.pop listStack2  
    let el3         = ExStack.top listStack3  

    let arStack1    = ExStack.push arStack  1  
    let arStack2    = ExStack.push arStack1 2  
    let ea2         = ExStack.top arStack2
    let ea1         = ExStack.top arStack1
    let arStack3    = ExStack.pop arStack2  
    let el3         = ExStack.top arStack3  

                  
    

    [<AbstractClass>]
    type Existential() = 
        abstract member Unpack : ExistentialClient<'R>  -> 'R  //forall R (forall U. f(U) -> R) -> R
    and Existential<'U> () =
        inherit Existential()
        override this.Unpack(mc) = mc.Apply<'U>(this)  //'U is the packed representation
    and ExistentialClient<'R> = interface
        abstract member Apply<'U> :  Existential<'U> -> 'R //forall U. f(U) -> R
    end

    let undefined<'a> : 'a = failwith "undefined"

    let unpackInt  : int -> int = undefined
    let unpackReal : int -> float = undefined
    let packInt  : int -> int = undefined
    let packReal : float -> int = undefined


    type PackedType = | PackedInt | PackedReal

    let getNumberOps (t : PackedType) =
        match t with
        | PackedInt  -> wrap { opPack = packInt ; opUnpack = unpackInt ; opAdd = (+) }
        | PackedReal -> wrap { opPack = packReal; opUnpack = unpackReal; opAdd = (+) }

    let addPacked (t : PackedType) (a : Packed) (b : Packed) =
        (getNumberOps t).Apply 
            { new ApplyNumberOps<_,_> with 
                member __.Apply({ opPack = pack; opUnpack = unpack; opAdd = add } as d) = 
                    d.opAdd (d.opUnpack a) (d.opUnpack b) |> d.opPack }
                

[<AbstractClass>]
type Existential() = 
    abstract member Unpack : ExistentialClient<'T,'R>  -> 'R  //forall R (forall U. f(U) -> R) -> R
and Existential<'U> () =
    inherit Existential()
    override this.Unpack(mc) = mc.Apply<'U>(this)
and ExistentialClient<'T,'R> = interface
    abstract member Apply<'U> :  Existential<'U> -> 'R //forall U. f(U) -> R
end

[<AbstractClass>]
type Term<'T>() = 
    //abstract member unsafeProj<'U> : unit -> E<'T->'U>
    //abstract member range:unit -> int
    abstract member Unpack: ITermClient<'T,'R> -> 'R  //forall R (forall U. f(U) -> R) -> R
and Term<'T,'U when 'U:equality>(term: E<'T->'U>)= 
    inherit Term<'T>()
//    override this.unsafeProj<'U_>() = 
//         assert(typeof<'U_> = typeof<'U>)
//         term :> obj :?> E<'T->'U_>
    member this.term() = 
         term 
    override this.Unpack(mc) = mc.body<'U>(this)
and ITermClient<'T,'R> = interface
   abstract member body<'U when 'U:equality> : //forall U. f(U) -> R
            Term<'T,'U> -> 'R
end

let v = new Term<(('SX*('SY*'SZ))*('TX*'TY*'TZ)),int>(
            <@ fun _ -> (%%Microsoft.FSharp.Quotations.Expr.Value<int>(5)) @>
           ) :> Term<(('SX*('SY*'SZ))*('TX*'TY*'TZ))>


  member this.trExp(links:Map<ColumnName,TableName>)
                       (env:Map<ColumnName,FieldDesc<('TX * 'TY * 'TZ)>>)
                       (exp) : Term<('SX*('SY*'SZ))*('TX*'TY*'TZ)> =
        match exp with 
        | Const n ->
           
        | If (b,e1,e2) ->
          let tb = this.trExp links env b
          let te1 = this.trExp links env e1
          let te2 = this.trExp links env e2
          match tb with 
            :? Term<('SX*('SY*'SZ))*('TX*'TY*'TZ),bool> as tb ->
            te1.Unpack 
             {
               new ITermClient<('SX*('SY*'SZ))*('TX*'TY*'TZ),Term<('SX*('SY*'SZ))*('TX*'TY*'TZ)>> with
                override this.body<'U1 when 'U1:equality>(te1:Term<('SX*('SY*'SZ))*('TX*'TY*'TZ),'U1>) =
                         new Term<(('SX*('SY*'SZ))*('TX*'TY*'TZ)),'U1>(
                            <@fun  x'xyz -> if (%tb.term()) x'xyz then (%te1.term()) x'xyz 
                                            else  (%te2.unsafeProj<'U1>()) x'xyz  @> 
                         ) :> Term<(('SX*('SY*'SZ))*('TX*'TY*'TZ))>
             }

