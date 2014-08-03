namespace existentials 
module converge =
    type StackOps<'Rep> = {                //forall t . Stack(t)
           empty   :'Rep
           isEmpty :'Rep ->  bool
           push    : (int *'Rep) -> 'Rep
           pop     :'Rep -> 'Rep
           top     :'Rep ->  int
           merge  : 'Rep -> 'Rep -> 'Rep
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

    //LIST based implementation
    let listOps =  
        { empty = List.empty
          isEmpty = List.isEmpty
          push = List.Cons
          pop = List.tail
          top = List.head  } 

    let listStack  = new Stack<List<int>>(List.empty,listOps)  :> ExistentialStack

    let listStack1  = ExStack.push listStack  1  
    let listStack2  = ExStack.push listStack1 2  
    let el2         = ExStack.top listStack2
    let el1         = ExStack.top listStack1
    let listStack3  = ExStack.pop listStack2  
    let el3         = ExStack.top listStack3  

    //ARRAY based implementation
    let arOps =  { empty = (Array.init 100 id,-1) ;
                   isEmpty =  fun (ar,i) -> i = -1 ;
                   push = fun (e,(ar,i)) -> ar.[i+1] <- e
                                            (ar,i+1);
                   pop = fun ((ar,i)) -> (ar,i-1);
                   top = fun ((ar,i)) -> ar.[i]  
                   merge  } 
    let arStack  = new Stack<_>(arOps.empty,arOps) :> ExistentialStack
       

    let arStack1    = ExStack.push arStack  1  
    let arStack2    = ExStack.push arStack1 2  
    let ea2         = ExStack.top arStack2
    let ea1         = ExStack.top arStack1
    let arStack3    = ExStack.pop arStack2  
    let el3         = ExStack.top arStack3  

                  
    