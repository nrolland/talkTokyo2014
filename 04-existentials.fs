// $ \forall t . Stack(t) $
type StackOps<'Rep> = {                
       empty   :'Rep
       isEmpty :'Rep ->  bool
       push    : (int *'Rep) -> 'Rep
       pop     :'Rep -> 'Rep
       top     :'Rep ->  int
    }

// $ \exists t. Stack(t) $ 
[<AbstractClass>]
type ExistentialStack()=
    // $ \forall x. (\forall t . Stack(t) -> x) -> x \cong \exists t. Stack(t) $
    abstract Apply : StackClient<'x> -> 'x 
and Stack<'Rep>(e:'Rep,ops:StackOps<'Rep>)=
    inherit ExistentialStack()
    member this.witness = e
    member this.getOps  = ops
    override this.Apply(mc) = mc.Apply<'Rep>(this)
and StackClient<'y> = interface
    // $\forall t. Stack(t) -> y$
    abstract member Apply<'t> :  Stack<'t> -> 'y
end

