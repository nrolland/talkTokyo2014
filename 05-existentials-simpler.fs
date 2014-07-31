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
                
