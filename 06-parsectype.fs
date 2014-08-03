type ParseReturn<'ret>   =  | Success of 'ret  | Failure of string

type Parser<'ret,'token> =  
    List<'token> -> ParseReturn<'ret> * List<'token>