type ParseReturn<'ret,'token> = 
  | Success of 'ret *('token list) 
  | Failure of string

type Parser<'ret,'token> =
  'token list -> ParseReturn<'ret,'token>