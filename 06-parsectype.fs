type ParseReturn<'ret,'token> = 
  | Success of 'ret
  | Failure of string

type Parser<'ret,'token> =
  'token list -> ParseReturn<'ret>