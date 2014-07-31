type ICurrencyQuoteProvider =
    abstract getQuote : string -> double

type BloombergCurrencyQuoteProvider(licenceToken:obj) = 
    interface ICurrencyQuoteProvider with 
        member this.getQuote cur = 120.0 //implementation logic 

type TestQuoteProvider() = 
    interface ICurrencyQuoteProvider with 
        member this.getQuote cur = 120.0

//program to **interface**
type Basket(curProv:ICurrencyQuoteProvider) =
    //add to basket etc...
    member this.getTotalJPY () = 0.0  //implementation logic
    member this.getTotalIn cur = this.getTotalJPY () * curProv.getQuote cur

let testBasket = Basket(TestQuoteProvider())
let prodBasket = Basket(TestQuoteProvider())
