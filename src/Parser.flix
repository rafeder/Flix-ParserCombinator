
//eff Amb {
//    def flip() : Bool
//}

type alias Sem = List[Char]
type alias PResult = Result[Unit, Sem]

eff Parse {
    def sat(p : (Char -> Bool)) : PResult
    def empty() : PResult
    def flip() : Bool
    def log(msg : String) : Unit
}

mod Parser{
    use Char.isDigit
    use Char.isLetter

    ///
    /// Constructs a parser that consumes no symbol and leaves the input unchanged.
    ///
    /// Returns a parser that decides L = {\epsilon}.
    ///
    pub def empty() : PResult \ Parse = Parse.empty()

    ///
    /// Constructs a parser that parses s as a singular symbol.
    ///
    /// Returns a parser that decides L = {s}.
    ///
    pub def symbol(x :  Char) : PResult \ Parse = Parse.sat(c -> x == c)

    ///
    /// Constructs a parser that parses a letter.
    ///
    /// Returns a parser that that decides L = {A, B, ..., Z, a, b, ..., z}.
    ///
    pub def letter() : PResult \ Parse = Parse.sat(isLetter)

    ///
    /// Constructs a parser that parses a digit. 
    ///
    /// Returns a parser that that decides L = {0, 1, ..., 9}.
    ///
    pub def digit() : PResult \ Parse = Parse.sat(isDigit)

    /// Constructs a parser that parses an entire string in a singel step.
    ///
    /// Returns a parser that decides L = {str}.
    ///
    pub def token(str : String) : PResult \ Parse = 
        let f = x -> x 
                |> List.map(c -> _ -> symbol(c))
                |> List.reduceLeft((a, b) -> (_ -> seq(a, b)));
        match f(String.toList(str)) {
            case Some(p) => p()
            case None    => Err()
        }

    ///
    /// Takes two parser "left" and "right" (both wrapped into lambdas to defer effects). 
    /// Constructs from them a parser that applies left and/or right according to the parsing strategy.
    ///
    /// If left decides L1 and right decides L2 then the resulting Parser decides L1 | L2.
    ///
    pub def choice(l : Unit -> PResult \ Parse , r : Unit -> PResult \ Parse) : PResult \ Parse = 
        let heads = Parse.flip(); 
        if(heads) l() else r()

    ///
    /// Takes two parsers "p1" and "p2" (wrapped into lambdas to defer effects)
    /// Builds a parser that applies them consecutively. 
    ///
    /// If p1 decides L1 and p2 decides L2 the resulting Parser decides L1L2
    ///
    pub def seq(a : Unit -> PResult \ Parse, b : Unit -> PResult \ Parse) : PResult \ Parse = 
        Parse.log("applying seq");
        {discard(a()); b()}

    ///
    /// Takes in a parser "p" (wrapped into a lambda to defer effects).
    /// Builds a parser that repeatedly applies p until it fails.
    ///
    /// If p decides L then the resulting Parser decides L*.
    ///
    /// Note that this parser can never fail. 
    ///
    pub def many(a : Unit -> PResult \ Parse) : PResult \ Parse = 
        Parse.log("applying many");
        choice(_ -> many1(a), _ -> empty())

    ///
    /// Takes in a parser "p" (wrapped into a lambda to defer effects).
    /// Builds a parser that repeatedly applies p until it fails, but at least once. 
    /// 
    /// If p decides L then the resulting parser decides L+.
    ///
    pub def many1(a : Unit -> PResult \ Parse) : PResult \ Parse = 
            Parse.log("applying many1");
            seq(a, _ -> many(a))



    def join(a : PResult, b : PResult) : PResult = 
        match (a, b) {
            case (Ok(l), Ok(r)) => Ok(l ::: r)
            case _              => Err()
        }
    
    def base(rest : List[Char]) : PResult = if(rest == Nil) Ok(Nil) else Err()

    // The main parsing dunction
    pub def invokeParser(input : String, parser : Unit -> PResult \ Parse): PResult \ IO =       
        region exec {
            let src = Ref.fresh(exec, String.toList(input));
            run{
                    parser()
                }with handler Parse {
                    def sat(p, k) = 
                        match Ref.get(src) {
                            case Nil     => Err()
                            case x::xs   => if(p(x)) {Ref.put(xs, src); join(Ok(x::Nil), k(base(Ref.get(src))))} else Err()
                        }
                    def empty(k) = join(Ok(Nil), k(base(Ref.get(src))))
                    def flip(k) = 
                        let stash = Ref.get(src);
                        let left = k(true);
                        if(left != Err()) left else {Ref.put(stash, src); k(false)}
                    def log(_msg, k) = k()//{println(msg); k()}
                }
        }
}
