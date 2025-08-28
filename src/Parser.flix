
//eff Amb {
//    def flip() : Bool
//}

eff Parse {
    def sat(p : (Char -> Bool)) : Bool
    def empty() : Bool
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
    pub def empty() : Bool \ Parse = Parse.empty()

    ///
    /// Constructs a parser that parses s as a singular symbol.
    ///
    /// Returns a parser that decides L = {s}.
    ///
    pub def symbol(x :  Char) : Bool \ Parse = Parse.sat(c -> x == c)

    ///
    /// Constructs a parser that parses a letter.
    ///
    /// Returns a parser that that decides L = {A, B, ..., Z, a, b, ..., z}.
    ///
    pub def letter() : Bool \ Parse = Parse.sat(isLetter)

    ///
    /// Constructs a parser that parses a digit. 
    ///
    /// Returns a parser that that decides L = {0, 1, ..., 9}.
    ///
    pub def digit() : Bool \ Parse = Parse.sat(isDigit)

    /// Constructs a parser that parses an entire string in a singel step.
    ///
    /// Returns a parser that decides L = {str}.
    ///
    pub def token(str : String) : Bool \ Parse = 
        let f = x -> x 
                |> List.map(c -> _ -> symbol(c))
                |> List.reduceLeft((a, b) -> (_ -> seq(a, b)));
        match f(String.toList(str)) {
            case Some(p) => p()
            case None    => false
        }

    ///
    /// Takes two parser "left" and "right" (both wrapped into lambdas to defer effects). 
    /// Constructs from them a parser that applies left and/or right according to the parsing strategy.
    ///
    /// If left decides L1 and right decides L2 then the resulting Parser decides L1 | L2.
    ///
    pub def choice(l : Unit -> Bool \ Parse , r : Unit -> Bool \ Parse) : Bool \ Parse = 
        let heads = Parse.flip(); 
        if(heads) l() else r()

    ///
    /// Takes two parsers "p1" and "p2" (wrapped into lambdas to defer effects)
    /// Builds a parser that applies them consecutively. 
    ///
    /// If p1 decides L1 and p2 decides L2 the resulting Parser decides L1L2
    ///
    pub def seq(a : Unit -> Bool \ Parse, b : Unit -> Bool \ Parse) : Bool \ Parse = 
        Parse.log("applying seq");
        {a(); b()}

    ///
    /// Takes in a parser "p" (wrapped into a lambda to defer effects).
    /// Builds a parser that repeatedly applies p until it fails.
    ///
    /// If p decides L then the resulting Parser decides L*.
    ///
    /// Note that this parser can never fail. 
    ///
    pub def many(a : Unit -> Bool \ Parse) : Bool \ Parse = 
        Parse.log("applying many");
        choice(_ -> many1(a), _ -> empty())

    ///
    /// Takes in a parser "p" (wrapped into a lambda to defer effects).
    /// Builds a parser that repeatedly applies p until it fails, but at least once. 
    /// 
    /// If p decides L then the resulting parser decides L+.
    ///
    pub def many1(a : Unit -> Bool \ Parse) : Bool \ Parse = 
            Parse.log("applying many1");
            seq(a, _ -> many(a))

    // The main parsing dunction
    pub def invokeParser(input : String, parser : Unit -> Bool \ Parse): Bool =
        region exec {
            let src = Ref.fresh(exec, String.toList(input));
                    run{
                        parser()
                    }with handler Parse {
                        def sat(p, k) = 
                            match Ref.get(src) {
                                case Nil     => false
                                case x::xs   => if(p(x)) {Ref.put(xs, src); k(xs == Nil)} else false
                            }

                        def empty(k) = k(true)

                        def flip(k) = 
                            let stash = Ref.get(src);
                            let left = k(true);
                            if(left) left else {Ref.put(stash, src); k(false)}

                        def log(_msg, k) = k()//{println(msg); k()}
                    }
        }
}
