
pub type alias PSemantic = List[Char]

pub enum PResult with Eq, ToString{
    case ParseOk(PSemantic),
    case ParseComplete(PSemantic),
    case ParseFail
}

///
/// Effect for the inner workings of a parser combination.
/// These effects specify exactly how the parser behaves
/// while parsing.
///
eff Parsing {

    /// This effect handles the execution of atomic parsers.
    def satisfy(p : List[Char] -> Option[(List[Char], List[Char])]) : PResult
    
    /// This effect allows us to implement binary decision points.
    def flip() : Bool

    def message(str : String) : Unit
}

///
/// Effect for handling the result of the parsing process.
/// 
/// Here we define how to handle failure but also how to 
/// combine the results from individual parser applications.
///
eff PStrategy {

    // This effect is triggered when the parsing process fails.
    def fail() : PResult

    // This effect allows to combine the result of two sequentially applied parsers 
    // to a single result. 
    //
    // At this stage the result will always be a recosntruction
    // of the input list. This could be adjusted to produce any intermediate result 
    // of a syntax-directed translation. This will become easier to work with 
    // once polymorphic effects have been introduced to flix.
    
    def join(lhs : PResult, rhs : PResult) : PResult
}

mod Parser {
    use PResult.ParseComplete
    use PResult.ParseOk
    use PResult.ParseFail

    use Char.isLetter
    use Char.isDigit

    /// 
    /// Builds the behavior of a single character-parse from a predicate.
    ///
    /// Returns a function that separates the head and tail of its input list in a tuple
    /// if the head satisfies the predicate.
    ///
    /// This is a helper function to easily construct parameters for the 
    /// Parsing.satisfy effect from a predicate.
    ///
    pub def sat(predicate : Char -> Bool) : List[Char] -> Option[(List[Char], List[Char])] = 
        input -> match input {
            case Nil => None
            case x::xs => if(predicate(x)) Some((x::Nil,xs)) else None
        }

    ///
    /// Constructs a parser that consumes no symbol and leaves the input unchanged.
    ///
    /// Returns a parser that decides L = {\epsilon}.
    ///
    pub def empty() : PResult \ Parsing = 
        //Parsing.message("applying empty");
        Parsing.satisfy(inp -> Some((Nil, inp)))

    ///
    /// Constructs a parser that parses s as a singular symbol.
    ///
    /// Returns a parser that decides L = {s}.
    ///
    pub def symbol(s : Char) : PResult \ Parsing = 
        //Parsing.message("applying symbol(${s})");
        Parsing.satisfy(sat(c -> c == s))

    ///
    /// Constructs a parser that parses a digit. 
    ///
    /// Returns a parser that that decides L = {0, 1, ..., 9}.
    ///
    pub def digit() : PResult \ Parsing = 
        //Parsing.message("applying digit");
        Parsing.satisfy(sat(isDigit))

    ///
    /// Constructs a parser that parses a letter.
    ///
    /// Returns a parser that that decides L = {A, B, ..., Z, a, b, ..., z}.
    ///
    pub def letter() : PResult \ Parsing = 
        //Parsing.message("applying letter");
        Parsing.satisfy(sat(isLetter))

    /// Constructs a parser that parses an entire string in a singel step.
    ///
    /// Returns a parser that decides L = {str}.
    ///
    pub def token(str : String) : PResult \ Parsing = 
        //Parsing.message("applying token(${str})");
        let token = String.toList(str);
        Parsing.satisfy(
                input -> match (List.isPrefixOf(token, input)) {
                    case false => None
                    case true  => Some((token, List.drop(List.size(token), input)))
            })

    ///
    /// Takes two parser "left" and "right" (both wrapped into lambdas to defer effects). 
    /// Constructs from them a parser that applies left and/or right according to the parsing strategy.
    ///
    /// If left decides L1 and right decides L2 then the resulting Parser decides L1 | L2.
    ///
    pub def choice(left : Unit -> PResult \ Parsing, right : Unit -> PResult \ Parsing) : PResult \ Parsing =
        //Parsing.message("applying choice");
        let decision = Parsing.flip();
        if(decision) left() else right()

    ///
    /// Takes two parsers "p1" and "p2" (wrapped into lambdas to defer effects)
    /// Builds a parser that applies them consecutively. 
    ///
    /// If p1 decides L1 and p2 decides L2 the resulting Parser decides L1L2
    ///
    pub def seq(p1 : Unit -> PResult \ Parsing, p2 : Unit -> PResult \ Parsing) : PResult \ Parsing = 
        //Parsing.message("applying seq");
        {p1(); p2()}

    ///
    /// Takes in a parser "p" (wrapped into a lambda to defer effects).
    /// Builds a parser that repeatedly applies p until it fails.
    ///
    /// If p decides L then the resulting Parser decides L*.
    ///
    /// Note that this parser can never fail. 
    ///
    pub def many(p : Unit -> PResult \ Parsing) : PResult \ Parsing = 
        let _ = Parsing.flip();
        choice(_ -> many1(p), empty)

    ///
    /// Takes in a parser "p" (wrapped into a lambda to defer effects).
    /// Builds a parser that repeatedly applies p until it fails, but at least once. 
    /// 
    /// If p decides L then the resulting parser decides L+.
    ///
    pub def many1(p : Unit -> PResult \ Parsing) : PResult \ Parsing = 
        //Parsing.message("applying many1");
        seq(p, _ -> many(p))

    /// 
    /// This Function invokes a parser on a given input while handling the Parsing effects. The
    /// function itself is effectful in terms of how the parsing result is produced, and will need
    /// to be handled accordingly. 
    ///
    /// Returns ...
    ///
    pub def runParser(input : List[Char], parser : Unit -> PResult \ Parsing) : PResult \ {PStrategy, IO} = 
        region exec {
            let src = Ref.fresh(exec, input);
            let parsed =  run{
                parser()
            }with handler Parsing {
                def satisfy(p, k) = 
                    match p(Ref.get(src)) {
                        case None => {println("fail on parsing ${Ref.get(src)}"); PStrategy.fail()}
                        case Some((v, rest)) => {println("sucessful parse: ${v} - rest: ${rest}"); Ref.put(rest, src); PStrategy.join(ParseOk(v), k(ParseOk(Nil)))} 
                    }
                def flip(k) = 
                    let stash = Ref.get(src);
                    let eager = k(true);
                    match eager {
                        case ParseFail => {println("Backtrack to: ${stash}");Ref.put(stash, src); k(false)}
                        case _         => eager
                    }
                def message(s, k) = {println(s); k()}
            };

            match (parsed, Ref.get(src)) {
                case (ParseOk(s), Nil)  => ParseComplete(s)
                case _                  => parsed 
            }
        }


    // ???
    pub def parse(input : String, parser : Unit -> PResult \ Parsing): PResult \ IO=
        run{
            runParser(String.toList(input), parser)
        }with handler PStrategy {
            def fail(k) = k(ParseFail)
            def join(lhs, rhs, k) = 
                k(
                    match (lhs, rhs) {
                    case (ParseOk(l), ParseOk(r))    => ParseOk(l ::: r)
                    //case (ParseFail, ParseOk(r))     => ParseOk(r) 
                    case _                           => ParseFail
                    }
                )
        }    
}

