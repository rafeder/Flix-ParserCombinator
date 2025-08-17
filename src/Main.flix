use Char.isLetter
use FileReadWithResult.isDirectory
use Char.isDigit

type alias PResult = List[Char]

///
/// Fffect for the inner workings of a parser combination.
/// These effects specify exactly how the parser behaves
/// while parsing.
///
eff Parsing {

    /// This effect handles the execution of atomic parsers.
    def satisfy(p : List[Char] -> Option[(List[Char], List[Char])]) : PResult
    
    /// This effect allows us to implement binary decision points.
    def flip() : Bool
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

    // This effect allows to combine the result of two sequentially applied parser
    // to a single result.
    def join(lhs : PResult, rhs : PResult) : PResult
}

/// 
/// Builds the behavior of a single character-parse from a predicate.
///
/// Returns a function that separates the head and tail of its input list in a tuple
/// if the head satisfies the predicate.
///
/// This is a helper function to easily construct parameters for the 
/// Parsing.satisfy effect from a predicate.
///
def sat(predicate : Char -> Bool) : List[Char] -> Option[(List[Char], List[Char])] = 
    input -> match input {
        case Nil => None
        case x::xs => if(predicate(x)) Some((x::Nil,xs)) else None
    }

///
/// Constructs a parser that consumes no symbol and leaves the input unchanged.
///
/// Returns a parser that decides L = {\epsilon}.
///
def empty() : PResult \ Parsing = 
    Parsing.satisfy(inp -> Some((Nil, inp)))

///
/// Constructs a parser that parses s as a singular symbol.
///
/// Returns a parser that decides L = {s}.
///
def symbol(s : Char) : PResult \ Parsing = 
    Parsing.satisfy(sat(c -> c == s))

///
/// Constructs a parser that parses a digit. 
///
/// Returns a parser that that decides L = {0, 1, ..., 9}.
///
def digit() : PResult \ Parsing = 
    Parsing.satisfy(sat(isDigit))

///
/// Constructs a parser that parses a letter.
///
/// Returns a parser that that decides L = {A, B, ..., Z, a, b, ..., z}.
///
def letter() : PResult \ Parsing = 
    Parsing.satisfy(sat(isLetter))

/// Constructs a parser that parses an entire string in a singel step.
///
/// Returns a parser that decides L = {str}.
///
def token(str : String) : PResult \ Parsing = 
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
def choice(left : Unit -> PResult \ Parsing, right : Unit -> PResult \ Parsing) : PResult \ Parsing =
    let decision = Parsing.flip();
    if(decision) left() else right()

///
/// Takes in a parser "p" (wrapped into a lambda to defer effects).
/// Builds a parser that repeatedly applies p until it fails.
///
/// If p decides L then the resulting Parser decides L*.
///
/// Note that this parser can never fail. 
///
def many(p : Unit -> PResult \ Parsing) : PResult \ Parsing = choice(_ -> many1(p), empty)

///
/// Takes in a parser "p" (wrapped into a lambda to defer effects).
/// Builds a parser that repeatedly applies p until it fails, but at least once. 
/// 
/// If p decides L then the resulting parser decides L+.
///
def many1(p : Unit -> PResult \ Parsing) : PResult \ Parsing = {p(); many(p)}

def runParser(input : List[Char]) : PResult \ PStrategy = 
    region exec {
        let src = Ref.fresh(exec, input);
        let parsed = run{
            many1(digit)
        }with handler Parsing {
            def satisfy(p, k) = 
                match p(Ref.get(src)) {
                    case None => PStrategy.fail() 
                    case Some((v, rest)) => {Ref.put(rest, src); PStrategy.join(v, k(Nil))} 
                }
            def flip(k) = 
                let stash = Ref.get(src);
                let eager = k(true);
                if (eager != Nil) eager else {Ref.put(stash, src); k(false)} // muss hier != Nil oder kann man direkt an fail() knüpfen?
        };
        
        if(Ref.get(src) == Nil) parsed else Nil
    }


// The main entry point.
def main(): PResult \ IO =
    let str = "(1+2)*(5+2)";
    let src = String.toList(str);
    run{
        runParser(src)
    }with handler PStrategy {
        def fail(k) = k(Nil)
        def join(lhs, rhs, k) = k(lhs ::: rhs)
    }

