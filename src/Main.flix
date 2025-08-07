use Char.isLetter
use FileReadWithResult.isDirectory
use Char.isDigit

type alias PResult = List[Char]

///
/// effect for the inner workings of a combined parser
/// these effects specify exactly how the parser behaves
/// while parsing
///
eff Parsing {

    /// this effect handles the execution of atomic parsers
    def satisfy(p : List[Char] -> Option[(List[Char], List[Char])]) : PResult
    
    /// this effect allows us to implement binary decision points
    def flip() : Bool
}

///
/// effect for handling the result of the parsing process
/// 
/// here we define how to handle failure but also how to 
/// combine the results from individual parsing steps
///
eff PStrategy {

    // triggered when parsing fails
    def fail() : PResult

    // construct a result from two sequential parses
    def join(lhs : PResult, rhs : PResult) : PResult
}

/// 
/// builds the behavior of a single character-parse from a predicate
///
/// this is a helper function to easily construct parameters for the 
/// Parsing.satisfy effect from a predicate, that only parse one symbol
///
/// Returns a function that separates the head and tail of its input list in a tuple
/// if the head satisfies the predicate
///
def sat(predicate : Char -> Bool) : List[Char] -> Option[(List[Char], List[Char])] = 
    input -> match input {
        case Nil => None
        case x::xs => if(predicate(x)) Some((x::Nil,xs)) else None
    }

///
/// constructs a parser that consumes no symbol and leaves the input untouched
///
/// returns a parser that decides L = {epsilon}
def empty() : PResult \ Parsing = 
    Parsing.satisfy(inp -> Some((Nil, inp)))

///
/// constructs a parser that parses s as a singular symbol
///
/// returns a parser that decides L = {s}
///
def symbol(s : Char) : PResult \ Parsing = 
    Parsing.satisfy(sat(c -> c == s))

///
/// constructs a parser that parses a digit 
///
/// returns a parser that that decides L = {0, 1, ..., 9}
///
def digit() : PResult \ Parsing = 
    Parsing.satisfy(sat(isDigit))

///
/// constructs a parser that parses a letter
///
/// returns a parser that that decides L = {A, B, ..., Z, a, b, ..., z}
///
def letter() : PResult \ Parsing = 
    Parsing.satisfy(sat(isLetter))


def token(str : String) : PResult \ Parsing = 
    let token = String.toList(str);
    Parsing.satisfy(
            input -> match (List.isPrefixOf(token, input)) {
                case false => None
                case true  => Some((token, List.drop(List.size(token), input)))
        })

///
/// takes 2 parser left and right (both wrapped into lambdas to defer execution) 
/// and constructs from them a parser that applies left and/or right depending on the parsing strategy
///
/// if left decide L1 and right decides L2 then the resulting Parser decides L1 + L2
///
def choice(left : Unit -> PResult \ Parsing, right : Unit -> PResult \ Parsing) : PResult \ Parsing =
    let decision = Parsing.flip();
    if(decision) left() else right()

///
/// takes in a parser p (wrapped into a lambda to defer execution)
/// then builds a parser that repeatedly applies p until it fails 
///
/// (TODO atm this fails when no symbol is parsed, it should however always succed on epsilon at least)
///
/// if p decides L then the resulting Parser decides L*
///
def many(p : Unit -> PResult \ Parsing) : PResult \ Parsing = choice(_ -> many1(p), empty)

///
/// takes in a parser p (wrapped into a lambda to defer execution)
/// then builds a parser that repeatedly applies p until it fails but at least once 
/// 
/// if p decides L then the resulting parser decides L+
///
def many1(p : Unit -> PResult \ Parsing) : PResult \ Parsing = {p(); many(p)}

    // S -> EXPR OP EXPR 
    // EXPR -> NUM | (S)
    // OP -> + | * 
    // NUM -> DIGIT | DIGIT NUM
    // DIGIT -> 1 | 2 | ... | 9 | 0

    // S -> EXPR OP EXPR -> NUM OP EXPR -> DIGIT OP EXPR -> 1 OP EXPR -> 1 OP 1 -> 1 + 1
    def parseS() : PResult \ Parsing = {parseEXPR(); parseOP(); parseEXPR()}
    def parseEXPR() : PResult \ Parsing = 
        choice(
            parseNUM,
            _ -> {symbol('('); parseS() ;symbol(')')}
        )
    def parseOP() : PResult \ Parsing = choice(_ -> symbol('+'), _ -> symbol('*'))
    def parseNUM() : PResult \ Parsing = many1(digit)




def runParser(input : List[Char]) : PResult \ PStrategy = 
    region exec {
        let src = Ref.fresh(exec, input);
        let parsed = run{
            parseS()
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
    

    // TODO
    // distinguish succesfull parse from parse that failed after reading a prefix
    // put parser in a module
    // build some examples
    // do test cases
