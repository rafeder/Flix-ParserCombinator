use Char.isLetter
use FileReadWithResult.isDirectory
use Char.isDigit

type alias PResult = List[Char]

eff Parsing {
    def satisfy(p : List[Char] -> Option[(Char, List[Char])]) : PResult
    def flip() : Bool
}

eff PStrategy {
    def fail() : PResult
    def join(lhs : PResult, rhs : PResult) : PResult
}

/// 
/// builds the behavior of a single character-parse from a predicate
///
/// this is a helper function to easily construct parameters for the 
/// Parsing.satisfy effect from a predicate
///
/// Returns a function that separates the head and tail of its input list in a tuple
/// if the head satisfies the predicate
///
def sat(predicate : Char -> Bool) : List[Char] -> Option[(Char, List[Char])] = 
    input -> match input {
        case Nil => None
        case x::xs => if(predicate(x)) Some((x,xs)) else None
    }

def symbol(s : Char) : PResult \ Parsing = 
    Parsing.satisfy(sat(c -> c == s))

def digit() : PResult \ Parsing = 
    Parsing.satisfy(sat(isDigit))

def letter() : PResult \ Parsing = 
    Parsing.satisfy(sat(isLetter))

///always fails
def pfail() : PResult \ Parsing =
    Parsing.satisfy(sat(_ -> false))

def choice(left : Unit -> PResult \ Parsing, right : Unit -> PResult \ Parsing) : PResult \ Parsing =
    let decision = Parsing.flip();
    if(decision) left() else right()

def many(p : Unit -> PResult \ Parsing) : PResult \ Parsing = choice(_ -> many1(p), _ -> pfail())

def many1(p : Unit -> PResult \ Parsing) : PResult \ Parsing = {p(); many(p)}

def runParser(input : List[Char]) : PResult \ PStrategy = 
    region exec {
        let src = Ref.fresh(exec, input);
        run{
            many(letter)
        }with handler Parsing {
            def satisfy(p, k) = 
                match p(Ref.get(src)) {
                    case None => PStrategy.fail() 
                    case Some((v, rest)) => {Ref.put(rest, src); PStrategy.join(v::Nil, k(Nil))} 
                }
            def flip(k) = 
                let stash = Ref.get(src);
                let eager = k(true);
                if (eager != Nil) eager else {Ref.put(stash, src); k(false)} // muss hier != Nil oder kann man direkt an fail() knüpfen?
        }
    }


// The main entry point.
def main(): PResult \ IO =
    let src = '1'::'b'::'a'::'a'::'a'::Nil;
    run{
        runParser(src)
    }with handler PStrategy {
        def fail(k) = k(Nil)
        def join(lhs, rhs, k) = k(lhs ::: rhs)
    }
    