use Char.isLetter
use FileReadWithResult.isDirectory
use Char.isDigit
use Parse.satisfy

type alias Parser = List[Char] -> Option[(Char, List[Char])]
type alias PResult = List[Char]

eff Parse {
    def satisfy(p : Parser) : PResult
}

eff ParseControl {
    def fail() : PResult
    def flip() : Bool
    def join(lhs : PResult, rhs : PResult) : PResult
}

def sat(predicate : Char -> Bool) : Parser = 
    input -> match input {
        case Nil => None
        case x::xs => if(predicate(x)) Some((x, xs)) else None
    }

def symbol(s : Char) : PResult \ Parse = 
    Parse.satisfy(sat(c -> c == s))

def digit() : PResult \ Parse = 
    Parse.satisfy(sat(isDigit))

def letter() : PResult \ Parse = 
    Parse.satisfy(sat(isLetter))

def choice(left : Unit -> PResult \ Parse, right : Unit -> PResult \ Parse) : PResult \ {Parse, ParseControl} =
    let decision = ParseControl.flip();
    if(decision) left() else right()

def runParser(input : List[Char]) : PResult \ ParseControl = 
    region exec {
        let src = Ref.fresh(exec, input);
        run{
            choice(_ -> letter(), _ -> {letter(); letter()})
        }with handler Parse {
            def satisfy(p, k) = 
                match p(Ref.get(src)) {
                    case None => ParseControl.fail() 
                    case Some((x, rest)) => {Ref.put(rest, src); ParseControl.join(x::Nil, k(Nil))} 
                }
        }
    }


// The main entry point.
def main(): PResult \ IO =
    let src = 'a'::'a'::'a'::Nil;
    run{
        runParser(src)
    }with handler ParseControl {
        def fail(_k) = Nil
        def flip(k) = {let goRight = k(false); if(goRight == Nil) goRight else k(true)}
        def join(lhs, rhs, k) = k(lhs ::: rhs)
    }
    