use Parse.satisfy

type alias Parser = List[Char] -> Option[(Char, List[Char])]

eff Parse {
    def satisfy(p : Parser) : List[Char]
}

eff ParseControl {
    def fail() : Void
    def flip() : Bool
    def join(lhs : List[Char], rhs : List[Char]) : List[Char] 
}

def symbol(s : Char) : List[Char] \ Parse = 
    Parse.satisfy(input -> match input {
        case Nil => None
        case x::xs => if(x == s) Some((x, xs)) else None
    })

//TODO how to do many and choice? 

def runParser(input : List[Char]) : List[Char] \ ParseControl = 
    region exec {
        let src = Ref.fresh(exec, input);
        run{
            symbol('a');
            symbol('b');
            symbol('c')
        }with handler Parse {
            def satisfy(p, k) = 
                match p(Ref.get(src)) {
                    case None => 'f'::Nil //call ParseControl.fail here? 
                    case Some((x, rest)) => {Ref.put(rest, src); ParseControl.join(x::Nil, k(Nil))} //TODO figure out what parsers should return
                }
        }
    }


// The main entry point.
def main(): List[Char] =
    let src = 'a'::'b'::'c'::Nil;
    run{
        runParser(src)
    }with handler ParseControl {
        def fail(k) = ??? // TODO figure out how to fail properly
        def flip(k) = ???
        def join(lhs, rhs, k) = k(lhs ::: rhs)
    }
    