use Parse.satisfy

type alias Parser = List[Char] -> Option[(Char, List[Char])]

eff Parse {
    def satisfy(p : Parser) : Char
}

def symbol(s : Char) : Char \ Parse = 
    Parse.satisfy(input -> match input {
        case Nil => None
        case x::xs => if(x == s) Some((x, xs)) else None
    })


// The main entry point.
def main(): Char =
    region exec {
        let src = Ref.fresh(exec, 'a'::'b'::'c'::Nil);
        run{
            symbol('a');
            symbol('q')
        }with handler Parse {
            def satisfy(p, k) = 
                match p(Ref.get(src)) {
                    case None => 'f' // Failure 
                    case Some((x, rest)) => {Ref.put(rest, src); k(x)}
                }
        }
    }