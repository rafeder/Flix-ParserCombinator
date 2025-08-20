use Parser.{parse, many, many1, choice, letter, empty}

pub def global(p : Unit -> PResult \ Parsing) : PResult \ Parsing = choice(_ -> many1(p), empty)
def main() : PResult = 
    let input = "abc";
    let _local = (p : Unit -> PResult \ Parsing) -> (choice(_ -> many1(p), _ -> empty())); 
    parse(input, _ -> many(letter))
    