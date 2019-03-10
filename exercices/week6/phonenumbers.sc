val in = "Hello World with some more word"
val words = in.split(' ').toList

val mnemonics = Map(
  '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
  '6' -> "MNO", '7' -> "PRRS", '8' -> "TUV", '9' -> "WXYZ")

//val charCode =
//  ('2' to '9' foldLeft Map[Char, Char]())((m, num) =>
//    m ++ (mnemonics(num) map (_ -> num)))

val charCode = for ((digit, str) <- mnemonics; ltr <- str) yield ltr -> digit

def wordCode(word: String): String = word map(c => charCode(c.toUpper))



def encode(number: String): Set[List[String]] =
  if (number.isEmpty) Set(List())
  else
    for {
      split <- 1 to number.length
      word <
    }


