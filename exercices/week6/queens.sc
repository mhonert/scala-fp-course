def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] =
    if (k == 0) Set(List())
  else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens
  placeQueens(n)
}

def isSafe(col: Int, queens: List[Int]): Boolean = {
  val q = queens.size
  queens.zipWithIndex forall (p => (p._1 - col) != 0 &&
    (p._2 - q).abs - (p._1 - col).abs != 0)
}

queens(4)








