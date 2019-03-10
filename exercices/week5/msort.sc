def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (zs, Nil) => zs
        case (Nil, zs) => zs
        case (x :: xtail, y :: ytail) =>
          if (ord.lt(x, y)) x :: merge(xtail, ys)
          else y :: merge(xs, ytail)
      }

    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}


msort(List(43, 3, 434, 24, 324, 1, 993))
msort(List(43, 3, 1, 434, 24, 324, 1, 993))

