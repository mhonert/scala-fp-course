class Pouring(capacity: Vector[Int]) {
  type State = Vector[Int]
  val initialState = capacity map (x => 0)

  trait Move {
    def change(state: State): State
  }
  case class Empty(glass: Int) extends Move {
    def change(state: State) = state updated (glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    def change(state: State) = state updated(glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State) = {
      val toRemainingCapacity = capacity(to) - state(to)
      val pourAmount = state(from) min toRemainingCapacity
      state.updated(from, state(from) - pourAmount)
           .updated(to, state(to) + pourAmount)
    }
  }

  val glasses = capacity.indices

  val moves =
    (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

  class Path(history: List[Move], val endState: State) {
    def extend(move: Move) = new Path(move :: history, move change endState)
    override def toString = (history.reverse mkString " ") + "---> " + endState
  }

  val initialPath = new Path(Nil, initialState)

  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }

  val pathSets = from(Set(initialPath), Set(initialState))

  def solutions(target: Int): Stream[Path] =
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
}

val problem = new Pouring(Vector(4, 7, 9))
println(problem.pathSets.take(2))
val i = 1
println(problem.solutions(6).take(1))


