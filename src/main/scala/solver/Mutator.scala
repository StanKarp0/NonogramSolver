package solver

/**
  * Created by wojciech on 22.02.17.
  */
object Mutator {

  def recursive(list: List[Int], size: Int)(filter: List[Block] => Boolean = _ => true): Iterator[List[Block]] = {
    def rec(ls: List[Block], ind: Int): Iterator[List[Block]] = ls.lift(ind) match {
      case Some(_: WB) if !filter(ls.take(ind+1)) => Iterator()
      case Some(_: WB) =>
        val left = ls.take(ind)
        val right = ls.drop(ind + 1)
        Iterator.from(1).map(x => left ::: (WB(x) :: right))
          .takeWhile(_.map(_.size).sum <= size)
          .flatMap(rec(_, ind + 1))
      case Some(_: WR) if ind == 0 =>
        val right = ls.tail
        Iterator.from(0).map(x => WR(x) :: right)
          .takeWhile(_.map(_.size).sum <= size)
          .flatMap(rec(_, 1))
      case Some(_: BB) => rec(ls, ind + 1)
      case _ =>
        Iterator(ls)
    }
    rec(Block(list), 0)
  }

  def reduceLists(list: List[List[Field]], pattern: Option[List[Field]] = None): List[Field] = {
    def reduce(list: List[Field]): Field = list match {
      case White :: tail if tail.forall{
        case White => true
        case _ => false
      } =>  White
      case Black(i) :: tail if tail.forall{
        case Black(j) if j == i => true
        case BlackAny => true
        case _ => false
      } => Black(i)
      case Black(_) :: tail if tail.forall{
        case Black(_) => true
        case _ => false
      } => BlackAny
      case _ => Empty
    }
    pattern.map{ p => list.filterNot{ l => p.zip(l).exists{
      case (White, Black(_)) => true
      case (BlackAny | Black(_), White) => true
      case _ => false
    }}}.getOrElse(list).transpose.map(reduce)

  }

}
