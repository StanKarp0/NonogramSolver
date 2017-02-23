package solver

/**
  * Created by wojciech on 22.02.17.
  */
sealed trait Block {
  def size: Int
  def toFields: List[Field]
}
//Black block
case class BB(size: Int, index: Int) extends Block {
  def toFields: List[Field] = List.fill(size)(Black(index))
}
//White block
case class WB(size: Int) extends Block {
  def toFields: List[Field] = List.fill(size)(White)
}
//White border block
case class WR(size: Int) extends Block {
  def toFields: List[Field] = List.fill(size)(White)
}

object Block {
  def apply(list: List[Int]): List[Block] =
    (WR(0) :: list.zipWithIndex.flatMap{
      case (x, i) => List(BB(x, i), WB(1))
    }.dropRight(1)) :+ WR(0)


}


sealed trait Field
case class Black(index: Int) extends Field
case object BlackAny extends Field {
  override def toString: String = "B "
}
case object White extends Field {
  override def toString: String = "W "
}
case object Empty extends Field {
  override def toString: String = "E "
}

object Field {
  def apply(list: List[Int]): List[Field] =
    Block(list).flatMap(_.toFields)

  def pattern(list: List[Int], p: List[Field]): List[Field] = {
    def fill2Size(list: List[Field]): List[Field] =
      list ::: List.fill(p.size - list.size)(White)

    Mutator.reduceLists(Mutator.recursive(list, p.size){ block =>
      // TODO
      true
    }.map(_.flatMap(_.toFields)).map(fill2Size).toList, Some(p))
  }

}

