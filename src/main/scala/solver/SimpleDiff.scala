package solver

/**
  * Created by wojciech on 22.02.17.
  */
object SimpleDiff {


  def apply(blocks: List[Int], size: Int): List[Field] = {
    val list = Field(blocks)
    List.fill(size - list.size)(White) match {
      case Nil => list
      case toAdd =>
        val left = toAdd ::: list
        val right = list ::: toAdd
        left.zip(right).map {
          case (Black(i1), Black(i2)) if i1 == i2 => Black(i1)
          case _ => Empty
        }
    }


  }



}