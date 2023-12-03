import java.awt.Point
import scala.io.Source

case object Utils {

    def readInput(fName: String): Seq[String] = {
        Source
          .fromResource(fName)
          .mkString
          .split("\n")
          .toSeq
    }


    // On a single line
    def extractNumberPosition(line: String): Iterable[NumberPosition] = {
        val digitsOnly = line
          .zipWithIndex
          .filter(_._1.isDigit)

        val consecutiveGrouped = digitsOnly.foldLeft(Seq[Seq[(Char, Int)]]()) { (acc, digit) =>
            acc match {
                // Acc is empty
                case Nil => Seq(Seq(digit))

                // Acc is not empty, element is consecutive with the last element of the head of the acc (using head, could also use last if we append to the end of the acc)
                case head :: tail if head.last._2 + 1 == digit._2 => (head ++ Seq(digit)) :: tail
                case _ => Seq(Seq(digit)) ++ acc
            }
        }

        consecutiveGrouped
          .map { group =>
              val xStart = group.head._2
              val groupContent = group.map(_._1).mkString.toInt
              NumberPosition(
                  value = groupContent,
                  positionY = -1,
                  positionX = xStart
              )
          }
    }

    // On the whole table
    def extracNumberPositionFromTable(inputTable: Seq[String]): Iterable[NumberPosition] = {
        inputTable
          .zipWithIndex
          .flatMap {
              case (s: String, index: Int) =>
                  // We could put the positionY as an arg of extractNumberPosition, but this does not make sense
                  // to have such an arg as a only handles a string, outside the table
                  // And we dont need to over-optimize this
                  extractNumberPosition(s).map(_.copy(positionY = index))
          }
    }

    def sumPartNumber(parts: Iterable[NumberPosition], input: Seq[String]) : Int = {
        parts
          .filter(_.isPartNumber(input))
          .map(_.value)
          .sum
    }

}
