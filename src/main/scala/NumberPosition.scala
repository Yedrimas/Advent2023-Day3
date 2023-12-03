import java.awt.Point

case class NumberPosition(
                           value: Int,
                           positionY: Int,
                           positionX: Int
                         ) {

    val lenght: Int = value.toString.length

    val valuePoints: Seq[Point] = {
        val startX = positionX
        val endX = positionX + lenght - 1
        (startX to endX).map(x => new Point(x, positionY))
    }
    def getAroundPoints(table: Seq[String]): Seq[Point] = {
        val maxX = table.head.length
        val maxY = table.length

        val startX = positionX - 1
        val endX = positionX + lenght

        val yCheck = Seq(
            // The line above
            positionY - 1,
            // The line bellow
            positionY + 1
        )

        val points =
            yCheck.flatMap { y =>
                (startX to endX).map { x => new Point(x, y) }
            }

        (points ++ Seq(
            new Point(startX, positionY),
            new Point(endX, positionY),
        ))
          .filter {
              p => p.x >= 0 && p.x < maxX && p.y >= 0 && p.y < maxY
          }
    }

    def isPartNumber(table: Seq[String]): Boolean = {
        val points = getAroundPoints(table)
        points.exists { point =>
            val c = table(point.y)(point.x)
            !c.isDigit && c != '.'
        }
    }

    def getCloseParts(parts: Iterable[NumberPosition], inputTable: Seq[String]): Seq[NumberPosition] = {
        val thisPoints = getAroundPoints(inputTable).toList
        parts.filter { part =>
              val partPoints = part.valuePoints
              thisPoints.intersect(partPoints).nonEmpty
          }
          .toList
    }

    def isAValidGear(parts: Iterable[NumberPosition], inputTable: Seq[String]): Boolean = {
        getCloseParts(parts, inputTable).length == 2
    }
}
