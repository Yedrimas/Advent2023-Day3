import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import java.awt.Point

case class ExampleTest() extends AnyFunSpec with Matchers {

    implicit class AutoSorter(l: Iterable[NumberPosition]) {
        def autoSort: Seq[NumberPosition] = {
            l.toSeq.sortBy(s => (s.value, s.positionX, s.positionY))
        }
    }

    implicit class AutoPointSorter(l: Iterable[Point]) {
        def autoSort: Seq[Point] = {
            l.toSeq.sortBy(p => (p.x, p.y))
        }
    }

    describe("A line of '123..123.123'") {
        it("Should group the consecutive numbers") {
            Utils
              .extractNumberPosition("123..123.123").autoSort shouldEqual Seq(
                NumberPosition(123, -1, 0),
                NumberPosition(123, -1, 5),
                NumberPosition(123, -1, 9),
            ).autoSort
        }
    }

    describe("Given the test input") {


        val input: Seq[String] = Utils.readInput("example.txt")
        val parts = Utils.extracNumberPositionFromTable(input)

        describe("Part1") {
            it("Should parse the example") {

                parts.autoSort shouldEqual Seq(
                    NumberPosition(467, 0, 0),
                    NumberPosition(114, 0, 5),
                    NumberPosition(35, 2, 2),
                    NumberPosition(633, 2, 6),
                    NumberPosition(617, 4, 0),
                    NumberPosition(58, 5, 7),
                    NumberPosition(592, 6, 2),
                    NumberPosition(755, 7, 6),
                    NumberPosition(664, 9, 1),
                    NumberPosition(598, 9, 5),
                ).autoSort
            }

            it("Should give points for the first element") {
                val numberPosition = NumberPosition(467, 0, 0)
                val aroundPoints = numberPosition.getAroundPoints(input)
                aroundPoints.autoSort shouldEqual Seq(
                    new Point(3, 0),
                    new Point(0, 1),
                    new Point(1, 1),
                    new Point(2, 1),
                    new Point(3, 1),
                ).autoSort
            }

            it("First element should be a part number") {
                NumberPosition(467, 0, 0).isPartNumber(input) shouldBe true
            }

            it("The second element should not be a part number") {
                NumberPosition(114, 0, 5).isPartNumber(input) shouldBe false

            }

            it("Should sum to 4361") {
                Utils.sumPartNumber(parts, input) shouldEqual 4361
            }
        }

        describe("Part 2") {
            val gears = Utils.getGearsFromTable(input)
            it("Should find the gears") {
                gears.autoSort shouldEqual Seq(
                    NumberPosition(1, 1, 3),
                    NumberPosition(1, 4, 3),
                    NumberPosition(1, 8, 5),
                ).autoSort
            }

            it("Should find 2 valid gears") {
                gears.filter(_.isAValidGear(parts, input)).toList.length shouldEqual 2
            }

            it("Should sum valid gear parts to 467835") {
                Utils.sumValidGears(parts, gears, input) shouldEqual 467835
            }
        }

    }

}
