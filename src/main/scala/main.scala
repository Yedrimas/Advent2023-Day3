case object main {


    def main(args: Array[String]): Unit = {
        val puzzle = Utils.readInput("puzzle.txt")
        val parts = Utils.extracNumberPositionFromTable(puzzle)

        // Part 1
        println(Utils.sumPartNumber(parts, puzzle))
    }

}
