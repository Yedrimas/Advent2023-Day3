case class RawNumberPosition(
                              chars: Seq[Char],
                              lastXPosition: Int
                            ) {
    def toNumberPosition: NumberPosition = {
        val len = chars.length
        NumberPosition(
            value = chars.mkString.toInt,
            positionY = -1,
            positionX = lastXPosition - (len - 1)
        )
    }
}
