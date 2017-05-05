package observatory

case class Location(lat: Double, lon: Double)

case class Color(red: Int, green: Int, blue: Int) {
  def +(other: Color): Color =
    Color(red + other.red, green + other.green, blue + other.blue)

  def *(scale: Double): Color =
    Color(math.round(red * scale).toInt, math.round(green * scale).toInt, math.round(blue * scale).toInt)

  def -(other: Color): Color =
    this + (other * -1)
}

