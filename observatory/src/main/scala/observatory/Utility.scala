package observatory

/**
  * Created by ikhoon on 07/05/2017.
  */
object Utility {
  import math._

  def show(marker: String, iterable: Iterable[_]): String = s"## $marker ${iterable.mkString("Seq(", ",\n", ")\n")}"

  def locationDistance(x: Location, y: Location): Double = {
    // d = r * delta sigma
    val radius = 6371
    val sigma = acos(
      sin(x.lat.toRadians) * sin(y.lat.toRadians)
        + cos(x.lat.toRadians) * cos(y.lat.toRadians) * cos(abs(x.lon.toRadians - y.lon.toRadians))
    )
    radius * sigma
  }

  def colorDistance(x: Color, y: Color): Double =
    sqrt(
      pow(y.red - x.red, 2) +
      pow(y.green - x.green, 2) +
      pow(y.blue - x.blue, 2)
    )
}
