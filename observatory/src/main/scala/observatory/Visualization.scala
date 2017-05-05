package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  import math._
  val power = 2
  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  // temperature (Location(0.0,0.0),10.0)
  // location : (Location(90.0,-180.0))
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    val distances = temperatures.map { case (loc, temp) => distance(location, loc) -> temp }
    val nearest = distances.minBy(_._1)
    if (nearest._1 < 1) nearest._2
    else {
      val ws = temperatures.map { case (loc, temp) => (weight(loc, location), temp) }
      ws.map { case (x , y) => x * y }.sum / ws.map(_._1).sum
    }
  }

  private def weight(x: Location, y: Location): Double =
    1 / pow(distance(x, y), power)

  def distance(x: Location, y: Location): Double = {
    // d = r * delta sigma
    val radius = 6371
    val sigma = acos(
        sin(x.lat.toRadians) * sin(y.lat.toRadians)
      + cos(x.lat.toRadians) * cos(y.lat.toRadians) * cos(abs(x.lon.toRadians - y.lon.toRadians))
    )
    radius * sigma
  }
  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    val sorted = points.toSeq.sortBy(_._1)
    val gt = sorted.filter(_._1 >= value)
    val max = if(gt.isEmpty) sorted.last else gt.minBy(_._1)
    val lt = sorted.filter(_._1 <= value)
    val min = if(lt.isEmpty) sorted.head else lt.maxBy(_._1)
    val delta: Color = (max._2 - min._2) * ((value - min._1) / (max._1 - min._1))
    min._2 + delta
  }

  val IMAGE_WIDTH = 360
  val IMAGE_HEIGHT = 180
  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    println(temperatures.mkString("\n\n######\n", "\n", "\n######") + colors.mkString("#####\n", "\n", "\n######"))
    Image(IMAGE_WIDTH, IMAGE_HEIGHT, pixelize(IMAGE_WIDTH, IMAGE_HEIGHT, temperatures, colors).map(_._3).toArray)
  }


  def pixelize(width: Int, height: Int, temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Seq[(Int, Int, Pixel)] = {
    val centerX = width / 2
    val centerY = height / 2
    (0 until width * height).map { i =>
      val lon = i % width - centerX
      val lat = -i / width + centerY
      val temp = predictTemperature(temperatures, Location(lat, lon))
      val color = interpolateColor(colors, temp)
      (i % width, i / width, Pixel(color.red, color.green, color.blue, 255))
    }
  }

}

