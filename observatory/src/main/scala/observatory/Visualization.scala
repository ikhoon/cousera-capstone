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
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    val distances = temperatures.map { case (loc, temp) => distance(location, loc) -> temp }
    val nearest = distances.minBy(_._1)
    if (nearest._1 < 1) nearest._2
    else {
      val ws = temperatures.map { case (loc, temp) => (weight(loc, location), temp) }
      ws.map { case (x , y) => x + y }.sum / ws.map(_._1).sum
    }
  }

  private def weight(x: Location, y: Location): Double =
    1 / pow(distance(x, y), power)

  private def distance(x: Location, y: Location): Double = {
    // d = r * delta sigma
    val radius = 6371
    val sigma = acos(sin(x.lat) * sin(y.lat) + cos(x.lat) * cos(y.lat) * cos(abs(x.lon - y.lon)))
    radius * sigma
  }
  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
//    throw new Exception(points.mkString("################\n", "\n", "\n############ (" + value + ")"))
    // 255, 0, 0 <=> 0, 0, 255 두개의
    // 젤 가까운 점을 두개 찾자 그리고 그점의 중간컬러를 찾자.
    val sorted = points.toSeq.sortBy(_._1)
    val max = sorted.filter(_._1 >= value).minBy(_._1)
    val min = sorted.filter(_._1 <= value).maxBy(_._1)
    val delta: Color = (max._2 - min._2) * ((value - min._1) / (max._1 - min._1))
    min._2 + delta
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val pixels: Seq[Pixel] = temperatures.map { case (loc, temp) => {
      val width = loc.lon + 180
      val height = (loc.lat - 90) * -1
      val color = interpolateColor(colors, temp)
      (width, height, Pixel(color.red, color.green, color.blue, 1))
      }
    }.toSeq.sortBy(r => r._1 * 360 + r._2)
    .map(_._3)
    Image(360, 180, pixels.toArray)
  }

}

