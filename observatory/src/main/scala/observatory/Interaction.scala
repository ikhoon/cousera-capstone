package observatory

import java.io.File

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  import math._
  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val lon = x / pow(2, zoom) * 360 - 180
    val lat = atan(sinh(Pi - y / pow(2, zoom) * 2 * Pi)) * 180 / Pi
    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    Image(256, 256, pixels(temperatures, colors, zoom, x, y))
  }

  def pixels(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Array[Pixel] = {
    import Visualization._
    (0 until 256 * 256)
      .map(i => tileLocation(zoom + 8, x * 256 + i % 256, y * 256 + i / 256))
      .map(loc => predictTemperature(temperatures, loc))
      .map(temp => interpolateColor(colors, temp))
      .map(color => Pixel(color.red, color.green, color.blue, 127))
      .toArray
  }


  def locationToPos(location: Location): (Int, Int) = {
    val x = location.lon + 180
    val y = (location.lat - 90) * -1
    (x.toInt, y.toInt)
  }


  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    for {
      zoom <- 0 to 3
      x <- 0 until pow(2, zoom).toInt
      y <- 0 until pow(2, zoom).toInt
      (year, data) <- yearlyData
    } generateImage(year, zoom, x, y, data)
  }

}
