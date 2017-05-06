package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

//---
  import com.sksamuel.scrimage.Image
  import Interaction._
  import scala.math.BigDecimal.RoundingMode
  import org.scalacheck.Prop._
  import Utility._

  test("tileLocation must return the corresponding latitude and longitude, given some Web Mercator coordinates") {
    val location = tileLocation(0, 0, 0)
    assert(
      BigDecimal(location.lat).setScale(4, RoundingMode.HALF_UP) == BigDecimal(85.0511).setScale(4, RoundingMode.HALF_UP) &&
        location.lon == -180.0
    )
  }

  test("tile pixel colors must be consistent with the given located temperatures and color scale") {
    check { (b: Boolean) =>
      val (t1, t2) = if (b) (10.0, 20.0) else (5.0, 30.0)
      val l1 = Location(45.0, -90.0)
      val l2 = Location(-45.0, 0.0)

      val temperatures = Seq(l1 -> t1, l2 -> t2)

      val c1 = Color(255, 0, 0)
      val c2 = Color(0, 0, 255)
      val scale = Seq(t1 -> c1, t2 -> c2)

      val image = tile(temperatures, scale, 0, 0, 0)

      image.points.forall { case (x, y ) =>
        val pixel = image.pixel(x, y)
        val l = tileLocation(8, x, y)
        val c = Color(pixel.red, pixel.green, pixel.blue)

        if((math.abs(locationDistance(l, l1) - locationDistance(l, l2)) < 1) ||
        ((locationDistance(l, l1) < locationDistance(l, l2)) == (colorDistance(c, c1) < colorDistance(c, c2)))) {
          true
        }
        else {
          println(s"x: $x, y: $y, l: $l, l1: $l1, l2: $l2, c: $c, c1: $c1, c2: $c2")
          false
        }
      }
    }
  }


  test("tile must be consistent accross zoom levels") {
    val temperatures =
      Array(
        Location(45.0, -90.0) -> 20.0,
        Location(45.0, 90.0) -> 0.0,
        Location(0.0, 0.0) -> 10.0,
        Location(-45.0, -90.0) -> 0.0,
        Location(-45.0, 90.0) -> 20.0
      )

    val scale =
      Seq(
        0.0 -> Color(255, 0, 0),
        10.0 -> Color(0, 255, 0),
        20.0 -> Color(0, 0, 255)
      )

    val image = tile(temperatures, scale, 0, 0, 0)
    val subImages =
      for {
        x <- 0 to 1
        y <- 0 to 1
      } yield {
        tile(temperatures, scale, 1, x, y)
          .padRight(256)
          .padBottom(256)
          .translate(x * 256, y * 256)
      }

    val `256?256` = (256, 256)
    assert(image.dimensions == `256?256`)

    val image2 =
      subImages.foldLeft(Image(512, 512))(_ underlay _)
        .scaleTo(256, 256)

    image2.pixels.zip(image.pixels).foreach { case (subPixel, pixel) =>
      val subColor = Color(subPixel.red, subPixel.green, subPixel.blue)
      val color = Color(pixel.red, pixel.green, pixel.blue)
      assert(colorDistance(subColor, color) < 30)
    }

  }

  test("generateTiles covers all the expected tiles") {
    val years = 1900 to 2000
    def temperaturess =
      years.to[Stream].map { year =>
        year -> (())
      }
    val tiles = TrieMap.empty[(Int, (Int, Int, Int)), Unit]
    def generateImage(year: Int, zoom: Int, x: Int, y: Int, data: Unit): Unit = {
      tiles += (((year, (zoom, x, y)), ()))
      ()
    }
    Interaction.generateTiles(temperaturess, generateImage)
    val expectedTiles =
      (
        for {
          year <- years
          z <- 0 to 3
          x <- 0 until (1 << z)
          y <- 0 until (1 << z)
        } yield (year, (z, x, y))
      ).to[Set]
    val unexpectedTiles = tiles.keySet -- expectedTiles
    val missingTiles = expectedTiles -- tiles.keySet
    val noExtraTiles = unexpectedTiles.isEmpty
    assert(noExtraTiles, s"Unexpected tiles: ${unexpectedTiles.take(10)}") // Note: we `take(10)` because coursera limits the output to 64 kB
    val noMissingTiles = missingTiles.isEmpty
    assert(noMissingTiles, s"Missing tiles: ${missingTiles.take(10)}")
  }


/*++++++*/
}