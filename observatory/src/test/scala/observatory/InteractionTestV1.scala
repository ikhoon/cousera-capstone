package observatory

import java.time.LocalDate

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

@RunWith(classOf[JUnitRunner])
class InteractionTestV1 extends FunSuite with Checkers {
  import Interaction._

  test("tile image size 256") {
    val temperatures = Seq(
      (Location(45.0,-90.0),5.0),
      (Location(-45.0,0.0),30.0)
    )
    val colors = Seq(
      (5.0,Color(255,0,0)),
      (30.0,Color(0,0,255))
    )

    val zoom = 0
    val x = 0
    val y = 0

    val image = tile(temperatures, colors, zoom, x, y)
    check(image.height == 256)
    check(image.width == 256)

  }

  test("location to pos") {
    val location = Location(0, 0)
    val (x, y) = locationToPos(location)
    assert(x == 180)
    assert(y == 90)
  }

  test("generate tile images") {

    val year = 1975
    val temperatures: Iterable[(LocalDate, Location, Double)] = Extraction.locateTemperatures(year, "/stations.csv", s"/$year.csv")
    val yearly = Extraction.locationYearlyAverageRecords(temperatures)
  }

}
