package observatory

import org.junit.runner.RunWith
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
//---
class ManipulationTest extends FunSuite  with Checkers {
  val milestoneName = "#4 - Data manipulation"
/*+++
class ManipulationTest extends FunSuite with Checkers {
+++*/

//---
  import Manipulation._
  import scala.math._
  import org.scalactic.Tolerance.convertNumericToPlusOrMinusWrapper
  import org.scalacheck.Prop._

  // lat [-89, 90] and a longitude in [-180, 179],
  val locationGen = for {
    lat <- Gen.choose(-89, 90)
    lon <- Gen.choose(-180, 179)
  } yield (lat, lon)

  test("makeGrid must return a grid whose predicted temperatures are consistent with the known temperatures") {
    // Note: I?m using a boolean rather than arbitrary doubles because high double values may cause too much numerical imprecision

    check { (b: Boolean) =>
      val (t1, t2) = if (b) (10.0, 20.0) else (5.0, 30.0)
      val l1 = Location(45.0, -90.0)
      val l2 = Location(-45.0, 0.0)
      val temperatures = Seq(l1 -> t1, l2 -> t2)

      val grid = makeGrid(temperatures)


      forAll(locationGen){ case (lat, lon) =>
        val l3 = Location(lat, lon)
        val t3 = grid(lat, lon)

        val temperatureBounds = t3 <= max(t1, t2) && t3 >= min(t1, t2)

        val temperatureLocation = (abs(Visualization.distance(l3, l1) - Visualization.distance(l3, l2)) < 1) || // Let?s check only if the delta is greater than ~1 meter, to avoid precision errors.
          ((Visualization.distance(l3, l1) < Visualization.distance(l3, l2)) == (abs(t3 - t1) < abs(t3 - t2)))
        temperatureBounds && temperatureLocation
      }
    }
  }

  val temperatureTolerance = 1
  test("average must return a grid whose predicted temperatures are the average of the known temperatures") {

    check { (b: Boolean) =>
      val (t1, t2) = if (b) (2.0, 20.0) else (10.0, 15.0)
      val ts1 = Seq(Location(45, -90) -> t1, Location(0, 90) -> t2)
      val ts2 = Seq(Location(0, -90) -> 11.0, Location(45, 90) -> 14.0)
      val expectedAverage = {
        val grid1 = makeGrid(ts1)
        val grid2 = makeGrid(ts2)
        (lat: Int, lon: Int) => (grid1(lat, lon) + grid2(lat, lon)) / 2
      }
      val temperaturess = Seq(ts1, ts2)
      val grid = average(temperaturess)

      forAll(locationGen){ case (lat, lon) =>
        grid(lat, lon) === expectedAverage(lat, lon) +- temperatureTolerance
      }
    }
  }

  test("deviation must return a grid whose predicted temperatures are the deviations of the known temperatures compared to the normals") {
    check { (b: Boolean) =>
      val (t1, t2) = if (b) (3.4, 5.6) else (7.8, 9.10)
      val temperatures1 = Seq(Location(45, -90) -> t1, Location(0, 90) -> t2)
      val temperatures2 = Seq(Location(0, -90) -> 14.0, Location(45, 90) -> 15.0)
      val normals = (lat: Int, lon: Int) => 12.0
      val expectedDeviations1 = {
        val grid1 = makeGrid(temperatures1)
        (lat: Int, lon: Int) => grid1(lat, lon) - normals(lat, lon)
      }
      val expectedDeviations2 = {
        val grid2 = makeGrid(temperatures2)
        (lat: Int, lon: Int) => grid2(lat, lon) - normals(lat, lon)
      }
      val deviationGrid1 = deviation(temperatures1, normals)
      val deviationGrid2 = deviation(temperatures2, normals)

      forAll(locationGen) { case (lat, lon) =>
        deviationGrid1(lat, lon) === expectedDeviations1(lat, lon) +- temperatureTolerance &&
        deviationGrid2(lat, lon) === expectedDeviations2(lat, lon) +- temperatureTolerance
      }
    }
  }

//  Commented because not really relevant (10 thousands years of data is unlikely to happen)
//  test("'average' should support 10 thousands years of data", 3) {
//    try {
//      def temperatures =
//        Stream.continually {
//          Stream.fill(1)(Location(0, 0) -> 0.0)
//        }.take(10000)
//      val grid =
//        average(temperatures)
//      assert(
//        forAllLatLon { (lat, lon) =>
//          grid(lat, lon) === 0.0
//        }
//      )
//    } catch {
//      case t: Throwable => fail(t)
//    }
//  }

/*++++++*/
}