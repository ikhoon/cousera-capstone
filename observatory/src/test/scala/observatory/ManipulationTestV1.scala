package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class ManipulationTestV1 extends FunSuite with Checkers {

  import Manipulation._
  test("make grid") {

    val temperatures = Seq(
      (Location(45.0,-90.0),-60.74435267469656),
      (Location(-45.0,0.0),26.43807771123359))
    val loc1 = Location(45.0,-90.0)
    val loc2 = Location(25.0, -70.0)
    val loc3 = Location(-45.0,0.0)
    assert(makeGrid(temperatures)(loc1.lat.toInt, loc1.lon.toInt) == -60.74435267469656)
    assert(makeGrid(temperatures)(loc3.lat.toInt, loc3.lon.toInt) == 26.43807771123359)
    val gridTemperature = makeGrid(temperatures)(loc2.lat.toInt, loc2.lon.toInt)
    assert(math.abs(gridTemperature - temperatures.head._2) < math.abs(gridTemperature - temperatures.last._2))
  }

}