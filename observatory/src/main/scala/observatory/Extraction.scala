package observatory

import java.io.File
import java.time.LocalDate

import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

import scala.util.Try

case class Station(stn: Option[Int], wban: Option[Int], latitude: Option[Double], longitude: Option[Double])
case class Temperature(stn: Option[Int], wban: Option[Int], month: Short, day: Short, temperature: Double)

trait CsvParser[A] {
  def parse(row: String): A
}
object CsvParser {
  implicit val stationParser = new CsvParser[Station] {
    override def parse(row: String): Station = {
      val tokens = row.split(",")
      Station(
        Try(tokens(0).toInt).toOption,
        Try(tokens(1).toInt).toOption,
        Try(tokens(2).toDouble).toOption,
        Try(tokens(3).toDouble).toOption
      )
    }

  }

  implicit val temperatureParser = new CsvParser[Temperature] {
    override def parse(row: String): Temperature = {
      val tokens = row.split(",")
      Temperature(
        Try(tokens(0).toInt).toOption,
        Try(tokens(1).toInt).toOption,
        tokens(2).toShort,
        tokens(3).toShort,
        fahrenheitToCelsius(tokens(4).toDouble)
      )
    }

  }

  @inline def fahrenheitToCelsius(fahrenheit: Double): Double = (fahrenheit - 32d) * 5 / 9
  def as[A](row: String)(implicit A: CsvParser[A]): A = A.parse(row)

}
/**
  * 1st milestone: data extraction
  */
object Extraction {
  import org.apache.log4j.{Level, Logger}
  Logger.getLogger("org.apache.spark").setLevel(Level.OFF)

  val conf = new SparkConf().setMaster("local[*]").setAppName("observatory") //.set("spark.driver.host", "localhost")
  val sc = new SparkContext(conf)

  var stations : Option[RDD[((Option[Int], Option[Int]), Iterable[Station])]] = None


  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {

    val stations = stationRDD(stationsFile)
    val temperatures = temperatureRDD(temperaturesFile)
    stations
      .join(temperatures)
      .flatMapValues { case (s, t) =>
        for {
          s1 <- s
          t2 <- t
        } yield (LocalDate.of(year, t2.month, t2.day), Location(s1.latitude.get, s1.longitude.get), t2.temperature)
      }
      .values
      .collect()
  }

  def stationRDD(stationsFile: String): RDD[((Option[Int], Option[Int]), Iterable[Station])] = {
    stations match {
      case Some(st) => st
      case None =>
        val st = sc.textFile(filePath(stationsFile)).map(CsvParser.as[Station])
          .filter(station => station.latitude.isDefined && station.longitude.isDefined)
          .groupBy(station => (station.stn, station.wban))
          .cache()
        stations = Some(st)
        st
    }
  }

  def temperatureRDD(temperaturesFile: String): RDD[((Option[Int], Option[Int]), Iterable[Temperature])] = {
    sc.textFile(filePath(temperaturesFile)).map(CsvParser.as[Temperature])
      .groupBy(temperature => (temperature.stn, temperature.wban))
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: RDD[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records
      .groupBy(_._2)
      .map { case (loc, list)=> loc -> (list.map(_._3).sum / list.size)}
      .collect()
  }

  def filePath(name: String): String =
    this.getClass.getResource(name).getFile




}
