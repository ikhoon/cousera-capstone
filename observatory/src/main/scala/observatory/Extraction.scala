package observatory

import java.io.File
import java.nio.file
import java.nio.file.Paths
import java.time.LocalDate

import org.apache.spark.rdd.RDD
import org.apache.spark.sql.functions.avg
import org.apache.spark.sql.{Dataset, Encoder, Encoders, SparkSession}
import org.apache.spark.sql.types._
import org.apache.spark.{SparkConf, SparkContext}

import scala.reflect.ClassTag
import scala.util.Try

case class Station(stn: Option[Int], wban: Option[Int], latitude: Option[Double], longitude: Option[Double])
case class Temperature(stn: Option[Int], wban: Option[Int], month: Int, day: Int, temperature: Double)

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
        tokens(4).toDouble
      )
    }

  }

  def as[A](row: String)(implicit A: CsvParser[A]): A = A.parse(row)

}
/**
  * 1st milestone: data extraction
  */
object Extraction {
  import org.apache.log4j.{Level, Logger}
  Logger.getLogger("org.apache.spark").setLevel(Level.OFF)

  implicit def kryoEncoder[A](implicit ct: ClassTag[A]) =
    org.apache.spark.sql.Encoders.kryo[A](ct)

  implicit def tuple3[A1, A2, A3](
    implicit
      e1: Encoder[A1],
      e2: Encoder[A2],
      e3: Encoder[A3]
  ): Encoder[(A1, A2, A3)] = Encoders.tuple[A1, A2, A3](e1, e2, e3)



  val session = SparkSession
    .builder()
    .appName("observatory")
    .config("spark.master", "local")
    .config("spark.driver.host", "localhost")
    .getOrCreate()

  import session.implicits._
//  val conf = new SparkConf().setMaster("local[*]").setAppName("observatory") //.set("spark.driver.host", "localhost")
//  val sc = new SparkContext(conf)

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stations = stationDS(stationsFile)
    val temperatures = temperatureDS(temperaturesFile)
    stations
      .join(temperatures, stations("stn") <=> temperatures("stn") && stations("wban") <=> temperatures("wban"))
      .map(row => {
        val localDate = LocalDate.of(year, row.getAs[Int]("month"), row.getAs[Int]("day"))
        val location = Location(row.getAs[Double]("latitude"), row.getAs[Double]("longitude"))
        val temperature = fahrenheitToCelsius(row.getAs[Double]("temperature"))
        (localDate, location, temperature)
      })
      .collect()
  }

  // stn: Option[Int], wban: Option[Int], latitude: Option[Double], longitude: Option[Double]
  def stationSchema: StructType =
    StructType(
      Seq(
        StructField("stn", IntegerType, true),
        StructField("wban", IntegerType, true),
        StructField("latitude", DoubleType, true),
        StructField("longitude", DoubleType, true)
      )
    )

  // stn: Option[Int], wban: Option[Int], month: Short, day: Short, temperature: Double
  def temperatureSchema: StructType =
    StructType(
      Seq(
        StructField("stn", IntegerType, true),
        StructField("wban", IntegerType, true),
        StructField("month", IntegerType, false),
        StructField("day", IntegerType, false),
        StructField("temperature", DoubleType, false)
      )
    )


  def stationDS(stationsFile: String): Dataset[Station] =
    session
      .read
      .option("header", false)
      .option("mode", "FAILFAST")
      .schema(stationSchema)
      .csv(filePath(stationsFile))
      .as[Station]
//    loadDataset[Station](filePath(stationsFile), stationSchema)
    .filter((station: Station) => station.longitude.isDefined && station.latitude.isDefined)

  def temperatureDS(temperatureFile: String): Dataset[Temperature] =
    session
      .read
      .option("header", false)
      .option("mode", "FAILFAST")
      .schema(temperatureSchema)
      .csv(filePath(temperatureFile))
      .as[Temperature]
//    loadDataset[Temperature](filePath(temperatureFile), temperatureSchema)
    .filter((temperature: Temperature) => temperature.temperature != 9999.9)

//  def loadDataset[A](path: String, schema: StructType) =
//    session
//      .read
//      .option("header", false)
//      .option("mode", "FAILFAST")
//      .schema(schema)
//      .csv(path)
//      .as[A]


  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    session
      .sparkContext
      .parallelize(records.toSeq)
      .toDF("data", "location", "temperature")
      .groupBy($"location")
      .agg($"location", avg($"temperature").as("temperature"))
      .select($"location".as[Location], $"temperature".as[Double])
      .collect
  }

  def filePath(name: String): String =
    Paths.get(this.getClass.getResource(name).toURI).toString


  def fahrenheitToCelsius(fahrenheit: Double): Double = (fahrenheit - 32d) * 5 / 9


}
