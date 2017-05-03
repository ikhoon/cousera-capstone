package observatory

import java.nio.file.Paths
import java.time.LocalDate

import akka.{Done, NotUsed}
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, IOResult}
import akka.stream.scaladsl.{FileIO, Flow, Keep, RunnableGraph, Sink, Source}
import akka.util.ByteString
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

//@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  import Extraction._

  val stationFile = "/stations.csv"
  val firstYearFile = "/1975.csv"

  test("load file with akka") {
    implicit val system = ActorSystem("file-reader")
    implicit val mat = ActorMaterializer()

    val source : Source[ByteString, Future[IOResult]] = FileIO.fromPath(Paths.get(getClass.getResource(stationFile).toURI))
    val flow: Flow[ByteString, String, NotUsed] =  Flow[ByteString].map(_.utf8String)
    val fs: Future[Stream[String]] = source.via(flow).runFold(Stream[String]()) { _.#::(_) }
    val list : Iterable[String] = Await.result(fs, Duration.Inf)
    list.foreach(println)
  }

  test("load file with spark") {
    val file: RDD[String] = sc.textFile(filePath(stationFile))
    file.foreach(x => println("## " + x))
  }

  test("file name") {
    filePath(stationFile).endsWith(stationFile)
  }

  test("join two files") {
    val stations: RDD[Station] = sc.textFile(filePath(stationFile)).map(CsvParser.as[Station])
    val firstYears: RDD[Temperature] = sc.textFile(filePath(firstYearFile)).map(CsvParser.as[Temperature])

    val a = stations
      .filter(station => station.latitude.isEmpty || station.longitude.isEmpty)
      .groupBy(station => (station.stn, station.wban))
    val b = firstYears.groupBy(temperature => (temperature.stn, temperature.wban))

    a.join(b)
    .mapValues { case (s, t) =>
      s.zip(t).map { case (s1, t2) =>  (LocalDate.of(2015, t2.month, t2.day), Location(s1.latitude.get, s1.longitude.get), t2.temperature)}
    }


 }
  test("join two data") {
    val stationRaw =
      """010013,,,
        |724017,03707,+37.358,-078.438
        |724017,,+37.350,-078.433""".stripMargin.lines.toSeq
    val temperatureRaw =
      """010013,,11,25,39.2
        |724017,,08,11,81.14
        |724017,03707,12,06,32
        |724017,03707,01,29,35.6""".stripMargin.lines.toSeq

    val stations: RDD[Station] = sc.parallelize(stationRaw).map(CsvParser.as[Station])
    val firstYears: RDD[Temperature] = sc.parallelize(temperatureRaw).map(CsvParser.as[Temperature])

    val a = stations
      .filter(station => station.latitude.isDefined && station.longitude.isDefined)
      .groupBy(station => (station.stn, station.wban))
    val b = firstYears.groupBy(temperature => (temperature.stn, temperature.wban))

    a.join(b).collect().foreach(println)
    val actural =
      a.join(b)
        .flatMap { case (_, (s, t)) =>
          for {
            s1 <- s
            t2 <- t
          } yield (LocalDate.of(2015, t2.month, t2.day), Location(s1.latitude.get, s1.longitude.get), t2.temperature)
        }.collect()

    val expected = Seq(
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
    )
    actural.foreach(println)
    assert(actural.size == expected.size)
    assert(actural.map(_._1).toSeq == expected.map(_._1))

  }

  test("csv parser") {
     val stations =
       """007034,,,
         |007037,,,
         |007044,,,
         |007047,,,
         |007052,,,
         |007059,,,
         |007064,,,
         |007070,,+00.000,+000.000
         |007072,,,
         |007076,,,
         |007083,,,
         |007084,,,
         |007094,,,
         |008268,,+32.950,+065.567
         |008307,,+00.000,+000.000""".stripMargin
    val parsed = stations.lines.map(CsvParser.as[Station])
    parsed.foreach(println)

    val temperatures = """010010,,01,01,23.2
                         |010010,,01,02,18.7
                         |010010,,01,03,14.2
                         |010010,,01,04,14.8
                         |010010,,01,05,14.9
                         |010010,,01,06,26.4
                         |010010,,01,07,22.5
                         |010010,,01,08,15.0
                         |010010,,01,09,17.6
                         |010010,,01,10,17.6
                         |010010,,01,11,15.8
                         |010010,,01,12,12.6
                         |010010,,01,13,15.1""".stripMargin

    val parsed2 = temperatures.lines.map(CsvParser.as[Temperature])
    parsed.foreach(println)
  }

  test("temperature conversion") {
    val fahrenheit = 32f
    assert(CsvParserfahrenheitToCelsius(fahrenheit) == 0f)
  }


  test("avergae temperature of year") {
    implicit val ordering = new Ordering[(Location, Double)] {
      override def compare(x: (Location, Double), y: (Location, Double)): Int = (x._2 - y._2).toInt
    }
    val daily = Seq(
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
    )
    val yearly = locationYearlyAverageRecords(daily).toList.sorted

    val expected = Seq(
      (Location(37.35, -78.433), 27.3),
      (Location(37.358, -78.438), 1.0)
    ).sorted
    assert(yearly == expected)

  }

  test("run spark") {
    val year = 1975
    locateTemperatures(year, "/stations.csv", s"/$year.csv")
  }
}