package observatory

import java.time.LocalDate

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.rdd.RDD

import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {

  @transient lazy val conf: SparkConf = new SparkConf().setMaster("local").setAppName("Observatory")
  @transient lazy val sc: SparkContext = new SparkContext(conf)
  sc.setLogLevel("WARN")

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  override def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String):
  Iterable[(LocalDate, Location, Temperature)] = {
    val stationsRDD: RDD[String] = getRDDFromResource(stationsFile)
    val temperatureRDD: RDD[String] = getRDDFromResource(temperaturesFile)

    val stations: RDD[(StationIdentifier, Location)] =
      parseStations(stationsRDD)
    val temperatureData: RDD[(StationIdentifier, TemperatureData)] =
      parseTemperatureData(year, temperatureRDD)
    val locateTemperaturesRDD: RDD[(LocalDate, Location, Temperature)] =
      stations join temperatureData map {
        case (_, (loc: Location, td: TemperatureData)) => (td.localDate, loc, td.temperature)
      }

    locateTemperaturesRDD.collect
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  override def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]):
  Iterable[(Location, Temperature)] = {
    val recordsRDD: RDD[(LocalDate, Location, Temperature)] = sc.parallelize(records.toSeq)

    val averageRecordsRDD: RDD[(Location, Temperature)] = {
      type TemperatureCounter = (Int, Temperature)

      val zeroValue: TemperatureCounter = (0, 0.0)

      def seqOp(acc: TemperatureCounter, t: Temperature): TemperatureCounter = (acc._1 + 1, acc._2 + t)

      def combOp(a: TemperatureCounter, b: TemperatureCounter): TemperatureCounter = (a._1 + b._1, a._2 + b._2)

      def toTemperature(x: TemperatureCounter): Temperature = x._2 / x._1

      (recordsRDD map {
        case (_, loc: Location, t: Temperature) => (loc, t)
      }).aggregateByKey(zeroValue)(seqOp, combOp) mapValues toTemperature
    }

    averageRecordsRDD.collect()
  }

  /**
    * Case class representing a station unique identifier
    *
    * @param stn  STN identifier
    * @param wban WBAN identifier
    */
  private case class StationIdentifier(stn: String, wban: String)

  /**
    * Case class representing a temperature at a specific date
    *
    * @param localDate   Date for this data point
    * @param temperature Temperature for this data point
    */
  private case class TemperatureData(localDate: LocalDate, temperature: Temperature)

  /**
    * Reads a resource from Source and parallelizes it as a RDD
    *
    * @param resource Resource name to open
    * @return
    */
  private def getRDDFromResource(resource: String): RDD[String] = {
    val fileStream = Source.getClass.getResourceAsStream(resource)
    sc.parallelize(Source.fromInputStream(fileStream).getLines().toList)
  }

  /**
    * Parses the stations RDD (with unparsed line strings) into a RDD with station identifiers as keys.
    *
    * @param stationsRDD RDD with lines to parse
    * @return
    */
  private def parseStations(stationsRDD: RDD[String]): RDD[(StationIdentifier, Location)] = {
    @inline def parseLine(line: String): Option[(StationIdentifier, Location)] = {
      val arr = line.split(",")

      if (arr.length != 4 || arr(2).isEmpty || arr(3).isEmpty)
        None
      else {
        val id = StationIdentifier(arr(0), arr(1))
        val location = Location(arr(2).toDouble, arr(3).toDouble)

        Some((id, location))
      }
    }

    stationsRDD flatMap parseLine
  }

  /**
    * Parses the temperature RDD (with unparsed line strings) into a RDD with station identifiers as keys.
    *
    * @param year           Year to assume for this provided dates
    * @param temperatureRDD Temperature RDD to parse
    * @return
    */
  private def parseTemperatureData(year: Year, temperatureRDD: RDD[String]): RDD[(StationIdentifier, TemperatureData)] = {
    @inline def parseTemperature(tempF: Double): Temperature = (tempF - 32) * 5 / 9

    @inline def parseLine(line: String): Option[(StationIdentifier, TemperatureData)] = {
      val arr = line.split(",")

      if (arr.length != 5)
        None
      else {
        val id = StationIdentifier(arr(0), arr(1))
        val temperatureData = TemperatureData(
          localDate = LocalDate.of(year, arr(2).toInt, arr(3).toInt),
          temperature = parseTemperature(arr(4).toDouble)
        )
        Some((id, temperatureData))
      }
    }

    temperatureRDD flatMap parseLine
  }

}
