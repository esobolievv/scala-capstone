package observatory

import java.time.LocalDate

import observatory.Spark._
import org.apache.spark.sql._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types.{DoubleType, IntegerType, StructField, StructType}

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {

  // For implicit conversions like converting RDDs to DataFrames, autoEncoders etc.

  import spark.implicits._

  val stationsSchema: StructType = StructType(
    Seq(
      StructField("stn", IntegerType, nullable = true),
      StructField("wban", IntegerType, nullable = true),
      StructField("lat", DoubleType, nullable = false),
      StructField("lon", DoubleType, nullable = false)
    )
  )

  val temperatureSchema: StructType = StructType(
    Seq(
      StructField("stn", IntegerType, nullable = true),
      StructField("wban", IntegerType, nullable = true),
      StructField("month", IntegerType, nullable = false),
      StructField("day", IntegerType, nullable = false),
      StructField("temperature", DoubleType, nullable = false)
    )
  )

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String = "/stations.csv", temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val stationDs: Dataset[StationModel] = spark.read.schema(stationsSchema)
      .option("mode", "DROPMALFORMED")
      .csv(getRDDFromResource(stationsFile))
      .as[StationModel]

    val temperatureDs: Dataset[TemperatureModel] = spark.read.schema(temperatureSchema)
      .option("mode", "DROPMALFORMED")
      .csv(getRDDFromResource(temperaturesFile))
      .as[TemperatureModel]

    val toCelsius = (value: Double) => (value - 32.0d) * 5.0d / 9.0d

    val joinCond: Column = stationDs("stn").eqNullSafe(temperatureDs("stn")) &&
      stationDs("wban").eqNullSafe(temperatureDs("wban")) &&
      stationDs("lat").isNotNull && stationDs("lon").isNotNull

    stationDs.join(temperatureDs, joinCond).mapPartitions(_.map { row =>
      val localDate = LocalDate.of(year, row.getAs[Int]("month"), row.getAs[Int]("day"))
      val location = Location(row.getAs[Double]("lat"), row.getAs[Double]("lon"))
      val temp = toCelsius(row.getAs[Temperature]("temperature"))
      (localDate, location, temp)
    }).collect
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    spark.sparkContext
      .parallelize(records.toIndexedSeq)
      .mapPartitions(_.map(row => (row._1.getYear, row._2, row._3)))
      .toDF("year", "loc", "temp")
      .groupBy("year", "loc").agg(avg("temp").as("temp"))
      .select(col("loc").as[Location], col("temp").as[Temperature])
      .collect
  }

}
