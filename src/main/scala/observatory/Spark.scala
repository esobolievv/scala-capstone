package observatory

import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql.{Dataset, Encoder, Encoders, SparkSession}
import org.apache.spark.sql.types.{DoubleType, IntegerType, StructField, StructType}

import scala.io.Source
import scala.reflect.ClassTag

object Spark {

  lazy val spark: SparkSession = SparkSession
    .builder()
    .appName("Spark capstone observatory")
    .master("local[*]")
    .getOrCreate()

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  lazy val stationsSchema: StructType = StructType(Seq(
    StructField("stn", IntegerType, nullable = true),
    StructField("wban", IntegerType, nullable = true),
    StructField("lat", DoubleType, nullable = false),
    StructField("lon", DoubleType, nullable = false)))

  lazy val temperatureSchema: StructType = StructType(Seq(
    StructField("stn", IntegerType, nullable = true),
    StructField("wban", IntegerType, nullable = true),
    StructField("month", IntegerType, nullable = false),
    StructField("day", IntegerType, nullable = false),
    StructField("temperature", DoubleType, nullable = false)))


  implicit def single[A](implicit c: ClassTag[A]): Encoder[A] = Encoders.kryo[A](c)

  implicit def tuple3[A1, A2, A3](implicit e1: Encoder[A1],
                                  e2: Encoder[A2],
                                  e3: Encoder[A3]): Encoder[(A1, A2, A3)] = Encoders.tuple[A1, A2, A3](e1, e2, e3)

  /**
    * Get absolute file path for file which locates inside project resource dir.
    *
    * @param resource Relative path starts from project resource dir (e.g. "/2015.csv")
    * @return
    */
  def getRDDFromResource(resource: String): Dataset[String] = {
    // For implicit conversions like converting RDDs to DataFrames, autoEncoders etc.
    import spark.implicits._
    val fileStream = Source.getClass.getResourceAsStream(resource)
    spark.sparkContext.parallelize(Source.fromInputStream(fileStream).getLines.toIndexedSeq).toDS
  }

}
