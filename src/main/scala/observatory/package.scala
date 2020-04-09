import org.apache.spark.sql.{Encoder, Encoders, SparkSession}
import org.apache.spark.sql.types.{DoubleType, IntegerType, StructField, StructType}

import scala.reflect.ClassTag

package object observatory {

  type Temperature = Double // °C, introduced in Week 1
  type Year = Int // Calendar year, introduced in Week 1

  val spark: SparkSession = SparkSession
    .builder()
    .appName("Spark capstone observatory")
    .master("local[*]")
    .getOrCreate()

  spark.sparkContext.setLogLevel("ERROR")

  val stationsSchema = StructType(Seq(
    StructField("stn", IntegerType, nullable = true),
    StructField("wban", IntegerType, nullable = true),
    StructField("lat", DoubleType, nullable = false),
    StructField("lon", DoubleType, nullable = false)))

  val temperatureSchema = StructType(Seq(
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
    * @param path Relative path starts from project resource dir (e.g. "/2015.csv")
    * @return
    */
  def getAbsolutePath(path: String): String = getClass.getResource(path).getPath
}
