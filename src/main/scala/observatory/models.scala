package observatory

import java.lang.Math.{PI, atan, sinh}
import java.net.URI

import scala.math._

/**
  * Introduced in Week 1. Represents a location on the globe.
  *
  * @param lat Degrees of latitude, -90 ≤ lat ≤ 90
  * @param lon Degrees of longitude, -180 ≤ lon ≤ 180
  */
case class Location(lat: Double, lon: Double) {
  // we need to have radius of Earth, cause we must calculate distance based on radius
  val EarthRadius: Double = 6371.0088 // (1d / 3d) * (2d * 6378.137 + 6356.752)
  val P: Double = 2.0
  val Radian: Double = Pi / 180

  // https://en.wikipedia.org/wiki/Inverse_distance_weighting
  def calcIdw(that: Location): Double = 1 / pow(calcDist(that), P)

  def calcDist(that: Location): Double = {
    val (rad1, rad2) = (this.toRadian, that.toRadian)
    val lonDeltaLambda = abs(rad1.lon - rad2.lon)

    val deltaDistance = if (rad1.lat == rad2.lat && rad1.lon == rad2.lon) 0
    else if (rad1.lat == -rad2.lat && rad1.lon == -rad2.lon) Pi
    else acos(sin(rad1.lat) * sin(rad2.lat) + cos(rad1.lat) * cos(rad2.lat) * cos(lonDeltaLambda))

    EarthRadius * deltaDistance
  }

  def toRadian: Location = Location(toRadian(lat), toRadian(lon))

  def toRadian(value: Double): Double = value * Radian
}

case class StationModel(stn: Option[Int], wban: Option[Int],
                        lat: Option[Double], lon: Option[Double])

case class TemperatureModel(stn: Option[Int], wban: Option[Int],
                            month: Option[Int], day: Option[Int],
                            temperature: Option[Temperature])

/**
  * Introduced in Week 3. Represents a tiled web map tile.
  * See https://en.wikipedia.org/wiki/Tiled_web_map
  * Based on http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  *
  * @param x    X coordinate of the tile
  * @param y    Y coordinate of the tile
  * @param zoom Zoom level, 0 ≤ zoom ≤ 19
  */
case class Tile(x: Int, y: Int, zoom: Int) {

  def subTiles(zoomDiff: Int): Seq[Tile] = {
    val size = 1 << zoomDiff
    for {
      i <- 0 until size
      j <- 0 until size
      subY = size * y + i
      subX = size * x + j
    } yield Tile(subX, subY, zoom + zoomDiff)
  }

  def toLocation: Location = {
    Location(this.getLatitude, this.getLongitude)
  }

  def getLatitude: Double = {
    atan(sinh(PI * (1.0 - 2.0 * this.y / (1 << this.zoom)))).toDegrees
  }

  def getLongitude: Double = {
    x / (1 << zoom) * 360.0 - 180.0
  }

  def toURI = new URI(s"https://tile.openstreetmap.org/$zoom/$x/$y.png")
}

case object Tile {
  def zero: Tile = Tile(0, 0, 0)
}


/**
  * Introduced in Week 4. Represents a point on a grid composed of
  * circles of latitudes and lines of longitude.
  *
  * @param lat Circle of latitude in degrees, -89 ≤ lat ≤ 90
  * @param lon Line of longitude in degrees, -180 ≤ lon ≤ 179
  */
case class GridLocation(lat: Int, lon: Int) {
  def getLocation: Location = Location(lat, lon)
}

/**
  * Introduced in Week 5. Represents a point inside of a grid cell.
  *
  * @param x X coordinate inside the cell, 0 ≤ x ≤ 1
  * @param y Y coordinate inside the cell, 0 ≤ y ≤ 1
  */
case class CellPoint(x: Double, y: Double)

/**
  * Introduced in Week 2. Represents an RGB color.
  *
  * @param red   Level of red, 0 ≤ red ≤ 255
  * @param green Level of green, 0 ≤ green ≤ 255
  * @param blue  Level of blue, 0 ≤ blue ≤ 255
  */
case class Color(red: Int, green: Int, blue: Int) {
  override def toString: String = s"$red,$green,$blue"
}

