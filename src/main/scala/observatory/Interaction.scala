package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  val ALPHA = 127
  val IMAGE_WIDTH = 256
  val IMAGE_HEIGHT = 256
  val P = 8

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    tile.toLocation
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param tile         Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val coordinates = for {h <- 0 until IMAGE_HEIGHT
                           w <- 0 until IMAGE_WIDTH} yield (h, w)
    val arrPixel: Array[Pixel] = coordinates.par.map {
      case (h, w) => Tile((tile.x * (1 << P)) + w, (tile.y * (1 << P)) + h, tile.zoom + P).toLocation
    }.map { location =>
      Visualization.interpolateColor(colors, Visualization.predictTemperature(temperatures, location))
    }.map { color =>
      Pixel(color.red, color.green, color.blue, ALPHA)
    }.toArray

    Image(IMAGE_WIDTH, IMAGE_HEIGHT, arrPixel)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](yearlyData: Iterable[(Year, Data)],
                          generateImage: (Year, Tile, Data) => Unit): Unit = {
    for {
      i <- 0 to 3
      tile <- Tile.zero.subTiles(i)
      (year, data) <- yearlyData
    } generateImage(year, tile, data)
  }

}
