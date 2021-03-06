package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = tile.location

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param tile         Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val colorPalette = ColorPaletteSorted(colors)
    val pixels: Array[Pixel] = new Array[Pixel](256 * 256)
    val alpha = 127

    TileSet(tile, 8).getSubtiles.par foreach {
      case (subTile: Tile, pixel: TileSet#Pixel) =>
        val temperature = Visualization.predictTemperature(temperatures, subTile.location)
        val color = colorPalette.colorAt(temperature)

        pixels(pixel.arrayIndex) = Pixel(color.argb(alpha))
    }

    Image(256, 256, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](yearlyData: Iterable[(Year, Data)], generateImage: (Year, Tile, Data) => Unit): Unit = {
    for {
      (year, data) <- yearlyData
      zoom <- 0 to 3
      y <- 0 until 1 << zoom
      x <- 0 until 1 << zoom
    } generateImage(year, Tile(x, y, zoom), data)
  }

}
