package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.math.{floor, ceil}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00   Top-left value
    * @param d01   Bottom-left value
    * @param d10   Top-right value
    * @param d11   Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(point: CellPoint,
                            d00: Temperature,
                            d01: Temperature,
                            d10: Temperature,
                            d11: Temperature): Temperature =
    point.bilinearInterpolation(d00, d01, d10, d11)

  /**
    * @param grid   Grid to visualize
    * @param colors Color scale to use
    * @param tile   Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(grid: GridLocation => Temperature, colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val colorPalette = ColorPaletteSorted(colors)
    val pixels: Array[Pixel] = new Array[Pixel](256 * 256)
    val alpha = 127

    TileSet(tile, 8).getSubtiles.par foreach {
      case (subTile: Tile, pixel: TileSet#Pixel) =>
        val location = subTile.location

        val (lat0, lat1) = (floor(location.lat).toInt, ceil(location.lat).toInt)
        val (lon0, lon1) = (floor(location.lon).toInt, ceil(location.lon).toInt)

        val cellPoint = CellPoint(location.lon - lon0, lat1 - location.lat)
        val (d00, d01, d10, d11) = (
          grid(GridLocation(lat1, lon0)),
          grid(GridLocation(lat0, lon0)),
          grid(GridLocation(lat1, lon1)),
          grid(GridLocation(lat0, lon1))
        )

        val temperature = cellPoint.bilinearInterpolation(d00, d01, d10, d11)
        val color = colorPalette.colorAt(temperature)

        pixels(pixel.arrayIndex) = Pixel(color.argb(alpha))
    }

    Image(256, 256, pixels)
  }
}