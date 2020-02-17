package observatory

import scala.collection.Searching._

/**
  * Introduced in Week 1. Represents a location on the globe.
  *
  * @param lat Degrees of latitude, -90 ≤ lat ≤ 90
  * @param lon Degrees of longitude, -180 ≤ lon ≤ 180
  */
case class Location(lat: Double, lon: Double)

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

  /**
    * @return The latitude and longitude of the top-left corner of the tile, as per
    *         http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def location: Location = {
    val n: Double = 1 << zoom
    val lon: Double = x.toDouble / n * 360 - 180
    val lat: Double = math.atan(
      math.sinh(math.Pi * (1 - 2 * y.toDouble / n))
    ) * 180 / math.Pi

    Location(lat, lon)
  }

}

/**
  * Convenience case class for representing tilesets -- divisions of a single tile into
  *   (2**deltaZoom x 2**deltaZoom) subtiles
  *
  * @param parentTile  Tile representing the parent tile
  * @param deltaZoom   Amount of zoom applied to subtiles
  */
case class TileSet(parentTile: Tile, deltaZoom: Int) {

  assert(deltaZoom >= 0)

  /**
    * Convenience case class representing integer pixels within a tileset
    * @param x  Left-to-right pixel coordinate
    * @param y  Top-to-bottom pixel coordinate
    */
  case class Pixel(x: Int, y: Int) {

    /**
      * Representation of the pixel as an array index within the tileset
      * @return
      */
    def arrayIndex: Int = y * (1 << deltaZoom) + x
  }

  /**
    * Produces an iterable of subtiles for this tile, along with its pixel coordinates
    *
    * @return An iterable of 2 ** deltaZoom tiles and their pixels
    */
  def getSubtiles: Iterable[(Tile, Pixel)] = {
    val n = 1 << deltaZoom
    for {
      yPixel <- 0 until n
      xPixel <- 0 until n
    } yield (
      Tile(n * parentTile.x + xPixel, n * parentTile.y + yPixel, parentTile.zoom + deltaZoom),
      Pixel(xPixel, yPixel)
    )
  }

}

/**
  * Introduced in Week 4. Represents a point on a grid composed of
  * circles of latitudes and lines of longitude.
  *
  * @param lat Circle of latitude in degrees, -89 ≤ lat ≤ 90
  * @param lon Line of longitude in degrees, -180 ≤ lon ≤ 179
  */
case class GridLocation(lat: Int, lon: Int)

/**
  * Introduced in Week 5. Represents a point inside of a grid cell.
  *
  * @param x X coordinate inside the cell, 0 ≤ x ≤ 1
  * @param y Y coordinate inside the cell, 0 ≤ y ≤ 1
  */
case class CellPoint(x: Double, y: Double) {

  /**
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  @inline def bilinearInterpolation(d00: Temperature, d01: Temperature, d10: Temperature, d11: Temperature): Temperature = {
    d00 * (1 - x) * (1 - y) + d01 * (1 - x) * y + d10 * x * (1 - y) + d11 * x * y
  }

}

/**
  * Introduced in Week 2. Represents an RGB color.
  *
  * @param red   Level of red, 0 ≤ red ≤ 255
  * @param green Level of green, 0 ≤ green ≤ 255
  * @param blue  Level of blue, 0 ≤ blue ≤ 255
  */
case class Color(red: Int, green: Int, blue: Int) {

  /**
    * Translates this color representation into an ARGB pixel
    * @param alpha  Transparency value for ARGB pixel
    * @return
    */
  @inline def argb(alpha: Int = 255): Int =
    (alpha << 24) + (red << 16) + (green << 8) + blue

}

/**
  * Convenience class for quick color interpolation
  * @param temperatures  Sorted sequence of temperatures
  * @param colors Corresponding sequence of colors
  */
case class ColorPaletteSorted(temperatures: Seq[Temperature], colors: Seq[Color]) {

  /**
    * Computes the interpolated pallete color at a given temperature
    * @param value  Temperature to obtain the interpolated color
    * @return  Interpolated color
    */
  def colorAt(value: Temperature): Color = {
    temperatures search value match {
      case Found(idx) => colors(idx)
      case InsertionPoint(idx) =>
        val nodeBefore = if (idx == 0) None else Some((temperatures(idx - 1), colors(idx - 1)))
        val nodeAfter = if (idx < temperatures.length) Some((temperatures(idx), colors(idx))) else None

        if (nodeBefore.isEmpty && nodeAfter.isEmpty) Color(0, 0, 0)
        else if (nodeBefore.isEmpty) nodeAfter.get._2
        else if (nodeAfter.isEmpty) nodeBefore.get._2
        else {
          val alpha: Double = (value - nodeBefore.get._1) / (nodeAfter.get._1 - nodeBefore.get._1)

          @inline def interpolate(s: Int, t: Int): Int = math.round(s + alpha * (t - s)).toInt

          val colorBefore = nodeBefore.get._2
          val colorAfter = nodeAfter.get._2

          val red = interpolate(colorBefore.red, colorAfter.red)
          val green = interpolate(colorBefore.green, colorAfter.green)
          val blue = interpolate(colorBefore.blue, colorAfter.blue)

          Color(red, green, blue)
        }
    }
  }
}

object ColorPaletteSorted {

  /**
    * Alternate constructor from an unsorted palette of colors
    * @param points  Iterable of (Temperature, Color) pairs
    * @return
    */
  def apply(points: Iterable[(Temperature, Color)]): ColorPaletteSorted = {
    val sortedPoints = points.toSeq.sortBy(_._1)
    val sortedTemperatures = sortedPoints map (_._1)
    val sortedColors = sortedPoints map (_._2)

    ColorPaletteSorted(sortedTemperatures, sortedColors)
  }
}
