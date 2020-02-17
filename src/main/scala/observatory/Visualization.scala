package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    averageTemperatures(temperatures.map {
      case (loc: Location, t: Temperature) => (greatCircleDistanceRelative(loc, location), t)
    })
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    ColorPaletteSorted(points).colorAt(value)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    visualize(temperatures, colors, 360, 180)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param imageWidth   Image width
    * @param imageHeight  Image height
    * @param alpha        Image alpha channel (0 to 255)
    * @return A imageWidth × imageHeight image where each pixel shows the predicted temperature at its location
    */
  private def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)],
                imageWidth: Int, imageHeight: Int, alpha: Int = 255): Image = {
    val colorPalette = ColorPaletteSorted(colors)

    @inline def pointToLocation(x: Int, y: Int): Location = {
      Location(90 - y * (180.0 / imageHeight), x * (360.0 / imageWidth) - 180)
    }

    val pixels: Array[Pixel] = new Array[Pixel](imageWidth * imageHeight)

    for {
      y <- (0 until imageHeight).par
      x <- (0 until imageWidth).par
    } {
      val location = pointToLocation(x, y)
      val temperature = predictTemperature(temperatures, location)
      val color = colorPalette.colorAt(temperature)

      pixels(y * imageWidth + x) = Pixel(color.argb(alpha))
    }

    Image(imageWidth, imageHeight, pixels)
  }

  /**
    * Computes the great circle distance between two locations in units of radius:
    *
    * 0 if a == b
    * \pi if a, b are antipodes
    * acos(sin p1 * sin p2 + cos p1 * cos p2 * cos (l1 - l2)) otherwise
    *
    * where a = (p1, l1),  b = (p2, l2) in radians.
    *
    * @param a first location to measure distance from
    * @param b second location to measure distance from
    * @return relative distance
    */
  private def greatCircleDistanceRelative(a: Location, b: Location): Double = {
    @inline def degToRad(x: Double): Double = x * Pi / 180

    if (a == b) 0
    else if (a.lat + b.lat == 0 && abs(a.lon - b.lon) % 360 == 180) Pi
    else {
      val dLambda = degToRad(a.lon - b.lon)
      val (phi1, phi2) = (degToRad(a.lat), degToRad(b.lat))

      math.acos(sin(phi1) * sin(phi2) + cos(phi1) * cos(phi2) * cos(dLambda))
    }
  }

  /**
    * Averages temperatures with potentially infinite weights:
    *
    * - The average of values with non-infinite weights is the usual average
    * - The average of a value with an infinite weight and a value with a finite weight is
    *   the average of the values with infinite weights only
    *
    * Effectively, each data point is treated as having weight A * Double.Infinite + B, where
    * A and B are finite values.
    *
    * @param x  Iterable of weighted temperatures to aggregate
    * @return   Computed temperature average
    */
  private def averageTemperatures(x: Iterable[(Double, Temperature)]): Temperature = {

    type WeightedTemperature = (Temperature, Double, Int)

    val zeroValue: WeightedTemperature = (0.0, 0.0, 0)

    def seqOp(wt: WeightedTemperature, k: (Double, Temperature)): WeightedTemperature = {
      val p = 2
      val distance: Double = k._1
      if (wt._2.isInfinite) {
        if (math.abs(distance) < 1e-6) (wt._1 + k._2, Double.PositiveInfinity, wt._3 + 1) else wt
      } else {
        if (math.abs(distance) < 1e-6) (k._2, Double.PositiveInfinity, 1)
        else {
          val w = 1.0 / math.pow(distance, p)
          (wt._1 + w * k._2, wt._2 + w, 0)
        }
      }
    }

    def combOp(a: WeightedTemperature, b: WeightedTemperature): WeightedTemperature = {
      if (a._2.isInfinite && b._2.isInfinite) (a._1 + b._1, Double.PositiveInfinity, a._3 + b._3)
      else if (a._2.isInfinite) a
      else if (b._2.isInfinite) b
      else (a._1 + b._1, a._2 + b._2, a._3 + b._3)
    }

    def toTemperature(a: WeightedTemperature): Temperature = {
      if (a._2.isInfinite) a._1 / a._3
      else if (a._2 != 0) a._1 / a._2
      else 0
    }

    toTemperature(x.aggregate(zeroValue)(seqOp, combOp))
  }

}
