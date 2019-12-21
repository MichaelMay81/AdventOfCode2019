package AoC_2019

object Day8 {
  case class Row(digits: List[Int])
  case class Layer(rows: List[Row]) { def digits: List[Int] = rows.flatMap(_.digits) }
  case class Image(layers: List[Layer], width: Int, height: Int)
  { def digits: List[Int] = layers.flatMap(_.digits) }

  def intsToImage(ints: List[Int], width: Int, height: Int): Image = {
    @annotation.tailrec
    def splitIntoRows(ints: List[Int], rows: List[Row] = List()): List[Row] = {
      if (ints.isEmpty)
        rows
      else
        splitIntoRows(ints.drop(width), Row(ints.take(width)) :: rows)
    }
    @annotation.tailrec
    def splitIntoLayers(rows: List[Row], layers: List[Layer] = List()): List[Layer] = {
      if (rows.isEmpty)
        layers
      else
        splitIntoLayers(rows.drop(height), Layer(rows.take(height)) :: layers)
    }

    Image(
      splitIntoLayers(splitIntoRows(ints)),
      width,
      height)
  }

  def stringToImage(str: String, width: Int, height: Int): Image = {
    val digits = str.map((c: Char) => c.asDigit).toList
    intsToImage(digits, width, height)
  }

  def calcCheckSum(image: Image): Int = {
    val layerWithMin0s = image.layers
      .map(l => (l, l.digits.count(_ == 0)))
      .minBy(_._2)._1

    val numberOf1s = layerWithMin0s.digits.count(_ == 1)
    val numberOf2s = layerWithMin0s.digits.count(_ == 2)

    numberOf1s * numberOf2s
  }

  def renderImage(image: Image, printImg: Boolean = true): Image = {
    def pixelCombine(pixel1: Int, pixel2: Int) : Int = if (pixel1 != 2) pixel1 else pixel2
    def layerCombine(layer1: List[Int], layer2: List[Int]): List[Int] =
      (layer1 zip layer2).map{ case (p1, p2) => pixelCombine(p1, p2)}

    val intsCombined = image.layers
      .map(l => l.digits)
      .reduce(layerCombine)

    val imgCombined = intsToImage(intsCombined, image.width, image.height)

    if (printImg)
      for (row <- imgCombined.layers.head.rows) {
        for (digit <- row.digits)
          if (digit == 0)
            print(" ")
          else
            print(digit)
        println()
      }

    imgCombined
  }
}
