package AoC_2019

object Day1 {
  val fuel_required: Int => Int = (mass: Int) => ((mass / 3).floor - 2).toInt

  @annotation.tailrec
  def total_fuel_required(mass: Int, total_fuel: Int = 0): Int = {
    val fuel = fuel_required(mass)
    if (fuel <= 0) total_fuel
    else total_fuel_required(fuel, total_fuel + fuel)
  }
}
