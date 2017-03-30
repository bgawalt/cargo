package com.gawalt.cargo

import scala.collection.mutable

/**
 * This source file created by Brian Gawalt, 3/29/17.
 * It is subject to the MIT license bundled with this package in the file LICENSE.txt.
 * Copyright (c) Brian Gawalt, 2017
 */
case class DesertItem(isCar: Boolean, distance: Double, amount: Double) {
  def toArray: Array[Double] = Array(if (isCar) 1.0 else 0.0, distance, amount)
}


class Desert2() {

  val tankMax = 100
  var carPosition: Double = 0
  var carFuel: Double = tankMax
  def tankRoom: Double = tankMax - carFuel
  var fuelCans = mutable.ArrayBuffer[GasCan]()

  def fillErUp() { carFuel = tankMax }

  def addFuel(can: GasCan) {
    var pos = 0
    while (fuelCans(pos).distance < can.distance) pos += 1
    fuelCans.insert(pos, can)
  }

  def pourFuel(can: GasCan): GasCan = {
    if (tankRoom >= can.amount) {
      carFuel += can.amount
      can.copy(amount = 0)
    } else {
      val amountLeft = can.amount - tankRoom
      fillErUp()
      can.copy(amount = amountLeft)
    }
  }

  def moveCar(move: Move) {
    if (move.forward) {
      val newPosition = carPosition + move.amount
      var i = 0
      while (i < fuelCans.length) {
        val can = fuelCans(i)
        if (can.distance >= carPosition && can.distance <= newPosition) {
          val distTraveled = can.distance - carPosition
          carFuel -= distTraveled
          carPosition = can.distance
          fuelCans(i) = pourFuel(can)
        }
        i += 1
      }
      carPosition = newPosition
    } else {
      val newPosition = carPosition - move.amount
      var i = fuelCans.length - 1
      while (i >= 0) {
        val can = fuelCans(i)
        if (can.distance <= carPosition && can.distance >= newPosition) {
          val distTraveled = carPosition - can.distance
          carFuel -= distTraveled
          carPosition = can.distance
          fuelCans(i) = pourFuel(can)
        }
        i -= 1
      }
      carPosition = newPosition
    }
  }

}
case class Desert(items: Seq[DesertItem]) {

  def sort: Desert = Desert(items.sortBy(_.distance))

  def moveCar(move: Move) : Desert = {
    val newItems = for (item <- items) yield {
      if (item.isCar) {
        if (move.forward) item.copy(distance = item.distance + move.amount)
        else item.copy(distance = item.distance - move.amount)
      } else {
        item
      }
    }
    Desert(newItems).sort
  }

  def storeFuel(can: GasCan) {
    Desert(
      Seq(DesertItem(isCar = false, distance = can.distance, amount = can.amount)) ++ items
    ).sort
  }
}
