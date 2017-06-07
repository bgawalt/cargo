package com.gawalt.cargo

import scala.collection.mutable

/**
 * This source file created by Brian Gawalt, 3/29/17.
 * It is subject to the MIT license bundled with this package in the file LICENSE.txt.
 * Copyright (c) Brian Gawalt, 2017
 */


case class DesertItem(isCar: Boolean, distance: Double, amount: Double) {
  /** Represent this item as a vector of 3 values */
  def featurize: Array[Double] = Array(if (isCar) 1.0 else 0.0, distance, amount)
}

object DesertItem {
  /** Desert items currently have three numerical features: car binary, distance, amount */
  val numFeatures = 3
}

/**
 * Represent the desert, including:
 * - Where the car is
 * - How much fuel is in it
 * - Where the gas cans currently sit
 * Provides functions for updating the state of the desert (e.g., moving the car, or dropping fuel.)
 */
class Desert() {

  val tankMax = 100
  var carPosition: Double = 0
  var carFuel: Double = tankMax
  def tankRoom: Double = tankMax - carFuel
  var fuelCans = mutable.ArrayBuffer[GasCan]()

  /**
   * Convenience function for maxing out a car's gas tank.  Especially useful if the car's returned
   * to the gas station at the start of the desert.
   */
  def fillErUp() { carFuel = tankMax }

  /**
   * Take a drop-can and add it to the list of fuel cans currently associated with the desert.
   * @param drop: Decision on how much fuel to drop
   */
  def dropFuelInDesert(drop: DropFuel) {
    var pos = 0
    val can = drop.toGasCan(distance = carPosition, totalFuel = carFuel)
    // TODO: Replace this linear scan with binary search
    while (fuelCans(pos).distance < can.distance) pos += 1
    fuelCans.insert(pos, can)
  }

  /**
   * If you come across a gas can in the desert while moving the car, add as much of the fuel to
   * the car as you can until the car's gas tank is full.
   * @param can The can you've come across, holding some amount of gas at some position.
   * @return The gas can's new state, after the car's been filled.  (Same position, new amount.)
   */
  def addFuelToCar(can: GasCan): GasCan = {
    if (tankRoom >= can.amount) {
      carFuel += can.amount
      can.copy(amount = 0)
    } else {
      val amountLeft = can.amount - tankRoom
      fillErUp()
      can.copy(amount = amountLeft)
    }
  }

  /**
   * Move the car in the given direction, until it's moved the distance prescribed by `move`, or
   * until it reaches a gas can.  If it reaches a gas can, the car will fill itself up with as
   * much fuel as it can hold or as much fuel as the can has to offer, and then it will stay at
   * that same position.
   * @param move Which direction to move, and by how much.
   */
  def moveCar(move: Move) {
    if (move.forward) {
      val newPosition = carPosition + move.amount(carFuel)
      var i = 0
      while (i < fuelCans.length) {
        val can = fuelCans(i)
        if (can.distance >= carPosition && can.distance <= newPosition) {
          val distTraveled = can.distance - carPosition
          carFuel -= distTraveled
          carPosition = can.distance
          fuelCans(i) = addFuelToCar(can)
        }
        i += 1
      }
      carPosition = newPosition
    } else {
      val newPosition = carPosition - move.amount(carFuel)
      var i = fuelCans.length - 1
      while (i >= 0) {
        val can = fuelCans(i)
        if (can.distance <= carPosition && can.distance >= newPosition) {
          val distTraveled = carPosition - can.distance
          carFuel -= distTraveled
          carPosition = can.distance
          fuelCans(i) = addFuelToCar(can)
        }
        i -= 1
      }
      carPosition = newPosition
    }
  }

  /** Represent the car as a DesertItem */
  def carToDesertItem: DesertItem =
    DesertItem(isCar = true, distance = carPosition, amount = carFuel)

  /**
   * Provide the ordered sequence of items in the desert: fuel cans, and the car.
   */
  def itemSequence(): Array[DesertItem] = {
    val out = new Array[DesertItem](fuelCans.length + 1)
    if (fuelCans.isEmpty) {
      out(0) = carToDesertItem
    } else {
      var out_pos = 0
      var can_pos = 0
      var carHasBeenAdded = false
      while (out_pos < out.length) {
        val can = fuelCans(can_pos)
        if (can.distance < carPosition || carHasBeenAdded) {
          out(out_pos) = can.toDesertItem
          can_pos += 1
        } else {
          out(out_pos) = carToDesertItem
          carHasBeenAdded = true
        }
        out_pos += 1
      }
    }
    out
  }

}