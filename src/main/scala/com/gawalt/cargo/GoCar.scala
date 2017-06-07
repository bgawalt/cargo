package com.gawalt.cargo

/**
 * This source file created by Brian Gawalt, 3/29/17.
 * It is subject to the MIT license bundled with this package in the file LICENSE.txt.
 * Copyright (c) Brian Gawalt, 2017
 */

/**
 * Representation of a driver's decision making process.  The driver is essentially a recurrent
 * neural network that scans through the DesertItem vectors and keeps track of state in
 * a series of memory neurons.
 * @param moveOrDropFuelWeights Weights to decide: Should we move the car, or drop fuel?
 * @param fuelAmountWeights Weights to decide: What share (0.0 to 1.0) of our fuel should we spend?
 * @param forwardOrBackWeights Weights to decide: If we're moving, forward or backwards?
 * @param memoryWeights Rules for updating each of the K memory neurons as a function of the current
 *                      DesertItem as well as the current memory state.
 */
case class Driver(moveOrDropFuelWeights: Array[Double],
                  fuelAmountWeights: Array[Double],
                  forwardOrBackWeights: Array[Double],
                  memoryWeights: Array[Array[Double]]) {
  val numMemoryNeurons = memoryWeights.length

  def makeDecision(desertItems: Array[DesertItem]): Decision = {
    // The driver starts with an empty mental state.
    var mentalState = Array.fill[Double](numMemoryNeurons)(0.0)
    val numItems = desertItems.length
    var itemPos = 0
    // Update the mental state for the first (N-1) items
    while (itemPos < numItems - 1) {
      mentalState = memoryWeights.map(w => Driver.dot(weights = w,
        itemFeatures = desertItems(itemPos).featurize, mentalState = mentalState))
      itemPos += 1
    }
    // Now, use the final item (and final mental state) to reach a decision.
    val finalItemFeatures = desertItems(numItems - 1).featurize
    val moveOrDrop = Driver.dot(weights = moveOrDropFuelWeights, itemFeatures = finalItemFeatures,
      mentalState = mentalState)
    val share = Driver.logitDot(weights = fuelAmountWeights, itemFeatures = finalItemFeatures,
      mentalState = mentalState)
    if (moveOrDrop > 0) {
      val forward_dec = Driver.dot(weights = forwardOrBackWeights, itemFeatures = finalItemFeatures,
        mentalState = mentalState)
      Move(forward = forward_dec > 0, share = share)
    } else {
      DropFuel(share)
    }
  }

}

object Driver {
  /**
   * Find the dot product between a particular set of weights (be it for determining a mental state
   * update or arriving at a final decision) and the current state of the driver and desert.
   * @param weights Decision rule for this dot product
   * @param itemFeatures Features associated with the desert item currently being examined
   * @param mentalState The driver's mental state at the moment the item is being examined
   * @return The dot product between features and weights
   */
  def dot(weights: Array[Double], itemFeatures: Array[Double],
          mentalState: Array[Double]): Double = {
    require(weights.length == itemFeatures.length + mentalState.length + 1)
    var sum: Double = weights(0)
    var ii = 0
    while (ii < itemFeatures.length) {
      sum += itemFeatures(ii) * weights(ii + 1)
      ii += 1
    }
    var jj = 0
    while (jj < mentalState.length) {
      sum += mentalState(jj) * weights(jj + itemFeatures.length + 1)
      jj += 1
    }
    sum
  }

  /**
   * Same as .dot(), but runs the dot product through a logit sigmoid.
   * @param weights Decision rule for this dot product
   * @param itemFeatures Features associated with the desert item currently being examined
   * @param mentalState The driver's mental state at the moment the item is being examined
   * @return The dot product between features and weights
   */
  def logitDot(weights: Array[Double], itemFeatures: Array[Double],
               mentalState: Array[Double]): Double = {
    1.0/(1.0 + math.exp(-1*dot(weights, itemFeatures, mentalState)))
  }
}

trait Decision {
  val share: Double
}
case class Move(forward: Boolean, share: Double) extends Decision {
  def amount(carFuel: Double): Double = share*carFuel
}
case class DropFuel(share: Double) extends Decision {
  def amount(carFuel: Double): Double = share*carFuel
  def toGasCan(distance: Double, totalFuel: Double): GasCan =
    GasCan(distance = distance, amount = totalFuel*share)
}

case class GasCan(distance: Double, amount: Double) {
  def toDesertItem: DesertItem = DesertItem(isCar = false, distance = distance, amount = amount)
}