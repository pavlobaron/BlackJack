package org.pbit.scala.blackjack

import actors.Actor

/*
 * Copyright 2010 Pavlo Baron
 *
 * This program has been created for SBB DeveloperDay 2010 to show
 * (some aspects of) the Scala language in 30 minutes.
 *
 * Feel free to grab/use the code as long as you leave its author alone :D
 */

/*
Abstract class to describe all kinds of persons at the game table. They all will be Scala actors
 */
abstract class Person extends Actor {

  /*
  Override this one to prefix the messages with the correct person information
   */
  def getPrefix : String

  var points : Int = 0
  var status : String = ""

  /*
  Right hand operator to give the person a card. Usage: card :: person
   */
  def ::(card: Int) {
    var acePoints = 11
    if (points + 11 > 21) acePoints = 1
    val weight = Game getCardWeight(card, acePoints)
    points = points + weight
  }

  /*
  Just print a message as this person
   */
  def msg(phrase: String) {
    println(getPrefix + phrase)
  }
}

/*
Statics container
 */
object Person {
  def have = "I have "
}