package org.pbit.scala.blackjack

import actors.Actor._
import java.util.Random
import java.io._

/*
 * Copyright 2010 Pavlo Baron
 *
 * This program has been created for SBB DeveloperDay 2010 to show
 * (some aspects of) the Scala language in 30 minutes.
 *
 * Feel free to grab/use the code as long as you leave its author alone :D   
 */

/*
The game engine. This class sends messages to the dealer and listens to those of the players.
It also manages the stack of cards (the only real state in the game).
It controls the game flow so the steps follow each other in the right order.
 */

class Game {
  /*
  Initialize persons. Feel free to modify the players list to have more or less players
   */
  val players = List(new Player("Peter"), new Player("Katja"), new Player("Fritz"))
  val dealer = new Dealer

  /*
   * Here, the game takes place
   */
  def run {
    /*
    Start scala actors
     */
    activateActors

    nextStep

    /*
    Tell dealer to start
     */
    dealer ! ("start", self, players)

    /*
    Await players' acknowledgement messages
     */
    for (i <- 1 to players.length) receivePlayingMessage

    nextStep

    /*
    shuffle cards
     */
    var cards = shuffle

    nextStep

    /*
    Tell dealer to play the first round. In the first round, dealer also takes a card
     */
    dealer ! ("1st round", self, players, cards)
    /*
    Await players' acknowledgement messages
     */
    for (i <- 1 to players.length) receivePlayingMessage

    /*
    Await modified cards stack
     */
    cards = receiveCardsMessage(cards)

    /*
    Play normal rounds where only players play. The normal rounds stop when all players deny the card
     */
    var b = false
    while (!b) {
      nextStep
      /*
      Tell dealer to play a normal round
       */
      dealer ! ("play", self, players, cards)
      b = true
      for (i <- 1 to players.length) {
        /*
        Await players' acknowledgement messages
        */
        if (receivePlayingMessage != Player.STATE_GAMEOVER) b = false
      }

      /*
      Await modified cards stack
      */
      cards = receiveCardsMessage(cards)
    }

    nextStep

    /*
    Tell dealer to play the last round where only dealer plays
     */
    dealer ! ("last round", self, cards)

    nextStep

    /*
    Find out who has won
     */
    tellWhoWins(players, dealer)
  }

  /*
  Find out who has won
   */
  def tellWhoWins(players: List[Player], dealer: Dealer) {
    if (dealer.points > 21) players foreach (player => player.msg(Game.won))
    else {
      var b = false
      var max = dealer.points
      players foreach (player => if ((player.points <= 21) && (player.points > max)) {max = player.points; b = true})
      players foreach (player => if (player.points == max) player.msg(Game.won))
      if (!b) dealer.msg(Game.won)
    }
  }

  /*
  Await the modified cards message. Dealer sends it back after he gave the cards to the players
  (has sent it with the messages to the players)
   */
  def receiveCardsMessage(cards: List[Int]) : List[Int] = {
    var cardsNew = cards
    receive {
      case ("cards", cs: List[Int]) =>
        cardsNew = cs
    }

    cardsNew
  }

  /*
  Await a player's message. It doesn't play a role which one - just the new state will be returned
   */
  def receivePlayingMessage : Int = {
    var ret : Int = 0
    receive {
      case (_, _, state: Int) =>
        ret = state
    }

    ret
  }

  /*
  Start Scala actors for persons
   */
  def activateActors() {
    dealer.start
    players foreach(player => player.start)
  }

  /*
  Shuffle the cards randomly
   */
  def shuffle : List[Int] = {
    val allCards = Game.packs * Game.cards
    val ret = new Array[Int](allCards)
    val random = new Random
    for (i <- 0 until allCards) ret(i) = 0
    for (i <- 1 to Game.packs) {
      for (j <- 1 to Game.cards) {
        var pos = random.nextInt(allCards)
        while (ret(pos) != 0) pos = random.nextInt(allCards)
        if (ret(pos) == 0) ret(pos) = j
      }
    }

    val l: List[Int] = ret.toList

    println("------------ Cards: ")
    println(l)
    println("---------------------")

    l
  }

  /*
  Prepare the next step in the game flow with user interaction
   */
  def nextStep {
    Thread.sleep(2000)
    println("------------- hit enter ---------------")
    val br : BufferedReader = new BufferedReader(new InputStreamReader(System.in))
    br readLine
  }
}

/*
Statics container
 */
object Game {
  def packs = 6
  def cards = 52
  def won = "I have won!"

  /*
  Return the "weight" of a card based on the current situation
   */
  def getCardWeight(card: Int, acePoints : Int) : Int = {
    var weight : Int = card % cards
    weight = weight % 13
    if (weight <= 8) weight = weight + 2
    else if (weight <= 11) weight = 10
    else weight = acePoints

    weight
  }
}

/*
Program runner. Use it to execute the game (Main "class")
 */
object Main {
  def main(args: Array[String]) {
    val game = new Game
    game run; System exit 0
  }
}