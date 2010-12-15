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
Black Jack dealer. He is a Scala actor, a person here
 */

class Dealer extends Person {
  /*
  Act as a Scala actor
   */
  def act {
    /*
    We don't have thousands of threads, so no loop. From here, messages are self explaining
     */
    while (true) {
      receive {
        case ("start", caller: Actor, players: List[Player]) =>
          status = Dealer.start
          msg(status)
          players foreach(player => player ! ("start", caller))

        case ("1st round", caller: Actor, players: List[Player], cards: List[Int]) =>
          deal
          caller ! ("cards", iPlay(playersPlay(caller, players, cards)))

        case ("play", caller: Actor, players: List[Player], cards: List[Int]) =>
          deal
          caller ! ("cards", playersPlay(caller, players, cards))

        case ("last round", caller: Actor, cards: List[Int]) =>
          deal
          var cs = cards
          while (points <= 16) { cs = iPlay(cs) }
          caller ! ("cards", cs)
      }
    }
  }

  /*
  I play - only dealer plays. Has to return the modified cards stack
   */
  def iPlay(cards: List[Int]) : List[Int] = {
    cards.head :: this
    msg(Person.have + points)
    cards.tail
  }

  /*
  Players play. Has to return the modified cards stack
   */
  def playersPlay(caller: Actor, players: List[Player], cards: List[Int]) : List[Int] = {
    var cs = cards
    players foreach(player => {player ! ("card", caller, cs.head); if (player.state != Player.STATE_GAMEOVER) cs = cs.tail})
    cs
  }

  override def getPrefix : String = {
    Dealer.dealer
  }

  def deal {
    status = Dealer.dealing
    msg(status)
  }
}

/*
Statics container
 */
object Dealer {
  def start = "Let's start. Players, are you ready?"
  def dealer = "Dealer: "
  def dealing = "Dealing..."
}