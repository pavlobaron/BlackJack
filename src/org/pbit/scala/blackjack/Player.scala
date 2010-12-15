package org.pbit.scala.blackjack

import actors.Actor._
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
Player. A Scala actor, a person here
 */

class Player(val name: String) extends Person {
  /*
  We have a little finite state machine (FSM) here
   */
  var state = Player.STATE_INACTIVE

  /*
  Act as scala actor
   */
  def act {
    /*
    We don't have thousands threads, so no loop here. From here, messages are self explaining
     */
    while (true) {
      receive {
        case ("start", caller: Actor) =>
          changeState(caller, Player.started, Player.ready, Player.STATE_READY)
        case ("card", caller: Actor, card: Int) =>
          if (state != Player.STATE_GAMEOVER) card :: this
          if (points < 18) changeState(caller, Player.playing, Person.have + points, Player.STATE_PLAYING)
          else changeState(caller, Player.done, Person.have + points, Player.STATE_GAMEOVER)
      }
    }
  }

  /*
  FSM: change the state and tell the caller that we did so. The player doesn't speak to the dealer,
  but always to the game
   */
  def changeState(caller: Actor, message: String, st: String, ste: Int) {
    status = st
    msg(status)
    state = ste
    caller ! (message, self, ste)
  }

  override def getPrefix : String = {
    name + ": "
  }
}

/*
Statics container
 */
object Player {
  def STATE_INACTIVE: Int = 0;
  def STATE_READY: Int = 1;
  def STATE_PLAYING: Int = 2;
  def STATE_GAMEOVER: Int = 3;

  def ready = "I'm ready!"
  def started = "player started"
  def done = "player done"
  def playing = "player playing"
}