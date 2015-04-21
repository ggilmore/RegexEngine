package com.ggilmore.core

import java.util.UUID


/**
 *
 * @param description a description of what this state represents.
 * @param id a unique identifier for this State instance
 */
case class State(description:String, id:UUID  = UUID.randomUUID){
  override def toString:String = id.toString
}


/**
 * A class that represents a series of states and transitions from one state to another that is designed to test
 * input strings to see if they match the regex that this class is supposed to represent (and no other regex).
 *
 * @param initialState the initial "start" state of this machine. 
 * @param finalState the final "matching" state of this machine
 * @param nTransitions describes transitions from one state to another state that aren't contingent on any particular 
 *                     character
 * @param dTransitions describes transitions from one state to another state that are contingent on a particular
 *                     character
 */
case class Machine(initialState:State, finalState:State, nTransitions:Map[State, Set[State]],
                   dTransitions: Map[State, Map[Char, Set[State]]]) {
  val states:Set[State] = nTransitions.keySet union nTransitions.values.toSet.flatten union dTransitions.keySet union
    dTransitions.values.foldLeft(Set[Set[State]]()){case (set, map) => set union map.values.toSet}.flatten

  private def genMap:Map[State, Int] = {
    this.states.foldLeft((Map[State, Int](), 0)){case ((map, num), state) => {
      (map + (state-> num), num + 1)}}._1
  }

  val idIntMap = genMap
}

object Machine {

  /**
   * Generates a DOT Format representation of Machine "machine"
   * @param machine the machine that we are making the representation of
   * @return a string that is a properly formatted DOT representation of this machine, with nodes being states and
   *         nTransition key-value pairs as unlabeled edges and dTransition key-value pairs being edges labeled with the character that
   *         the transition is contingent on
   */
  def toDOTFileFormat(machine:Machine, validStates:Option[Set[State]] = None):String = {
    val builder = new StringBuilder


    builder.append(s"digraph MACHINE {\n")

    machine.nTransitions.foreach { case (state, set) =>
      set.foreach { case state2 =>
        builder.append(s"""${if (state == machine.initialState) """START""" else s"""${machine.idIntMap(state)}"""} -> ${if (state2 == machine.finalState) """FINAL""" else machine.idIntMap(state2)};"""+"\n") }
    }

    machine.dTransitions.foreach { case (state, map) => map.foreach { case (char, set) =>
      set.foreach { case state2 =>
        builder.append(s"""${if (state == machine.initialState) "START" else s"""${machine.idIntMap(state)}"""} -> ${if (state2 == machine.finalState) """FINAL"""else machine.idIntMap(state2)} [ label = "$char"];""" + "\n")}}}

    machine.states.foreach{case state =>
      if (state != machine.initialState && state !=machine.finalState) builder.append(s"""${machine.idIntMap(state)} [label = "", shape = "circle"];""" + "\n")}

    builder.append(s"""START [shape= "circle"];"""+"\n")
    builder.append(s"""FINAL [shape= "doublecircle"]; """+"\n")
    validStates match {
      case Some(states) => {
        states.foreach(state => builder.append(s"""${if (state == machine.initialState) "START"
        else if (state == machine.finalState) "FINAL"
        else machine.idIntMap(state)} [color = "red"];""" + "\n"))
      }
    }
    builder.append( "}\n")
    builder.toString

  }



  /**
   * Generates a machine that matches a single character 'char'
   * @param singleCharString the character that we want to match
   * @return a machine that matches 'singleCharString' and nothing else
   */
  def singleChar(singleCharString: String): Machine = {
    require(singleCharString.length == 1)
    val char = singleCharString.charAt(0)
    val newInitial = State(s"initial state for singleChar $char.")
    val newFinal = State(s"final state for singleChar $char.")
    val dTransitions = Map(newInitial -> Map(char -> Set(newFinal)))
    Machine(newInitial, newFinal, Map(), dTransitions)
  }

  /**
   *
   * @param firstMachine the first machine that is part of the "OR"
   * @param secondMachine the second machine that is part of the "OR"
   * @return a machine that matches iff the firstMachine matches OR the secondMachine matches,
   *         i.e. firstMachine|secondMachine
   */
  def or(firstMachine: Machine, secondMachine: Machine): Machine = {
    val newInitial: State = State(s"initial state for OR of $firstMachine and $secondMachine.")
    val newFinal: State = State(s"final state for OR of $firstMachine and $secondMachine.")

    val newNTrans = Seq(firstMachine.finalState -> Set(newFinal), newInitial -> Set(secondMachine.initialState,
      firstMachine.initialState), secondMachine.finalState -> Set(newFinal))

    Machine(newInitial, newFinal, firstMachine.nTransitions ++ secondMachine.nTransitions ++ newNTrans,
      firstMachine.dTransitions ++ secondMachine.dTransitions)
  }

  /**
   *
   * @param firstMachine the first machine that is part of the "OR"
   * @param secondMachine the second machine that is part of the "OR"
   * @return a machine that matches iff both the firstMachine matches and then the secondMachine matches,
   *         i.e. firstMachinesecondMachine
   */
  def and(firstMachine: Machine, secondMachine: Machine): Machine = {
    val newInitial: State = State(s"initial state for AND of $firstMachine and $secondMachine.")
    val newFinal: State = State(s"final state for AND of $firstMachine and $secondMachine.")

    val newNTrans = Seq(newInitial -> Set(firstMachine.initialState), firstMachine.finalState -> Set(secondMachine.initialState),
      secondMachine.finalState -> Set(newFinal))

    Machine(newInitial, newFinal, firstMachine.nTransitions ++ secondMachine.nTransitions ++ newNTrans,
      firstMachine.dTransitions ++ secondMachine.dTransitions)
  }

  /**
   *
   * @param target the machine that we want to create the "oneOrMore" out of
   * @return a machine that matches iff the input matches target or , targettarget, or targettargettarget, i.e. target+
   */
  def oneOrMore(target: Machine): Machine = {
    val newInitial: State = State(s"initial state for oneOrMore of $target,")
    val newFinal: State = State(s"final state for oneOrMore of $target.")

    val newNTrans = Seq(newInitial -> Set(target.initialState), target.finalState -> Set(newFinal, target.initialState))

    Machine(newInitial, newFinal, target.nTransitions ++ newNTrans, target.dTransitions)
  }

  /**
   *
   * @param target the machine that we want to create the "zeroOrOne" out of
   * @return a machine that matches iff the input matches a single target, or doesn't match a single target at all, 
   *         i.e. target?
   */
  def zeroOrOne(target: Machine): Machine = {
    val newInitial: State = State(s"initial state for zeroOrOne of $target.")
    val newFinal: State = State(s"final state for zeroOrOne of $target.")

    val newNTrans = Seq(newInitial -> Set(target.initialState, newFinal), target.finalState -> Set(newFinal))

    Machine(newInitial, newFinal, target.nTransitions ++ newNTrans, target.dTransitions)
  }

  /**
   *
   * @param target the machine that we want to create the "oneOrMore" out of
   * @return a machine that matches iff the input doesn't match target at all, or the input matches target or ,
   *         targettarget, or targettargettarget, i.e. target*
   */
  def someOrNone(target: Machine): Machine = {
//   val tempMachine = oneOrMore(target)
//    val prevTransitions:Set[State] = tempMachine.nTransitions(tempMachine.initialState)
//    Machine(tempMachine.initialState, tempMachine.finalState,
//      tempMachine.nTransitions + (tempMachine.initialState -> (prevTransitions + tempMachine.finalState)) ,
//      tempMachine.dTransitions)
    oneOrMore(zeroOrOne(target))
  }
}