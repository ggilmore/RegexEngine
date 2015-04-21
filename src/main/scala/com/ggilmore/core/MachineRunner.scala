package com.ggilmore.core

import scala.annotation.tailrec

/**
 * Contains methods that are using to "run" machines and test input to see if the input matches the regex that is
 * describing a particular machine
 */
object MachineRunner {

  /**
   * @param input the input that we are using to advance the machine's state
   * @param machine the machine that we are using to attempt to match the input string
   * @param validStates the set of states that we are currently evaluating
   * @return a new set of states that we are currently evaluating based on the new input "input" to the
   *         Machine "machine"
   */
  def nextStep(input: Char, machine: Machine, validStates: Set[State]): (Set[State], Seq[ProgessionWithChar]) = {

    /**
     * Generates a new set of valid states using the non deterministic transitions that are dictated by the machine
     * @param beforeStates the set of valid
     * @return
     */
    def handleNTransitions(beforeStates: Set[State], progression:Seq[ProgessionWithChar] = Seq()): (Set[State], Seq[ProgessionWithChar]) = {
      @tailrec
      def loop(beforeStates: Set[State], visitedSet:Set[State], progression: Seq[ProgessionWithChar]): (Set[State], Seq[ProgessionWithChar]) = {
        val (newStates, newVisitedStates) = beforeStates.foldLeft((Set[State](), visitedSet)) { case ((newStatesSet, newVisitedSet), state) => {
          if (newVisitedSet.contains(state)) (newStatesSet + state, newVisitedSet)
          else if (machine.nTransitions.contains(state)) (newStatesSet union machine.nTransitions(state), newVisitedSet + state)
          else (newStatesSet + state, newVisitedSet + state)
        }
        }
        if (newStates == beforeStates) (newStates, progression) else loop(newStates, visitedSet union newVisitedStates,
          progression :+ ProgessionWithChar("",newStates))
      }
      val finalStates = loop(beforeStates, Set(), progression)
      finalStates
    }

    /**
     * Generates a new set of valid states using the transitions on a particular character, as dictated by the machine
     * @param beforeStates
     * @return
     */
    def handleDTransitions(beforeStates: Set[State], progression:Seq[ProgessionWithChar]): (Set[State], Seq[ProgessionWithChar]) = {
      val result = beforeStates.foldLeft(Set[State]()) { case (newStates, state) => {
        if (machine.dTransitions.contains(state) && machine.dTransitions(state).contains(input))
          newStates ++ machine.dTransitions(state)(input)
        else newStates
      }
      }
      (result, progression :+ ProgessionWithChar(input.toString, result))
    }
    val firstRoundNTransitions = handleNTransitions(validStates)
    val allDTransitions = handleDTransitions(firstRoundNTransitions._1, firstRoundNTransitions._2)
    val lastRoundNTransitions = handleNTransitions(allDTransitions._1, allDTransitions._2)
    lastRoundNTransitions
  }

  case class ProgessionWithChar(input: String, progression: Set[State])

  /**
   * @param machine that machine that we are using to attempt to match the input "inputString"
   * @param inputString the input that we are evaluating to see if it matches the regex that is described by the machine
   * @return true if "inputString" matches the regex described by the machine, false otherwise
   */
  def testInput(machine: Machine, inputString: String): (Boolean, Seq[ProgessionWithChar]) = {
    @tailrec
    def loop(testStr: String, validStates: Set[State], progression:Seq[ProgessionWithChar]): (Set[State], Seq[ProgessionWithChar]) = {
      if (testStr.isEmpty) (validStates, progression)
      else {
        val (newValidStates, newProgression):(Set[State], Seq[ProgessionWithChar]) = nextStep(testStr.charAt(0), machine, validStates)
        loop(testStr.drop(1), newValidStates, progression ++ newProgression)
      }
    }
    val (finalStates, progression) = loop(inputString, Set(machine.initialState), Seq(ProgessionWithChar("", Set(machine.initialState))))
    (finalStates.contains(machine.finalState), progression)
  }

}