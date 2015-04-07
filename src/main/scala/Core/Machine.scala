package Core

import java.io.File
import java.util.UUID


/**
 * 
 * @param descr a description of what this state represents.
 */
case class State(descr:String){
  val id:UUID = UUID.randomUUID
}


/**
 * A class that represents a series of states and transitions from one state to another that is designed to test
 * input strings to see if they match the regex that this class is supposed to represent (and no other regex).
 * 
 * @param initialState the initial "start" state of this machine. 
 * @param finalState the final "matching" state of this matchine
 * @param nTransitions describes transitions from one state to another state that aren't contingent on any particular 
 *                     character
 * @param dTransitions describes transitions from one state to another state that are contingent on a particular
 *                     character
 */
case class Machine(initialState:State, finalState:State, nTransitions:Map[State, Set[State]],
                   dTransitions: Map[State, Map[Char, Set[State]]]) {
  val states:Set[State] = nTransitions.keySet union nTransitions.values.toSet.flatten union dTransitions.keySet union
    dTransitions.values.foldLeft(Set[Set[State]]()){case (set, map) => set union map.values.toSet}.flatten
}

object Machine {

  /**
   * Generates a DOT Format representation of Machine "machine"
   * @param machine the machine that we are making the representation of
   * @return a string that is a properly formatted DOT representation of this machine, with nodes being states and
   *         nTransition
   *         key-value pairs as unlabeled edges and dTransition key-value paris being labeled with the character that
   *         the transition is contingent on
   */
  def toDOTFileFormat(machine:Machine):String = {
    val builder = new StringBuilder

    def genMap:Map[UUID, Int] = {
      var num = 0
      machine.states.foldLeft(Map[UUID, Int]()){case (map, state) => {
        num = num + 1
        map + (state.id -> num)}}
    }
    val idIntMap = genMap

    builder.append(s"digraph MACHINE {\n")
    machine.nTransitions.foreach { case (state, set) =>
      set.foreach { case state2 =>
        builder.append(s"""${if (state == machine.initialState) """START""" else s"""${idIntMap(state.id)}"""} -> ${if (state2 == machine.finalState) """FINAL""" else idIntMap(state2.id)};"""+"\n") }
    }

    machine.dTransitions.foreach { case (state, map) => map.foreach { case (char, set) =>
      set.foreach { case state2 =>
        builder.append(s"""${if (state == machine.initialState) "START" else s"""${idIntMap(state.id)}"""} -> ${if (state2 == machine.finalState) """FINAL"""else idIntMap(state2.id)} [ label = "$char"];""" + "\n")}}}

    machine.states.foreach{case state =>
      if (state != machine.initialState && state !=machine.finalState) builder.append(s"""${idIntMap(state.id)} [label = "", shape = "circle"];""" + "\n")}

    builder.append(s"""START [shape= "circle"];"""+"\n")
    builder.append(s"""FINAL [shape= "doublecircle"]; """+"\n")
    builder.append( "}\n")

    builder.toString
  }

  /**
   * Generates a machine that matches a single character 'char'
   * @param singleCharString the character that we want to match
   * @return a machine that matches 'char' and nothing else
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

    val newNTrans = Seq(firstMachine.finalState -> Set(newFinal), newInitial -> Set(secondMachine.initialState, firstMachine.initialState),
      secondMachine.finalState -> Set(newFinal))

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
   val tempMachine = oneOrMore(target)
    val prevTranstions:Set[State] = tempMachine.nTransitions(tempMachine.initialState)
    Machine(tempMachine.initialState, tempMachine.finalState,
      tempMachine.nTransitions + (tempMachine.initialState -> (prevTranstions + tempMachine.finalState)) ,
      tempMachine.dTransitions)
  }
}