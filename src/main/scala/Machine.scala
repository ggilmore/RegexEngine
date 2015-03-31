/**
 * Created by gmgilmore on 3/31/15.
 */


case class State(name:String)


case class Machine(initialState:State, finalState:State, nTransitions:Map[State, Set[State]], dtransitions: Map[State, Map[Char, Set[State]]])

object Machine {
  def singleChar(initialState: State, finalState: State, char: Char): Machine = {
    val dTransitions = Map(initialState -> Map(char, Set(finalState)))
    Machine(initialState, finalState, Map(), dTransitions)
  }

  def or(firstMachine: Machine, secondMachine: Machine): Machine = {
    val newInitial: State = State(s"initial state for OR of $firstMachine and $secondMachine.")
    val newFinal: State = State(s"final state for OR of $firstMachine and $secondMachine.")

    val newNTrans = Seq(firstMachine.finalState -> Set(newFinal), newInitial -> Set(firstMachine.initialState),
      secondMachine.finalState -> Set(newFinal), newInitial -> Set(secondMachine.initialState))

    Machine(newInitial, newFinal, firstMachine.nTransitions ++ secondMachine.nTransitions ++ newNTrans,
      firstMachine.dtransitions ++ secondMachine.dtransitions)
  }

  def and(firstMachine: Machine, secondMachine: Machine): Machine = {
    val newInitial: State = State(s"initial state for AND of $firstMachine and $secondMachine.")
    val newFinal: State = State(s"final state for AND of $firstMachine and $secondMachine.")

    val newNTrans = Seq(newInitial -> Set(firstMachine.initialState), firstMachine.finalState -> Set(secondMachine.initialState),
      secondMachine.finalState -> Set(newFinal))

    Machine(newInitial, newFinal, firstMachine.nTransitions ++ secondMachine.nTransitions ++ newNTrans,
      firstMachine.dtransitions ++ secondMachine.dtransitions)
  }

  def oneOrMore(target: Machine): Machine = {
    val newInitial: State = State(s"initial state for oneOrMore of $target,")
    val newFinal: State = State(s"final state for oneOrMore of $target.")

    val newNTrans = Seq(newInitial -> Set(target.initialState), target.finalState -> Set(newFinal),
      target.finalState -> Set(target.initialState))

    Machine(newInitial, newFinal, target.nTransitions ++ newNTrans, target.dtransitions)
  }

  def zeroOrOne(target: Machine): Machine = {
    val newInitial: State = State(s"initial state for zeroOrOne of $target.")
    val newFinal: State = State(s"final state for zeroOrOne of $target.")

    val newNTrans = Seq(newInitial -> Set(target.initialState), target.finalState -> Set(newFinal),
      newInitial -> Set(newFinal))

    Machine(newInitial, newFinal, target.nTransitions ++ newNTrans, target.dtransitions)
  }

  def someOrNone(target: Machine): Machine = {
   val tempMachine = oneOrMore(target)
    val prevTranstions:Set[State] = tempMachine.nTransitions(tempMachine.initialState)
    Machine(tempMachine.initialState, tempMachine.finalState,
      tempMachine.nTransitions + (tempMachine.initialState -> (prevTranstions + tempMachine.finalState)) ,
      tempMachine.dtransitions)
  }
}