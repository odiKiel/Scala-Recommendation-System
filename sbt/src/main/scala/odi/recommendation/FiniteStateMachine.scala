package odi.recommendation
import scala.collection.mutable.HashMap


class FiniteStateMachine {
  //transition is a triple (src, key, dst) it belongs to its src => state has an id and a list? of transitions
  //case class State(id: String, transitions: Seq[Transition])
  type State = (Int, Int)
  //var transition = Map(State(0,0) -> Map())
  val transition = new HashMap[State, HashMap[String, State]]() 
  val finalStates = collection.mutable.Set[(Int, Int)]()

  val start_state: [(Int, Int)] = Null
  def apply(init_state: [(Int, Int)]) = {
    start_state = init_state
  }
  
  def addTransition(src: State, input: String, dst: State) = {
    if(transition.contains(src)){
      transition(src) += (input -> dst)
    }
    else {
      transition += (src -> HashMap(input -> dst))
    }
  }

  def getInputs(states: Seq[State]): Set[String] = {
    states.flatMap(state => transition.get(state).getOrElse(HashMap()).keys).toSet

  }

  def addFinalState(state: State) = {
    finalStates += state
  }

}
