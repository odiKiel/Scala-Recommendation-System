package odi.recommendation
import scala.collection.mutable.HashMap

class DeterministicFiniteStateMachine {
  type State = (Int, Int)

  val transition = new HashMap[State, HashMap[Char, collection.mutable.Set[State]]]() 
  val finalStates = collection.mutable.Set[(Int, Int)]()
  val defaultTransition = new HashMap[State, collection.mutable.Set[State]]()
  val firstState = (0,0)

  def addTransition(src: State, input: Char, dst: collection.mutable.Set[State]) = {
    if(transition.contains(src)){
      if(transition(src).contains(input)){
        transition(src)(input) ++= dst
      }
      else {
        transition(src) += (input -> dst)
      }
    }
    else {
      transition += (src -> HashMap(input -> dst))
    }
  }



  def addDefaultTransition(src: State, dst: collection.mutable.Set[State]) = {
    defaultTransition += (src -> dst)
  }

  def addFinalState(state: State) = {
    finalStates += state
  }

  def isFinal(state: State): Boolean = {
    finalStates(state)
  }

  def nextState(src: State, input: Char): collection.mutable.Set[State] = {
    val ret = collection.mutable.Set[State]()
    if(transition.contains(src)) {
      ret ++= transition(src).get(input).getOrElse(collection.mutable.Set[State]())
    }
    ret ++ defaultTransition.get(src).getOrElse(collection.mutable.Set[State]())
  }

  def nextStateSet(srcs: collection.mutable.Set[State], input: Char): collection.mutable.Set[State] = {
    srcs.flatMap((src: State) => nextState(src, input))
  }

  def isInDistance(term: String): Boolean = {
    var currentStates = collection.mutable.Set[State](firstState)
    var c: Char = 'F'
    var i: Int = 0

    while(currentStates.size > 0 && i < term.length) {
      c = term(i).toLower
      currentStates = nextStateSet(currentStates, c)
      i += 1
    }
    currentStates.exists((state: State) => isFinal(state))
  }

}

