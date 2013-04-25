package odi.recommendation
import scala.collection.mutable.HashMap

class DeterministicFiniteStateMachine(degree: Int) {
  type State = (Int, Int)

  val transition = new HashMap[State, HashMap[Char, collection.mutable.Set[State]]]() 
  val finalStates = collection.mutable.Set[(Int, Int)]()
  val defaultTransition = new HashMap[State, collection.mutable.Set[State]]()
  val firstStates = collection.mutable.Set() ++ (1 to degree).map((i: Int) => (i, i))
  //not correct a deterministic can only have one start state

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

  def nextStateSingle(src: State, input: Char): collection.mutable.Set[State] = {
    val ret = collection.mutable.Set[State]()
    if(transition.contains(src)) {
      ret ++= transition(src).get(input).getOrElse(collection.mutable.Set[State]())
    }
    ret ++ defaultTransition.get(src).getOrElse(collection.mutable.Set[State]())
  }

  def nextState(srcs: collection.mutable.Set[State], input: Char): collection.mutable.Set[State] = {
    //(0,0) needs the states for the first insertion
    if(srcs.contains((0,0))) {
      srcs ++= firstStates
    }
    srcs.flatMap((src: State) => nextStateSingle(src, input))
  }

  def isInDistance(term: String): Boolean = {
    var currentStates = collection.mutable.Set((0,0))
    var c: Char = 'F'
    var i: Int = 0

    while(currentStates.size > 0 && i < term.length) {
      c = term(i).toLower
      currentStates = nextState(currentStates, c)
      i += 1
    }
    currentStates.exists((state: State) => isFinal(state))
  }

}

