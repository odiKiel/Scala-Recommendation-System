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
        println("create transition for existing input: "+input)
      }
      else {
        transition(src) += (input -> dst)
        println("create transition for not existing input: "+input)
      }
    }
    else {
      transition += (src -> HashMap(input -> dst))
      println("create transition for not existing src: "+input+" "+dst)
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
    term.foreach((c: Char) => {
      println("from: "+currentStates+" with: "+c)
      currentStates = nextStateSet(currentStates, c)
      println("to: "+currentStates)
    })
    println("final States: "+currentStates)
    currentStates.exists((state: State) => isFinal(state))
  }

}

