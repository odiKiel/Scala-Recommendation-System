package odi.recommendation
import scala.collection.mutable.HashMap

/** this class represents a deterministic state machine 
  * also known as deterministic automata
  * it has the functionality for creating a levenshtein automata
  */
class DeterministicFiniteStateMachine(degree: Int) {
  type State = (Int, Int)

  val transition = new HashMap[State, HashMap[Char, collection.mutable.Set[State]]]() 
  val finalStates = collection.mutable.Set[(Int, Int)]()
  val defaultTransition = new HashMap[State, collection.mutable.Set[State]]()
  val firstStates = collection.mutable.Set() ++ (1 to degree).map((i: Int) => (i, i))

  /** add a transition to the automata
    *
    * @param src the source of the transition
    * @param input the letter that is read
    * @param dst the destination of the transition
    */
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



  /** add a default transition this transition is used if no other transition has the correct label
    *
    * @param src the source of the transition
    * @param dst the destination of the transition
    */
  def addDefaultTransition(src: State, dst: collection.mutable.Set[State]) = {
    defaultTransition += (src -> dst)
  }

  /** mark the state as a final state
    *
    * @param state the state that should be marked
    */
  def addFinalState(state: State) = {
    finalStates += state
  }

  /** check if the state is a final state */
  def isFinal(state: State): Boolean = {
    finalStates(state)
  }

  /** return the next state for a state and an input char, this method is called by nextState */
  def nextStateSingle(src: State, input: Char): collection.mutable.Set[State] = {
    val ret = collection.mutable.Set[State]()
    if(transition.contains(src)) {
      ret ++= transition(src).get(input).getOrElse(collection.mutable.Set[State]())
    }
    ret ++ defaultTransition.get(src).getOrElse(collection.mutable.Set[State]())
  }

  /** return the next state for a set of sources and an input character
    *
    * @param srcs the states that are used for this transition
    * @param input the character that is read
    * @return a set of states that represent the next state
    */
  def nextState(srcs: collection.mutable.Set[State], input: Char): collection.mutable.Set[State] = {
    //(0,0) needs the states for the first insertion
    if(srcs.contains((0,0))) {
      srcs ++= firstStates
    }
    srcs.flatMap((src: State) => nextStateSingle(src, input))
  }

  /** check if a word reaches a final state in this automata
    *
    * @param term the word that should be checked
    * @return a boolean that indicates if the word reaches a final state
    */
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

