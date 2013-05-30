package odi.recommendation
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack


class FiniteStateMachine {
  //(startState_: (Int, Int))
  type State = (Int, Int)
  val firstState = (0,0)

  val transition = collection.mutable.HashMap[State, HashMap[Char, collection.mutable.Set[State]]]() 
  val finalStates = collection.mutable.Set[(Int, Int)]()
  var degree = 0

  //val startState: (Int, Int) = startState_
  
  def addTransition(src: State, input: Char, dst: State) = {
    if(transition.contains(src)){
      if(transition(src).contains(input)){
        transition(src)(input) += dst
      }
      else {
        transition(src) += (input -> collection.mutable.Set(dst))
      }
    }
    else {
      transition += (src -> HashMap(input -> collection.mutable.Set(dst)))
    }
  }

  def getInputs(states: Seq[State]): Set[Char] = {
    states.flatMap(state => transition.get(state).getOrElse(HashMap()).keys).toSet

  }

  def addFinalState(state: State) = {
    finalStates += state
  }

   
  def nextState(src: State, input: Char): collection.mutable.Set[State] = {
    expand(collection.mutable.Set(src)).flatMap((state: State) => nextStateExpanded(state, input))
  }

  def nextStateExpanded(src: State, input: Char): collection.mutable.Set[State] = {
    if(transition.contains(src)) {
      val ret = transition(src).get(input).getOrElse(collection.mutable.Set[State]())
      ret ++= transition(src).get('*').getOrElse(collection.mutable.Set[State]())
      expand(ret)
    }
    else {
      collection.mutable.Set[State]()
    }

  }

  def isFinal(state: State): Boolean = {
    finalStates(state)
  }

  //get all states that are available thru an epsilon (insertion)
  def expand(states: collection.mutable.Set[State]) = {
    val current_states = collection.mutable.Stack[State]() ++ states
    while(current_states.size > 0) {
      val state = current_states.pop
      if(transition.contains(state)) {
        val new_states = transition(state).get('_').getOrElse(collection.mutable.Set[State]()) &~ (states)
        states ++= new_states
        current_states.pushAll(new_states)
      }
    }
    states
  }


  /** This method transforms the current non deterministic automata to a deterministic automata.
    *
    * For each epsilon transition one transition is introduced for every transition t_n that starts at the destination of the epsilon transition.
    * These transitions start at the source of the epsilon transition and end at the destinations of each t_n as label the label of the t_n transition is used.
    * The wildcard transitions are transformed in the way that they only accept labels that are not used by any other transition from the current source.
    */
  def toDfsm() = {
    val dfa = new DeterministicFiniteStateMachine(degree)
    val currentStates = new Stack[State]()
    currentStates.push(firstState)
    val seen = collection.mutable.Set[State]()

    while(currentStates.length > 0) {
      val current = currentStates.pop
      val inputs = getInputs(Seq(current))
      inputs.foreach((input: Char) => {
        if(input != '_') {
          val newStates = nextState(current, input)
          newStates.foreach((newState: State) => {
            if(!seen(newState)) {
              currentStates.push(newState)
              seen += newState
              if(isFinal(newState)) {
                dfa.addFinalState(newState)
              }
            }
          })
          if(input == '*') {
            dfa.addDefaultTransition(current, newStates)
          }
          else {
            dfa.addTransition(current, input, newStates)
          }
        }
      })
    }
    dfa
  }

  //words will be lowercased
  def levenshteinFiniteStateMachine(term: String, k: Int): FiniteStateMachine = {
    val fsm = this
    degree = k
    term.toLowerCase.zipWithIndex.foreach((e: (Char, Int)) => {
      (0 to k).foreach((i: Int) => {
        val current_state = (e._2, i)
        //correct character
        fsm.addTransition(current_state, e._1, (e._2+1, i))
        if(i < k) {
          //Deletion
          fsm.addTransition(current_state, '*', (e._2, i+1))
          //Insertion
          fsm.addTransition(current_state, '_', (e._2+1, i+1))
          //Substitution
          fsm.addTransition(current_state, '*', (e._2+1, i+1))
        } 
      })
      
    })
    (0 to k).foreach((e: Int) => {
      if(e<k) {
        //Deletion for last State
        fsm.addTransition((term.size, e), '*', (term.size, e+1))
      }
      //add final states
      fsm.addFinalState((term.size, e))
    })
    fsm
  }

}
