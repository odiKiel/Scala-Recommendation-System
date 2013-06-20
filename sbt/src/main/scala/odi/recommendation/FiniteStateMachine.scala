package odi.recommendation
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack


/** this class represents a finite state machine also known as non deterministic automata */
class FiniteStateMachine {
  type State = (Int, Int)
  val firstState = (0,0)

  val transition = collection.mutable.HashMap[State, HashMap[Char, collection.mutable.Set[State]]]() 
  val finalStates = collection.mutable.Set[(Int, Int)]()
  var degree = 0

  
  /** add a transition to the automata
    *
    * @param src the source of the transition
    * @param input the input character that is used as the label
    * @param dst the destination of the transition
    */
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

  /** return the possible input charachters for a state */
  def getInputs(states: Seq[State]): Set[Char] = {
    states.flatMap(state => transition.get(state).getOrElse(HashMap()).keys).toSet

  }

  /** mark a state as a final state
    *
    * @param state the state that should be marked as final
    */
  def addFinalState(state: State) = {
    finalStates += state
  }

   
  /** return the next states that can be reached from the current state with the input character
    *
    * @param src the state that is requested
    * @param input the character that the transitions should correspond with
    * @return a set of states that can be reached
    */
  def nextState(src: State, input: Char): collection.mutable.Set[State] = {
    expand(collection.mutable.Set(src)).flatMap((state: State) => nextStateExpanded(state, input))
  }

  /** return the next states for an input character as well as all states that can be reached after an
    * epsilon transition is used with the character
    * @param src the state that is tested
    * @param input the character that is tested
    * @return a set of states that can be reached
    */
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

  /** check if a state is a final state */
  def isFinal(state: State): Boolean = {
    finalStates(state)
  }

  //get all states that are available through an epsilon transition (insertion)
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
    * These transitions start at the source of the epsilon transition and ends at the destinations of each t_n as label the label of the t_n transition is used.
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
  /** create a non deterministic levenshtein automata for a word and a maximum distance
    *
    * @param term the word that is used for creating the levenshtein automata
    * @param k the maximum allowed levenshtein distance
    */
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
