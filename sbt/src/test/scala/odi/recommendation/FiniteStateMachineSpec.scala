package odi.recommendation
import org.specs2.mutable._

object FiniteStateMachineSpec extends Specification {
    val fst = new FiniteStateMachine()
    fst.levenshteinFiniteStateMachine("test", 2)
    "should create a finite state machine" in {
      fst.getInputs(Seq((0,0))) mustEqual Set('_', 't', '*')
      fst.nextState((0,0), 't') mustEqual Set((1,0), (0,1), (1,1), (1,2), (2,2), (2,1), (3,2))
      fst.nextState((0,0), 'F') mustEqual Set((0,1),(1,1),(1,2),(2,2))
      fst.nextState((0,0), 'F') mustEqual Set((0,1),(1,2),(1,1), (2,2))
    }
}

