package odi.recommendation
import org.specs2.mutable._

object DeterministicFiniteStateMachineSpec extends Specification {
    val fst = new FiniteStateMachine()
    fst.levenshteinFiniteStateMachine("test", 2)
    val dfsm = fst.toDfsm()
    
    //short explanation: (a,b) a => current letter number, b error count
    "should generate a deterministic final state machine with max distance 3" in {
      dfsm.nextState((0,0), 't') mustEqual Set((1,0), (0,1), (1,1), (1,2), (2,2), (2,1), (3,2))
      dfsm.nextState((0,0), 'F') mustEqual Set((0,1), (1,2), (1,1), (2,2))
      dfsm.nextState((1,0), 'F') mustEqual Set((2,1), (1,1), (2,2), (3,2))
    }
    "returns true if string is in distance (equal)" in {
      dfsm.isInDistance("test") mustEqual true
    }
    "returns true if string is in distance capital insensitive (equal)" in {
      dfsm.isInDistance("TEST") mustEqual true
    }

    "returns true if string is in distance (deletion middle)" in {
      dfsm.isInDistance("tesst") mustEqual true
    }
    "returns true if string is in distance (deletion beginning)" in {
      dfsm.isInDistance("ttest") mustEqual true
    }
    "returns true if string is in distance (deletion end)" in {
      dfsm.isInDistance("testt") mustEqual true
    }

    "returns true if string is in distance (substitution beginning)" in {
      dfsm.isInDistance("eest") mustEqual true
    }
    "returns true if string is in distance (substitution middle)" in {
      dfsm.isInDistance("teat") mustEqual true
    }
    "returns true if string is in distance (substitution end)" in {
      dfsm.isInDistance("tesd") mustEqual true
    }

    "returns true if string is in distance (insertion beginning)" in {
      dfsm.isInDistance("est") mustEqual true
    }
    "returns true if string is in distance (insertion middle)" in {
      dfsm.isInDistance("tst") mustEqual true
    }
    "returns true if string is in distance (insertion end)" in {
      dfsm.isInDistance("tes") mustEqual true
    }

    "returns false if string is not in distance (beginning)" in {
      dfsm.isInDistance("123test") mustEqual false
    }
    "returns false if string is not in distance (middle)" in {
      dfsm.isInDistance("te123st") mustEqual false
    }
    "returns false if string is not in distance (end)" in {
      dfsm.isInDistance("test123") mustEqual false
    }


}

