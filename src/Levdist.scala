import math._

object Levdist {
  def levdist(s:String, t:String):Int = {
    var cost = 0


    if(s.length() == 0) {
      return t.length()
    }
    else if(t.length() == 0) {
      return s.length
    }

    if(s.last != t.last) {
      cost = 1
    }


    return math.min(levdist(s, t.substring(0, t.length()-1))+1, math.min(levdist(s.substring(0, s.length()-1), t)+1, levdist(s.substring(0, s.length()-1), t.substring(0, t.length()-1))+cost))
  }

  def main(args: Array[String]) {
    var test = levdist("test", "tett1")
    println("test: "+test)
  }
}


