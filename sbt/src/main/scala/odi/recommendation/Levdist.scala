package odi.recommendation
import math._

object Levdist {
  def apply(s:String, t:String):Int = {
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


    math.min(apply(s, t.substring(0, t.length()-1))+1, math.min(apply(s.substring(0, s.length()-1), t)+1, apply(s.substring(0, s.length()-1), t.substring(0, t.length()-1))+cost))
  }

}


