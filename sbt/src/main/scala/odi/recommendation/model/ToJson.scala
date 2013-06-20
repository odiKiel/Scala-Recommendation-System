package odi.recommendation

/** abstract class that expresses that the inherit class is able to return a json string */
abstract class ToJson {
  def toJson: String
}
