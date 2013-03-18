package odi.recommendation

object Services {
  def apply(service: String): String = {
    service match {
      case "queryStwServer" => "11500"
      case "taggerService" => "11000"
      case "levenshteinDistanceService" => "11600"
    }
  }
}
