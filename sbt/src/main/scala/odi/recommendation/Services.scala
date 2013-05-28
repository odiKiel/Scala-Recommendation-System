package odi.recommendation

object Services {
  def apply(service: String): String = {
    service match {
      case "queryStwServer" => "11500"
      case "taggerService" => "11000"
      case "levenshteinDistanceService" => "11600"
      case "recommendationService" => "13000"
      case "itemBasedService" => "13100"
      case "svdBasedService" => "13200"
      case "webService" => "10000"
      case "ratingService" => "12000"
    }
  }
}
