App.Views.Recommendation = Backbone.View.extend({
    initialize: function() {
        this.model = this.options.model;
        this.render();
    },
    events: {
        "click input": "getRecommendation"
    },
    getRecommendation: function(event){ 
      $("#loading").css("display", "block");
      var userId = $(this.$el).children("select").val();
      $.when($.ajax("calculate/recommendations/"+userId), $.ajax("calculate/recommendationsItemBased/"+userId)).done(function(resultSVD, resultItem) {
        $("#loading").css("display", "none");

        function svdRecommendationTable(){
          $("#results").append("<div id='svdRecommendations'>SVD Based Recommendations</div>");
          new App.Views.Table({el: $("#svdRecommendations"), headers: ["Item", "Rating"], tbodyId: "svdRecommendationEntries", emptyCorner: false});
          var array = JSON.parse(resultSVD[0])
          _.each(array, function(recommendation){
            $("#svdRecommendationEntries").append("<tr><td>"+App.ItemCollection.get(recommendation._1).attributes.title+"</td><td>"+recommendation._2+"</td></tr>")
          })
        }
        svdRecommendationTable()

        function itemBasedRecommendationTable(){
          $("#results").append("<div id='itemBasedRecommendations'>Item Based Recommendations</div>");
          new App.Views.Table({el: $("#itemBasedRecommendations"), headers: ["Item", "Rating"], tbodyId: "itemBasedRecommendationEntries", emptyCorner: false});
          var array = JSON.parse(resultItem[0]);
          _.each(array, function(recommendation){
            $("#itemBasedRecommendationEntries").append("<tr><td>"+App.ItemCollection.get(recommendation._1).attributes.title+"</td><td>"+recommendation._2+"</td></tr>")
          })
        }
        itemBasedRecommendationTable()
          

          
      })
    },
    render: function() {
      var template = $("#recommendationInput").html();
      this.$el.html(_.template(template,{users: this.model}));
    }
});

