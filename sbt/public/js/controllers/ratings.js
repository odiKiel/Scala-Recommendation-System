//controller for backbonejs
App.Routers.Ratings = Backbone.Router.extend({
    routes: {
        "ratings/:id":            "edit",
        "":                         "index",
        "new":                      "newRating"
    },
    
    edit: function(id) {
        var rating = new Rating({ id: id });
        rating.fetch({
            success: function(model, resp) {
                new App.Views.Edit({ model: rating });
            },
            error: function() {
                new Error({ message: 'Could not find that rating.' });
                window.location.hash = '#';
            }
        });
    },

    index: function() {
        var addRating = function(ratingArray, itemArray, currentUser) {
          var ret
          //done with this table row
          if(itemArray.length == 0) {
            ret = ratingArray
          }
          //still ratings that must be added
          else {

            //function for adding a new rating to the table
            var addRatingView = function(model) {
              $("#ratingInputs > tr:last").append("<td id='rating"+model.cid+"'></td>");
              new App.Views.Rating({el: $("#rating"+model.cid), model: model})
            }

            if(ratingArray.length == 0) {
              var newRating = new Rating;
              addRatingView(newRating);
              ret = addRating(ratingArray, itemArray.slice(1), currentUser)
            }
            else {

              var currentRating = ratingArray[0];
              var currentItem = itemArray[0];
              if(currentRating.attributes.itemId == currentItem.attributes.id) {
                addRatingView(currentRating);
                ret = addRating(ratingArray.slice(1), itemArray.slice(1), currentUser)
              }
              else {
                var newRating = new Rating;
                newRating.set({"itemId": parseInt(currentItem.attributes.id), "userId": parseInt(currentUser.attributes.id), "prediction": false})
                addRatingView(newRating);
                ret = addRating(ratingArray, itemArray.slice(1), currentUser)
              }
            }
          }
          return ret
        }

        var ratings = new App.Collections.Ratings();
        var items = new App.Collections.Items();
        var users = new App.Collections.Users();
        //on success?
        $.when(ratings.fetch(), items.fetch(), users.fetch()).done(function(ratings_t, items_t, users_t){
          var ratings = new App.Collections.Ratings(ratings_t[0])
          var items = new App.Collections.Items(items_t[0])
          var users = new App.Collections.Users(users_t[0])
          new App.Views.Index({el: $("#app"), ratings: ratings, items: items});
        //call addrating for each user
        //append<tr><td>UserId</td>></tr>
          var ratingArray = ratings.models
          _.each(users.models, function(user){
            $("#ratingInputs").append("<tr><td>"+user.attributes.name+"</td></tr>");
            ratingArray = addRating(ratingArray, items.models, user)
          })
          //$("#ratingInputs").append("<td id='rating"+ratings.models[0].attributes.id+"'></td>")
          ///new App.Views.Rating({el: $("#rating"+ratings.models[0].attributes.id), model: ratings.models[0]})
        });
    },
    
    
    newRating: function() {
        new App.Views.Edit({ model: new Rating() });
    }
});


