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
        var ratings = new App.Collections.Ratings();
        var items = new App.Collections.Items();
        //on success?
        $.when(ratings.fetch(), items.fetch()).done(function(ratings, items){
          new App.Views.Index({ratings: ratings[0], items: items[0]});
        });
    },
    
    
    newRating: function() {
        new App.Views.Edit({ model: new Rating() });
    }
});


