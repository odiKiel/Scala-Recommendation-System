//application for backbonejs
var App = {
    Views: {},
    Routers: {},
    Collections: {},
    init: function() {
        new App.Routers.Ratings();
        Backbone.history.start();
    }
};

