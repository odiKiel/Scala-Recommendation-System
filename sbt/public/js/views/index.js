App.Views.Index = Backbone.View.extend({
    initialize: function() {
        this.ratings = this.options.ratings;
        this.items = this.options.items;
        this.render();
    },
    
    render: function() {
      if(this.ratings.length > 0) {
        var template = $("#ratingTable").html();
        this.$el.html(_.template(template,{ratings:this.ratings.models, items:this.items.models}));
      }
    }
});

