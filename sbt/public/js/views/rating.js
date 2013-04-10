App.Views.Rating = Backbone.View.extend({
    initialize: function() {
        this.model = this.options.model;
        this.render();
    },
    events: {
        "blur input": "save"
    },
    save: function(event){ 
         var value = $(event.currentTarget).val()
         this.model.set({'rating': parseInt(value)}) 
         this.model.save()
    },
    render: function() {
      var template = $("#rating").html();
      this.$el.html(_.template(template,{rating:this.model}));
    }
});

