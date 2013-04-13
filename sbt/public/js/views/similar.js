App.Views.Similar = Backbone.View.extend({
    initialize: function() {
        this.similarity = this.options.model.attributes.similarity
        if(this.similarity == undefined) {
          this.similarity = ""
        }
        this.render();
    },

    render: function() {
      var template = $("#tableTd").html();
      this.$el.html(_.template(template,{value:this.similarity}));
    }
});

