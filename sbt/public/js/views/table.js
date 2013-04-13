App.Views.Table = Backbone.View.extend({
    initialize: function() {
        this.headers = this.options.headers;
        this.tbodyId = this.options.tbodyId;
        this.emptyCorner = this.options.emptyCorner;
        this.render();
    },
    
    render: function() {
      if(this.headers.length > 0) {
        var template = $("#tableWithHeader").html();
        this.$el.append(_.template(template,{headers: this.headers, tbodyId: this.tbodyId, emptyCorner: this.emptyCorner}));
      }
    }
});

