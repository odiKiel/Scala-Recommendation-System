//model for backbonejs
var Rating = Backbone.Model.extend({
    url : function() {
      var base = 'data/ratings';
      if (this.isNew()) return base;
      return base + (base.charAt(base.length - 1) == '/' ? '' : '/') + this.id;
    }
});


