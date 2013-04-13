//model for backbonejs
var SimilarItem = Backbone.Model.extend({
    url : function() {
      var base = 'data/similarItems';
      if (this.isNew()) return base;
      return base + (base.charAt(base.length - 1) == '/' ? '' : '/') + this.id;
    }
});


