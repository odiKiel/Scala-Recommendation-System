//model for backbonejs
var SimilarUser = Backbone.Model.extend({
    url : function() {
      var base = 'data/similarUsers';
      if (this.isNew()) return base;
      return base + (base.charAt(base.length - 1) == '/' ? '' : '/') + this.id;
    }
});


