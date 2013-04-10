//model for backbonejs
var Item = Backbone.Model.extend({
    url : function() {
      var base = 'data/items';
      if (this.isNew()) return base;
      return base + (base.charAt(base.length - 1) == '/' ? '' : '/') + this.id;
    }
});


