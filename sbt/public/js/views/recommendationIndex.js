App.Views.RecommendationIndex = Backbone.View.extend({
    initialize: function() {
        this.model = this.options.model;
        this.render();
    },
    events: {
              //todo watch out that this is the correct input
        "click #tableControls input": "clickInput",
        "click #calculateSimilarities": "calculateSimilarities"
    },
    clickInput: function(event){ 
         var reloadWindow = function(){
           window.location.reload();
           window.location.href=window.location.href;
         }
         var id = $(event.currentTarget).attr('id');
         switch(id) {
           case "addItem":
             var item = new Item;
             var number = App.ItemCollection.length+1;
             item.save({"title": "Item "+number}, {success: reloadWindow, error: reloadWindow});
             break;
           case "addUser":
             var user = new User;
             var number = App.UserCollection.length+1;
             user.save({"name": "User "+number}, {success: reloadWindow, error: reloadWindow});
             break;
           case "deleteItem":
             var item = App.ItemCollection.last();
             item.destroy({success: reloadWindow, error: reloadWindow});
             break;
           case "deleteUser":
             var user = App.UserCollection.last();
             user.destroy({success: reloadWindow, error: reloadWindow});
             break;
         }
    },
    calculateSimilarities: function() {
      $("#loading").css("display", "block");
      $.ajax({url: "calculate/similarities"}).done(function(result) {
        $("#loading").css("display", "none");
        $("#results").html("")
        $("#controls").html("")

        function displaySimilarUsersGraph() {
          //var data = [{ data: JSON.parse(result), label: "Pressure", color: "#333" }];
          var data = JSON.parse(result);
          var colors = ["#000000", "#808080", "#FF0000", "#FFFF00", "#0080000", "#00FF00", "#008080", "#00FFFF", "#0000FF", "#FF00FF"]
          var plotData = _.map(data, function(value, key, list){
            var user = "User"+(key+1);
            var color = colors[key];
            return {data: [value], label: user, color: color}
          })
          
          $.plot($("#placeholder"), plotData, {
            points: {show: true, border: false, fillColor: false},
            xaxis: { min: 0, max: 1, autoscaleMargin: 0.02 },
            yaxis: { min: -1, max: 1 },
          });
        }
        displaySimilarUsersGraph();

        var addTableEntry = function(array1, array2, currentObject, viewFunction, getFirstObjectSpecialId) {
          var ret
          if((array2.length == 0) || array1.length == 0 || (getFirstObjectSpecialId(array1[0]) != currentObject.attributes.id)) {
            ret = array1
          }
          else {
            var current1 = array1[0];
            var current2 = array2[0];
            viewFunction(current1)
            ret = addTableEntry(array1.slice(1), array2.slice(1), currentObject, viewFunction, getFirstObjectSpecialId)
          }
          return ret
        };

        var addTdForDiff = function(currentId, models, viewFunction){
          var counter = 0;
          var collectionId = models[counter].attributes.id;
          while(currentId != collectionId && counter<models.length){
            var simModel = new SimilarItem;
            viewFunction(simModel);
            counter +=1;
            collectionId = models[counter].attributes.id;
          }
        }

        function addSimilarItemTable(){
          var items = new App.Collections.Items;
          var similarItems = new App.Collections.SimilarItems;
          $.when(similarItems.fetch(), items.fetch()).done(function(similarItems_t, items_t) {
            var similarItemCollection = new App.Collections.SimilarItems(similarItems_t[0]);
            var itemCollection = new App.Collections.Items(items_t[0]);
            App.ItemCollection = itemCollection;

            var headers = _.map(itemCollection.models, function(item){return item.attributes.title})
            $("#results").append("<div id='itemSimilarityTable'></div>")
            new App.Views.Table({el: $("#itemSimilarityTable"), headers: headers, tbodyId: "itemSimilarity"});
            var addSimilarItemView = function(model) {
              $("#itemSimilarity > tr:last").append("<td id='itemSimilarity"+model.cid+"'></td>");
              new App.Views.Similar({el: $("#itemSimilarity"+model.cid), model: model})
            }

            var getItemOneId = function(model) {
              return model.attributes.itemOneId
            }

            var similarItemArray = similarItemCollection.models
            _.each(itemCollection.models, function(item, key){
              if(similarItemArray[0] != undefined) {
                $("#itemSimilarity").append("<tr><td>"+item.attributes.title+"</td></tr>")
                //var diff = similarItemArray[0].attributes.itemTwoId - itemCollection.models[0].attributes.id
                addTdForDiff(similarItemArray[0].attributes.itemTwoId, itemCollection.models, addSimilarItemView)
                similarItemArray = addTableEntry(similarItemArray, itemCollection.models, item, addSimilarItemView, getItemOneId)
              }
            })

          })
        }
        addSimilarItemTable();

        function addSimilarUserTable(){
          var users = new App.Collections.Users;
          var similarUsers = new App.Collections.SimilarUsers;
          $.when(users.fetch(), similarUsers.fetch()).done(function(users_t, similarUsers_t) {
            var userCollection = new App.Collections.Users(users_t[0]);
            App.UserCollection = userCollection;
            var similarUserCollection = new App.Collections.SimilarUsers(similarUsers_t[0]);

            var headers = _.map(userCollection.models, function(user){return user.attributes.name});
            $("#results").prepend("<div id='userSimilarityTable'></div>");
            new App.Views.Table({el: $("#userSimilarityTable"), headers: headers, tbodyId: "userSimilarity"});

            var addSimilarUserView = function(model) {
              $("#userSimilarity > tr:last").append("<td id='userSimilarity"+model.cid+"'>");
              new App.Views.Similar({el: $("#userSimilarity"+model.cid), model: model});
            }

            var getUserOneId = function(model) {
              return model.attributes.userOneId
            }

            var similarUserArray = similarUserCollection.models;
            _.each(userCollection.models, function(user, key){
              if(similarUserArray[0] != undefined) {
                $("#userSimilarity").append("<tr><td>"+user.attributes.name+"</td></tr>");
                //var diff = similarUserArray[0].attributes.userTwoId - userCollection.models[0].attributes.id;
                addTdForDiff(similarUserArray[0].attributes.userTwoId, userCollection.models, addSimilarUserView);
                similarUserArray = addTableEntry(similarUserArray, userCollection.models, user, addSimilarUserView, getUserOneId);
              }
            })
            $("#controls").append("<div id='recommendation'></div>")
            new App.Views.Recommendation({el: $("#recommendation"), model: userCollection})
          })
        }
        addSimilarUserTable()
      })
    },

    render: function() {
      var template = $("#ratingTableApp").html();
      this.$el.html(_.template(template,{rating:this.model}));
    }
});

