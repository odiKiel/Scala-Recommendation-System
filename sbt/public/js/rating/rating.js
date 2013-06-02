(function() {        
    var timer;
    var startedAt = -1;
    window.scrollTime = 0;
    window.onscroll = function () {
        if(startedAt == -1) {
          console.log('started scrolling');
          startedAt = new Date().getTime();
        }
        clearTimeout(timer);
        timer = setTimeout( refresh , 150 );
    };
    var refresh = function () { 
        // do stuff
        window.scrollTime += (new Date().getTime() - startedAt)
        console.log('scrolled for: '+window.scrollTime); 
        startedAt = -1;
    };
})();

(function() {
    var hidden = "hidden";
    var timer = new Date().getTime();
    window.visibleTime = 0;
    window.visible = true;

    // Standards:
    if (hidden in document)
        document.addEventListener("visibilitychange", onchange);
    else if ((hidden = "mozHidden") in document)
        document.addEventListener("mozvisibilitychange", onchange);
    else if ((hidden = "webkitHidden") in document)
        document.addEventListener("webkitvisibilitychange", onchange);
    else if ((hidden = "msHidden") in document)
        document.addEventListener("msvisibilitychange", onchange);
    // IE 9 and lower:
    else if ('onfocusin' in document)
        document.onfocusin = document.onfocusout = onchange;
    // All others:
    else
        window.onpageshow = window.onpagehide 
            = window.onfocus = window.onblur = onchange;

    var sendData = function(visibleTime, scrollTime){
      var xmlhttp;
      if (window.XMLHttpRequest)
      {// code for IE7+, Firefox, Chrome, Opera, Safari
        xmlhttp=new XMLHttpRequest();
      }
      else
      {// code for IE6, IE5
        xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
      }
      xmlhttp.open("POST","http://localhost:10000/rating/calculateRating",true);
      xmlhttp.setRequestHeader("Content-type","application/x-www-form-urlencoded");

      var post = [""+visibleTime, ""+scrollTime, _rating['item'], _rating['user'], ""+0] //last parameter is user interaction
      xmlhttp.send(JSON.stringify(post));
      
    }

    function onchange(evt) {
      if(isVisible(evt, this)) {
        window.visible = true;
        console.log("visible");
        timer = new Date().getTime();
      }
      else {
        window.visible = false;
        window.visibleTime += (new Date().getTime() - timer)
        //sendData(window.visibleTime, window.scrollTime);
        console.log("hidden visible for "+window.visibleTime);
        timer = -1;
      }
    }

    function isVisible(evt, object) {
        var v = 'visible', h = 'hidden',
            evtMap = { 
                focus:v, focusin:v, pageshow:v, blur:h, focusout:h, pagehide:h 
            };

        evt = evt || window.event;
        if (evt.type in evtMap) {
          console.log("evtMap")
          return true;
        }
        else { 
          console.log(object[hidden])
            if(object[hidden]) {
              return false;
            }
            else {
              return true;
            }
        }

    }

    var sendDataTimer = function() {
      if(window.visible){
        window.visibleTime += (new Date().getTime() - timer);
        timer = new Date().getTime();
        sendData(window.visibleTime, window.scrollTime);
      }
      window.setTimeout(sendDataTimer, 2000);
    }; 

    window.setTimeout(sendDataTimer, 2000);

    

})();

    
(function() {
  var sendInitialItem = function() {
    var xmlhttp;
    if (window.XMLHttpRequest)
    {// code for IE7+, Firefox, Chrome, Opera, Safari
      xmlhttp=new XMLHttpRequest();
    }
    else
    {// code for IE6, IE5
      xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
    }
    xmlhttp.open("POST","http://localhost:10000/rating/currentItem/"+_rating['item']+"/"+_rating['user'],true);
    xmlhttp.setRequestHeader("Content-type","application/x-www-form-urlencoded");
    var post = {};
    post['text'] = $(_rating['information']).text();
    post['title'] = $(_rating['title']).text();
    post['url'] = document.URL;

    xmlhttp.send(JSON.stringify(post));
  }
 

  if(document.readyState === "complete") {
    sendInitialItem();
  }
  else {
    window.addEventListener("onload", function () {sendInitialItem()}, false);
  }
})();

