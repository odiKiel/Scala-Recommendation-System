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
    var visibleTime = 0;
    var visible = false;

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

    function onchange(evt) {
      if(isVisible(evt, this)) {
        visible = true;
        console.log("visible");
        timer = new Date().getTime();
      }
      else {
        visible = false;
        visibleTime += (new Date().getTime() - timer)
        sendData(visibleTime, window.scrollTime);
        console.log("hidden visible for "+visibleTime);
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

  window.onbeforeunload = function() {
    //send rating to server
      console.log('bye')
      if(visible){
        visibleTime += (new Date().getTime() - timer)
        timer = new Date().getTime();
        sendData(visibleTime, window.scrollTime);
      }
      return "Active website: "+visibleTime+" scroll time: "+window.scrollTime
  }

})();

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
  xmlhttp.open("POST","ajax_test.asp",true);
  xmlhttp.setRequestHeader("Content-type","application/x-www-form-urlencoded");
  xmlhttp.send("visibleTime="+visibleTime+"&scrollTime="+scrollTime);

 
  
}  
(function() {
  var sendInitialItemId = function() {
    var xmlhttp;
    if (window.XMLHttpRequest)
    {// code for IE7+, Firefox, Chrome, Opera, Safari
      xmlhttp=new XMLHttpRequest();
    }
    else
    {// code for IE6, IE5
      xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
    }
    xmlhttp.open("POST","ajax_test.asp",true);
    xmlhttp.onreadystatechange = runJs;
    xmlhttp.setRequestHeader("Content-type","application/x-www-form-urlencoded");
    xmlhttp.send("visibleTime="+visibleTime+"&scrollTime="+scrollTime);

    function runJs()
    {
      if (xmlhttp.readyState == 4) {
        var result = xmlHttp.responseText;
        if(result != 'OK') {
          eval(result);
        }
      }

      /* if you've returned javascript instead of xml or text, 
      you can eval(result) to access the javascript variables returned.
      */
    }
  }
 

  if(document.readyState === "complete") {
    //Already loaded!
  }
  else {
    //Add onload or DOMContentLoaded event listeners here: for example,
    window.addEventListener("onload", function () {/* your code here */}, false);
    //or
    //document.addEventListener("DOMContentLoaded", function () {/* code */}, false);
  }
})();
