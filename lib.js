var lib=
 (lib ? lib : {});

lib.thread_start = function(callback) {
  var exitCondition, onEnd, onKill;
  exitCondition = function(t){
    return true;
  };
  onEnd = onKill = function(t){
    // Do nothing
  };
  
  var jThread = new JThread(callback, exitCondition, onEnd, onKill);
  jThread.run();
}

lib.alert = function(s) {
    window.alert(s);
};

lib.getCurrentTime = function() {
    var d = new Date();
    return d.getTime();
};

lib.getCurrentHours = function() {
    var d = new Date();
    return d.getHours();
};

lib.getCurrentMinutes = function() {
    var d = new Date();
    return d.getMinutes();
};

lib.getCurrentSeconds = function() {
    var d = new Date();
    return d.getSeconds();
};

lib.mousePosition = function() {
    /* var posx = 0;
	var posy = 0;
	var e = jQuery.event;
	if (!e) var e = window.event;
	if (e.pageX || e.pageY) {
		posx = e.pageX;
		posy = e.pageY;
	}
	else if (e.clientX || e.clientY) 	{
		posx = e.clientX + document.body.scrollLeft
			+ document.documentElement.scrollLeft;
		posy = e.clientY + document.body.scrollTop
			+ document.documentElement.scrollTop;
	}
	// posx and posy contain the mouse position relative to the document
	// Do something with this information
	
	return [posx, posy]; */
	
	/*
	var doc = $(document);
	return [doc.pageX, doc.pageY];
	*/
	
	return [2,4];
};

lib.mouseX = function() {
    return 1;

    return Math.floor(Math.random() * 20);

    var position = lib.mousePosition();
    return position[0];
};

lib.mouseY = function() {
    return 2;

    return Math.floor(Math.random() * 20);

    var position = lib.mousePosition();
    return position[1];
};

lib.changeDiv = function(div, text) {
    lib.thread_start(function() {
      try {
        document.getElementById(div).innerHTML = text;
      } catch (err) { }
    });
};

lib.changeButtonText = function(div, text) {
    lib.thread_start(function() {
      try {
        document.getElementById(div).value = text;
      } catch (err) { }
    });
};

lib.state = undefined;
lib.setState = function(t) {
    lib.state = t;
};
lib.getState = function() {
    return lib.state;
};

lib.addEvent = function(ev_name, params) {
    if (ev_name == "timeout") {
        var closure = function() {
            eventCallback(ev_name);
        };
        setInterval(closure, 1000);
    }
    if (ev_name == "lifebeat") {
        var closure = function() {
            eventCallback(ev_name);
        };
        setInterval(closure, 100);
    }
    if (ev_name == "click") {
        var closure = function() {
            eventCallback(ev_name);
        };
        alert("Zas");
        $(params).click(closure);
    }
};
