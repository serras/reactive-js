var lib=
 (lib ? lib : {});

lib.alert = function(s) {
    window.alert(s);
};

lib.getCurrentTime = function() {
    var d = new Date();
    return d.getTime();
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
    var position = lib.mousePosition();
    return position[0];
};

lib.mouseY = function() {
    var position = lib.mousePosition();
    return position[1];
};

lib.changeText = function(div, text) {
    document.getElementById(div).innerHTML = text;
};

lib.state = undefined;
lib.setState = function(t) {
    lib.state = t;
};
lib.getState = function() {
    return lib.state;
};

lib.addEvent = function(ev_name) {
    if (ev_name == "timeout") {
        var closure = function() {
            eventCallback(ev_name);
        };
        setTimeout(closure, 30);
    }
};
