function getCurrentTime() {
    var d = new Date();
    return d.getTime();
}

function mousePosition() {
    var posx = 0;
	var posy = 0;
	if (!e) var e = window.event;
	if (e.pageX || e.pageY) 	{
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
	
	return [posx, posy];
}

function mouseX() {
    var position = mousePosition();
    return position[0];
}

function mouseY() {
    var position = mousePosition();
    return position[1];
}

function changeText(div, text) {
    document.getElementById(div).innerHTML = text;
}

function addEvent(div, event, guiref, rhandle, response) {
    if (event == "timeout") {
        var closure = function() {
            respond(guiref, rhandle, response);
        }
        setTimeout(closure, 30);
    }
}
