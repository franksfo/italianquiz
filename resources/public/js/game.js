var background = "white";
var DEBUG = 3;
var INFO  = 2;
var logging_level = INFO;
var radius = 15;
// TODO: get width and height of #game from DOM, not hardcoded.
var game_width = 1000;
var game_height = 500;
var offset=0;
var this_many_clouds = 5;

// how often a droplet falls.
var rain_time = 1000;
// how often wind blows on clouds
var blow_time =   30;

function start_game() {
    var svg = d3.select("#svgarena");
    add_clouds();
    make_it_rain(svg);

    setInterval(function() {
	make_it_rain(svg);
    },rain_time);
    setInterval(function() {
	blow_clouds(0);
    },blow_time);
}

var global_cloud_id = 0;

function add_clouds() {
    while ($(".fa-cloud").length < this_many_clouds) {
	add_cloud(global_cloud_id);
	global_cloud_id++;
    }
}

var cloud_speeds = {};

function add_cloud(cloud_id) {
    var sz = Math.floor(Math.random()*4) + 1;
    percent = (100 / this_many_clouds ) * $(".fa-cloud").length;
    $("#sky").append("<i id='cloud_" + cloud_id + "' class='fa fa-cloud x"+sz+"' style='left:" + percent + "%; top: 30px'> </i>");

    var cloud_text_dom_id = "cloud_" + cloud_id + "_text";


    var cloud_obj = $("#cloud_" + cloud_id)[0];
    var classes = cloud_obj.getAttribute("class")

    // TODO: duplication between CSS and this:
    var word_vertical = 80;
    if (classes.match(/x2\b/)) {
	word_vertical = 120;
    }
    if (classes.match(/x3\b/)) {
	word_vertical = 160;
    }
    if (classes.match(/x4\b/)) {
	word_vertical = 180;
    }

    $("#sky").append("<div id='cloud_" + cloud_id + "_text' class='cloudtext' style='top: " + word_vertical + "px'>" + ".." + "</div>");

    cloud_speeds["cloud_" + cloud_id] = Math.random()*.08;

    update_cloud_fn = function (content) {
        $("#"+cloud_text_dom_id).html(content);
    }

    // fill in the cloud's text in the background.
    $.ajax({
        dataType: "html",
        url: "/game/generate",
        success: update_cloud_fn
    });
}

function make_it_rain(svg) {
    // index_fn: what key to use to compare items for equality.
    var index_fn = function(d) {return d.name;};
    var new_x = Math.floor(Math.random()*game_width);
    newdata_array = [ {"name":"drop" + new_x,
		       "x":new_x}]; 

    if (existing) {
	debug("existing:" + 
	      existing.map(function(a){return a.name;}));
    }
    debug("new group:" + 
		newdata_array.map(function(a){return a.name;}));

    var newdata = svg.selectAll("circle").data(newdata_array,index_fn);

    var cloud = $(".fa-cloud")[Math.floor(Math.random()*$(".fa-cloud").length)];

    // Add items unique to input_data.
    newdata.enter().append("circle").
	attr("cx",function(c) {
	    var val= parseInt(cloud.style.left.replace('%',''));
	    return (val + 6) + "%";
	}).
	attr("cy",function(c) {return (parseInt(cloud.style.top.replace("px","")) + 130) + "px";}).
        attr("r", function(c) {return radius;}).
	attr("class",function(c) {
	    return c.name;
	}).
	transition().duration(rain_time).
	attr("cy", game_height - (100 + Math.floor(Math.random()*75)));
    
    // Remove items not in new data.
    newdata.exit().transition().duration(rain_time)
	.style("fill","lightgreen")
	.style("stroke","lightgreen")
	.remove();

    existing = newdata_array;
}

function blow_clouds(i) {
    var cloud =  $(".fa-cloud")[i];
    if (cloud) {
	blow_cloud(cloud);
	blow_clouds(i+1);
    }
}

function blow_cloud(cloud) {
    var cloud_left= parseFloat(cloud.style.left.replace('%',''));
    var cloud_id = cloud.id;
    if (cloud_left < 0) {
	// wrap clouds on left of screen.
	cloud.style.left = "95%";
    } else {
	if (cloud_left > 90) {
	    // wrap clouds on right of screen.
	    cloud.style.left = "1%";
	} else {
	    if (false) {
		var incr = Math.floor(Math.random()*30);
		if (incr == 0) {
		    cloud.style.left = (cloud_left - .0) + "%";
		} else {
		    if (incr < 10) {
			cloud.style.left = (cloud_left + .1) + "%";
		    }
		}
	    } else {
		var cloud_key = cloud_id;
		cloud.style.left = (cloud_left + cloud_speeds[cloud_key])+"%";
	    }
	}
    }

    
    cloud_left = parseFloat(cloud.style.left.replace('%',''));
    var cloud_text = $("#" + cloud_id + "_text")[0];
    debug("cloud text object: " + cloud_text);
    if (cloud_text.style != undefined) {
	cloud_text.style.left = (cloud_left+2) + "%";
    }


}

function debug(str) {
    if (logging_level >= DEBUG) {
	console.log("DEBUG: " + str);
    }
}

function random_set() {
    var choice_i = Math.floor(Math.random()*(set_of_maps.length));
    var set_name = set_of_maps[choice_i].name;
    d3.select("#status").html("New set chosen: " + set_name);
    return set_of_maps[choice_i];
}

function keys(arg) {
    return Object.keys(arg);
}
var existing = null;
