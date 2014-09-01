var background = "white";
var DEBUG = 3;
var INFO  = 2;
var logging_level = INFO;
var radius = 15;
// TODO: get width and height of #game from DOM, not hardcoded.
var game_width = 1000;
var game_height = 500;
var offset=0;
var transition_time = 1000;

function start_game() {
    var svg = d3.select("#svgarena");
    make_it_rain(svg);
    setInterval(function() {
	make_it_rain(svg);
    },transition_time);
    setInterval(function() {
	blow_clouds(0);
    },30);
}

function debug(str) {
    if (logging_level >= DEBUG) {
	console.log("DEBUG: " + str);
    }
}

function blow_clouds(i) {
    var cloud =  $(".fa-cloud")[i];
    if (cloud) {
	blow_cloud(cloud);
	blow_clouds(i+1);
    }
}

function blow_cloud(cloud) {
    var val= parseInt(cloud.style.left.replace('%',''));
    if (val < 0) {
	cloud.style.left = "90%";
    } else {
	if (val > 90) {
	    cloud.style.left = "1%";
	} else {
	    var incr = Math.floor(Math.random()*30);
	    if (incr == 0) {
		cloud.style.left = (val - 1) + "%";
	    } else {
		if (incr == 1) {
		    cloud.style.left = (val + 1) + "%";
		}
	    }
	}
    }
}

var allow_duplicates = true;
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
	    return cloud.style.left;
	}).
	attr("cy",function(c) {return (parseInt(cloud.style.top.replace("px","")) + 100) + "px";}).
        attr("r", function(c) {return radius;}).
	attr("class",function(c) {
	    return c.name;
	}).
	transition().duration(transition_time*1.5).
	attr("cy",game_height-50);
    
    // Remove items not in new data.
    newdata.exit().transition().duration(transition_time)
	.style("fill","lightgreen")
	.style("stroke","lightgreen")
	.remove();

    existing = newdata_array;
}

var previous_set = null;

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
