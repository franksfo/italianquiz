// <configurable>
var cloud_ceiling = 20;
var cloud_altitude = function() {
    return cloud_ceiling + Math.floor(Math.random()*10);
}

var logging_level = DEBUG;
var fa_cloud = "fa-cloud";
//var fa_cloud = "fa-bicycle";
//var fa_cloud = "fa-fighter-jet";
var background = "white";
var radius = 15;
// TODO: get width and height of #game from DOM, not hardcoded.
var game_width = 1000;
var game_height = 500;
var offset=0;
var this_many_clouds = 5;

// how often a droplet falls.
var rain_time = 1000;
// timer for cloud motion interval, in milliseconds.
// a low blow_time looks smooth but will chow your clients' CPUs.
var blow_time = 1000;

// </configurable>


function start_game() {
    var svg = d3.select("#svgarena");
    add_clouds(this_many_clouds);

    setInterval(function() {
	blow_clouds(0);
    },blow_time);
}

var global_cloud_id = 0;

function add_clouds(add_this_many) {
    var done = 0;
    while (done < add_this_many) {
	add_cloud(global_cloud_id);
	global_cloud_id++;
	done++;
    }
}

var cloud_speeds = {};

function add_cloud(cloud_id) {
    log(INFO,"add_cloud(" + cloud_id + ")");
    var sz = Math.floor(Math.random()*4) + 1;
    var top = cloud_altitude();
    var percent = (100 / this_many_clouds ) * $(".motion").length;
    $("#sky").append("<i id='cloud_" + cloud_id + "' class='fa motion " + fa_cloud + " x"+sz+"' style='left:" + percent + "%; top: " + top + "px '> </i>");

    var cloud_q_dom_id = "cloud_" + cloud_id + "_q";
    var cloud_a_dom_id = "cloud_" + cloud_id + "_a";

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

    $("#sky").append("<div id='cloud_" + cloud_id + "_q' class='cloudq' style='top: " + word_vertical + "px'>" + ".." + "</div>");
    $("#gameform").append("<input id='cloud_" + cloud_id + "_a' class='cloud_answer'> </input>");

    cloud_speeds["cloud_" + cloud_id] = Math.random()*.20;

    update_answer_fn = function(content) {
	log(INFO,"answer: " + content);
	evaluated  = jQuery.parseJSON(content);
	log(INFO,"italian:" + evaluated.italian);
	log(INFO,"italian length:" + evaluated.italian.length);
	log(INFO,"cloud_id:" + evaluated.cloud_id);
	var cloud_a_dom_id = "cloud_" + evaluated.cloud_id + "_a";
	log(INFO,"Updating answer input with dom id: " + cloud_a_dom_id);
	// TODO: pass JSON directly rather than using the DOM as a data store.
	// Though the DOM has some advantages in that you can use it for presentation purposes.
	$("#"+cloud_a_dom_id).val(evaluated.italian);
    }

    update_cloud_fn = function (content) {
	evaluated = jQuery.parseJSON(content);
        $("#"+cloud_q_dom_id).html(evaluated.english);
	log(DEBUG,"Sending request: /game/generate-answers?cloud_id="+ cloud_id + "&semantics=" + evaluated.semantics);

	$.ajax({
	    dataType: "html",
	    url: "/game/generate-answers?cloud_id="+ cloud_id + "&semantics=" + evaluated.semantics,
	    success: update_answer_fn
	    });
    }

    // fill in the cloud's q in the background.
    $.ajax({
        dataType: "html",
        url: "/game/generate-question",
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

    var cloud = $(".motion")[Math.floor(Math.random()*$(".motion").length)];

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
    var cloud =  $(".motion")[i];
    if (cloud) {
	blow_cloud(cloud);
	blow_clouds(i+1);
    }
}

function blow_cloud(cloud) {
    var cloud_left= parseFloat(cloud.style.left.replace('%',''));
    var cloud_id = cloud.id;
    cloud.style.left = (cloud_left + cloud_speeds[cloud_id])+"%";

    if (cloud_left < 0) {
	// wrap clouds on left of screen.
	cloud.style.left = "95%";
    }
    if (cloud_left > 99) {
	// wrap clouds on right of screen.
	cloud.style.left = "1%";
    }

    var cloud_q_left_offset = 2;
    var cloud_q = $("#" + cloud_id + "_q")[0];
    log(TRACE,"cloud q object: " + cloud_q);
    if (cloud_q.style != undefined) {
	cloud_left = parseFloat(cloud.style.left.replace('%',''));
	cloud_q.style.left = (cloud_left+cloud_q_left_offset) + "%";
    }

    var incr = Math.floor(Math.random()*100);
    if (incr < 5) {
        cloud_speeds[cloud_id] = cloud_speeds[cloud_id] - 0.01;
    } else {
        if (incr < 10) {
	    cloud_speeds[cloud_id] = cloud_speeds[cloud_id] + 0.01;
        }
    }
    if (cloud_speeds[cloud_id] < 0) {
	cloud_speeds[cloud_id] = 0;
    }
    if (cloud_speeds[cloud_id] > 10) {
	cloud_speeds[cloud_id] = 5;
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

// TODO: pass the answers in as a javascript array rather than having to parse them from the HTML input value.
function submit_game_response(form_input_id) {
    var guess = $("#"+form_input_id).val();

    log(DEBUG,"submit_game_response: " + guess);

    var matched_q = $(".cloud_answer").map(function(answer) {
	answer = $(".cloud_answer")[answer];
	log(DEBUG,"ANSWER: " + answer);
	var answers = answer.value.split(",");
	log(DEBUG,"Answers: " + answers);
	    
	var i;
	for (i = 0; i < answers.length; i++) {
	    var answer_text = answers[i];
	    log(DEBUG,"answer_text is:: " + answer_text);
	    log(DEBUG,"checking guess: " + guess + " against answer: " + answer_text);
	    if (answer_text === guess) {
		log(INFO,"YOU GOT ONE RIGHT!");
		var answer_id = answer.id;
		$("#"+form_input_id).val("");
		$("#"+form_input_id).focus();

	
		// get the bare id (just an integer), so that we can manipulate related DOM elements:
		var answer_id = answer.id;	    
		var re = /cloud_([^_]+)_a/;
		answer_id = answer_id.replace(re,"$1");
		log(DEBUG,"post_re:(bare):" + answer_id);
		$("#cloud_" + answer_id + "_q").text(answer_text);
		$("#cloud_" + answer_id)[0].style.color = "lightgrey";
		$("#cloud_" + answer_id).fadeOut(3000);
		$("#cloud_" + answer_id + "_q").fadeOut(3000);
		$("#cloud_" + answer_id + "_a").remove();
		add_clouds(1);
	    }
	}
    });
}
