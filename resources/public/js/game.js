// <configurable>
var logging_level = INFO;
var fa_cloud = "fa-cloud";
//var fa_cloud = "fa-bicycle";
// var fa_cloud = "fa-fighter-jet";
var background = "white";
var radius = 15;
// TODO: get width and height of #game from DOM, not hardcoded.
var game_width = 1000;
 var game_height = 500;
 var offset=0;
 var this_many_clouds = 3;

 // beginners' maximum speed is 10, but it gets increased when you correctly answer questions.
 var current_speed_limit = 10;

 var min_speed = 5;
 var max_speed = 15;

 // how often a droplet falls.
 var rain_time = 1000;
 // timer for cloud motion interval, in milliseconds.
 // a low blow_time looks smooth but will chow your clients' CPUs.
 var blow_time = 50;

 var cloud_ceiling = 20;
 var cloud_altitude = function() {
     return cloud_ceiling + Math.floor(Math.random()*70);
 }

 // </configurable>

var wind_is_frozen = false;

function freeze_wind() {
    wind_is_frozen = true;
}

function unfreeze_wind() {
    wind_is_frozen = false;
}

function start_game() {
    normal_returnkey_mode();
    var svg = d3.select("#svgarena");
    add_clouds(this_many_clouds);
    
    $("#game_input").focus();
    
    setInterval(function() {
	if (wind_is_frozen == false) {
	    blow_clouds(0);
	} else {
	    log(DEBUG,"WIND IS FROZEN..");

	}
    },blow_time);
}

 var global_cloud_id = 0;

 function add_clouds(add_this_many) {
     var added = 0;
     while (added < add_this_many) {
	 add_cloud(global_cloud_id);
	 added++;
	 global_cloud_id++;
    }
}

var cloud_speeds = {};

function add_cloud(cloud_id) {
    log(INFO,"add_cloud(" + cloud_id + ")");
    var size = Math.floor(Math.random()*4) + 1;
    var top = cloud_altitude();
    var left = 1;
    $("#sky").append("<i id='cloud_" + cloud_id + "' class='fa motion " + fa_cloud + " x"+size+"' style='display:none;left:" + left + "%; top: " + top + "px '> </i>");

    var cloud_q_dom_id = "cloud_" + cloud_id + "_q";
    var cloud_a_dom_id = "cloud_" + cloud_id + "_a";

    var cloud_obj = $("#cloud_" + cloud_id)[0];
    var classes = cloud_obj.getAttribute("class")

    // TODO: remove duplication between CSS and this:
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

    $("#sky").append("<div id='cloud_" + cloud_id + "_q' class='cloudq' style='display:none;top: " + word_vertical + "px'>" + ".." + "</div>");
    $("#gameform").append("<input id='cloud_" + cloud_id + "_a' class='cloud_answer'> </input>");

    // start nice and slow.
    cloud_speeds["cloud_" + cloud_id] = Math.random()*0.010;
//    cloud_speeds["cloud_" + cloud_id] = 1;

    update_answer_fn = function(content) {
	evaluated  = jQuery.parseJSON(content);

	var cloud_a_dom_id = "cloud_" + evaluated.cloud_id + "_a";
	var cloud_q_dom_id = "cloud_" + evaluated.cloud_id + "_q";

	var lca_dom_id = "lca_" + evaluated.cloud_id;
	var answer_dom_id = "answer_" + evaluated.cloud_id;
	var rca_dom_id = "rca_" + evaluated.cloud_id;

	log(INFO,"Updating answer input with dom id: " + cloud_a_dom_id);
	// TODO: pass JSON directly rather than using the DOM as a data store.
	// Though the DOM has some advantages in that you can use it for presentation purposes.
	$("#"+cloud_a_dom_id).val(evaluated.answer);
	$("#"+lca_dom_id).html(evaluated.lca);
	$("#"+rca_dom_id).html(evaluated.rca);
	log(DEBUG,"Updating question color for dom id: " + cloud_q_dom_id);
	$("#cloud_"+evaluated.cloud_id).fadeIn(500,function() {
	    $("#"+cloud_q_dom_id).fadeIn(100);
	});
    }

    update_cloud_fn = function (content) {
	log(DEBUG,"Updating cloud with question content: " + content);
	evaluated = jQuery.parseJSON(content);
	// TODO: avoid munging html like this - it's hard to understand.
        $("#"+cloud_q_dom_id).html("<span class='lca' id='lca_"+cloud_id+"'>" + "(fill me in1)" + "</span>" +
				   "<span class='question' id='question_"+cloud_id+"'> " + evaluated.question + " </span>" +
				   "<span class='spacing'> </span>" +
				   "<span class='answer'   id='answer_"+cloud_id+"'> </span>" +
				   "<span class='rca' id='rca_"+cloud_id+"'>" + "(fill me in2)" + "</span>");
				   
	log(DEBUG,"Sending request: /game/generate-answers?cloud_id="+ cloud_id + "&semantics=" + evaluated.semantics);

	$.ajax({
	    dataType: "html",
	    url: "/game/generate-answers?cloud_id="+ cloud_id + "&semantics=" + JSON.stringify(evaluated.semantics),
	    success: update_answer_fn
	    });
    }

    // fill in the cloud's q in the background.
    $.ajax({
	cache: false,
        dataType: "html",
        url: "/game/generate-question",
        success: update_cloud_fn
    });
}

function blow_clouds(i) {
    log(DEBUG,"blow_clouds(" + i + ")");
    var cloud =  $(".motion")[i];
    if (cloud) {
	blow_cloud(cloud);
	blow_clouds(i+1);
    }
}

function normal_returnkey_mode() {
    log(INFO,"NORMAL RETURNKEY MODE.");
    $(document).ready(function() {
	$(window).keydown(function(event){
	    if(event.keyCode == 13) {
		log(INFO,"YOU PRESSED RETURN IN NORMAL MODE.");
		submit_game_response('game_input');
		return false;
	    }
	});
    });
}

function correction_returnkey_mode() {
    log(INFO,"CORRECTION RETURNKEY MODE.");
    $(document).ready(function() {
	$(window).keydown(function(event){
	    if(event.keyCode == 13) {
		log(INFO,"YOU PRESSED RETURN IN CORRECTION MODE.");
		submit_correction_response('game_input');
		return false;
	    }
	});
    });
}

function submit_correction_response(form_input_id) {
    var guess = $("#"+form_input_id).val();
    log(INFO,"Got here: you are trying to correct the response with your guess: " + guess);
    if (guess === $("#correct_answer").html()) {
	log(INFO,"Good! you got it right; you can continue with the game.");
	var bare_id = $("#correction_bare_id").val();
	log(INFO,"bare_id: " + bare_id);
	log(INFO,"Clearing dialog..");
	$("#correction_dialog")[0].style.display = "none";
	$("#correct_button")[0].style.display = "none";
	$("#answer_button")[0].style.display = "block";
	log(INFO,"Cleared dialog.");
	log(INFO,"Cleaning up cloud: " + bare_id);
	clean_up_cloud_quickly(bare_id,guess,"game_input");
	unfreeze_wind();
	normal_returnkey_mode();
    } else {
	log(INFO,"Sorry, keep trying.");
	$("#game_input").val("");	
	$("#game_input").focus();
    }
}

function correction_dialog(question_lca_text,question_text,correct_answer,bare_id) {
    $("#correction_dialog")[0].style.display = "block";
    $("#correct_button")[0].style.display = "block";
    $("#answer_button")[0].style.display = "none";
    $("#correction_input").val("");
    $("#correction_input").focus();
    $("#correction_bare_id").val(bare_id);
    $("#cd_lca").html(question_lca_text);
    $("#correct_answer").html(correct_answer);
}

function correct_user(cloud) {
    $("#game_input").val("");	
    $("#game_input").focus();
    correction_returnkey_mode();
    log(INFO,"correcting user on cloud: " + cloud.id);
    freeze_wind();
    // get the bare id (just an integer), so that we can manipulate related DOM elements.
    var re = /cloud_([^_]+)/;
    bare_id = cloud.id.replace(re,"$1");
    var answer_text = $("#cloud_" + bare_id + "_a").val();
    var question_lca_text = $("#lca_" + bare_id).text();
    var question_text = $("#question_" + bare_id).text();
    correction_dialog(question_lca_text,question_text,answer_text,bare_id);
}

function blow_cloud(cloud) {
    var cloud_left= parseFloat(cloud.style.left.replace('%',''));
    var cloud_id = cloud.id;
    cloud.style.left = (cloud_left + cloud_speeds[cloud_id])+"%";

    if (cloud_left > 90) {
	if (cloud.getAttribute("class").match(/solved/)) {
	    log(INFO,"You solved this one, no correction needed..");
	} else {
	    correct_user(cloud);

	    // slow down: user is struggling.
	    if (current_speed_limit > min_speed) {
		current_speed_limit--;
	    }
	    log(INFO,"After missing one, your current speed is: " + current_speed_limit);
	}
    }

    var cloud_q_left_offset = 2;
    var cloud_q = $("#" + cloud_id + "_q")[0];
    log(TRACE,"cloud q object: " + cloud_q);
    if (cloud_q.style != undefined) {
	cloud_left = parseFloat(cloud.style.left.replace('%',''));
	cloud_q.style.left = (cloud_left+cloud_q_left_offset) + "%";
    }

    if (cloud_speeds[cloud_id] < 0) {
	cloud_speeds[cloud_id] = 0.1;
	return;
    }

    if (cloud_speeds[cloud_id] > 5) {
	cloud_speeds[cloud_id] = 5;
	return;
    }

    var incr = Math.floor(Math.random()*100);

    if (incr < 5) {
        cloud_speeds[cloud_id] = cloud_speeds[cloud_id] - 0.1;
	log(DEBUG,"cloud " + cloud_id + " slowed down to: " + cloud_speeds[cloud_id]);
    } else {
        if (incr < current_speed_limit) {
	    cloud_speeds[cloud_id] = cloud_speeds[cloud_id] + 0.05;
	    log(DEBUG,"cloud " + cloud_id + " sped up to: " + cloud_speeds[cloud_id]);
        }
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

function clean_up_cloud(bare_id,answer_text,form_input_id) {
    // TODO: make 'solved' lightgrey.
    $("#cloud_" + bare_id)[0].style.color = "lightgrey";
    $("#cloud_" + bare_id).addClass("solved");
    $("#question_" + bare_id).remove();
    $("#answer_" + bare_id).html(answer_text);

    $("#cloud_" + bare_id).fadeOut(1000,function () {$("#cloud_" + bare_id).remove();});
    $("#cloud_" + bare_id + "_q").fadeOut(2000,function () {$("#cloud_" + bare_id + "_a").remove();});
    $("#"+form_input_id).focus();
    add_clouds(1);
}

function clean_up_cloud_quickly(bare_id,answer_text,form_input_id) {
    $("#cloud_" + bare_id).remove();
    $("#cloud_" + bare_id + "_q").remove();
    $("#"+form_input_id).focus();
    add_clouds(1);
}

// Submits the user's guess and look at the available questions to see if the user
// got one of them right. It's possible that the user's guess might be correct for more
// than one question, but only one question may be solved by a single guess.
// TODO: pass the answers in as a javascript array rather than having to parse
// them from the HTML input value.
function submit_game_response(form_input_id) {
    var guess = $("#"+form_input_id).val();

    log(DEBUG,"submit_game_response(): " + guess);
    var matched = false;

    // try all of the possible remaining questions, as represented by the
    // set of class='cloud_answer' DOM elements; stopping at the first one.
    var matched_q = $(".cloud_answer").map(function(answer) {
	if (matched == true) {
	    $("#"+form_input_id).focus();
	    return false;
	}
	answer = $(".cloud_answer")[answer];
	log(DEBUG,"Answer: " + answer);

	var answer_id = answer.id;	    
	// get the bare id (just an integer), so that we can manipulate related DOM elements.
	var re = /cloud_([^_]+)_a/;
	bare_id = answer_id.replace(re,"$1");
	var cloud = $("#cloud_" + bare_id)[0];
	log(INFO,"The cloud: " + cloud);
	var classes = cloud.getAttribute("class")
	log(INFO,"The classes: " + classes);
	solved = classes.match(/solved/);
	if (solved != null) {
	    $("#"+form_input_id).focus();
	    return false;
	}

	// A given question may have more than one possible right answer, separated by commmas.
	var answers = answer.value.split(",");
	log(DEBUG,"Answers: " + answers);

	var i;
	for (i = 0; i < answers.length; i++) {
	    var answer_text = answers[i];
	    log(DEBUG,"answer_text is:: " + answer_text);
	    log(DEBUG,"checking guess: " + guess + " against answer: " + answer_text);
	    if ((answer_text === guess) && (solved == null)) {
		matched = true;
		log(INFO,"You got one right!");
		if (current_speed_limit < max_speed) {
		    current_speed_limit += 1;
		}
		log(INFO,"After getting one, your current speed is: " + current_speed_limit);
		log(DEBUG,"Max speed: " + current_speed_limit);
		var answer_id = answer.id;
		$("#"+form_input_id).val("");	
		clean_up_cloud(bare_id,answer_text,form_input_id);
		return false;
	    }
	}
	// no matches if we got here.
	log(WARN,"Your guess: '" + guess + "' did not match any answers.");
	$("#"+form_input_id).focus();
	$("#"+form_input_id).val("");
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
