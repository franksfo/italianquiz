// Begin configurable section.

var logging_level = INFO;

var fa_cloud = "fa-cloud";

// Other possibilities:
//var fa_cloud = "fa-bicycle";
//var fa_cloud = "fa-fighter-jet";

var background = "white";
var radius = 15;
// TODO: get width and height of #game from DOM, not hardcoded - use client's preferred dimensions.
var game_width = 1000;
var game_height = 500;
var offset=0;
var this_many_questions_at_a_time = 1;
var initial_tree_size = 50; // in pixels: size of smallest tree.

// beginners' maximum speed is 10, but it gets increased when you correctly answer questions.
var current_speed_limit = 10;

var min_speed = 5;
var max_speed = 15;

// how often a droplet falls.
// (rain not used at present)
var rain_time = 1000;
// timer for cloud motion interval, in milliseconds.
// a low blow_time looks smooth but will chow javascript engine CPUs.
var blow_time = 50;

// every X milliseconds, decrement remaining time to answer this question on a tour.
var tour_question_decrement_interval = 5000;

var cloud_ceiling = 20;
var cloud_altitude = function() {
    return cloud_ceiling + Math.floor(Math.random()*70);
}

var in_correction_mode = false;

// TODO: move to css and use classes (e.g. tree0,tree1, etc)
var tree_colors = ["lightgreen","darkgreen","forestgreen","seagreen","mediumseagreen","springgreen","mediumspringgreen","lightseagreen"];

// End of configurable section.

// Begin variable declarations.

// 'freezed' rather than 'frozen' in order to make freeze-related functionality more easily searchable.
var wind_is_freezed = false;

var answer_info = {}; // key: question_id; val: answer information.

// In the DOM is a set of trees: tree_<infinitive_verb>.
// Each tree is created when a question having that infinitive_verb is answered correctly.
// The tree grows bigger as the user answers more correct questions with that infinitive verb.

var global_cloud_id = 0;

var cloud_speeds = {};

// End variable declarations.

// Begin function declarations (until end of file).

function freeze_wind() {
    wind_is_freezed = true;
}

function unfreeze_wind() {
    wind_is_freezed = false;
}

function start_game() {
    normal_returnkey_mode();
    var svg = d3.select("#svgarena");
    add_clouds(this_many_questions_at_a_time);
    
    $("#game_input").focus();
    
    setInterval(function() {
	if (wind_is_freezed == false) {
	    blow_clouds(0);
	} else {
	    log(DEBUG,"WIND IS FROZEN..");

	}
    },blow_time);
}

// TODO: openstreetmaps and googlemaps origins are not equivalent geographically:
// they use different coordinate systems, and so are not easily compared.
var openstreetmaps_origin = "http://www.openstreetmap.org/export/embed.html?bbox=3%2C43.5%2C11.5%2C44&layer=mapnik";

// origin: Napoli centro: https://www.google.com/maps/@40.8526241,14.2671395,18z?hl=en-US
// origin: Napoli centro: https://www.google.com/maps/@40.852624,14.267139,345m/data=!3m1!1e3?hl=en-US
//https://www.google.com/maps?ll=40.852176,14.268379&z=18&t=m&hl=en-US&gl=US&mapclient=embed
var googlemaps_origin = "https://www.google.com/maps?ll=40.851114,14.268615&z=18&t=h&hl=en-US&gl=US&deg=0&mapclient=embed";

var googlemaps_lat_origin  = 40.852176;
var googlemaps_long_origin = 14.268379;
var googlemaps_zoom_origin = 17;

var maps_origin = googlemaps_origin;

var maps_current_lat = googlemaps_lat_origin;
var maps_current_long = googlemaps_long_origin;
var maps_current_zoom = googlemaps_zoom_origin;

function update_map() {
    // how far you can get in one direction in one turn.
    var increment = 0.0005;

    var direction_x = Math.floor(Math.random()*3) - 1; // possible values [-1 0 1]
    var direction_y = Math.floor(Math.random()*3) - 1; // possible-values [-1 0 1]

    var increment_x = direction_x * increment;
    var increment_y = direction_y * increment;

    maps_current_lat = maps_current_lat + increment_x;
    maps_current_lat = maps_current_lat + increment_y;
    
    $("#mapframe").attr('src', create_googlemaps_url(maps_current_lat,
						     maps_current_long,
						     maps_current_zoom));
}

function increment_map_score() {
    var distance = 0.5;
    $("#scorevalue").html(parseInt($("#scorevalue").html()) + distance);
}

function create_googlemaps_url(new_lat,new_long,new_zoom) {
    maps_current_lat = new_lat;
    maps_current_long = new_long;
    maps_current_zoom = new_zoom;
    return "https://maps.google.com/maps?z="+new_zoom+
	"&ll="+new_lat+","+new_long+"&output=embed";
}

function start_tour() {
    $("#mapframe").attr('src',create_googlemaps_url(googlemaps_lat_origin,
						    googlemaps_long_origin,
						    googlemaps_zoom_origin));



    var map = L.map('map').setView([51.505, -0.09], 13);

    L.tileLayer('https://{s}.tiles.mapbox.com/v3/{id}/{z}/{x}/{y}.png', {
	maxZoom: 18,
	attribution: 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, ' +
	    '<a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, ' +
	    'Imagery © <a href="http://mapbox.com">Mapbox</a>',
	id: 'examples.map-i875mjb7'
    }).addTo(map);
    
    
    L.marker([51.5, -0.09]).addTo(map)
	.bindPopup("<b>Hello world!</b><br />I am a popup.").openPopup();

    L.circle([51.508, -0.11], 500, {
	color: 'red',
	fillColor: '#f03',
	fillOpacity: 0.5
    }).addTo(map).bindPopup("I am a circle.");
    
    L.polygon([
	[51.509, -0.08],
	[51.503, -0.06],
	[51.51, -0.047]
    ]).addTo(map).bindPopup("I am a polygon.");
    
    
    var popup = L.popup();
    
    function onMapClick(e) {
	popup
	    .setLatLng(e.latlng)
	    .setContent("You clicked the map at " + e.latlng.toString())
	    .openOn(map);
    }
    
    map.on('click', onMapClick);
    
    normal_returnkey_mode();
    tour_loop();
}

function tour_loop() {
    create_tour_question();
    $("#game_input").focus();
    $("#game_input").val("");
    
    // TODO: when timeout expires, pop up correction dialog: currently we don't do anything here.
    setInterval(function() {
	decrement_remaining_tour_question_time();
    },tour_question_decrement_interval);
}

var answer_info;

function create_tour_question() {

    // We use this function as the callback after we generate a question, so that
    // the answer is a function of the question. that is, we generate a question,
    // then generate the correct possible answers. The server will reply with all of 
    // the correct answers, but for most games, the user needs only to respond with one of them.
    // The server's set of correct answers are stored in the global answer_info variable.
    //
    // We evaluate the user's guess against this set in submit_tour_response().

    update_tour_answer_fn = function(content) {
	var evaluated  = jQuery.parseJSON(content);
	log(INFO,"map from the server's answer response: " + evaluated);
	answer_info = evaluated;
    }

    update_tour_question = function (content) {
	var evaluated = jQuery.parseJSON(content);
	log(INFO,"Updating tour with question:" + evaluated.full_question);
	$("#tourquestion").html(evaluated.full_question);

	$.ajax({
	    cache: false,
	    dataType: "html",
	    url: "/cloud/generate-answers?semantics=" + encodeURIComponent(JSON.stringify(evaluated.semantics)),
	    success: update_tour_answer_fn
	});
    }

    $.ajax({
	cache: false,
        dataType: "html",
        url: "/cloud/generate-question",
        success: update_tour_question
    });
}

function decrement_remaining_tour_question_time() {
    log(INFO,"decrement remaining time..");
}

function add_clouds(add_this_many) {
    var added = 0;
    while (added < add_this_many) {
	add_cloud(global_cloud_id);
	added++;
	global_cloud_id++;
    }
}

function add_cloud(cloud_id) {
    log(INFO,"add_cloud(" + cloud_id + ")");
    var fixed_cloud_size = true
    var size;
    if (fixed_cloud_size == true) {
	size = 4;
    } else {
	size = Math.floor(Math.random()*4) + 1;
    }

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

    $("#sky").append("<div id='cloud_" + cloud_id + "_q' class='cloudq' style='display:none;top: " + word_vertical + "px'>" + "(please standby..)" + 
		     "</div>");
    // TODO: remove gameform as a way of passing information around: use javascript external variables instead.
    $("#gameform").append("<input id='cloud_" + cloud_id + "_a' class='cloud_answer'> </input>");

    // clouds are stopped until answer arrives from server via an asynchronous update.
    cloud_speeds["cloud_" + cloud_id] = undefined;

    update_answer_fn = function(content) {
	evaluated  = jQuery.parseJSON(content);

	// TODO: this is a big hairy mess: don't use the DOM for temporary storage like this.
	log(INFO,"map from the server's <answer> response: " + evaluated);

	var cloud_a_dom_id = "cloud_" + evaluated.cloud_id + "_a";
	var cloud_q_dom_id = "cloud_" + evaluated.cloud_id + "_q";

	// start nice and slow with this question cloud:
	cloud_speeds["cloud_" + evaluated.cloud_id] = Math.random()*0.010;
	// other possibilities:
	//    cloud_speeds["cloud_" + evaluated.cloud_id] = Math.random()*0.001;
	//    cloud_speeds["cloud_" + evaluated.cloud_id] = 0.1;

	var left_context_of_answer_dom_id = "left_context_of_answer_" + evaluated.cloud_id;
	var answer_dom_id = "answer_" + evaluated.cloud_id;
	var rca_dom_id = "rca_" + evaluated.cloud_id;

	log(INFO,"Updating answer input with dom id: " + cloud_a_dom_id);
	// TODO: pass JSON directly rather than using the DOM as a data store.
	// Though the DOM has some advantages in that you can use it for presentation purposes.
	$("#"+cloud_a_dom_id).val(evaluated.answer);
	$("#"+left_context_of_answer_dom_id).html(evaluated.left_context_of_answer);
	$("#"+rca_dom_id).html(evaluated.rca);
	log(DEBUG,"Updating question color for dom id: " + cloud_q_dom_id);
	$("#cloud_"+evaluated.cloud_id).fadeIn(500,function() {
	    $("#"+cloud_q_dom_id).fadeIn(100);
	});

	$("#gameform").append("<div class='answer_info' id='answer_info_" + evaluated.cloud_id + "> " +  content + " </div>");
	answer_info[evaluated.cloud_id] = evaluated;
    }

    update_cloud_fn = function (content) {
	log(DEBUG,"Updating cloud with question content: " + content);
	evaluated = jQuery.parseJSON(content);
	// TODO: avoid munging html like this - it's hard to understand.
        $("#"+cloud_q_dom_id).html("<span class='left_context_of_question question' id='left_context_of_question_"+cloud_id+"'>" + evaluated.left_context_of_question + "</span>" +
				   "<span class='spacing'> </span>" +
				   "<span class='question'                          id='question_"+cloud_id+"'>" + evaluated.question + "</span>" +
				   "<span class='full_question'                     id='fullquestion_"+cloud_id+"'>" + evaluated.full_question + "</span>" +
				   "<span class='spacing'> </span>" +
				   "<span class='left_context_of_answer hidden'     id='left_context_of_answer_"+cloud_id+"'> </span>" +
				   "<span class='spacing'> </span>" +
				   "<span class='answer hidden'                     id='answer_"+cloud_id+"'> </span>" +
				   "<span class='spacing'> </span>" +
				   "<span class='right_context_of_answer hidden'    id='right_context_of_answer_"+cloud_id+"'>" + "" + "</span>");
				   
	log(DEBUG,"Sending request: /cloud/generate-answers?cloud_id="+ cloud_id + "&semantics=" + evaluated.semantics);

	$.ajax({
	    dataType: "html",
	    url: "/cloud/generate-answers?cloud_id="+ cloud_id + "&semantics=" + encodeURIComponent(JSON.stringify(evaluated.semantics)),
	    success: update_answer_fn
	    });
    }

    // fill in the cloud's q in the background.
    $.ajax({
	cache: false,
        dataType: "html",
        url: "/cloud/generate-question",
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

function submit_game_response(form_input_id) {
    if (in_correction_mode) {
	submit_correction_response(form_input_id);
    } else {
	submit_normal_response(form_input_id);
    }
}

function submit_tour_response(form_input_id) {
    if (in_correction_mode) {
	submit_correction_response(form_input_id);
    } else {
	var guess = $("#"+form_input_id).val();
	log(INFO,"submit_tour_response() guess: " + guess);
	var matched = false;

	// A given question may have more than one possible right answer, separated by commas.
	// TODO: server should use a javascript array rather than embedding an array within a string
	// as is the case currently.
	var answers = answer_info.answer.split(",");

	var i;
	for (i = 0; i < answers.length; i++) {
	    var answer_text = answers[i];
	    if (answer_text === guess) {
		log(INFO,"You got one right!");
		update_map();
		increment_map_score(); // here the 'score' is in kilometri (distance traveled)
		// TODO: score should vary depending on the next 'leg' of the trip.
		// go to next question.
		return tour_loop();
	    }
	}
	log(INFO, "Your guess: '" + guess + "' did not match any answers, unfortunately.");
	return false;
    }
}

function grow_tree(question_id) {
    // question with id <question_id> was answered correctly: grow its tree.
    log(INFO,"question_id: " + question_id);
    log(INFO,"answer_info:" + answer_info[question_id]);
    log(INFO,"answer_info group by:" + answer_info[question_id]["group_by"]);
    var group_by = answer_info[question_id]["group_by"];

    // group_by is currently simply the infinitive form of the verb for this question.
    // This means that there will be one tree for all questions with the same infinitive verb:
    // e.g. all questions about "parlare" will grow the "parlare" tree.
    //
    // In the future, group_by may change, e.g. split the possible groups into a larger set such as
    // infinitive+tense. 
    if ((group_by != undefined) && $("#tree_" + group_by)[0]) {
	// grow the existing tree for this group.
	log(INFO,"Growing an existing tree.");
	var tree_dom_id = "#tree_"+group_by[0];
	var tree = $(tree_dom_id)[0];
	
	var existing_font_size = $("#tree_" + group_by).css("font-size");
	log(INFO,"Existing font size(1): " + existing_font_size);
	existing_font_size = existing_font_size.replace(/px/,"");
	log(INFO,"Existing font size(2): " + existing_font_size);
	existing_font_size = parseInt(existing_font_size);
	log(INFO,"Existing font size(3): " + existing_font_size);
	var new_font_size = existing_font_size + 10;
	log(INFO,"New font size: " + new_font_size);
	$("#tree_"+group_by).css({ 'font-size': new_font_size + "px" });

	// we need to make the top lower if the tree grows; otherwise the tree might float in the air (have a top smaller than the ground's top).
	var existing_top = $("#tree_" + group_by).css("top");
	log(INFO,"Existing top(1): " + existing_top);
	log(INFO,"Existing top(2): " + existing_top);
	existing_top = parseInt(existing_top);
	log(INFO,"Existing top(3): " + existing_top);
	var new_top = existing_top - 10;
	log(INFO,"New top: " + new_top);
	$("#tree_"+group_by).css({ 'top': new_top + "px" });
    } else {
	// add a new tree for this group, since it doesn't exist yet.
	var left=Math.floor(Math.random()*80) + 10;
	var top=Math.floor(Math.random()*65) - 20;
	var font_size = initial_tree_size;
	log(INFO,"Planting a new tree with top: " + top);
	$("#ground").append("<i id='tree_" + group_by + "' class='fa fa-tree' style='font-size:" + font_size + "px; left:"+left+"%; top:-"+top+"px'> </i>");	

	color = tree_colors[Math.floor(Math.random()*tree_colors.length)];

	log(INFO,"Tree color: " + color);
	$("#tree_"+group_by).css({'top':top+"px"});
	$("#tree_"+group_by).css({'color':color});
    }
}

// Submits the user's guess and look at the available questions to see if the user
// got one of them right. It's possible that the user's guess might be correct for more
// than one question, but only one question may be solved by a single guess.
// TODO: pass the answers in as a javascript array rather than having to parse
// them from the HTML input value.
function submit_normal_response(form_input_id) {
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
	log(INFO,"Looking for cloud with bare_id: " + bare_id);
	log(INFO,"Answer_id is: " + answer_id);
	var cloud = $("#cloud_" + bare_id)[0];
	if (cloud == undefined) {
	    log(INFO,"cloud with bare_id" + bare_id + " was undefined! giving up on it.");
	    return false;
	}
	var classes = cloud.getAttribute("class")
	log(INFO,"This cloud has the following classes: " + classes);
	solved = classes.match(/solved/);
	if (solved != null) {
	    log(INFO,"This cloud has already been solved; returning false.");
	    $("#"+form_input_id).focus();
	    return false;
	}

	// A given question may have more than one possible right answer, separated by commas.
	// TODO: server should use a javascript array rather than embedding an array within a string
	// as is the case currently.
	var answers = answer.value.split(",");
	log(INFO,"Total answers: " + answers.length);
	log(INFO,"Possible answers: " + answers);

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

		grow_tree(bare_id);

		clean_up_cloud(bare_id,answer_text,form_input_id);

		increment_score(100);

		return false;
	    }
	}
	// no matches if we got here.
	log(WARN,"Your guess: '" + guess + "' did not match any answers.");
	$("#"+form_input_id).focus();
	if (false) { // false: I don't think we want to zero out user's input,
	    // even if it's wrong.
	    $("#"+form_input_id).val("");
	}
    });
}

function increment_score(by) {
    $("#scorevalue").html(parseInt($("#scorevalue").html()) + by);
}

// http://stackoverflow.com/questions/155188/trigger-a-button-click-with-javascript-on-the-enter-key-in-a-text-box
function normal_returnkey_mode() {
    in_correction_mode = false;
    $("#game_input").keyup(function(event){
	if(event.keyCode == 13){
	    if (in_correction_mode == true) {
		log(WARN,"Correction mode is true but unexpectedly the normal keyup function got called. No worries, though; transferring you right over to where you need to be.");
		submit_correction_response("form_input");
		log(INFO,"Finished recovering from getting unexpectedly in normal_returnkey_mode.");
		return;
	    }

	    log(INFO,"It's time to try your normal-mode excellent guess: " + $("#game_input").val());
	    log(INFO,"In correction mode is: " + in_correction_mode);
	    log(INFO,"Correction mode IS FALSE (right?): " + in_correction_mode);
            $("#answer_button").click();
	}
    });
    log(DEBUG,"You are doing great! NORMAL RETURNKEY MODE.");
}

function correction_returnkey_mode() {
    in_correction_mode = true;
    $("#game_input").keyup(function(event){
	if(event.keyCode == 13){
	    log(INFO,"It's time to pick yourself up and dust yourself off with your correction: " + $("#game_input").val());
	    log(INFO,"Correction mode IS TRUE (right?): " + in_correction_mode);
            $("#answer_button").click();
	}
    });
    log(DEBUG,"Sadly you are in need of a more basic kind of learning. CORRECTION RETURNKEY MODE.");
    in_correction_mode = true;
}

function submit_correction_response(form_input_id) {
    var guess = $("#"+form_input_id).val();
    log(DEBUG,"submit_correction_response(): " + guess);

    if (guess === $("#correct_answer").html()) {
	// User is out of the penalty box, yay!
	log(INFO,"Good! you got it right; you can continue with the game.");
	$("#"+form_input_id).val("");	
	var bare_id = $("#correction_bare_id").val();
	log(INFO,"bare_id: " + bare_id);
	log(INFO,"Clearing dialog..");
	$("#correction_dialog")[0].style.display = "none";
	log(INFO,"Cleared dialog.");
	log(INFO,"Cleaning up cloud: " + bare_id);
	clean_up_cloud_quickly(bare_id,guess,"game_input");
	unfreeze_wind();
	normal_returnkey_mode();
	$("#answer_button")[0].innerHTML = "Answer";
    } else {
	log(INFO,"Sorry, keep trying.");
	$("#"+form_input_id).val("");	
	$("#"+form_input_id).focus();
    }
}

function correction_dialog(question_left_context_of_answer_text,question_text,correct_answer,bare_id,full_question) {
    log(INFO,"Popping up the correction_dialog and populating it with stuff.");
    $("#correction_dialog")[0].style.display = "block";
    $("#game_input").focus();
    $("#correction_bare_id").val(bare_id);
    $("#answer_button")[0].innerHTML = "Correct";
    $("#cd_left_context_of_answer").html(question_left_context_of_answer_text + " ");
    var answers = correct_answer;
    var first_correct_answer = answers.split(",")[0];
    $("#correct_answer").html(first_correct_answer);
    $("#full_question").html(full_question);
    log(INFO,"correction_dialog: done populating.");
}

function correct_user(cloud) {
    $("#game_input").focus();
    log(INFO,"switching to correction mode.");
    correction_returnkey_mode();
    log(INFO,"correcting user on cloud: " + cloud.id);
    freeze_wind();
    // get the bare id (just an integer), so that we can manipulate related DOM elements.
    var re = /cloud_([^_]+)/;
    bare_id = cloud.id.replace(re,"$1");
    var answer_text = $("#cloud_" + bare_id + "_a").val();
    var question_left_context_of_answer = $("#left_context_of_answer_" + bare_id).text();
    var question_text = $("#question_" + bare_id).text();
    var full_question = $("#fullquestion_" + bare_id).text();
    log(INFO,"calling correction_dialog() with: " + question_left_context_of_answer + "," +
	question_text + "," +
	answer_text + "," +
	bare_id + "," + 
	full_question);
    correction_dialog(question_left_context_of_answer,question_text,answer_text,bare_id,full_question);
}

function blow_cloud(cloud) {
    var cloud_id = cloud.id;
    if (cloud_speeds[cloud_id] == undefined) {
	log(DEBUG,"Cloud can't blow until it gets an answer.");
	return;
    } else {
	log(DEBUG,"Cloud is ready to blow.");
    }

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
	    log(DEBUG,"After slowing down due to a miss, your current speed is: " + current_speed_limit);
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
    // show left context_of_answer
    $("#left_context_of_answer_" + bare_id).removeClass("hidden");
    $("#right_context_of_answer_" + bare_id).removeClass("hidden");
    $("#answer_" + bare_id).removeClass("hidden");
    $("#question_" + bare_id).addClass("hidden");
    $("#left_context_of_question_" + bare_id).addClass("hidden");
    $("#full_question_" + bare_id).addClass("hidden");

    // TODO: make 'solved' lightgrey in CSS rather than here in Javascript.
    $("#cloud_" + bare_id)[0].style.color = "lightgrey";
    // stop this cloud in its tracks: set speed to 0.
    cloud_speeds["cloud_" + bare_id] = 0;
    $("#cloud_" + bare_id).addClass("solved");
    $("#cloud_" + bare_id + "_a").remove;
    $("#question_" + bare_id).remove();
    var lca = $("#left_context_of_answer_" + bare_id).html();
    if (answer_text.substring(0,lca.length) === lca) {
	$("#left_context_of_answer_" + bare_id).html("");
    }
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
