// Begin configurable section.

var scope = 45; // how much to show from left to right in tryptich.

function get_quadrant(path,step) {
    var lat0 = path[step][0];
    var lat1 = path[step+1][0];

    var long0 = path[step][1];
    var long1 = path[step+1][1];

    var quadrant = 0;

    if ((lat1 >= lat0) && (long1 >= long0)) {
	log(INFO,"NORTHEAST: " + long1 + " => " + long0);
	quadrant = 0;
    }

    if ((lat1 >= lat0) && (long1 < long0)) {
	log(INFO,"NORTHWEST: " + long1 + " < " + long0);
	quadrant = 3;
    }

    if ((lat1 < lat0) && (long1 >= long0)) {
	log(INFO,"SOUTHEAST");
	quadrant = 1;
    }

    if ((lat1 < lat0) && (long1 < long0)) {
	log(INFO,"SOUTHWEST");
	quadrant = 2;
    }
    $("#quadrant").val(quadrant);
    return quadrant;
}

function get_heading(path,position_index) {
    var quadrant = get_quadrant(path,position_index);

    var lat0 = tour_path[position_index][0];
    var long0 = tour_path[position_index][0];

    var lat1 = tour_path[position_index+1][0];
    var long1 = tour_path[position_index+1][0];

    // lat1 > lat0: you are headed north.
    // lat1 < lat0: you are headed south.
//    var delta_x = Math.abs(lat0 - lat1);
    var delta_x = lat0 - lat1;

    // long1 > long0: you are headed east 
    // long1 < long0: you are headed west. 

//    var delta_y = Math.abs(long0 - long0);
    var delta_y = long0 - long0;

    var offset =  Math.abs((Math.atan2(delta_x,delta_y))) * (180/Math.PI);
    //var heading = offset + (90 * quadrant);
    var heading = (90 * quadrant) + 45;

    $("#offset").val(offset);

    return heading;
}

// every X milliseconds, decrement remaining time to answer this question on a tour.
var tour_question_decrement_interval = 5000;

var logging_level = DEBUG;

var step = 0;
var direction = 1;
var map;
var marker;

var current_lat = tour_path[step][0];
var current_long = tour_path[step][1];
var quadrant = get_quadrant(tour_path,step);
var heading = get_heading(tour_path,0);
var current_zoom = 17;

function start_tour(target_language) {
    map = L.map('map').setView([current_lat, current_long], current_zoom);

    L.tileLayer('https://{s}.tiles.mapbox.com/v3/{id}/{z}/{x}/{y}.png', {
	maxZoom: 18,
	attribution: 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, ' +
	    '<a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, ' +
	    'Imagery © <a href="http://mapbox.com">Mapbox</a>',
	id: 'examples.map-i875mjb7'
    }).addTo(map);
    
    
    if (target_language == "it") {
	marker = L.marker([current_lat, current_long]).addTo(map)
	    .bindPopup("<b>Benvenuti!</b>").openPopup();
    }
    if (target_language == "es") {
	marker = L.marker([current_lat, current_long]).addTo(map)
	    .bindPopup("<b>Bienvenidos!</b>").openPopup();
    }
    
    L.circle([current_lat, 
	      current_long], 10, {
	color: 'lightblue',
	fillColor: 'green',
	fillOpacity: 0.5
    }).addTo(map).bindPopup("start")

    var popup = L.popup();

    // set streetview to the first location we are at:
    $("#streetviewimage").attr("src","https://maps.googleapis.com/maps/api/streetview?size=275x275&location="+current_lat+","+current_long+"&fov=90&heading="+heading+"&pitch=10");

    $("#heading").val(heading);
    $("#lat").val(current_lat);
    $("#long").val(current_long);
    
    $("#lat1").val(tour_path[step+1][0]);
    $("#long1").val(tour_path[step+1][1]);
    
    user_keypress(target_language);
    tour_loop(target_language);
}

function tour_loop(target_language) {
    create_tour_question(target_language);
    $("#gameinput").focus();
    $("#gameinput").val("");
    
    // TODO: when timeout expires, pop up correction dialog: currently we don't do anything here.
    setInterval(function() {
	decrement_remaining_tour_question_time();
    },tour_question_decrement_interval);
}

var answer_info = {};
var correct_answers = [];

function create_tour_question(target_language) {

    $("#gameinput").css("background","white");
    $("#gameinput").css("color","black");

    // We use this function as the callback after we 
    // generate a question-and-answers pair.
    update_tour_q_and_a = function (content) {
	// question is .source; answers are in .targets.
	var q_and_a = jQuery.parseJSON(content);
	var question = q_and_a.source;
	log(INFO,"Updating tour with question:" + question);
	$("#tourquestion").html(question);

	// update answers with all targets.
	var i=0;
	$("#correctanswer").html("");

	log(DEBUG,"TARGETS ARE: " + q_and_a.targets);
	correct_answers = q_and_a.targets;

	$.each(q_and_a.targets, function(index,value) {
	    log(DEBUG,"TARGET INDEX IS: " + index);
	    log(DEBUG,"TARGET VALUE IS: " + value);
	    $("#correctanswer").append("<div id='answer_"+i+"'>" + value + "</div>");
	    i++;
	});
    }

    // generate a question by calling /tour/<language>/generate-q-and-a on the server.
    $.ajax({
	cache: false,
        dataType: "html",
        url: "/tour/" + target_language + "/generate-q-and-a",
        success: update_tour_q_and_a
    });
}

function decrement_remaining_tour_question_time() {
    log(DEBUG,"decrement remaining time..");
}

function submit_user_guess(guess,correct_answer,target_language) {
    log(INFO,"submit_user_guess() guess: " + guess);
    if (guess == correct_answer) {
	log(INFO,"You got one right!");
	update_map($("#tourquestion").html(), guess);
	$("#gameinput").html("");
	$("#gameinput").css("background","transparent");
	$("#gameinput").css("color","lightblue");
	
	increment_map_score(); // here the 'score' is in kilometri (distance traveled)
	// TODO: score should vary depending on the next 'leg' of the trip.
	// go to next question.
	return tour_loop(target_language);
    }
    log(INFO, "Your guess: '" + guess + "' did not match any answers, unfortunately.");
    return false;
}

function longest_prefix_and_correct_answer(user_input,correct_answers) {
    log(INFO,"user input: " + user_input);
    log(INFO,"correct_answers: " + correct_answers);
    var prefix = "";
    var longest_answer = "";
    $.each(correct_answers,function(index,value) {
	var i;
	for (i = 0; i <= user_input.length; i++) {
	    if (value.substring(0,i).toLowerCase() == user_input.substring(0,i).toLowerCase()) {
		if (i > prefix.length) {
		    prefix = value.substring(0,i);
		    longest_answer = value;
		}
	    }
	}
    });
    log(INFO,"longest correct answer: " + longest_answer);
    log(INFO,"prefix: " + prefix);
    return {"prefix":prefix,
	    "correct_answer":longest_answer};
}

function increment_map_score() {
    var score = 100;
    $("#scorevalue").html(parseInt($("#scorevalue").html()) + score);
}

function add_a_grave(the_char,target_language) {
    $("#gameinput").val($("#gameinput").val() + "à");
    update_user_input(target_language);
    $("#gameinput").focus();
}

function add_e_grave(the_char,target_language) {
    $("#gameinput").val($("#gameinput").val() + "è");
    update_user_input(target_language);
    $("#gameinput").focus();
}
function add_n_tilde(the_char,target_language) {
    $("#gameinput").val($("#gameinput").val() + "ñ");
    update_user_input(target_language);
    $("#gameinput").focus();
}
function add_o_grave(the_char,target_language) {
    $("#gameinput").val($("#gameinput").val() + "ò");
    update_user_input(target_language);
    $("#gameinput").focus();
}
function add_u_acute(the_char,target_language) {
    $("#gameinput").val($("#gameinput").val() + "ú");
    update_user_input(target_language);
    $("#gameinput").focus();
}

function update_user_input(target_language) {
    var user_input = $("#gameinput").val();
    log(INFO,"updating your progress - so far you are at: " + user_input);

    // Find the longest common prefix of the user's guess and the set of possible answers.
    // The common prefix might be an empty string (e.g. if user_input is empty before user starts answering).
    var prefix_and_correct_answer = longest_prefix_and_correct_answer(user_input,correct_answers);
    var prefix = prefix_and_correct_answer.prefix;
    var correct_answer = prefix_and_correct_answer.correct_answer;
    log(INFO,"longest prefix: " + prefix);
    log(INFO,"longest correct_answer: " + correct_answer);

    log(INFO,"common prefix: " + prefix.trim());
    log(INFO,"common length: " + prefix.trim().length);

    // update the user's input with this prefix (shared with one or more correct answer).
    $("#gameinput").val(prefix);

    log(INFO,"answer prefix: " + correct_answer);
    log(INFO,"answer length: " + correct_answer.length);

    var max_length = 0;

    // find the length of the longest match between what the possible correct answers and what the user has typed so far.

    var percent = (prefix.length / correct_answer.length) * 100;
	
    log(INFO,"percent: " + percent);
	
    $("#userprogress").css("width",percent+"%");
	
    if ((prefix != '') && (prefix.toLowerCase() == correct_answer.toLowerCase())) {
	/* user got it right - 'flash' their answer and submit the answer for them. */
	$("#gameinput").css("background","lime");
	
	setTimeout(function(){
	    log(INFO,"submitting correct answer: " + correct_answer);
	    submit_user_guess(prefix,correct_answer,target_language);
	    // reset userprogress bar
	    $("#userprogress").css("width","0");
	    }, 1000);
    }
}

// http://stackoverflow.com/questions/155188/trigger-a-button-click-with-javascript-on-the-enter-key-in-a-text-box
function user_keypress(target_language) {
    $("#gameinput").keyup(function(event){
	log(INFO,"You hit the key: " + event.keyCode);
	update_user_input(target_language);
    });
}

function update_map(question,correct_answer) {    
    L.circle([current_lat, 
	      current_long], 10, {
	color: 'lightblue',
	fillColor: 'green',
	fillOpacity: 0.5
    }).addTo(map).bindPopup(question + " &rarr; <i>" + correct_answer + "</i><br/>" + "<tt>["+current_lat+","+current_long+"]</tt>")
    step = step + direction;

    navigate_to(step,true);
}

function non_so() {
    $("#correctanswer").css("display","block");
    $("#correctanswer").fadeOut(3000,function () {$("#correctanswer").css("display","none");});
    if ((direction == 1) && (step > 1)) {
	step = step - 1;
    }
    if (direction == -1) {
	step = step + 1;
    }
    navigate_to(step,false);
    $("#scorevalue").html(parseInt($("#scorevalue").html()) - 100);
    $("#gameinput").focus();
}

function navigate_to(step,do_encouragement) {
    heading = get_heading(tour_path,step);

    current_lat = tour_path[step][0];
    current_long = tour_path[step][1];

    $("#heading").val(heading);
    $("#lat").val(current_lat);
    $("#long").val(current_long);

    $("#lat1").val(tour_path[step+1][0]);
    $("#long1").val(tour_path[step+1][1]);

    get_quadrant(tour_path,step);

    map.panTo(tour_path[step]);
   
    // update the marker too:
    marker.setLatLng(tour_path[step]);
    if (do_encouragement == true) {
	var encouragement = Math.floor(Math.random()*encouragements.length);
	marker.setPopupContent("<b>" + encouragements[encouragement] + 
			       "</b> " + step + "/" + tour_path.length);
    }

    // update streetview:
    $("#streetviewimage").attr("src","https://maps.googleapis.com/maps/api/streetview?size=275x275&location="+current_lat+","+current_long+"&fov=90&heading="+heading+"&pitch=10");

}
