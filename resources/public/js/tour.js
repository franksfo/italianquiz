// Begin configurable section.

var scope = 45; // how much to show from left to right in tryptich.

// latitude and longitude paths of tour through Firenze.
var Firenze = [
[43.775535,11.248898],
[43.775537,11.249122],
[43.775424,11.249213],
[43.775264,11.249637],
[43.775083,11.250322],
[43.774725,11.250225],
[43.774432,11.250526],
[43.773976,11.251197],
[43.773583,11.251905],
[43.773434,11.252492],
[43.773438,11.252675],
[43.773437,11.255007],
[43.773293,11.255443],
[43.773125,11.255433],
[43.773125,11.255433],
[43.772913,11.25504],
[43.772962,11.254787],
[43.772813,11.254568],
[43.772474,11.254442],
[43.7719,11.254427],
[43.771667,11.254418],
[43.771167,11.254392],
[43.770543,11.254395],
[43.769915,11.254388],
[43.769647,11.254366],
[43.76958,11.254328],
[43.769569,11.254636],
[43.769438,11.255171],
[43.76938,11.255764],
[43.768931,11.255849],
[43.76866,11.255725],
[43.768378,11.255544],
[43.76759,11.254992],
[43.76759,11.254992],
[43.768177,11.254134],
[43.768353,11.253685],
[43.768495,11.253531],
[43.768495,11.253531],
[43.768407,11.253471],
[43.768307,11.253362],
[43.768031,11.253189],
[43.767785,11.253002],
[43.767329,11.252667],
[43.767413,11.252713],
[43.76751,11.252666],
[43.76761,11.252331],
[43.767804,11.25149],
[43.767999,11.250636],
[43.768127,11.249987],
[43.768139,11.249584],
[43.768116,11.249741],
[43.768501,11.249973],
[43.768577,11.249752],
[43.76881,11.249134],
[43.769005,11.2487],
[43.769364,11.247616],
[43.769594,11.246837],
[43.769594,11.246837],
[43.76984,11.24702],
[43.770268,11.247337],
[43.770736,11.247684],
[43.770934,11.247933],
[43.771202,11.247809],
[43.771334,11.247944],
[43.772086,11.248715],
[43.772762,11.249423],
[43.772901,11.249383],
[43.773552,11.248385],
[43.77384,11.247966],
[43.774799,11.246618],
[43.775238,11.246001],
[43.775933,11.245032],
[43.776063,11.244851],
[43.776118,11.244964],
[43.776592,11.246022],
[43.776593,11.246187],
[43.776448,11.246372],
[43.776017,11.24692],
[43.775521,11.247414],
[43.775308,11.248022]
];

// latitude and longitude paths of tour through Napoli
var Napoli = [
    [40.8526231,         14.2722163],  // Napoli Centrali train station
    [40.85318758861975,  14.271989576518536],
    [40.853398582562534, 14.27162479609251],
    [40.854177631299656, 14.271528236567972],
    [40.854128941021976, 14.270348064601421],
    [40.85401533023488,  14.269661419093609],
    [40.85345538850926,  14.269779436290264],
    [40.85266010082301,  14.269639961421488],
    [40.85255460275972,  14.268921129405499],
    [40.85258706372015,  14.268234483897686],
    [40.85262763989835,  14.267622940242289],
    [40.85294413323541,  14.266914837062359],
    [40.85305774585944,  14.266678802669047],
    [40.85330120082628,  14.266238920390606],
    [40.85278182914886,  14.266238920390606],
    [40.8527737139341,   14.265681020915508],
    [40.85270879218017,  14.264747612178324],
    [40.85243287401638,  14.263782016932964],
    [40.85296036362221,  14.26338504999876],
    [40.852676331279376, 14.262430183589458],
    [40.85244098927289,  14.261732809245586],
    [40.852205646430455, 14.260971061885357],
    [40.85191349553206,  14.260091297328472],
    [40.851694381512836, 14.259351007640362],
    [40.851377882205846, 14.258503429591656],
    [40.851012688819125, 14.257452003657818],
    [40.85086661090081,  14.256958477199078],
    [40.85066372436894,  14.256336204707623],
    [40.850412144206636, 14.255660288035868],
    [40.850266064964366, 14.255231134593487],
    [40.850144332016434, 14.254834167659283],
    [40.85001448329216,  14.2544050142169],
    [40.85066372436894,  14.254018776118754],
    [40.85128861289717,  14.253675453364849],
    [40.851596997271706, 14.253439418971539],
    [40.85132107447789,  14.252570383250713],
    [40.851183112650055, 14.251754991710184],
    [40.852027109923384, 14.251636974513529],
    [40.8527980595754,   14.251583330333233],
    [40.85322816443017,  14.251540414988995] // Museo Archeologico Nazionale
];

var tour_path = Firenze;

var encouragements = [
    "Bene!",
    "Certo!",
    "Così mi piace!",
    "Fantastico..",
    "Ottimo"
    ]

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

function start_tour() {
    map = L.map('map').setView([current_lat, current_long], current_zoom);

    L.tileLayer('https://{s}.tiles.mapbox.com/v3/{id}/{z}/{x}/{y}.png', {
	maxZoom: 18,
	attribution: 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, ' +
	    '<a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, ' +
	    'Imagery © <a href="http://mapbox.com">Mapbox</a>',
	id: 'examples.map-i875mjb7'
    }).addTo(map);
    
    
    marker = L.marker([current_lat, current_long]).addTo(map)
	.bindPopup("<b>Benvenuto!</b>").openPopup();
    
    L.circle([current_lat, 
	      current_long], 10, {
	color: 'lightblue',
	fillColor: 'green',
	fillOpacity: 0.5
    }).addTo(map).bindPopup("start")

    var popup = L.popup();
    
    function onMapClick(e) {
	popup
	    .setLatLng(e.latlng)
	    .setContent("[" + e.latlng.lat + "," + e.latlng.lng + "]")
	    .openOn(map);
	$("#streetviewimage").attr("src","https://maps.googleapis.com/maps/api/streetview?size=500x500&location="+e.latlng.lat+","+e.latlng.lng+"&fov=90&heading="+heading+"&pitch=10");

    }
    
    map.on('click', onMapClick);

    // update streetview:
    $("#streetviewimage").attr("src","https://maps.googleapis.com/maps/api/streetview?size=400x400&location="+current_lat+","+current_long+"&fov=90&heading="+heading+"&pitch=10");

    var heading_left = heading - scope;
   $("#streetviewimageleft").attr("src","https://maps.googleapis.com/maps/api/streetview?size=400x400&location="+current_lat+","+current_long+"&fov=90&heading="+heading_left+"&pitch=10");

    var heading_right = heading + scope;
   $("#streetviewimageright").attr("src","https://maps.googleapis.com/maps/api/streetview?size=400x400&location="+current_lat+","+current_long+"&fov=90&heading="+heading_right+"&pitch=10");


    $("#heading").val(heading);
    $("#lat").val(current_lat);
    $("#long").val(current_long);
    
    $("#lat1").val(tour_path[step+1][0]);
    $("#long1").val(tour_path[step+1][1]);
    
    user_keypress();
    tour_loop();
}

function tour_loop() {
    create_tour_question();
    $("#gameinput").focus();
    $("#gameinput").val("");
    
    // TODO: when timeout expires, pop up correction dialog: currently we don't do anything here.
    setInterval(function() {
	decrement_remaining_tour_question_time();
    },tour_question_decrement_interval);
}

var answer_info = {};

function create_tour_question() {

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
	$.each(q_and_a.targets, function(index,value) {
	    $("#answer"+i").html(value);
	});

    }

    // generate a question by calling /tour/generate-question on the server.
    // The server's response to this causes the above update_tour_question() to be
    // executed here in the client's Javascript interpreter, which in turn causes
    // the client to make a call to the server for /tour/generate-answers.
    $.ajax({
	cache: false,
        dataType: "html",
        url: "/tour/generate-q-and-a",
        success: update_tour_q_and_a
    });
}

function decrement_remaining_tour_question_time() {
    log(INFO,"decrement remaining time..");
}

function submit_user_guess(form_input_id) {
    var guess = $("#"+form_input_id).val();
    guess = guess.trim();
    log(INFO,"submit_user_guess() guess: " + guess);

    log(INFO,"ANSWER INFO: " + answer_info.answer);
    var matched = false;
    // A given question may have more than one possible right answer, separated by commas.
    // TODO: server should use a javascript array rather than embedding an array within a string
    // as is the case currently.
    var answers = answer_info.answer.split(",");

    var i;
    for (i = 0; i < answers.length; i++) {
	var answer_text = answers[i];
	answer_text = answer_text.trim();
	log(INFO,"Comparing: '" + answer_text + "' with your guess: '" + guess + "'");
	
	if (answer_text.toLowerCase() === guess.toLowerCase() && answer_text != '') {
	    log(INFO,"You got one right!");
	    update_map($("#tourquestion").html(), guess);
	    $("#gameinput").html("");
	    $("#gameinput").css("background","transparent");
	    $("#gameinput").css("color","lightblue");

	    increment_map_score(); // here the 'score' is in kilometri (distance traveled)
	    // TODO: score should vary depending on the next 'leg' of the trip.
	    // go to next question.
	    return tour_loop();
	}
    }
    log(INFO, "Your guess: '" + guess + "' did not match any answers, unfortunately.");

    return false;
}

function increment_map_score() {
    var score = 100;
    $("#scorevalue").html(parseInt($("#scorevalue").html()) + score);
}

// http://stackoverflow.com/questions/155188/trigger-a-button-click-with-javascript-on-the-enter-key-in-a-text-box
function user_keypress() {
    $("#gameinput").keyup(function(event){
	log(INFO,"You hit the key: " + event.keyCode);
	log(INFO,"updating your progress - so far you are at: " + $("#gameinput").val());
	    
	/* update the feedback box so users know how they are doing */
	if ($("#correctanswer").html().length > 0) {
	    var common = common_prefix($("#gameinput").val(),$("#correctanswer").html());
	    $("#gameinput").val(common);

	    log(INFO,"common length: " + common.trim().length);
	    log(INFO,"answer length: " + $("#correctanswer").html().length);

	    var percent = (common.trim().length / $("#correctanswer").html().length) * 100;

	    log(INFO,"percent: " + percent);
	    
	    $("#userprogress").css("width",percent+"%");
	    
	    if ((common.trim() != '') && (common.toLowerCase() == $("#correctanswer").html().toLowerCase())) {
		/* user got it right - 'flash' their answer and submit the answer for them. */
		$("#gameinput").css("background","lime");

		setTimeout(function(){
	            submit_user_guess('gameinput');
		    // reset userprogress bar
		    $("#userprogress").css("width","0");
		}, 1000);
	    }
	} else {
	    log(WARN,"No answer from server yet, or server answer was empty.");
	}
    });
}

function common_prefix(user_guess,correct_answer) {
    var i;
    var prefix = "";
    var case_sensitive_correct_answer = correct_answer;
    user_guess = user_guess.toLowerCase();
    correct_answer = correct_answer.trim().toLowerCase();
    for (i = 0; i < user_guess.length; i++) {
	var user_char = user_guess[i];
	var correct_char = correct_answer[i];
	if (user_char == correct_char) {
	    prefix = prefix + case_sensitive_correct_answer[i];
	} else {
	    break;
	}
    }
    return prefix;
}

function update_map(question,correct_answer) {    
    L.circle([current_lat, 
	      current_long], 10, {
	color: 'lightblue',
	fillColor: 'green',
	fillOpacity: 0.5
    }).addTo(map).bindPopup(question + " &rarr; <i>" + correct_answer + "</i><br/>" + "<tt>["+current_lat+","+current_long+"]</tt>")

    heading = get_heading(tour_path,step);
    step = step + direction;
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
    var encouragement = Math.floor(Math.random()*encouragements.length);
    marker.setPopupContent("<b>" + encouragements[encouragement] + 
			   "</b> " + step + "/" + tour_path.length);

    // update streetview:
    $("#streetviewimage").attr("src","https://maps.googleapis.com/maps/api/streetview?size=400x400&location="+current_lat+","+current_long+"&fov=90&heading="+heading+"&pitch=10");

    var heading_left = heading - scope;
   $("#streetviewimageleft").attr("src","https://maps.googleapis.com/maps/api/streetview?size=400x400&location="+current_lat+","+current_long+"&fov=90&heading="+heading_left+"&pitch=10");

    var heading_right = heading + scope;
   $("#streetviewimageright").attr("src","https://maps.googleapis.com/maps/api/streetview?size=400x400&location="+current_lat+","+current_long+"&fov=90&heading="+heading_right+"&pitch=10");


}
