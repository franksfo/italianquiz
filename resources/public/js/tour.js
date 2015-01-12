// Begin configurable section.

// latitude and longitude paths of tour through Napoli
var tour_path = [
    [40.8526231,14.2722163],  // Napoli Centrali train station
    [40.85318758861975,14.271989576518536],
    [40.853398582562534,14.27162479609251], // TODO bear north
    [40.854177631299656,14.271528236567972],
    [40.854128941021976,14.270348064601421],
    [40.85401533023488,14.269661419093609], // TODO bear south
    [40.85345538850926,14.269779436290264], // TODO bear south
    [40.85266010082301,14.269639961421488],
    [40.85255460275972,14.268921129405499],
    [40.85258706372015,14.268234483897686],
    [40.85262763989835,14.267622940242289],
    [40.85294413323541,14.266914837062359],
    [40.85305774585944,14.266678802669047],
    [40.85330120082628,14.266238920390606],
    [40.85278182914886,14.266238920390606],
    [40.8527737139341,14.265681020915508],
    [40.85270879218017,14.264747612178324],
    [40.85243287401638,14.263782016932964],
    [40.85296036362221,14.26338504999876],
    [40.852676331279376,14.262430183589458],
    [40.85244098927289,14.261732809245586],
    [40.852205646430455,14.260971061885357],
    [40.85191349553206,14.260091297328472],
    [40.851694381512836,14.259351007640362],
    [40.851377882205846,14.258503429591656],
    [40.851012688819125,14.257452003657818],
    [40.85086661090081,14.256958477199078],
    [40.85066372436894,14.256336204707623],
    [40.850412144206636,14.255660288035868],
    [40.850266064964366,14.255231134593487],
    [40.850144332016434,14.254834167659283],
    [40.85001448329216,14.2544050142169],
    [40.85066372436894,14.254018776118754], // TODO bear north
    [40.85128861289717,14.253675453364849], // TODO bear north
    [40.851596997271706,14.253439418971539],
    [40.85132107447789,14.252570383250713],
    [40.851183112650055,14.251754991710184],
    [40.852027109923384,14.251636974513529],
    [40.8527980595754,14.251583330333233],
    [40.85322816443017,14.251540414988995] // Museo Archeologico Nazionale

];

var encouragements = [
    "Bene!",
    "Certo!",
    "Così mi piace!",
    "Fantastico..",
    "Ottimo"
    ]

var current_lat = tour_path[0][0];
var current_long = tour_path[0][1];
var current_zoom = 17;
var heading = 270; // headed west.

// every X milliseconds, decrement remaining time to answer this question on a tour.
var tour_question_decrement_interval = 5000;

var logging_level = DEBUG;

var step = 0;
var direction = 1;
var map;
var marker;

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
	.bindPopup("<b>Benvenuto a Napoli!</b>").openPopup();
    
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
    
    normal_returnkey_mode();
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

    // We use this function as the callback after we generate a question, so that
    // the answer is a function of the question. that is, we generate a question,
    // then generate the correct possible answers. The server will reply with all of 
    // the correct answers, but for most games, the user needs only to respond with one of them.
    // The server's set of correct answers are stored in the global answer_info variable.
    //
    // We evaluate the user's guess against this set in submit_user_guess().

    update_tour_answer_fn = function(content) {
	answer_info  = jQuery.parseJSON(content);
	$("#correctanswer").html(answer_info.answer.split(",")[0]);
    }

    update_tour_question = function (content) {
	var evaluated = jQuery.parseJSON(content);
	var question = evaluated.full_question;
	log(INFO,"Updating tour with question:" + question);
	$("#tourquestion").html(question);

	$.ajax({
	    cache: false,
	    dataType: "html",
	    url: "/tour/generate-answers?semantics=" + encodeURIComponent(JSON.stringify(evaluated.semantics)),
	    success: update_tour_answer_fn
	});
    }

    // generate a question by calling /tour/generate-question on the server.
    // The server's response to this causes the above update_tour_question() to be
    // executed here in the client's Javascript interpreter, which in turn causes
    // the client to make a call to the server for /tour/generate-answers.
    $.ajax({
	cache: false,
        dataType: "html",
        url: "/tour/generate-question",
        success: update_tour_question
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
	
	if (answer_text === guess) {
	    log(INFO,"You got one right!");
	    update_map($("#tourquestion").html(), guess);
	    $("#userprogress").html("");
	    $("#userprogress").css("background","");
	    $("#userprogress").css("color","");

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
function normal_returnkey_mode() {
    $("#gameinput").keyup(function(event){
	log(INFO,"You hit the key: " + event.keyCode);
	if (event.keyCode == 13){
	    log(INFO,"It's time to try your normal-mode excellent guess: " + $("#gameinput").val());
            $("#answer_button").click();
	} else {
	    log(INFO,"updating your progress - so far you are at: " + $("#gameinput").val());
	    
	    /* update the feedback box so users know how they are doing */
	    var common = common_prefix($("#gameinput").val(),$("#correctanswer").html());
	    $("#userprogress").html(common);
	    if (common == $("#correctanswer").html()) {
		/* user got it right - 'flash' their answer and click the butotn for them. */
		$("#userprogress").css("background","lightblue");
		$("#userprogress").css("color","black");

		setTimeout(function(){
		    $("#answer_button").click();
		}, 1000);

	    }
	}
    });
}

function common_prefix(user_guess,correct_answer) {
    var i;
    var prefix = "";

    user_guess = user_guess.trim();
    correct_answer = correct_answer.trim();
    for (i = 0; i < user_guess.length; i++) {
	if (user_guess[i] == correct_answer[i]) {
	    prefix = prefix + user_guess[i];
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


    if (step >= (tour_path.length - 1)) {
	step = tour_path.length - 1;
	direction = -1;
	heading = 90; // headed east.
    } else {
	if (step <= 0) {
	    step = 0;
	    direction = 1;
	    heading = 270; // headed west.
	}
    }

    step = step + direction;
    current_lat = tour_path[step][0];
    current_long = tour_path[step][1];

    map.panTo(tour_path[step]);
   
    // update the marker too:
    marker.setLatLng(tour_path[step]);
    var encouragement = Math.floor(Math.random()*encouragements.length);
    marker.setPopupContent("<b>" + encouragements[encouragement] + 
			   "</b> " + step + "/" + tour_path.length);

    // update streetview:
    $("#streetviewimage").attr("src","https://maps.googleapis.com/maps/api/streetview?size=400x400&location="+current_lat+","+current_long+"&fov=90&heading="+heading+"&pitch=10");

}
