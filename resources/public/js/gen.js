var logging_level = INFO;

/* this iterates through all verbs on the page (each verb has class "gen_source"), 
   calling gen_from_verb() for each. */
function gen_per_verb() {
    $(".gen_source").each(function() {
	var verb_dom_id = this.id;
	log(INFO,"verb_dom_id: " + verb_dom_id);
	var verb = verb_dom_id.replace(/^verb_/,"");
	log(INFO,"verb:" + verb);
	gen_from_verb(verb);
    });
}

function refresh_verb(verb) {
    $("#verb_"+verb).html("<i class='fa fa-spinner fa-spin'> </i>");
    $("#english_verb_"+verb).html("<i class='fa fa-spinner fa-spin'> </i>");
    $("#english_translation_"+verb).html("<i class='fa fa-spinner fa-spin'> </i>");
    gen_from_verb(verb);
}

function gen_from_verb(verb) {
    function generate_with_verb(content) {
	var evaluated = jQuery.parseJSON(content);
	var example = evaluated.it;
	var pred = evaluated.pred;
	var semantics = evaluated.semantics;
	var response = example;
	if (example == "") {
	    response = "<i class='fa fa-times-circle'> </i>";
	}
	$("#verb_"+verb).html("<a href='/engine/generate?pred="+verb+"&lang=it'>" + response + "</a>");
	
	// reload link:
	$("#reload_"+verb).attr("onclick","javascript:refresh_verb('"+verb+"');");

	function translate_verb(content) {
	    evaluated = jQuery.parseJSON(content);
	    if (evaluated.response == "") {
		// could not translate: show a link with an error icon (fa-times-circle)
		$("#english_verb_"+pred).html("<a href='/engine/lookup?lang=en&spec=" + 
					      encodeURIComponent(JSON.stringify({"synsem": {"cat": "verb",
											    "sem": {"pred": pred},
											    "infl": "infinitive"}})) + "'>" +
					      "<i class='fa fa-times-circle'> </i>" + " </a>");

	    } else {
		$("#english_verb_"+pred).html(evaluated.en);
	    }

	    var generate_semantics_url = "/engine/generate-from-semantics?model=en&semantics=" + encodeURIComponent(JSON.stringify(semantics));

	    $.ajax({
		cache: false,
		dataType: "html",
		url: generate_semantics_url,
		success: function translate(content) {
		    var evaluated  = jQuery.parseJSON(content);
		    var response = evaluated.response;
		    if (response == "") {
			// could not generate anything: show a link with an error icon (fa-times-circle)
			response = "<i class='fa fa-times-circle'> </i>";
		    }
		    $("#english_translation_"+pred).html("<a href='" + generate_semantics_url + "'>" + response + "</a>");
		}
	    });
	}

	$.ajax({
	    cache: false,
	    dataType: "html",
	    url: "/engine/lookup?lang=en&spec=" + encodeURIComponent(JSON.stringify({"synsem": {"cat": "verb",
												"sem": {"pred": pred},
												"infl": "infinitive"}})),
	    success: translate_verb
	});

    }

    $.ajax({
	cache: false,
	dataType: "html",
	url: "/engine/generate?pred="+verb+"&lang=it",
	success: generate_with_verb
    });
}


