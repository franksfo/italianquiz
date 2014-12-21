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
	if (example == "") {
	    $("#verb_"+verb).html("<a href='/engine/generate?pred="+verb+"&lang=it'>" + 
				  "<i class='fa fa-times-circle'> </i>" + " </a>");
	} else {
	    $("#verb_"+verb).html(example);
	}

	
	// reload link:
	$("#reload_"+verb).attr("onclick","javascript:refresh_verb('"+verb+"');");


	// hide semantics for now
	//	$("#semantics_"+verb).html(evaluated.semantics_display);

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

	    $.ajax({
		cache: false,
		dataType: "html",
		url: "/engine/generate-from-semantics?model=en&semantics=" + encodeURIComponent(JSON.stringify(semantics)),
		success: function translate(content) {
		    evaluated  = jQuery.parseJSON(content);
		    if (evaluated.response == "") {
			// could not generate anything: show a link with an error icon (fa-times-circle)
			$("#english_translation_"+pred).html("<a href='/engine/generate-from-semantics?lang=en&semantics=" +
							     encodeURIComponent(JSON.stringify(semantics)) + "'>" + 
							     "<i class='fa fa-times-circle'> </i>" + " </a>");
		    } else {
			$("#english_translation_"+pred).html(evaluated.response);
		    }
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


