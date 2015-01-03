var logging_level = INFO;

var default_prefix = "";

var source_language = "it";
var source_language_model = "small";

var target_language = "en";
var target_language_model = "small";

function italian_present_tense(pred) {
    return {"synsem": {
	"infl": "present",
	"sem": {
	    "tense": "present",    
	    "pred": pred}}};
};

function english_present_tense(pred) {
    // italian and english are the same for present tense, so simply use one for the other.
    return italian_present_tense(pred);
};

function italian_passato_spec(verb) {
    return {"synsem": {
	"infl": "present",
	"sem": {
	    "tense": "past",    
	    "pred": pred}}};
}

function english_passato_spec(verb) {
    return {"synsem": {
	"sem": {
	    "tense": "past",    
	    "pred": pred}}};
}


/* This is the entry point that editor.clj tell the client to use in its onload().
   It looks through the DOM and populates each node with what its contents should be. The initial nodes
   are all of the verbs supplied by verb.clj:(defn generation-table), which creates a <tr> for each verb, 
   along with the <tr>'s interior <td> skeleton that gen_per_verb() fleshes out. */
function gen_per_verb(prefix) {
    var verb_rows;

    if (prefix == undefined) {
	prefix = default_prefix;
	verb_rows = get_verb_rows_by_class();
    } else {
	verb_rows = get_verb_rows_by_prefix(prefix);
    }

    // now call gen_from_verb() for each verb row, which will fill in all of the <td>s.
    verb_rows.each(function() {
	var verb_dom_id = this.id;
	var verb = verb_dom_id;
	var re = new RegExp("^" + prefix);
	verb = verb.replace(re,"");
	    verb = verb.replace(/^verb_/,"");
	gen_from_verb(verb,prefix);
    });
}

function sentence_with_tense_info(language,pred) {
    if (language == "it") {
	return italian_present_tense(pred);
    }
    if (language == "en") {
	return english_present_tense(pred);
    }
    return italian_present_tense(pred);
}

function gen_from_verb(verb,prefix) {
    log(INFO,"gen_from_verb(" + verb + "," + prefix + ");");

    // not sure why or if this is necessary..??
    var re = new RegExp("^" + prefix);
    verb = verb.replace(re,"");
    verb = verb.replace(/^verb_/,"");

    spec = sentence_with_tense_info(source_language,verb);
    var serialized_spec = encodeURIComponent(JSON.stringify(spec));

    $.ajax({
	cache: false,
	dataType: "html",
	url: "/engine/generate?lang=" + source_language + "&model=" + source_language_model + "&spec="+serialized_spec,
	success: generate_with_verb
    });

    function generate_with_verb(content) {
	// 1. The server sent us an example sentence: parse the response into a JSON object.
	var evaluated = jQuery.parseJSON(content);
	var response = evaluated[source_language];
	var spec = evaluated.spec;
	var pred = spec["synsem"]["sem"]["pred"];

	var semantics = evaluated.semantics;
	if (response == "") {
	    response = "<i class='fa fa-times-circle'> </i>";
	}

	var serialized_spec = encodeURIComponent(JSON.stringify({"synsem": {"sem": semantics}}));


	// 2. Now that we have the example sentence in the source language in the variable _response_: now, paste it in to the DOM tree in the right place:
	$("#"+prefix+"verb_"+verb).html("<a href='/engine/generate?" + 
					"&spec=" + serialized_spec + 
					"&lang=" + source_language + 
					"&model=" + source_language_model + 
					"&debug=true" +
					"'>" + response + "</a>");

	// reload link:
	$("#"+prefix+"reload_"+verb).attr("onclick","javascript:refresh_row('" + verb + "','" + prefix + "');return false;");

	var infinitive_spec = {"synsem": {"cat": "verb",
					  "sem": {"pred": verb},
					  "infl": "infinitive"}};

	var serialized_infinitive_spec = encodeURIComponent(JSON.stringify(infinitive_spec));

	$.ajax({
	    cache: false,
	    dataType: "html",
	    url: "/engine/lookup?lang=" + target_language + "&spec=" + serialized_infinitive_spec,
	    success: translate_from_semantics
	});

	function translate_from_semantics(content) {
	    evaluated = jQuery.parseJSON(content);
	    var response;
	    if (evaluated.response == "") {
		// could not translate: show a link with an error icon (fa-times-circle)
		response = "<a href='/engine/lookup?lang="+ target_language + "&spec=" + serialized-spec + "'>" +
		    "<i class='fa fa-times-circle'> </i>" + " </a>";
	    } else {

		response = evaluated[target_language];
	    }
	    $("#"+prefix+"target_verb_"+pred).html(response);

	    var generate_target_language_url = "/engine/generate?lang="+ target_language + "&model=" + 
		target_language_model + "&spec=" + serialized_spec;

	    $.ajax({
		cache: false,
		dataType: "html",
		url: generate_target_language_url,
		success: function translate(content) {
		    var evaluated  = jQuery.parseJSON(content);
		    var response = evaluated[target_language];
		    if (response == "") {
			// could not generate anything: show a link with an error icon (fa-times-circle)
			response = "<i class='fa fa-times-circle'> </i>";
		    }
		    $("#"+prefix+"target_translation_"+pred).html("<a href='" + generate_target_language_url + "'>" + response + "</a>");
		}
	    });
	}
    }
}

function refresh_row(verb,prefix) {
    $("#"+prefix+"verb_"+verb).html("<i class='fa fa-spinner fa-spin'> </i>");
    $("#"+prefix+"target_verb").html("<i class='fa fa-spinner fa-spin'> </i>");
    $("#"+prefix+"target_translation_"+verb).html("<i class='fa fa-spinner fa-spin'> </i>");
    gen_from_verb(verb,prefix);
}

function get_verb_rows_by_class() {
    return $(".gen_source");
}

function get_verb_rows_by_prefix(prefix) {
    return $('#'+"generation_list_"+prefix).find(".gen_source");
}

