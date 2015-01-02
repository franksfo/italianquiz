var logging_level = INFO;

var default_prefix = "";


var source_language = "it";
var source_language_model = "small";

var target_language = "en";
var target_language_model = "small";


/* This is the entry point that editor.clj tell the client to use in its onload().
   It looks through the DOM and populates each node with what its contents should be. The initial nodes
   are all of the verbs supplied by verb.clj:(defn generation-table), which creates a <tr> for each verb, 
   along with the <tr>'s interior <td> skeleton that gen_per_verb() fleshes out. */
function gen_per_verb(prefix) {
    if (prefix == undefined) {
	prefix = default_prefix;
	$(".gen_source").each(function() {
	    var verb_dom_id = this.id;
	    var verb = verb_dom_id;
	    var re = new RegExp("^" + prefix);
	    verb = verb.replace(re,"");
	    verb = verb.replace(/^verb_/,"");
	    gen_from_verb(verb,prefix);
	});
    } else {
	var selector = '#'+"generation_list_"+prefix;
	$(selector).find(".gen_source").each(function() {
	    var verb_dom_id = this.id;
	    var verb = verb_dom_id;
	    var re = new RegExp("^" + prefix);
	    verb = verb.replace(re,"");
	    verb = verb.replace(/^verb_/,"");
	    gen_from_verb(verb,prefix);
	});
    }
}

function gen_from_verb(verb,prefix) {
    log(INFO,"gen_from_verb(" + verb + "," + prefix + ");");
    var re = new RegExp("^" + prefix);
    verb = verb.replace(re,"");
    verb = verb.replace(/^verb_/,"");

    function generate_with_verb(content) {
	var evaluated = jQuery.parseJSON(content);
	var example = evaluated[source_language];
	var spec = evaluated.spec;
	var pred = spec["synsem"]["sem"]["pred"];

	var semantics = evaluated.semantics;
	var response = example;
	if (example == "") {
	    response = "<i class='fa fa-times-circle'> </i>";
	}

	var serialized_spec = encodeURIComponent(JSON.stringify(spec));

	$("#"+prefix+"verb_"+verb).html("<a href='/engine/generate?" + 
					"&spec=" + serialized_spec + 
					"&lang=" + source_language + 
					"&model=" + source_language_model + 
					"&debug=true" +
					"'>" + response + "</a>");

	// reload link:
	$("#"+prefix+"reload_"+verb).attr("onclick","javascript:refresh_row('" + verb + "','" + prefix + "');return false;");

	function translate_verb(content) {
	    evaluated = jQuery.parseJSON(content);
	    if (evaluated.response == "") {
		// could not translate: show a link with an error icon (fa-times-circle)
		$("#"+prefix+"english_verb_"+pred).html("<a href='/engine/lookup?lang="+ target_language + "&spec=" + 
					      encodeURIComponent(JSON.stringify(

						  {"synsem": {"cat": "verb",
							      "sem": {"pred": pred},
							      "infl": "infinitive"}}

					      )) + "'>" +
					      "<i class='fa fa-times-circle'> </i>" + " </a>");

	    } else {
		$("#"+prefix+"english_verb_"+pred).html(evaluated[target_language]);
	    }

	    var spec = {"synsem": {"sem": semantics}};

	    var generate_target_language_url = "/engine/generate?lang="+ target_language + "&model=" + 
		target_language_model + "&spec=" + encodeURIComponent(JSON.stringify(spec));

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
		    $("#"+prefix+"english_translation_"+pred).html("<a href='" + generate_target_language_url + "'>" + response + "</a>");
		}
	    });
	}

	var infinitive_spec = {"synsem": {"cat": "verb",
					  "sem": {"pred": verb},
					  "infl": "infinitive"}};

	var serialized_spec = encodeURIComponent(JSON.stringify(infinitive_spec));

	$.ajax({
	    cache: false,
	    dataType: "html",
	    url: "/engine/lookup?lang=" + target_language + "&spec=" + serialized_spec,
	    success: translate_verb
	});

    }

    var spec = {"synsem": {"sem": {"pred": verb}}};
    var serialized_spec = encodeURIComponent(JSON.stringify(spec));

    $.ajax({
	cache: false,
	dataType: "html",
	url: "/engine/generate?lang=it&model=small&spec="+serialized_spec,
	success: generate_with_verb
    });
}

function refresh_row(verb,prefix) {
    $("#"+prefix+"verb_"+verb).html("<i class='fa fa-spinner fa-spin'> </i>");
    $("#"+prefix+"english_verb_"+verb).html("<i class='fa fa-spinner fa-spin'> </i>");
    $("#"+prefix+"english_translation_"+verb).html("<i class='fa fa-spinner fa-spin'> </i>");
    gen_from_verb(verb,prefix);
}
