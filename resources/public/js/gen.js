var logging_level = INFO;

// TODO: div_id is not used yet, cannot populate tables yet (id is hard-wired to 'example_q').
function gen(div_id,index,upto) {
    log(INFO,"generating sentence at index:" + index);

    // generating in per-lexeme table for now, so disabling this with false.
    if (false && index <= upto) {
	function generate_at_row(content) {
	    var evaluated = jQuery.parseJSON(content);
	    var question = evaluated.full_question;
	    log(INFO,"Updating tour with question:" + question + " with index: " + index);
	    $("#example_q_"+index).html(question);
	    
	    // The following actions need to be taken:
	    // 1. generate next question
	    // 2. generate answer to that question.
	    // The order of 1 and 2 don't matter; they are independent of one another.
	    // From the client side, 1 and 2 are simply requests to the server
	    // (for which we receive answer asynchronously).

	    // 1. generate next question.
	    gen(div_id,index+1,upto);
	    
	    // 2. generate answer to this question.
	    function update_answer_fn(content) {
		evaluated  = jQuery.parseJSON(content);
		$("#example_a_"+index).html(evaluated.answer);
	    }

	    $.ajax({
		cache: false,
		dataType: "html",
		url: "/engine/generate-answers?semantics=" + encodeURIComponent(JSON.stringify(evaluated.semantics)),
		success: update_answer_fn
	    });
	}

	$.ajax({
	    cache: false,
            dataType: "html",
            url: "/engine/generate-question",
            success: generate_at_row
	});
    }
}

function gen_per_verb() {
    $(".gen_source").each(function() {
	var verb_dom_id = this.id;
	log(INFO,"verb_dom_id: " + verb_dom_id);
	var verb = verb_dom_id.replace(/^verb_/,"");
	log(INFO,"verb:" + verb);
	gen_from_verb(verb);
    });
}

function gen_from_verb(verb) {
    function generate_with_verb(content) {
	var evaluated = jQuery.parseJSON(content);
	var example = evaluated.it;
	var pred = evaluated.pred;
	var semantics = evaluated.semantics;
	$("#verb_"+verb).html(example);

	// hide semantics for now
	//	$("#semantics_"+verb).html(evaluated.semantics_display);

	function translate_verb(content) {
	    evaluated = jQuery.parseJSON(content);
	    $("#english_verb_"+pred).html(evaluated.en);

	    function translate(content) {
		evaluated  = jQuery.parseJSON(content);
		$("#english_translation_"+pred).html(evaluated.response);
	    }

	    $.ajax({
		cache: false,
		dataType: "html",
		url: "/engine/generate-from-semantics?lang=en&semantics=" + encodeURIComponent(JSON.stringify(semantics)),
		success: translate
	    });
	}

	$.ajax({
	    cache: false,
	    dataType: "html",
	    url: "/engine/lookup?lang=en&pred=" + pred,
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


