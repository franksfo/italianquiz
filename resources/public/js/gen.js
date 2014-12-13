var logging_level = INFO;

// TODO: div_id is not used yet, cannot populate tables yet (id is hard-wired to 'example_q').
function gen(div_id,index,upto) {
    log(INFO,"generating sentence at index:" + index);

    if (index <= upto) {
	function generate_at_row(content) {
	    var evaluated = jQuery.parseJSON(content);
	    var question = evaluated.full_question;
	    log(INFO,"Updating tour with question:" + question + " with index: " + index);
	    $("#example_q_"+index).html(question);
	    
	    // order of 1 and 2 below don't matter; they are independent of one another.
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
		url: "/cloud/generate-answers?semantics=" + encodeURIComponent(JSON.stringify(evaluated.semantics)),
		success: update_answer_fn
	    });
	}

	$.ajax({
	    cache: false,
            dataType: "html",
            url: "/cloud/generate-question",
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
	$("#verb_"+verb).html(example);
    }

    $.ajax({
	cache: false,
	dataType: "html",
	url: "/cloud/generate?pred="+verb+"&lang=it",
	success: generate_with_verb
    });
}


