var logging_level = INFO;

// TODO: div_id is not used yet, cannot populate tables yet (id is hard-wired to 'example_q').
function gen(div_id,index,upto) {
    log(INFO,"generating sentence at index:" + index);

    if (index < upto) {
    function generate_at_row(content) {
	var evaluated = jQuery.parseJSON(content);
	var question = evaluated.full_question;
	log(INFO,"Updating tour with question:" + question + " with index: " + index);
	$("#example_q_"+index).html(question);

	// generate next question.
	gen(div_id,index+1,upto);
    }

    $.ajax({
	cache: false,
        dataType: "html",
        url: "/cloud/generate-question",
        success: generate_at_row
    });
    }
}

    

