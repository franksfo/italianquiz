var logging_level = INFO;

function gen(div_id,index,upto) {
    log(INFO,"generating sentence at index:" + index);

    if (index < upto) {
    function generate_at_row(content) {
	var evaluated = jQuery.parseJSON(content);
	var question = evaluated.full_question;
	log(INFO,"Updating tour with question:" + question + " with index: " + index);
	$("#example_q_"+index).html(question);
    }

    $.ajax({
	cache: false,
        dataType: "html",
        url: "/cloud/generate-question",
        success: generate_at_row
    });
    }
}

    

