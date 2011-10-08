var staticContentRow = 1;

function got(here) {
    $("#guessbody").prepend("<tr style='display:none' id='got_row_" + staticContentRow + "'><th>"+staticContentRow+"</th><th>got</th><td>" + here + "</td></tr>");
    $("#got_row_"+staticContentRow).fadeIn("slow");
    staticContentRow++;
}

var guessNumber = 1;

function addguess(english,italian) {
    $("#guess-table").prepend("<tr style='display:none' id='guess_row_" + guessNumber + "'><th>" + guessNumber + "</th><th>" + english + "</th><td>" + italian + "</td></tr>");
    $("#guess_row_"+guessNumber).fadeIn("slow");
    guessNumber++;
}

function ajax_refresh(form_input_id) {
    var guess = $("#"+form_input_id).val();

    // apply user's guess to guess evaluation..
    $.ajax({
        dataType: "html",
        url: "/evaluate/tr/?guess="+escape(guess),
        success: function (content) {
            $("#ajax_update").prepend(content);
        }
    });
    get_next_question();

}

function get_next_question() {
    $.ajax({
        dataType: "html",
        url: "/guess/question/",
        success: function (content) {
            $("#ajax_question").html(content);
        }
    });
}

