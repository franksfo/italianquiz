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

function ajax_refresh(content) {
 var new_content = $.ajax({
     dataType: "html",
     url: '/guess/tr/?guess=gennario',
     success: function (content) {
         $("#ajax_update").prepend(content);
     }
 });
}

