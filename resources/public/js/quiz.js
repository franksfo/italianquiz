var staticContentRow = 1;
var guessNumber = 1;

function got(here) {
    $("#guessbody").prepend("<tr style='display:none' id='row_" + staticContentRow + "'><th>"+staticContentRow+"</th><th>got</th><td>" + here + "</td></tr>");
    $("#row_"+staticContentRow).fadeIn("slow");
    staticContentRow++;
}

function addguess(english,italian) {
    $("#guess-table").prepend("<tr style='display:none' id='row_" + guessNumber + "'><th>" + guessNumber + "</th><th>" + english + "</th><td>" + italian + "</td></tr>");
    $("#row_"+guessNumber).fadeIn("slow");
    guessNumber++;
}

