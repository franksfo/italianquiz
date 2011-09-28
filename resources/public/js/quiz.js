var number = 1;

function got(here) {
    $("#guessbody").prepend("<tr style='display:none' id='row_" + number + "'><th>"+number+"</th><th>got</th><td>" + here + "</td></tr>");
    $("#row_"+number).fadeIn("slow");
    number++;
}

