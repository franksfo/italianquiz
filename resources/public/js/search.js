function search() {
    var search_exp = $("#search").val();

    $.ajax({
        dataType: "html",
        url: "/italian/search/q/?search="+search_exp,
        success: function (content) {
            $("#searchresults").html(content);
            $("#search").focus();
        }
    });



}