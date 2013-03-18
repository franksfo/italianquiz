function workbook() {
    var search_exp = $("#workbookq").val();
    $.ajax({
        dataType: "html",
        url: "/italian/workbook/q/?attrs=italian+english&search="+escape(search_exp),
        success: function (content) {
            $("#workbooka").prepend(content);
            $("#workbookq").focus();
        }
    });
}

