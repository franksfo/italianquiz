var logging_level = INFO;

function toggle_expand(expand_this) {
    if (expand_this.style.height == "100px") {
	expand_this.style.height = "auto";
	expand_this.style.width = "auto";
	expand_this.style.overflow = "auto";
    } else {
	expand_this.style.height = "100px";
	expand_this.style.width = "200px";
	expand_this.style.overflow = "scroll";
    }
}

function delete_game_dialog(game_id,game_name) {
    // modify the delete tag so that the form input named 'game' is set to game_id.
    $("#delete_game").css("display","block");
    $("#delete_game_name").html(game_name);
}

function delete_game_confirm(game_id) {
    alert("WTF");
    $("#delete_game_" + game_id).submit();
}

