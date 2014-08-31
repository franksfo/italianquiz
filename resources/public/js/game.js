var background = "white";

var radius = 35;
// TODO: get width and height of #game from DOM, not hardcoded.
var game_width = 1000;
var game_height = 500;
var offset=0;
var transition_time = 1500;
var animals = [
    {"name":"bear",
     "x":50+offset
    },
    {"name":"cat",
     "x":150+offset
    },
    {"name":"cow",
     "x":250+offset
    },
    {"name":"dog",
     "x":350+offset
    },
    {"name":"gecko",
     "x":450+offset
    },
    {"name":"otter",
     "x":550+offset
    },
    {"name":"snake",
     "x":650+offset
    },
    {"name":"wolf",
     "x":750+offset
    }
];

var bear  = animals[0];
var cat   = animals[1];
var cow   = animals[2];
var dog   = animals[3];
var gecko = animals[4];
var otter = animals[5];
var snake = animals[6];
var wolf  = animals[7];

var set_of_maps = [ {"name":"friends",
		     "animals":[bear, cat, otter]},
		    {"name":"family",
		     "animals":[dog,  cat]},
		    {"name":"canine",
		     "animals":[dog, wolf]},
		    {"name":"wild",
		     "animals":[bear, otter, snake, wolf]},
		    {"name":"mammals",
		     "animals":[bear, cat, cow, dog, otter, wolf]},
		    {"name":"reptiles",
		     "animals":[gecko, snake]},
		    {"name":"pets",
		     "animals":[cat, dog, snake]}];

function start_game() {
    var svg = d3.select("#gamesvg");
    show_animal_set(svg);
    setInterval(function() {
	show_animal_set(svg);
    },transition_time);
}

function show_animal_set(svg) {
    // index_fn: what key to use to compare items for equality.
    var index_fn = function(d) {return d.name;};
    // show the next set in animal_sets.
    var animal_set = random_set();
    if (animal_set == previous_set) {
	// try again
	return show_animal_set(svg);
    }
    previous_set = animal_set;
    console.log("new animal set:" + animal_set.name + "(" + animal_set.animals.map(function(e) {return e.name;}) + ")");
    newdata_array = animal_set.animals;

    if (existing) {
	console.log("existing:" + 
		    existing.map(function(a){return a.name;}));
    }
    console.log("new group:" + 
		newdata_array.map(function(a){return a.name;}));

    console.log("introducing:" + 
		newdata_array.map(function(a){return a.name;}));
    var newdata = svg.selectAll("circle").data(newdata_array,index_fn);

    // Add items unique to input_data.
    newdata.enter().append("circle").
	attr("cx",function(c) {
	    return c.x;
	}).
	attr("cy",function(c) {return -50;}).
        attr("r", function(c) {return radius;}).
	attr("class",function(c) {
	    return c.name;
	}).
	transition().duration(transition_time/2).
	attr("cy",240);
    
    // Remove items not in new data.
    newdata.exit().transition().duration(transition_time/2)
        .style("fill",background)   // use background color: causes a fade-out effect.
	.attr("cy",
	      function(animal) {
		  return game_height+200;
	      }).remove();

    existing = newdata_array;
}

var previous_set = null;

function random_set() {
    var choice_i = Math.floor(Math.random()*(set_of_maps.length));
    var set_name = set_of_maps[choice_i].name;
    d3.select("#status").html("New set chosen: " + set_name);
    return set_of_maps[choice_i];
}

function keys(arg) {
    return Object.keys(arg);
}
var existing = null;

function find_animal(needle,haystack) {
    var i = 0;
    for(i = 0; i < haystack.length; i++) {
	if (needle.name == haystack[i].name) {
	    return true;
	}
    }
    return false;
}

