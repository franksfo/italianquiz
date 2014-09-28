var logging_level = DEBUG;

var cloud_speeds = {};

wind_speed = 0;

function start_lab() {
    log(DEBUG,"Hello, starting lab.");


    cloud_speeds["cloud_0"] = Math.random()*0.010;


    setInterval(function() {
	blow_clouds(0);
    },wind_speed);

    log(DEBUG,"lab has started.");
}

function blow_clouds(i) {
    log(DEBUG,"blow_clouds(" + i + ")");
    var cloud =  $(".motion")[i];
    if (cloud) {
	blow_cloud(cloud);
	blow_clouds(i+1);
    }
}

function blow_cloud(cloud) {
    var cloud_id = cloud.id;
    if (cloud_speeds[cloud_id] == undefined) {
	log(DEBUG,"Cloud can't blow until it gets an answer.");
	return;
    } else {
	log(DEBUG,"Cloud is ready to blow.");
    }

    log(DEBUG,"current cloud left: " + cloud.style.left);

    var cloud_left= parseFloat(cloud.style.left.replace('%',''));
    var cloud_id = cloud.id;

    log(DEBUG,"new cloud left will be: " + (cloud_left + cloud_speeds[cloud_id])+"%");

    cloud.style.left = (cloud_left + cloud_speeds[cloud_id])+"%";

    if (cloud_left > 90) {
	if (cloud.getAttribute("class").match(/solved/)) {
	    log(INFO,"You solved this one, no correction needed..");
	} else {
	    correct_user(cloud);

	    // slow down: user is struggling.
	    if (current_speed_limit > min_speed) {
		current_speed_limit--;
	    }
	    log(DEBUG,"After slowing down due to a miss, your current speed is: " + current_speed_limit);
	}
    }

    if (cloud_speeds[cloud_id] < 0) {
	cloud_speeds[cloud_id] = 0.1;
	return;
    }

    if (cloud_speeds[cloud_id] > 5) {
	cloud_speeds[cloud_id] = 5;
	return;
    }

    var incr = Math.floor(Math.random()*100);

    if (incr < 5) {
        cloud_speeds[cloud_id] = cloud_speeds[cloud_id] - 0.1;
	log(DEBUG,"cloud " + cloud_id + " slowed down to: " + cloud_speeds[cloud_id]);
    } else {
        if (incr < current_speed_limit) {
	    cloud_speeds[cloud_id] = cloud_speeds[cloud_id] + 0.05;
	    log(DEBUG,"cloud " + cloud_id + " sped up to: " + cloud_speeds[cloud_id]);
        }
    }
}

