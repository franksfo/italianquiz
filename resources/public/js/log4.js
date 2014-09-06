// lib-ish:
var TRACE = 4;
var DEBUG = 3;
var INFO  = 2;
var logging = {
    4 : "TRACE",
    3 : "DEBUG",
    2 : "INFO"
};

function log(level,str) {
    if (logging_level >= level) {
	console.log(logging[level] + ":" + str);
    }
}
