var http = require("http");
var https = require("https");
var url = require("url");
var fs = require("fs");
var path = require("path");
var util = require("util");

var rosterfile_arg = "rosters.json"; //which roster file to use
var rosters_file = fs.readFileSync(rosterfile_arg);
var rosters = JSON.parse(rosters_file);



console.log("Fake API for testing \n");

var delay = 0;


// regexp is the regular expression to match the url path.
// fun is the callback invoked to handle such url.
// fun signature is  (request, response, arg1, arg2, ..argn) where arg1,..argn are
// the captured strings from the regexp, if any.  (like in user_roster)
var handlers = [
	{regexp : new RegExp("^/api/auth$"), fun : auth},
	{regexp : new RegExp("^/api/user$"), fun : user},
	{regexp : new RegExp("^/api/roster$"), fun : roster},
	{regexp : new RegExp("^/api/notify$"), fun : notify},
	];


function onRequest(request, response) {
	var info = url.parse(request.url, true);
	var splitted_path = info.pathname.split("/");
	for(var i in handlers) {
		var h  = handlers[i];
		var result = h.regexp.exec(info.pathname);
		if (result != null) {
			result = result.slice(1, result.length);
			var params = [request, response].concat(result);
			h.fun.apply(h.fun, params);
		    	return;
		}
	}
	console.log("Invalid request path: " + info.pathname);
	response.writeHead(404, {"Content-Type": "text/plain"});
	response.write("404 Not found");
	response.end();
}

function roster(request, response) {
  parts = url.parse(request.url, true);
  query = parts.query;
  username = query.username
  console.log("Retrieving roster for user " + username );

  request.on('data', function(data) {
  });
  request.on('end', function() {
          if (username in rosters) {
              response.writeHead(200, {"Content-Type": "application/json"});
              response.write(JSON.stringify(rosters[username]));
          } else {
            console.log("User not found: " + username);
            response.writeHead(404, {"Content-Type": "text/plain"});
            response.write("404 Not found");
          }
      response.end();
  });
}

function auth(request, response) {
  parts = url.parse(request.url, true);
  query = parts.query;
  console.log("Authenticating user " + query.username + " with pass " + query.password);

  request.on('data', function(data) {
  });
  request.on('end', function() {
      response.writeHead(200, {"Content-Type": "application/json"});
      response.write(JSON.stringify({}));
      response.end();
  });
}
function user(request, response) {
  parts = url.parse(request.url, true);
  query = parts.query;
  console.log("Checking user " + query.username);
  request.on('data', function(data) {
  });
  request.on('end', function() {
      response.writeHead(200, {"Content-Type": "application/json"});
      response.write(JSON.stringify({}));
      response.end();
  });
}


function notify(request, response) {
    if (request.method == 'POST') {
        var body = '';
	request.on('data', function(data) {
	    body += data;
	    // Too much POST data, kill the connection!
            if (body.length > 1e6)
                request.connection.destroy();
	});

	request.on('end', function() {
	    parts = url.parse(request.url, true);
	    query = parts.query;
	    body = JSON.stringify(JSON.parse(body));
	    console.log("Received notification from " + query.from +
			" with payload:\n" + body);
	    response.writeHead(200, {"Content-Type": "application/json"});
	    response.write(body);
	    response.end();
	})
    } else {
	request.on('data', function(data) {
	});
	request.on('end', function() {
	    response.writeHead(500, {"Content-Type": "application/json"});
	    response.write(JSON.stringify({"error": "bad method"}));
	    response.end();
	})
    }
}


var options = {
  key: fs.readFileSync('privatekey.pem'),
  cert: fs.readFileSync('certificate.pem')
};

https.createServer(options, onRequest).listen(443);

console.log("Available API endpoints:");
for(var i in handlers) {
	var h  = handlers[i];
	console.log(h.regexp.source);
}