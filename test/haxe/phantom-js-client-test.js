var url = 'http://localhost:9090/';
console.log("Trying to connect to : " + url);

var page = require('webpage').create();

var successRun = false;
page.onConsoleMessage = function(msg) {
    if(msg.indexOf("Tests failed      0") !== -1) {
        successRun = true;
    }
    console.log('page : ' + msg);
};

page.open(url, function(status) {
    if(status === "success") {
        console.log("connected. page text : " + page.plainText);
        var jsToInject = 'bin/js/Main.js';
        if (page.injectJs(jsToInject)) {
            if(successRun) {
                phantom.exit(0);
            } else {
                console.log("error on running tests");
                phantom.exit(1);
            }
        } else {
            console.log("error in injecting " + jsToInject);
            phantom.exit(1);
        }
    } else {
        console.log("Status: " + status);
        console.log("Please run test web server : make php_web_server");
        phantom.exit(1);
    }
});


