/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
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


