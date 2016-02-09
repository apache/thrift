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
 /* jshint -W100 */

/*
 * JavaScript test suite for ThriftTest.thrift. These tests
 * will run only with AngularJS (-gen js:angular) Apache Thrift
 * interfaces. To create client code:
 *      $ thrift -gen js:angular ThriftTest.thrift
 *
 * See also:
 * ++ test.js for generic tests
 * ++ test-jq.js for "-gen js:jquery" tests
 * ++ test-nojq.js for "-gen js" only tests
 */

angular.module('ngthriftApp').config(["ThriftTestClientProvider", function(ThriftTestClientProvider) {
  // provide ThriftTestClient with proper server url
  ThriftTestClientProvider.setUrl('/service'); // configure the service url
}]);

var getClient = function(){
    // get reference to ThriftTestClient service
    return angular.element($('body')).injector().get('ThriftTestClient');
};


module("Angular Async");
  test("I32", function() {
    expect( 3 );
    var client = getClient();

    QUnit.stop();
    client.testI32(Math.pow(2,30), function(result) {
      equal(result, Math.pow(2,30));
      QUnit.start();
    });

    QUnit.stop();
    var promise = client.testI32(Math.pow(-2,31), function(result) {
      equal(result, Math.pow(-2,31));
    });
    promise.success(function(result) {
      equal(result, Math.pow(-2,31));
      QUnit.start();
    });
  });

  test("I64", function() {
    expect( 4 );
    var client = getClient();

    QUnit.stop();
    //This is usually 2^60 but JS cannot represent anything over 2^52 accurately
    client.testI64(Math.pow(2,52), function(result) {
      equal(result, Math.pow(2,52));
      QUnit.start();
    });

    QUnit.stop();
    //This is usually 2^60 but JS cannot represent anything over 2^52 accurately
    client.testI64(Math.pow(-2, 52), function (result) {
      equal(result, Math.pow(-2, 52));
        })
        .error(function (data, status, headers, config) {
          ok(false, data);
          ok(true);
          QUnit.start();
        })
        .success(function (result) {
          //This is usually 2^60 but JS cannot represent anything over 2^52 accurately
          equal(result, Math.pow(-2, 52));
          ok(true);
          QUnit.start();
        });
  });

  test("Xception", function() {
    expect( 2 );
    var client = getClient();

    QUnit.stop();

    // provide sth as "callback"
    var promise = client.testException("Xception", true).success(function(result) {
      ok(false);
      QUnit.start();
    }).catch(function(e){
      // catch exception
      equal(e.errorCode, 1001);
      equal(e.message, "Xception");
      QUnit.start();
    });
  });
