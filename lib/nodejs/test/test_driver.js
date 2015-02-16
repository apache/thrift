/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * 'License'); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * 'AS IS' BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

 // This is the Node.js test driver for the standard Apache Thrift
 // test service. The driver invokes every function defined in the
 // Thrift Test service with a representative range of parameters.
 //
 // The ThriftTestDriver function requires a client object
 // connected to a server hosting the Thrift Test service and
 // supports an optional callback function which is called with
 // a status message when the test is complete.

var assert = require('assert');
var ttypes = require('./gen-nodejs/ThriftTest_types');
var Int64 = require('node-int64');
var testCases = require('./test-cases');

exports.ThriftTestDriver = function(client, callback) {

  function makeAsserter(assertionFn) {
    return function(c) {
      var fnName = c[0];
      var expected = c[1];
      client[fnName](expected, function(err, actual) {
        assert(!err);
        assertionFn(actual, expected);
      })
    };
  }

  testCases.simple.forEach(makeAsserter(assert.equal));
  testCases.deep.forEach(makeAsserter(assert.deepEqual));

  client.testStruct(testCases.out, function(err, response) {
    assert(!err);
    checkRecursively(testCases.out, response);
  });

  client.testNest(testCases.out2, function(err, response) {
    assert(!err);
    checkRecursively(testCases.out2, response);
  });

  client.testInsanity(testCases.crazy, function(err, response) {
    assert(!err);
    checkRecursively(testCases.insanity, response);
  });

  client.testException('TException', function(err, response) {
    assert(!err);
    assert(!response);
  });

  client.testException('Xception', function(err, response) {
    assert(!response);
    assert.equal(err.errorCode, 1001);
    assert.equal('Xception', err.message);
  });

  client.testException('no Exception', function(err, response) {
    assert(!err);
    assert.equal(undefined, response); //void
  });

  client.testOneway(0, function(err, response) {
    assert(false); //should not answer
  });

  checkOffByOne(function(done) {
    client.testI32(-1, function(err, response) {
      assert(!err);
      assert.equal(-1, response);
      done();
    });
  }, callback);

};

exports.ThriftTestDriverPromise = function(client, callback) {

  function makeAsserter(assertionFn) {
    return function(c) {
      var fnName = c[0];
      var expected = c[1];
      client[fnName](expected)
        .then(function(actual) {
          assert.equal(actual, expected);
        })
        .fail(failTest);
    };
  }

  testCases.simple.forEach(makeAsserter(assert.equal));
  testCases.deep.forEach(makeAsserter(assert.deepEqual));

  client.testStruct(testCases.out)
    .then(function(response) {
      checkRecursivelyP(testCases.out, response);
    })
    .fail(failTest);

  client.testNest(testCases.out2)
    .then(function(response) {
      checkRecursivelyP(testCases.out2, response);
    })
    .fail(failTest);

  client.testInsanity(testCases.crazy)
    .then(function(response) {
      checkRecursivelyP(testCases.insanity, response);
    })
    .fail(failTest);

  client.testException('TException')
    .then(failTest);

  client.testException('Xception')
    .then(function(response) {
      assert.equal(err.errorCode, 1001);
      assert.equal('Xception', err.message);
    })
    .fail(failTest);

  client.testException('no Exception')
    .then(function(response) {
      assert.equal(undefined, response); //void
    })
    .fail(failTest);

  client.testOneway(0, failTest); //should not answer

  checkOffByOne(function(done) {
    client.testI32(-1)
      .then(function(response) {
          assert.equal(-1, response);
          done();
      })
      .fail(function() {
        assert(false);
      });
  }, callback);

};


// Helper Functions
// =========================================================

function failTest() {
  assert(false);
}

// This is the version of checkRecursively that was in the vanilla callback
// version of test_driver.
function checkRecursively(map1, map2) {
  if (typeof map1 !== 'function' && typeof map2 !== 'function') {
    if (!map1 || typeof map1 !== 'object') {
      //Handle int64 types (which use node-int64 in Node.js JavaScript)
      if ((typeof map1 === "number") && (typeof map2 === "object") &&
          (map2.buffer) && (map2.buffer instanceof Buffer) && (map2.buffer.length === 8)) {
        var n = new Int64(map2.buffer);
        assert.equal(map1, n.toNumber());
      } else {
        assert.equal(map1, map2);
      }
    } else {
      for (var key in map1) {
        checkRecursively(map1[key], map2[key]);
      }
    }
  }
}

// This is the version of checkRecursively that was in the promise version of
// test_driver.
// deepEqual doesn't work with fields using node-int64
function checkRecursivelyP(map1, map2) {
  if (typeof map1 !== 'function' && typeof map2 !== 'function') {
    if (!map1 || typeof map1 !== 'object') {
        assert.equal(map1, map2);
    } else {
      for (var key in map1) {
        checkRecursivelyP(map1[key], map2[key]);
      }
    }
  }
}

function checkOffByOne(done, callback) {

  var retry_limit = 30;
  var retry_interval = 100;
  var test_complete = false;
  var retrys = 0;

  /**
   * redo a simple test after the oneway to make sure we aren't "off by one" --
   * if the server treated oneway void like normal void, this next test will
   * fail since it will get the void confirmation rather than the correct
   * result. In this circumstance, the client will throw the exception:
   *
   * Because this is the last test against the server, when it completes
   * the entire suite is complete by definition (the tests run serially).
   */
  done(function() {
    test_complete = true;
  });

  //We wait up to retry_limit * retry_interval for the test suite to complete
  function TestForCompletion() {
    if(test_complete && callback) {
      callback("Server successfully tested!");
    } else {
      if (++retrys < retry_limit) {
        setTimeout(TestForCompletion, retry_interval);
      } else if (callback) {
        callback("Server test failed to complete after " +
                 (retry_limit * retry_interval / 1000) + " seconds");
      }
    }
  }

  setTimeout(TestForCompletion, retry_interval);
}
