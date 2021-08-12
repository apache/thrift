//To build dist/thrift.js, dist/thrift.min.js and doc/*
//run grunt at the command line in this directory.
//Prerequisites:
// Node Setup -   nodejs.org
// Grunt Setup -  npm install  //reads the ./package.json and installs project dependencies
// Run grunt -    npx grunt  // uses project-local installed version of grunt (from package.json)

module.exports = function(grunt) {
  'use strict';

  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),
    concat: {
      options: {
        separator: ';'
      },
      dist: {
        src: ['src/**/*.js'],
        dest: 'dist/<%= pkg.name %>.js'
      }
    },
    jsdoc : {
        dist : {
            src: ['src/*.js', './README.md'],
            options: {
              destination: 'doc'
            }
        }
    },
    uglify: {
      options: {
        banner: '/*! <%= pkg.name %> <%= grunt.template.today("dd-mm-yyyy") %> */\n'
      },
      dist: {
        files: {
          'dist/<%= pkg.name %>.min.js': ['<%= concat.dist.dest %>']
        }
      }
    },
    shell: {
      InstallThriftJS: {
        command: 'cp src/thrift.js test/build/js/thrift.js'
      },
      InstallThriftNodeJSDep: {
        command: 'cd ../.. && npm install'
      },
      InstallTestLibs: {
        command: 'cd test && ant download_jslibs'
      },
      ThriftGen: {
        command: [
          '"../../compiler/cpp/thrift" -gen js --out test/gen-js ../../test/ThriftTest.thrift',
          '"../../compiler/cpp/thrift" -gen js --out test/gen-js ../../test/JsDeepConstructorTest.thrift',
          '"../../compiler/cpp/thrift" -gen js:jquery --out test/gen-js-jquery ../../test/ThriftTest.thrift',
          '"../../compiler/cpp/thrift" -gen js:node --out test/gen-nodejs ../../test/ThriftTest.thrift',
          '"../../compiler/cpp/thrift" -gen js:es6 --out test/gen-js-es6 ../../test/ThriftTest.thrift',
          '"../../compiler/cpp/thrift" -gen js:node,es6 --out ./test/gen-nodejs-es6 ../../test/ThriftTest.thrift',
        ].join(' && ')
      },
      ThriftGenJQ: {
        command: '../../compiler/cpp/thrift -gen js:jquery -gen js:node -o test ../../test/ThriftTest.thrift'
      },
      ThriftGenDeepConstructor: {
        command: '../../compiler/cpp/thrift -gen js -o test ../../test/JsDeepConstructorTest.thrift'
      },
      ThriftBrowserifyNodeInt64: {
        command: [
          './node_modules/browserify/bin/cmd.js ./node_modules/node-int64/Int64.js -s Int64 -o test/build/js/lib/Int64.js',
          './node_modules/browserify/bin/cmd.js ../nodejs/lib/thrift/int64_util.js -s Int64Util -o test/build/js/lib/Int64Util.js',
          './node_modules/browserify/bin/cmd.js ./node_modules/json-int64/index.js -s JSONInt64 -o test/build/js/lib/JSONInt64.js'
        ].join(' && ')
      },
      ThriftGenInt64: {
        command: '../../compiler/cpp/thrift -gen js -o test ../../test/Int64Test.thrift'
      },
      ThriftGenDoubleConstants: {
        command: '../../compiler/cpp/thrift -gen js -o test ../../test/DoubleConstantsTest.thrift'
      },
      ThriftTestServer: {
        options: {
          async: true,
          execOptions: {
            cwd: "./test",
            env: {NODE_PATH: "../../nodejs/lib:../../../node_modules"}
          }
        },
        command: "node server_http.js",
      },
      ThriftTestServerES6: {
        options: {
          async: true,
          execOptions: {
            cwd: "./test",
            env: {NODE_PATH: "../../nodejs/lib:../../../node_modules"}
          }
        },
        command: "node server_http.js --es6",
      },
      ThriftTestServer_TLS: {
        options: {
          async: true,
          execOptions: {
            cwd: "./test",
            env: {NODE_PATH: "../../nodejs/lib:../../../node_modules"}
          }
        },
        command: "node server_https.js",
      },
      ThriftTestServerES6_TLS: {
        options: {
          async: true,
          execOptions: {
            cwd: "./test",
            env: {NODE_PATH: "../../nodejs/lib:../../../node_modules"}
          }
        },
        command: "node server_https.js --es6",
      },
    },
    qunit: {
      ThriftJS: {
        options: {
          urls: [
            'http://localhost:8089/test-nojq.html'
          ],
          puppeteer: {
            headless: true,
            args: ['--no-sandbox'],
          },
        }
      },
      ThriftJSJQ: {
        options: {
          urls: [
            'http://localhost:8089/test.html'
          ],
          puppeteer: {
            headless: true,
            args: ['--no-sandbox'],
          },
        }
      },
      ThriftJS_DoubleRendering: {
        options: {
          urls: [
            'http://localhost:8089/test-double-rendering.html'
          ],
          puppeteer: {
            headless: true,
            args: ['--no-sandbox'],
            ignoreHTTPSErrors: true,
          },
        }
      },
      ThriftJS_Int64: {
        options: {
          urls: [
            'http://localhost:8089/test-int64.html'
          ],
          puppeteer: {
            headless: true,
            args: ['--no-sandbox'],
            ignoreHTTPSErrors: true,
          },
        }
      },
      ThriftWS: {
        options: {
          urls: [
            'http://localhost:8089/testws.html'
          ],
          puppeteer: {
            headless: true,
            args: ['--no-sandbox'],
          },
        }
      },
      ThriftJS_TLS: {
        options: {
          urls: [
            'https://localhost:8091/test-nojq.html'
          ],
          puppeteer: {
            headless: true,
            args: ['--no-sandbox'],
            ignoreHTTPSErrors: true,
          },
        }
      },
      ThriftJSJQ_TLS: {
        options: {
          urls: [
            'https://localhost:8091/test.html'
          ],
          puppeteer: {
            headless: true,
            args: ['--no-sandbox'],
            ignoreHTTPSErrors: true,
          },
        }
      },
      ThriftWS_TLS: {
        options: {
          urls: [
            'https://localhost:8091/testws.html'
          ],
          puppeteer: {
            headless: true,
            args: ['--no-sandbox'],
            ignoreHTTPSErrors: true,
          },
        }
      },
      ThriftDeepConstructor: {
        options: {
          urls: [
            'http://localhost:8089/test-deep-constructor.html'
          ],
          puppeteer: {
            headless: true,
            args: ['--no-sandbox'],
          },
        }
      },
      ThriftWSES6: {
        options: {
          urls: [
            'http://localhost:8088/test-es6.html'
          ],
          puppeteer: {
            headless: true,
            args: ['--no-sandbox'],
          },
        }
      }
    },
    jshint: {
      // The main Thrift library file. not es6 yet :(
      lib: {
        src: ['src/**/*.js'],
      },
      // The test files use es6
      test: {
        src: ['Gruntfile.js', 'test/*.js'],
        options: {
          esversion: 6,
        }
      },
      gen_js_code: {
        src: ['test/gen-js/*.js', 'test/gen-js-jquery/*.js'],
      },
      gen_es6_code: {
        src: ['test/gen-js-es6/*.js'],
        options: {
          esversion: 6,
        }
      },
      gen_node_code: {
        src: ['test/gen-nodejs/*.js'],
        options: {
          node: true,
        }
      },
      gen_node_es6_code: {
        src: ['test/gen-nodejs-es6/*.js'],
        options: {
          node: true,
          esversion: 6,
        }
      }
    },
  });

  grunt.loadNpmTasks('grunt-contrib-uglify');
  grunt.loadNpmTasks('grunt-contrib-jshint');
  grunt.loadNpmTasks('grunt-contrib-qunit');
  grunt.loadNpmTasks('grunt-contrib-concat');
  grunt.loadNpmTasks('grunt-jsdoc');
  grunt.loadNpmTasks('grunt-shell-spawn');

  grunt.registerTask('wait', 'Wait just one second for the server to start', function () {
    var done = this.async();
    setTimeout(function() {
      done(true);
    }, 1000);
  });

  grunt.registerTask('CreateDirectories', 'Creating required local directories', function () {
    grunt.file.mkdir('test/build/js/lib');
    grunt.file.mkdir('test/gen-js');
    grunt.file.mkdir('test/gen-js-jquery');
    grunt.file.mkdir('test/gen-nodejs');
    grunt.file.mkdir('test/gen-js-es6');
    grunt.file.mkdir('test/gen-nodejs-es6');
});

  grunt.registerTask('installAndGenerate', [
    'CreateDirectories',
    'shell:InstallThriftJS',
    'shell:InstallThriftNodeJSDep',
    'shell:ThriftGen',
    'shell:ThriftGenDeepConstructor',
    'shell:ThriftGenDoubleConstants',
    'shell:InstallTestLibs',
    'shell:ThriftBrowserifyNodeInt64',
    'shell:ThriftGenInt64'
  ]);

  grunt.registerTask('test', [
    'installAndGenerate',
    'jshint',
    'shell:ThriftTestServer',
    'shell:ThriftTestServer_TLS',
    'shell:ThriftTestServerES6',
    'shell:ThriftTestServerES6_TLS',
    'wait',
    'qunit:ThriftDeepConstructor',
    'qunit:ThriftJS',
    'qunit:ThriftJS_TLS',
    'qunit:ThriftJS_DoubleRendering',
    'qunit:ThriftWS',
    'qunit:ThriftJSJQ',
    'qunit:ThriftJSJQ_TLS',
    'qunit:ThriftWSES6',
    'qunit:ThriftJS_Int64',
    'shell:ThriftTestServer:kill',
    'shell:ThriftTestServer_TLS:kill',
    'shell:ThriftTestServerES6:kill',
    'shell:ThriftTestServerES6_TLS:kill',
  ]);
  grunt.registerTask('default', ['test', 'concat', 'uglify', 'jsdoc']);
};
