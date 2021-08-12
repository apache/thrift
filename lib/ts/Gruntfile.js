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
    shell: {
      InstallThriftJS: {
        command: 'mkdir -p test/build/ts/lib; cp ../js/src/thrift.js test/build/ts/thrift.js'
      },
      InstallThriftNodeJSDep: {
        command: 'cd ../..; npm install'
      },
      InstallTestLibs: {
        command: 'cd test; ant download_jslibs'
      },
      ThriftGen: {
        command: [
          'mkdir -p test/gen-js',
          '../../compiler/cpp/thrift -gen js:ts --out test/gen-js ../../test/ThriftTest.thrift',
          'mkdir -p test/gen-nodejs',
          '../../compiler/cpp/thrift -gen js:node,ts --out test/gen-nodejs ../../test/ThriftTest.thrift',
        ].join(' && ')
      },
      ThriftBrowserifyNodeInt64: {
        command: [
          './node_modules/browserify/bin/cmd.js ./node_modules/node-int64/Int64.js -s Int64 -o test/build/js/lib/Int64.js',
          './node_modules/browserify/bin/cmd.js ../nodejs/lib/thrift/int64_util.js -s Int64Util -o test/build/js/lib/Int64Util.js',
          './node_modules/browserify/bin/cmd.js ./node_modules/json-int64/index.js -s JSONInt64 -o test/build/js/lib/JSONInt64.js'
        ].join(' && ')
      },
      ThriftGenInt64: {
        command: '../../compiler/cpp/thrift -gen js:ts -o test ../../test/Int64Test.thrift'
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
      BuildTS: {
        options: {
          execOptions: {
            cwd: "./test",
          }
        },
        command : "../node_modules/typescript/bin/tsc --listFiles --outDir build/ts"
      },
      BrowserifyCompiledTS: {
        command: [
          "./node_modules/browserify/bin/cmd.js test/build/ts/test.js -o test/build/ts/lib/test.js --standalone test",
          "./node_modules/browserify/bin/cmd.js test/build/ts/test-int64.js -o test/build/ts/lib/test-int64.js --standalone testInt64",
        ].join(" && ")
      },
      InstallGeneratedCode: {
        command: [
          "mkdir -p test/build/ts",
          "cp -r test/gen-js test/build/ts"
        ].join(" && ")
      },
    },
    qunit: {
      ThriftJS: {
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
    },
    jshint: {
      // The main Thrift library file. not es6 yet :(
      lib: {
        src: ['../js/src/**/*.js'],
      },
      // The test files use es6
      test: {
        src: ['Gruntfile.js', 'test/*.js'],
        options: {
          esversion: 6,
        }
      },
      gen_js_code: {
        src: ['test/gen-js/*.js'],
      },
      gen_node_code: {
        src: ['test/gen-nodejs/*.js'],
        options: {
          node: true,
        }
      },
    },
  });

  grunt.loadNpmTasks('grunt-contrib-jshint');
  grunt.loadNpmTasks('grunt-contrib-qunit');
  grunt.loadNpmTasks('grunt-shell-spawn');

  grunt.registerTask('wait', 'Wait just one second for the server to start', function () {
    var done = this.async();
    setTimeout(function() {
      done(true);
    }, 1000);
  });

  grunt.registerTask('installAndGenerate', [
    'shell:InstallThriftJS',
    'shell:InstallThriftNodeJSDep',
    'shell:ThriftGen',
    'shell:ThriftBrowserifyNodeInt64',
    'shell:ThriftGenInt64',
    'shell:InstallTestLibs',
    'shell:BuildTS',
    'shell:InstallGeneratedCode',
    'shell:BrowserifyCompiledTS',
  ]);

  grunt.registerTask('test', [
    'installAndGenerate',
    'jshint',
    'shell:ThriftTestServer',
    'wait',
    'qunit:ThriftJS',
    'qunit:ThriftJS_Int64',
    'shell:ThriftTestServer:kill',
  ]);
  grunt.registerTask('default', ['test']);
};
