//To build dist/thrift.js, dist/thrift.min.js and doc/*
//run grunt at the command line in this directory.
//Prerequisites:
// Node Setup -   nodejs.org
// Grunt Setup -  npm install  //reads the ./package.json and installs project dependencies

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
            src: ['src/*.js', './README'], 
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
        command: 'mkdir test/build; mkdir test/build/js; cp src/thrift.js test/build/js/thrift.js'
      },
      ThriftGen: {
        command: 'thrift -gen js -gen js:node -o test ../../test/ThriftTest.thrift'
      }
    },
    external_daemon: {
      ThriftTestServer: {
        options: {
          startCheck: function(stdout, stderr) {
            return (/Thrift Server running on port/).test(stdout);
          },
          nodeSpawnOptions: {cwd: "test"}
        },
        cmd: "node",
        args: ["server_http.js"]
      }
    },
    qunit: {
      all: {
        options: {
          urls: [
            'http://localhost:8088/test-nojq.html'
          ]
        }
      }
    },
    jshint: {
      files: ['Gruntfile.js', 'src/**/*.js', 'test/*.js'],
      options: {
        // options here to override JSHint defaults
        globals: {
          jQuery: true,
          console: true,
          module: true,
          document: true
        }
      }
    },
  });

  grunt.loadNpmTasks('grunt-contrib-uglify');
  grunt.loadNpmTasks('grunt-contrib-jshint');
  grunt.loadNpmTasks('grunt-contrib-qunit');
  grunt.loadNpmTasks('grunt-contrib-concat');
  grunt.loadNpmTasks('grunt-jsdoc');
  grunt.loadNpmTasks('grunt-external-daemon');
  grunt.loadNpmTasks('grunt-shell');

  grunt.registerTask('test', ['jshint', 'shell', 'external_daemon', 'qunit']);
  grunt.registerTask('default', ['jshint', 'shell', 'external_daemon', 'qunit', 'concat', 'uglify', 'jsdoc']);
};
