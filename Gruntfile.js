module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({ 
  
    libFiles: [
      "src/**/*.purs.hs",
      "bower_components/purescript-*/src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs.hs"
    ],
    
    clean: {
      tests: ["tmp"],
      lib: ["js", "externs"]
    },
  
    "purescript-make": {
      options: {
        tco: true,
        magicDo: true
      },
      lib: {
        src: "<%=libFiles%>"
      }
    },
  
    purescript: {
      tests: {
        options: {
          tco: true,
          magicDo: true,
          module: ["Main"],
          main: true
        },
        src: ["tests/tests.purs.hs", "<%=libFiles%>"],
        dest: "tmp/tests.js"
      }
    },
    
    execute: {
      tests: {
        src: "tmp/tests.js"
      }
    }
      
  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-execute");
  
  grunt.registerTask("test", ["clean:tests", "purescript:tests", "execute:tests"]);
  grunt.registerTask("lib", ["purescript-make:lib"]);
  grunt.registerTask("default", ["test", "lib"]);
};
