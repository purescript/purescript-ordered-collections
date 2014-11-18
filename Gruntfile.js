module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({ 
  
    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs",
    ],
    
    clean: {
        lib: ["output"],
        tests: ["tmp"]
    },
  
    pscMake: ["<%=libFiles%>"],
    dotPsci: ["<%=libFiles%>"],
    pscDocs: {
        readme: {
            src: "src/**/*.purs",
            dest: "README.md"
        }
    },
    
    psc: {
      tests: {
        options: {
          module: "Tests",
          main: "Tests"
        },
        src: ["tests/**/*.purs", "<%=libFiles%>"],
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
  
  grunt.registerTask("test", ["clean:tests", "psc", "execute"]);
  grunt.registerTask("make", ["pscMake", "dotPsci", "pscDocs"]);
  grunt.registerTask("default", ["make", "test"]);
};
