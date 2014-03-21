module.exports = function(grunt) {

    "use strict";

    grunt.initConfig({ 
    
        purescript: {
            options: {
                tco: true,
                magicDo: true
            },
            lib: {
                files: {
                    "js/lib.js":
                        [ "src/**/*.purs.hs"
                        , "bower_components/purescript-*/src/**/*.purs"
                        , "bower_components/purescript-*/src/**/*.purs.hs"
                        ]
                }
            },
            tests: {
                options: {
                    module: ["Main"],
                    main: true
                },
                files: {
                    "js/tests.js": 
                        [ "src/**/*.purs.hs"
                        , "tests/tests.purs.hs"
                        , "bower_components/purescript-*/src/**/*.purs"
                        , "bower_components/purescript-*/src/**/*.purs.hs"
                        ]
                }
            }
        
        }
        
    });

    grunt.loadNpmTasks("grunt-purescript");
    grunt.registerTask("default", ["purescript:lib", "purescript:tests"]);
};
