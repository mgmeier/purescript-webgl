module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs.hs"
    ],

    clean: {
      tests: ["tmp"],
      lib: ["js", "externs"]
    },

    pscMake: ["<%=libFiles%>"],
    dotPsci: ["<%=libFiles%>"],

    psc: {
        options: {
          modules: ["WebGL"],
          main: "WebGL"
        },
       all: {
        src: ["<%=libFiles%>"],
        dest: "dist/main.js"
       }
    }


  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-execute");

  grunt.registerTask("default", ["psc:all"]);
};
