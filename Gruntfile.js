module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    srcFiles: ["src/**/*.purs", "bower_components/**/src/**/*.purs"],

    psc: {
        options: {
            modules: ["Main","Data.Matrix4"]
        },
      all: {
      src: ["<%=srcFiles%>"],
        dest: "dist/Main.js"
      }
    },

    dotPsci: ["<%=srcFiles%>"],
    pscDocs: {
    readme: {
        src: "src/**/*.purs",
        dest: "docs/Module.md"
    }
    },
  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-execute");
  grunt.loadNpmTasks("grunt-purescript");
  grunt.registerTask("default", ["psc:all", "dotPsci", "pscDocs"]);
};
