module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    srcFiles: [
      "src/**/*.purs",
      "bower_components/**/src/**/*.purs"
    ],

    psc: {
        options: {
            modules: ["Main"]
        },
      example1: {
      src: ["examples/Example1.purs","<%=srcFiles%>"],
        dest: "dist/Main.js"
      },
      example2: {
      src: ["examples/Example2.purs","<%=srcFiles%>"],
        dest: "dist/Main.js"
      },
      example3: {
      src: ["examples/Example3.purs","<%=srcFiles%>"],
        dest: "dist/Main.js"
      },
      example4: {
      src: ["examples/Example4.purs","<%=srcFiles%>"],
        dest: "dist/Main.js"
      },
      example5: {
      src: ["examples/Example5.purs","<%=srcFiles%>"],
        dest: "dist/Main.js"
      },
      example6: {
      src: ["examples/Example6.purs","<%=srcFiles%>"],
        dest: "dist/Main.js"
      },
      all: {
      src: ["<%=srcFiles%>","examples/Main.purs"],
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
  grunt.registerTask("example1", ["psc:example1"]);
  grunt.registerTask("example2", ["psc:example2"]);
  grunt.registerTask("example3", ["psc:example3"]);
  grunt.registerTask("example4", ["psc:example4"]);
  grunt.registerTask("example5", ["psc:example5"]);
  grunt.registerTask("example6", ["psc:example6"]);
};
