/* global exports */

// module Graphics.WebGL

    "use strict";

    exports.shaderBindings_ = function (program) {
          return function() {
            var bindings = {};
            var numUniforms = gl.getProgramParameter(program, gl.ACTIVE_UNIFORMS);
            for (var i = 0; i < numUniforms; i += 1) {
                var uniform = gl.getActiveUniform(program, i);
                var uniformLocation = gl.getUniformLocation(program, uniform.name);
                var newUniform = {};
                newUniform.uLocation=uniformLocation;
                newUniform.uName=uniform.name;
                newUniform.uType=uniform.type;
                bindings[uniform.name]=newUniform;
              }
            var numAttributes = gl.getProgramParameter(program, gl.ACTIVE_ATTRIBUTES);
            for (var i2 = 0; i2 < numAttributes; i2 += 1) {
                var attribute = gl.getActiveAttrib(program, i2);
                var attribLocation = gl.getAttribLocation(program, attribute.name);
                gl.enableVertexAttribArray(attribLocation);
                var newAttr = {};
                newAttr.aLocation=attribLocation;
                newAttr.aName=attribute.name;
                newAttr.aItemType=attribute.type;
                switch (attribute.type) {
                  case gl.FLOAT_VEC2:
                    newAttr.aItemSize=2;
                    break;
                  case gl.FLOAT_VEC3:
                    newAttr.aItemSize=3;
                    break;
                  case gl.FLOAT_VEC4:
                    newAttr.aItemSize=4;
                    break;
                  default:
                    LOG("Unsupported attribute type: " + attribute.type);
                    newAttr.aItemSize=1;
                    break;
                }
                bindings[attribute.name]=newAttr;
            }
            return bindings;
        };};


    exports.initGL_ = function (canvasId, attr) {
            return function() {
              var canvas = document.getElementById(canvasId);
              try {
                gl = canvas.getContext("webgl", attr) || canvas.getContext("experimental-webgl", attr);
              }
              catch(e) {return false;}
              if (!gl)
              {
                gl = null;
                return false;
              }
              return true;
          };
      };

        exports.getCanvasWidth_ = function(canvasId) {
          return function() {
            var canvas = document.getElementById(canvasId);
            return canvas.width;
            };
        };

        exports.getCanvasHeight_ = function(canvasId) {
          return function() {
            var canvas = document.getElementById(canvasId);
            return canvas.height;
            };
        };

        exports.requestAnimationFrame_ = function(x){
            if (typeof rAF === 'undefined') {
               var rAF = (function(){
                  return  window.requestAnimationFrame       ||
                          window.webkitRequestAnimationFrame ||
                          window.mozRequestAnimationFrame    ||
                          function( callback ){
                            window.setTimeout(callback, 1000 / 60);
                          };
                      })();
            }
            return function(){
              return rAF(x);
            };
        };

      exports.bufferData__ = function(target,data,usage)
         {return function()
          {gl.bufferData(target,data,usage);};};

      exports.bufferSubData__ = function(target,offset,data)
         {return function()
          {gl.bufferSubData(target,offset,data);};};
