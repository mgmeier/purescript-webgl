module WebGL where

import Control.Monad.Eff

foreign import data GLContext :: *

foreign import data GLShader :: *

foreign import data GLProgram :: *

foreign import initGL """
        function initGL(canvasId) {
          return function() {
            var canvas = document.getElementById(canvasId);
            try {
            window.gl = canvas.getContext("webgl") || canvas.getContext("experimental-webgl");
            }
            catch(e) {}
            if (!window.gl)
            {
              alert("Unable to initialize WebGL. Your browser may not support it.");
              gl = null;
              return false;
            }
            window.gl.viewPortWidth = canvas.width;
            window.gl.viewPortHeight = canvas.height;
            console.log("Init WebGL Ok");
            return true;
            }

        }""" :: forall eff.String -> (Eff (eff) Boolean)

foreign import glClearColor """
        function glClearColor(r) {
          return function(g) {
          return function(b) {
          return function(a) {
          return function()  {
            window.gl.clearColor(r,g,b,a);
        };};};};}""" :: forall eff.Number -> Number -> Number -> Number -> (Eff (eff) Unit)

foreign import glViewPort """
        function glViewPort(x) {
          return function(y) {
          return function(w) {
          return function(h) {
          return function()  {
            window.gl.viewport(x,y,w,h);
        };};};};}""" :: forall eff.Number -> Number -> Number -> Number -> (Eff (eff) Unit)

foreign import glClear """
        function glClear() {
            window.gl.clear(window.gl.COLOR_BUFFER_BIT | window.gl.DEPTH_BUFFER_BIT);
            window.gl.viewPortHeight = canvas.height;
        }""" :: forall eff.Eff (eff) Unit

foreign import getShader """
        function getShader(id) {
          return function() {
          var shaderScript = document.getElementById(id);
          if (!shaderScript) {
            return null;
          }
          var str = '';
          var k = shaderScript.firstChild;
          while (k) {
            if (k.nodeType == 3) {
              str += k.textContent;
            }
            k = k.nextSibling;
          }
          var shader;
          if (shaderScript.type == 'x-shader/x-fragment') {
            shader = window.gl.createShader(window.gl.FRAGMENT_SHADER);
          } else if (shaderScript.type == 'x-shader/x-vertex') {
            shader = window.gl.createShader(gl.VERTEX_SHADER);
          } else {
            return null;
          }
          console.log("shader:", shader);
          window.gl.shaderSource(shader, str);
          window.gl.compileShader(shader);
          return shader;
        };}""" :: forall eff.String -> (Eff (eff) GLShader)

foreign import glCreateProgram """
        function glCreateProgram() {
            return window.gl.createProgram();
        }""" :: forall eff.Eff (eff) GLProgram

foreign import glAttachShader """
        function glAttachShader(program) {
          return function(shader) {
          return function() {
            window.gl.attachShader(program, shader);
            console.log("as " ,program);
            return {};
        };};}""" :: forall eff.GLProgram -> GLShader -> Eff (eff) Unit

foreign import glLinkProgram """
        function glLinkProgram(program) {
          return function() {
            window.gl.linkProgram(program);
            console.log(program);
            return {};
        };}""" :: forall eff.GLProgram -> Eff (eff) Unit

foreign import glUseProgram """
        function glUseProgram(program) {
          return function() {
            window.gl.useProgram(program);
            return {};
        };}""" :: forall eff.GLProgram -> Eff (eff) Unit
