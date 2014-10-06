module Main where

import WebGL
import Signal
import Signal.DOM
import Debug.Trace

main = do 
  initGL "canvas"
  vshader <- getShader "shader-vs"
  fshader <- getShader "shader-fs"
  program <- glCreateProgram
  glAttachShader program vshader
  glAttachShader program fshader
  glLinkProgram program
  glUseProgram program
  glClearColor 0.0 0.0 0.0 1.0
  glViewPort 0 0 100 100
  glClear
  return Unit
--main = trace "Hello"
