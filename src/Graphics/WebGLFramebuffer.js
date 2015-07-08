/* global exports */

// module Graphics.WebGLFramebuffer

"use strict";

exports.unbindRenderbuffer_ = function(target)
    {return function()
     {gl.bindRenderbuffer(target,null);};};

exports.unbindFramebuffer_ = function(target)
{return function()
 {gl.bindFramebuffer(target,null);};};

 exports.readPixels__ = function(x)
   {return function(y)
    {return function(width)
     {return function(height)
      {return function(format)
       {return function(type)
        {return function(pixels)
         {return function()
          { gl.readPixels(x,y,width,height,format,type,pixels);};};};};};};};};
