/* global exports */

// module Graphics.WebGLTexture

(function () {
    "use strict";

    exports.loadImage = function(name)
     {return function(continuation)
       {return function()
        {var i = new Image();
         i.src = name;
         i.onload = continuation (i);
          };};};

    exports.texImage2D__ = function(target)
       {return function(level)
        {return function(internalformat)
            {return function(format)
             {return function(type)
              {return function(pixels)
               {return function()
                {gl.texImage2D(target,level,internalformat,format,type,pixels);};};};};};};};

    exports.texImage2DNull_ = function(target)
       {return function(level)
        {return function(internalformat)
         {return function(width)
          {return function(height)
           {return function(border)
            {return function(format)
             {return function(type)
               {return function()
                {gl.texImage2D(target,level,internalformat,width,height,border,format,type,null);};};};};};};};};};

    exports.bindTexture__ = function(target)
        {return function()
         {gl.bindTexture(target,null);};};
})();
