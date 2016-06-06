/* global exports */

// module Graphics.WebGLTexture


    "use strict";

    exports.asArrayBufferView_ = function (it) {
        return (it);
    };

    exports.loadImage_ = function(name,continuation)
       {return function()
        {var i = new Image();
         i.src = name;
         i.onload = continuation (i);
          };};

    exports.texImage2D__ = function(target,level,internalformat,format,type,pixels)
               {return function()
                {gl.texImage2D(target,level,internalformat,format,type,pixels);};};

    exports.texImage2DNull_ = function(target,level,internalformat,width,height,border,format,type)
               {return function()
                {gl.texImage2D(target,level,internalformat,width,height,border,format,type,null);};};

    exports.bindTexture__ = function(target)
        {return function()
         {gl.bindTexture(target,null);};};

    exports.texSubImage2D__ = function (target,level,xoffset,yoffset,format,type,pixels)
    {return function ()
   {gl.texSubImage2D(target,level,xoffset,yoffset,format,type,pixels);}
   ;}
