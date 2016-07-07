/* global exports */

// module Graphics.WebGLTexture


    "use strict";

    exports.asArrayBufferView_ = function (it) {
        return (it);
    };

    exports.loadImage_ = function(name) {
        return function(continuation) {
            return function()
                {var i = new Image();
                 i.src = name;
                 i.onload = continuation (i);
                  };
              };
          };

    exports.texImage2D__ = function (target)
    {return function(level)
     {return function(internalformat)
         {return function(format)
          {return function(type)
           {return function(pixels)
            {return function()
             {gl.texImage2D(target,level,internalformat,format,type,pixels);}
            ;}
          ;}
        ;}
      ;}
    ;}
   ;};

    exports.texImage2DNull_ = function (target)
    {return function(level)
     {return function(internalformat)
      {return function(width)
       {return function(height)
        {return function(border)
         {return function(format)
          {return function(type)
            {return function()
             {gl.texImage2D(target,level,internalformat,width,height,border,format,type,null);}
             ;}
            ;}
           ;}
          ;}
         ;}
        ;}
       ;}
      ;}
    ;

    exports.bindTexture__ = function(target)
        {return function()
         {gl.bindTexture(target,null);};};

    exports.texSubImage2D__ = function (target)
    {return function(level)
     {return function(xoffset)
      {return function(yoffset)
       {return function(format)
          {return function(type)
           {return function(pixels)
            {return function()
             {gl.texSubImage2D(target,level,xoffset,yoffset,format,type,pixels);}
             ;}
            ;}
           ;}
          ;}
         ;}
        ;}
       ;}
      ;
