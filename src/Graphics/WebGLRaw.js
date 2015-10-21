// Auto generated: don't change manually, use purescript-webgl-generator to modify!!
/* global exports */

// module Graphics.WebGLRaw

  "use strict";


    exports.getContextAttributes_ = function () 
    {return function ()
   {var res = gl.getContextAttributes();
    if (res === undefined){
      throw "Undefined in  getContextAttributes"};
    return res;}
   ;}
    
    exports.isContextLost_ = function () 
    {return function ()
   {var res = gl.isContextLost();
    if (res === undefined){
      throw "Undefined in  isContextLost"};
    return res;}
   ;}
    
    exports.getSupportedExtensions_ = function () 
    {return function ()
   {var res = gl.getSupportedExtensions();
    if (res === undefined){
      throw "Undefined in  getSupportedExtensions"};
    return res;}
   ;}
    
    exports.getExtension_ = function (name)
    {return function ()
   {var res = gl.getExtension(name);
    if (res === undefined){
      throw "Undefined in  getExtension"};
    return res;}
   ;}
    
    exports.activeTexture_ = function (texture)
    {return function ()
   {gl.activeTexture(texture);}
   ;}
    
    exports.attachShader_ = function (program,shader)
    {return function ()
   {gl.attachShader(program,shader);}
   ;}
    
    exports.bindAttribLocation_ = function (program,index,name)
    {return function ()
   {gl.bindAttribLocation(program,index,name);}
   ;}
    
    exports.bindBuffer_ = function (target,buffer)
    {return function ()
   {gl.bindBuffer(target,buffer);}
   ;}
    
    exports.bindFramebuffer_ = function (target,framebuffer)
    {return function ()
   {gl.bindFramebuffer(target,framebuffer);}
   ;}
    
    exports.bindRenderbuffer_ = function (target,renderbuffer)
    {return function ()
   {gl.bindRenderbuffer(target,renderbuffer);}
   ;}
    
    exports.bindTexture_ = function (target,texture)
    {return function ()
   {gl.bindTexture(target,texture);}
   ;}
    
    exports.blendColor_ = function (red,green,blue,alpha)
    {return function ()
   {gl.blendColor(red,green,blue,alpha);}
   ;}
    
    exports.blendEquation_ = function (mode)
    {return function ()
   {gl.blendEquation(mode);}
   ;}
    
    exports.blendEquationSeparate_ = function (modeRGB,modeAlpha)
    {return function ()
   {gl.blendEquationSeparate(modeRGB,modeAlpha);}
   ;}
    
    exports.blendFunc_ = function (sfactor,dfactor)
    {return function ()
   {gl.blendFunc(sfactor,dfactor);}
   ;}
    
    exports.blendFuncSeparate_ = function (srcRGB,dstRGB,srcAlpha,dstAlpha)
    {return function ()
   {gl.blendFuncSeparate(srcRGB,dstRGB,srcAlpha,dstAlpha);}
   ;}
    
    exports.bufferData_ = function (target,data,usage)
    {return function ()
   {gl.bufferData(target,data,usage);}
   ;}
    
    exports.bufferSubData_ = function (target,offset,data)
    {return function ()
   {gl.bufferSubData(target,offset,data);}
   ;}
    
    exports.checkFramebufferStatus_ = function (target)
    {return function ()
   {var res = gl.checkFramebufferStatus(target);
    if (res === undefined){
      throw "Undefined in  checkFramebufferStatus"};
    return res;}
   ;}
    
    exports.clear_ = function (mask)
    {return function ()
   {gl.clear(mask);}
   ;}
    
    exports.clearColor_ = function (red,green,blue,alpha)
    {return function ()
   {gl.clearColor(red,green,blue,alpha);}
   ;}
    
    exports.clearDepth_ = function (depth)
    {return function ()
   {gl.clearDepth(depth);}
   ;}
    
    exports.clearStencil_ = function (s)
    {return function ()
   {gl.clearStencil(s);}
   ;}
    
    exports.colorMask_ = function (red,green,blue,alpha)
    {return function ()
   {gl.colorMask(red,green,blue,alpha);}
   ;}
    
    exports.compileShader_ = function (shader)
    {return function ()
   {gl.compileShader(shader);}
   ;}
    
    exports.copyTexImage2D_ = function (target,level,internalformat,x,y,width,height,border)
    {return function ()
   {gl.copyTexImage2D(target,level,internalformat,x,y,width,height,border);}
   ;}
    
    exports.copyTexSubImage2D_ = function (target,level,xoffset,yoffset,x,y,width,height)
    {return function ()
   {gl.copyTexSubImage2D(target,level,xoffset,yoffset,x,y,width,height);}
   ;}
    
    exports.createBuffer_ = function () 
    {return function ()
   {var res = gl.createBuffer();
    if (res === undefined){
      throw "Undefined in  createBuffer"};
    return res;}
   ;}
    
    exports.createFramebuffer_ = function () 
    {return function ()
   {var res = gl.createFramebuffer();
    if (res === undefined){
      throw "Undefined in  createFramebuffer"};
    return res;}
   ;}
    
    exports.createProgram_ = function () 
    {return function ()
   {var res = gl.createProgram();
    if (res === undefined){
      throw "Undefined in  createProgram"};
    return res;}
   ;}
    
    exports.createRenderbuffer_ = function () 
    {return function ()
   {var res = gl.createRenderbuffer();
    if (res === undefined){
      throw "Undefined in  createRenderbuffer"};
    return res;}
   ;}
    
    exports.createShader_ = function (type)
    {return function ()
   {var res = gl.createShader(type);
    if (res === undefined){
      throw "Undefined in  createShader"};
    return res;}
   ;}
    
    exports.createTexture_ = function () 
    {return function ()
   {var res = gl.createTexture();
    if (res === undefined){
      throw "Undefined in  createTexture"};
    return res;}
   ;}
    
    exports.cullFace_ = function (mode)
    {return function ()
   {gl.cullFace(mode);}
   ;}
    
    exports.deleteBuffer_ = function (buffer)
    {return function ()
   {gl.deleteBuffer(buffer);}
   ;}
    
    exports.deleteFramebuffer_ = function (framebuffer)
    {return function ()
   {gl.deleteFramebuffer(framebuffer);}
   ;}
    
    exports.deleteProgram_ = function (program)
    {return function ()
   {gl.deleteProgram(program);}
   ;}
    
    exports.deleteRenderbuffer_ = function (renderbuffer)
    {return function ()
   {gl.deleteRenderbuffer(renderbuffer);}
   ;}
    
    exports.deleteShader_ = function (shader)
    {return function ()
   {gl.deleteShader(shader);}
   ;}
    
    exports.deleteTexture_ = function (texture)
    {return function ()
   {gl.deleteTexture(texture);}
   ;}
    
    exports.depthFunc_ = function (func)
    {return function ()
   {gl.depthFunc(func);}
   ;}
    
    exports.depthMask_ = function (flag)
    {return function ()
   {gl.depthMask(flag);}
   ;}
    
    exports.depthRange_ = function (zNear,zFar)
    {return function ()
   {gl.depthRange(zNear,zFar);}
   ;}
    
    exports.detachShader_ = function (program,shader)
    {return function ()
   {gl.detachShader(program,shader);}
   ;}
    
    exports.disable_ = function (cap)
    {return function ()
   {gl.disable(cap);}
   ;}
    
    exports.disableVertexAttribArray_ = function (index)
    {return function ()
   {gl.disableVertexAttribArray(index);}
   ;}
    
    exports.drawArrays_ = function (mode,first,count)
    {return function ()
   {gl.drawArrays(mode,first,count);}
   ;}
    
    exports.drawElements_ = function (mode,count,type,offset)
    {return function ()
   {gl.drawElements(mode,count,type,offset);}
   ;}
    
    exports.enable_ = function (cap)
    {return function ()
   {gl.enable(cap);}
   ;}
    
    exports.enableVertexAttribArray_ = function (index)
    {return function ()
   {gl.enableVertexAttribArray(index);}
   ;}
    
    exports.finish_ = function () 
    {return function ()
   {gl.finish();}
   ;}
    
    exports.flush_ = function () 
    {return function ()
   {gl.flush();}
   ;}
    
    exports.framebufferRenderbuffer_ = function (target,attachment,renderbuffertarget,renderbuffer)
    {return function ()
   {gl.framebufferRenderbuffer(target,attachment,renderbuffertarget,renderbuffer);}
   ;}
    
    exports.framebufferTexture2D_ = function (target,attachment,textarget,texture,level)
    {return function ()
   {gl.framebufferTexture2D(target,attachment,textarget,texture,level);}
   ;}
    
    exports.frontFace_ = function (mode)
    {return function ()
   {gl.frontFace(mode);}
   ;}
    
    exports.generateMipmap_ = function (target)
    {return function ()
   {gl.generateMipmap(target);}
   ;}
    
    exports.getActiveAttrib_ = function (program,index)
    {return function ()
   {var res = gl.getActiveAttrib(program,index);
    if (res === undefined){
      throw "Undefined in  getActiveAttrib"};
    return res;}
   ;}
    
    exports.getActiveUniform_ = function (program,index)
    {return function ()
   {var res = gl.getActiveUniform(program,index);
    if (res === undefined){
      throw "Undefined in  getActiveUniform"};
    return res;}
   ;}
    
    exports.getAttachedShaders_ = function (program)
    {return function ()
   {var res = gl.getAttachedShaders(program);
    if (res === undefined){
      throw "Undefined in  getAttachedShaders"};
    return res;}
   ;}
    
    exports.getAttribLocation_ = function (program,name)
    {return function ()
   {var res = gl.getAttribLocation(program,name);
    if (res === undefined){
      throw "Undefined in  getAttribLocation"};
    return res;}
   ;}
    
    exports.getParameter_ = function (pname)
    {return function ()
   {var res = gl.getParameter(pname);
    if (res === undefined){
      throw "Undefined in  getParameter"};
    return res;}
   ;}
    
    exports.getBufferParameter_ = function (target,pname)
    {return function ()
   {var res = gl.getBufferParameter(target,pname);
    if (res === undefined){
      throw "Undefined in  getBufferParameter"};
    return res;}
   ;}
    
    exports.getError_ = function () 
    {return function ()
   {var res = gl.getError();
    if (res === undefined){
      throw "Undefined in  getError"};
    return res;}
   ;}
    
    exports.getFramebufferAttachmentParameter_ = function (target,attachment,pname)
    {return function ()
   {var res = gl.getFramebufferAttachmentParameter(target,attachment,pname);
    if (res === undefined){
      throw "Undefined in  getFramebufferAttachmentParameter"};
    return res;}
   ;}
    
    exports.getProgramParameter_ = function (program,pname)
    {return function ()
   {var res = gl.getProgramParameter(program,pname);
    if (res === undefined){
      throw "Undefined in  getProgramParameter"};
    return res;}
   ;}
    
    exports.getProgramInfoLog_ = function (program)
    {return function ()
   {var res = gl.getProgramInfoLog(program);
    if (res === undefined){
      throw "Undefined in  getProgramInfoLog"};
    return res;}
   ;}
    
    exports.getRenderbufferParameter_ = function (target,pname)
    {return function ()
   {var res = gl.getRenderbufferParameter(target,pname);
    if (res === undefined){
      throw "Undefined in  getRenderbufferParameter"};
    return res;}
   ;}
    
    exports.getShaderParameter_ = function (shader,pname)
    {return function ()
   {var res = gl.getShaderParameter(shader,pname);
    if (res === undefined){
      throw "Undefined in  getShaderParameter"};
    return res;}
   ;}
    
    exports.getShaderInfoLog_ = function (shader)
    {return function ()
   {var res = gl.getShaderInfoLog(shader);
    if (res === undefined){
      throw "Undefined in  getShaderInfoLog"};
    return res;}
   ;}
    
    exports.getShaderSource_ = function (shader)
    {return function ()
   {var res = gl.getShaderSource(shader);
    if (res === undefined){
      throw "Undefined in  getShaderSource"};
    return res;}
   ;}
    
    exports.getTexParameter_ = function (target,pname)
    {return function ()
   {var res = gl.getTexParameter(target,pname);
    if (res === undefined){
      throw "Undefined in  getTexParameter"};
    return res;}
   ;}
    
    exports.getUniform_ = function (program,location)
    {return function ()
   {var res = gl.getUniform(program,location);
    if (res === undefined){
      throw "Undefined in  getUniform"};
    return res;}
   ;}
    
    exports.getUniformLocation_ = function (program,name)
    {return function ()
   {var res = gl.getUniformLocation(program,name);
    if (res === undefined){
      throw "Undefined in  getUniformLocation"};
    return res;}
   ;}
    
    exports.getVertexAttrib_ = function (index,pname)
    {return function ()
   {var res = gl.getVertexAttrib(index,pname);
    if (res === undefined){
      throw "Undefined in  getVertexAttrib"};
    return res;}
   ;}
    
    exports.getVertexAttribOffset_ = function (index,pname)
    {return function ()
   {var res = gl.getVertexAttribOffset(index,pname);
    if (res === undefined){
      throw "Undefined in  getVertexAttribOffset"};
    return res;}
   ;}
    
    exports.hint_ = function (target,mode)
    {return function ()
   {gl.hint(target,mode);}
   ;}
    
    exports.isBuffer_ = function (buffer)
    {return function ()
   {var res = gl.isBuffer(buffer);
    if (res === undefined){
      throw "Undefined in  isBuffer"};
    return res;}
   ;}
    
    exports.isEnabled_ = function (cap)
    {return function ()
   {var res = gl.isEnabled(cap);
    if (res === undefined){
      throw "Undefined in  isEnabled"};
    return res;}
   ;}
    
    exports.isFramebuffer_ = function (framebuffer)
    {return function ()
   {var res = gl.isFramebuffer(framebuffer);
    if (res === undefined){
      throw "Undefined in  isFramebuffer"};
    return res;}
   ;}
    
    exports.isProgram_ = function (program)
    {return function ()
   {var res = gl.isProgram(program);
    if (res === undefined){
      throw "Undefined in  isProgram"};
    return res;}
   ;}
    
    exports.isRenderbuffer_ = function (renderbuffer)
    {return function ()
   {var res = gl.isRenderbuffer(renderbuffer);
    if (res === undefined){
      throw "Undefined in  isRenderbuffer"};
    return res;}
   ;}
    
    exports.isShader_ = function (shader)
    {return function ()
   {var res = gl.isShader(shader);
    if (res === undefined){
      throw "Undefined in  isShader"};
    return res;}
   ;}
    
    exports.isTexture_ = function (texture)
    {return function ()
   {var res = gl.isTexture(texture);
    if (res === undefined){
      throw "Undefined in  isTexture"};
    return res;}
   ;}
    
    exports.lineWidth_ = function (width)
    {return function ()
   {gl.lineWidth(width);}
   ;}
    
    exports.linkProgram_ = function (program)
    {return function ()
   {gl.linkProgram(program);}
   ;}
    
    exports.pixelStorei_ = function (pname,param)
    {return function ()
   {gl.pixelStorei(pname,param);}
   ;}
    
    exports.polygonOffset_ = function (factor,units)
    {return function ()
   {gl.polygonOffset(factor,units);}
   ;}
    
    exports.readPixels_ = function (x,y,width,height,format,type,pixels)
    {return function ()
   {gl.readPixels(x,y,width,height,format,type,pixels);}
   ;}
    
    exports.renderbufferStorage_ = function (target,internalformat,width,height)
    {return function ()
   {gl.renderbufferStorage(target,internalformat,width,height);}
   ;}
    
    exports.sampleCoverage_ = function (value,invert)
    {return function ()
   {gl.sampleCoverage(value,invert);}
   ;}
    
    exports.scissor_ = function (x,y,width,height)
    {return function ()
   {gl.scissor(x,y,width,height);}
   ;}
    
    exports.shaderSource_ = function (shader,source)
    {return function ()
   {gl.shaderSource(shader,source);}
   ;}
    
    exports.stencilFunc_ = function (func,ref,mask)
    {return function ()
   {gl.stencilFunc(func,ref,mask);}
   ;}
    
    exports.stencilFuncSeparate_ = function (face,func,ref,mask)
    {return function ()
   {gl.stencilFuncSeparate(face,func,ref,mask);}
   ;}
    
    exports.stencilMask_ = function (mask)
    {return function ()
   {gl.stencilMask(mask);}
   ;}
    
    exports.stencilMaskSeparate_ = function (face,mask)
    {return function ()
   {gl.stencilMaskSeparate(face,mask);}
   ;}
    
    exports.stencilOp_ = function (fail,zfail,zpass)
    {return function ()
   {gl.stencilOp(fail,zfail,zpass);}
   ;}
    
    exports.stencilOpSeparate_ = function (face,fail,zfail,zpass)
    {return function ()
   {gl.stencilOpSeparate(face,fail,zfail,zpass);}
   ;}
    
    exports.texImage2D_ = function (target,level,internalformat,width,height,border,format,type,pixels)
    {return function ()
   {gl.texImage2D(target,level,internalformat,width,height,border,format,type,pixels);}
   ;}
    
    exports.texParameterf_ = function (target,pname,param)
    {return function ()
   {gl.texParameterf(target,pname,param);}
   ;}
    
    exports.texParameteri_ = function (target,pname,param)
    {return function ()
   {gl.texParameteri(target,pname,param);}
   ;}
    
    exports.texSubImage2D_ = function (target,level,xoffset,yoffset,width,height,format,type,pixels)
    {return function ()
   {gl.texSubImage2D(target,level,xoffset,yoffset,width,height,format,type,pixels);}
   ;}
    
    exports.uniform1f_ = function (location,x)
    {return function ()
   {gl.uniform1f(location,x);}
   ;}
    
    exports.uniform1fv_ = function (location,v)
    {return function ()
   {gl.uniform1fv(location,v);}
   ;}
    
    exports.uniform1i_ = function (location,x)
    {return function ()
   {gl.uniform1i(location,x);}
   ;}
    
    exports.uniform1iv_ = function (location,v)
    {return function ()
   {gl.uniform1iv(location,v);}
   ;}
    
    exports.uniform2f_ = function (location,x,y)
    {return function ()
   {gl.uniform2f(location,x,y);}
   ;}
    
    exports.uniform2fv_ = function (location,v)
    {return function ()
   {gl.uniform2fv(location,v);}
   ;}
    
    exports.uniform2i_ = function (location,x,y)
    {return function ()
   {gl.uniform2i(location,x,y);}
   ;}
    
    exports.uniform2iv_ = function (location,v)
    {return function ()
   {gl.uniform2iv(location,v);}
   ;}
    
    exports.uniform3f_ = function (location,x,y,z)
    {return function ()
   {gl.uniform3f(location,x,y,z);}
   ;}
    
    exports.uniform3fv_ = function (location,v)
    {return function ()
   {gl.uniform3fv(location,v);}
   ;}
    
    exports.uniform3i_ = function (location,x,y,z)
    {return function ()
   {gl.uniform3i(location,x,y,z);}
   ;}
    
    exports.uniform3iv_ = function (location,v)
    {return function ()
   {gl.uniform3iv(location,v);}
   ;}
    
    exports.uniform4f_ = function (location,x,y,z,w)
    {return function ()
   {gl.uniform4f(location,x,y,z,w);}
   ;}
    
    exports.uniform4fv_ = function (location,v)
    {return function ()
   {gl.uniform4fv(location,v);}
   ;}
    
    exports.uniform4i_ = function (location,x,y,z,w)
    {return function ()
   {gl.uniform4i(location,x,y,z,w);}
   ;}
    
    exports.uniform4iv_ = function (location,v)
    {return function ()
   {gl.uniform4iv(location,v);}
   ;}
    
    exports.uniformMatrix2fv_ = function (location,transpose,value)
    {return function ()
   {gl.uniformMatrix2fv(location,transpose,value);}
   ;}
    
    exports.uniformMatrix3fv_ = function (location,transpose,value)
    {return function ()
   {gl.uniformMatrix3fv(location,transpose,value);}
   ;}
    
    exports.uniformMatrix4fv_ = function (location,transpose,value)
    {return function ()
   {gl.uniformMatrix4fv(location,transpose,value);}
   ;}
    
    exports.useProgram_ = function (program)
    {return function ()
   {gl.useProgram(program);}
   ;}
    
    exports.validateProgram_ = function (program)
    {return function ()
   {gl.validateProgram(program);}
   ;}
    
    exports.vertexAttrib1f_ = function (indx,x)
    {return function ()
   {gl.vertexAttrib1f(indx,x);}
   ;}
    
    exports.vertexAttrib1fv_ = function (indx,values)
    {return function ()
   {gl.vertexAttrib1fv(indx,values);}
   ;}
    
    exports.vertexAttrib2f_ = function (indx,x,y)
    {return function ()
   {gl.vertexAttrib2f(indx,x,y);}
   ;}
    
    exports.vertexAttrib2fv_ = function (indx,values)
    {return function ()
   {gl.vertexAttrib2fv(indx,values);}
   ;}
    
    exports.vertexAttrib3f_ = function (indx,x,y,z)
    {return function ()
   {gl.vertexAttrib3f(indx,x,y,z);}
   ;}
    
    exports.vertexAttrib3fv_ = function (indx,values)
    {return function ()
   {gl.vertexAttrib3fv(indx,values);}
   ;}
    
    exports.vertexAttrib4f_ = function (indx,x,y,z,w)
    {return function ()
   {gl.vertexAttrib4f(indx,x,y,z,w);}
   ;}
    
    exports.vertexAttrib4fv_ = function (indx,values)
    {return function ()
   {gl.vertexAttrib4fv(indx,values);}
   ;}
    
    exports.vertexAttribPointer_ = function (indx,size,type,normalized,stride,offset)
    {return function ()
   {gl.vertexAttribPointer(indx,size,type,normalized,stride,offset);}
   ;}
    
    exports.viewport_ = function (x,y,width,height)
    {return function ()
   {gl.viewport(x,y,width,height);}
   ;}
    
