/* @flow */

// This file is part of JEST.
// 
// JEST is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your
// option) any later version.
// 
// JEST is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the LICENSE for more details.
// 
// A copy of the GNU General Public License should have been included
// along with JEST in a file named LICENSE. If not, see
// <http://www.gnu.org/licenses/>.


'use strict';

/*::declare var policy: {levelImpl: () => Level;
                         locationLevel: (chan: string, write: boolean) => Label;};*/

function ESSLField(p/*:Box<any>*/)/*:PropertyDescriptor*/ {
  return {enumerable: false
	 ,writable: true
	 ,configurable: true
	 ,value: p};
}

function ESSLImmutableField(p/*:Box<any>*/)/*:PropertyDescriptor*/ {
  return {enumerable: false
	 ,writable: false
	 ,configurable: false
	 ,value: p};
}

function ObjectCtor()/*:Box<Object>*/ {
  //1
  if (arguments.length > 1) {
    var value/*: Box<any>*/ = arguments[0];
    //1.a
    if (IsObjectBox(value)) return value;
    if (IsPrimBox(value)) {
      //1.b-1.d
      var tv = typeof value.v;
      if (tv === "string" || tv === "boolean" || tv === "number")
	return ToObjectBox(value);
    }
  }
  //4
  this.m.class = "Object";
  this.t = 1;
  return this;
}

var ObjectProto = ObjectProxy(null, {class: "Object"}, false);
var FunctionProto = ObjectProxy(ObjectProto, {class: "Function"}, false);
var ArrayProto = ObjectProxy(ObjectProto, {class: "Array"}, true);
var StringProto = ObjectProxy(ObjectProto, {class: "String"}, false);
var BooleanProto = ObjectProxy(ObjectProto, {class: "Boolean"}, false);
var NumberProto = ObjectProxy(ObjectProto, {class: "Number"}, false);
var ErrorProto = ObjectProxy(ObjectProto, {class: "Error"}, false);

function nativeDefinePropertyProxy (O/*: Box<Object>*/, P/*: Box<string>*/, Attributes/*: Box<Object>*/)/*: void*/ {
    Object.defineProperty(O.v, P.v, ConvertPropertyDescriptor(Attributes.v));
}

var ObjectFields = {
  defineProperties: ESSLField(FunctionProxy(function (O/*: Box<Object>*/, properties/*: Box<Object>*/)/*: void*/ {
    EnsureObjectBox(O);
    var props/*: Box<Object>*/ = ToObjectBox(properties);
    push(props, 1);
    for (var P in props.v) {
      nativeDefinePropertyProxy(O, primlow(P), readField(props, primlow(P)));
    };
    pop(null,1);
  })),
  defineProperty: ESSLField(FunctionProxy(nativeDefinePropertyProxy))
};

var ObjectProtoFields = {toString: ESSLField(FunctionProxy(function () {
    var s/*:string*/;
    if (IsPrimBox(this)) {
      if (typeof this.v === "undefined")
	s = "[object Undefined]";
      else if (this === null)
	s = "[object Null]";
    } else {
      var o/*: Box<Object>*/ = ToObjectBox(this);
      var class_ = o.m.class || "Object";
      s = "[object " + class_ + "]";
    }
    return primbox(s, this.l);
  })),
   valueOf: ESSLField(FunctionProxy(function () {
      return ToObjectBox(this);
   }))
  };

addFields(ObjectProto, ObjectProtoFields);

function FunctionCtor (/*::...x: Array<Box<any>>*/)/*: Box<Function>*/ {
  //Spec 15.3.2.1:
  //1.
  var argCount = arguments.length;
  //2.
  var P = "";
  var Pl = lowlevel();
  //3.
  if (argCount === 0) var body = primlow("");
  //4, 5.
  else {//argCount > 0
    for (var k = 0; k < argCount - 1; k++) {
      var a = ToStringBox(arguments[k]);
      P += (k > 0?",":"") + a.v;
      Pl = Pl.join(a.l);
    }
    //6
    body = ToStringBox(arguments[k]);
  }
  //7-8 -- implicit flow
  // TODO: don't push if no params or if bottom
  if (argCount > 0) pushExceptionLabel(Pl.join(body.l));
  //7-11
  InitFunctionProxy(this, new Function(P, body.v));
  this.l = Pl.join(body.l);
  return this;
}

var FunctionFields = {};

var FunctionProtoFields = {apply: ESSLField(FunctionProxy(function (thisArg/*: Box<any>*/, argArray/*::?: Box<any>*/) {
    //15.3.4.3
    //1.
    pushExceptionLabel(this.l);
    if (!IsFunBox(this)) throwTypeError();
    //2-3.
    if (typeof argArray === 'undefined' || argArray.v === null || typeof(argArray.v) === 'undefined')
      //9
      return invokeCommon(this, thisArg, []);
    else {
      /*:: if (argArray instanceof Box) {*/
      pushExceptionLabel(argArray.l);
      if (!IsObjectBox(argArray))
	throwTypeError();
      //4
      var len = readField(argArray, primlow("length"));
      //5
      var n/*:Box<number>*/ = ToUInt32Box(len);
      //7-8
      var argList = new Array(n.v);
      for (var index = 0; index < n.v; index++) {
	argList[index] = readField(argArray, primlow(index.toString()));
      }
      //9
      return invokeCommon(this, thisArg, argList);
      /*:: }*/
    }
  }))
  ,toString: ESSLField(FunctionProxy(function ()/*:Box<string>*/ {
    //15.3.4.2
    if (IsFunBox(this)) {
      if(IsObjectBox(this)) return primbox(this.m.fun.toString(), this.l);
      else return primbox(this.v.toString(), this.l);
    } else throwTypeError();
    /*:: return primbox("", this.l);*/
  }))
  };

addFields(FunctionProto, FunctionProtoFields);

function ArrayCtor ()/*: Box<Object>*/ {
  var len/*:number*//*::=0*/;
  if (arguments.length == 1) len = ToNumberBox(arguments[0]).v;
  else {
    len = arguments.length;
    for (var i = 0; i < len; this.v[i] = arguments[i], i++);
  }
  Object.defineProperty(this.v, "length", {configurable: false, enumerable: false,
  writable: false, value: newref(getArrayLength, setArrayLength)});
  this.m = {length: len
	   ,class: "Array"};
  this.t = 5; //1010b
  return this;
}

function getArrayLength()/*:Box<number>*/ {
  var t/*:Box<Object>*/ = this;
  var len = t.m.length;
  return primlow(len?len:0);
}

function setArrayLength(l/*:Box<any>*/, Throw/*::?:boolean*/)/*:Box<boolean>*/ {
  function Reject() {
    if(shouldThrowOnFailure) throwTypeError();
    return primbox(false, resultLevel);
  }
  
  //Loosely following 15.4.5.1, step 3; Right now the main difference
  //is that 'length' is always going to be writable no matter
  //what. (Which, I think, does not affect the observable effects)
  //c.
  var ix = ToUInt32Box(l);
  var shouldThrowOnFailure = Throw || false;
  var resultLevel = this.l.join(l.l);
  if (shouldThrowOnFailure) pushExceptionLabel(resultLevel);
  var newLen = ix.v;
  var len = this.m.length;
  var oldLen = len?len:0;
  //d.
  var validIndex = ix.v.toString() === ToStringBox(l).v;
  if (!validIndex) throw newObject(globalProxy.v.RangeError, []);
  //f.
  if (newLen >= oldLen) this.m.length = newLen;
  else {
    nsuxCheck(this, l);
    //l.
    for (var i = oldLen; i >= newLen; i--) {
      var deleteSucceeded = delete(this.v[i]);
      if (!deleteSucceeded) {
	this.m.length = i;
	return Reject();
      }
    }
    //m.
    this.m.length = i + 1;
  }
  //n.
  return primbox(true, resultLevel);
}

var ArrayFields = {isArray: ESSLField(FunctionProxy(function (arg/*:Box<any>*/)/*:Box<boolean>*/ {
  return primbox(IsObjectBox(arg) && IsArrayBox(arg), arg.l);
}))};


var ArrayProtoFields = 
  {toString: ESSLField(FunctionProxy(function ()/*:Box<string>*/ {
    //15.4.4.2
    //1.
    var array = ToObjectBox(this);
    //2.
    var func = readField(array, primlow("join"));
    if (!IsFunBox(func)) {
      //3.
      func = ObjectProto.v.toString;
    }
    return invokeCommon(func, this, []);
   })),
   // valueOf: ESSLField(FunctionProxy(function ()/*:Box<Object>*/ {
   //   return ToObjectBox(this);
   // })),
   indexOf: ESSLField(FunctionProxy(function (searchElement/*:Box<any>*/, fromIndex/*::?:Box<any>*/)/*:Box<number>*/ {
       //15.4.4.14
       //1.
       var O = ToObjectBox(this);
       //2.
       var lenValue = readField(O, primlow("length"));
       var retLabel = lenValue.l;
       //3.
       var len = ToUInt32Box(lenValue).v;
       //4.
       if (len === 0) return primbox(-1, retLabel);
       //5.
       var n = fromIndex ? ToIntegerBox(fromIndex) : primlow(0);
       retLabel = retLabel.join(n.l);
       //6.
       if (n.v >= len) return primbox(-1, retLabel);
       //7,8.
       var k/*:number*/ = n.v >=0 ? n.v : len - Math.abs(n.v);
       //9.
       while (k < len) {
	   //a.
	   var kPresent = typeof(O.v[k.toString()]) !== 'undefined';
	   //b.
	   if (kPresent) {
	       var elementK = readField(O, primlow(k));
	       if (AbstractStrictEqCmp(searchElement, elementK)) 
		   return primbox(k, retLabel.join(searchElement.l).join(elementK.l));
	   }
	   //c.
	   k++;
       }
       //10.
       return primbox(-1, retLabel);
   })),
   join: ESSLField(FunctionProxy(function (separator/*::?: Box<any>*/) {
     //15.4.4.5
     //10.c
     function convert (el/*: Box<any>*/)/*: Box<string>*/ {
       var v = el.v;
       if (v === (void 0) || v === null) return primbox("", el.l);
       else return ToStringBox(el);
     }
     //1.
     var O = ToObjectBox(this);
     //2, 3.
     var len = ToUInt32Box(readField(O, primlow("length")));
     //4, 5.
     var sep = separator ? ToStringBox(separator) : primlow(",");
     //6.
     if (len.v === 0) return primbox("", this.l);
     //7-10
     var R = "", k = 0, level = len.l.join(this.l);
     while (k < len.v) {
       if (k > 0) R += sep.v;
       var s = convert(readField(O, primbox(k, len.l)));
       R += s.v;
       level = level.join(sep.l).join(s.l);
       k++;
     }
     //11
     return primbox(R, level);

   })),
   splice: ESSLField(FunctionProxy(function (start/*:Box<any>*/, deleteCount/*:Box<any>*/){
       //15.4.4.12
       //1.
       var O = ToObjectBox(this);
       //2.
       var A = newObject(ArrayConstructor, []);
       //3.
       var lenVal = readField(O, primlow("length"));
       var retLabel = lenVal.l;
       //4.
       var len/*:number*/ = ToUInt32Box(lenVal).v;
       //5.
       var relativeStart/*:Box<number>*/ = ToIntegerBox(start);
       //6.
       var actualStart/*:number*/ = relativeStart.v<0 ? Math.max(len + relativeStart.v, 0)
                                                      : Math.min(relativeStart.v, len);
       var actualStartLabel = lenVal.l.join(start.l);
       //7.
       var actualDeleteCount/*:number*/ = Math.min(Math.max(ToIntegerBox(deleteCount).v, 0)
						  ,len - actualStart);
       var actualDeleteCountLabel = lenVal.l.join(deleteCount.l).join(actualStartLabel);
       //8,9.
       for (var k = 0; k < actualDeleteCount; k++) {
	   var from = actualStart + k;
	   if (O.v[from]) {
	       var kbox = primbox(k, actualDeleteCountLabel);
	       var fromValue = readField(O, kbox);
	       assignField(A, kbox, fromValue);
	   }
       }
       //10.
       var items = new Array(arguments.length - 2);
       for (var i = 2; i < arguments.length; i++) 
	   items[i-2] = arguments[i];
       //11.
       var itemCount = items.length;
       if (itemCount < actualDeleteCount) {
           //12.
	   for (k = actualStart; k < len - actualDeleteCount; k++) {
	       from = k + actualDeleteCount;
	       var to/*:number*/ = k + itemCount;
	       var toBox = primbox(k, pclabel.join(actualDeleteCountLabel));
	       if (O.v[from]) {
		   var fromBox/*:Box<number>*/ = primbox(from, pclabel.join(actualDeleteCountLabel));
		   fromValue = readField(O, fromBox);
		   assignField(O, toBox, fromValue);
	       } else deleteField(O, toBox);
	   }
       } else {
	   //13.
	   for (k = len - actualDeleteCount; k > actualStart; k--) {
	       from = k + actualDeleteCount - 1;
	       to   = k + itemCount - 1;
	       toBox = primbox(k, pclabel.join(actualDeleteCountLabel));

	       if (O.v[from]) {
		   fromBox = primbox(from, pclabel.join(actualDeleteCountLabel));
		   fromValue = readField(O, fromBox);
		   assignField(O, toBox, fromValue);
	       } else deleteField(O, toBox);
	   }
       }
       //14,15.
       for (k = actualStart; items.length > 0; k--) {
	   var E = items.shift();
	   assignField(O, primbox(k, pclabel.join(actualStartLabel)), E);
       }
       //16.
       O.m.length = len - actualDeleteCount + itemCount;
       //17.
       return A;
   })),
   push: ESSLField(FunctionProxy(function() {
     'use strict';
     //no exceptions
     var lenval = readField(this,primlow("length")); //2
     var n/*:Box<number>*/ = ToUInt32Box(lenval); //3
     for (var i = n; i.v < arguments.length; i.v++) { //5, 5c
        var e = /*Box<any>*/arguments[i.v]; //5a
        assignField(this, ToStringBox(i), e); 
        this.l = this.l.join(e.l);
     }
     assignField(this, primlow("length"), i); //6
     return join2(i, this.l); //returns length of the array
   })),
   reduce: ESSLField(FunctionProxy(function (callbackfn, initValue){
       var lenbox = readField(this,primlow("length"));
       var len = ToUInt32Box(lenbox).v;
       var ival = initValue || initVar();
       if (initValue) {
	   var acc = initValue;
       } else {if (len > 0) acc = readField(this, primlow(0));
               else {pushExceptionLabel(lenbox.l);
   		     throw newObject(ErrorConstructor, []);
		    }
	      };
       pushExceptionLabel(callbackfn.l);
       push(this, 1);

       var il = lenbox.l;
       var accl;
       for (var i = 0; i < len; i++) {
	 accl = acc.l;
	 acc = invokeFunction(callbackfn, [acc, readField(this, primbox(i, il))
					  ,primbox(i, il),this]);
       }
       pop(null, 1);
   })),
   reverse: ESSLField(FunctionProxy(function (){
     ///TODO
   }))
};

addFields(ArrayProto, ArrayProtoFields);

function StringCtor(value/*::?: Box<any>*/)/*: Box<Object>*/ {
  this.m = {primitiveValue: ""
           ,class: "String"};
  if (value) {
    var sv = ToStringBox(value);
    this.primitiveValue = sv.v;
    this.l.join(sv.l);
  }
  return this;
}

var StringFields = {};

var StringProtoFields =   {valueOf: ESSLField(FunctionProxy(function ()/*:Box<string>*/ {
    var s = "";
    if (IsPrimBox(this)) {
      if (typeof this.v === "string") s = this.v;
      else throwTypeError();
    } else if (this.m.class === "String")
      s = this.m.primitiveValue;
    else throwTypeError();
    return primbox(s, this.l);
  })),
   toString: ESSLField(FunctionProxy(function ()/*:Box<string>*/ {
     return invokeCommon(this, this.v.valueOf, []);
  })),
   indexOf: ESSLField(FunctionProxy(function(s,n) {
     'use strict';
     var o = ToStringBox(this); 
     var c = ToStringBox(s);
     if (typeof(n) !== "undefined") {
       var k = ToNumberBox(n);
       k.v = String.prototype.indexOf.call(o.v,s.v,k.v);
     } else {
           k = primlow(0); 
       k.v = String.prototype.indexOf.call(o.v,s.v); 
     }
     return join2(k, c.l.join(o.l));
  })),
   substr: ESSLField(FunctionProxy(function(x,y) {
     'use strict';
     var o = ToStringBox(this);
     var r = primlow("");
     var m = ToNumberBox(x);
     if (typeof y !== "undefined") {
       var n = ToNumberBox(y);
       r.v = String.prototype.substring.call(o.v, m.v, n.v);
     } else {
       var n = primlow(0);
       r.v = String.prototype.substring.call(o.v, m.v);
     }
     return join2(r, join2(n, join2(m, o.l).l).l);
  })),
   split: ESSLField(FunctionProxy(function(s,l) {
     'use strict';
     var o = ToStringBox(this);
     if (s) var m = ToStringBox(s);
     else       m = primlow("");
     if (l) var n = ToNumberBox(l);
     else       n = primlow("");
     if (s) {
       if (l) var res = String.prototype.split.call(o.v, m.v, n.v);
       else       res = String.prototype.split.call(o.v, m.v);
     } else       res = String.prototype.split.call(o.v);
     var reslev = n.l.join(m.l.join(o.l));
     return arraybox(res.map(function (s, i, ar) {return primbox(s, reslev);})
                            ,reslev);
  })),
   match: ESSLField(FunctionProxy(function(regexp) {
     //15.5.4.10
     'use strict';
     var o = ToStringBox(this);
     if (IsObjectBox(regexp) && regexp.m.class == "RegExp") {
       //regexp is an object box
       var rx = regexp;
     } else {
       //regexp is a string box
       var rx = autobox(new RegExp(regexp.v), regexp.l);
     }
     var r = arraylow([]);
     r.v = String.prototype.match.call(o.v, rx.m.underlying.regexp);
     return join2(r, o.l.join(rx.l)); 
  })),
  replace: ESSLField(FunctionProxy(function(search,replace) {
    'use strict';
    var o = ToStringBox(this);
    var r = ToStringBox(replace); 
    if (IsObjectBox(search) && regexp.m.class == "RegExp") {
      var rx/*:Box<string|Object>*/ = search;
      r.v = String.prototype.replace.call(o.v, rx.m.underlying.regexp, r);
    } else {
      var rx = ToStringBox(search);
      r.v = String.prototype.replace.call(o.v, rx.v, r);
    } 
    return join2(r, o.l.join(rx.l));
  })),
  trim: ESSLField(FunctionProxy(function () {
      var S = ToStringBox(this);
      S.v = S.v.trim();
      return S;
  }))
};

addFields(StringProto, StringProtoFields);
  
function BooleanCtor (value/*: Box<any>*/)/*: Box<Object>*/ {
  //15.6.2.1
  var bx = ToBooleanBox(value);
  this.m = {primitiveValue: bx.v
           ,class: "Boolean"};
  this.l = this.l.join(bx.l);
  return this;
}

var BooleanFields = {};

var BooleanProtoFields = {valueOf: ESSLField(FunctionProxy(function ()/*:Box<boolean>*/ {
    //15.6.4.2
    //1.
    var B = this;
    if (IsPrimBox(B) && typeof (B.v) === "boolean")
      //2.
      var b = B.v;
    else if (IsObjectBox(B) && B.m.class === "Boolean")
      //3
      b = B.m.primitiveValue;
    //4
    else throwTypeError();
    //5
    return primbox(b, B.l);
  })),
   toString: ESSLField(FunctionProxy(function ()/*:Box<string>*/ {
     //15.6.4.3
     var B = this;
     if (IsPrimBox(B) && typeof (B.v) === "boolean")
       //2.
       var b = B.v;
     else if (IsObjectBox(B) && B.m.class === "Boolean")
       //3
       b = B.m.primitiveValue;
     //4
     else throwTypeError();
     //5
     return primbox(b?"true":"false", B.l);
   }))};

addFields(BooleanProto, BooleanProtoFields);

function NumberCtor (x/*::?: Box<any>*/)/*: Box<Object>*/ {
  var bx = x?ToNumberBox(x):primlow(+0);
  this.m = {primitiveValue: bx.v
	   ,class: "Number"};
  this.l = this.l.join(bx.l);
  return this;
}

var NumberFields = {MAX_VALUE: ESSLImmutableField(ValueProxy(Number.MAX_VALUE))
		   ,MIN_VALUE: ESSLImmutableField(ValueProxy(Number.MIN_VALUE))
		   ,NaN: ESSLImmutableField(ValueProxy(Number.NaN))
		   ,NEGATIVE_INFINITY: ESSLImmutableField(ValueProxy(Number.NEGATIVE_INFINITY))
		   ,POSITIVE_INFINITY: ESSLImmutableField(ValueProxy(Number.POSITIVE_INFINITY))
		   };


var NumberProtoFields = {
  valueOf: ESSLField(FunctionProxy(function ()/*: Box<number>*/ {
    var n = 0;
    if (IsPrimBox(this)) {
      if (typeof this.v === "number") n = this.v;
      else throwTypeError();
    } else if (IsObjectBox(this) && this.m.class === "Number")
      n = this.m.primitiveValue;
    else throwTypeError();
    return this.primbox(n, this.l);
  })),
  toString: ESSLField(FunctionProxy(function (radix/*: ?Box<number>*/)/*: Box<string>*/ {
    var n = 0;
    var rad = radix ? ToNumberBox(radix) : primlow(10);
    if (IsPrimBox(this)) {
      if (typeof this.v === "number") n = this.v;
      else {throwTypeError(); 
	    /*::return primlow("");*/
	   }
    } else if (this.m.class === "Number")
      n = this.m.primitiveValue;
    else {throwTypeError(); 
	  /*::return primlow("");*/
	 }
    return primbox(n.toString(rad.v), this.l.join(rad.l));
  }))};

addFields(NumberProto, NumberProtoFields);

NumberProto.m.primitiveValue = +0;

function ErrorCtor (message/*::?: Box<any>*/)/*: Box<Object>*/ {
  //15.11.2.1
  this.m = {class: this.v.name.v};
  if (typeof(message) !== 'undefined') {
    this.v.message = ToStringBox(message);
    this.l = this.l.join(message.l);
  }
  return this;
}

var ErrorFields = {};

var ErrorProtoFields = {name: ESSLField(ValueProxy("Error"))
  ,message: ESSLField(ValueProxy(""))
  ,toString: ESSLField(FunctionProxy(function ()/*:Box<string>*/ {
    //15.11.4.4
    var retlev = lowlevel();
    var retval = "";
    //1.
    var O/*:Box<any>*/ = this;
    //2.
    if (!IsObjectBox(O)) throwTypeError();
    //3.
    var name = readField(O, primlow("name"));
    //4.
    if (typeof (name.v) === 'undefined') name = primbox("Error", name.l);
    else name = ToStringBox(name);
    //5.
    var msg = readField(O, primlow("message"));
    //6
    if (typeof (msg.v) === 'undefined') msg = primbox("", msg.l);
    else msg = ToStringBox(msg);
    retlev = msg.l.join(name.l);
    //7
    if (name.v === "" && msg.v === "") retval = "Error";
    else {
    //8
      if (name.v === "") retval = msg.v;
      else {
      //9
	if (msg.v === "") retval = name.v;
	//10
	else retval = name.v + ": " + msg.v;
      }
    }
    return primbox(retval, retlev);
  }))
  };


addFields(ErrorProto, ErrorProtoFields);

var TypeErrorCtor /*: (message?: Box<any>) => Box<Object>*/ = ErrorCtor;

var TypeErrorFields = {};

var TypeErrorProto = ObjectProxy(ErrorProto, {class: "TypeErrorPrototype"}, false);

var TypeErrorProtoFields =   {name: ESSLField(ValueProxy("TypeError"))
  ,message: ESSLField(ValueProxy(""))
  };

addFields(TypeErrorProto, TypeErrorProtoFields);

var RangeErrorCtor /*: (message?: Box<any>) => Box<Object>*/ = ErrorCtor;

var RangeErrorFields = {};

var RangeErrorProto = ObjectProxy(ErrorProto, {class: "RangeErrorPrototype"}, false);

var RangeErrorProtoFields = {name: ESSLField(ValueProxy("RangeError"))
  ,message: ESSLField(ValueProxy(""))
  };

addFields(TypeErrorProto, TypeErrorProtoFields);

function EnsureObjectBox(x/*:Box<any>*/)/*:void*/ {
  pushExceptionLabel(x.l);
  if (!IsObjectBox(x)) throwTypeError();
}

var RegExpProto = ObjectProxy(ObjectProto, {class: "RegExp"}, false);
function RegExpCtor (pattern, flags)/*: Box<Object>*/ {
  'use strict';
  var r = new RegExp(pattern, flags);
  this.m.underlying = r;
  this.m.class = "RegExp";
  return this;
}
var RegExpFields = {};
var RegExpProtoFields = {};
addFields(RegExpProto, RegExpProtoFields);



var global/*: Box<Object>*/ = ObjectProxy(null, {}, false);

var ObjectConstructor = ConstructorProxy(ObjectCtor, ObjectProto);
addFields(ObjectConstructor, ObjectFields);

var FunctionConstructor = ConstructorProxy(FunctionCtor, FunctionProto);
addFields(FunctionConstructor, FunctionFields);

var ArrayConstructor = ConstructorProxy(ArrayCtor, ArrayProto);
addFields(ArrayConstructor, ArrayFields);

var StringConstructor = ConstructorProxy(StringCtor, StringProto);
addFields(StringConstructor, StringFields);

var RegExpConstructor = ConstructorProxy(RegExpCtor, RegExpProto);
addFields(RegExpConstructor, RegExpFields);

var BooleanConstructor = ConstructorProxy(BooleanCtor, BooleanProto);
addFields(BooleanConstructor, BooleanFields);

var NumberConstructor = ConstructorProxy(NumberCtor, NumberProto);
addFields(NumberConstructor, NumberFields);

var ErrorConstructor = ConstructorProxy(ErrorCtor, ErrorProto);
addFields(ErrorConstructor, ErrorFields);

var TypeErrorConstructor = ConstructorProxy(TypeErrorCtor, TypeErrorProto);
addFields(TypeErrorConstructor, TypeErrorFields);

var RangeErrorConstructor = ConstructorProxy(RangeErrorCtor, RangeErrorProto);
addFields(RangeErrorConstructor, RangeErrorFields);

var MathObject = ObjectProxy(ObjectProto, {class: "Math"}, false);
addFields(MathObject, {
  max: ESSLField(FunctionProxy(function () {
     var rl = lowlevel();
     var args = [];
     for (var i = 0; i < arguments.length; i++) {
       rl = rl.join(arguments[i].l);
       args.push(arguments[i].v);
     }
     return primbox(Math.max.apply(Math, args), rl);
  })),
  abs: ESSLField(FunctionProxy(function (x/*:Box<any>*/)/*:Box<number>*/ {
    var n = ToNumberBox(x);
    n.v = Math.abs(n.v);
    return n;
  })),
  round: ESSLField(FunctionProxy(function (x/*:Box<any>*/)/*:Box<number>*/ {
    var n = ToNumberBox(x);
    n.v = Math.round(n.v);
    return n;
  })),
  sqrt: ESSLField(FunctionProxy(function (x/*:Box<any>*/)/*:Box<number>*/{
    var n = ToNumberBox(x);
    n.v = Math.sqrt(n.v);
    return n;
  })),
  PI: ESSLImmutableField(ValueProxy(Math.PI)),
  sin: ESSLField(FunctionProxy(function (x/*:Box<any>*/)/*:Box<number>*/ {
    var n = ToNumberBox(x);
    n.v = Math.sin(n.v);
    return n;

  })),
  cos: ESSLField(FunctionProxy(function (x/*:Box<any>*/)/*:Box<number>*/ {
    var n = ToNumberBox(x);
    n.v = Math.cos(n.v);
    return n;
  })),
  random: ESSLField(FunctionProxy(function ()/*:Box<number>*/{
    return primlow(Math.random());
  })),
  floor: ESSLField(FunctionProxy(function (x/*:Box<any>*/)/*:Box<number>*/{
    var n = ToNumberBox(x);
    n.v = Math.floor(n.v);
    return n;
  })),
  ceil: ESSLField(FunctionProxy(function (x/*:Box<any>*/)/*:Box<number>*/{
    var n = ToNumberBox(x);
    n.v = Math.ceil(n.v);
    return n;
  })),
  pow: ESSLField(FunctionProxy(function (x/*:Box<any>*/, y/*:Box<any>*/)/*:Box<number>*/{
    var n = ToNumberBox(x);
    var p = ToNumberBox(y);
    return primbox(Math.pow(n.v, p.v), n.l.join(p.l));
  }))
});

var encodeURIProxy = FunctionProxy(function (s/*:Box<any>*/)/*:Box<string>*/ {
  var str = ToStringBox(s);
  return primbox(encodeURI(str.v), str.l);
});

var JSONObject = ObjectProxy(ObjectProto, {class: "JSON"}, false);

addFields(JSONObject, {
  parse: ESSLField(FunctionProxy(function (text/*:Box<any>*/)/*:Box<any>*/ {
    //15.12.2
    var JText = ToStringBox(text);
    pushExceptionLabel(text.l);
    return autobox(JSON.parse(JText.v), text.l);
  }))
});

/* JEST extensions: declassify and upgrade */

//upgrade the label on expression e to, at least, that of the input
//label of channel c and the label of the channel name
function upgrade(e/*:Box<any>*/, c/*:Box<string>*/)/*:Box<any>*/ {
   //get the input label of c
   var ciLabel = policy.locationLevel(ToStringBox(c).v, false);
   var targetLabel = ciLabel.join(c.l).join(e.l).join(pclabel);
   e.l = targetLabel;
   return e;
}

//downgrade the label on expression e to that of the output label of
//channel c
function declassify(e/*:Box<any>*/, c/*:Box<string>*/)/*:Box<any>*/ {
   //get the output label of c
   var channel/*:string*/ = ToStringBox(c).v;
   var coLabel = policy.locationLevel(channel, true);
   //make sure that the PC label and label of c are both below the
   //target label (coLabel)
   if (! (pclabel.leq(coLabel) && c.l.leq(coLabel)))
       stop("", 0, 0, "Robust declassification violation: attempted to declassify a value with label " + e.l.toString() + " to the output label of channel " + channel + " with label " + coLabel.toString() + " when either the PC label or the label of the channel name are not subsumed by the channel label itself");
   
   else e.l = coLabel;
   return e; 
}

var evalProxy = FunctionProxy(function(s) {
   var evalGlobal = eval;
   return evalGlobal(inlineEval(s));
});

evalProxy.m.nativeEvalProxy = true;

var globalESSLFields = {
  Object: ESSLField(ObjectConstructor),
  Function: ESSLField(FunctionConstructor),
  Array: ESSLField(ArrayConstructor),
  String: ESSLField(StringConstructor),
  Boolean: ESSLField(BooleanConstructor),
  Number: ESSLField(NumberConstructor),
  RegExp: ESSLField(RegExpConstructor),
  Error: ESSLField(ErrorConstructor),
  TypeError: ESSLField(TypeErrorConstructor),
  RangeError: ESSLField(RangeErrorConstructor),
  Math: ESSLField(MathObject),
  JSON: ESSLField(JSONObject),
  encodeURI: ESSLField(encodeURIProxy),
  declassify: ESSLField(FunctionProxy(declassify)),
  upgrade: ESSLField(FunctionProxy(upgrade)),
  eval: ESSLField(evalProxy)
};

addFields(global, globalESSLFields);
