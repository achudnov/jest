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


/* externs */
/*::declare class Level {
  static bottom (): Label;
};*/
/*::declare class Label{
 leq (l: Label): boolean;
 join (l: Label): Label;
} */

/*::var globalProxy = global;*/

function stop(file/*: string*/, line/*: number*/, col/*: number*/, reason/*: string*/){};

/* Note [Augmented values]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The transformed program operates on augmented values, with the help of
the monitor. Augmented values are ECMAScript objects that are required
to have a field "t" (for type) of type Int31 ("small int"). However, only the 4
least significant bits are used. The rest are set to 0. The bit-flag
semantics ---the leftmost bit being the least significant--- is as
follows:

  <object?><function?><array?><ref?>00000...0000 (27 zeroes padding)

the <ref?> flag is mutually exclusive with the other flags.

Augmented values are further structurally subtyped into Boxes and Refs.

Variables always store boxes.

Fields can have setters and getters that we have to emulate (that is so that we can provide the proper value for 'this'). Thus, fields store augmented values.

This design is informed by performance tests:
http://jsperf.com/instanceof-versus-flags-in-prototype
http://jsperf.com/getter-setter-emulation-performance-study/2
http://jsperf.com/boxes-and-getters-setters
*/

/* Note [Boxes]
~~~~~~~~~~~~~~~
Boxes are run-time representations of labelled ECMAScript values. In
addition to the "t" field they have the following:

 - "v" (value) stores the boxed value
 - "l" (label) stores the security label of the boxed value
 - "m" (meta) stores the metadata necessary for the implementation of methods
      associated with the boxed value, or for monitor operations. Meta
      is always an object or null.
*/

/*::type Meta = {[key: string]: any;
                 primitiveValue?: any;
                 length?: number;
                 parent?: Box<?Object>;
                 children?: Box<Array<Box<Object>>>;
                 class?: string;
                 func?: Function;
                 implicitThis?: Box<Object>;
                 funId?: number;
                 nativeEvalProxy?: boolean;
                 length?: number;
                 byId?: {[key: string]: Box<Object>};
                 handlers?: {[key: string]: Box<Object>};
                };*/

/*::declare class Box<T> {
   t: number;
   v: T;
   l: Label;
   m: Meta;
}*/

// function CBox/*::<T>*/(v/*: T*/, l/*: Label*/, m/*: Meta*/, o/*: boolean*/, f/*: boolean*/, a/*: boolean*/)/*: void*/ {
//   this.t = (o?1:0) | (f?2:0) | (a?4:0);
//   this.v = v;
//   this.l = l;
//   this.m = m;
// }

/* Note [Primitive boxes]
~~~~~~~~~~~~~~~~~~~~~~~~~
Primitive boxes store primitive ECMAScript values in their value
field. Their type field always holds the value 0. Their meta field is
null.
*/

/* Note [Fast function boxes]
~~~~~~~~~~~~~~~~~~~~~~~~~
Functions, being ECMAScript values, can be stored in both Function
Object boxes (see the corresponding note in RTS.APISpec.ESSL) and in
fast function boxes. The difference is that FFBs can only be used in
calls and other uses (including as an object) require a type
conversion. This is an optimization. Their type field holds the value
of 2 (that is LSB01000...0MSB). The value field stores the ECMAScript
function. The meta field is null.
*/



function newbox (v/*: any*/, l/*: Label*/, m/*: Meta*/, o/*: boolean*/, f/*: boolean*/, a/*: boolean*/)/*: Box<any>*/ {
  'use strict';
   // We use object literals instead of new-calls to create boxes
   // because field accesses for the former tend to be more
   // consistently optimized. According to
   // https://jsperf.com/new-vs-object-literal-3 in Chrome field
   // access for object literals is ~10x faster, but almost no
   // difference in Firefox.
  return  {t: (o?1:0) | (f?2:0) | (a?4:0)
	  ,v: v
	  ,l: l
	  ,m: m};
}

function copybox/*::<T>*/ (src/*: Box<T>*/, dst/*: Box<T>*/)/*: Box<T>*/ {
  'use strict';
  dst.t = src.t;
  dst.v = src.v;
  dst.l = src.l;
  dst.m = censorMeta(src.m);
  return dst;
}

function censorMeta (m/*:Meta*/)/*:Meta*/ {
  var censoredMeta = m;
  var p;
  // Meta properties that can't be copied
  var metaUniqueProps = ["implicitThis"];

  if (typeof(censoredMeta) === 'object') {
      for (var i = 0; i < metaUniqueProps.length; i++) {
	  p = metaUniqueProps[i];
	  if (p in m) delete m[p];
      }
  }
  return censoredMeta;
}

function clonebox/*::<T>*/ (box/*: Box<T>*/)/*: Box<T>*/ {
  'use strict';
  return {t: box.t
	 ,v: box.v
	 ,l: box.l
	 ,m: censorMeta(box.m)
         };
}

/* Note [Refs]
~~~~~~~~~~~~~~
These are "references" that one can update and read from (can have
side effects). Used in implementing getters and setters, including
computed fields in the APIs. Right now the getters/setters are
unboxed; but for full ES5 support they need to be as getters and
setters can be set by the user. Refs have the following fields:

- "g" stores the getter function: () -> Box

- "s" stores the setter function: Box -> Box -> ()

The type field of a box is set to 8 (0...01000)
*/

/*::declare class Ref {
 t: number; 
 g (): Box<any>;
 s?: (v: Box<any>) => void;
};*/

// function CRef (g/*: (o: Box<Object>) => Box<any>*/, s/*::?: (o: Box<Object>, v: Box<any>) => void*/)/*: void*/ {
//     this.t = 8;
//     this.g = g;
//     s?this.s = s:null;
// }

function newref (g/*: () => Box<any>*/, s/*: (v: Box<any>) => void*/)/*: Ref*/ {
  'use strict';
  return {t: 8
	 ,g: g
         ,s: s
	 };
}

function nsuxCheck (object/*: Box<Object>*/, fieldName/*: Box<string>*/)/*: void*/ {
  'use strict';
  var fs = ToStringBox(fieldName);
  var ob = ToObjectBox(object);
  var ll = lowlevel();
  if (! (fs.v in ob.v) && (!ob.l.leq(ll) || !pclabel.leq(ll) || ! fs.l.leq(ll)))
      stop("", 0, 0, "NSUX failure");
}

function nsuCheckLev (lhslev/*: Label*/)/*: void*/{
  'use strict';
  if (! pclabel.leq(lhslev))
      stop("", 0, 0, "NSU failure");
}

function nsuCheck (lhs/*: Box<any>*/)/*: void*/ {
  'use strict';
  nsuCheckLev(lhs.l);
}

function assignField/*::<T>*/ (lhsobj/*: Box<any>*/, lhsfield/*: Box<any>*/, rhs/*: Box<T>*/)/*: Box<T>*/ {
  'use strict';
  var lhsobj2 = ToObjectBox(lhsobj);
  var fieldname = ToStringBox(lhsfield);
  var lhs/*: Box<any> | Ref | void*/ = lhsobj2.v[fieldname.v];
  if (typeof lhs === "undefined") {
    nsuxCheck(lhsobj2,fieldname);
    var lhs2/*:Box<any> | Ref*/ = lhsobj2.v[fieldname.v] = initVar();
  } else lhs2 = lhs;
  var rhs2 = join2(rhs, pclabel);
  if (/*::lhs2 instanceof Ref && */ IsRef(lhs2)) {
    //TODO: need to push the label on the setter and the object onto PCLS
    lhs2.s.call(lhsobj2, rhs2);
  } else {
    /*:: if (lhs2 instanceof Box) {*/
    nsuCheck(lhs2);
    copybox(rhs2, lhs2);
    /*::}*/
  }
  MaintainArrayLength(lhsobj2, fieldname.v);
  return rhs;
}

var intregexp/*:RegExp*/ = /^\d+$/;
var maxint = Math.pow(2, 32);

//15.4.5.1, step 4:
function MaintainArrayLength (obj/*: Box<Object>*/, field/*: string*/)/*: void*/ {
  'use strict';
  if (IsArrayBox(obj) && intregexp.test(field)) {
    var ix/*:number*/ = ToUInt32(field);
    if (ix < maxint) {
      var len/*:number*/ = /*:: typeof (obj.m.length) !== 'undefined'?*/obj.m.length/*:::0*/;
      if (ix > len+1) obj.m.length = ix+1;
    }
  }
}

function assignFieldOp (op/*: (l: Box, r: Box) => Box*/, lhsobj/*: Box*/, lhsfield/*: Box*/, rhs/*: Box*/)/*: Box*/ {
  'use strict';
  var lhsobj2 = ToObjectBox(lhsobj);
  var fieldname = ToStringBox(lhsfield);
  var lhs/*: Box | Ref | void*/ = lhsobj2.v[fieldname];
   var rhs2/*:Box<any>*/, rhs3/*Box<any>*/;
  if (typeof lhs === "undefined") {
    nsuxCheck(lhsobj2,fieldname);
    lhs = lhsobj2.v[fieldname.v] = initVar();
  }
  if (/*:: lhs instanceof Ref && lhs.s && lhs.g && */ IsRef(lhs)) {
    rhs2 = op.call(this, lhs.g.call(lhsobj2), rhs);
    rhs3 = join2(rhs2, pclabel);
    lhs.s.call(lhsobj2,rhs3);
  } else {
    /*:: if (lhs instanceof Box) {*/
    nsuCheck(lhs);
    rhs2 = join2(op.call(this,lhs,rhs), pclabel);
    rhs3 = join2(rhs2, pclabel);
    copybox(rhs3,lhs);
    /*::} else rhs2 = primlow(void 0);*/
  }
  MaintainArrayLength(lhsobj2, fieldname.v);
  return rhs2;
}

function assignVar (lhs/*: Box*/, rhs/*: Box*/)/*: Box*/ {
  'use strict';
  nsuCheck(lhs);
  copybox(join2(rhs, pclabel), lhs);
  return rhs;
}

function assignVarOp (op/*: (l: Box, r: Box) => Box*/, lhs/*: Box*/, rhs/*: Box*/)/*: Box*/ {
  'use strict';
  nsuCheck(lhs);
  var rhs2 = op.call(this,lhs,rhs);
  copybox(join2(rhs2, pclabel), lhs);
  return rhs2;
}
 
function readField (obj/*: Box<any>*/, fname/*: Box<any>*/)/*: Box<any>*/ {
  'use strict';
  var obj1 = ToObjectBox(obj);
  var fld/*: Box<any> | Ref | void*/ = obj1.v[ToStringBox(fname).v];
  if (typeof fld === "undefined")
     var fv/*:Box<any>*/ = initVar();
  else {
      if (/*:: fld instanceof Ref && */ IsRef(fld))
	  fv = fld.g.call(obj1);
      else /*::if (fld instanceof Box)*/
	fv = fld;
      /*:: else fv = primlow(void 0);*/
  }
  return join2(fv, obj1.l.join(fname.l));
}

function deleteField (obj/*:Box<any>*/, fname/*:Box<any>*/)/*:Box<void>*/ {
   'use strict';
   nsuxCheck(obj, fname);
   delete obj.v[ToStringBox(fname).v];
   return primlow(void 0);
}

function isNullUndefined (x/*: mixed*/)/*: boolean*/ {
  'use strict';
  var tv = typeof x;
  return tv === "undefined" || tv === "null";
}

function invokeMethod (obj/*: Box<any>*/, field/*: Box<any>*/, args/*: Array<any>*/)/*: Box<any>*/ {
  'use strict';
  var fun = readField(obj,field);
  return invokeCommon(fun,obj,args);
}

function invokeFunction (func/*: Box<any>*/, args/*: Array<any>*/)/*: Box<any>*/ {
  'use strict';
  if (func.m.implicitThis)
      var thisVal = func.m.implicitThis;
  else thisVal = globalProxy;
  return invokeCommon(func, thisVal, args);
}

function invokeCommon (func/*: Box<any>*/, this_/*: Box<any>*/, args/*: Array<any>*/)/*: Box<any>*/ {
  'use strict';
  if (! IsFunBox(func)) {
    pushCtx(func.l, 0, contextFun);
    throwTypeError();
    /*:: return primlow(void 0);*/ //satisfy the typechecker
  } else {
      pushFunc(func);
      if (IsObjectBox(func)) var f/*:Function*/ = func.m.func;
      else f = func.v;
      var retval = f.apply(this_,args);
      popFunc(func);
      return retval;
  }
}

function newObject (ctor/*: Box<any>*/, args/*: Array<any>*/)/*: Box<Object>*/ {
  'use strict';
  var proto = readField(ctor, primlow("prototype"));
  if (! IsObjectBox(proto)) proto = globalProxy.v.Object.v.prototype;
  var newobj = newbox(Object.create(proto.v),  lowlevel(), {class: "Object"}, true, false, false);
  var result = invokeCommon(ctor, newobj, args);
  if (typeof result !== "object")
    result = newobj;
  return join2(result,proto.l);
}

function lowlevel ()/*: Label*/ {
  'use strict';
  return Level.bottom();
}

function join2/*::<T>*/ (e/*: Box<T>*/, l/*: Label*/)/*:Box<T>*/ {
  'use strict';
  var rv = clonebox(e);
  rv.l = rv.l.join(l);
  return rv;
}

 
function initVar ()/*: Box<void>*/  {
  'use strict';
  return primlow(void 0);
}

function primbox/*::<T>*/ (v/*: T*/, l/*: Label*/)/*: Box<T>*/ {
  'use strict';
  return newbox(v,l,{},false,false,false);
}

function primlow/*::<T>*/ (x/*: T*/)/*: Box<T>*/ {
  'use strict';
  return primbox(x,lowlevel());
}

function funbox (f/*: Function*/, l/*: Label*/)/*:Box<Object>*/ {
  'use strict';
  var fo = newObject(globalProxy.v.Function, []);
  fo.m.func = f;
  fo.l = l;
  return fo;
}
 
function funlow (f/*: Function*/)/*: Box<Object>*/ {
  'use strict';
  return funbox(f,lowlevel());
}

function objectbox (obj/*: Object*/, l/*: Label*/)/*: Box<Object>*/ {
  'use strict';
  var ob = newObject(globalProxy.v.Object, []);
  for (var f in obj)
    ob.v[f] = obj[f];
  return join2(ob, l);
}
 
function objectlow (obj/*: Object*/)/*: Box<Object>*/ {
  'use strict';
  return objectbox(obj,lowlevel());
}

function arraybox (ar/*:Array<Box<any>>*/, l/*: Label*/) /*: Box<Object>*/ {
    'use strict';
    var abox;
    if (ar.length === 1) {
       abox = newObject(globalProxy.v.Array, [primlow(1)]);
       abox.v[0] = ar[0];
    }
    else abox = newObject(globalProxy.v.Array, ar);

    return join2(abox, l);
}


function arraylow (ar/*:Array<any>*/)/*:Box<Object>*/{
    'use strict';
    return arraybox(ar, lowlevel());
}


function autobox (v/*: any*/, l/*: Label*/)/*: Box<any>*/ {
  'use strict';
  if (typeof v === 'undefined' || typeof v === 'boolean' || typeof v === 'number' || typeof v === 'string')
       return primbox(v, l);
  else if (typeof v === 'function') return funbox(v, l);
  else if (typeof v === 'object') { 
      if (v === null) return primbox(v, l);
      if (Array.isArray(v)) {
        var result = arraylow([]);
        for (var i=0; i<v.length; i++)
          result.v[i] = autobox(v[i], l);
          result.m.length = v.length;
          return result;
	} else {
          var tempobj = {};
          for (var prop in v)
            tempobj[prop] = autobox(v[prop], l);
          return objectbox(tempobj, l);
        }
  }
  //satisfy the typechecker
  return initVar();
}

function autolow (v/*: any*/)/*: Box<any>*/ {
  'use strict';
  return autobox (v, lowlevel());
}

function ToPrimitiveBox (b/*: Box<any>*/, hint/*::?: boolean*/)/*: Box<any>*/ {
  'use strict';
  if (IsPrimBox(b))
    return b;
  else return DefaultValue(b,hint);
}

function ToBooleanBox (b/*: Box<any>*/)/*: Box<any>*/ {
  'use strict';
  if (IsPrimBox(b)) {
    var rv = clonebox(b);
    rv.v = Boolean(rv.v);
    return rv;
  } else return primlow(true);
}
 
function ToNumberBox (b/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
  if (IsPrimBox(b)) {
    var rv = clonebox(b);
    rv.v = Number(rv.v);
    return rv;
  } else {
    if (IsObjectBox(b))
      return ToNumberBox(DefaultValue(b, true));
    else return ToNumberBox(ToObjectBox(b));
  }
}

function DefaultValue (b/*: Box<any>*/, hint/*: boolean|void*/)/*: Box<any>*/ {
  'use strict';
  var isNumber = typeof hint === "undefined" ? true : hint;
  var val,first,second;
  if (hint) {
    first = readField(b, primlow("valueOf"));
    second = readField(b, primlow("toString"));
  } else {
    first = readField(b,
    primlow("toString"));
    second = readField(b,
    primlow("valueOf"));
  }
  if (IsFunBox(first)) {
    val = invokeCommon(first,b,[]);
    if (IsPrimBox(val))
      return val;
  }
  if (IsFunBox(second)) {
    val = invokeCommon(second,b,[]);
    if (IsPrimBox(val))
      return val;
  }
  throwTypeError();
  //unreachable, but satisfies the typechecker
  return b;
}

function ToIntegerBox (b/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
  return ToNumberBox(b);
}

function ToInt32Box (b/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
  var n/*: Box<number>*/ = ToNumberBox(b);
  n.v = ~~n.v;
  return n;
}

function ToUInt32Box (b/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
  var n/*: Box<number>*/ = ToNumberBox(b);
  n.v = n.v >>> 0;
  return n;
}

function ToUInt32 (x/*: number|string*/)/*: number*/ {
  'use strict';
  return (x/*: any*/) >>> 0;
}

function ToUInt16Box (b/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
  return ToNumberBox(b);
};

function ToStringBox (b/*: Box<any>*/)/*: Box<string>*/ {
  'use strict';
  if (IsPrimBox(b)) {
    var rv = clonebox(b);
    rv.v = String(rv.v);
    return rv;
  } else {
    if (IsObjectBox(b))
      return ToStringBox(DefaultValue(b, false));
    else return ToStringBox(ToObjectBox(b));
  }
}

function throwTypeError ()/*: void*/ {
  'use strict';
  throw newObject(globalProxy.v.TypeError, []);
}

function ToObjectBox (b/*: Box<any>*/)/*: Box<Object>*/ {
  'use strict';
  if (IsObjectBox(b))
    return b;
  else if (IsFunBox(b))
    return ToFunObject(b);
  else {
    var ty = typeof b.v;
    if (ty === "boolean")
       return newObject(globalProxy.v.Boolean, [b]);
    else if (ty === "number")
	return newObject(globalProxy.v.Number, [b]);
    else if (ty === "string")
        return newObject(globalProxy.v.String, [b]);
  }
  throwTypeError();
  //to satisfy the typechecker
  return b;
}

function ToFunObject (b/*: Box<Function>*/)/*: Box<Object>*/ {
  'use strict';
  var fo = newObject(globalProxy.v.Function, []);
  fo.m.func = b.v;
  fo.l = b.l;
  return fo;
}
 
function IsPrimBox (b/*: Box<any>|Ref*/)/*: boolean*/ {
  'use strict';
  return b.t === 0;
}

function IsObjectBox (b/*: Box<any>| Ref*/)/*: boolean*/ {
  'use strict';
  return b.t & 1?true:false;
}

function IsFunBox (b/*: Box<any> | Ref*/)/*: boolean*/ {
  'use strict';
  return b.t & 2?true:false;
}

function IsArrayBox (b/*: Box<any> | Ref*/)/*: boolean*/ {
  'use strict';
  return b.t & 4?true:false;
}


function IsRef (b/*: Box<any> | Ref*/)/*: boolean*/ {
  'use strict';
  return b.t & 8?true:false;
}

var temp/*: any*/;

function AbstractEqCmp (x/*: Box<any>*/, y/*: Box<any>*/)/*: boolean*/ {
  'use strict';

  if (SameType(x,y))
    return AbstractStrictEqCmp(x,y);
  else {
    if (IsPrimBox(x) && IsPrimBox(y))
      return x.v == y.v;
    else {
      var a,b;
      if (IsFunBox(x) || IsObjectBox(x))
        a = ToPrimitiveBox(ToObjectBox(x));
      else a = x;
      if (IsFunBox(y) || IsObjectBox(y))
        b = ToPrimitiveBox(ToObjectBox(y));
      else b = y;
      return a.v == b.v;
    }
  }
}

function SameType (x/*: Box<any>*/, y/*: Box<any>*/)/*: boolean*/ {
  'use strict';

  return IsFunBox(x) && IsFunBox(y) || x.t === y.t;
}

function AbstractStrictEqCmp (x/*: Box<any>*/, y/*: Box<any>*/)/*: boolean*/ {
  'use strict';

  if (! SameType(x,y))
    return false;
  if (! IsFunBox(x))
    return x.v === y.v;
  else {
    var a,b;
    if (IsObjectBox(x))
      a = x.m.func;
    else a = x.v;
    if (IsObjectBox(y))
      b = y.m.func;
    else b = y.v;
    return a === b;
  }
}

/*:: type Context = {

  id: number; //smallint (31bit); multiplexing node id's,
              //remember/restore id's, exception and function context id's, 
              //using the 30th and 29th (0-based) bits to indicate which one 
              //of the four it is:
              //00 -- IPD (node) ID
              //01 -- Exception ID
              //10 -- Remember/restore ID
              //11 -- Function context ID
              //^  -- 30th bit
              // ^ -- 29th bit
  label: Label;
};*/

// Make a new context ID.
// @param id -- the unique numeric identifier of the context, should
// be a positive integer < 0x0FFFFFFF
// @param ctxtype -- 0 is Node, 1 is Exception, 2 is Remember/restore, 3 is Function
function mkContextId(id/*:number*/, ctxtype/*:number*/)/*:number*/ {
   return id | (ctxtype << 28);
}

var contextNode = 0;
var contextExn = 1;
var contextRR = 2;
var contextFun = 3;

/* Invariants:
 topctx = pcls.length > 0 ? pcls[pcls.length - 1] : null; // i.e. topctx always points to the last element of the pcls array, if such element exists
 pclabel = topctx ? topctx.label : bottom; // if there are any contexts on pcls then pclabel is always the label of the top context, bottom label otherwise
 \forall i j: 0 < i < j < pcls.length => pcls[i].label.leq(pcls[j].label) // the context labels monotonically increase
 */

var pcls /*:Array<Context>*/ = [];
var topctx /*: ?Context*/ = null;
var pclabel = lowlevel();

// Push puts a label and the identifier of the branching node's IPD on
// the stack. If there is an element with the same identifier on top
// of the stack, we don't put a new element, but, instead, update the
// label to the join of the old label and the label being pushed. Id
// should be < 2^30.
function push(e/*: Box<any>*/, id/*: number*/) /*:Box<any>*/ {
    'use strict';
    pushCtx(e.l, id, contextNode);
    return e;
} 

//special push when we call functions
function pushFunc(f/*:Box<any>*/)/*:Box<any>*/ {
    'use strict';
    pushCtx(f.l, f.m.funId || 0, contextFun);
    return f;
}

//Id should be < 2^30.
function pushCtx(l/*:Label*/, id/*:number*/, ctxtype/*:number*/) /*:void*/ {
    'use strict';
    var cid = mkContextId(id, ctxtype);
    if (topctx && topctx.id === cid) {
	// same region or nested with the same IPD, so the label can't
	// decrease until the IPD is reached. Reuse the top element of
	// the stack.
	topctx.label = topctx.label.join(l);
    } else {
	// different region, so we need to push an element on the stack
	topctx = {label: pclabel.join(l)
               	 ,id: cid};
	pcls.push(topctx);
    }
    //update the pclabel with the label of the top context
    pclabel = topctx.label;
}

function pushException(b/*:Box<any>*/)/*:Box<any>*/ {
   'use strict';
   pushExceptionLabel(b.l);
   return b;
}

//For use in API facades only
function pushExceptionLabel(l/*:Label*/) {
    'use strict';
    pushCtx(l, 0, contextExn);
}

//removes one element from pcls; adjusts topctx and pclabel
function pop (e/*:?Box<any>*/, id/*:number*/) {
    'use strict';
    popCtxId(id, contextNode);
    return e;
}

function popFunc(func/*:Box<Object>*/) {
    'use strict';
    popCtxId(func.m.funId || 0, contextFun);
}


function popCtxId(id/*:number*/, ctxtype/*:number*/)/*:?Context*/ {
  'use strict';
  
  if (topctx && topctx.id === mkContextId(id, ctxtype)) popCtx();
  return topctx;
};

function popCtx()/*:?Context*/ {
    'use strict';
    var len = pcls.length;
    var ctx;
    if (len > 0 ) {
	ctx = pcls.pop();
	topctx = ctx==undefined?null:pcls[len-2];
	if (topctx) {
	    pclabel = topctx.label;
	} else {
	    pclabel = lowlevel();
	}
    }

    return topctx;
}

//Id should be < 2^30.
function remember (id/*: number*/)/*: void*/ {
  'use strict';  
  pushCtx(pclabel, id, contextRR);
}

function restore (id/*: number*/)/*: void*/ {
  'use strict';
  var ctx;
  var targetId = mkContextId(id, contextRR);
  while ((ctx = popCtx ()) && ctx.id !== targetId);
};


/* Object adaptation to be provided as a parameter of 'with'. Requires
* ECMAScript 6 proxies */

function adaptObject(objbox/*:Box<Object>*/)/*:Object*/ {
   var handler = {get: function (target/*:Object*/, prop/*:string*/, reciever)/*:Box<any>*/ {
                         // spec 6: 9.5.8
                         // Use readField to read the property
                         var v = readField(objbox, primlow(prop));
                         if (IsFunBox(v)) v.m.implicitThis = objbox;
                         return v;
                       }
		 ,set: function (target/*:Object*/, prop/*:string*/, value/*:Box<any>*/, receiver)/*:bool*/ {
                         // spec 6: 9.5.9
                         // Use assignField to write the property
                         assignField(objbox, primlow(prop), value);
                         return true;
                       }
                 ,deleteProperty: function (target/*:Object*/, prop/*:string*/)/*:bool*/ {
                                     // spec 6: 9.5.9
		                     // perform an NSUX check
                                     if (objbox.l.leq(lowlevel()) && 
                                         pclabel.leq(lowlevel())) {
                                         delete objbox.v[prop];
					 return true;
                                     } else {
                                         stop("", 0, 0, "NSUX failure");
                                         return false;
                                     }
                                  }
                 };
   return new Proxy(objbox.v, handler);
}


/* Eval handling */

/**
 * Creates an XMLHttpRequest objects independent of the
 * browser. Adapted from http://www.quirksmode.org/js/xmlhttp.html 
 *
 * @return an instance of XMLHttpRequest
*/
function createXMLHTTPObject() {
  if (window && "XMLHttpRequest" in window) {
      return new window.XMLHttpRequest();
  } else if ("ActiveXObject" in window) {
      try {return new window.ActiveXObject("Msxml2.XMLHTTP.6.0");}
      catch (e) {};
      try {return new window.ActiveXObject("Msxml2.XMLHTTP.3.0");}
      catch (e) {};
      try {return new window.ActiveXObject("Microsoft.XMLHTTP");}
      catch (e) {};
  }
  return null;
}

var inlineEvalXHR = createXMLHTTPObject();

var monitorPrefix = "";

// Perform inlining on a string. Right now uses the proxy server.
function inlineEval (body/*:string*/)/*:string*/ {
   if (inlineEvalXHR !== null) {
       // the request is synchronous because we need to evaluate the
       // code before we can resume program execution
       inlineEvalXHR.open("GET", document.URL, false);

       var noVarDecls = pclabel.leq(lowlevel());
       // set a custom header to signal to the proxy that this is an eval
       // inlining request
       inlineEvalXHR.setRequestHeader("X-Inline", "monitorvar=" + monitorPrefix + ";novardecls="+noVarDecls.toString());
       inlineEvalXHR.send(body);
       var inlined = inlineEvalXHR.responseText;
       return inlined;
   } else throw "Dynamic code evaluation is impossible: cannot create XMLHttpRequest";
}

// return the original eval function
function originalEval ()/*:Function*/ {
   return eval;
}

function inlineScriptURL(url/*:string*/)/*:string*/ {
   if (inlineEvalXHR !== null) {
       // the request is synchronous because we need to evaluate the
       // code before we can resume program execution
       inlineEvalXHR.open("GET", document.URL, false);

       var noVarDecls = pclabel.leq(lowlevel());
       // set a custom header to signal to the proxy that this is an eval
       // inlining request
       inlineEvalXHR.setRequestHeader("X-Inline-URL", "monitorvar=" + monitorPrefix + ";novardecls="+noVarDecls.toString());
       inlineEvalXHR.send(url);
       var inlined = inlineEvalXHR.responseText;
       return inlined;
   } else throw "Dynamic script creation is impossible: cannot create XMLHttpRequest";

}

// return true if the supplied box is a reference to the original eval
// proxy; false otherwise
function isEvalProxy (b/*:Box<any>*/) {
   return IsFunBox(b) && b.m.nativeEvalProxy;
}


function oplt (l/*: Box<any>*/, r/*: Box<any>*/)/*: Box<boolean>*/ {
  'use strict';

  var pl = ToPrimitiveBox(l,true),
  pr = ToPrimitiveBox(r,true);
  return primbox(pl.v < pr.v,pl.l.join(pr.l));
}

function opleq (l/*: Box<any>*/, r/*: Box<any>*/)/*: Box<boolean>*/ {
  'use strict';

  var pl = ToPrimitiveBox(l,true),
      pr = ToPrimitiveBox(r,true);
  return primbox(pl.v <= pr.v, pl.l.join(pr.l));
}

function opgt (l/*: Box<any>*/, r/*: Box<any>*/)/*: Box<boolean>*/ {
  'use strict';

  var pl = ToPrimitiveBox(l,true),
      pr = ToPrimitiveBox(r,true);
  return primbox(pl.v > pr.v,pl.l.join(pr.l));
}

function opgeq (l/*: Box<any>*/, r/*: Box<any>*/)/*: Box<boolean>*/ {
  'use strict';
  var pl = ToPrimitiveBox(l,true),
      pr = ToPrimitiveBox(r,true);
  return primbox(pl.v >= pr.v, pl.l.join(pr.l));
}
 
function opin (l/*: Box<any>*/, r/*: Box<any>*/)/*: Box<boolean>*/ {
  'use strict';
  if (! (IsObjectBox(r) || IsFunBox(r))) {
    throwTypeError();
    // unreachable, but satisfies the typechecker
    return l;
  } else {
    var or = (IsFunBox(r) && !IsObjectBox(r))  ?  ToObjectBox(r)  :  r;
    var ls = ToStringBox(l);
    return primbox(ls.v in or.v, ls.l.join(or.l));
  }
}

function opinstanceof (o/*: Box<any>*/, f/*: Box<any>*/)/*: Box<boolean>*/ {
  'use strict';
  if (!IsFunBox(f)) {
    throwTypeError();
  } else {
    var outlev = o.l.join(f.l);
    //15.3.5.3
    if (!(IsObjectBox(f) || IsFunBox(f))) {
      //1
      return primbox(false, outlev);
    } else {
      //2
      var fproto/*:Box<Object>*/ = readField(f, primlow("prototype"));
      outlev = outlev.join(fproto.l);
      //3
      if (!(IsObjectBox(fproto) || IsFunBox(fproto))) {
	throwTypeError();
      }
      //4
      var O = fproto.v;
      var V = ToObjectBox(o).v;
      //termination relies on the fact that a prototype chain is acyclic and is rooted at null
      while (true){
	//4.a
	V = Object.getPrototypeOf(V);
	//4.b
	if (V === null) return primbox(false, outlev);
        else if (O === V) return primbox(true, outlev);
      }
      return primbox(false, outlev);
    }
  }
  /*:: return primbox(false, f.l.join(f.l));*/
}

function opeq (l/*: Box<any>*/, r/*: Box<any>*/)/*: Box<boolean>*/ {
  'use strict';
  return primbox(AbstractEqCmp(l,r), l.l.join(r.l));
}
 
function opneq (l/*: Box<any>*/, r/*: Box<any>*/)/*: Box<boolean>*/ {
  'use strict';
  return primbox(! AbstractEqCmp(l,r), l.l.join(r.l));
}
 
function opstricteq (l/*: Box<any>*/, r/*: Box<any>*/)/*: Box<boolean>*/ {
  'use strict';
  return primbox(AbstractStrictEqCmp(l, r), l.l.join(r.l));
}
 
function opstrictneq (l/*: Box<any>*/, r/*: Box<any>*/)/*: Box<boolean>*/ {
  'use strict';
  return primbox(! AbstractStrictEqCmp(l, r), l.l.join(r.l));
}

function opmul (l/*: Box<any>*/, r/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
  var pl = ToNumberBox(l),
      pr = ToNumberBox(r);
  return primbox(pl.v * pr.v,pl.l.join(pr.l));
}

function opdiv (l/*: Box<any>*/, r/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
  var pl = ToNumberBox(l),
      pr = ToNumberBox(r);
  return primbox(pl.v / pr.v,pl.l.join(pr.l));
}

function opmod (l/*: Box<any>*/, r/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
  var pl = ToNumberBox(l),
      pr = ToNumberBox(r);
  return primbox(pl.v % pr.v,pl.l.join(pr.l));
}

function opsub (l/*: Box<any>*/, r/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
  var pl = ToNumberBox(l),
      pr = ToNumberBox(r);
  return primbox(pl.v - pr.v,pl.l.join(pr.l));
}

function oplshift (l/*: Box<any>*/, r/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
  var pl = ToNumberBox(l),
      pr = ToNumberBox(r);
  return primbox(pl.v << pr.v, pl.l.join(pr.l));
}

function opsprshift (l/*: Box<any>*/, r/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
  var pl = ToNumberBox(l),
      pr = ToNumberBox(r);
  return primbox(pl.v >> pr.v, pl.l.join(r.l));
}

function opzfrshift (l/*: Box<any>*/, r/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
  var pl = ToNumberBox(l),
      pr = ToNumberBox(r);
  return primbox(pl.v >>> pr.v, pl.l.join(pr.l));
}

function opband (l/*: Box<any>*/, r/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
  var pl = ToNumberBox(l),
      pr = ToNumberBox(r);
  return primbox(pl.v & pr.v, pl.l.join(pr.l));
}

function opbxor (l/*: Box<any>*/, r/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
  var pl = ToNumberBox(l),
      pr = ToNumberBox(r);
  return primbox(pl.v ^ pr.v, pl.l.join(pr.l));
}

function opbor (l/*: Box<any>*/, r/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
  var pl = ToNumberBox(l),
      pr = ToNumberBox(r);
  return primbox(pl.v | pr.v,pl.l.join(pr.l));
}

function opadd (l/*: Box<any>*/, r/*: Box<any>*/)/*: Box<number|string>*/ {
  'use strict';
  var pl = ToPrimitiveBox(l),
      pr = ToPrimitiveBox(r);
  return primbox(pl.v + pr.v,pl.l.join(pr.l));
}
 
function oplnot (x/*: Box<any>*/)/*: Box<boolean>*/ {
  'use strict';
  var px = ToBooleanBox(x);
  return primbox(! px.v,px.l);
}
 
function opbnot (x/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
  var px = ToNumberBox(x);
  return primbox(~ px.v,px.l);
}
 
function opplus (x/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
  var px = ToNumberBox(x);
  return primbox(+ px.v,px.l);
}

function opminus (x/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
  var px = ToNumberBox(x);
  return primbox(- px.v,px.l);
}

function optypeof (x/*: Box<any>*/)/*: Box<string>*/ {
  'use strict';
  var tys;
  if (IsPrimBox(x))
    tys = typeof x.v;
  else if (IsFunBox(x))
    tys = "function";
  else tys = "object";
  return primbox(tys,x.l);
}

function varprefixinc (v/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
  var px = ToNumberBox(v);
  px.v++;
  assignVar(v, px);
  return px;
}

function fieldprefixinc (obj/*: Box<any>*/, field/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
  var f = readField(obj, field);
  var px = ToNumberBox(f);
  px.v++;
  return assignField(obj, field, px);
}

function varprefixdec (v/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
  var px = ToNumberBox(v);
  px.v--;
  assignVar(v, px);
  return px;
}

function fieldprefixdec (obj/*: Box<any>*/, field/*:Box<any>*/)/*: Box<number>*/ {
  'use strict';
  var f = readField(obj, field);
  var px = ToNumberBox(f);
  px.v--;
  return assignField(obj, field, px);
}
 
function varpostfixinc (v/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
   var oldv = ToNumberBox(v);
   oldv.v++;
   assignVar(v, oldv);
   oldv.v--;
   return oldv;
}

function fieldpostfixinc (obj/*: Box<any>*/, field/*:Box<any>*/)/*: Box<number>*/ {
  'use strict';
  var f = readField(obj, field);
  var px = ToNumberBox(f);
  px.v++;
  assignField(obj, field, px);
  px.v--;
  return px;
}

function varpostfixdec (v/*: Box<any>*/)/*: Box<number>*/ {
  'use strict';
   var oldv = ToNumberBox(v);
   oldv.v--;
   assignVar(v, oldv);
   oldv.v++;
   return oldv;
}

function fieldpostfixdec (obj/*: Box<any>*/, field/*:Box<any>*/)/*: Box<number>*/ {
   'use strict';
   var f = readField(obj, field);
   var px = ToNumberBox(f);
   px.v--;
   assignField(obj, field, px);
   px.v++;
   return px;
}
