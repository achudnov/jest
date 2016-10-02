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

/*::type FieldMap = {[key: string]: PropertyDescriptor;
                     prototype?: PropertyDescriptor;}*/

/*::declare class ValueField {
  enumerable: boolean;
  writable: boolean;
  configurable: boolean;
  value: any;
};*/
/*::declare class AccessorField {
  enumerable: boolean;
  configurable: boolean;
  get: () => any;
  set: (x: any) => void;
};*/
/*::declare class ReadOnlyAccessor {
  enumerable: boolean;
  configurable: boolean;
  get: () => any;
}*/

/*::type PropertyDescriptor = ValueField | AccessorField | ReadOnlyAccessor;*/

function ObjectProxy(proto/*: ?Box<Object>*/, meta/*: Meta*/, arrayLike/*: boolean*/)/*: Box<Object>*/ {
  'use strict';
  var obj = Object.create(proto?proto.v:null);
  return newbox(obj, lowlevel(), meta, true, false, arrayLike);
};

function addFields(object/*: Box<Object>*/, fields/*: FieldMap*/)/*: void*/ {
  'use strict';
  Object.defineProperties(object.v, compileFieldMap(true, fields));
}

var nextFunctionId = 1;

//Spec 13.2 (follows partially)
function InitFunctionProxy(fp/*:Box<Object>*/, f/*:Function*/, prototype/*::?:Box<Object>*/)/*:Box<Object>*/ {
  fp.t = 3;
  fp.m.func = f;
  //3.
  fp.m.class = "Function";
   
  //14
  var len = primbox(f.length, lowlevel());
  //15
  Object.defineProperty(fp.v, "length", {value: len, writable: false, enumerable: false, configurable: false});
  //16
  var proto = prototype?prototype:ObjectProxy(ObjectProto, {class: "Object"}, false);
  //17
  Object.defineProperty(proto.v, "constructor", {value: fp, writable: true, enumerable: false, configurable: true});
  //18
  Object.defineProperty(fp.v, "prototype", {value: proto, writable: true, enumerable: false, configurable: false});
  //19 is unsupported for now
  //20

  fp.m.funId = nextFunctionId++;
  return fp;
}

function FunctionProxy(f/*: Function*/)/*: Box<Object>*/ {
  'use strict';
  var fp = ObjectProxy(FunctionProto, {}, false);
  return InitFunctionProxy(fp, f);
}

function ConstructorProxy(ctor/*:Function*/, proto/*:Box<Object>*/)/*:Box<Object>*/ {
  'use strict';
  return (function (constructor, prototype) {
    //A helper function to implement the "native constructor called as a
    //function" paradigm
    var ctor2 = function () {
      // check if 'this' is an instance of our constructor
      if (this && Object.getPrototypeOf(this.v) === prototype.v) 
	  return constructor.apply(this, arguments);
      else return newObject(cb, arguments);
    };
    var cb = ObjectProxy(FunctionProto, {}, false);
    InitFunctionProxy(cb, ctor2, prototype);
    return cb;
  })(ctor, proto);
}

function ValueProxy(v/*: any*/)/*: Box<any>*/ {
  'use strict';
  return autolow(v);
}

function compileFieldMap(attemptLazy/*: boolean*/, fields/*: FieldMap*/)/*: FieldMap*/ {
  'use strict';
  var pdm/*:FieldMap*/ = {};
  for (var f in fields) {
    pdm[f] = compilePropertyDescriptor(attemptLazy, f, fields[f]);
  }
  return pdm;
}

function compilePropertyDescriptor(attemptLazy/*: boolean*/, fieldName/*: string*/, field/*: PropertyDescriptor*/)/*: PropertyDescriptor*/ {
  'use strict';
  var lazyInit = attemptLazy && field.configurable;
  if (lazyInit) {
    var pd/*:PropertyDescriptor*/ = 
	  {enumerable: field.enumerable
	  ,configurable: field.configurable
	  ,get: function () {
	    Object.defineProperty(this, fieldName, compilePropertyDescriptor(false, fieldName, field));
	    var O/*:Object*/ = this;
	    return O[fieldName];}
	  ,set: function (v) {
	    if (/*:: field instanceof ValueField && */ "writable" in field && field.writable) {
	      Object.defineProperty(this, fieldName, compilePropertyDescriptor(false, fieldName, field));
	      var O/*:Object*/ = this;
	      O[fieldName] = v;
	    };
	  }
	  };
  } else {
    pd = ConvertPropertyDescriptor(field);
  }
  return pd;
}

/* Note [Conversion of property descriptors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Since we need to reimplement the getter/setter mechanism, we cannot
use property descriptors verbatim. Hence, property descriptors are
translated as follows (where nativeDesc is the original descriptor,
compiledDesc is the descriptor actually used to define the property).

compiledDesc.enumerable = nativeDesc.enumerable
compiledDesc.configurable = nativeDesc.configurable
compiledDesc.value = (nativeDesc.value !== undefined)?nativeDesc.value:Ref(nativeDesc.get, nativeDesc.set)
compiledDesc.writable = nativeDesc.writable
*/

function ConvertPropertyDescriptor(desc/*:PropertyDescriptor*/)/*: ValueField*/ {
  if (/*::desc instanceof ValueField &&*/ "value" in desc) {
    var converted /*:ValueField*/ = {enumerable: desc.enumerable
				    ,configurable: desc.configurable
				    ,value: desc.value
		          	    ,writable: desc.writable
				    };
  } else {
    /*:: if (desc instanceof AccessorField) */
    var getter = desc.get || function () {return primlow(void 0);};
    var setter = desc.set || function (x) {};
    converted = {enumerable: desc.enumerable
		,configurable: desc.configurable
		,value: newref(getter, setter)
		,writable: true};
  }
  return converted;
}

/* Note [DOM Facades]
~~~~~~~~~~~~~~~~~~~~~

There are several differences between ECMAScript standard library and
DOM facades. First, while DOM objects have a prototype hierarchy, many
don't have constructors. But some of those can still be constructed
using factory functions/interfaces. Second, most DOM object facades
need to be linked to the underlying objects to be able to perform
IO. We need to be able to construct facades for a given object --- a
process we call lifting.

For every DOM interface we define a "lifter", which is a pair of an
initializer function and a prototype object. The initializer takes an
underlying native DOM object and a boxed instance of the facade and
initializes it's value and meta fields to serve as a facade for the
native. The prototype object is the object that is supposed to be the
prototype of the value part of the facade box.

There is (single) inheritance in lifters that is interpreted as
follows: 
- the prototype object of the child has the prototype object of the
parent for a prototype
- the initializer of the parent is invoked before the initializer of
the child on the same objects

The lifter than has two methods: 'initialize', 'lift'. The first one
invokes the initializer functions up the inheritance chain. The second
one takes a native DOM object, constructs a new object that is an
instance of the prototype and then passes the new object and the
native object to 'initialize'.

These methods are defined for every lifter. The user only needs to
specify the initializer, the prototype object and the parent (if any).
*/

function Lifter(initer/*:(native: Object, facade: Box<Object>) => Box<Object>*/
	       ,prototypeFields/*:FieldMap*/
	       ,prototypeMeta/*:Meta*/
	       ,parent/*:Lifter|null*/)/*:Lifter*/ {
    this.initializer = initer;
    this.parent = parent;
    var protoProto/*:Box<Object>|null*/ = parent?parent.proto:ObjectProto;
    this.proto = ObjectProxy(protoProto, prototypeMeta, false);
    addFields(this.proto, prototypeFields);
    return this;
}

Lifter.prototype =
   {initialize: function (native/*:Object*/, facade/*: Box<Object>*/)/*:Box<Object>*/ {
	 var facade2 = facade;
	 if (this.parent) {
            facade2 = this.parent.initialize(native, facade2);
         }
	 return this.initializer(native, facade2);
      }
   ,lift: function (native/*:Object*/)/*:Box<Object>*/ {
      return this.initialize(native, ObjectProxy(this.proto, {}, false));
      }
   };

// WebIDL = WebIDL specification, http://www.w3.org/TR/WebIDL/

// WebIDL 4.4.5
function const_(p/*: Box<any>*/)/*:ValueField*/ {
  return constant(p);
}

function constant(b/*:Box<any>*/)/*:ValueField*/ {
  return {enumerable: true
	 ,writable: false
	 ,configurable: false
	 ,value: b};
}

// WebIDL 4.4.6
function attribute(getter/*: () => Box<any>*/, setter/*::?: (x: Box<any>) => void*/, unforgeable/*::?: boolean*/)/*: AccessorField | ReadOnlyAccessor*/ {
  if (setter !== null && typeof(setter) !== 'undefined')
    var retval /*:AccessorField | ReadOnlyAccessor*/ =
      {enumerable: true
      ,configurable: (typeof unforgeable === 'boolean'? !unforgeable : true)
      ,get: getter
      ,set: setter
      };
  else retval = {enumerable: true
		,configurable: (typeof unforgeable === 'boolean'? !unforgeable : true)
		,get: getter
		};
  return retval;
}

function field(initial/*::?: any*/)/*:ValueField*/ {
  return {enumerable: true
	 ,writable: true
	 ,configurable: true
	 ,value: initial};
}

// WebIDL 4.4.7
function method(m/*:Function*/)/*:ValueField*/ {
  return {enumerable: true
	 ,configurable: true
         ,writable: true
	 ,value: FunctionProxy(m)
	 };
}

// WebIDL 4.4.1.1, an interface without a [Constructor] extended
// attribute. Use ConstructorProxy for an interface with a
// [Constructor] extended attribute
function interfaceObject(proto /*:?Box<Object>*/)/*: Box<Object>*/ {
    var i = ObjectProxy(proto, {}, false);
    return InitFunctionProxy(i, function () {throwTypeError();});
}
