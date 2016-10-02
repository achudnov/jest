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


//inspired by dom.js by Andreas Gal and Mozilla Corp.

//externs
/*::declare var policy: {levelImpl: () => Level;
                         locationLevel: (chan: string, write: boolean) => Label;};*/

// HTML DOM Environment externs

//policy check: we are attempting to output a piece of data with level
//'dlev' on channel with name 'channel'. If it does not agree with the
//policy, raise a monitor violation.
function policyCheck(dlev/*: Label*/, channel/*: string*/)/*: void*/ {
  if (!checkOutputLevel(channel, dlev))
    stop("", 0, 0, "Output policy violation: attempted to output data of level "+
	dlev.toString() + " on channel " + channel + " with maximum level of " + policy.locationLevel(channel, true).toString());
}

//Returns the input level of a channel
function inputLevel(channel/*:string*/)/*: Label*/ {
  return policy.locationLevel(channel, false);
}

function checkOutputLevel(channel/*:string*/, label/*:Label*/)/*:boolean*/ {
  var chanLab = policy.locationLevel(channel, true);
  return pclabel.join(label).leq(chanLab);
}

// Follows the DOM4 spec (http://www.w3.org/TR/dom/) and HTML5 spec (http://www.w3.org/TR/html5/)

// var EventListenerProto = ObjectProxy({
//   handleEvent: method(function (event/*:Box<object>*/)/*:Box<void>*/ {
//     return primlow(void 0);
//   })
// }, ObjectProto, {class: "EventListener"}, false);

// var EventListenerCtor = ConstructorProxy(function (f/*::?:Box<any>*/) {
//   if (f && IsFunBox(f)) this.v.handleEvent = f;
// }, EventListenerProto, {});

/* Event facade */

var EventProto = ObjectProxy(ObjectProto, {class: "EventPrototype"}, false);

addFields(EventProto, {
  preventDefault: method(function () {
    nsuCheckLev(lowlevel());
    this.m.underlying.preventDefault();
  })
  ,stopPropagation: method(function () {
    nsuCheckLev(lowlevel());
    this.m.underlying.stopPropagation();
  })
});

var EventCtor = ConstructorProxy(function (type/*:Box<any>*/)/*:Box<Object>*/ {
  var eventType = ToStringBox(type).v;
  this.m.underlying = new Event(eventType);
  addFields(this, {type: constant(type)});
  return this;
}, EventProto);

function liftEvent(evt/*:Event*/)/*:Box<Object>*/ {
  var e = newObject(EventCtor, [primlow(evt.type)]);
  e.m.underlying = evt;
  if ("target" in evt) 
      addFields(e, {target: constant(liftEventTarget(evt.target))});
  return e;
}

/* Note [Event Handlers]
~~~~~~~~~~~~~~~~~~~~~~~~

Events are a potential source of implicit flows: the fact that the
handler has been called depends on the PC at the time of setting,
security level of the callback itself, as well as the PC level under
which the event was raised. It also depends on whether stopPropagation
has been called in an event handler invoked in a downstream
node. However, if any handler are invoked for a node, then all of the
other handlers are guaranteed to be invoked.

Additionally, calling preventDefault might prevent side effects, such
as form submission.

StopPropagation and preventDefault effectively assign to an internal
low field, thus they are subject to an NSU check: we need to raise a
violation if they are called in a high context.

When attaching an event handler we need to set the level of it's
reference to the join of argument levels and pc. When the handler is
called the PC will be set to the level of the reference.

Handlers are stored in an array of boxes in this.m.handlers.<event_name>. We
only attach one event listener to the underlying node: it will call
dispatchEvent on the fa\c cade. The native handler is added when the
first listener is added to the fa\c cade and removed when the last one
is. It is saved in this.m.nativeHandler to enable removing it later.

We don't do existence tracking/checking for the array of event
handlers, as there isn't a way to check a handlers existence.
*/

/* EventTarget facade*/

var EventTargetProtoFields = {
  addEventListener: method(function (type/*:Box<any>*/, callback/*:Box<any>*/, capture/*::?: Box<any>*/)/*:Box<void>*/ {
    // function liftEventListener(cb /*:Box<any>*/){
    //    if (IsFunBox(cb)) {
    // 	 var listener = newObject(EventListenerCtor, [cb]);
    //    } else if (opinstanceof(cb, EventListenerCtor).v) {
    // 	 listener = cb;
    //    } else listener = null;
    //    return listener;
    // }

    // Makes a native event listener that would invoke dispatchEvent
    // on a given facade
    function makeNativeListener(evtTargetFacade/*:Box<Object>*/, event_type/*:string*/)
      /*:function*/ {
	  return (function(facade) {
	      return function (event) {
		  //if the event was sent from the browser, it is unboxed
		  if (event instanceof Event)
		      var boxEvent = liftEvent(event);
		  //otherwise, the event was sent from the application and the
		  //event is boxed
		  else boxEvent = event;
		  invokeMethod(facade, primlow("dispatchEvent"), [boxEvent]);
	      };
	  })(clonebox(evtTargetFacade));
    }

    //We only support event listeners that are functions; this is not
    //quite spec compliant, but, nevertheless, practical.
    if (IsFunBox(callback)) {
	var typ = ToStringBox(type);
	var cap = capture?ToBooleanBox(capture):primlow(false);
	
	var listener = join2(callback, pclabel.join(typ.l.join(cap.l)));
	
	var event_name = typ.v;
	this.m.handlers = this.m.handlers || [];
	this.m.handlers[event_name] = this.m.handlers[event_name] || [];
	var needNativeHandler = !(this.m.handlers[event_name].length > 0) 
                             && !this.m.nativeHandler;
	if (needNativeHandler) this.m.underlying.addEventListener(typ.v, makeNativeListener(this, event_name), cap.v);
	this.m.handlers[event_name].push(listener);
    }
    return primlow(void 0);
  })
 //TODO: removeEventListener
 ,dispatchEvent: method(function (event/*:Box<Event>*/)/*:void*/{
     var event_name = ToStringBox(readField(event, primlow("type"))).v;
     var handlers = this.m.handlers?this.m.handlers[event_name]:(void 0);
     if (handlers && Array.isArray(handlers)) 
	 handlers.forEach(function (bh) {
	     invokeCommon(bh, this, [event]);
	 }, this);
  })
};

function initEventTarget(etarget /*:EventTarget*/, facade/*:Box<Object>*/)/*:Box<Object>*/ {
   facade.m.class = "EventTarget";
   facade.m.handlers = {};
   return facade;
}

var EventTargetLifter = new Lifter(initEventTarget, EventTargetProtoFields, {class: "EventTargetProto"}, null);



/* Note [DOM Forest Proxying]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We maintain a facade for DOM that partially mimics the structure of
the underlying forest. There is an injective mapping from nodes and
edges of Proxy DOM to those in the actual DOM, which is an isomorphism
for edges: if two nodes n1 and n2 are in PDOM, and their images in DOM
have an edge between them, there is a corresponding edge in PDOM as
well. The mapping is implemented as the "underlying" field in box metadata.

When a client script requests a reference to a DOM node that is
present in the underlying DOM, but not in the proxy forest, the
underlying node is lifted to a proxy. We check whether its children
and parent in DOM already have proxy nodes, so we could link them in
PDOM and maintain the isomorphism. In order to do this, we maintain a
partial injective mapping from DOM nodes to their proxies. It is
implementaed as an extra property "facade" in DOM nodes that points to
the PDOM node, if it exists.

We initialize the security levels of the proxy node equal to
the current PC level. Note that it is safe because the only way this
situation can happen is if the node was created from the underlying
HTML document, and hasn't been touched (yet) by any scripts.

The lifting is done by a function liftNode.

 np={v: Object (with NodeProtoProxy in the prototype chain)
    ,l: level
    ,m: {underlying: DOMNode ({facade: np})
        ,parent: NodeProxy (of the parent)
	,children: Box<Array<NodeProxy>> (of the children)
        }
    ,t: <Object>
    }

We use an Array box instead of another custom structure for storing
children. While this is not according to spec, this allows us to reuse
structure storage channel handling in arrays, which should be similar
if not identical. We conjecture that as long as this array box is not
exposed to the client code directly, it's going to be
indistinguishable from the intended semantics of a node list.
 */

/* Node facade */

//Takes a DOM Node instance and determines it's most specific interface ("class")
function getNodeClass(n/*:Node*/)/*:?string*/ {
   switch (n.nodeType) {
      case Node.ELEMENT_NODE: /*::if (n instanceof Element) */
	 return n.tagName.toLowerCase();
      case Node.TEXT_NODE:
	 return "text";
      case Node.DOCUMENT_NODE:
	return "document";
      default: return;
   }
}


//Looks up the lifter in 'nodeLifters'; if not found, returns a generic lifter
function getLifterByTagName(tag/*:string*/)/*:Lifter*/{
   var lifter = nodeLifters[tag];
   if (!lifter) lifter = genericElementLifter;
   return lifter;
}

//Creates a facade for an existing DOM node. Initializes everything
//and maintains the isomorphism for edges. See Note [DOM Forest
//Proxying] for more details
function liftNode (n/*:Node*/)/*:Box<Object>*/{
   //If the node already has a facade, return it
   if (!n) return primlow(n);
   if (n.facade) return n.facade;
    
    //Otherwise we need to determine what kind of node is it and create
   //an appropriate proxy for it.
   var cl = getNodeClass(n);
   if (!cl) {throwTypeError();
             /*:: cl = "";*/
	    }

   var lifter = getLifterByTagName(cl);
   
   return lifter.lift(n);
}


function defineLazyPropertyWithInit(o, p, f) {
  Object.defineProperty(o, p, 
  {configurable: true
  ,enumerable: true
  ,get: (function (obj, prop, fun) {
      return function () {
	  var newval = fun();
	  Object.defineProperty(obj, prop, {configurable: true, enumerable: true, writable: true, value: newval});
    return newval;
      };
  })(o, p, f)
  ,set: (function (obj, prop) {
      return function (v) {
	  Object.defineProperty(obj, prop, {configurable: true, enumerable: true, writable: true, value: v});
      };
  })(o, p)
  });
}

function NodeInitializer (node/*:Node*/, facade/*:Box<Object>*/)/*:Box<Object>*/ {
   //Link the facade and the underlying nodes
   facade.m.underlying = node;
   node.facade = facade;

   //Now we need to load children and the parent *lazily*. They are
   //stored in m.children and m.parent respectively.
   defineLazyPropertyWithInit(facade.m, "parent", (function(node) {
      return function () {
	    return liftNode(node.parentNode);
      };
   })(node));
   defineLazyPropertyWithInit(facade.m, "children", (function (node) {
      return function () {
	 var kids  = [];
	 var ukids = node.childNodes;
	 var ukl   = ukids.length;
	 for (var i = 0; i < ukl; i++) {
	    kids.push(liftNode(ukids[i]));
	 }
	 return arraylow(kids);
      };
   })(node));
   return facade;      
}

var NodeProtoFields = {parentNode: attribute(function ()/*:Box<?Object>*/ {
      var O/*:Box<Object>*/ = this;
      var p = O.m.parent;
      return (p?p:primlow(null));
    })
    ,hasChildNodes: const_(FunctionProxy(function()/*:Box<boolean>*/ {
	var O /*: Box<Object>*/ = this;
	var children = this.m.children;
	var len = ToNumberBox(readField(children, primlow("length")));
	return primbox(len.v > 0, len.l);
     }))
    ,childNodes: attribute(function ()/*:Box<Array<Box<Object>>>*/ {
	// TODO: Adapt the array to a NodeList, i.e. freeze it.
	return this.m.children; 
    })
    ,firstChild: attribute(function()/*:Box<?Object>*/ {
	var O/*:Box<Object>*/ = this;
	var children = this.m.children;
	var len = ToNumberBox(readField(children, primlow("length")));
	var retlab = len.l;
	return len.v === 0 ? primbox(null, len.l) : readField(children, primlow("0"));
        })
    ,lastChild: attribute(function()/*:Box<?Object>*/ {
	var O /*:Box<Object>*/ = this;
	var children = this.m.children;
	var len = ToNumberBox(readField(children, primlow("length")));
	return len.v === 0 ? primbox(null, len.l) : 
	  readField(children, primbox(len.v - 1, len.l));
    })
    ,nextSibling: attribute(function() {
    	var O /*:Box<Object>*/ = this;
    	var parent = this.m.parent;
    	if (!parent || (!parent.v /*::&&typeof parent.v === null*/)) return primbox(null, parent.l);
    	var sibs = parent.m.children;
    	var i = readField(this, primlow("index")).v;
        var l = readField(sibs, primlow("sibs")).v;
    	return i+1 === l
                ? primlow(null)
                : readField(sibs, primlow(i+1));
    })
    ,appendChild: const_(FunctionProxy(function(child/*:Box<Object>*/)/*:Box<Object>*/ {
      return preInsert(child, this, primlow(null));
    }))
    ,removeChild: const_(FunctionProxy(function(child/*:Box<Object>*/)/*:Box<Object>*/ {
	return preRemove(child, this);
      }))
};

var NodeLifter = new Lifter(NodeInitializer, NodeProtoFields, {class: "NodePrototype"}, EventTargetLifter);

var NodeIFace = interfaceObject(NodeLifter.proto);

//Utility functions for Node proxies
function preInsert (node/*:Box<Object>*/, parent/*:Box<Object>*/, child/*:Box<?Object>*/)/*:Box<Object>*/ {
    /*TODO: ensure pre-insertion validity of the node*/
    var refChild = child;
    if (node === refChild) refChild = readField(child, primlow("nextSibling"));
    /*TODO: adopt the child into the parent's document*/
    insert(node, parent, refChild);
    return node;
}

function preRemove (node/*: Box<Object>*/, parent/*: Box<Object>*/)/*: Box<Object>*/ {
    pushExceptionLabel(node.l.join(parent.l));
    if (node.m.parent.v !== parent.v) throw NotFoundError();
    remove(node);
    return node;
}

var NotFoundErrorCtor = ErrorCtor;
var NotFoundErrorProto = ObjectProxy(ErrorProto, {class: "NotFoundError"}, false);
var NotFoundErrorProtoFields = {name: ESSLField(ValueProxy("NotFoundError"))
			       ,message: ESSLField(ValueProxy("The object can not be found here"))
			       };
addFields(NotFoundErrorProto, NotFoundErrorProtoFields);


// Remove this node from its parent
function remove(node/*:Box<Object>*/)/*:void*/ {
  // Remove this node from its parents array of children
  if (node.m.parent && node.m.parent.v && node.m.parent.m.children) {
    var sibs = node.m.parent.m.children;
    var ix = invokeMethod(sibs, primlow("indexOf"), [node]);
    invokeMethod(sibs, primlow("splice"), [ix, primlow(1)]);
    
    node.m.parent.m.underlying.removeChild(node.m.underlying);
    // Forget this node's parent
    node.m.parent = primlow(null);
  }
}
  
function insert(node/*:Box<Object>*/, parent/*:Box<Object>*/, child/*:Box<?Object>*/)/*:void*/ {
  /* TODO: support for node being a DocumentFragment */
  var nodeChildCount = 1;
  var children = parent.m.children;
  if (child.v !== null)
      var spliceIx = invokeMethod(children, primlow("indexOf"), [child]);
  else spliceIx = readField(children, primlow("length"));
  
  //insert the new child
  invokeMethod(children, primlow("splice"), [spliceIx, primlow(0), node]);
  
  //remove the node from its old parent
  remove(node);
  
  //TODO: support node adoption

  //set the new parent
  node.m.parent = parent;

  //propagate changes to the actual DOM
  var beforeNode = (child.v === null)?null:child.m.underlying;
  parent.m.underlying.insertBefore(node.m.underlying, beforeNode);
}

var NodeIface = interfaceObject(NodeLifter.proto);

addFields(NodeIface, {ELEMENT_NODE: const_(primlow(1))
 		     ,ATTRIBUTE_NODE: const_(primlow(2))
		     ,TEXT_NODE: const_(primlow(3))
		     ,CDATA_SECTION_NODE: const_(primlow(4))
		     ,ENTITY_REFERENCE_NODE: const_(primlow(5))
		     ,ENTITY_NODE: const_(primlow(6))
		     ,PROCESSING_INSTRUCTION_NODE: const_(primlow(7))
		     ,COMMENT_NODE: const_(primlow(8))
		     ,DOCUMENT_NODE: const_(primlow(9))
		     ,DOCUMENT_TYPE_NODE: const_(primlow(10))
		     ,DOCUMENT_FRAGMENT_NODE: const_(primlow(11))
		     ,NOTATION_NODE: const_(primlow(12))});

/* CharacterData facade */

var CharacterDataProtoFields =  {data: attribute(function ()/*:Box<string>*/ {
    return primbox(this.m.underlying.data, this.m.dataLevel);
  }, function (data/*:Box<any>*/)/*:void*/ {
    this.m.dataLevel = data.l;
    this.m.underlying.data = ToStringBox(data).v;
  })
  ,nodeValue: attribute(function (){return readField(this, primlow("data"));}
		       ,function (value){assignField(this, primlow("data"), value);})
};

function CharacterDataInitializer (cdata/*:CharacterData*/, facade/*:Box<Object>*/)/*:Box<Object>*/ {
   return facade;   
}

var CharacterDataLifter = new Lifter(CharacterDataInitializer, CharacterDataProtoFields, {class: "CharacterDataPrototype"}, NodeLifter);

var CharacterDataIFace = interfaceObject(CharacterDataLifter.proto);

/* Text facade */

function initText(t/*:Text*/, facade/*:Box<Object>*/)/*:Box<Object>*/{
  //we are passing an unboxed value; newObject actually doesn't care
  //whether we pass boxes as arguments
  facade.m.class = "Text";
  facade.m.dataLevel = lowlevel();
  facade.m.underlying = t?t:new Text();
  return facade;
}

var TextLifter = new Lifter(initText,{},{class: "TextPrototype"},CharacterDataLifter);

var TextConstructor = ConstructorProxy(function (data/*::?:Box<any>*/)/*:Box<Object>*/{
   var underlying = data?new Text(ToStringBox(data).v):new Text();
   var lab = data?data.l:lowlevel();
   var facade = TextLifter.initialize(underlying, this);
   return join2(facade, lab);
}, TextLifter.proto);


/* Element facade*/

function initElement(el/*:Element*/, facade/*:Box<Object>*/)/*:Box<Object>*/{
   facade.m.class = "Element";
   return facade;
}

var ElementLifter = new Lifter(initElement, {}, {class: "ElementPrototype"}, NodeLifter);

var ElementIFace = interfaceObject(ElementLifter.proto);

/* HTMLElement facade */

var HTMLElementLifter = new Lifter(function (el, facade) {facade.m.class = "HTMLElement"; return facade;}
				   ,{}, {class: "HTMLElementPrototype"}, ElementLifter);

var HTMLElementIFace = interfaceObject(HTMLElementLifter.proto);


/* Note [DOM Element IO Tracking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We would like to enforce the policy on individual HTML elements --- yet be flow-sensitive. 

A naive approach where, e.g., an input element returns the value boxed with the input level from the policy and allows writing anything up to the output level from the policy is prone to laundering: since the output level is always at least the input level in the policy (invariant), writing with a value higher level and reading it might reduce the label on that value. Instead, we use a floating valueLabel which is the upper bound of the input label, the previous valueLabel and the label on data written by the program. This prevents laundering. 

A more flexible approach would be to detect when the user edits the value herself and reset the valueLabel to the input level.
 */

/* HTMLSelectElement facade */

var HTMLSelectElementProtoFields = {
  //HTML5 4.10.7
  value: attribute(function ()/*:Box<any>*/ {
    /*The 'value' attribute, on getting, must return the value of the first option element in the list of options in tree order that has its selectedness set to true, if any. If there isn't one, then it must return the empty string.*/
    // temporary hack until the list of options, and the correct semantics for 'value', are implemented
    return primbox(this.m.underlying.value, this.m.valueLevel);
  },function (opt/*:Box<any>*/)/*:void*/ {
    /*On setting, the value attribute must set the selectedness of all the option elements in the list of options to false, and then the first option element in the list of options, in tree order, whose value is equal to the given new value, if any, must have its selectedness set to true and its dirtiness set to true.*/
    // temporary hack until the list of options, and the correct semantics for 'value', are implemented
    nsuCheckLev(this.m.valueLevel);
    if (checkOutputLevel("dom://" + this.m.underlying.id, opt.l)) {
      this.m.valueLevel = opt.l;
      this.m.underlying.value = ToStringBox(opt).v;
    }
  })
};

function HTMLSelectElementInitializer (el/*:HTMLSelectElement*/, facade/*:Box<Object>*/)/*:Box<Object>*/ {
   facade.m.class = "HTMLSelectElement";
   facade.m.valueLevel = inputLevel("dom://" + this.m.underlying.id);
   return facade;
}


var HTMLSelectElementLifter = new Lifter(HTMLSelectElementInitializer, HTMLSelectElementProtoFields, {class: "HTMLSelectElementPrototype"}, HTMLElementLifter);

/* HTMLScriptElement facade */


var HTMLScriptElementProtoFields = {
  src: attribute(function ()/*:Box<string>*/{ 
    // temporary hack
    return primbox(this.m.script.src, this.m.srcLevel);
  }, function (value/*:Box<any>*/)/*:void*/ {
    var url = ToStringBox(value).v;
    policyCheck(value.l, url);
    nsuCheckLev(this.m.srcLevel);

    this.m.srcLevel = value.l;
    this.m.script.text = inlineScriptURL(url);
  })
};

function initScriptElement(script/*:HTMLScriptElement*/, facade /*:Box<Object>*/)/*:Box<Object>*/{
  facade.m.class = "HTMLScriptElement";
  facade.m.srcLevel = lowlevel();
  facade.m.script = script;
  return facade;
}

var HTMLScriptElementLifter = new Lifter(initScriptElement, HTMLScriptElementProtoFields, {class: "HTMLScriptElementPrototype"}, HTMLElementLifter);

/* HTMLFormElement facade */

function initHTMLForm (form/*:HTMLFormElement*/, facade/*:Box<Object>*/)/*:Box<Object>*/ {
    facade.m.class = "HTMLFormElement";
    // TODO get the elements from the underlying element
    var ownedElems = facade.m.underlying.elements;
    var ownedElemsProxies /*:Array<Box<Object>>*/ = new Array(ownedElems.length);
    for (var i=0; i < ownedElems.length; i++) {
	ownedElemsProxies[i] = liftNode(ownedElems[i]);
    }
    facade.m.ownedElements = ownedElemsProxies;
    return facade;
}

var HTMLFormElementProtoFields = {
    // TODO: this would need to be changed to support live collectionsm
    elements: attribute(function () {
	return arraylow(this.m.ownedElements);
    })
};

var HTMLFormElementLifter = new Lifter(initHTMLForm, HTMLFormElementProtoFields, {class: "HTMLFormElementPrototype"}, HTMLElementLifter);

/* HTMLInputElement facade */

function initHTMLInputElement(input/*:HTMLInputElement*/, facade/*:Box<Object>*/)/*:Box<Object>*/{
  facade.m.class = "HTMLInputElement";
  facade.m.valueLevel = inputLevel("dom://" + facade.m.underlying.id);
  facade.v.name = primlow(facade.m.underlying.name);
  facade.v.type = primlow(facade.m.underlying.type);
  return facade;
}

var HTMLInputElementProtoFields = {
  value: attribute(function ()/*:Box<any>*/ {
    /*The 'value' attribute, on getting, must return the value of the first option element in the list of options in tree order that has its selectedness set to true, if any. If there isn't one, then it must return the empty string.*/
    // temporary hack until the list of options, and the correct semantics for 'value', are implemented
    return primbox(this.m.underlying.value, this.m.valueLevel);
  },function (v/*:Box<any>*/)/*:void*/ {
    /*On setting, the value attribute must set the selectedness of all the option elements in the list of options to false, and then the first option element in the list of options, in tree order, whose value is equal to the given new value, if any, must have its selectedness set to true and its dirtiness set to true.*/
    // temporary hack until the list of options, and the correct semantics for 'value', are implemented
    nsuCheckLev(this.m.valueLevel);
    if (checkOutputLevel("dom://" + this.m.underlying.id, v.l)) {
      this.m.valueLevel = v.l;
      this.m.underlying.value = ToStringBox(v).v;
    }
  }),
  files: attribute(function ()/*:Box<Object|undefined>*/ {
      if (this.m.underlying.files) {
	  var files = [];
	  for (var i=0; i < this.m.underlying.files.length; i++) {
	      var file = this.m.underlying.files[i];
	     files.push(join2(FileLifter.lift(file)
			     ,inputLevel("file://" + file.name)));
	  }
	  return arraylow(files);
      }
      else return primlow(void 0);
  }),
  name: field(primlow("")),
  type: field(primlow(""))
};

var HTMLInputElementLifter = new Lifter(initHTMLInputElement, HTMLInputElementProtoFields, {class: "HTMLInputElementPrototype"}, HTMLElementLifter);

/* HTMLTableElement facade */

function initHTMLTableElement(table/*:HTMLTableElement*/, facade/*:Box<Object>*/)/*:Box<Object>*/{
    facade.m.class = "HTMLTableElement";
    var tbods = facade.m.underlying.tBodies;
    var childTBodies /*:Array<Box<Object>>*/ = new Array(tbods.length);
    for (var i=0; i < tbods.length; i++) {
	childTBodies[i] = liftNode(tbods[i]);
    }
    facade.m.tbodies = childTBodies;

    return facade;
}

var HTMLTableElementProtoFields = {
    tBodies: attribute(function () {
	// TODO: this will need to be changed when we introduce support for live collections
	return arraylow(this.m.tbodies);
    })
};

var HTMLTableElementLifter = new Lifter(initHTMLTableElement, HTMLTableElementProtoFields, {class: "HTMLTablePrototype"}, HTMLElementLifter);

/* HTMLTableSectioElement facade */

function initHTMLTableSectionElement(tsection/*:HTMLTableSectionElement*/, facade/*:Box<Object>*/)/*:Box<Object>*/ {
    facade.m.class = "HTMLTableSectionElement";
    var rows = facade.m.underlying.rows;
    var rowProxies /*:Array<Box<Object>>*/ = new Array(rows.length);
    for (var i = 0; i < rows.length; i++) {
	rowProxies[i] = liftNode(rows[i]);
    }
    facade.m.rows = rowProxies;

    return facade;
}

var HTMLTableSectionElementProtoFields = {
    rows: attribute(function () {
	// TODO: this will need to be changed when we introduce support for live collections
	return arraylow(this.m.rows);
    })
};

var HTMLTableSectionElementLifter = new Lifter(initHTMLTableSectionElement, HTMLTableSectionElementProtoFields, {class: "HTMLTableSectionPrototype"}, HTMLElementLifter);

/* Document facade */

var DocumentProtoFields = {
    getElementById: method(function (id/*:Box<any>*/)/*:Box<Object|null>*/ {
	var sid = ToStringBox(id);
	var retlev = id.l; //TODO: account for level of existence
	var n/*:Box<Object|null>*/ = this.m.byId[sid.v];
	if (!n) {
	   // we fall back to looking up the element id's using the
	   // native interface; this would only return a non-null
	   // result if there is a node with this id in the original
	   // document, and no id's have been added by the JavaScript
	   // code; this also tells us that the security level of this
	   // node is only going to depend on that of the id and the PC
	   // since the node itself hasn't been created by the program.
	   var mun = document.getElementById(sid.v);
	   n = mun? liftNode(mun) : primlow(null);
	   n.l = n.l.join(sid.l).join(pclabel);
	}
	if (Array.isArray(n)) // there was more than one element with this id
	  return n[0];  // array is sorted in document order
	return n;
    })
   ,createElement: method(function (name/*:Box<any>*/)/*:Box<Object>*/{
      var tag = ToStringBox(name).v;
      var lifter = getLifterByTagName(tag);
      var underlying = document.createElement(tag);
      return join2(lifter.lift(underlying), name.l);
  })
   ,createTextNode: method(function (data/*::?: Box<any>*/)/*:Box<Object>*/{
      if (data) var params = [data]; else params = [];

      return newObject(TextConstructor, params);
  })
};

//DOM4 5.5

function initDocument(d/*:Document*/, facade/*:Box<Object>*/)/*:Box<Object>*/ {
   facade.m.class = "Document";
   facade.m.byId = {};
   return facade;
}

var DocumentLifter = new Lifter(initDocument, DocumentProtoFields, {class: "DocumentPrototype"}, NodeLifter);

/* HTMLIFrameElement facade */

function initIFrameElement(iframe/*:HTMLIFrameElement*/, facade/*:Box<Object>*/)/*:Box<Object>*/ {
    facade.m.class = "HTMLIFrameElement";
    facade.m.underlying = el?el:document.createElement("iframe");

    return facade;
}

var HTMLIFrameElementProtoFields = {
    contentWindow: attribute(function () {
	return this.m.underlying.contentWindow.facade;
    })
};

var HTMLIFrameElementLifter = new Lifter(initIFrameElement, HTMLIFrameElementProtoFields, {class: "HTMLIFrameElementPrototype"}, HTMLElementLifter);

var genericElementLifter = HTMLElementLifter;

//A map from "class"/tag names to facade lifters
var nodeLifters /*:{[key: string]:Lifter}*/ = {
   document: DocumentLifter,
   text: TextLifter,
   script: HTMLScriptElementLifter,
   input: HTMLInputElementLifter,
   form: HTMLFormElementLifter,
   select: HTMLSelectElementLifter,
   table: HTMLTableElementLifter,
   tbody: HTMLTableSectionElementLifter,
   iframe: HTMLIFrameElementLifter
};

/* XMLHttpRequestEventTarget facade */

function initXMLHttpRequestEventTarget(etarget/*:XMLHttpRequestEventTarget*/, facade/*:Box<Object>*/)/*:Box<Object>*/ {
   return facade;
}

var XMLHttpRequestEventTargetLifter = new Lifter(initXMLHttpRequestEventTarget, {}, {class: "XMLHttpRequestEventTargetPrototype"}, EventTargetLifter);

/* XMLHttpRequest facade */

function initXMLHttpRequest(xhr/*:XMLHttpRequest*/, facade/*:Box<Object>*/)/*:Box<Object>*/ {
  function nativeReadyStateChangeHandler() {
    var ors = this.v.onreadystatechange;
    if (ors && IsFunBox(ors)) invokeCommon(this, this.v.onreadystatechange, []);
  }
  this.m.xhr = new XMLHttpRequest();
  this.m.xhr.onreadystatechange = nativeReadyStateChangeHandler.bind(this);
  this.m.rslevel = lowlevel(); //level of the ready-state flag
  this.m.txlevel = lowlevel(); //data transmission level
  return this;
   
}

var XMLHttpRequestProtoFields = {
   onreadystatechange: field(undefined)
  ,UNSENT: const_(primlow(0))
  ,OPENED: const_(primlow(1))
  ,HEADERS_RECEIVED: const_(primlow(2))
  ,LOADING: const_(primlow(3))
  ,DONE: const_(primlow(4))
  ,readyState: attribute(function ()/*:Box<number>*/ {return primbox(this.m.xhr.readyState, this.m.xhr.rslevel);})
  ,open: method(function(meth/*:Box<string>*/, url/*:Box<string>*/, async/*::?: Box<boolean>*/, username/*::?: Box<string>*/, password/*::?: Box<string>*/)/*:Box<void>*/{
    //XHR Spec 4.5.1
    //Throws a SyntaxError exception if either method is not a valid HTTP method or url cannot be parsed.
    //Throws a SecurityError exception if method is a case-insensitive match for `CONNECT`, `TRACE` or `TRACK`.
    //Upper bound on the leaked information
    var lev/*:Label*/ = meth.l.join(url.l);
    pushExceptionLabel(lev);
    //Throws an InvalidAccessError exception if async is false, the JavaScript global environment is a document environment, and either the timeout attribute is not zero, the withCredentials attribute is true, or the responseType attribute is not the empty string.
    if (ToBooleanBox(async).v) {pushExceptionLabel(async.l); lev = lev.join(async.l);}
    var smeth/*:string*/ = ToStringBox(meth).v;
    var surl/*:string*/ = ToStringBox(url).v;
    this.m.url = surl;
    var basync /*:?boolean*/ = async?ToBooleanBox(async).v:undefined;
    if (username) lev = lev.join(username.l);
    if (password) lev = lev.join(password.l);
    var suser /*:?string*/ = username?ToStringBox(username).v:undefined;
    var spassword /*:?string*/ = password?ToStringBox(password).v:undefined;
    try {
      this.m.xhr.open(smeth, surl, basync, suser, spassword);
      this.m.txlevel = lev;
      this.m.rslevel = lev;
    } catch (e) {
      //TODO convert exceptions to appropriate proxies: SyntaxError, SecurityError, InvalidAccessError
    }
    return primlow(void 0);
  })
  ,send: method(function(body/*::?:Box<any>*/)/*:void*/ {
    //XHR Spec 4.5.6
      //Throws an InvalidStateError exception if either state is not opened or the send() flag is set.
    pushExceptionLabel(this.m.rslevel);
    var sbody = body?ToStringBox(body).v:null;
    var lev = (body?body.l:lowlevel()).join(this.m.txlevel);
    policyCheck(lev, this.m.url);
    this.m.xhr.send(body);
  })
  ,status: attribute(function ()/*:Box<number>*/{
    return primbox(this.m.xhr.status, this.m.rslevel);
  })
  ,responseText: attribute(function ()/*:Box<string>*/{
    return primbox(this.m.xhr.responseText, inputLevel(this.m.url));
  })
  ,setRequestHeader: method(function (name/*:Box<any>*/, value/*:Box<any>*/)/*:void*/ {
    pushExceptionLabel(this.m.rslevel.join(name.l).join(value.l));
    this.m.txlevel = this.m.txlevel.join(name.l).join(value.l);
    this.m.xhr.setRequestHeader(ToStringBox(name).v, ToStringBox(value).v);
  })
};

var XMLHttpRequestLifter = new Lifter(initXMLHttpRequest, XMLHttpRequestProtoFields, {class: "XMLHttpRequestPrototype"}, XMLHttpRequestEventTargetLifter);

var XMLHttpRequestCtor = ConstructorProxy(function ()/*:Box<Object>*/ {
   return XMLHttpRequestLifter.initialize(new XMLHttpRequest(), this);
}, XMLHttpRequestLifter.proto);

/* Blob facade */

function initBlob (blob/*:Blob*/, facade/*:Box<Object>*/)/*: Box<Object>*/ {
  'use strict';
  facade.m.underlying = blob;
  facade.m.class = "Blob";
  return facade;
}

var BlobLifter = new Lifter(initBlob, {}, {class: "Blob"}, null);

var BlobConstructor = ConstructorProxy(function () {
   var underlying = new Blob();
   return BlobLifter.initialize(underlying, this);
}, BlobLifter.proto);

/* File facade*/

function initFile (f/*:File*/, facade/*:Box<Object>*/)/*:Box<Object>*/ {
  'use strict';
  if (f) {
    facade.m.underlying = f;
    facade.l = facade.l.join(inputLevel("file://" + f.name));
  } else facade.m.underlying = new File();
  facade.m.class = "File";
  return facade;
}

var FileLifter = new Lifter(initFile, {}, {class: "File"}, BlobLifter);

var FileConstructor = ConstructorProxy(function (){
   var underlying = new File();
   return FileLifter.initialize(underlying, this);
}, FileLifter.proto);

/* FileReader facade */

function initFileReader (reader/*:FileReader*/, facade/*:Box<Object>*/)/*: Box<Object>*/ {
  'use strict';
  facade.m.underlying = reader;
  facade.m.readyState = primlow(0);
  facade.m.class = "FileReader";
  facade.m.resultLabel = lowlevel();
  return facade;
}

var FileReaderProtoFields = {
  readAsText: ESSLField(FunctionProxy(function(blob, encoding) {
    'use strict';
    // throws "InvalidStateError" - need to find out how to do this one right
    // in this one, the processing of the read file is done with a callback (asynchronously)
    // not sure how this should be done for JEST
    var oblob = ToObjectBox(blob);
    if (oblob.m.class !== "Blob" && oblob.m.class !== "File" ) {
	pushExceptionLabel(blob.l);
	throwTypeError();
    }
    pushExceptionLabel(this.m.readyState.l);
    this.m.readyState.l = pclabel;
    var rv = this.m.underlying.readAsText(oblob.m.underlying, ToStringBox(encoding).v);
    this.m.readyState.v = this.m.underlying.readyState;
    this.m.resultLabel = blob.l.join(pclabel);
    return primbox(rv, this.m.resultLabel);
  })),
  readyState: attribute(function () {
    return this.m.readyState;
  }),
  result: attribute(function () {
    return primbox(this.m.underlying.result, this.m.resultLabel);
  })
};

var FileReaderLifter = new Lifter(initFileReader, FileReaderProtoFields, {class: "FileReader"}, EventTargetLifter);

var FileReaderConstructor = ConstructorProxy(function () {
   var underlying = new FileReader();
   return FileReaderLifter.initialize(underlying, this);
}, FileReaderLifter.proto);

/* Window facade */

function initWindow(w/*:Window*/, facade/*:Box<Object>*/)/*:Box<Object>*/ {
   facade.m.underlying = w;
   facade.m.class = "Window";
   addFields(facade, {window: constant(global)
                     ,document: constant(DocumentLifter.lift(document))
		     ,XMLHttpRequest: field(XMLHttpRequestCtor)
		     ,alert: method(function (b/*:Box<any>*/)/*:void*/ {
			alert(ToStringBox(b).v);
		     })
		     ,postMessage: method(function (message/*:Box<any>*/
						   ,targetOrigin/*:Box<any>*/
						   ) {
		     //TODO: support the optional transfer argument
		     var msg = join2(message, targetOrigin.l);
		     this.m.underlying.postMessage(msg, ToStringBox(targetOrigin).v);
		     })
		     ,File: ESSLField(FileConstructor)
		     ,Blob: ESSLField(BlobConstructor)
		     ,FileReader: ESSLField(FileReaderConstructor)
		     ,Node: ESSLField(NodeIFace)
		     ,Text: ESSLField(TextConstructor)
		     ,Element: ESSLField(ElementIFace)
		     ,HTMLElement: ESSLField(HTMLElementIFace)
		     });
   return facade;
}

var WindowLifter = new Lifter (initWindow, {}, {class: "Window"}, EventTargetLifter);

var eventTargetClassMatchers =
       [{native: XMLHttpRequest
        ,facade: XMLHttpRequestLifter}
       ,{native: FileReader
	,facade: FileReaderLifter   
	}
       ,{native: Window
        ,facade: WindowLifter}
       ];

//Takes an EventTarget instance and determines it's most specific interface ("class")
function liftEventTarget(et/*:EventTarget*/)/*:Box<Object>*/ {
   if ("nodeType" in et /*::&& et instanceof Node*/)
      //this is a Node
      return liftNode(et);
   else {
      for (var i = 0; i < eventTargetClassMatchers.length; i++) {
	 if (et instanceof eventTargetClassMatchers[i].native)
	    return eventTargetClassMatchers[i].facade.lift(et);
      }
   }
}


//Global needs to inherit from EventTarget and have the real window in m.underlying.
//Redo the creation of the global facade (it's fast) to implement it.
global = WindowLifter.lift(window);

addFields(global, globalESSLFields);
