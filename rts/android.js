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

var androidObjectFacade = ObjectProxy(ObjectProto, {class: "Android"}, false);

var androidFields /*:FieldMap*/ = {};

if ("Android" in window && Android) {
    for (var f in Android) {
	androidFields[f] = (function (fname) {
	    return ESSLField(FunctionProxy(function () {
		var unboxedArgs = [];
		var fnchannel = "func://" + fname;
		for (var i = 0; i < arguments.length; i++) {
		    var cl = policy.locationLevel(fnchannel + "@" + i.toString(), true);
		    var dl = arguments[i].l.join(pclabel);
		    if (dl.leq(cl))
			unboxedArgs.push(ToStringBox(arguments[i]).v);
		    else stop("Android", 0, 0, "Policy violation");
		}
		
		var facaderet = Android[fname].apply(Android, unboxedArgs);
		if (typeof facaderet === 'string') {
		    var jsonret = JSON.parse(facaderet);
		    var retl = lowlevel();
		    for (var i = 0; i < jsonret.level.length; i ++) {
			retl = retl.join(policy.locationLevel(jsonret.level[i]));
		    }
		return primbox(jsonret.value, retl);
		} else return initVar();
	    }))})(f);
    }
}

addFields(androidObjectFacade, androidFields);
addFields(global, {Android: ESSLField(androidObjectFacade)});
