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


var f = monitorfunlow(function (y) {
   monitorassignVar(x, monitorprimlow(2));}), x = monitorinitVar();
monitorassignVar(x, monitorprimlow(1));
monitorassignVar(function () {
   try {
      return z;
   } catch (x) {
      return monitorglobalProxy.z = monitorinitVar();
   } 
}(), monitorprimlow(2));
if (monitorToBooleanBox(monitorpush(153, z)).v)
monitorinvokeFunction(f, [monitorprimlow(1)]);
monitorassignVar(x, monitorprimlow(3));
