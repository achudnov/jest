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


var e1 = monitorinitVar(), e2 = monitorinitVar(), e3 = monitorinitVar();
monitorassignVar(e1, primlow(0));
monitorassignVar(e2, primlow(0));
monitorassignVar(e3, primlow(0));
monitorenter(3);
l1: while (monitorToBooleanBox(monitorupdate(0, e1)).v) {
    ;
    while (monitorToBooleanBox(monitorupdate(1, e2)).v) {
	if (monitorToBooleanBox(monitorupdate(2, e3)).v) break;
	else break l1;
    }
}
monitorexit(1);
