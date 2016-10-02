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


// A constructor for a bit-vector based representation of security
// levels.  More efficient, less flexible. A level is represented by
// an integer internally, which in turn is interpreted as a
// bit-vector. Join is just bit-wise 'or'. Lattice inequality (leq) is
// just bit-wise implication.

function LevelCtor () {
  this.level = 0;
}
LevelCtor.bottom = function () {return new LevelCtor();};
LevelCtor.prototype.join = function (l2) {
  var rv = new LevelCtor();
  rv.level = this.level | l2.level;
  return rv;
};
LevelCtor.prototype.leq = function (l2) {
  return ((~this.level | l2.level) === (0xFFFFFFFF & 0xFFFFFFFF));
};
LevelCtor.prototype.toString = function () {return "Level("+this.level+")";};
