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


// A constructor for a set-of-strings-based representation of security
// levels. More flexible, less efficient than bit-vectors.

function LevelCtor () {
  this.level = {};
}

LevelCtor.bottom = function () {
  return new LevelCtor();
};

LevelCtor.prototype.join = function(l2) {
  var result = new LevelCtor ();
  for (var elem in this.level)
    result.level[elem] = true;
  for (elem in l2.level)
    result.level[elem] = true;
  return result;
};

LevelCtor.prototype.leq = function(l2) {
  for (var elem in this.level)
    if (!(elem in l2.level)) return false;
  return true;
};

LevelCtor.prototype.toString = function () {
  var s = "Level(";
  var first = true;
  for (var p in this.level) {
    s += (first?"":",") + p;
    first = false;
  }
  return (s + ")");
};
