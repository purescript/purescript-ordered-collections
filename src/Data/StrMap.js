/* global exports */
"use strict";

// module Data.StrMap

exports._copy = function (m) {
  var r = {};
  for (var k in m) {
    if (m.hasOwnProperty(k)) {
      r[k] = m[k];
    }
  }
  return r;
};

exports._copyEff = function (m) {
  return function () {
    var r = {};
    for (var k in m) {
      if (m.hasOwnProperty(k)) {
        r[k] = m[k];
      }
    }
    return r;
  };
};

exports.empty = {};

exports.runST = function (f) {
  return f;
};

// jshint maxparams: 2
exports._fmapStrMap = function (m0, f) {
  var m = {};
  for (var k in m0) {
    if (m0.hasOwnProperty(k)) {
      m[k] = f(m0[k]);
    }
  }
  return m;
};

// jshint maxparams: 1
exports._foldM = function (bind) {
  return function (f) {
    return function (mz) {
      return function (m) {
        var acc = mz;
        function g(k) {
          return function (z) {
            return f(z)(k)(m[k]);
          };
        }
        for (var k in m) {
          if (m.hasOwnProperty(k)) {
            acc = bind(acc)(g(k));
          }
        }
        return acc;
      };
    };
  };
};

// jshint maxparams: 4
exports._foldSCStrMap = function (m, z, f, fromMaybe) {
  for (var k in m) {
    if (m.hasOwnProperty(k)) {
      var maybeR = f(z)(k)(m[k]);
      var r = fromMaybe(null)(maybeR);
      if (r === null) return z;
      else z = r;
    }
  }
  return z;
};

// jshint maxparams: 1
exports.all = function (f) {
  return function (m) {
    for (var k in m) {
      if (m.hasOwnProperty(k) && !f(k)(m[k])) return false;
    }
    return true;
  };
};

exports.size = function (m) {
  var s = 0;
  for (var k in m) {
    if (m.hasOwnProperty(k)) {
      ++s;
    }
  }
  return s;
};

// jshint maxparams: 4
exports._lookup = function (no, yes, k, m) {
  return k in m ? yes(m[k]) : no;
};

// jshint maxparams: 2
exports._unsafeDeleteStrMap = function (m, k) {
  delete m[k];
  return m;
};

// jshint maxparams: 4
exports._lookupST = function (no, yes, k, m) {
  return function () {
    return k in m ? yes(m[k]) : no;
  };
};

function _collect(f) {
  return function (m) {
    var r = [];
    for (var k in m) {
      if (m.hasOwnProperty(k)) {
        r.push(f(k)(m[k]));
      }
    }
    return r;
  };
}

exports._collect = _collect;

exports.keys = Object.keys || _collect(function (k) {
  return function () { return k; };
});
