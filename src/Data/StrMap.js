"use strict";

exports._copyEff = function (m) {
  return function () {
    var r = {};
    for (var k in m) {
      if (hasOwnProperty.call(m, k)) {
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

exports._fmapStrMap = function (m0, f) {
  var m = {};
  for (var k in m0) {
    if (hasOwnProperty.call(m0, k)) {
      m[k] = f(m0[k]);
    }
  }
  return m;
};

exports._mapWithKey = function (m0, f) {
  var m = {};
  for (var k in m0) {
    if (hasOwnProperty.call(m0, k)) {
      m[k] = f(k)(m0[k]);
    }
  }
  return m;
};

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
          if (hasOwnProperty.call(m, k)) {
            acc = bind(acc)(g(k));
          }
        }
        return acc;
      };
    };
  };
};

exports._foldSCStrMap = function (m, z, f, fromMaybe) {
  var acc = z;
  for (var k in m) {
    if (hasOwnProperty.call(m, k)) {
      var maybeR = f(acc)(k)(m[k]);
      var r = fromMaybe(null)(maybeR);
      if (r === null) return acc;
      else acc = r;
    }
  }
  return acc;
};

exports.all = function (f) {
  return function (m) {
    for (var k in m) {
      if (hasOwnProperty.call(m, k) && !f(k)(m[k])) return false;
    }
    return true;
  };
};

exports.size = function (m) {
  var s = 0;
  for (var k in m) {
    if (hasOwnProperty.call(m, k)) {
      ++s;
    }
  }
  return s;
};

exports._lookup = function (no, yes, k, m) {
  return k in m ? yes(m[k]) : no;
};

exports._unsafeDeleteStrMap = function (m, k) {
  delete m[k];
  return m;
};

exports._lookupST = function (no, yes, k, m) {
  return function () {
    return k in m ? yes(m[k]) : no;
  };
};

function toArrayWithKey(f) {
  return function (m) {
    var r = [];
    for (var k in m) {
      if (hasOwnProperty.call(m, k)) {
        r.push(f(k)(m[k]));
      }
    }
    return r;
  };
}

exports.toArrayWithKey = toArrayWithKey;

exports.keys = Object.keys || toArrayWithKey(function (k) {
  return function () { return k; };
});
