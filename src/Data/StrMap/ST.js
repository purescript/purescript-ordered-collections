/* global exports */
"use strict";

// module Data.StrMap.ST

exports._new = function() {
  return {};
};

exports.peek = function(m) {
  return function(k) {
    return function() {
      return m[k];
    }
  }
};

exports.poke = function(m) {
  return function(k) {
    return function(v) {
      return function() {
        m[k] = v;
        return m;
      };
    };
  };
};

exports._delete = function(m) {
  return function(k) {
    return function() {
      delete m[k];
      return m;
    };
  };
};