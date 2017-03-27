"use strict";

exports["new"] = function () {
  return {};
};

exports.peekImpl = function (just) {
  return function (nothing) {
    return function (m) {
      return function (k) {
        return function () {
          return {}.hasOwnProperty.call(m, k) ? just(m[k]) : nothing;
        };
      };
    };
  };
};

exports.poke = function (m) {
  return function (k) {
    return function (v) {
      return function () {
        m[k] = v;
        return m;
      };
    };
  };
};

exports["delete"] = function (m) {
  return function (k) {
    return function () {
      delete m[k];
      return m;
    };
  };
};
