"use strict";

exports.unsafeIndex = function (m) {
  return function (k) {
    return m[k];
  };
};
