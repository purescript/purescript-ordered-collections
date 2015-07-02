/* global exports */
"use strict";

// module Data.StrMap.Unsafe

exports.unsafeIndex = function (m) {
  return function (k) {
    return m[k];
  };
};
