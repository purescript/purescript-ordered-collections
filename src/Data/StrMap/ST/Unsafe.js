/* global exports */
"use strict";

// module Data.StrMap.ST.Unsafe

exports.unsafeGet = function (m) {
  return function () {
    return m;
  };
};
