"use strict";

exports.unsafeGet = function (m) {
  return function () {
    return m;
  };
};
