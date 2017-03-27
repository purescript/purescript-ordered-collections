"use strict";

exports.unsafeFreeze = function (m) {
  return function () {
    return m;
  };
};
