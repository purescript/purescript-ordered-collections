/* jshint node: true */
"use strict";

var gulp = require("gulp");
var plumber = require("gulp-plumber");
var purescript = require("gulp-purescript");
var rimraf = require("rimraf");

var sources = [
  "src/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
  "src/**/*.js",
  "bower_components/purescript-*/src/**/*.js"
];

gulp.task("clean-docs", function (cb) {
  rimraf("docs", cb);
});

gulp.task("clean-output", function (cb) {
  rimraf("output", cb);
});

gulp.task("clean", ["clean-docs", "clean-output"]);

gulp.task("make", function() {
  return gulp.src(sources)
    .pipe(plumber())
    .pipe(purescript.pscMake({ ffi: foreigns }));
});

gulp.task("docs", ["clean-docs"], function () {
  return gulp.src(sources)
    .pipe(plumber())
    .pipe(purescript.pscDocs({
      docgen: {
        "Data.Map"                 : "docs/Data/Map.md",
        "Data.StrMap"              : "docs/Data/StrMap.md",
        "Data.StrMap.ST"           : "docs/Data/StrMap/ST.md",
        "Data.StrMap.ST.Unsafe"    : "docs/Data/StrMap/ST/Unsafe.md",
        "Data.StrMap.Unsafe"       : "docs/Data/StrMap/Unsafe.md"
      }
    }));
});

gulp.task("dotpsci", function () {
  return gulp.src(sources)
    .pipe(plumber())
    .pipe(purescript.dotPsci());
});

gulp.task("default", ["make", "docs", "dotpsci"]);