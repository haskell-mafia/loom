"use strict";

var browserify = require('browserify');
var envify = require('envify/custom');
var uglifyify = require('uglifyify');
var util = require('util');
var path = require('path');

// Helpers
function bail(msg) {
  console.log(msg);
  process.exit(2);
}
function bailWithUsage() {
  console.log(
    "Usage: node browserify.js path/to/app entryPoint1.js [entryPoint2.js ...]"
    + "\nWill print the resulting JavaScript bundle to STDOUT."
  );
  process.exit(1);
}



// Script

// ["/usr/bin/node", "browserify.js", pathToApp, arg1, arg2, ...]
var appPath = process.argv[2];
if (appPath === undefined) {
  bailWithUsage();
}

var inputFiles = process.argv.slice(3);
if (inputFiles.length < 1) {
  bailWithUsage();
}

var isProduction = true; // TODO

browserify({
  debug: !(isProduction), // Toggles sourcemaps
  detectGlobals: false,
  paths: [
    path.join(appPath, 'node_modules'),
  ],
  entries: inputFiles,
})
  .transform(
    envify({
      "_": "purge",
      NODE_ENV: (isProduction ? "production" : "development")
    }),
    { global: true }
  )
  .transform(
    uglifyify,
    { global: true }
  )
  .bundle()
  .pipe(process.stdout)
// TODO: A bunch of .require(pathToJSFile, { expose: "pathToExposeAs" }),
//       per entry. See gulpfile.

// Synchronous part done; now waiting for browserify to asynchronously finish.
