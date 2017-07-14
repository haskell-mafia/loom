"use strict";

var browserify = require('browserify');
var envify = require('envify/custom');
var uglifyify = require('uglifyify');
var util = require('util');

// Helpers
function bailWithUsage() {
  console.log([
    "Usage: echo <JSON> | node browserify.js",
    "",
    "Will print the resulting JavaScript bundle to STDOUT.",
    "Expected JSON:",
    '  { isProduction: true',
    '  , paths: ["path/to/app/node_modules", ...]',
    '  , entries:',
    '      { "path/to/app/file": "exposed/as/this",',
    '      , "path/to/file/not/exposed": null',
    '      , ...',
    '      }',
    '  }',
  ].join("\n"));
  process.exit(1);
}

function getStdinThen(callback) {
  var result = "";
  if (process.stdin.isTTY) {
    callback(result);
    return;
  }
  process.stdin.setEncoding('utf8');
  process.stdin.on('readable', () => {
    var chunk;
    while (chunk = process.stdin.read()) {
      result += chunk;
    }
  });
  process.stdin.on('end', function(){
    callback(result);
  });
}

function oMap(f, obj) {
  var res = [];
  for (var k in obj) {
    if (obj.hasOwnProperty(k)) {
      res.push(f(k, obj[k]));
    }
  }
  return res;
}

function get(obj, k, validate) {
  if (obj.hasOwnProperty(k)) {
    var val = obj[k];
    if (validate(val) === true) {
      return obj[k];
    } else {
      throw new Error("Value in key '"+k+"' not valid: "+util.inspect(obj));
    }
  } else {
    throw new Error("Key '"+k+"' not found in: "+util.inspect(obj));
  }
}


// Script

getStdinThen(function(stdin){
  var input = JSON.parse(stdin);
  if (input == null || Array.isArray(input) || typeof input !== 'object') {
    return bailWithUsage();
  }

  var paths = get(input, "paths", Array.isArray);
  var entries = get(input, "entries", function(x){ return typeof x == 'object' });
  var isProduction = get(input, "isProduction", function(x){ return typeof x == 'boolean' });

  var bundler =
    browserify({
      debug: !(isProduction), // Toggles sourcemaps
      detectGlobals: true,
      paths: paths,
      entries: oMap(function(key, val) { return key; }, entries),
    })

  oMap(function(key, val) { return [key, val]; }, entries).forEach(function(pair){
    var file = pair[0]; // eg. /foo/bar/qux.js
    var exposedAs = pair[1]; // eg. bar/qux
    bundler = bundler.require(file, { expose: exposedAs }); // to allow require('bar/qux')
  });

  bundler =
    bundler.transform(
      envify({
        "_": "purge",
        NODE_ENV: (isProduction ? "production" : "development")
      }),
      { global: true }
    )

  if (isProduction) {
    bundler =
      bundler.transform(
        uglifyify,
        { global: true }
      )
  }

  bundler
    .bundle()
    .pipe(process.stdout)

  // Done here; process will for browserify to asynchronously finish.
});


// Example invocation:
// echo '{"isProduction":true, "paths":["../../blah/node_modules"], "entries":{"../../blah/entry1.js":"blah/entry1"}}' | node browserify.js
