"use strict";

var Q         = require('q')
  , _         = require('underscore')
  , tests     = require('./tests/list')

var testList  = tests.list();
var testCount = 0;
_.each(testList, function (t) {
    var descr = t.description();
    console.log("Running test "+testCount+": "+descr);
    t.run();
    testCount++;
});
