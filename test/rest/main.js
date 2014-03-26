"use strict";

var Q         = require('q')
  , _         = require('underscore')
  , tests     = require('./tests/list')


// Test lists can contain sub-test lists, so flatten into a single
// list of tests.
var testList  = _.flatten(tests.list());

var funcs = _.map(testList, function (test, testIdx) {
    return function () {
        console.log("Running test "+testIdx+": "+test.description);
        return test.run()
    };
});

return funcs.reduce(Q.when, Q()).done();
