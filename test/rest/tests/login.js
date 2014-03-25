"use strict";

var test     = require("../test.js")
  , Q        = require("q")

/*------------------------------------------------------------------*/
var TestSimple = test.Test.extend({
    init: function () {
    },

    description: function () {
        return "Simplest app context test";
    },

    run: function() {
        var resp = test.get({url: test.restUrl('/rest/app')});

        Q.when(resp, function (r) {
            test.assertStatusCodeOK(r);
            var json = JSON.parse(r.body);
            test.assert(!json.loginError, "Shouldn't be logged out in server test mode");
            return r;
        }).done();
    }

});

/*------------------------------------------------------------------*/
module.exports = {
    test: new TestSimple()
};
