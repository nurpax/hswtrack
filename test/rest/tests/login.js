"use strict";

var test = require("../test.js")
  , Q    = require("q")

/*------------------------------------------------------------------*/
var TestSimple = test.Test.extend({
    init: function () {
        this.description = "Simplest app context test";
    },

    run: function() {
        var resp = test.get({url: test.restUrl('/rest/app')});

        return resp.then(function (r) {
            test.assertStatusCodeOK(r);
            var json = JSON.parse(r.body);
            test.assert('loginError' in json, "Shouldn't be logged out in server test mode");
            return r;
        });
    }

});

/*------------------------------------------------------------------*/
module.exports = new TestSimple;
