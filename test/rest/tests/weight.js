"use strict";

var test = require("../test.js")
  , Q    = require("q")

/*------------------------------------------------------------------*/
var TestWeight = test.Test.extend({
    init: function () {
        this.description = "Weight tracking: note create/delete";
    },

    run: function() {
        var resp = test.post( {
            url: test.restUrl('/rest/note'),
            form: { text: "note text" }
        });

        return resp.then(function (r) {
            test.assertStatusCodeOK(r);
            var json = JSON.parse(r.body);
            var noteId = json.id;
            test.assert(json.id);
            test.assert(json.text == "note text");
            return test.del( {
                url: test.restUrl('/rest/note'),
                form: { id: json.id }
            });
        })
        .then(function (r) {
            test.assertStatusCodeOK(r);
        });
    }

});

/*------------------------------------------------------------------*/
module.exports = [new TestWeight];
