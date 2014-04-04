"use strict";

var test = require("../test.js")
  , Q    = require("q")

/*------------------------------------------------------------------*/
var TestNote = test.Test.extend({
    init: function () {
        this.description = "Weight tracking: note create/delete";
    },

    run: function() {
        var resp = test.post( {
            url: test.restUrl('/rest/note'),
            form: { date:"2014-04-04", text: "note text" }
        });

        // Insert a note for a given date, then query list of notes
        // for another day, and expect it to be empty.  Then query a
        // list of notes for the correct date and expect to get the
        // note back.  Then delete it.
        return resp.then(function (r) {
            test.assertStatusCodeOK(r);
            var json = JSON.parse(r.body);
            var noteId = json.id;
            test.assert(json.id);
            test.assert(json.text == "note text");
            return test.get( {
                url: test.restUrl("/rest/notes"),
                form: { date: "2014-04-05" }
            }).then(function (r) {
                var json = JSON.parse(r.body);
                test.assert(json.length == 0);
                return test.get( {
                    url: test.restUrl("/rest/notes"),
                    form: { date: "2014-04-04" }
                });;
            }).then(function (r) {
                var notes = JSON.parse(r.body);
                test.assert(notes.length == 1);
                test.assert(notes[0].id == noteId);
                return test.del( {
                    url: test.restUrl('/rest/note'),
                    form: { id: json.id }
                });
            });
        })
        .then(function (r) {
            test.assertStatusCodeOK(r);
        });
    }

});

/*------------------------------------------------------------------*/
var TestWeight = test.Test.extend({
    init: function () {
        this.description = "Weight tracking: set/clear weight";
    },

    run: function() {
        var resp = test.post( {
            url: test.restUrl('/rest/weight'),
            form: { date:"2014-04-04", weight: 80 }
        });

        return resp.then(function (r) {
            test.assertStatusCodeOK(r);
            var weight = JSON.parse(r.body);
            test.assert(weight.id);
            test.assert(weight.weight == 80);
            return test.del( {
                url: test.restUrl('/rest/weight'),
                form: { id: weight.id }
            });
        })
        .then(function (r) {
            test.assertStatusCodeOK(r);
        });
    }

});

/*------------------------------------------------------------------*/
module.exports = [new TestNote, new TestWeight];
