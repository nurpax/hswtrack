define(['jquery', 'underscore', 'app/class'], function($, _, obj) {
    "use strict";

    var Class = obj.Class;

    // TODO these should be in a model class
    function loadAppContext() {
        return $.ajax({
            type: "GET",
            url: "/rest/app",
            data: []
        });
    }

    function loadWeights(ndays) {
        return $.ajax({
            type: "GET",
            url: "/rest/weights",
            data: { days: ndays }
        });
    }

    function loadNotes() {
        return $.ajax({
            type: "GET",
            url: "/rest/notes",
            data: []
        });
    }

    /*--------------------------------------------------------------*/
    var ModelBase = Class.extend({
        setUpdateHandler: function (cb) {
            this.onUpdate = cb;
        },

        update: function () {
            if (this.onUpdate)
                this.onUpdate(this);
        }
    });

    /*--------------------------------------------------------------*/
    // Weight tracking
    /*--------------------------------------------------------------*/

    var Weights = ModelBase.extend({
        init: function(weights) {
            this.weights = weights;
        },

        addWeight: function (r) {
            this.weights.push(r);
        },

        deleteWeight: function (r) {
            this.weights = _.filter(this.weights, function (w) { return w.id != r.id; });
        },

        load: function (days) {
            var self = this;
            $.when(loadWeights(days)).done(function (w) {
                self.weights = w.payload;
                self.update();
            });
        }
    });

    var Notes = ModelBase.extend({
        init: function(n) {
            this.notes = n;
        },

        deleteById: function (id_) {
            var self = this;
            $.ajax({ url: "/rest/note",
                     type: "DELETE",
                     data: { id: id_ },
                     success: function () {
                         self.notes = _.filter(self.notes, function (n) { return n.id != id_; });
                         self.update();
                     }
                   });
        },

        addNote: function (text) {
            var self = this;
            $.ajax({ url: "/rest/note",
                     type: "POST",
                     data: { text: text },
                     success: function (resp) {
                         self.notes.push(resp.payload);
                         self.update();
                     }
                   });
        }

    });

    var WeightCont = ModelBase.extend({
        init: function() {
        },

        load: function(selectedGraphDays) {
            var self = this;

            $.when(loadAppContext(), loadWeights(selectedGraphDays), loadNotes()).done(function (a, w, n) {
                self.app     = a[0].payload;
                self.weights = new Weights(w[0].payload);
                self.notes   = new Notes(n[0].payload);
                self.update();
            });
        },

        setWeight: function (newWeight) {
            var self = this;
            $.ajax({
                type: "POST",
                url: "/rest/weight",
                data: { weight: newWeight },
                success: function (r) {
                    var weight = r.payload;
                    self.app.context.weight = weight;
                    self.weights.addWeight(weight);
                    self.update();
                }
            });
        },

        clearWeight: function () {
            var self = this;
            if (self.app.context.weight) {
                $.ajax({
                    type: "DELETE",
                    url: "/rest/weight",
                    data: { id: self.app.context.weight.id },
                    success: function () {
                        self.weights.deleteWeight(self.app.context.weight);
                        self.app.context.weight = null;
                        self.update();
                    }
                });
            }
        },

        update: function () {
            this._super();
            this.weights.update();
            this.notes.update();
        }
    });
    // export
    return {
        'WeightCont':    WeightCont
    };
});
