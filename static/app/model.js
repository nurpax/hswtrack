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

    function loadExerciseTypes() {
        return $.ajax({
            type: "GET",
            url: "/rest/exercise"
        });
    }

    function loadWorkouts() {
        return $.ajax({
            type: "GET",
            url: "/rest/workout"
        });
    }

    function loadWorkout(id) {
        return $.ajax({
            type: "GET",
            url: "/rest/workout",
            data: { id: id }
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


    /*--------------------------------------------------------------*/
    // Exercise tracking
    /*--------------------------------------------------------------*/

    function sortExerciseTypes(es) {
        return _.sortBy(es, function (e) { return e.name.toLowerCase(); });
    }

    /*--------------------------------------------------------------*/
    var ExerciseTypes = ModelBase.extend({
        init: function(es) {
            this.exerciseTypes = es;
        },

        load: function() {
            var self = this;
            $.when(loadExerciseTypes()).done(function (es) {
                self.exerciseTypes = sortExerciseTypes(es.payload);
                self.update();
            });
        },

        getExerciseById: function (id) {
            return  _.find(this.exerciseTypes, function (x) { return x.id == id; });
        },

        addExercise: function (data) {
            var self = this;
            $.ajax( { url: "/rest/exercise",
                      type: "POST",
                      data: data,
                      success: function (resp) {
                          self.exerciseTypes.push(resp.payload);
                          self.exerciseTypes = sortExerciseTypes(self.exerciseTypes);
                          self.update();
                      }
                    });
        }
    });

    /*--------------------------------------------------------------*/
    var Exercise = ModelBase.extend({

        init: function(e) {
            this.id       = e.id;
            this.name     = e.name;
            this.type     = e.type;
            this.sets     = e.sets ? e.sets : [];
        },

        addSetPrivate: function (params, cb) {
            var self = this;
            $.ajax({ url: "/rest/workout/exercise",
                     type: "POST",
                     data: params,
                     success: function (resp) {
                         self.sets.push(resp.payload);
                         cb();
                     }
                   });
        },

        addSet: function (params) {
            var self = this;
            this.addSetPrivate(params, function () { self.update(); });
        },

        deleteSet: function (params) {
            var self = this;
            $.ajax({ url: "/rest/workout/exercise",
                     type: "DELETE",
                     data: params,
                     success: function () {
                         self.sets = _.filter(self.sets, function (s) { return s.id != params.id; });
                         self.update();
                     }
                   });
        }
    });



    /*--------------------------------------------------------------*/
    // Single workout, contains what exercises were done
    var Workout = ModelBase.extend({

        init: function(w) {
            this.id        = w.id;
            this.exercises = _.map(w.exercises, function (e) { return new Exercise(e); });
        },

        update: function () {
            this._super();
            _.each(this.exercises, function (e) { e.update(); });
        },

        getExercises: function () { return this.exercises; },

        addExerciseSet: function (exercise, data) {
            var self = this;
            var e    = _.find(this.getExercises(), function (x) { return x.id == exercise.id; });

            if (!e) {
                e = new Exercise(exercise);
                e.addSetPrivate(data,
                                function () {
                                    self.exercises.push(e);
                                    self.update();
                                });
            } else {
                e.addSetPrivate(data,
                                function () {
                                    self.update();
                                });
            }
        }
    });


    /*--------------------------------------------------------------*/
    // Top workout container, lists day's workout sessions
    var WorkoutCont = ModelBase.extend({
        init: function () {
            this.workouts      = [];
            this.exerciseTypes = [];
            this.readOnly      = false;
        },

        update: function () {
            this._super();
            _.each(this.workouts, function (x) { x.update(); });
        },

        setWorkouts: function (ws) {
            this.workouts = _.map(ws, function (w) { return new Workout(w); });
        },

        load: function (id) {
            var self = this;

            // If no 'id' specified, load today's workouts
            if (!id) {
                $.when(loadWorkouts(), loadExerciseTypes()).done(function (ws, es) {
                    self.readOnly = !ws[0].loggedIn;
                    self.setWorkouts(ws[0].payload);
                    self.exerciseTypes = new ExerciseTypes(es[0].payload);
                    self.update();
                });
            } else {
                $.when(loadWorkout(id), loadExerciseTypes()).done(function (w, es) {
                    self.readOnly = !w[0].loggedIn;
                    self.setWorkouts([w[0].payload]);
                    self.exerciseTypes = new ExerciseTypes(es[0].payload);
                    self.update();
                });
            }
        },

        newWorkout: function () {
            var self = this;
            $.ajax( { url: "/rest/workout",
                      type: "POST",
                      data: [],
                      success: function (resp) {
                          var w = new Workout(resp.payload);
                          self.workouts.push(w);
                          self.update();
                      }
                    });
        },

        getWorkouts: function () { return this.workouts; },

        getExerciseTypes: function () { return this.exerciseTypes.exerciseTypes; },

        getExerciseById: function (id) {
            return this.exerciseTypes.getExerciseById(id);
        }

    });

    // export
    return {
        'WeightCont': WeightCont,
        'WorkoutCont': WorkoutCont,
        'ExerciseTypes': ExerciseTypes,
    };
});
