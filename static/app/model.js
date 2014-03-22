define(['jquery', 'underscore', 'app/class'], function($, _, obj) {
    "use strict";

    var Class = obj.Class;

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
    var ExerciseTypes = ModelBase.extend({
        init: function(es) {
            this.exerciseTypes = es;
        },

        load: function() {
            var self = this;
            $.when(loadExerciseTypes()).done(function (es) {
                self.exerciseTypes = es;
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
                          self.exerciseTypes.push(resp);
                          self.exerciseTypes = _.sortBy(self.exerciseTypes,
                                                        function (e) { return e.name.toLowerCase(); });
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
                         self.sets.push(resp);
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
        },

        update: function () {
            this._super();
            _.each(this.workouts, function (x) { x.update(); });
        },

        setWorkouts: function (ws) {
            this.workouts = _.map(ws, function (w) { return new Workout(w); });
        },

        load: function () {
            var self = this;
            $.when(loadWorkouts(), loadExerciseTypes()).done(function (ws, es) {
                self.setWorkouts(ws[0]);
                self.exerciseTypes = new ExerciseTypes(es[0]);
                self.update();
            });
        },

        newWorkout: function () {
            var self = this;
            $.ajax( { url: "/rest/workout",
                      type: "POST",
                      data: [],
                      success: function (resp) {
                          var w = new Workout(resp);
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
        'WorkoutCont': WorkoutCont,
        'ExerciseTypes': ExerciseTypes,
    };
});
