define(['jquery', 'underscore'], function($, _) {
    function loadExerciseTypes() {
        return $.ajax({
            type: "GET",
            url: "/rest/exercise"
        });
    };

    function loadWorkouts() {
        return $.ajax({
            type: "GET",
            url: "/rest/workout"
        });
    };

    /*--------------------------------------------------------------*/
    function ExerciseTypes(es) {
        this.exerciseTypes = es;
        this.onUpdate      = null;
    };

    ExerciseTypes.prototype.update = function () {
        if (this.onUpdate) {
            this.onUpdate(this);
        }
    };

    ExerciseTypes.prototype.setUpdateHandler = function (cb) { this.onUpdate = cb; };

    ExerciseTypes.prototype.load = function () {
        var self = this;
        $.when(loadExerciseTypes()).done(function (es) {
            self.exerciseTypes = es;
            self.update();
        });
    };

    ExerciseTypes.prototype.getExerciseById = function (id) {
        return  _.find(this.exerciseTypes, function (x) { return x.id == id; });
    };

    ExerciseTypes.prototype.addExercise = function (data) {
        var self = this;
        $.ajax( { url: "/rest/exercise",
                  type: "POST",
                  data: data,
                  success: function (resp) {
                      self.exerciseTypes = resp;
                      self.update();
                  }
                });
    };

    /*--------------------------------------------------------------*/
    function Exercise(e) {
        this.id       = e.id;
        this.name     = e.name;
        this.type     = e.type;
        this.sets     = e.sets;
        this.onUpdate = null;
    };

    Exercise.prototype.addSetPrivate = function (params, cb) {
        var self = this;
        $.ajax({ url: "/rest/workout/exercise",
                 type: "POST",
                 data: params,
                 success: function (resp) {
                     self.sets = resp.sets;
                     cb();
                 }
               });
    };

    Exercise.prototype.addSet = function (params) {
        var self = this;
        this.addSetPrivate(params, function () { self.update() });
    };

    Exercise.prototype.deleteSet = function (params) {
        var self = this;
        $.ajax({ url: "/rest/workout/exercise",
                 type: "DELETE",
                 data: params,
                 success: function (resp) {
                     self.sets = resp.sets;
                     self.update();
                 }
               });
    };

    Exercise.prototype.update = function () {
        if (this.onUpdate) {
            this.onUpdate(this);
        }
    };

    Exercise.prototype.setUpdateHandler = function (cb) { this.onUpdate = cb; };

    /*--------------------------------------------------------------*/
    // Single workout, contains what exercises were done
    function Workout(w) {
        this.id        = w.id;
        this.exercises = _.map(w.exercises, function (e) { return new Exercise(e); });
        this.onUpdate  = null;
    };

    Workout.prototype.update = function () {
        if (this.onUpdate) {
            this.onUpdate(this);
        }
        _.each(this.exercises, function (e) { e.update(); });
    };

    Workout.prototype.setUpdateHandler = function (cb) { this.onUpdate = cb; };
    Workout.prototype.getExercises = function () { return this.exercises; }

    Workout.prototype.addExerciseSet = function (exercise, data) {
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
    };

    /*--------------------------------------------------------------*/
    // Top workout container, lists day's workout sessions
    function WorkoutCont() {
        this.workouts      = [];
        this.exerciseTypes = [];
        this.onUpdate      = null;
    };

    WorkoutCont.prototype.setUpdateHandler = function (cb) { this.onUpdate = cb; };

    WorkoutCont.prototype.update = function () {
        if (this.onUpdate) {
            this.onUpdate(this);
        }
        _.each(this.workouts, function (x) { x.update(); });
    };

    WorkoutCont.prototype.setWorkouts = function (ws) {
        this.workouts = _.map(ws, function (w) { return new Workout(w); });
    };

    WorkoutCont.prototype.load = function () {
        var self = this;
        $.when(loadWorkouts(), loadExerciseTypes()).done(function (ws, es) {
            self.setWorkouts(ws[0]);
            self.exerciseTypes = new ExerciseTypes(es[0]);
            self.update();
        });
    };

    WorkoutCont.prototype.newWorkout = function () {
        var self = this;
        $.ajax( { url: "/rest/workout",
                  type: "POST",
                  data: [],
                  success: function (resp) {
                      self.setWorkouts(resp);
                      self.update();
                  }
                });
    };


    WorkoutCont.prototype.getWorkouts = function () { return this.workouts; };

    WorkoutCont.prototype.getExerciseTypes = function () { return this.exerciseTypes.exerciseTypes; }

    WorkoutCont.prototype.getExerciseById = function (id) {
        return this.exerciseTypes.getExerciseById(id);
    }

    // export
    return {
        'WorkoutCont': WorkoutCont,
        'ExerciseTypes': ExerciseTypes,
    };
});
