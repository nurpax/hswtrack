
// Page for adding new exercise types
function ExerciseTypes() {
    this.exercises = [];
    this.mainTemplate = Handlebars.compile($("#new-exercise-template").html());
};

ExerciseTypes.prototype.setExerciseList = function (lst) {
    this.exercises = lst;
};

ExerciseTypes.prototype.list = function (lst) {
    return this.exercises;
};

ExerciseTypes.prototype.getExerciseById = function (id) {
    for (var i = 0; i < this.exercises.length; i++) {
        if (this.exercises[i].id == id)
            return this.exercises[i];
    }
    return null;
};

ExerciseTypes.prototype._loadExerciseTypes = function () {
    return $.ajax({
        type: "GET",
        url: "/rest/exercise"
    });
};

ExerciseTypes.prototype._renderExerciseList = function () {
    var self = this;
    $("#app-container").html(self.mainTemplate({ exercises: this.list() }));

    $("form#new-exercise").each(function () {
        var form = this;
        $("button#new-workout", form).click(function (e) {
            e.preventDefault();
            var exerciseName = $("input#exercise-name", form).val();
            var type         = $("input[name=exercise-type-radios]:checked", form).val();

            $.ajax( { url: "/rest/exercise",
                      type: "POST",
                      data: { name: exerciseName, type:type },
                      success: function (resp) {
                          self.setExerciseList(resp);
                          self._renderExerciseList();
                      }
                    });
        });
    });
};

ExerciseTypes.prototype.render = function () {
    var self = this;

    $.when(this._loadExerciseTypes()).done(function (es) {
        self.setExerciseList(es);
        self._renderExerciseList(es);
    });
};

function Workout(exerciseTypes) {
    this.exerciseTypes = exerciseTypes;
    this.mainTemplate = Handlebars.compile($("#workouts-template").html());
    this.workoutTemplate = Handlebars.compile($("#workout-template").html());
    this.exerciseTemplate = Handlebars.compile($("#exercise-template").html());
    this.addExerciseTemplate = Handlebars.compile($("#add-exercise-template").html());
    this.addExerciseSetsTemplate = Handlebars.compile($("#add-exercise-sets-template").html());
    Handlebars.registerPartial("addSetControl", $("#add-set-control-partial").html());

    Handlebars.registerHelper('ifBodyweight', function(v, options) {
        if(v.type === "BW") {
            return options.fn(this);
        }
        return options.inverse(this);
    });
};


var assert = function(condition, message) {
    if (!condition)
        throw Error("Assert failed" + (typeof message !== "undefined" ? ": " + message : ""));
};

Workout.prototype._loadWorkouts = function () {
    return $.ajax({
        type: "GET",
        url: "/rest/workout"
    });
};

Workout.prototype._attachAddExercise = function (elt, exercise, renderCallback, workoutId) {
    var self = this;

    $("form.add-set", elt).each(function () {
        var form = this;

        $('a#collapse-weighted', form).click(function(e) {
            e.preventDefault();
            var c = $(this).closest('.collapse-group').find('.collapse');
            c.collapse('toggle');
            $(this).hide();
        });


        $("button", this).click(function (e) {
            e.preventDefault();
            var reps   = $("input.reps", form).val();
            var weight = $("input.weight", form).val();

            if (reps === '' || reps === '0') {
                return;
            }

            if (exercise.type == 'BW') {
                if (weight === '') {
                    weight = 0;
                }
            } else {
                if (weight === '') {
                    return;
                }
            }

            var data = {
                reps: reps,
                weight: weight,
                exerciseId: exercise.id,
                workoutId: workoutId
            };

            $.ajax({ url: "/rest/workout/exercise",
                     type: "POST",
                     data: data,
                     success: renderCallback
                   });
        });
    });
};

Workout.prototype._renderExercise = function (elt, workoutId, exercise) {
    var self = this;
    $(elt).html(self.exerciseTemplate(exercise));

    var render = function (resp) {
        self._renderExercise(elt, workoutId, resp);
    };

    this._attachAddExercise(elt, exercise, render, workoutId);
};

Workout.prototype._renderWorkout = function (elt, workout) {
    var self = this;

    $(elt).html(self.workoutTemplate(workout));

    $(".exercise", elt).each(function (exerciseIdx) {
        var exercise = workout.exercises[exerciseIdx];
        self._renderExercise(this, workout.id, exercise);
    });

    $("div.add-exercise", elt).html(self.addExerciseTemplate( {exerciseTypes: self.exerciseTypes.list() } ));

    $("select.select-exercise", elt).change(function () {
        var selectedExerciseId = $(this).val();
        var addExerciseScope = $(".add-exercise-sets", elt);
        var e = self.exerciseTypes.getExerciseById(selectedExerciseId);
        addExerciseScope.html(self.addExerciseSetsTemplate(e));

        var render = function (resp) {
            // Reload workout & rerender
            $.ajax({ url: "/rest/workout",
                     type: "GET",
                     data: { id: workout.id },
                     success: function (workoutResp) {
                         self._renderWorkout(elt, workoutResp);
                     }
                   });
        };
        self._attachAddExercise(addExerciseScope, e, render, workout.id);
    });
};

Workout.prototype._renderWorkouts = function (ws) {
    var self = this;
    var workouts = { workouts: ws };

    $("#app-container").html(self.mainTemplate(workouts));

    $(".workout").each(function (workoutIdx) {
        var workout = workouts.workouts[workoutIdx];
        self._renderWorkout(this, workout);
    });

    $("button#new-workout").click(function (elt) {
        $.ajax( { url: "/rest/workout",
                  type: "POST",
                  data: [],
                  success: function (resp) {
                      self._renderWorkouts(resp);
                  }
                });
    });
};

Workout.prototype.render = function () {
    var self = this;

    $.when(this._loadWorkouts(), this.exerciseTypes._loadExerciseTypes()).done(function (ws, es) {
        self.exerciseTypes.setExerciseList(es[0]);
        self._renderWorkouts(ws[0]);
    });
};
