
function Workout() {
    this.mainTemplate = Handlebars.compile($("#workouts-template").html());
    this.workoutTemplate = Handlebars.compile($("#workout-template").html());
    this.exerciseTemplate = Handlebars.compile($("#exercise-template").html());
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

Workout.prototype._renderExercise = function (elt, workoutId, exercise) {
    var self = this;
    $(elt).html(self.exerciseTemplate(exercise));

    $("form.add-set", elt).each(function () {
        var form = this;
        $("button", this).click(function (e) {
            e.preventDefault();
            var reps   = $("input.reps", form).val();
            var weight = $("input.weight", form).val();

            assert(reps != '');
            assert(weight != '');

            var data = {
                reps: reps,
                weight: weight,
                exerciseId: exercise.exerciseId,
                workoutId: workoutId
            };

            $.ajax({ url: "/rest/workout/exercise",
                     type: "POST",
                     data: data,
                     success: function (resp) {
                         self._renderExercise(elt, workoutId, resp);
                     }
                   });
        });
    });
};

Workout.prototype._renderWorkout = function (elt, workout) {
    var self = this;
    $(elt).html(self.workoutTemplate(workout));

    $(".exercise").each(function (exerciseIdx) {
        var exercise = workout.exercises[exerciseIdx];
        self._renderExercise(this, workout.id, exercise);
    });
};

Workout.prototype.render = function () {
    var self = this;

    $.when(this._loadWorkouts()).done(function (ws) {
        var workouts = { workouts: ws };

        $("#app-container").html(self.mainTemplate(workouts));

        $(".workout").each(function (workoutIdx) {
            var workout = workouts.workouts[workoutIdx];
            self._renderWorkout(this, workout);
        });
    });
};
