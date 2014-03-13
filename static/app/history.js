define(['jquery', 'handlebars', 'underscore'], function($, Handlebars, _) {
    "use strict";

    // View workout history
    function History() {
        var self = this;
        this.mainTemplate = Handlebars.compile($("#workout-history-main-template").html());
        Handlebars.registerPartial("renderExercises", $("#exercise-no-edit-template").html());
    }

    History.prototype.loadPastWorkouts = function () {
        return $.ajax({
            type: "GET",
            data: { limit: 14 },
            url: "/rest/history/workout"
        });
    };

    History.prototype.renderPastWorkouts = function (ws) {
        var self = this;
        var workouts = { workouts: ws };

        // Compute some nice-to-know stats, like total reps count for each exercise
        _.each(workouts.workouts,
               function (w) {
                   _.each(w.exercises, function (e) {
                       if (e.type == "BW") {
                           e.totalReps = _.reduce(e.sets, function (a, s) { return a+s.reps; }, 0);
                       }
                   });
               });

        $("#app-container").html(self.mainTemplate(workouts));
    };

    History.prototype.render = function () {
        var self = this;

        $.when(self.loadPastWorkouts()).done(function (ws, es) {
            self.renderPastWorkouts(ws);
        });
    };

    // export
    return {
        'History': History
    };
});
