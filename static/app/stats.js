define(['jquery', 'handlebars', 'underscore'], function($, Handlebars, _) {
    "use strict";

    // View workout stats/history
    function Stats() {
        var self = this;
        this.mainTemplate = Handlebars.compile($("#workout-stats-main-template").html());
        Handlebars.registerPartial("renderExercises", $("#exercise-no-edit-template").html());
    }

    Stats.prototype.loadPastWorkouts = function () {
        return $.ajax({
            type: "GET",
            data: { limit: 14 },
            url: "/rest/history/workout"
        });
    };

    Stats.prototype.renderPastWorkouts = function (ws) {
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

    Stats.prototype.render = function () {
        var self = this;

        $.when(self.loadPastWorkouts()).done(function (ws, es) {
            self.renderPastWorkouts(ws);
        });
    };

    // export
    return {
        'Stats': Stats
    };
});
