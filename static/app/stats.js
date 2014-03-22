define(['jquery', 'handlebars', 'underscore'], function($, Handlebars, _) {
    "use strict";

    // View workout stats/history
    function Stats() {
        var self = this;
        this.mainTemplate = Handlebars.compile($("#workout-stats-main-template").html());
        this.historyTemplate = Handlebars.compile($("#workout-stats-history-template").html());
        Handlebars.registerPartial("renderExercises", $("#exercise-no-edit-template").html());
    }

    Stats.prototype.loadPastWorkouts = function () {
        return $.ajax({
            type: "GET",
            data: { limit: 14 },
            url: "/rest/stats/workout"
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
        $("#history-tab").html(self.historyTemplate(workouts));

        $('a[data-toggle="tab"]').on('shown.bs.tab', function (e) {
            var target = $(e.target).attr("href");
            // target tab selected
        });
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
