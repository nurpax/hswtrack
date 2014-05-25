define(['jquery', 'handlebars', 'underscore', 'app/view', 'hbs!templates/stats-main', 'hbs!templates/stats-history', 'hbs!templates/exercise-no-edit'], function($, Handlebars, _, view, templateStatsMain, templateStatsHistory, templateExercise) {
    "use strict";

    function loadPastWorkouts () {
        return $.ajax({
            type: "GET",
            data: { limit: 14 },
            url: "/rest/stats/workout"
        });
    }

    // View workout stats/history
    var StatsView = view.View.extend({
        init: function () {
            this._super();
            Handlebars.registerPartial('exerciseNoEdit', templateExercise);
        },

        renderPastWorkouts: function (ws) {
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

            $("#app-container").html(templateStatsMain(workouts));
            $("#history-tab").html(templateStatsHistory(workouts));

            $('a[data-toggle="tab"]').on('shown.bs.tab', function (e) {
                var target = $(e.target).attr("href");
                // target tab selected
            });
        },

        render: function () {
            var self = this;

            $.when(loadPastWorkouts()).done(function (ws, es) {
                self.renderPastWorkouts(ws);
            });
        }
    });

    // export
    return {
        'StatsView': StatsView
    };
});
