define(['jquery', 'handlebars', 'underscore', 'app/view', 'app/model', 'hbs!templates/stats-main', 'hbs!templates/stats-history', 'hbs!templates/exercise-no-edit'], function($, Handlebars, _, view, model, templateStatsMain, templateStatsHistory, templateExercise) {
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

            $("#app-container").html(templateStatsMain(workouts));
            $("#history-tab").html(templateStatsHistory(workouts));

            $('a[data-toggle="tab"]').on('shown.bs.tab', function (e) {
                var target = $(e.target).attr("href");
                // target tab selected
            });
        },

        render: function () {
            var self = this;

            $.when(loadPastWorkouts()).done(function (ws) {
                var workouts = _.map(ws.payload, function(w) { return new model.Workout(w); });
                self.renderPastWorkouts(workouts);
            });
        }
    });

    // export
    return {
        'StatsView': StatsView
    };
});
