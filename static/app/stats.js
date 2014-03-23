define(['jquery', 'handlebars', 'underscore', 'app/view'], function($, Handlebars, _, view) {
    "use strict";

    // View workout stats/history
    var StatsView = view.View.extend({
        init: function () {
            this._super();
            this.compileTemplates(['stats-main-template', 'stats-history-template']);
            this.registerPartials(Handlebars, ['exercise-no-edit-partial']);
        },

        loadPastWorkouts: function () {
            return $.ajax({
                type: "GET",
                data: { limit: 14 },
                url: "/rest/stats/workout"
            });
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

            $("#app-container").html(self.templates.statsMain(workouts));
            $("#history-tab").html(self.templates.statsHistory(workouts));

            $('a[data-toggle="tab"]').on('shown.bs.tab', function (e) {
                var target = $(e.target).attr("href");
                // target tab selected
            });
        },

        render: function () {
            var self = this;

            $.when(self.loadPastWorkouts()).done(function (ws, es) {
                self.renderPastWorkouts(ws);
            });
        }
    });

    // export
    return {
        'StatsView': StatsView
    };
});
