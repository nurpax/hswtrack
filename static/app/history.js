define(['jquery', 'handlebars'], function($, Handlebars) {
    // View workout history
    function History() {
        var self = this;
        this.mainTemplate = Handlebars.compile($("#workout-history-main-template").html());
        Handlebars.registerPartial("renderExercises", $("#exercise-no-edit-template").html());
    };

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
