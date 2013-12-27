
function Workout() {
    this.mainTemplate = Handlebars.compile($("#workout-template").html());
};

Workout.prototype.render = function () {
    $("#app-container").html(this.mainTemplate({}));
};
