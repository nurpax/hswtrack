
var mockData = {
    workouts: [
        { id: 1,
          name: "Workout 1",
          exercises: [
              { id: 1,
                exerciseId: 3,
                name: "Bench",
                sets: [
                    {id: 1, reps: 5, weight:80},
                    {id: 2, reps: 5, weight:80},
                    {id: 3, reps: 5, weight:80}
                ]
              },
              { id: 4,
                exerciseId: 4,
                name: "Barbell Front Squat",
                sets: [
                    {id: 4, reps: 5, weight:80},
                    {id: 5, reps: 5, weight:80},
                    {id: 6, reps: 5, weight:80}
                ]
              }
          ]
        }
    ]
};

function Workout() {
    this.mainTemplate = Handlebars.compile($("#workouts-template").html());
};

Workout.prototype.render = function () {
    var workouts = mockData;

    $("#app-container").html(this.mainTemplate(workouts));

    $(".workout").each(function (workoutIdx) {
        var workout = workouts.workouts[workoutIdx];
        $(".exercise").each(function (exerciseIdx) {
            var exercise = workout.exercises[exerciseIdx];
        });
    });
};
