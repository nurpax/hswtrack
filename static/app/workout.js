define(['jquery', 'handlebars', 'app/model', 'app/view'], function($, Handlebars, model, view) {
    "use strict";

    var View = view.View;

    // Page for adding new exercise types
    var ExerciseTypeView = View.extend({
        init: function () {
            var self = this;
            this._super();
            this.compileTemplates(['new-exercise-template']);
            this.exercises = new model.ExerciseTypes();
            this.exercises.setUpdateHandler(function (c) { self.renderExerciseList(); });
        },

        renderExerciseList: function () {
            var self = this;
            $("#app-container").html(self.templates.newExercise({ exercises: self.exercises.exerciseTypes }));

            $("form#new-exercise").each(function () {
                var form = this;
                $("button#new-workout", form).click(function (e) {
                    e.preventDefault();
                    var exerciseName = $("input#exercise-name", form).val();
                    var type         = $("input[name=exercise-type-radios]:checked", form).val();

                    self.exercises.addExercise({ name: exerciseName, type:type });
                });
            });
        },

        render: function () {
            this.exercises.load();
        }
    });


    function WorkoutView() {
        var self = this;

        this.mainTemplate = Handlebars.compile($("#workouts-template").html());
        this.workoutTemplate = Handlebars.compile($("#workout-template").html());
        this.exerciseTemplate = Handlebars.compile($("#exercise-template").html());
        this.addExerciseTemplate = Handlebars.compile($("#add-exercise-template").html());
        this.addExerciseSetsTemplate = Handlebars.compile($("#add-exercise-sets-template").html());
        Handlebars.registerPartial("addSetControl", $("#add-set-control-partial").html());

        this.model = new model.WorkoutCont();
    }


    WorkoutView.prototype.attachAddExercise = function (elt, addSetCB, exercise, workoutId) {
        var self = this;

        $("form.add-set", elt).each(function () {
            var form = this;

            $('a#collapse-weighted', form).click(function(e) {
                e.preventDefault();
                var c = $(this).closest('.collapse-group').find('.collapse');
                c.collapse('toggle');
                $(this).hide();
            });


            $("button", this).click(function (e) {
                e.preventDefault();
                var reps   = $("input.reps", form).val();
                var weight = $("input.weight", form).val();

                if (reps === '' || reps === '0') {
                    return;
                }

                if (exercise.type == 'BW') {
                    if (weight === '') {
                        weight = 0;
                    }
                } else {
                    if (weight === '') {
                        return;
                    }
                }

                var data = {
                    reps: reps,
                    weight: weight,
                    exerciseId: exercise.id,
                    workoutId: workoutId
                };

                // attachAddExercise may either need to refresh just
                // the current exercise sets list, or the whole
                // workout where the set is being added.  This is why
                // the set addition is parametrized, and we don't
                // simply call exercise.addSet here.
                addSetCB(data);
            });
        });
    };

    WorkoutView.prototype.renderExercise = function (exercise, workoutId, elt) {
        var self = this;
        $(elt).html(self.exerciseTemplate(exercise));
        this.attachAddExercise(elt, function (d) { exercise.addSet(d); }, exercise, workoutId);

        // Activate delete buttons
        $("table tr", elt).each(function (idx) {
            $("a.rm-set", this).each(function () {
                $(this).click(function (e) {
                    e.preventDefault();

                    if (!confirm("OK to delete set?"))
                        return;

                    var data = { id: exercise.sets[idx].id };
                    exercise.deleteSet(data);
                });
            });
        });
    };

    WorkoutView.prototype.renderWorkout = function (workout, elt) {
        var self = this;

        var c = { exercises: workout.getExercises() };
        $(elt).html(self.workoutTemplate(c));

        $(".exercise", elt).each(function (exerciseIdx) {
            var exerciseElt = this;
            var exercise    = c.exercises[exerciseIdx];
            exercise.setUpdateHandler(function (e) { self.renderExercise(e, workout.id, exerciseElt); });
        });

        $("div.add-exercise", elt).html(self.addExerciseTemplate(
            { exerciseTypes: self.model.getExerciseTypes() }
        ));

        $("select.select-exercise", elt).change(function () {
            var selectedExerciseId = $(this).val();
            var addExerciseScope = $(".add-exercise-sets", elt);
            var e = self.model.getExerciseById(selectedExerciseId);
            addExerciseScope.html(self.addExerciseSetsTemplate(e));

            self.attachAddExercise(addExerciseScope,
                                   function (d) { workout.addExerciseSet(e, d); },
                                   e,
                                   workout.id);
        });
    };

    WorkoutView.prototype.renderWorkouts = function (workoutCont) {
        var self = this;
        var workouts = { workouts: workoutCont.getWorkouts() };

        $("#app-container").html(self.mainTemplate(workouts));

        $(".workout").each(function (workoutIdx) {
            var workoutElt = this;
            var workout    = workouts.workouts[workoutIdx];
            workout.setUpdateHandler(function (w) { self.renderWorkout(w, workoutElt); });
        });

        $("button#new-workout").click(function (elt) {
            workoutCont.newWorkout();
        });
    };

    WorkoutView.prototype.render = function () {
        var self = this;

        self.model.setUpdateHandler(function (c) { self.renderWorkouts(c); });
        self.model.load();
    };

    // export
    return {
        'WorkoutView': WorkoutView,
        'ExerciseTypeView': ExerciseTypeView,
    };
});
