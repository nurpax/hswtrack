
define(['jquery', 'handlebars', 'bootstrap', 'd3', 'router', 'app/workout', 'app/stats'],
       function($, Handlebars, bootstrap, d3, router, workout, stats) {
    "use strict";

    function App() {
        this.selectedGraphDays = 3*30;
    }

    App.prototype.checkLogin = function(err) {
        if (err.status == 403) {
            router.goto("/login");
        }
    };

    App.prototype.renderPlot = function(app, weights) {
        var self = this;
        var data   = weights;
        var svgDiv = $("div#weight-plot");

        var margin = {top: 10, right: 10, bottom: 30, left: 25},
        width  = svgDiv.width() - margin.left - margin.right,
        height = svgDiv.width()*2/3 - margin.top - margin.bottom;

        var parseDate = d3.time.format("%Y-%m-%d").parse;

        var x = d3.time.scale()
            .range([0, width]);

        var y = d3.scale.linear()
            .range([height, 0]);

        var xAxis = d3.svg.axis()
            .scale(x)
            .orient("bottom");

        var yAxis = d3.svg.axis()
            .scale(y)
            .orient("left");

        var line = d3.svg.line()
            .x(function(d) { return x(d.date); })
            .y(function(d) { return y(d.weight); });

        d3.select("div#weight-plot svg").remove();

        var svg = d3.select("div#weight-plot").append("svg")
            .attr("width", width + margin.left + margin.right)
            .attr("height", height + margin.top + margin.bottom)
            .append("g")
            .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

        var pd = data.map(function (d) { return { date: parseDate(d.date), weight: d.weight }; });
        x.domain(d3.extent(pd, function(d) { return d.date; }));

        if (app.context.options.minGraphWeight)
            y.domain([app.context.options.minGraphWeight, d3.max(pd, function(d) { return d.weight; })]);
        else
            y.domain(d3.extent(pd, function(d) { return d.weight; }));

        svg.append("g")
            .attr("class", "x axis")
            .attr("transform", "translate(0," + height + ")")
            .call(xAxis);

        svg.append("g")
            .attr("class", "y axis")
            .call(yAxis)
            .append("text")
            .attr("transform", "rotate(-90)")
            .attr("y", 6)
            .attr("dy", ".71em")
            .style("text-anchor", "end")
            .text("Weight (kg)");

        svg.append("path")
            .datum(pd)
            .attr("class", "line")
            .attr("d", line);
    };

    App.prototype.loadWeights = function(ndays) {
        return $.ajax({
            type: "GET",
            url: "/rest/weights",
            data: { days: ndays }
        });
    };

    App.prototype.loadAppContext = function() {
        return $.ajax({
            type: "GET",
            url: "/rest/app",
            data: []
        });
    };

    App.prototype.loadNotes = function() {
        return $.ajax({
            type: "GET",
            url: "/rest/notes",
            data: []
        });
    };

    App.prototype.renderLoginScreen = function(context, url) {
        var self = this;
        var ctx = context ? context : {};

        if (url == "new_user")
            ctx.loginForm = false;
        else if (url == "login")
            ctx.loginForm = true;

        $("#app-container").html(self.templateLogin(ctx));

        $("form#login").submit(function (e) {
            e.preventDefault();
            $.ajax({ url: "/rest/"+url,
                     type: "POST",
                     data: $(this).serialize(),
                     success: function(resp) {
                         if (!resp.loggedIn) {
                             self.renderLoginScreen(resp, url);
                         } else {
                             router.goto("/");
                         }
                     }});
        });
    };

    App.prototype.renderNewUser = function() {
        this.renderLoginScreen(null, "new_user");
    };

    App.prototype.renderLogin = function() {
        this.renderLoginScreen(null, "login");
    };

    App.prototype.renderSettings = function() {
        var self = this;
        $.when(this.loadAppContext()).done(
            function (app) {
                $("#app-container").html(self.templateSettings(app.context));
            });
    };

    App.prototype.renderComments = function(notes)
    {
        var self = this;
        $("#comments").html(self.templateNotes({ notes: notes }));

        // Delete buttons
        $("#comments").each(function () {
            $("a#rm-note", this).each(function (idx) {
                $(this).click(function (e) {
                    e.preventDefault();

                    if (!confirm("OK to delete note?"))
                        return;

                    $.ajax({ url: "/rest/note",
                             type: "DELETE",
                             data: { id: notes[idx].id },
                             success: function (resp) {
                                 self.renderComments(resp);
                             }
                           });
                });
            });
        });

        // Add button
        $("button#note-input-btn").click(function () {
            $.ajax({ url: "/rest/note",
                     type: "POST",
                     data: { text: $("input#note-input").val() },
                     success: function (resp) {
                         self.renderComments(resp);
                     }
                   });
        });
    };

    App.prototype.renderHome = function(appContext, weights, notes) {
        var self = this;
        var plot = function(days) {
            self.selectedGraphDays = days;
            $.when(self.loadWeights(days)).done(function (ws) {
                self.renderPlot(appContext, ws);
            });
        };

        $("#app-container").html(self.templateHome(appContext.context));
        self.renderPlot(appContext, weights);
        self.renderComments(notes);

        var attachRadio = function (name, n) {
            $(name)
                .each(function () {
                    if (self.selectedGraphDays == n)
                        $(this).addClass("active");
                })
                    .click(function () {
                        plot(n);
                    });
        };

        attachRadio("label#graph-3-mo", 3*30);
        attachRadio("label#graph-12-mo", 12*30);
        attachRadio("label#graph-24-mo", 24*30);
        attachRadio("label#graph-all", 0);

        // Weight input
        $("#weight-input-btn").click(function () {
            var newWeight = $("input#weight-input").val();
            $.ajax({
                type: "POST",
                url: "/rest/weight",
                data: { weight: newWeight },
                success: function (r) { self.reloadHome(); }
            });
        });

        // Weight clear
        $("#weight-clear-btn").click(function () {
            $.ajax({
                type: "POST",
                url: "/rest/weight",
                data: { weight: null },
                success: function (r) { self.reloadHome(); }
            });
        });
    };

    App.prototype.reloadHome = function()
    {
        var self = this;
        // Load home & weights concurrently
        $.when(self.loadAppContext(),
               self.loadWeights(self.selectedGraphDays),
               self.loadNotes()).done(
            function (resp, wresp, nresp) {
                self.renderHome(resp[0], wresp[0], nresp[0]);
            });
    };

    App.prototype.start = function () {
        var self = this;

        Handlebars.registerHelper('round', function(num, dec) {
            return new Handlebars.SafeString(num.toFixed(dec));
        });

        Handlebars.registerHelper('ifBodyweight', function(v, options) {
            if(v.type === "BW") {
                return options.fn(this);
            }
            return options.inverse(this);
        });

        Handlebars.registerHelper('dateString', function(v, options) {
            return new Handlebars.SafeString((new Date(v)).toLocaleString());
        });

        // Compile templates
        this.templateHome = Handlebars.compile($("#home-template").html());
        this.templateNotes = Handlebars.compile($("#notes-template").html());
        this.templateSettings = Handlebars.compile($("#settings-template").html());
        this.templateLogin = Handlebars.compile($("#login-template").html());

        // Instruct router to go to the login screen if any latter AJAX
        // call returns "Login required" 403.
        $.ajaxSetup({ error: function (jqXHR, ts, e) {
            self.checkLogin(jqXHR);
        }});

        var et = new workout.ExerciseTypeView();
        var w  = new workout.WorkoutView();
        var s  = new stats.StatsView();

        router.add("/",         function()  { self.reloadHome(); });
        router.add("/workout",  function () { w.render(); });
        router.add("/workout/add-exercise",
                   function () { et.render(); });
        router.add("/stats",    function () { s.render(); });
        router.add("/settings", function()  { self.renderSettings(); });
        router.add("/login",    function()  { self.renderLogin(); });
        router.add("/new_user", function()  { self.renderNewUser(); });
        router.start();
    };

    // export
    return {
        'App': App
    };
});
