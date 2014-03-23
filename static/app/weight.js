define(['jquery', 'handlebars', 'underscore', 'app/view'], function($, Handlebars, _, view) {
    "use strict";

    // TODO these should be in a model class
    function loadAppContext() {
        return $.ajax({
            type: "GET",
            url: "/rest/app",
            data: []
        });
    }

    function loadWeights(ndays) {
        return $.ajax({
            type: "GET",
            url: "/rest/weights",
            data: { days: ndays }
        });
    }

    function loadNotes() {
        return $.ajax({
            type: "GET",
            url: "/rest/notes",
            data: []
        });
    }

    // View workout stats/history
    var WeightView = view.View.extend({
        init: function () {
            this._super();
            this.selectedGraphDays = 3*30;

            this.templateHome = Handlebars.compile($("#home-template").html());
            this.templateNotes = Handlebars.compile($("#notes-template").html());
        },

        renderPlot: function(app, weights) {
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
        },

        renderComments: function(notes)
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
        },

        renderHome: function(appContext, weights, notes) {
            var self = this;
            var plot = function(days) {
                self.selectedGraphDays = days;
                $.when(loadWeights(days)).done(function (ws) {
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
        },

        reloadHome: function()
        {
            var self = this;
            // Load home & weights concurrently
            $.when(loadAppContext(),
                   loadWeights(self.selectedGraphDays),
                   loadNotes()).done(
                       function (resp, wresp, nresp) {
                           self.renderHome(resp[0], wresp[0], nresp[0]);
                       });
        }

    });

    // export
    return {
        'WeightView': WeightView
    };
});
