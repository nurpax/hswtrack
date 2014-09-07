define(['jquery', 'd3', 'handlebars', 'underscore', 'app/view', 'app/model', 'hbs!templates/weight', 'hbs!templates/weight-notes'], function($, d3, Handlebars, _, view, model, templateHome, templateNotes) {
    "use strict";

    // View workout stats/history
    var WeightView = view.View.extend({
        init: function () {
            this._super();
            this.selectedGraphDays = 3*30;
            this.model = new model.WeightCont();
        },

        renderPlot: function(weights) {
            var self    = this;
            var data    = weights.weights;
            var context = self.model.app.context;
            var svgDiv  = $("div#weight-plot");

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

            if (context.options.minGraphWeight)
                y.domain([context.options.minGraphWeight, d3.max(pd, function(d) { return d.weight; })]);
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
            $("#comments").html(templateNotes(notes));

            // Delete buttons
            $("#comments").each(function () {
                $("a#rm-note", this).each(function (idx) {
                    $(this).click(function (e) {
                        e.preventDefault();

                        if (!confirm("OK to delete note?"))
                            return;

                        notes.deleteById(notes.notes[idx].id);
                    });
                });
            });

            // Add button
            $("button#note-input-btn").click(function () {
                notes.addNote($("input#note-input").val());
            });
        },

        renderHome: function() {
            var self = this;

            Handlebars.registerHelper('round', function(num, dec) {
                return new Handlebars.SafeString(num.toFixed(dec));
            });

            $("#app-container").html(templateHome(self.model.app.context));

            self.model.weights.setUpdateHandler(function (w) { self.renderPlot(w); });
            self.model.notes.setUpdateHandler(function (n) { self.renderComments(n); });

            // Weight input
            $("#weight-input-btn").click(function () {
                var newWeight = $("input#weight-input").val();
                self.model.setWeight(newWeight);
            });

            // Weight clear
            $("#weight-clear-btn").click(function () {
                self.model.clearWeight();
            });

            var attachRadio = function (name, days) {
                $(name).each(function () {
                    if (self.selectedGraphDays == days)
                        $(this).addClass("active");
                }).click(function () {
                    self.selectedGraphDays = days;
                    self.model.weights.load(days);
                });
            };

            attachRadio("label#graph-3-mo", 3*30);
            attachRadio("label#graph-12-mo", 12*30);
            attachRadio("label#graph-24-mo", 24*30);
            attachRadio("label#graph-all", 0);
        },

        render: function () {
            var self = this;
            this.model.setUpdateHandler(function () { self.renderHome(); });
            this.model.load(this.selectedGraphDays);
        }

    });

    // export
    return {
        'WeightView': WeightView
    };
});
