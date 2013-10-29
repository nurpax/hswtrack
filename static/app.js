
var appContext = null;
var templateHome = null;
var templateSettings = null;

var selectedGraphDays = 3*30;

function renderPlot()
{
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

    var getParams = "?days=" + selectedGraphDays;

    d3.json("/rest/weights"+getParams, function(error, data) {
        data.forEach(function(d) {
            d.date = parseDate(d.date);
        });

        x.domain(d3.extent(data, function(d) { return d.date; }));
        y.domain(d3.extent(data, function(d) { return d.weight; }));

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
            .datum(data)
            .attr("class", "line")
            .attr("d", line);
    });
}

function renderSettings()
{
    $("#app-container").html(templateSettings(appContext));
}

function renderHome()
{
    var plot = function(days) {
        selectedGraphDays = days;
        renderPlot();
    };

    $("#app-container").html(templateHome(appContext));
    renderPlot();

    $("label#graph-3-mo").click(function () { plot(3*30); });
    $("label#graph-12-mo").click(function () { plot(12*30); });
    $("label#graph-24-mo").click(function () { plot(24*30); });
    $("label#graph-all").click(function () { plot(0) });
}

$(function () {
    // Compile templates
    templateHome = Handlebars.compile($("#home-template").html());
    templateSettings = Handlebars.compile($("#settings-template").html());

    // Load UI parameters and setup routing + render main page after
    // finished all loading
    $.ajax({
        type: "GET",
        url: "/rest/app",
        data: [],
        success: function(resp) {
            appContext = resp;

            // Routes
            router.add("/", renderHome);
            router.add("/settings", renderSettings);
            router.start();
        }});

});
