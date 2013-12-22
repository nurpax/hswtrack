
var templateHome = null;
var templateNotes = null;
var templateSettings = null;
var templateLogin = null;

var selectedGraphDays = 3*30;

function checkLogin(err) {
    if (err.status == 403) {
        router.goto("/login");
    }
}

function renderPlot(app, weights) {
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

    var pd = data.map(function (d) { return { date: parseDate(d.date), weight: d.weight } });
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
}

function loadWeights(ndays) {
    return $.ajax({
        type: "GET",
        url: "/rest/weights",
        data: { days: ndays }
    });
}

function loadAppContext() {
    return $.ajax({
        type: "GET",
        url: "/rest/app",
        data: []
    });
}

function loadNotes() {
    return $.ajax({
        type: "GET",
        url: "/rest/notes",
        data: []
    });
}

function renderLoginScreen(ctx, url) {
    var ctx = ctx ? ctx : {};

    if (url == "new_user")
        ctx["loginForm"] = false;
    else if (url == "login")
        ctx["loginForm"] = true;

    $("#app-container").html(templateLogin(ctx));

    $("form#login").submit(function (e) {
        e.preventDefault();
        $.ajax({ url: "/rest/"+url,
                 type: "POST",
                 data: $(this).serialize(),
                 success: function(resp) {
                     if (!resp.loggedIn) {
                         renderLoginScreen(resp, url);
                     } else {
                         router.goto("/");
                     }
                 }});
    });
}

function renderNewUser() {
    renderLoginScreen(null, "new_user");
}

function renderLogin() {
    renderLoginScreen(null, "login");
}

function renderSettings() {
    $.when(loadAppContext()).done(
        function (app) {
            $("#app-container").html(templateSettings(app.context));
        });
}

function renderComments(notes)
{
    $("#comments").html(templateNotes({ notes: notes }));

    // Delete buttons
    $("#comments").each(function () {
        $("a#rm-note", this).each(function (idx) {
            $(this).click(function (e) {
                e.preventDefault();
                $.ajax({ url: "/rest/note",
                         type: "DELETE",
                         data: { id: notes[idx].id },
                         success: function (resp) {
                             renderComments(resp);
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
                     renderComments(resp);
                 }
               });
    });
}

function renderHome(appContext, weights, notes) {
    var plot = function(days) {
        selectedGraphDays = days;
        wdfd = loadWeights(days);
        $.when(wdfd).done(function (ws) {
            renderPlot(appContext, ws);
        });
    };

    $("#app-container").html(templateHome(appContext.context));
    renderPlot(appContext, weights);
    renderComments(notes);

    var attachRadio = function (name, n) {
        $(name)
            .each(function () {
                if (selectedGraphDays == n)
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
            success: function (r) { reloadHome(); }
        });
    });

    // Weight clear
    $("#weight-clear-btn").click(function () {
        $.ajax({
            type: "POST",
            url: "/rest/weight",
            data: { weight: null },
            success: function (r) { reloadHome(); }
        });
    });
}

function reloadHome()
{
    // Load home & weights concurrently
    $.when(loadAppContext(), loadWeights(selectedGraphDays), loadNotes()).done(
        function (resp, wresp, nresp) {
            renderHome(resp[0], wresp[0], nresp[0]);
        });
}

$(function () {
    Handlebars.registerHelper('round', function(num, dec) {
        return new Handlebars.SafeString(num.toFixed(dec));
    });

    // Compile templates
    templateHome = Handlebars.compile($("#home-template").html());
    templateNotes = Handlebars.compile($("#notes-template").html());
    templateSettings = Handlebars.compile($("#settings-template").html());
    templateLogin = Handlebars.compile($("#login-template").html());

    // Instruct router to go to the login screen if any latter AJAX
    // call returns "Login required" 403.
    $.ajaxSetup({ error: function (jqXHR, ts, e) {
        checkLogin(jqXHR);
    }});

    router.add("/",         reloadHome);
    router.add("/settings", renderSettings);
    router.add("/login",    renderLogin);
    router.add("/new_user", renderNewUser);
    router.start();
});
