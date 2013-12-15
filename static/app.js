
var appContext = null;
var weights = null;
var templateHome = null;
var templateSettings = null;
var templateLogin = null;

var selectedGraphDays = 3*30;

function checkLogin(err) {
    if (err.status == 403) {
        appContext = null;
        router.start();
        router.goto("/login");
    }
}

function renderPlot() {
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

    if (appContext.context.options.minGraphWeight)
        y.domain([appContext.context.options.minGraphWeight, d3.max(pd, function(d) { return d.weight; })]);
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

function loadWeightsDfd(ndays) {
    return $.ajax({
        type: "GET",
        url: "/rest/weights",
        data: { days: ndays }
    });
}

function loadAppContextDfd() {
    return $.ajax({
        type: "GET",
        url: "/rest/app",
        data: []
    });
}

function reloadHome(f) {
    // Load home & weights concurrently
    $.when(loadAppContextDfd(), loadWeightsDfd(selectedGraphDays)).done(
        function (resp, wresp) {
            appContext = resp[0];
            weights    = wresp[0];
            if (f)
                f();
            router.goto("/");
        });
}

function renderLoginScreen(url) {
    var ctx = appContext ? appContext : { };

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
                     appContext = resp;
                     if (!resp.loggedIn) {
                         router.goto(url);
                     } else {
                         router.start();
                         reloadHome();
                     }
                 }});
    });
}

function renderNewUser() {
    renderLoginScreen("new_user");
}

function renderLogin() {
    renderLoginScreen("login");
}

function renderSettings() {
    if (!appContext || !appContext.loggedIn) {
        renderLogin();
        return;
    }

    $("#app-container").html(templateSettings(appContext.context));
}

function renderHome() {
    if (!appContext || !appContext.loggedIn) {
        renderLogin();
        return;
    }

    var plot = function(days) {
        selectedGraphDays = days;
        wdfd = loadWeightsDfd(days);
        $.when(wdfd).done(function (ws) {
            weights = ws;
            renderPlot();
        });
    };

    $("#app-container").html(templateHome(appContext.context));
    renderPlot();

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

$(function () {
    Handlebars.registerHelper('round', function(num, dec) {
        return new Handlebars.SafeString(num.toFixed(dec));
    });

    // Compile templates
    templateHome = Handlebars.compile($("#home-template").html());
    templateSettings = Handlebars.compile($("#settings-template").html());
    templateLogin = Handlebars.compile($("#login-template").html());

    // Instruct router to go to the login screen if any latter AJAX
    // call returns "Login required" 403.
    $.ajaxSetup({ error: function (jqXHR, ts, e) {
        checkLogin(jqXHR);
    }});

    router.add("/",         renderHome);
    router.add("/settings", renderSettings);
    router.add("/login",    renderLogin);
    router.add("/new_user", renderNewUser);

    // Ask to reload the home page and if load OK, start router.  We
    // don't want to start the router before the AJAX load for home
    // succeeds, otherwise we're always first taken to a login page.
    // On error, automated AJAX error handlers will take us to the
    // login screen.
    reloadHome(function () { router.start(); });
});
