
var appContext = null;
var templateHome = null;
var templateSettings = null;

function renderSettings()
{
    $("#app-container").html(templateSettings(appContext));
}

function renderHome()
{
    $("#app-container").html(templateHome(appContext));
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
