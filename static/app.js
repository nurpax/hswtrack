
var templateHome = null;
var templateSettings = null;

function renderSettings()
{
    var context = { username: "Janne" };
    $("#app-container").html(templateSettings(context));
}

function renderHome()
{
    var context = { username: "Janne" };
    $("#app-container").html(templateHome(context));
}

$(function () {
    // Compile templates
    templateHome = Handlebars.compile($("#home-template").html());
    templateSettings = Handlebars.compile($("#settings-template").html());

    // Routes
    router.add("/", renderHome);
    router.add("/settings", renderSettings);
    router.start();
});
