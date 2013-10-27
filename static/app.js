
function routeSettings()
{
    console.log("#settings");
}

function routeHome()
{
    console.log("#home");
}

$(function () {
    router.add("/", routeHome);
    router.add("/settings", routeSettings);
    router.start();
    console.log("hello world");
});
