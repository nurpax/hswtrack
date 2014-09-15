
define(['jquery', 'router', 'jsx/login', 'jsx/workout', 'jsx/weight', 'jsx/stats', 'jsx/exerciseEdit', 'jsx/settings'],
       function($, router, login, workout, weight, stats, exerciseEdit, settings) {
    "use strict";

    function App() {
        this.router = new Router();
    }

    App.prototype.checkLogin = function(err) {
        if (err.status == 403) {
            this.router.navigate("/login");
        }
    };

    App.prototype.renderLoginScreen = function(context, url) {
      var self     = this;
      var ctx      = context ? context : {};
      var onSubmit = function (formParams) {
        $.ajax({url: "/rest/"+url,
                type: "POST",
                data: formParams,
                success: function(resp) {
                  if (!resp.loggedIn) {
                    self.renderLoginScreen(resp, url);
                  } else {
                    self.router.navigate("/");
                  }
                }});
        return false;
      };
      login.renderLogin(url, ctx, onSubmit);
    };

    App.prototype.renderNewUser = function() {
        this.renderLoginScreen(null, "new_user");
    };

    App.prototype.renderLogin = function() {
        this.renderLoginScreen(null, "login");
    };

    App.prototype.start = function () {
        var self = this;

        // Instruct router to go to the login screen if any latter AJAX
        // call returns "Login required" 403.
        $.ajaxSetup({ error: function (jqXHR, ts, e) {
            self.checkLogin(jqXHR);
        }});

        self.router.route("/exercise/edit", function () { exerciseEdit.render(); });
        self.router.route("/workout/:id", function (id) { workout.render(id); });
        self.router.route("/workout",     function ()   { workout.renderToday(); });
        self.router.route("/stats",       function ()   { stats.render(); });
        self.router.route("/settings",    function()    { settings.render(); });
        self.router.route("/login",       function()    { self.renderLogin(); });
        self.router.route("/new_user",    function()    { self.renderNewUser(); });
        self.router.route("/",            function()    { weight.render(); });
        self.router.start();
    };

    // export
    return {
        'App': App
    };
});
