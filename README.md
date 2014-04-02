hswtrack - fitness app
========

A web application for tracking your weight and keeping track of your exercises.  I found this to be the best way to combine exercising and programming.

[Imgur](http://i.imgur.com/3kgnvm7)

Tech details
----------------

Written in Haskell using the Snap Framework using Sqlite3 for persistence.

Departs from typical Snap apps in that it doesn't use Heist templating.  Rather all HTML templating is done in JavaScript on the client using Handlebars.js.  Various other JavaScript libraries like JQuery, RequireJS and D3.js are used.
