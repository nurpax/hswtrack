hswtrack - fitness app
========

A web application for tracking your weight and keeping track of your exercises.  I found this to be the best way to combine exercising and programming.

Here's a screenshot taken on a Nexus 5:

<a href="http://imgur.com/3kgnvm7"><img src="http://i.imgur.com/3kgnvm7l.png" title="Hosted by imgur.com"/></a>

Running unit tests
------------------

 1. Start the hswtrack server in test config with: ```cabal run -- -e test```
 2. In another shell, run: ```cabal test```

JSX building
------------

./node_modules/.bin/jsx --watch static/app/jsx static/jsx-build

Building minified Javascript
----------------------------

```
npm install -g requirejs
cd <project_root>
r.js  -o app.build.js
```

Building custom jquery with only AJAX features
----------------------------------------------

From http://stackoverflow.com/questions/15853584/extract-ajax-part-from-jquery-library:

./node_modules/.bin/grunt custom:-css,-deprecated,-dimensions,-effects,-event-alias,-offset,-sizzle

For more details, see: https://github.com/jquery/jquery#how-to-build-your-own-jquery

Tech details
----------------

Written in Haskell using the Snap Framework using Sqlite3 for persistence.

Departs from typical Snap apps in that it doesn't use Heist templating.  Rather all HTML templating is done in JavaScript on the client using ReactJS.
