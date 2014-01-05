/* This little package sets up a router */

window.router = {
		options: {
			hashFormat: '#',
			pruneFilename: true,
			interval: true,
			indexFilename: 'index.html'
		},
		hashMatch: null,
		interval: null,
		paths: {},
		current_hash: '666',
		start: function(options) {
		
			// incorporate parameters into objet configuration
			for (var o in options) {
				this.options[o] = options[o];
			}
			
			// set up hash pattern
			this.hashMatch = new RegExp('^'+this.options.hashFormat);

			// set base url
			if (this.options.pruneFilename) {
				this.base_url = window.location.pathname.replace(/[^\/]*$/, "");
			}

			// if we're going to use the automatic hash check instead of binding click handlers,
			// do that!
			if (this.options.interval && !this.interval) {
				this.interval = setInterval(function() { router.checkURL(); },200);
			}
			
			// fire up the router by checking current URL
			this.checkURL();
		},
		route: function(hash) {
			for (path in this.paths) {
				var pattern = path.replace(/\//g,"\\/");
				pattern = new RegExp('^'+pattern+'$','i');
				var matches = hash.match(pattern)
				if (matches!=null) {
					matches.shift();
					var func = this.paths[path];
					func.call(null,matches);					
					return true;
				}
			}
			
			if (this.paths['404']) {
				var func = this.paths['404'];
				func.call(null,hash);
				return true;
			}			
		},
		checkURL: function() {
			var hash = window.location.hash;
			var href = window.location.pathname;
			var atRoot = true;


			if (this.base_url!=href) {
				atRoot = false;		
				if (hash=='' || !hash) {
					hash = href.replace(this.base_url,'');
					if (hash[0]!='/') {
						hash = '/'+hash;
					}
					// make sure index.html is the same as /
					if (hash == '/'+this.options.indexFilename) {
						hash = '';
						atRoot = true;
					}

				}
			}
			
			
			if (atRoot && !hash) { hash ='/'; }

			// normalize hash so it matches url strings
			if (hash.match(this.hashMatch)) {
				hash = hash.substr(this.options.hashFormat.length,hash.length);
			}

			if (hash!=this.current_hash) {
			
				// if user is at a URL that is not the root url
				// but has navigated to a hash on that sub-url within the app,
				// make it look like they're actually at the root of the app.
				// example: mydomain.com/permalink.html#home -> mydomain.com/#home
				if (!atRoot && hash!='') {
					if (history.replaceState) {
						history.replaceState(null,null,this.base_url+hash.substr(1,hash.length));
					} else {
						window.location = this.base_url+this.options.hashFormat+hash;
					}
				}

				this.current_hash = hash;

				this.route(hash);				
			}	
		},	
		// accepts either a path and a callback
		// or an array of paths mapped to callbacks
		add: function(path,callback) {
			if (typeof path == 'object') {
				for (var p in path) {
					this.paths[p] = path[p];
				}
			} else {
				this.paths[path]=callback;
			}
		},
		goto: function(path) {
			if (path.match(this.hashMatch)) {
				path = path.substr(this.options.hashFormat.length,path.length);
			}
			if (path.match(/\:\/\//)) {
				return false;
			}
			// make sure paths are formatted like absolute links
			if (path[0] != '/') {
				path = '/' + path;
			}
			if (this.route(path)) {
				if (history.pushState) {
					if (this.base_url[this.base_url.length-1]=='/') {
						url = this.base_url + path.substr(1,path.legth);
					} else {
						url = this.base_url + path;
					}

		
					// reset hash so we don't trigger a change.
					this.current_hash=path;
					
					// push this url onto the history stack.
					history.pushState(null,null,url);

				} else {
					// no pushstate,
					// so translate this into a hash url instead.
					url = this.base_url+this.options.hashFormat+path;
					
					// setting the location will trigger the router
					window.location = url;
				}
				return true;
			} else {
				return false;
			}
		}
};
