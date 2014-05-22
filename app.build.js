({
    appDir: "",
    baseUrl: "static",
    dir: "build-js",
    mainConfigFile: 'static/main.js',
    paths: {
      // The handlebars library we include is the runtime one
      "handlebars": "lib/handlebars-1.3.0/handlebars.runtime-v1.3.0",

      // Let's define the compiler to the full one, so the hbs-builder can
      // precompile templates
      "handlebars-compiler": "lib/handlebars-1.3.0/handlebars-v1.3.0",
    },
    optimize: "uglify",
    // We don't need handlebars-compiler in the final module (we already have
    // handlebars-runtime). Exclude it from any module you define.
    excludeShallow: ["handlebars-compiler"],
    name: "main"
})
