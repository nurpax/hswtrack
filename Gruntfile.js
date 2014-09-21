/*global module:false*/
module.exports = function(grunt) {
  grunt.initConfig({
    
    react: {
      jsx: {
        files: [
          {
            expand: true,
            cwd: 'static/app/jsx',
            src: [ '**/*.js' ],
            dest: 'static/jsx-build',
            ext: '.js'
          }
        ]
      }
    },
  });

  grunt.loadNpmTasks('grunt-contrib-watch');
  grunt.loadNpmTasks('grunt-react');
  
  grunt.registerTask('default', ['react']);
};
