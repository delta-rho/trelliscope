module.exports = function (grunt) {
  grunt.loadNpmTasks('grunt-contrib-jshint');
  grunt.loadNpmTasks('grunt-contrib-qunit');


  grunt.initConfig({
    qunit: {
      all: {
        options: {
          timeout: 3 * 60 * 1000,
          urls: [
            'http://localhost:8100/?test=1'
          ]
        }
      }
    },
    jshint: {
      all: ['z_grunt_test/js/*.js']
    }
  });

  grunt.registerTask('test', ['jshint', 'qunit']);
  grunt.registerTask('default', ['test']);
};
