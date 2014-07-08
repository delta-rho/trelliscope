module.exports = function (grunt) {
  grunt.loadNpmTasks('grunt-contrib-jshint');
  grunt.loadNpmTasks('grunt-contrib-qunit');


  grunt.initConfig({
    // https://www.npmjs.org/package/grunt-contrib-qunit
    qunit: {
      all: {
        options: {
          timeout: 10 * 60 * 1000, // 10 minutes (matches travis-ci timeout)
          urls: [
            'http://localhost:8100?test=1'
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
