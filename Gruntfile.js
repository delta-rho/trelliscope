module.exports = function (grunt) {
  grunt.loadNpmTasks('grunt-contrib-jshint');
  grunt.loadNpmTasks('grunt-contrib-qunit');

  grunt.initConfig({
    jshint: {
      all: ['z_grunt_test/js/*.js']
    },
    qunit: {
      all: ['z_grunt_test/index.html']
    }
  });

  grunt.registerTask('test', ['jshint', 'qunit']);
  grunt.registerTask('default', ['test']);
};
