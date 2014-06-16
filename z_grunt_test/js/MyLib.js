(function (global) {
  var originalMyLib = global.MyLib;

  function MyLib(a, b) {
    if (a !== undefined) {
      this.a = a;
    }
    if (b !== undefined) {
      this.b = b;
    }
  }

  /* Static members */

  /**
   * We love everything that isn't undefined.
   * If it is defined, it is cool in our book.
   *
   * @param {mixed} o
   * @return {boolean}
   */
  MyLib.isAwesome = function isAwesome(o) {
    return o !== undefined;
  };

  /* Instance members */

  MyLib.prototype.a = 'a';

  MyLib.prototype.b = 'b';

  if (typeof module !== 'undefined' && module.exports) {
    module.exports = MyLib;
  } else {
    global.MyLib = MyLib;

    /**
     * For browsers, also implement a noConflict that restores
     * the orignal value of the used global (if any).
     * @example
     * <code>
     *     var someLib = MyLib.noConflict();
     * </code>
     */
    MyLib.noConflict = function () {
      global.MyLib = originalMyLib;
      return MyLib;
    };
  }

}(this));
