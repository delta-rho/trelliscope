QUnit.module('General');

QUnit.test('some basic tests', 7, function (assert) {
  var x, y;

  assert.equal('Foo', 'Foo', 'Similar strings are.. equal');

  assert.equal(true, 1, 'Boolean true and 1 are similar');

  assert.notStrictEqual(true, 1, '... but, boolean true and 1 are not *strictly* the same');

  // Primitive values are compared by value. Two trues are both stictly the same.
  assert.strictEqual(true, true, 'of course one boolean true is *strictly* the same as another boolean true');

  x = { one : 1, two: 2 };
  y = x;
  assert.strictEqual(
    x,
    y,
    'assert.strictEqual compares by reference, same references are equal'
  );
  assert.notStrictEqual(
    { one : 1, two: 2 },
    { one: 1, two: 2 },
    'assert.strictEqual compares by reference, different references with the same values are not equal'
  );
  assert.deepEqual(
    { one : 1, two: 2 },
    { one: 1, two: 2 },
    'assert.deepEqual compares values, not different references with the same values are equal'
  );

  // example of failing tests


  // assert.equal('This string is not the same', 'as this one', 'different strings are.. different, this fails!');



  // assert.equal(
  //   { one : 1, two: 2 },
  //   { one: 1, two: 2 },
  //   'assert.equal compares objects by reference, different (but similar) objects are not equal'
  // );

});

QUnit.module('myLib');

QUnit.test('isAwesome', 4, function (assert) {
  assert.strictEqual(MyLib.isAwesome('hey'), true, 'Strings are awesome');
  assert.strictEqual(MyLib.isAwesome(123), true, 'Numbers are awesome');

  assert.strictEqual(MyLib.isAwesome(), false, 'Undefined (implicit) is not cool');
  assert.strictEqual(MyLib.isAwesome(undefined), false, 'Undefined (explicit) is not cool');
});


QUnit.module('Delayed Callback');

QUnit.asyncTest('5 sec timeout', 2, function (assert) {
  assert.equal("a", "a", "A is A");

  my_callback = function() {
    assert.equal("a", "a", "A is A in callback");
    QUnit.start();
  };
  setTimeout(my_callback, 5000);
});

