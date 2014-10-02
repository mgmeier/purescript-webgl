starter-kit
===========

A basic project template using Grunt and Bower.

Getting Started
---------------
 
This tutorial uses `grunt` as its build tool. If you don't have `grunt` installed, install it now:

    npm install -g grunt-cli

We will also be using `bower` for package management. If you don't have `bower` installed, install it now:

    npm install -g bower

Now install a grunt instance and plugins locally, as specified in `package.json`:

    npm install

And pull required dependencies specified in `bower.json`:

    bower update

You should now be able to build the project and run the test suite:

    grunt

You should see something like the following:

```
Running "clean:tests" (clean) task
Cleaning tmp...OK

Running "purescript:tests" (purescript) task
>> Created file tmp/tests.js.

Running "execute:tests" (execute) task
-> executing tmp/tests.js
The differences of an empty list are empty.
All tests passed
The differences of a single-element list are empty.
All tests passed
The differences of a pair of equal elements are zero.
All tests passed
The diffs function returns Just (...) for a sorted list.
All tests passed
The diffs function returns Nothing for a reverse-sorted list with at least one pair of unequal elements.
All tests passed
-> completed tmp/tests.js (50ms)

>> 1 file and 0 calls executed (60ms)

Running "purescript-make:lib" (purescript-make) task
>> Make was successful.

Done, without errors.
```
