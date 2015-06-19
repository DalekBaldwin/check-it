# check-it

[![Build Status](https://travis-ci.org/DalekBaldwin/check-it.svg?branch=master)](https://travis-ci.org/DalekBaldwin/check-it)

This is a randomized property-based testing library for Common Lisp. Rather than being a full-fledged general test framework in its own right, it's designed to embed randomized tests in whatever framework you like.

## Table of Contents
  * [Generating](#generating)
    * [Number Generators](#number-generators)
    * [Character Generators](#character-generators)
    * [List and Tuple Generators](#list-and-tuple-generators)
    * [Or Generator](#or-generator)
    * [Guard Generator](#guard-generator)
    * [Struct Generator](#struct-generator)
    * [Mapped Generators](#mapped-generators)
    * [Chained Generators](#chained-generators)
    * [User-Defined Generators](#user-defined-generators)
  * [Checking](#checking)
    * [Regression Cases](#regression-cases)
  * [Shrinking](#shrinking)

## Generating

Generator objects are constructed using the `generator` macro, which takes a generator type spec as a single argument. Generator type specs are not exactly the same as Common Lisp type specs, but they're designed to look syntactically similar (and in some cases exactly the same).

Values are generated by calling `generate` on generator objects. This function is normally called behind the scenes when using the library's full functionality, but it can also be used standalone to generate interesting data for other purposes.

The different varieties are:

### Number Generators

The `integer` generator accepts the same syntax as the standard compound type specifier syntax for integers:

```lisp
;; generates any integer
(integer)

;; also generates any integer
(integer * *)

;; generates integers greater than or equal to 5
(integer 5)

;; generates integers less than or equal to 10
(integer * 10)

;; generates integers between -3 and 4
(integer -3 4)
```

The `real` generator works similarly.

In addition to the constraints you choose in the type specifier, the absolute values of generated numbers are also bounded by the parameter `*size*`.

### Character Generators

The `character` generator accepts a syntax much like the number generators. It accepts either characters or character codes as bounding values:

```lisp
;; any character
(character)

;; any character, again
(character * *)

(character #\a #\f)

;; same as previous
(character 97 102)
```

Two additional character generators are predefined for common cases: `alpha` for `#\a`-`#\z` and `#\A`-`#\Z`, and `alphanumeric` for `#\a`-`#\z`, `#\A`-`#\Z`, and `#\0`-`#\9`.

### List and Tuple Generators

The `list` generator generates a list of values each generated by its subgenerator. The generated list has a random length bounded by the parameter `*list-size*`.

```lisp
(let ((a-generator
       (generator (list (integer)))))
  (generate a-generator))
;; sample result: (9 -3 3 -2 3 10 6 -8 9 10)
```

You can also specify length bounds with `:min-length` and `:max-length`. `:max-length` provides a permanent bound, but `*list-size*` can change in certain situations described later.

```lisp
(generator (list (integer) :min-length 5 :max-length 10))
```

The `tuple` generator generates a list containing one result from each of its subgenerators in order, so unlike the `list` generator, its length is fixed:

```lisp
(let ((another-generator
       (generator (tuple (integer) (real)))))
  (generate another-generator))
;; sample result: (-6 -4.168296)
```

A special form of the `list` generator (equally length bounded by `*list-size*`) is the `str` generator to generate strings consisting of randomly chosen alphanumeric characters.

```lisp
(generate (generator (str)))
;; sample output: "2k464h72CZ4TE1KQ"
```

### Or Generator

The `or` generator randomly chooses one of its subgenerators and returns its result. It biases its choice in certain situations, to be described later.

```lisp
(generate (generator (or (integer) (real))))
;; sample result: -1.0087109
```

### Guard Generator

The `guard` generator generates a result from its subgenerator and checks whether the result passes the guard predicate. If not, it keeps generating new results until it finds a value that does.

```lisp
;; same range of values as (integer 5)
(guard (lambda (x) (>= x 5)) (integer))
```

### Struct Generator

The `struct` generator generates a struct of a given struct type with slots generated by the corresponding slot generators. So if you have a struct definition that looks like:

```lisp
(defstruct a-struct
  a-slot
  another-slot)
```

You can create a generator type spec that looks like:

```lisp
(struct a-struct :a-slot (integer) :another-slot (real))
```

In order to use this kind of generator, the struct type must have a default constructor function. Of course, you can always use simple generators to specify all the atomic data elements you need and manually assemble them into more complex data structures at the start of your test body. However, for complex, nested generator forms, it may be difficult or impossible to unambiguously specify where to find the output of some deep sub-sub-generator that you wish to transform. For more those situations, you need...

### Mapped Generators

A mapped generator applies a transformation to their subgenerator. The transformation function should not mutate its argument.

```lisp
(let ((g (generator (tuple (map (lambda (x) (list x x x)) (integer 3 20))
                           (map (lambda (x) (list x x)) (integer 3 20))))))
  (generate g))
;;; sample result: ((8 8 8) (4 4))
```

### Chained Generators

You can create generators whose behavior is parameterized by other generators with `chain`. Every time a value is created from a chained generator,

1. Each parameterizing generator generates a new value,
2. Those values are bound to the associated variable names in the body of the `chain` form,
3. The body is evaluated to construct a new parameterized generator, and
4. The parameterized generator generates the end value.

For example, you can generate random NxN matrices like this:

```lisp
(let ((g (generator
          (chain ((n (integer 1 5)))
            (generator (list (list (integer) :min-length n :max-length n)
                             :min-length n :max-length n))))))
  (generate g))
;; sample result: ((-2 0 -9 -6) (6 7 -3 -7) (9 -10 10 6) (-6 -10 -10 8))
```

The `generator` macro is implicitly wrapped around each parameterizer form for brevity. However, the body is regular Lisp code, so you must use `generator` to define your parameterized generator there.

If you provide only a name for a parameterizer instead of a list containing a name and a generator form, the name is assumed to be a variable already bound to a generator.

```lisp
(let ((x (generator (integer 1 3)))
      (y (generator (integer 8 10))))
  (generator (chain (x y)
               (list (integer) :min-length x :max-length y))))
```

### User-Defined Generators

You can define your own generator types with `defgenerator`. User-defined generator types can be recursive. Here's a useless example:

```lisp
;; same range of values as (integer)
(defgenerator recursive () `(generator (or (integer) (recursive))))
```

With naive generation strategies, recursive generators can easily generate values of unbounded size. There are currently two ways to dampen the feedback explosion of recursively-generated data structures.

When a user-defined generator appears as an alternative in an `or` generator, its relative probability of being chosen decreases with each recursive descent.

```lisp
;; normally there would be an 87.5% chance of recursing on each generation
;; essentially guaranteeing unbounded growth
(defgenerator tuple-explode ()
  `(generator (tuple (or (integer) (tuple-explode))
                     (or (integer) (tuple-explode))
                     (or (integer) (tuple-explode)))))

(let ((*recursive-bias-decay* 1.2)
      (*bias-sensitivity* 1.5))
  (generate (generator (tuple-explode))))
;; sample result: ((((-6 ((9 -10 2) 8 -7) 8) 9 9) 8 2) 7 5)
```

The change in bias at each recursive step is controlled by the parameter `*recursive-bias-decay*`, and the way biases of different alternatives interact to produce the actual relative probabilities is controlled by `*bias-sensitivity*`. The whole apparatus is set up in such a way that these parameters can be tuned without causing one alternative's probability to sharply shoot off toward zero or one, so you can play around with them and discover values that produce a reasonable distribution for your needs.

Additionally, the maximum possible list length is reduced with every `list` generation that occurs within the dynamic scope of another `list` generation.

```lisp
(defgenerator list-explode ()
  `(generator (or (integer) (list (list-explode)))))

(let ((*list-size* 10))
  (generate (generator (list-explode))))
;; sample result:
;; ((((-5 -9 (-7 -4)) NIL NIL (6) NIL) (-7 (3 (-4 (NIL)))) (5 -3) 3
;;   ((1 (4) 5) NIL ((NIL) -10)) -5 9)
;;  ((((-3) -6) NIL -4 (-5) -4)) ((-9) 0 1 0 3) -3 NIL)
```

But it's your responsibility not to write type specs that can't possibly generate anything other than unbounded values.

```lisp
(defgenerator inherently-unbounded ()
  `(generator (tuple (integer) (inherently-unbounded))))
```

## Checking

Use the `check-it` macro to perform a test run. Here's another useless example:

```lisp
(let ((*num-trials* 100))
  (check-it (generator (integer))
            (lambda (x) (integerp x))))
```

This will generate `*num-trials*` random values and test them against the test predicate. If a random value fails, check-it will search for values of smaller complexity until it finds the least complex value it can that fails the test while respecting the generator's composite type spec, and print a failure description to `*check-it-output*`.

`check-it` itself returns `t` if every trial passed and `nil` if one failed, so you can embed `check-it` forms within whatever test framework you're already using. Here's what a check-it test might look like using Stefil:

```lisp
(deftest useless-test ()
  (let ((*num-trials* 100))
    (is (check-it (generator (integer))
                  (lambda (x) (integerp x))))))
```

### Regression Cases

You can configure the `check-it` macro to automatically add new deterministic regression tests to your project when a randomized test fails. Here's the worst example yet:

```lisp
(deftest some-test-with-regression-cases ()
  (is
   (check-it (generator (struct a-struct
                                :a-slot (integer)
                                :another-slot (integer)))
             (lambda (x) (= (slot-value x 'a-slot) 0))
             :regression-id some-test-with-regression-cases
             :regression-file my-regression-test-file)))
```

This will (most likely) discover a failing case, shrink it, and append the following code to `my-regression-test-file`:

```lisp
(REGRESSION-CASE SOME-TEST-WITH-REGRESSION-CASES "#S(A-STRUCT :A-SLOT 1 :ANOTHER-SLOT 0)")
```

Regression cases must be given a `regression-id` to know which `check-it` form they apply to. It's probably simplest just to tag them with the same symbol you used to name the test. (This makes your test code a little more redundant, but it keeps check-it simple and framework-agnostic.) Although generators are meant to generate data with readable print representations, some objects (like structs) cannot be dumped into FASL files, so regression cases are encoded as strings which are read after the file is loaded in order to delay their construction.

It is recommended that you output your regression cases to a special file that initially contains only the line `(in-package :same-package-as-test-package)` and is loaded as part of the same system as your tests. Then the next time you run a check-it test, all the regression cases will be checked before any random cases are generated, which should hopefully light a fire under your ass to fix bugs you've already discovered before you hunt for new ones.

Eventually, check-it will be integrated more elegantly with individual test frameworks by extending their APIs. For now, you can reduce code repetition by declaring a file to output regression cases for all `regression-id`s in a given package with `(register-package-regression-file :package file)`, so you don't have to mention the file in each test.

You can also provide a list of explicit inline examples that will also be checked before any random cases are generated using the `:examples` keyword argument to `check-it`.

## Shrinking

When a generated value fails a test, check-it searches for a simpler value as follows:

An `integer` generator shrinks toward zero, or if zero is not included in its range, toward the bound in its type spec that has the smaller absolute value.

A `real` generator doesn't shrink, because the shrinkage procedure only really makes sense over discrete search spaces.

A `list` generator shrinks by repeatedly removing elements from the list and/or shrinking individual elements while respecting the type spec of the subgenerator.

An `or` generator shrinks by shrinking the one generator among its alternatives that was involved in generating the failed value. However, if this generator's value cannot be shrunk further, and other alternatives available to the `or` generator specify constant values, then one of those constant values may be substituted instead. So if the generator `(or (integer 5) :a-keyword)` failed after generating 5 from the `integer` subgenerator, check-it will attempt to use `:a-keyword`. This opens up more possible shrinkage paths in the overall space defined by the `or` generator's parent generators.

Other generators shrink by recursively shrinking their subgenerators while still respecting the overall type spec. For mapped generators, the subgenerator is shrunk and its result is tested with the mapping reapplied. For chained generators, the parameterizers are not involved in the shrinking process; the parameterized generator is shrunk according to the parameters it had when it failed the test.
