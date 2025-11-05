# Design stuff

This is a list of design decisions, things to implement, and any other random thoughts regarding the language design.

This is for me to consume, so there's very little context to anything.

## Semantic analysis

At the moment, referencing a variable that doesn't exist is a runtime error.

Whoops.

## Statements

Keep these as minimal as possible. I don't want to be adding tons of statements to the language.

## The more data types a function can operate on, the better

```
[1, 2, 3].reverse() == [3, 2, 1]
"abcdef".reverse() == "fedcba"
(1..10).length() == 10
"abcdef".length() == 6
{ "foo" => 1, "bar" => 2 }.length() == 2
```

## How can we have optional parameters for functions?

```
"numbers.txt".lines() == ["1", "2", "3"]
"numbers.txt".lines(parseNum) == [1, 2, 3]
foo.sort() and foo.sort(fn(a, b) => a <=> b)
"foo\tbar\nbaz quux".split()
```

## Array things

```
(1..10).partition(3) == [[1,2,3], [4,5,6], [7,8,9], [10]]
(1..5).reverse() == [5, 4, 3, 2, 1]
```

* Indexing
 - foo[0] ?
 - We have the `nth` function, but perhaps we need some sugar?
* Slicing
 - foo[0..9] ?
 - foo[..9] implies 0..9
 - foo[..-2] excludes the last 2 items

## Missing operators

* Power `**`

## String things

```
"foo\nbar\rbaz".splitLines() == ["foo", "bar", "baz"]
```

* Are strings made up of chars?
* Can we index strings?

## Non-decimal number expressions

```
0xffff
0b0000_1111
0o777
```

## Maps

```
{ "foo" => 10, "bar" => 20 }
```

* Indexing
  - `foo["key"]` ?
  - `foo.get("key")` ?

How should map/filter/reduce work on maps?

```
{ "foo" => 10 }.keys() == ["foo"]
{ "foo" => 10, "bar" => 10 }.values() == [10, 10]
{ "foo" => 10, "bar" => 20 }.select(["foo"]) == { "foo" => 10 }
```

assoc and dissoc?

## Sets

```
#{ :foo, :bar, :baz }
```

## IO

```
"foo.txt".spit("foo")
"foo.txt".spit(["line 1", "line 2"])
"foo.txt".append("foo")
"foo.txt".append(["foo", "bar"])
```

* `slurp()` to support URLs
* `system()` to shell out?
  - Is there something more ergonomic? But I'd like the params to be an array, rather than
    doing heaps of string manipulation.

## cond/when expressions

More expressive than long chains of `else if`

Maybe like Clojure's `cond`, or like Kotlin's `when`

```
when
    condition -> expr,
    condition -> expr,
    condition -> expr,
    else -> expr
```

Nice to have, but `else if` chains will do for now.

## Bitwise operators

```
~(a & b) == ~a | ~b
```

Implement them when I need them... it's not like I use them every day

## Regex support

```
if (foo =~ /^whatever$/)
    "it match :D"
else
    "it don't match :("
```

`foo !~ /^whatever$/` for negative matches

How to extract capture groups? Perl used $1, $2, etc. Not sure I want to introduce this idea? But maybe...

Any regex objects can be interned.

## Repl features

* Multi-line support. Too important not to have.
* Inspect currently defined vars
* View functions in scope
* Reference previous expression results

## String interpolation and escape chars

```
let thing = "bar" in "foo ${thing}"
say "foo\tbar"
```

One day...

## "Simple" pattern-matching

```
let [a, b] = [1, 2] in a + b
(1..10).zip(2..11).map(fn ([a, b]) => a + b)
let [a, b] = [1, 2, 10] in a + b == 3
let [_, b, _] = [1, 2, 10] in b == 2
```

Destructuring arrays is something I use _a lot_

## Change lambda syntax

Options:

```
\x, y => x + y
|x, y| x + y
```

I think I like the first one.

What does a function with no parameters look like?

```
\ => 10
\() => 10
|| 10
```

# Things that didn't make the cut

Here's a bunch of ideas that I had that didn't make the cut, and any notes why.

## Keywords

```
:foo-bar
```

Not now. I thought I really wanted these, but I just don't see the need for them yet.

## Remove commas from function parameters, function arguments, and from arrays

```
let add = fn(a b) => a + b in add(2 8)
let a = 1 in add((a + 1) 8)
map((fn(a) => a + 1) [1 2 3])
```

This example is kinda ugly. It's trying to be too much like a Lisp.

## Optional need for parentheses for function calls?

```
defn add(a, b) = a + b
add 5, 10 == 15  # what's the precedence here, though?
```

Like the idea, but no

## Use square brackets for function parameters

```
defn add [a, b] = a + b
map(fn [a] => a + 1, 1..3)
```
I like this more than `map(fn(a) => a + 1, 1..3)`

But we could also do something like `map(it + 1, 1..3)` although I don't love the `it` syntax that Scala, Kotlin, and Nim have

I've decided no, I don't like this

## Remove all the `=>` around the place

```
let inc = fn(a) a + b in inc(10)
def add = fn(a, b) a + b
```

For now, I don't mind using `=>`. I like the look of it. Not enough languages have fat commas.
