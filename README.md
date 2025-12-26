<h1 align="center">
    cliq
</h1>

<p align="center">
    <strong>Dynamically typed, functional-ish language for fun</strong>
</p>

<p align="center">
    With inspiration from Perl, Clojure, Nim, and OCaml
</p>

- **Simple.** Clean, straightforward syntax that's easy to read without needing to understand many concepts.
- **Expressive.** Code should more closely favor _what_ it's doing, over _how_ it's doing it.
- **Friendly.** Feedback should come in the form of helpful, friendly error messages.

cliq files have the file extension `.clq`, and they look like this:

```
defn rpn(xs) =
	let
		interpret = \s, o =>
			if (o == "+")
				s.dropLast(2) : s[-2] + s[-1]
			else if (o == "-")
				s.dropLast(2) : s[-2] - s[-1]
			else if (o == "*")
				s.dropLast(2) : s[-2] * s[-1]
			else if (o == "/")
				s.dropLast(2) : s[-2] / s[-1]
			else
				s : o
	in
		xs.reduce(interpret, [])

let tokens = [4, 1, 2, "+", "*", 3, "/", 2, "-"] in
say rpn(tokens).first()
# 2

```

There's not much documentation yet. For now, take a tour of the language in [tour.clq](tour.clq)

## Planned work

- Missing language features. There's still quite a few things, including syntax changes for things I've grown to dislike.
- A better repl. Multi-line support, history, introspection functions, etc. I like to use repl's a lot.
- Bytecode VM. At the moment it's a tree-walk interpreter, which is serving me fine for now. But eventually I'd like to implement a bytecode VM.
- Integration tests. Before working on the bytecode VM, I'll need a fairly extensive set of tests, so that I can test the compiler easily.
- Neovim plugin. Neovim is my editor of choice, so this would be nice, even if it's just for syntax highligting.
- Code formatter. Feels like a fun problem to solve.

[DESIGN.md](DESIGN.md) also has a list of design decisions, and planned changes to syntax and features, but it's fairly raw and not written for others, so beware.

## The name

cliq is named for my wife Cherie, and for my daughters Lucy and Quinn. The language should be
expressive, emotive, cheeky, and fun — just like them. You should be able to write programs fast,
because per Cherie's family's mantra: "a quick game's a good game".
