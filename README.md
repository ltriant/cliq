<h1 align="center">
    cliq
</h1>

<p align="center">
    <strong>Dynamically typed, functional-ish language for funsies</strong>
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

- Actually friendly error messages. The framework for okay-ish error messages is there. But I want it much friendlier.
- Missing language features. There's still quite a few things, including syntax changes for things I've started hating. No doubt I'll change more parts of the syntax...
- A better repl. Multi-line support, history, introspection functions, etc. I like to use repl's a lot.
- Bytecode VM. At the moment it's a tree-walk interpreter, which is fine for now. But eventually I'd like to implement a bytecode VM.
- Integration tests. There are no tests. I know, I know... I'll add contract tests. Before working on the bytecode VM, I'll need a fairly extensive set of tests, so that I can test the compiler easily.
- Neovim plugin. Neovim is my editor of choice, so this would be nice, even if it's just for syntax highligting.
- Code formatter. Feels like a fun problem to solve? I'm sure I'll come to regret this one.

[DESIGN.md](DESIGN.md) also has a list of design decisions, and planned changes to syntax and features, but it's fairly raw and not written for others, so beware.

## The name

cliq is named for my wife Cherie, and for my daughters Lucy and Quinn. The language should be
expressive, emotive, cheeky, and fun — just like them.

## Who's it for?

It's just for me, and for what I want in a language.
