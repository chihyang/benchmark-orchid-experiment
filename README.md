minikanren
==========

minikanren (logic programming language) for guile.

You need to point the GUILE_LOAD_PATH to this directory to load this code.

To test the cd into tests/ and make test.

To get started inside a guile repl load the module (use-modules (minikanren language))

Editing
=======

To get emacs to indent minikanren code correctly add to .emacs:

(put 'fresh 'scheme-indent-function 1)

Jason Experiments
=================

I put a default argument in the accumulator-passing `mapo` so that we
don’t need to edit code in the use-site to switch between them.

Now there are three easy flags for tweaking several different kinds of
experiments; these are in different files right now.

```
|                   | mapo/acc | mapo/rec-first | mapo/goal-first |
| no delay str cdrs |        6 |             22 | ~∞              |
| delay stream cdrs |       29 |             78 | ~∞              |
```

```
| streams.scm | (define DELAY-CDRS? #t)                |
| lists.scm   | (define MAPO-WITH-ACCUMULATOR? #t)     |
| lists.scm   | (define MAPO-WITH-RECURSION-FIRST? #f) |
```

1. It seems that delaying the `cdr`s *doesn’t* help.

2. But using that accumulator does work. 

What it’s doing is putting the goals `p` _last_, after working over
all of the lists. The lengths of the lists put a big constraint on the
problem. 

But that’s *not* the same as uniformly doing “left associating
conjunctions”. It *might* still be that there are problems where left
associating conjunctions make a big performance impact. But this isn’t
an example of left associating conjunctions. And *applying* actual
left associative conjunction to this example doesn’t make a big
impact. See the mkw-2020-paper repo for examples where I translated
this program over to our examples.

