# To run

```
roc build --optimize ./main.roc
./brainroc ./helloworld.bf
```

# To debug

```bash
roc dev ./main.roc -- ./helloworld.bf
```

which will print `dbg` statements

# Problems

Simple programs like `helloworld.bf` and `print0to999.bf` run to completion.

But any longer programs result in a segfault.

When running in debug-mode, the state is printed after each instruction, which contains `iter` that counts for how many iterations the program has been running.
It seems like after 249 iterations, **regardless of which program is run(!)** the program exits. (249 on roc nightly, 244 on roc from-source, in both cases originating from commit 8d2bcf7)
Is this a stack-overflow?
If so, that's very weird since unless I'm mistaken,
both in the source and when looking at the IR, it seems like the program should tail-recurse correctly.

But maybe it might be the case that the 244 iterations limit only happens when running in `dev` mode and that the program stopping there
does not coincide with the segfault that happens when you do not?
