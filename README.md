# Experimenting with the `Progress` datatype

Results:

```
                 -O0                   -O2
                 top-lazy  top-strict  top-lazy  top-strict

david            ~150M     --          --        --
david-separate   ~150M     --          --        --
lazylet          ~150M     --          --        --

david2           ~600M     ~600M       ~600M     ~600M
david2-separate  ~600M     ~600M       ~600M     ~600M
explicit         ~600M     ~600M       ~600M     ~600M
explicit_inlined ~150M     --          --        --

lazyst           ~150M     --          --        -- 
```

with `--` indicating constant space.

Tentative conclusion: the lazy ST solution is not _necessary_; when writing the
code in a very careful way the selector thunk optimization can achieve the same
result. However, it is extremely sensitive to inlining and the exact nature of
the memory layout, and is therefore very brittle. We can also look at it
another way: the progress stuff is designed to be able to say "if you evaluate
this bit overhere, also evaluate that bit overthere"; that's also what the
selector thunk optimization attempts: it says "if you evaluate this `fst (x, y)`
overhere, also evaluate that `snd (x, y)` bit overthere".
