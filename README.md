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

lazyst           ~150M     --          --        -- 
```

with `--` indicating constant space.
