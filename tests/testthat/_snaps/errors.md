# parse errors carry the file, position and tokens

    Code
      cat(conditionMessage(err))
    Output
      run1.ctl:12:5: found `ELSEIF`, expected `ELSE` or `ENDIF`

# a parse error can explain itself instead of listing tokens

    Code
      cat(conditionMessage(err))
    Output
      run1.ctl:40:1: not translated: verbatim Fortran at line 40

# write errors carry the construct, location and formats

    Code
      cat(conditionMessage(err))
    Output
      Cannot write inter-occasion variability (at `random_effects$iov`). No supported format can hold it.

# engine errors carry the engine and its own error

    Code
      cat(conditionMessage(err))
    Output
      The mrgsolve engine failed: compilation failed

# an engine error without a parent says the engine is missing

    Code
      cat(conditionMessage(err))
    Output
      The rxode2 engine is not installed.

