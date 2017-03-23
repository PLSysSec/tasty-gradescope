# tasty-gradescope

`tasty-gradescope` is a framework for creating Haskell assignment graders using
`tasty`. It outputs test results to a GradeScope compatible json file.

## Options

Unfortunately, *top level* option parameters do not work when specified inside
the test suite yet, so you must pass them in when you run the tester, e.g.

```
stack test --test-arguments "--scores results_hs.json --NegScoring True --TotalPoints 55"                                                   master ✭ ◼
```

the top level options are:

- `--scores <File.json>` : where to output the json file
- `--NegScoring <Bool>`: Use negative scoring (deduct from total) or not.
  Default: `false`
- `--TotalPoints <Int>`: Total score for use with neg scoring.


## Specifying points
You can locally alter the points for each test in the tester (default:
1), using the `scored` combinator:

```
scored 2 $ testProperty "padZero"    prop_padZero
```
