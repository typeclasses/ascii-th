### 1.2.0.1 (2023-06-26)

Raise language to GHC2021

### 1.2.0.0 (2023-01-06)

The constraints on `lower` and `upper` quasi-quotations in an expression context
are changes from `FromString` to `ToCasefulString 'LowerCase` and
`ToCasefulString 'UpperCase` respectively. This expands the range of types
inhabited by lower/uppercase quotes to include `ASCII'lower` and `ASCII'upper`,
which were previously not able to be expressed using quasi-quotations.

Additions to `ASCII.TemplateHaskell`:

```haskell
upperStringExp, lowerStringExp :: [CaselessChar] -> Q Exp
```

### 1.1.1.0 (2023-01-03)

New in `ASCII.QuasiQuoters`:

- `caseless`
- `upper`
- `lower`

New in `ASCII.TemplateHaskell`:

- `caselessListExp`
- `caselessListPat`
- `caselessIsStringPat`

### 1.1.0.0 (2023-01-03)

Change `ascii-superset` version from 1.0 to 1.1

The constraints on the quasi-quotes and template-haskell splices have changed
accordingly with the changes to the classes as of ascii-superset-1.1.

### 1.0.0.14 (2023-01-02)

Change test suite from `hedgehog` to `hspec`

### 1.0.0.13 (2023-01-02)

Minor Cabal correction (change `extra-doc-files` to `extra-source-files`)

### 1.0.0.12 (2022-12-30)

Metadata changes only

### 1.0.0.11 (2022-10-04)

Drop support for `base` 4.11 (GHC 8.4) and `base` 4.12 (GHC 8.6)

### 1.0.0.10 (2022-03-22)

Switch test-suite over to `hedgehog`

### 1.0.0.8 (2022-01-09)

Support GHC 9.2

### 1.0.0.6 (2021-09-26)

Add a test suite

### 1.0.0.4 (2021-02-10)

Support GHC 9.0

### 1.0.0.2 (2020-05-18)

Support GHC 8.10

### 1.0.0.0 (2020-05-05)

Initial release
