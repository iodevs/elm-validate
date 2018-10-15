# elm-validate
This library helps with validation of input forms and it's based on Enrico Buonanno lessons on [egghead.io](https://egghead.io/courses/form-validation-in-elm/).

Install package usually a way:
```
elm install iodevs/elm-validate
```


## Usage:
* see `example` directory in this repository
* or as live [demo](https://iodevs.github.io/elm-validate/)

## ChangeLog
2018-10-12
* `Validation.preValidatedField : (val -> String) -> val -> Field String val` (not `preValidatedField : val -> Field String val` anymore)
* `(|:)` deleted and replaced `applyValidity`
* `(>&&)` changed to `composite`

2018-02-12

There is a couple of changes:
* for composing two Validators we use `(>&&)` instead of `(>=>)`
* we added a few functions: `isFloat`, `isPositiveFloat`, `isInList`, `isUrl`, `isValidField` and `preValidatedField`
* functions `isNatural` and `isPositiveInt` were refactored to `isPositiveInt`
* name of function `apply` we rewrote on `applyValidity`
