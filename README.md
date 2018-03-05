# elm-validate
This library helps with validation of input forms and it's based on Enrico Buonanno lessons on [egghead.io](https://egghead.io/courses/form-validation-in-elm).

Install package usually a way:
```
elm-package install iodevs/elm-validate
```


## Usage:
* see `example` directory in this repository
* or as live [demo](https://iodevs.github.io/elm-validate)


## Notes:
There is a couple of changes:
* for composing two Validators we use `(>&&)` instead of `(>=>)`
* we added a few functions: `isFloat`, `isPositiveFloat`, `isInList`, `isUrl`, `isValidField` and `preValidatedField`

* functions `isNatural` and `isPositiveInt` were refactored to `isPositiveInt`
* name of function `apply` we rewrote on `applyValidity`
