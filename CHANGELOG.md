# ChangeLog

2018-11-09
* deleted `isPositiveFloat, isPositiveInt` functions and added `isAtLeast, isAtMost, isGreaterThan, isLessThan, isRange` functions to Validators
* added `invalidate` function to Validation


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
