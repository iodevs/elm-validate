# elm-validate
This library helps with a validation of input forms and it's based on Enrico Buonanno lessons on [egghead.io](https://egghead.io/courses/form-validation-in-elm/).

## Install
* usual a way:
```
elm install iodevs/elm-validate
```


## Usage:
* for example as a validation of register / login /... form, see [example](https://github.com/iodevs/elm-validate/tree/master/example) directory in this repository or a live [demo](https://iodevs.github.io/elm-validate/)
* or as a validation of input forms for your applications, see below very simplified an example.

Let say, you have an employee form by which you can add new an employee to your system. You want to save his a photo, a name, an email and an age. The form responds to record `EmployeeForm` where for each input form is defined type `Fields`.

![](docs/employee-example.png)

For sending these data to server you'll need to define a record `Employee`:

```elm
type alias Employee =
    { name : String
    , email : String
    , age : Int
    , photo : ...
    }
```

and functions for transforming between these two records `formToEmployee`:

```elm
formToEmployee : EmployeeForm -> Result String Employee
formToEmployee form =
    let
        fieldOk =
            withField Ok

        valueOk =
            withoutField Ok
    in
    toModel
        Employee
        ( valueOk ( \item -> form.photo )
            >> fieldOk( \item -> form.name )
            >> fieldOk( \item -> form.email )
            >> fieldOk( \item -> form.age )
        )
        form
```

and `employeeToForm`:

```elm
employeeToForm : Employee -> EmployeeForm
employeeToForm employee =
    { name = preValidatedField employee.name
    , email = preValidatedField employee.email
    , age = preValidatedField employee.age
    , photo = ...
    }
```

Now if you have the record `Employee` it's enough to write a decoder:

```elm
decodeEmployee : Decoder Employee
decodeEmployee = ...
```

and an encoder for him:

```elm
encodeEmployee : Employee -> Value
encodeEmployee employee = ...
```

Last step before sending data is their validation by `OnSubmit`:

```elm
setValidityEmployeeForm : EmployeeForm -> EmployeeForm
setValidityEmployeeForm form =
    let
        name =
            form.name
                |> validate OnSubmit nameValidation

        email =
            form.email
                |> validate OnSubmit emailValidation

        age =
            form.age
                |> validate OnSubmit ageValidation

    in
        { form
            | name = name
            , email = email
            , age = age
        }
```

where particular validations can be defined as:

```elm
nameValidation : Validator String String
nameValidation =
    isNotEmpty "Name of employee is required."

emailValidation : Validator String String
emailValidation =
    composite
        (isNotEmpty "Email of employee is required.")
        (isEmail "Incorrect email.")

ageValidation : Validator String Int
ageValidation =
    isRange (isInt "Age must be integer number!") 10 100 "You cannot employ a person younger than 10 year old or elder 100!"
```

Of course, also these validations you'll use that same a way as in previous Register form example. A relevant parts (messages `InputEmail`, `BlurEmail`, etc.) are same or similar and omitted here.

Finally, somewhere inside `update` function will be:

```elm
SendEmplyeeToServer employeeForm ->
    let
        form =
            employeeForm
                |> setValidityEmployeeForm
    in
        employeeForm
            |> formToEmployee
            |> Result.map
                (\data ->
                    ( { model
                        | form = form
                      }
                    , Http.post
                        { url = ...
                        , body = Http.jsonBody (encodeEmployee data)
                        , ...
                        }
                    )
                )
            |> Result.withDefault
                ( model, Cmd.none )
```

where message `SendEmplyeeToServer` will be called by clicking "check" button at the employee form. Similarly for `GetEmplyeeFromServer` message somewhere else in your app:

```elm
GetEmplyeeFromServer (Ok data) ->
    data
        |> Decode.decodeValue decodeEmployee
        |> Result.map
            (\initData ->
                ( { model
                    | form = employeeToForm initData
                  }
                , Cmd.none
                )
            )
        |> Result.withDefault
            ( model, Cmd.none )

GetEmplyeeFromServer (Err err) ->
    ...
```


## ChangeLog
see [CHANGELOG](https://github.com/iodevs/elm-validate/blob/master/CHANGELOG)


## License
[![MIT](https://img.shields.io/packagist/l/doctrine/orm.svg)](https://github.com/iodevs/elm-validate/blob/master/LICENSE)
