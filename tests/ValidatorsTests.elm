module ValidatorsTests exposing (suite)

import Expect
import Fuzz exposing (Fuzzer, bool, constant, float, int, list, oneOf, string)
import Test exposing (..)
import Validation exposing (Field(..), Validity(..), field, preValidatedField)
import Validators exposing (..)


suite : Test
suite =
    describe "is now verifying the function"
        [ describe "isFloat, which"
            [ fuzz (oneOf [ Fuzz.map String.fromFloat float, string ]) "should return float validator result" <|
                \val ->
                    let
                        result =
                            isFloat "ERROR" val
                    in
                    case result of
                        Ok v ->
                            Expect.equal (Ok v) result

                        Err msg ->
                            Expect.equal "ERROR" msg
            ]
        , describe "isPositiveFloat, which"
            [ fuzz float "should return positive float validator result" <|
                \num ->
                    let
                        result =
                            num
                                |> String.fromFloat
                                |> isPositiveFloat "ERROR"
                    in
                    if 0 < num then
                        Expect.equal (Ok num) result

                    else
                        Expect.equal (Err "ERROR") result
            ]
        , describe "isInt, which"
            [ fuzz (oneOf [ Fuzz.map String.fromInt int, string ]) "should return int validator result" <|
                \val ->
                    let
                        result =
                            isInt "ERROR" val
                    in
                    case result of
                        Ok v ->
                            Expect.equal (Ok v) result

                        Err msg ->
                            Expect.equal "ERROR" msg
            ]
        , describe "isPositiveInt, which"
            [ fuzz int "should return positive int validator result" <|
                \num ->
                    let
                        result =
                            num
                                |> String.fromInt
                                |> isPositiveInt "ERROR"
                    in
                    if 0 < num then
                        Expect.equal (Ok num) result

                    else
                        Expect.equal (Err "ERROR") result
            ]
        , describe "isTrue, which"
            [ fuzz bool "should return true validator result" <|
                \bool ->
                    let
                        result =
                            isTrue "ERROR" bool
                    in
                    if bool then
                        Expect.equal (Ok True) result

                    else
                        Expect.equal (Err "ERROR") result
            ]
        , describe "isEqualTo, which"
            [ test "should return validator result" <|
                \_ ->
                    let
                        fields =
                            [ ( Field "10" NotValidated, 10, Ok 10 )
                            , ( Field "10" NotValidated, 5, Ok 5 )
                            , ( Field "10" (Valid 10), 10, Ok 10 )
                            , ( Field "10" (Valid 10), 5, Err "ERROR" )
                            , ( Field "10" (Invalid "invalid"), 10, Ok 10 )
                            , ( Field "10" (Invalid "invalid"), 5, Ok 5 )
                            ]

                        results =
                            fields
                                |> List.map (\( f, s, b ) -> isEqualTo f "ERROR" s == b)
                    in
                    Expect.true "Expected True." (List.foldl (&&) True results)
            ]
        , describe "isNotEmpty, which"
            [ fuzz string "should check if string is nonempty" <|
                \str ->
                    let
                        result =
                            isNotEmpty "ERROR" str
                    in
                    if String.isEmpty str then
                        Expect.equal (Err "ERROR") result

                    else
                        Expect.equal (Ok str) result
            ]
        , describe "isEmail, which"
            [ test "should validate email address" <|
                \_ ->
                    let
                        results =
                            List.map check emailExamples

                        check ( email, reason, valid_ ) =
                            if valid_ then
                                isEmail reason email

                            else
                                case isEmail "ERROR" email of
                                    Ok _ ->
                                        Err reason

                                    Err _ ->
                                        Ok email
                    in
                    Expect.ok (List.foldl resultAnd (Result.Ok "ok") results)
            ]
        , describe "isInList, which"
            [ fuzz2 float (list float) "should return validator result" <|
                \val list ->
                    let
                        result =
                            isInList "ERROR" ( val, list )
                    in
                    if List.member val list then
                        Expect.equal (Ok val) result

                    else
                        Expect.equal (Err "ERROR") result
            ]
        , describe "isValidField, which"
            [ test "should return bool" <|
                \_ ->
                    let
                        fields =
                            [ ( Field "10" NotValidated, False )
                            , ( Field "10" (Valid 10), True )
                            , ( Field "10" (Invalid "invalid"), False )
                            ]

                        results =
                            fields
                                |> List.map (\( f, b ) -> isValidField f == b)
                    in
                    Expect.true "Expected True." (List.foldl (&&) True results)
            ]
        ]


resultAnd : Result String a -> Result String a -> Result String a
resultAnd res2 res1 =
    case res1 of
        Ok val1 ->
            case res2 of
                Ok val2 ->
                    Ok val1

                Err err2 ->
                    Err err2

        Err err1 ->
            Err err1


valid : String -> String -> ( String, String, Bool )
valid validEmail reason =
    ( validEmail, reason, True )


invalid : String -> String -> ( String, String, Bool )
invalid invalidEmail reason =
    ( invalidEmail, reason, False )



-- copied from http://hackage.haskell.org/package/email-validate-2.3.2.8/src/tests/Main.hs
-- and https://code.iamcal.com/php/rfc822/tests
-- and subsequently modified


emailExamples : List ( String, String, Bool )
emailExamples =
    [ --valid "\r\n (\r\n x \r\n ) \r\n first\r\n ( \r\n x\r\n ) \r\n .\r\n ( \r\n x) \r\n last \r\n (  x \r\n ) \r\n @example.com" "id0:"
      valid "_somename@example.com" "id1:"
    , valid "_Yosemite.Sam@example.com" "id2:"
    , invalid "-- test --@example.com" "id3: No spaces allowed in local part"
    , invalid "-@..com" "id4: Double dot at domain"
    , invalid "-@a..com" "id5: Double dot at domain"
    , valid "!def!xyz%abc@example.com" "id6:"
    , invalid ".@" "id7:"

    -- , invalid ".@example.com" "id8: Phil Haack says so"
    -- , invalid ".dot@example.com" "id9: Doug Lovell says this should fail"
    -- , invalid ".first.last@example.com" "id10: Local part starts with a dot"
    , invalid "\"@example.com" "id11: Local part cannot end with a backslash"
    , invalid "()[]\\;:,><@example.com" "id12: Disallowed Characters"

    -- , valid "(foo)cal(bar)@(baz)iamcal.com(quux)" "id13: A valid address containing comments"
    , invalid "[test]@example.com" "id14: Square brackets only allowed within quotes"
    , valid "{_test_}@example.com" "id15:"
    , invalid "{^c\\@**Dog^}@cartoon.com" "id16: This is a throwaway example from Doug Lovell's article. Actually it's not a valid address."
    , invalid "@@bar.com" "id17:"
    , invalid "@example.com" "id18: No local part"
    , invalid "@NotAnEmail" "id19: Phil Haack says so"

    -- , valid "\"[[ test ]]\"@example.com" "id20:"
    , invalid "\"\"@example.com" "id21: Local part is effectively empty"
    , invalid "\"\"\"@example.com" "id22: Local part contains unescaped excluded characters"
    , invalid "\"\\\"@example.com" "id23: Local part cannot end with a backslash"

    -- , valid "\"Abc@def\"@example.com" "id24:"
    -- , valid "\"Abc\\@def\"@example.com" "id25:"
    -- , valid "\"Austin@Powers\"@example.com" "id26:"
    , invalid "\"Doug \"Ace\" L.\"@example.com" "id27: Doug Lovell says this should fail"

    -- , valid "\"Doug \\\"Ace\\\" L.\"@example.com" "id28:"
    -- , valid "\"first last\"@example.com" "id29: Contains quoted spaces"
    -- , valid "\"first..last\"@example.com" "id30: obs-local-part form as described in RFC 2822"
    -- , valid "\"first.middle.last\"@example.com" "id31: obs-local-part form as described in RFC 2822"
    -- , valid "\"first.middle\".\"last\"@example.com" "id32: obs-local-part form as described in RFC 2822"
    -- , valid "\"first(last)\"@example.com" "id33:"
    -- , valid "\"first@last\"@example.com" "id34: Contains quoted at-sign"
    -- , valid "\"first\".\"last\"@example.com" "id35:"
    -- , valid "\"first\".\"middle\".\"last\"@example.com" "id36: obs-local-part form as described in RFC 2822"
    -- , valid "\"first\".last@example.com" "id37: obs-local-part form as described in RFC 2822"
    -- , valid "\"first\".middle.\"last\"@example.com" "id38:"
    , invalid "\"first\"last\"@example.com" "id39: Local part contains unescaped excluded characters"

    -- , valid "\"first\\\"last\"@example.com" "id40: Contains quoted escaped quote"
    , invalid "\"first\\\\\"last\"@example.com" "id41: Contains an unescaped quote"

    -- , valid "\"first\\\\\\\"last\"@example.com" "id42:"
    -- , valid "\"first\\\\last\"@example.com" "id43: Contains quoted escaped backslash"
    -- , valid "\"first\\last\"@example.com" "id44: Any character can be escaped in a quoted string"
    , invalid "\"foo\"(yay)@(hoopla)[1.2.3.4]" "id45: Address literal can't be commented (RFC5321)"

    -- , valid "\"Fred Bloggs\"@example.com" "id46:"
    -- , valid "\"Fred\\ Bloggs\"@example.com" "id47:"
    -- , valid "\"hello my name is\"@stutter.com" "id48:"
    -- , valid "\"Ima Fool\"@example.com" "id49:"
    -- , valid "\"Ima.Fool\"@example.com" "id50:"
    -- , valid "\"Joe.\\\\Blow\"@example.com" "id51:"
    -- , valid "\"Joe\\\\Blow\"@example.com" "id52:"
    -- , valid "\"null \\\u{0000}\"@char.com" "id54: can have escaped null character"
    , invalid "\"null \u{0000}\"@char.com" "id56: cannot have unescaped null character"
    , invalid "\"qu@example.com" "id57: Doug Lovell says this should fail"

    -- , valid "\"Test \\\"Fail\\\" Ing\"@example.com" "id58:"
    -- , valid "\"test.test\"@example.com" "id59:"
    -- , valid "\"test@test\"@example.com" "id60:"
    , invalid "\"test\"blah\"@example.com" "id61: Phil Haack says so"
    , invalid "\"test\"test\"@example.com" "id62: Quotes cannot be nested"

    -- , valid "\"test\\\"blah\"@example.com" "id63:"
    -- , valid "\"test\\\\blah\"@example.com" "id64:"
    , invalid "\"test\\\u{000D}\n blah\"@example.com" "id65: Folding white space can't appear within a quoted pair"

    -- , valid "\"test\\\rblah\"@example.com" "id66: Quoted string specifically excludes carriage returns unless escaped"
    -- , valid "\"test\\blah\"@example.com" "id67: Any character can be escaped in a quoted string"
    -- , valid "\"test\r\n blah\"@example.com" "id68: This is a valid quoted string with folding white space"
    , invalid "\"test\u{000D}blah\"@example.com" "id69: Quoted string specifically excludes carriage returns"
    , valid "+@b.c" "id70: TLDs can be any length"
    , valid "+@b.com" "id71:"
    , valid "+1~1+@example.com" "id72:"
    , valid "~@example.com" "id73:"
    , valid "$A12345@example.com" "id74:"

    -- , valid "1234   @   local(blah)  .machine .example" "id75: Canonical example from RFC5322"
    , valid "1234567890@example.com" "id76:"
    , valid "123456789012345678901234567890123456789012345678901234567890@12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.1.example.com" "id77: Entire address is 254 characters"

    -- , invalid "123456789012345678901234567890123456789012345678901234567890@12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.12.example.com" "id78: Entire address is longer than 254 characters (this is 255)"
    -- , invalid "123456789012345678901234567890123456789012345678901234567890@12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.123.example.com" "id79: Entire address is longer than 254 characters (this is 256)"
    -- , invalid "123456789012345678901234567890123456789012345678901234567890@12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.1234.example.com" "id80: Entire address is longer than 254 characters (this is 257)"
    , valid "1234567890123456789012345678901234567890123456789012345678901234@example.com" "id81:"

    -- , invalid "12345678901234567890123456789012345678901234567890123456789012345@example.com" "id82: Local part more than 64 characters"
    , valid "a-b@bar.com" "id83:"
    , invalid "a(a(b(c)d(e(f))g)(h(i)j)@example.com" "id84: Braces are not properly matched"

    -- , valid "a(a(b(c)d(e(f))g)h(i)j)@example.com" "id85:"
    , invalid "a@-b.com" "id86: Domain can't start with hyphen"
    , valid "a@b" "id87:"
    , invalid "a@b-.com" "id88: Domain label can't end with hyphen"
    , valid "a@b.co-foo.uk" "id89:"
    , valid "a@bar" "id90:"
    , valid "a@bar.com" "id91:"

    -- , valid "a@bar.com." "id92:"
    , invalid "aaa.com" "id93:"
    , invalid "aaa@.123" "id94:"
    , invalid "aaa@.com" "id95:"

    -- , valid "aaa@[123.123.123.123]" "id96:"
    , invalid "aaa@[123.123.123.123]a" "id97: extra data outside ip"
    , invalid "aaa@[123.123.123.333]" "id98: not a valid IP"
    , invalid "abc@def@example.com" "id99: Doug Lovell says this should fail"
    , invalid "abc\\@def@example.com" "id100: This example from RFC3696 was corrected in an erratum"
    , invalid "Abc\\@def@example.com" "id101: Was incorrectly given as a valid address in the original RFC3696"
    , invalid "abc\\@example.com" "id102: Doug Lovell says this should fail"
    , invalid "abc\\\\@def@example.com" "id103: Doug Lovell says this should fail"
    , invalid "abc\\\\@example.com" "id104: This example from RFC3696 was corrected in an erratum"

    -- , valid "c@(Chris's host.)public.example" "id105: Canonical example from RFC5322"
    , invalid "cal(foo(bar)@iamcal.com" "id106: Unclosed parenthesis in comment"
    , invalid "cal(foo)bar)@iamcal.com" "id107: Too many closing parentheses"
    , invalid "cal(foo\\)@iamcal.com" "id108: Backslash at end of comment has nothing to escape"

    -- , valid "cal(foo\\)bar)@iamcal.com" "id109: A valid address containing comments and an escaped parenthesis"
    -- , valid "cal(foo\\@bar)@iamcal.com" "id110: A valid address containing comments"
    -- , valid "cal(woo(yay)hoopla)@iamcal.com" "id111: A valid address containing comments"
    -- , valid "cal@iamcal(woo).(yay)com" "id112: A valid address containing comments"
    , valid "customer/department@example.com" "id113:"
    , valid "customer/department=shipping@example.com" "id114:"
    , valid "dclo@us.ibm.com" "id115:"
    , invalid "doug@" "id116: No domain part"
    , invalid "Doug\\ \\\"Ace\\\"\\ L\\.@example.com" "id117: Doug Lovell says this should fail"
    , invalid "Doug\\ \\\"Ace\\\"\\ Lovell@example.com" "id118: Escaping can only happen in a quoted string"

    -- , invalid "first..last@example.com" "id119: Local part has consecutive dots"
    -- , valid "first.(\")middle.last(\")@example.com" "id120: Comment can contain a quote"
    -- , valid "first.(\r\n middle\r\n )last@example.com" "id121: Comment with folding white space"
    , invalid "first.\"\".last@example.com" "id122: Contains a zero-length element"

    -- , valid "first.\"last\"@example.com" "id123: obs-local-part form as described in RFC 2822"
    -- , valid "first.\"mid\\dle\".\"last\"@example.com" "id124: Backslash can escape anything but must escape something"
    , invalid "first.last" "id125: No @"

    -- , invalid "first.last.@example.com" "id126: Local part ends with a dot"
    , invalid "first.last@" "id127: No domain"
    , invalid "first.last@[::12.34.56.78]" "id128: IPv6 tag is missing"
    , invalid "first.last@[.12.34.56.78]" "id129: Only char that can precede IPv4 address is ':'"

    -- , valid "first.last@[12.34.56.78]" "id130: IP address"
    , invalid "first.last@[12.34.56.789]" "id131: Can't be interpreted as IPv4 so IPv6 tag is missing"
    , invalid "first.last@[IPv5:::12.34.56.78]" "id132: IPv6 tag is wrong"
    , invalid "first.last@[IPv6::::]" "id133: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6::::11.22.33.44]" "id134: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6::::b3:b4]" "id135: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6::::b4]" "id136: IPv6 authority is RFC 4291"

    -- , valid "first.last@[IPv6:::]" "id137: IPv6 authority is RFC 4291"
    -- , valid "first.last@[IPv6:::1111:2222:3333:4444:5555:6666]" "id138:"
    -- , valid "first.last@[IPv6:::12.34.56.78]" "id139: IPv6 address"
    -- , valid "first.last@[IPv6:::a2:a3:a4:b1:b2:b3:b4]" "id140: :: only elides one zero group (IPv6 authority is RFC 4291)"
    -- , valid "first.last@[IPv6:::a2:a3:a4:b1:ffff:11.22.33.44]" "id141: :: only elides one zero group (IPv6 authority is RFC 4291)"
    -- , valid "first.last@[IPv6:::a3:a4:b1:ffff:11.22.33.44]" "id142: IPv6 authority is RFC 4291"
    -- , valid "first.last@[IPv6:::b3:b4]" "id143: IPv6 authority is RFC 4291"
    -- , valid "first.last@[IPv6:::b4]" "id144: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6::]" "id145: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6::11.22.33.44]" "id146: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6::a2::b4]" "id147: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6::a2:a3:a4:b1:b2:b3:b4]" "id148: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6::b3:b4]" "id149: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6::b4]" "id150: IPv6 authority is RFC 4291"

    -- , valid "first.last@[IPv6:0123:4567:89ab:cdef::]" "id151: IPv6 authority is RFC 4291"
    -- , valid "first.last@[IPv6:0123:4567:89ab:CDEF::]" "id152: IPv6 authority is RFC 4291"
    -- , valid "first.last@[IPv6:0123:4567:89ab:cdef::11.22.33.44]" "id153: IPv6 authority is RFC 4291"
    -- , valid "first.last@[IPv6:0123:4567:89ab:CDEF::11.22.33.44]" "id154: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6:0123:4567:89ab:cdef::11.22.33.xx]" "id155: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6:0123:4567:89ab:CDEFF::11.22.33.44]" "id156: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6:1111:2222::3333::4444:5555:6666]" "id157: Too many '::' (can be none or one)"

    -- , valid "first.last@[IPv6:1111:2222:3333::4444:12.34.56.78]" "id158:"
    , invalid "first.last@[IPv6:1111:2222:3333::4444:5555:12.34.56.78]" "id159: Too many IPv6 groups (4 max)"
    , invalid "first.last@[IPv6:1111:2222:3333::4444:5555:6666:7777]" "id160: Too many IPv6 groups (6 max)"

    -- , valid "first.last@[IPv6:1111:2222:3333::4444:5555:6666]" "id161:"
    , invalid "first.last@[IPv6:1111:2222:3333:4444:5555:12.34.56.78]" "id162: Not enough IPv6 groups"

    -- , valid "first.last@[IPv6:1111:2222:3333:4444:5555:6666::]" "id163:"
    -- , valid "first.last@[IPv6:1111:2222:3333:4444:5555:6666:12.34.56.78]" "id164:"
    , invalid "first.last@[IPv6:1111:2222:3333:4444:5555:6666:12.34.567.89]" "id165: IPv4 part contains an invalid octet"
    , invalid "first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777:12.34.56.78]" "id166: Too many IPv6 groups (6 max)"
    , invalid "first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777:8888:9999]" "id167: Too many IPv6 groups (8 max)"

    -- , valid "first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777:8888]" "id168:"
    , invalid "first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777]" "id169: Not enough IPv6 groups"
    , invalid "first.last@[IPv6:1111:2222:33333::4444:5555]" "id170: 33333 is not a valid group in an IPv6 address"
    , invalid "first.last@[IPv6:1111:2222:333x::4444:5555]" "id171: x is not valid in an IPv6 address"
    , invalid "first.last@[IPv6:a1:::]" "id172: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6:a1:::11.22.33.44]" "id173: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6:a1:::b4]" "id174: IPv6 authority is RFC 4291"

    -- , valid "first.last@[IPv6:a1::]" "id175: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6:a1::11.22.33.44.55]" "id176: IPv6 authority is RFC 4291"

    -- , valid "first.last@[IPv6:a1::11.22.33.44]" "id177: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6:a1::11.22.33]" "id178: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6:a1::a4:b1::b4:11.22.33.44]" "id179: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6:a1::b2::11.22.33.44]" "id180: IPv6 authority is RFC 4291"

    -- , valid "first.last@[IPv6:a1::b2:11.22.33.44]" "id181: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6:a1::b211.22.33.44]" "id182: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6:a1::b3:]" "id183: IPv6 authority is RFC 4291"

    -- , valid "first.last@[IPv6:a1::b4]" "id184: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6:a1:]" "id185: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6:a1:11.22.33.44]" "id186: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6:a1:a2:::]" "id187: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6:a1:a2:::11.22.33.44]" "id188: IPv6 authority is RFC 4291"

    -- , valid "first.last@[IPv6:a1:a2::]" "id189: IPv6 authority is RFC 4291"
    -- , valid "first.last@[IPv6:a1:a2::11.22.33.44]" "id190: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6:a1:a2:]" "id191: IPv6 authority is RFC 4291"

    -- , valid "first.last@[IPv6:a1:a2:a3:a4::11.22.33.44]" "id192: IPv6 authority is RFC 4291"
    , invalid "first.last@[IPv6:a1:a2:a3:a4::b1:b2:b3:b4]" "id193: IPv6 authority is RFC 4291"

    -- , valid "first.last@[IPv6:a1:a2:a3:a4:b1::11.22.33.44]" "id194: :: only elides one zero group (IPv6 authority is RFC 4291)"
    -- , valid "first.last@[IPv6:a1:a2:a3:a4:b1:b2:b3::]" "id195: :: only elides one zero group (IPv6 authority is RFC 4291)"
    , invalid "first.last@[IPv6:a1:a2:a3:a4:b1:b2:b3:]" "id196: IPv6 authority is RFC 4291"
    , valid "first.last@123.example.com" "id197:"
    , valid "first.last@1xample.com" "id198:"

    -- , invalid "first.last@com" "id199: Mail host must be second- or lower level"
    , invalid "first.last@e.-xample.com" "id200: Label can't begin with a hyphen"
    , invalid "first.last@exampl-.e.com" "id201: Label can't end with a hyphen"

    -- , invalid "first.last@example.123" "id202: TLD can't be all digits"
    , valid "first.last@example.com" "id203:"

    -- , valid "first.last@example.com." "id204: Dot allowed on end of domain"
    , invalid "first.last@sub.do,com" "id205: Mistyped comma instead of dot (replaces old #3 which was the same as #57)"

    -- , valid "first.last@x(1234567890123456789012345678901234567890123456789012345678901234567890).com" "id206: Label is longer than 63 octets, but not with comment removed"
    , valid "first.last@x23456789012345678901234567890123456789012345678901234567890123.example.com" "id207:"
    , invalid "first.last@x234567890123456789012345678901234567890123456789012345678901234.example.com" "id208: Label can't be longer than 63 octets"

    -- , valid "first().last@example.com" "id209: A valid address containing an empty comment"
    , invalid "first(12345678901234567890123456789012345678901234567890)last@(1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890)example.com" "id210: Too long with comments, not too long without"

    -- , valid "first(a\"bc.def).last@example.com" "id211: Comment can contain double quote"
    -- , valid "first(abc.def).last@example.com" "id212: Comment can contain a dot"
    , invalid "first(abc(\"def\".ghi).mno)middle(abc(\"def\".ghi).mno).last@(abc(\"def\".ghi).mno)example(abc(\"def\".ghi).mno).(abc(\"def\".ghi).mno)com(abc(\"def\".ghi).mno)" "id213: Can't have comments or white space except at an element boundary"

    -- , valid "first(abc\\(def)@example.com" "id214: Comment can contain quoted-pair"
    , invalid "first(middle)last@example.com" "id215: Can't have a comment or white space except at an element boundary"

    -- , valid "first(Welcome to\r\n the (\"wonderful\" (!)) world\r\n of email)@example.com" "id216: Silly example from my blog post"
    , invalid "first\\@last@example.com" "id217: Escaping can only happen within a quoted string"
    , invalid "first\\\\@last@example.com" "id218: Local part contains unescaped excluded characters"
    , invalid "first\\last@example.com" "id219: Unquoted string must be an atom"
    , invalid "foo@[\\1.2.3.4]" "id220: RFC 5321 specifies the syntax for address-literal and does not allow escaping"

    -- , invalid "foobar@192.168.0.1" "id221: ip need to be []"
    , invalid "Fred\\ Bloggs@example.com" "id222: Was incorrectly given as a valid address in the original RFC3696"

    -- , valid "gatsby@f.sc.ot.t.f.i.tzg.era.l.d." "id223:"
    , invalid "hello world@example.com" "id224: Doug Lovell says this should fail"

    -- , valid "HM2Kinsists@(that comments are allowed)this.is.ok" "id225:"
    , invalid "Ima Fool@example.com" "id226: Phil Haack says so"
    , valid "Ima.Fool@example.com" "id227:"
    , invalid "Invalid \\\n Folding \\\n Whitespace@example.com" "id228: This isn't FWS so Dominic Sayers says it's invalid"
    , invalid "invalid@special.museum-" "id229: domain can't end with hyphen"

    -- , valid "jdoe@machine(comment).  example" "id230: Canonical example from RFC5322"
    , invalid "Joe.\\\\Blow@example.com" "id231: Was incorrectly given as a valid address in the original RFC3696"
    , invalid "local@exam_ple.com" "id232: Underscore not permitted in domain"
    , invalid "NotAnEmail" "id233: Phil Haack says so"
    , invalid "ote\"@example.com" "id234: Doug Lovell says this should fail"

    -- , valid "pete(his account)@silly.test(his host)" "id235: Canonical example from RFC5322"
    , invalid "phil.h\\@\\@ck@haacked.com" "id236: Escaping can only happen in a quoted string"
    , valid "shaitan@my-domain.thisisminekthx" "id237: Disagree with Paul Gregg here"
    , valid "t*est@example.com" "id238:"
    , valid "test-test@example.com" "id239:"

    -- , valid "test. \r\n \r\n obs@syntax.com" "id240: obs-fws allows multiple lines (test 2: space before break)"
    -- , invalid "test.@example.com" "id241: Doug Lovell says this should fail"
    -- , valid "test.\"test\"@example.com" "id242: Obsolete form, but documented in RFC2822"
    -- , valid "test.\r\n \r\n obs@syntax.com" "id243: obs-fws allows multiple lines"
    -- , valid "Test.\r\n Folding.\r\n Whitespace@example.com" "id244:"
    , invalid "test.\u{000D}\n\u{000D}\n obs@syntax.com" "id245: obs-fws must have at least one WSP per line"
    , invalid "test.example.com" "id246:"
    , valid "test.test@example.com" "id247:"
    , invalid "test@." "id248: Dave Child says so"
    , invalid "test@...........com" "id249: ......"
    , invalid "test@.org" "id250:"
    , invalid "test@[123.123.123.123" "id251: Dave Child says so"

    -- , valid "test@[123.123.123.123]" "id252:"
    , invalid "test@@example.com" "id253: Double @"

    -- , invalid "test@123.123.123.123" "id254: Top Level Domain won't be all-numeric (see RFC3696 Section 2). I disagree with Dave Child on this one."
    , invalid "test@123.123.123.123]" "id255: Dave Child says so"
    , valid "test@123.123.123.x123" "id256:"
    , invalid "test@123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012.com" "id257: 255 characters is maximum length for domain. This is 256."

    -- , invalid "test@example" "id258: Dave Child says so"
    , invalid "test@example." "id259: Dave Child says so"
    , valid "test@example.com" "id260:"
    , valid "TEST@example.com" "id261:"
    , valid "test@example.example.com" "id262:"
    , valid "test@example.example.example.com" "id263:"
    , invalid "test@test@example.com" "id264:"
    , valid "test+test@example.com" "id265:"
    , valid "user%uucp!path@somehost.edu" "id266:"
    , valid "user+mailbox@example.com" "id267:"
    , valid "valid@special.museum" "id268:"

    -- , invalid "x@x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456" "id269: Domain exceeds 255 chars"
    ]
