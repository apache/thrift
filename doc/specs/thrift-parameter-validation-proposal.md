# Thrift Parameter Validation Proposal

> Version 1.1
>
> Dec 15, 2021
>
> duanyi.aster@bytedance.com, wangtieju@bytedance.com

### 1. Abstract
***
This document presents a proposed set of annotations to the Thrift IDL. The new annotations will supports parameter validation using build-in or third-party validators. The goal of this proposal is to define semantics and behavior of validation annotations, rather than to discuss their implementation.

### 2. Background
***
Parameter validation is a common need for web service. In the past, we usually write our validating logics after a RPC message deserialized by thrift. This ways works flexibly enough but restrict poorly: It is dangerous that service A and service B using the same IDL have two different validating rule, which often misdirects developers. Even if we extract our validating codes to a single module, simple and repeated work (ex. `if xx.Field1 > 1 then ...`) is really disturbing. If we can use build tool to generating codes for simple and unchangeable restraint, the web service will be more robust and developers will benefits from lighter work.
Compared to other IDL, the parameter validation gradually gets strong community supports like PGV ([protoc-gen-validate](https://github.com/envoyproxy/protoc-gen-validate)), benefiting from pb's strong plugin mechanism (lacking official plugin mechanism is one reason for we submit this proposal). Take a long-term view, auto-generated parameter validation may be a step towards code-less web service.

### 3. Proposal
***
This proposal includes three part: Validate Annotation Semantics, Validate Rule and Validate Feedback. The first declare how to write a validate annotation, the middle explain how every annotation should behave, the last introduces a mechanism of validating feedback.

#### 3.1 Validate Annotation Semantics
This semantics uses same rule of [Thrift IDL](https://thrift.apache.org/docs/idl). The validate option only works on struct fields, thus we must start from Field semantics part.
- Field 
```peg
Field <- FieldID? FieldReq? FieldType Identifier ('=' ConstValue)? ValidateAnnotations? ListSeparator?
```
- ValidateAnnotations
```peg
ValidateAnnotations <- '(' ValidateRule+ ListSeparator? ')'
```
- ValidateRule
```peg
ValidateRule <- ('validate' | 'vt') Validator+ = '"' ValidatingValue? '"'
```
- Validator

    Build-in validating logics. See [Supported Validator](#321-supported-validator) part.
```peg
Validator <- '.' Identifier
```
- ValidatingValue
```peg
ValidatingValue <- (ToolFunction '(' )? Arguments ')'?
```
- ToolFunction

    Build-in or user-defined tool functions. See [Tool Function](#325-tool-function) part.
```peg
ToolFunction <- '@' Identifier
```
- Arguments
```peg
Arguments <- (DynamicValue ListSeparator?)*
```
- DynamicValue
```peg
DynamicValue <- ConstValue | FieldReference
```
- FieldReference
  
    See [Field Reference](#324-field-reference) part.
```apache
FieldReference <- '$' ReferPath
ReferPath <- FieldName? ( ('['IntConstant']') | ('.'Identifier) )?
```
- All other semantics keep same with [standard definition](https://thrift.apache.org/docs/idl)

### 3.2 Validate Rule
The validate rule is works as a Boolean Expression, and Validator is core logic for one validate rule. Every Validator works like an Operator, calculating the Validating Value and Field Value, and then compare. For example, `gt` (greater than) will compare the right Validating Value with value of the field it belongs to, and return `true` if field value is greater than value or `false` if field value is not. We appoint that: Only if the validate rule returns true, the validated parameter is valid. If there are several validate rules defined in annotations of a field, Validator will take the logical relation as "and". Simply put, commas in annotations can be treated as "and".


#### 3.2.1 Supported Validator
Below lists the support validators. Value type means the type of validating value, field type means type of validated field.

| validator    | behavior                              | value type                           | field type             | secondary validator |
| ------------ | ------------------------------------- | ------------------------------------ | ---------------------- | ------------------- |
| const        | must be constant                      | string, bool                         | same with value        | -                   |
| defined_only | must be defined value                 | enum                                 | enum                   | -                   |
| not_nil      | must not be empty                     | "true"                               | any                    | -                   |
| skip         | skip validate                         | "true"                               | any                    | -                   |
| eq           | equals to (`==`)                      | i8, i16, i32, i64, f64, string, bool | same with value        | -                   |
| ne           | not equals to (`!=`)                  | i8, i16, i32, i64, f64, string, bool | same with value        | -                   |
| lt           | less than (`<`)                       | i8, i16, i32, i64, f64               | same with value        | -                   |
| le           | less equal (`<=`)                     | i8, i16, i32, i64, f64               | same with value        | -                   |
| gt           | greater than (`>`)                    | i8, i16, i32, i64, f64               | same with value        | -                   |
| ge           | greater equal (`>=`)                  | i8, i16, i32, i64, f64               | same with value        | -                   |
| in           | within given container                | i8, i16, i32, i64, f64, enum         | same with value        | -                   |
| not_in       | not within given container            | i8, i16, i32, i64, f64, enum         | same with value        | -                   |
| elem         | field's element constraint            | any                                  | list, set              | support             |
| key          | field's element key constraint        | any                                  | map                    | support             |
| value        | field's element value constraint      | any                                  | map                    | support             |
| min_size     | minimal length                        | i8, i16, i32, i64                    | string, list, set, map | -                   |
| max_size     | maximal length                        | i8, i16, i32, i64                    | string, list, set, map | -                   |
| prefix       | field prefix must be (case-sensitive) | string                               | string                 | -                   |
| suffix       | suffix must be (case-sensitive)       | string                               | string                 | -                   |
| contains     | must contain (case-sensitive)         | string                               | string                 | -                   |
| not_contains | must not contain (case-sensitive)     | string                               | string                 | -                   |
| pattern      | basic regular expression              | string                               | string                 | -                   |

- Basic Regular Expression (BRE), the syntax of BRE can be found in [manual](https://www.gnu.org/software/sed/manual/html_node/BRE-syntax.html) of GNU sed.
- Secondary validator (`elem`, `key` and `value`) is a successive validator, usually used at container-type field. See below Set/List/Map examples.
- Add suffix "_escape" to validators to prevent value of rule conflicting with tool function. For example, you can use `"vt.eq_escape" = "@len(A)"` to match literal `@len(A)`.

#### 3.2.2 IDL example
- Number
```
struct NumericDemo{
    1: double Value (validator.ge = "1000.1", validator.le = "10000.1")
    2: i8 Type (validator.in = "[1, 2, 4]")
}
```
- String/Binary
``` 
struct StringDemo{
    1: string Uninitialized (vt.const = "abc")
    2: string Name (vt.min_size = "6", vt.max_size = "12")
    3: string SomeStuffs (vt.pattern = "[0-9A-Za-z]+")
    4: string DebugInfo (vt.prefix = "[Debug]")
    5: string ErrorMessage (vt.contains = "Error")
}
```
- Bool
```
struct BoolDemo {
    1: bool AMD (vt.const = "true")
}
```
- Enum
```
enum Type {
    Bool
    I8
    I16
    I32
    I64
    String
    Struct
    List
    Set
    Map
}

struct EnumDemo {
    1: Type AddressType (vt.in = "[String]")
    2: Type ValueType (vt.defined_only = "true")
}
```
- Set/List
```
struct SetListDemo {
    1: list<string> Persons (vt.min_size = "5", vt.max_size = "10")
    2: set<double> HealthPoints (vt.elem.gt = "0")
}
```
- Map
```
struct MapDemo {
    1: map<i32, string> IdName (vt.min_size = "5", vt.max_size = "10")
    2: map<i32, double> Some (vt.key.gt = "0", vt.value.lt = "1000")
}
```

#### 3.2.3 Arguments
Arguments can by static literals or dynamic variables. If one literal expression contains any Field Reference or Tool Function, it becomes dynamic variables. Every dynamic variables finally get calculated and finally become a Thrift Constant Value.

#### 3.2.4 Field Reference
Field Reference is used to refer to another field's value in Validating Value, therefore user can compare more than one field. The referenced field must be within same struct. Identifier must be the field name referred.
- Field Reference Rule
1. `$x` represents a variable named x, and its scope is within current struct
2. `$` indicates the current field in which the validator is located
3. `$x['k']` indicates a reference to the key k of variable x (which must be map)
4. `$x[i]` indicates a reference to the i + 1 element of variable x (which must be list)
- Example
```
struct FieldReferenceExample {
    1: string A (vt.eq = "$B") //field A must equal to field B
    2: list<string> C
}
```

#### 3.2.5 Tool Function
Tool Function is use to enhance the operating of Validating Value. For example, if we want to ensure one field is larger than the length of string field A, we can use `len()` function: `vt.gt = "@len($A)"`. The arguments can be either literals or variables, and no size limit. However, we won't suggest any build-in function here, because the category is too big and always language-related. Instead, we only propose one mechanism for thrift developers to extends their implementation according to used language.

Supported functions:
| function | behavior                | arguments                           | results  | supported language |
| -------- | ----------------------- | ----------------------------------- | -------- | ------------------ |
| len      | the length of the field | 1: string, binary, list, set or map | 1. int64 | go                 |

### 3.3 Feedback
The generated validating codes should be included in struct's `Validate() TApplicationException` method. If all validate rule declared by one struct get passed, the struct's `Validate() TApplicationException` method returns nil (or just returns without exception, depending on specific language implementation); Otherwise it returns `TApplicationException` and report feedback message indicating failure reason. Due to language function implementations are different, we won't constrain the interface of feedback messages. However, by practice we suggest developers to give below three detail information:

- The position where first validating failure happens.
- The validator who reports the failure.
- The red-handed field value and validating value when the failure happens
