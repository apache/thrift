Typical usage
=============
```
thrift.exe --audit <oldFile> <newFile>
```

Compatibility options
=====================
The audit remains strict by default. These options suppress specific audit
errors; callers are responsible for verifying that the selected change is safe
for every language binding in use.

* `--audit-allow-optional-field-removal` allows removal of fields declared
  `optional`. It does not allow removal of fields with default or required
  requiredness. Removing an explicitly optional field is wire-compatible.
* `--audit-allow-required-field-to-default` allows a field declared `required`
  to change to default requiredness. It does not allow changing that field to
  `optional`, or changing a default field to `required`. The option also applies
  to service method arguments. Explicit `required` is illegal in a `throws`
  clause and is normalized to default requiredness by the parser, so `throws`
  clauses are not affected by this option.

Changing `required` to default requiredness is binding- and application-
dependent, and is not universally wire-compatible. For example, the standard
C++ generator writes ordinary default-requiredness values, including default-
constructed strings, containers, and nested structs. In contrast, the Java
generator may omit a default-requiredness field when its value is `null`. An
older reader generated from an explicitly `required` field rejects that omitted
field. Other generators may behave differently, and C++ exception-typed fields
and generated result structs also have separate set-state handling.

Before using `--audit-allow-required-field-to-default`, verify the write
behavior of every language binding and field type in use.
Upgrade every reader away from explicit `required` before deploying any writer
that may omit the field. For a service method argument, an older server can
reject the request before invoking the handler.

Example run
===========
```
> thrift.exe --audit test.thrift break1.thrift
[Thrift Audit Failure:break1.thrift] New Thrift File has missing function base_function3
[Thrift Audit Warning:break1.thrift] Constant const3 has different value
```

Problems that the audit tool can catch
======================================
Errors
* Removing an enum value
* Changing the type of a struct field
* Changing the required-ness of a struct field (unless explicitly allowed)
* Removing a struct field (unless explicitly allowed)
* Adding a required struct field
* Adding a struct field 'in the middle'.  This usually indicates an old ID has been recycled
* Struct removed
* Oneway-ness change
* Return type change
* Missing function
* Missing service
* Change in service inheritance

Warnings
* Removing a language namespace declaration
* Changing a namespace
* Changing an enum value's name
* Removing an enum class
* Default value changed
* Struct field name change
* Removed constant
* Type of constant changed
* Value of constant changed
