# Thrift module: DocTest

Fixture for markdown generator @param/@return rendering tests. 

| Module | Services & Functions | Data types | Constants |
| --- | --- | --- | --- |
|DocTest|[DocTest](#service-doctest)|||
||    [ &bull; compute](#function-doctestcompute)|||
||    [ &bull; plain](#function-doctestplain)|||
||    [ &bull; withbare](#function-doctestwithbare)|||

***
## Services

### Service: DocTest
A service with documented functions. 
#### Function: DocTest.compute
Computes a value from two inputs.

| Name | Description |
| --- | --- |
| `i32 x` | the numeric input |
| `string label` | a label for the result |
| **Returns** `i32` | the computed result |



```i32```
 _compute_(```i32``` x,
```string``` label)


#### Function: DocTest.plain
Plain prose doc, no @param or @return tags. 

```void```
 _plain_(```i32``` x)


#### Function: DocTest.withbare
| Name | Description |
| --- | --- |
| `i32 bare` | no preceding prose |



```void```
 _withbare_(```i32``` bare)



