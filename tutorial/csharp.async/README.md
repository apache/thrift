# Tutorial Framework 4.5+ Clent Async

## Step
1. "shared.thrift" file add namespace: namespace csharp tutorial
2. "tutorial.thrift" file add namespace: namespace csharp tutorial
3. thrift -gen csharp:async shared.thrift
4. thrift -gen csharp:async tutorial.thrift
5. copy generated directories for sample code to "Tutorial" folder
6. Find the AsyncProcessory method of the calculator file. 
7. Replace "shared.SharedService.Processor" with "shared.SharedService.AsyncProcessor"
8. generate && run

