dotnet --info

dotnet restore

dotnet build **/*/project.json -r win10-x64 
dotnet build **/*/project.json -r osx.10.11-x64 
dotnet build **/*/project.json -r ubuntu.16.04-x64 