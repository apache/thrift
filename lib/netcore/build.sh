#!/usr/bin/env bash

#exit if any command fails
#set -e

dotnet --info

dotnet restore

# dotnet test ./test/TEST_PROJECT_NAME -c Release -f netcoreapp1.0

# Instead, run directly with mono for the full .net version 
dotnet build **/*/project.json -r win10-x64 
dotnet build **/*/project.json -r osx.10.11-x64 
dotnet build **/*/project.json -r ubuntu.16.04-x64 

#revision=${TRAVIS_JOB_ID:=1}  
#revision=$(printf "%04d" $revision) 

#dotnet pack ./src/PROJECT_NAME -c Release -o ./artifacts --version-suffix=$revision  