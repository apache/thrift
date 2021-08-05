<!---
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-->

# AppVeyor Build

AppVeyor is capable of building MSVC as well as MSYS2, MinGW and Cygwin builds targeting the MS Windows platform. It has many versions of boost and python installed as well. See what appveyor has
[installed on build workers](https://www.appveyor.com/docs/installed-software/).

We run a matrix build on AppVeyor. See appveyor.yml for more details.
