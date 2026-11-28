/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

//Missing definitions for *NIX systems. This sample project
//was initially created on Windows.

#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#define TEXT(str) str

inline int Sleep(int ms)
{
	return sleep(ms/1000); //sleep() param is in seconds
}

inline int _tcscmp(const char* str1, const char* str2)
{
	return strcmp(str1, str2);
}

inline int _tstoi(const char* str)
{
	return atoi(str);
}

