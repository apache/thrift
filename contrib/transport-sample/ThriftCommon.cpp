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

// ThriftCommon.cpp : Common functions for sample Thrift client and server
//

#include "ThriftCommon.h"

namespace thriftcommon
{
	//----------------------------------------------------------------------------
	//Launch child process and pass R/W anonymous pipe handles on cmd line.
	//This is a simple example and does not include elevation or other
	//advanced features.
	//
	bool LaunchAnonPipeChild(std::string app, boost::shared_ptr<TServerTransport> transport)
	{
#ifdef _WIN32
		PROCESS_INFORMATION pi;
		STARTUPINFOA si;
		GetStartupInfoA(&si);  //set startupinfo for the spawned process
		char handles[MAX_PATH];  //Stores pipe handles converted to text

		sprintf(handles, "%s %d %d", app.c_str(),
			(int)boost::shared_dynamic_cast<TPipeServer>(transport)->getClientRdPipeHandle(),
			(int)boost::shared_dynamic_cast<TPipeServer>(transport)->getClientWrtPipeHandle());

		//spawn the child process
		if (!CreateProcessA(nullptr, handles, nullptr,nullptr,TRUE,0,nullptr,nullptr,&si,&pi))
		{
			TOutput::instance().perror("TPipeServer CreateProcess failed, GLE=", GetLastError());
			return false;
		}

		CloseHandle(pi.hThread);
		CloseHandle(pi.hProcess);
#endif
		return true;
	}
}
