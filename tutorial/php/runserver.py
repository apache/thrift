#!/usr/bin/env python3

#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

import os
import shutil
import sys

# PHP may name a command on PATH (php8.3) or a path. Resolve it before the
# chdir below, which moves into a directory that has a php/ subdirectory.
php_command = os.environ.get('PHP') or 'php'
php = shutil.which(php_command)
if not php:
    sys.exit('%s not found; set the PHP environment variable to a php command or path' % php_command)
php = os.path.abspath(php)

# Serve the tutorial directory with PHP's built-in web server, so that
# PhpClient.php --http finds the server at http://localhost:8080/php/PhpServer.php.
os.chdir(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))
os.execv(php, [php, '-S', 'localhost:8080', '-t', '.'])
