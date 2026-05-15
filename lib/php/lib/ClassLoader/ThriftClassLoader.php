<?php

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
 *
 * ClassLoader to load Thrift library and definitions
 * Inspired from UniversalClassLoader from Symfony 2
 *
 * @package thrift.classloader
 */

declare(strict_types=1);

namespace Thrift\ClassLoader;

class ThriftClassLoader
{
    /**
     * Namespaces path
     *
     * @var array<string, list<string>>
     */
    protected array $namespaces = [];

    /**
     * Thrift definition paths
     *
     * @var array<string, list<string>>
     */
    protected array $definitions = [];

    /**
     * Set autoloader to use APCu cache
     */
    public function __construct(
        protected bool $apcu = false,
        protected ?string $apcu_prefix = null,
    ) {
    }

    /**
     * Registers a namespace.
     *
     * @param string|list<string> $paths The location(s) of the namespace
     */
    public function registerNamespace(string $namespace, string|array $paths): void
    {
        $this->namespaces[$namespace] = (array)$paths;
    }

    /**
     * Registers a Thrift definition namespace.
     *
     * @param string|list<string> $paths The location(s) of the definition namespace
     */
    public function registerDefinition(string $namespace, string|array $paths): void
    {
        $this->definitions[$namespace] = (array)$paths;
    }

    /**
     * Registers this instance as an autoloader.
     */
    public function register(bool $prepend = false): void
    {
        spl_autoload_register([$this, 'loadClass'], true, $prepend);
    }

    /**
     * Loads the given class, definition or interface.
     */
    public function loadClass(string $class): void
    {
        if (
            (true === $this->apcu && ($file = $this->findFileInApcu($class)))
            || ($file = $this->findFile($class))
        ) {
            require_once $file;
        }
    }

    /**
     * Loads the given class or interface in APCu.
     */
    protected function findFileInApcu(string $class): ?string
    {
        if (false === $file = apcu_fetch($this->apcu_prefix . $class)) {
            apcu_store($this->apcu_prefix . $class, $file = $this->findFile($class));
        }

        return $file !== false ? $file : null;
    }

    /**
     * Find class in namespaces or definitions directories
     */
    public function findFile(string $class): ?string
    {
        // Remove first backslash
        if ('\\' == $class[0]) {
            $class = substr($class, 1);
        }

        if (false !== $pos = strrpos($class, '\\')) {
            // Namespaced class name
            $namespace = substr($class, 0, $pos);

            // Iterate in normal namespaces
            foreach ($this->namespaces as $ns => $dirs) {
                //Don't interfere with other autoloaders
                if (0 !== strpos($namespace, $ns)) {
                    continue;
                }

                foreach ($dirs as $dir) {
                    $className = substr($class, $pos + 1);

                    $file = $dir . DIRECTORY_SEPARATOR .
                        str_replace('\\', DIRECTORY_SEPARATOR, $namespace) .
                        DIRECTORY_SEPARATOR .
                        $className . '.php';

                    if (file_exists($file)) {
                        return $file;
                    }
                }
            }

            // Iterate in Thrift namespaces

            // Remove first part of namespace
            $m = explode('\\', $class);

            // Ignore wrong call
            if (count($m) <= 1) {
                return null;
            }

            $class = array_pop($m);
            $namespace = implode('\\', $m);

            foreach ($this->definitions as $ns => $dirs) {
                //Don't interfere with other autoloaders
                if (0 !== strpos($namespace, $ns)) {
                    continue;
                }

                foreach ($dirs as $dir) {
                    /**
                     * Available in service: Interface, Client, Processor, Rest
                     * And every service methods (_.+)
                     */
                    if (
                        0 === preg_match('#(.+)(if|client|processor|rest)$#i', $class, $n)
                        && 0 === preg_match('#(.+)_[a-z0-9]+_(args|result)$#i', $class, $n)
                    ) {
                        $className = 'Types';
                    } else {
                        $className = $n[1];
                    }

                    $file = $dir . DIRECTORY_SEPARATOR .
                        str_replace('\\', DIRECTORY_SEPARATOR, $namespace) .
                        DIRECTORY_SEPARATOR .
                        $className . '.php';

                    if (file_exists($file)) {
                        return $file;
                    }
                }
            }
        }

        return null;
    }
}
