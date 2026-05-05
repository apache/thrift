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
 * @package thrift.base
 */

namespace Thrift\Base;

/**
 * Backwards-compatibility shim for THRIFT-5959.
 *
 * THRIFT-5959 dropped the trailing-underscore convention from instance
 * properties across the PHP runtime (e.g. `$host_` -> `$host`,
 * `$contextStack_` -> `$contextStack`). This trait lets downstream code
 * that still reads or writes the old names keep working while emitting
 * an E_USER_DEPRECATED notice. It is intended as a transitional aid and
 * may be removed in a future major release.
 *
 * Static properties (e.g. `$_TSPEC`) cannot be shimmed this way because
 * PHP has no `__getStatic` magic method, so consumers of those names
 * must migrate at the same time as the upgrade.
 */
trait TrailingUnderscorePropertyCompat
{
    public function __get(string $name)
    {
        $realName = self::resolveDeprecatedUnderscoreName($name);
        if ($realName !== null) {
            return $this->$realName;
        }
        @trigger_error(
            'Undefined property: ' . static::class . '::$' . $name,
            E_USER_NOTICE
        );
        return null;
    }

    public function __set(string $name, $value): void
    {
        $realName = self::resolveDeprecatedUnderscoreName($name);
        if ($realName !== null) {
            $this->$realName = $value;
            return;
        }
        $this->$name = $value;
    }

    public function __isset(string $name): bool
    {
        $realName = self::resolveDeprecatedUnderscoreName($name, false);
        if ($realName !== null) {
            return isset($this->$realName);
        }
        return false;
    }

    public function __unset(string $name): void
    {
        $realName = self::resolveDeprecatedUnderscoreName($name);
        if ($realName !== null) {
            unset($this->$realName);
        }
    }

    private static function resolveDeprecatedUnderscoreName(string $name, bool $warn = true): ?string
    {
        if ($name === '' || !str_ends_with($name, '_')) {
            return null;
        }
        $real = substr($name, 0, -1);
        if ($real === '' || !property_exists(static::class, $real)) {
            return null;
        }
        if ($warn) {
            @trigger_error(
                sprintf(
                    'Property %s::$%s is deprecated since THRIFT-5959, use $%s instead.',
                    static::class,
                    $name,
                    $real
                ),
                E_USER_DEPRECATED
            );
        }
        return $real;
    }
}
