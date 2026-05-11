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
 */

declare(strict_types=1);

namespace Test\Thrift\Unit\Lib;

trait ReflectionHelper
{
    /**
     * Get a reflection method and make it accessible if needed
     *
     * @param object|string $objectOrClass
     * @param string $methodName
     * @return \ReflectionMethod
     */
    protected function getAccessibleMethod($objectOrClass, string $methodName): \ReflectionMethod
    {
        $method = new \ReflectionMethod($objectOrClass, $methodName);

        // Only call setAccessible for PHP < 8.1.0
        if (PHP_VERSION_ID < 80100) {
            $method->setAccessible(true);
        }

        return $method;
    }

    /**
     * Get a reflection property and make it accessible if needed
     *
     * @param object|string $objectOrClass
     * @param string $propertyName
     * @return \ReflectionProperty
     */
    protected function getAccessibleProperty($objectOrClass, string $propertyName): \ReflectionProperty
    {
        $ref = new \ReflectionClass($objectOrClass);
        $property = $ref->getProperty($propertyName);

        // Only call setAccessible for PHP < 8.1.0
        if (PHP_VERSION_ID < 80100) {
            $property->setAccessible(true);
        }

        return $property;
    }

    /**
     * Get the value of a private/protected property
     *
     * @param object $object
     * @param string $propertyName
     * @return mixed
     */
    protected function getPropertyValue($object, string $propertyName)
    {
        $property = $this->getAccessibleProperty($object, $propertyName);

        return $property->getValue($object);
    }

    /**
     * Set the value of a private/protected property
     *
     * @param object $object
     * @param string $propertyName
     * @param mixed $value
     * @return void
     */
    protected function setPropertyValue($object, string $propertyName, $value): void
    {
        $property = $this->getAccessibleProperty($object, $propertyName);
        $property->setValue($object, $value);
    }
}
