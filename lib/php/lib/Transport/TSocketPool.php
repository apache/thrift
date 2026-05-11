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
 * @package thrift.transport
 */

declare(strict_types=1);

namespace Thrift\Transport;

use Thrift\Exception\TException;

/**
 * Sockets implementation of the TTransport interface that allows connection
 * to a pool of servers.
 *
 * @package thrift.transport
 */
class TSocketPool extends TSocket
{
    /**
     * Remote servers. Array of associative arrays with 'host' and 'port' keys
     *
     * @var list<array{host: string, port: int}>
     */
    private array $servers = [];

    /**
     * How many times to retry each host in connect
     */
    private int $numRetries = 1;

    /**
     * Retry interval in seconds, how long to not try a host if it has been
     * marked as down.
     */
    private int $retryInterval = 60;

    /**
     * Max consecutive failures before marking a host down.
     */
    private int $maxConsecutiveFailures = 1;

    /**
     * Try hosts in order? or Randomized?
     */
    private bool $randomize = true;

    /**
     * Always try last host, even if marked down?
     */
    private bool $alwaysTryLast = true;

    /**
     * Use apcu cache
     */
    private bool $useApcuCache;

    /**
     * Socket pool constructor
     *
     * @param array $hosts List of remote hostnames
     * @param mixed $ports Array of remote ports, or a single common port
     * @param bool $persist Whether to use a persistent socket
     * @param mixed $debugHandler Function for error logging
     */
    public function __construct(
        $hosts = ['localhost'],
        $ports = [9090],
        $persist = false,
        $debugHandler = null
    ) {
        parent::__construct('', 0, $persist, $debugHandler);

        if (!is_array($ports)) {
            $port = $ports;
            $ports = [];
            foreach ($hosts as $key => $val) {
                $ports[$key] = $port;
            }
        }

        foreach ($hosts as $key => $host) {
            $this->addServer($host, $ports[$key]);
        }

        $this->useApcuCache = function_exists('apcu_fetch');
    }

    /**
     * Add a server to the pool
     *
     * This function does not prevent you from adding a duplicate server entry.
     *
     * @param string $host hostname or IP
     * @param int $port port
     */
    public function addServer(string $host, int $port)
    {
        $this->servers[] = ['host' => $host, 'port' => $port];
    }

    /**
     * Sets how many time to keep retrying a host in the connect function.
     *
     * @param int $numRetries
     */
    public function setNumRetries($numRetries)
    {
        $this->numRetries = $numRetries;
    }

    /**
     * Sets how long to wait until retrying a host if it was marked down
     *
     * @param int $numRetries
     */
    public function setRetryInterval($retryInterval)
    {
        $this->retryInterval = $retryInterval;
    }

    /**
     * Sets how many time to keep retrying a host before marking it as down.
     *
     * @param int $numRetries
     */
    public function setMaxConsecutiveFailures($maxConsecutiveFailures)
    {
        $this->maxConsecutiveFailures = $maxConsecutiveFailures;
    }

    /**
     * Turns randomization in connect order on or off.
     *
     * @param bool $randomize
     */
    public function setRandomize($randomize)
    {
        $this->randomize = $randomize;
    }

    /**
     * Whether to always try the last server.
     *
     * @param bool $alwaysTryLast
     */
    public function setAlwaysTryLast($alwaysTryLast)
    {
        $this->alwaysTryLast = $alwaysTryLast;
    }

    /**
     * Connects the socket by iterating through all the servers in the pool
     * and trying to find one that works.
     */
    public function open()
    {
        // Check if we want order randomization
        if ($this->randomize) {
            shuffle($this->servers);
        }

        // Count servers to identify the "last" one
        $numServers = count($this->servers);

        for ($i = 0; $i < $numServers; ++$i) {
            $host = $this->servers[$i]['host'];
            $port = $this->servers[$i]['port'];

            // Check APCu cache for a record of this server being down
            $failtimeKey = 'thrift_failtime:' . $host . ':' . $port . '~';

            // Cache miss? Assume it's OK
            $lastFailtime = $this->apcuFetch($failtimeKey);
            if ($lastFailtime === false) {
                $lastFailtime = 0;
            }

            $retryIntervalPassed = false;

            // Cache hit...make sure enough the retry interval has elapsed
            if ($lastFailtime > 0) {
                $elapsed = time() - $lastFailtime;
                if ($elapsed > $this->retryInterval) {
                    $retryIntervalPassed = true;
                    if ($this->debug) {
                        call_user_func(
                            $this->debugHandler,
                            'TSocketPool: retryInterval ' .
                            '(' . $this->retryInterval . ') ' .
                            'has passed for host ' . $host . ':' . $port
                        );
                    }
                }
            }

            // Only connect if not in the middle of a fail interval, OR if this
            // is the LAST server we are trying, just hammer away on it
            $isLastServer = false;
            if ($this->alwaysTryLast) {
                $isLastServer = ($i == ($numServers - 1));
            }

            if (
                ($lastFailtime === 0) ||
                ($isLastServer) ||
                ($lastFailtime > 0 && $retryIntervalPassed)
            ) {
                // Set underlying TSocket params to this one
                $this->host = $host;
                $this->port = $port;

                // Try up to numRetries_ connections per server
                for ($attempt = 0; $attempt < $this->numRetries; $attempt++) {
                    try {
                        // Use the underlying TSocket open function
                        parent::open();

                        // Only clear the failure counts if required to do so
                        if ($lastFailtime > 0) {
                            $this->apcuStore($failtimeKey, 0);
                        }

                        // Successful connection, return now
                        return;
                    } catch (TException $tx) {
                        // Connection failed
                    }
                }

                // Mark failure of this host in the cache
                $consecfailsKey = 'thrift_consecfails:' . $host . ':' . $port . '~';

                // Ignore cache misses
                $consecfails = $this->apcuFetch($consecfailsKey);
                if ($consecfails === false) {
                    $consecfails = 0;
                }

                // Increment by one
                $consecfails++;

                // Log and cache this failure
                if ($consecfails >= $this->maxConsecutiveFailures) {
                    if ($this->debug) {
                        call_user_func(
                            $this->debugHandler,
                            'TSocketPool: marking ' . $host . ':' . $port .
                            ' as down for ' . $this->retryInterval . ' secs ' .
                            'after ' . $consecfails . ' failed attempts.'
                        );
                    }
                    // Store the failure time
                    $this->apcuStore($failtimeKey, time());

                    // Clear the count of consecutive failures
                    $this->apcuStore($consecfailsKey, 0);
                } else {
                    $this->apcuStore($consecfailsKey, $consecfails);
                }
            }
        }

        // Oh no; we failed them all. The system is totally ill!
        $error = 'TSocketPool: All hosts in pool are down. ';
        $hosts = [];
        foreach ($this->servers as $server) {
            $hosts [] = $server['host'] . ':' . $server['port'];
        }
        $hostlist = implode(',', $hosts);
        $error .= '(' . $hostlist . ')';
        if ($this->debug) {
            call_user_func($this->debugHandler, $error);
        }
        throw new TException($error);
    }

    /**
     * This library makes use of APCu cache to make hosts as down in a web
     * environment. If you are running from the CLI or on a system without APCu
     * installed, then these null functions will step in and act like cache
     * misses.
     */
    private function apcuFetch($key, &$success = null)
    {
        return $this->useApcuCache ? apcu_fetch($key, $success) : false;
    }

    private function apcuStore($key, $var, $ttl = 0)
    {
        return $this->useApcuCache ? apcu_store($key, $var, $ttl) : false;
    }
}
