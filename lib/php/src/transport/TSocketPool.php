<?php

/** Inherits from Socket */
require_once $GLOBALS['THRIFT_ROOT'].'/transport/TSocket.php';

/**
 * This library makes use of APC cache to make hosts as down in a web
 * environment. If you are running from the CLI or on a system without APC
 * installed, then these null functions will step in and act like cache
 * misses.
 */
if (!function_exists('apc_fetch')) {
  function apc_fetch($key) { return FALSE; }
  function apc_store($key, $var, $ttl=0) { return FALSE; }
}

/**
 * Sockets implementation of the TTransport interface that allows connection
 * to a pool of servers.
 *
 * @package thrift.transport
 * @author Mark Slee <mcslee@facebook.com>
 */
class TSocketPool extends TSocket {

  /**
   * Remote hostname
   * 
   * @var array
   */
  private $hosts_ = array('localhost');

  /**
   * Remote ports
   * 
   * @var array
   */
  private $ports_ = array('9090');

  /**
   * How many times to retry each host in connect
   *
   * @var int
   */
  private $numRetries_ = 1;

  /**
   * Retry interval in seconds, how long to not try a host if it has been
   * marked as down.
   *
   * @var int
   */
  private $retryInterval_ = 60;

  /**
   * Max consecutive failures before marking a host down.
   *
   * @var int
   */
  private $maxConsecutiveFailures_ = 1;

  /**
   * Try hosts in order? or Randomized?
   *
   * @var bool
   */
  private $randomize_ = TRUE;

  /**
   * Always try last host, even if marked down?
   *
   * @var bool
   */
  private $alwaysTryLast_ = TRUE;

  /**
   * Socket pool constructor
   *
   * @param array  $hosts   List of remote hostnames
   * @param mixed  $ports   Array of remote ports, or a single common port
   * @param bool   $persist Whether to use a persistent socket
   */
  public function __construct($hosts=array('localhost'),
                              $ports=array(9090),
                              $persist=FALSE) {
    parent::__construct(null, 0, $persist);
    $this->hosts_ = $hosts;

    // Ports may be an array or a single port
    if (is_array($ports)) {
      $this->ports_ = $ports;
    } else {
      $this->ports_ = array();
      $num = count($hosts);
      for ($i = 0; $i < $num; ++$i) {
        $this->ports_ []= $ports;
      }
    }
  }

  /**
   * Sets how many time to keep retrying a host in the connect function.
   *
   * @param int $numRetries
   */
  public function setNumRetries($numRetries) {
    $this->numRetries_ = $numRetries;
  }

  /**
   * Sets how long to wait until retrying a host if it was marked down
   *
   * @param int $numRetries
   */
  public function setRetryInterval($retryInterval) {
    $this->retryInterval_ = $retryInterval;
  }

  /**
   * Sets how many time to keep retrying a host before marking it as down.
   *
   * @param int $numRetries
   */
  public function setMaxConsecutiveFailures($maxConsecutiveFailures) {
    $this->maxConsecutiveFailures_ = $maxConsecutiveFailures;
  }

  /**
   * Turns randomization in connect order on or off.
   *
   * @param bool $randomize
   */
  public function setRandomize($randomize) {
    $this->randomize_ = $randomize;
  }

  /**
   * Whether to always try the last server.
   *
   * @param bool $alwaysTryLast
   */
  public function setAlwaysTryLast($alwaysTryLast) {
    $this->alwaysTryLast_ = $alwaysTryLast;
  }


  /**
   * Connects the socket by iterating through all the servers in the pool
   * and trying to find one that works.
   */
  public function open() {
    $numServers = count($this->hosts_);

    // Check if a random server from the pool should be hit
    if ($this->randomize_) {
      $startingPoint = mt_rand(0, $numServers-1);
    } else {
      $startingPoint = 0;
    }
    $i = $startingPoint;

    do {     
      $host = $this->hosts_[$i];
      $port = $this->ports_[$i];

      // Check APC cache for a record of this server being down
      $failtimeKey = 'thrift_failtime:'.$host_.':'.$port.'~';

      // Cache miss? Assume it's OK
      $lastFailtime = apc_fetch($failtimeKey);
      if ($lastFailtime === FALSE) {
        $lastFailtime = 0;
      }

      $retryIntervalPassed = FALSE;

      // Cache hit...make sure enough the retry interval has elapsed
      if ($lastFailtime > 0) {
        $elapsed = time() - $lastFailtime;
        if ($elapsed > $retryInterval) {
          $retryIntervalPassed = TRUE;
          if ($this->debug_) {
            error_log('TSocketPool: retryInterval '.
                      '('.$this->retryInterval_.') '.
                      'has passed for host '.$host.':'.$port);
          }
        }
      }

      // Only connect if not in the middle of a fail interval, OR if this
      // is the LAST server we are trying, just hammer away on it
      $isLastServer = FALSE;
      if ($alwaysTryLast) {
        $isLastServer =
          ( (($i+1) % $numServers) == $startingPoint ) ? TRUE : FALSE;
      }

      if (($lastFailtime === 0) ||
          ($isLastServer) ||
          ($lastFailtime > 0 && $retryIntervalPassed)) {

        // Set underlying TSocket params to this one
        $this->host_ = $host;
        $this->port_ = $port;
          
        for ($attempt = 0; $attempt < $this->numRetries_; $attempt++) {
          try {
            parent::open();

            // Only clear the failure counts if required to do so
            if ($lastFailtime > 0) {
              apc_store($failtimeKey, 0);
            }
            // Successful connection, return now
            return;

          } catch (Exception $x) {
            // Connection failed
          }
        }

        // Mark failure of this host in the cache
        $consecfailsKey = 'thrift_consecfails:'.$host.':'.$port.'~';

        // Ignore cache misses
        $consecfails = apc_fetch($consecfailsKey);
        if ($consecfails === FALSE) {
          $consecfails = 0;
        }

        // Increment by one
        $consecfails++;

        // Log and cache this failure
        if ($consecfails >= $this->maxConsecutiveFailures_) {
          if ($this->debug_) {
            error_log('TSocketPool: marking '.$host.':'.$port.
                      ' as down for '.$this->retryInterval.' seconds '.
                      'after '.$consecfails.' failed connect attempts.');
          }
          // Store the failure time
          apc_store($failtimeKey, time());

          // Clear the count of consecutive failures
          apc_store($consecfailsKey, 0);
        } else {
          apc_store($consecfailsKey, $consecfails);
        }
      }
      $i = ($i + 1) % $numServers;

    } while ($i != $startingPoint);

    // Holy shit we failed them all. The system is totally ill!
    $error = 'TSocketPool: All hosts in pool are down. ';
    $hostlist = implode(',', $this->hosts_);
    $error .= '('.$hostlist.':'.$this->port_.')';
    if ($this->debug_) {
      error_log($error);
    }
    throw new Exception($error);
  }
}

?>
