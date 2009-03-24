/**
 * fb303.thrift
 *
 * Copyright (c) 2006- Facebook
 * Distributed under the Thrift Software License
 *
 * See accompanying file LICENSE or visit the Thrift site at:
 * http://developers.facebook.com/thrift/
 *
 *
 * Definition of common Facebook data types and status reporting mechanisms
 * common to all Facebook services. In some cases, these methods are
 * provided in the base implementation, and in other cases they simply define
 * methods that inheriting applications should implement (i.e. status report)
 *
 */

namespace java com.facebook.fb303
namespace cpp facebook.fb303

/**
 * Common status reporting mechanism across all services
 */
enum fb_status {
  DEAD = 0,
  STARTING = 1,
  ALIVE = 2,
  STOPPING = 3,
  STOPPED = 4,
  WARNING = 5,
}

/**
 * Standard base service
 */
service FacebookService {

  /**
   * Returns a descriptive name of the service
   */
  string getName(),

  /**
   * Returns the version of the service
   */
  string getVersion(),

  /**
   * Gets the status of this service
   */
  fb_status getStatus(),

  /**
   * User friendly description of status, such as why the service is in
   * the dead or warning state, or what is being started or stopped.
   */
  string getStatusDetails(),

  /**
   * Gets the counters for this service
   */
  map<string, i64> getCounters(),

  /**
   * Gets the value of a single counter
   */
  i64 getCounter(1: string key),

  /**
   * Sets an option
   */
  void setOption(1: string key, 2: string value),

  /**
   * Gets an option
   */
  string getOption(1: string key),

  /**
   * Gets all options
   */
  map<string, string> getOptions(),

  /**
   * Returns a CPU profile over the given time interval (client and server
   * must agree on the profile format).
   */
  string getCpuProfile(1: i32 profileDurationInSec),

  /**
   * Returns the unix time that the server has been running since
   */
  i64 aliveSince(),

  /**
   * Tell the server to reload its configuration, reopen log files, etc
   */
  oneway void reinitialize(),

  /**
   * Suggest a shutdown to the server
   */
  oneway void shutdown(),

}
