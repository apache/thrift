// Licensed to the Apache Software Foundation(ASF) under one
// or more contributor license agreements.See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership.The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.

namespace Apache.Cassandra.Test
{
    /// <summary>
    ///     The ConsistencyLevel is an enum that controls both read and write
    ///     behavior based on the ReplicationFactor of the keyspace.  The
    ///     different consistency levels have different meanings, depending on
    ///     if you're doing a write or read operation.
    ///     If W + R > ReplicationFactor, where W is the number of nodes to
    ///     block for on write, and R the number to block for on reads, you
    ///     will have strongly consistent behavior; that is, readers will
    ///     always see the most recent write. Of these, the most interesting is
    ///     to do QUORUM reads and writes, which gives you consistency while
    ///     still allowing availability in the face of node failures up to half
    ///     of
    ///     <ReplicationFactor>
    ///         . Of course if latency is more important than
    ///         consistency then you can use lower values for either or both.
    ///         Some ConsistencyLevels (ONE, TWO, THREE) refer to a specific number
    ///         of replicas rather than a logical concept that adjusts
    ///         automatically with the replication factor.  Of these, only ONE is
    ///         commonly used; TWO and (even more rarely) THREE are only useful
    ///         when you care more about guaranteeing a certain level of
    ///         durability, than consistency.
    ///         Write consistency levels make the following guarantees before reporting success to the client:
    ///         ANY          Ensure that the write has been written once somewhere, including possibly being hinted in a
    ///         non-target node.
    ///         ONE          Ensure that the write has been written to at least 1 node's commit log and memory table
    ///         TWO          Ensure that the write has been written to at least 2 node's commit log and memory table
    ///         THREE        Ensure that the write has been written to at least 3 node's commit log and memory table
    ///         QUORUM       Ensure that the write has been written to
    ///         <ReplicationFactor>
    ///             / 2 + 1 nodes
    ///             LOCAL_QUORUM Ensure that the write has been written to
    ///             <ReplicationFactor>
    ///                 / 2 + 1 nodes, within the local datacenter (requires NetworkTopologyStrategy)
    ///                 EACH_QUORUM  Ensure that the write has been written to
    ///                 <ReplicationFactor>
    ///                     / 2 + 1 nodes in each datacenter (requires NetworkTopologyStrategy)
    ///                     ALL          Ensure that the write is written to <code>&lt;ReplicationFactor&gt;</code> nodes
    ///                     before responding to the client.
    ///                     Read consistency levels make the following guarantees before returning successful results to the
    ///                     client:
    ///                     ANY          Not supported. You probably want ONE instead.
    ///                     ONE          Returns the record obtained from a single replica.
    ///                     TWO          Returns the record with the most recent timestamp once two replicas have replied.
    ///                     THREE        Returns the record with the most recent timestamp once three replicas have replied.
    ///                     QUORUM       Returns the record with the most recent timestamp once a majority of replicas have
    ///                     replied.
    ///                     LOCAL_QUORUM Returns the record with the most recent timestamp once a majority of replicas within
    ///                     the local datacenter have replied.
    ///                     EACH_QUORUM  Returns the record with the most recent timestamp once a majority of replicas within
    ///                     each datacenter have replied.
    ///                     ALL          Returns the record with the most recent timestamp once all replicas have replied
    ///                     (implies no replica may be down)..
    /// </summary>
    public enum ConsistencyLevel
    {
        ONE = 1,
        QUORUM = 2,
        LOCAL_QUORUM = 3,
        EACH_QUORUM = 4,
        ALL = 5,
        ANY = 6,
        TWO = 7,
        THREE = 8,
    }
}