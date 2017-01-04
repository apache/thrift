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

using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Threading;
using System.Threading.Tasks;
using Thrift;
using Thrift.Collections;
using System.ServiceModel;
using System.Runtime.Serialization;
using Thrift.Protocols;
using Thrift.Protocols.Entities;
using Thrift.Protocols.Utilities;
using Thrift.Transports;
using Thrift.Transports.Client;
using Thrift.Transports.Server;


namespace Apache.Cassandra.Test
{
    public partial class Cassandra
    {
        [ServiceContract(Namespace = "")]
        public interface IAsync
        {
            [OperationContract]
            [FaultContract(typeof(AuthenticationExceptionFault))]
            [FaultContract(typeof(AuthorizationExceptionFault))]
            Task loginAsync(AuthenticationRequest auth_request, CancellationToken cancellationToken);

            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            Task set_keyspaceAsync(string keyspace, CancellationToken cancellationToken);

            /// <summary>
            ///     Get the Column or SuperColumn at the given column_path. If no value is present, NotFoundException is thrown. (This
            ///     is
            ///     the only method that can throw an exception under non-failure conditions.)
            /// </summary>
            /// <param name="key"></param>
            /// <param name="column_path"></param>
            /// <param name="consistency_level"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            [FaultContract(typeof(NotFoundExceptionFault))]
            [FaultContract(typeof(UnavailableExceptionFault))]
            [FaultContract(typeof(TimedOutExceptionFault))]
            Task<ColumnOrSuperColumn> @getAsync(byte[] key, ColumnPath column_path, ConsistencyLevel consistency_level,
                CancellationToken cancellationToken);

            /// <summary>
            ///     Get the group of columns contained by column_parent (either a ColumnFamily name or a ColumnFamily/SuperColumn name
            ///     pair) specified by the given SlicePredicate. If no matching values are found, an empty list is returned.
            /// </summary>
            /// <param name="key"></param>
            /// <param name="column_parent"></param>
            /// <param name="predicate"></param>
            /// <param name="consistency_level"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            [FaultContract(typeof(UnavailableExceptionFault))]
            [FaultContract(typeof(TimedOutExceptionFault))]
            Task<List<ColumnOrSuperColumn>> get_sliceAsync(byte[] key, ColumnParent column_parent,
                SlicePredicate predicate, ConsistencyLevel consistency_level, CancellationToken cancellationToken);

            /// <summary>
            ///     returns the number of columns matching <code>predicate</code> for a particular <code>key</code>,
            ///     <code>ColumnFamily</code> and optionally <code>SuperColumn</code>.
            /// </summary>
            /// <param name="key"></param>
            /// <param name="column_parent"></param>
            /// <param name="predicate"></param>
            /// <param name="consistency_level"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            [FaultContract(typeof(UnavailableExceptionFault))]
            [FaultContract(typeof(TimedOutExceptionFault))]
            Task<int> get_countAsync(byte[] key, ColumnParent column_parent, SlicePredicate predicate,
                ConsistencyLevel consistency_level, CancellationToken cancellationToken);

            /// <summary>
            ///     Performs a get_slice for column_parent and predicate for the given keys in parallel.
            /// </summary>
            /// <param name="keys"></param>
            /// <param name="column_parent"></param>
            /// <param name="predicate"></param>
            /// <param name="consistency_level"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            [FaultContract(typeof(UnavailableExceptionFault))]
            [FaultContract(typeof(TimedOutExceptionFault))]
            Task<Dictionary<byte[], List<ColumnOrSuperColumn>>> multiget_sliceAsync(List<byte[]> keys,
                ColumnParent column_parent, SlicePredicate predicate, ConsistencyLevel consistency_level,
                CancellationToken cancellationToken);

            /// <summary>
            ///     Perform a get_count in parallel on the given list<binary> keys. The return value maps keys to the count found.
            /// </summary>
            /// <param name="keys"></param>
            /// <param name="column_parent"></param>
            /// <param name="predicate"></param>
            /// <param name="consistency_level"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            [FaultContract(typeof(UnavailableExceptionFault))]
            [FaultContract(typeof(TimedOutExceptionFault))]
            Task<Dictionary<byte[], int>> multiget_countAsync(List<byte[]> keys, ColumnParent column_parent,
                SlicePredicate predicate, ConsistencyLevel consistency_level, CancellationToken cancellationToken);

            /// <summary>
            ///     returns a subset of columns for a contiguous range of keys.
            /// </summary>
            /// <param name="column_parent"></param>
            /// <param name="predicate"></param>
            /// <param name="range"></param>
            /// <param name="consistency_level"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            [FaultContract(typeof(UnavailableExceptionFault))]
            [FaultContract(typeof(TimedOutExceptionFault))]
            Task<List<KeySlice>> get_range_slicesAsync(ColumnParent column_parent, SlicePredicate predicate,
                KeyRange range, ConsistencyLevel consistency_level, CancellationToken cancellationToken);

            /// <summary>
            ///     Returns the subset of columns specified in SlicePredicate for the rows matching the IndexClause
            /// </summary>
            /// <param name="column_parent"></param>
            /// <param name="index_clause"></param>
            /// <param name="column_predicate"></param>
            /// <param name="consistency_level"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            [FaultContract(typeof(UnavailableExceptionFault))]
            [FaultContract(typeof(TimedOutExceptionFault))]
            Task<List<KeySlice>> get_indexed_slicesAsync(ColumnParent column_parent, IndexClause index_clause,
                SlicePredicate column_predicate, ConsistencyLevel consistency_level, CancellationToken cancellationToken);

            /// <summary>
            ///     Insert a Column at the given column_parent.column_family and optional column_parent.super_column.
            /// </summary>
            /// <param name="key"></param>
            /// <param name="column_parent"></param>
            /// <param name="column"></param>
            /// <param name="consistency_level"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            [FaultContract(typeof(UnavailableExceptionFault))]
            [FaultContract(typeof(TimedOutExceptionFault))]
            Task insertAsync(byte[] key, ColumnParent column_parent, Column column, ConsistencyLevel consistency_level,
                CancellationToken cancellationToken);

            /// <summary>
            ///     Increment or decrement a counter.
            /// </summary>
            /// <param name="key"></param>
            /// <param name="column_parent"></param>
            /// <param name="column"></param>
            /// <param name="consistency_level"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            [FaultContract(typeof(UnavailableExceptionFault))]
            [FaultContract(typeof(TimedOutExceptionFault))]
            Task @addAsync(byte[] key, ColumnParent column_parent, CounterColumn column,
                ConsistencyLevel consistency_level, CancellationToken cancellationToken);

            /// <summary>
            ///     Remove data from the row specified by key at the granularity specified by column_path, and the given timestamp.
            ///     Note
            ///     that all the values in column_path besides column_path.column_family are truly optional: you can remove the entire
            ///     row by just specifying the ColumnFamily, or you can remove a SuperColumn or a single Column by specifying those
            ///     levels too.
            /// </summary>
            /// <param name="key"></param>
            /// <param name="column_path"></param>
            /// <param name="timestamp"></param>
            /// <param name="consistency_level"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            [FaultContract(typeof(UnavailableExceptionFault))]
            [FaultContract(typeof(TimedOutExceptionFault))]
            Task @removeAsync(byte[] key, ColumnPath column_path, long timestamp, ConsistencyLevel consistency_level,
                CancellationToken cancellationToken);

            /// <summary>
            ///     Remove a counter at the specified location.
            ///     Note that counters have limited support for deletes: if you remove a counter, you must wait to issue any following
            ///     update
            ///     until the delete has reached all the nodes and all of them have been fully compacted.
            /// </summary>
            /// <param name="key"></param>
            /// <param name="path"></param>
            /// <param name="consistency_level"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            [FaultContract(typeof(UnavailableExceptionFault))]
            [FaultContract(typeof(TimedOutExceptionFault))]
            Task remove_counterAsync(byte[] key, ColumnPath path, ConsistencyLevel consistency_level,
                CancellationToken cancellationToken);

            /// <summary>
            ///     Mutate many columns or super columns for many row keys. See also: Mutation.
            ///     mutation_map maps key to column family to a list of Mutation objects to take place at that scope.
            ///     *
            /// </summary>
            /// <param name="mutation_map"></param>
            /// <param name="consistency_level"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            [FaultContract(typeof(UnavailableExceptionFault))]
            [FaultContract(typeof(TimedOutExceptionFault))]
            Task batch_mutateAsync(Dictionary<byte[], Dictionary<string, List<Mutation>>> mutation_map,
                ConsistencyLevel consistency_level, CancellationToken cancellationToken);

            /// <summary>
            ///     Truncate will mark and entire column family as deleted.
            ///     From the user's perspective a successful call to truncate will result complete data deletion from cfname.
            ///     Internally, however, disk space will not be immediatily released, as with all deletes in cassandra, this one
            ///     only marks the data as deleted.
            ///     The operation succeeds only if all hosts in the cluster at available and will throw an UnavailableException if
            ///     some hosts are down.
            /// </summary>
            /// <param name="cfname"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            [FaultContract(typeof(UnavailableExceptionFault))]
            [FaultContract(typeof(TimedOutExceptionFault))]
            Task truncateAsync(string cfname, CancellationToken cancellationToken);

            /// <summary>
            ///     for each schema version present in the cluster, returns a list of nodes at that version.
            ///     hosts that do not respond will be under the key DatabaseDescriptor.INITIAL_VERSION.
            ///     the cluster is all on the same version if the size of the map is 1.
            /// </summary>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            Task<Dictionary<string, List<string>>> describe_schema_versionsAsync(CancellationToken cancellationToken);

            /// <summary>
            ///     list the defined keyspaces in this cluster
            /// </summary>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            Task<List<KsDef>> describe_keyspacesAsync(CancellationToken cancellationToken);

            /// <summary>
            ///     get the cluster name
            /// </summary>
            [OperationContract]
            Task<string> describe_cluster_nameAsync(CancellationToken cancellationToken);

            /// <summary>
            ///     get the thrift api version
            /// </summary>
            [OperationContract]
            Task<string> describe_versionAsync(CancellationToken cancellationToken);

            /// <summary>
            ///     get the token ring: a map of ranges to host addresses,
            ///     represented as a set of TokenRange instead of a map from range
            ///     to list of endpoints, because you can't use Thrift structs as
            ///     map keys:
            ///     https://issues.apache.org/jira/browse/THRIFT-162
            ///     for the same reason, we can't return a set here, even though
            ///     order is neither important nor predictable.
            /// </summary>
            /// <param name="keyspace"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            Task<List<TokenRange>> describe_ringAsync(string keyspace, CancellationToken cancellationToken);

            /// <summary>
            ///     returns the partitioner used by this cluster
            /// </summary>
            [OperationContract]
            Task<string> describe_partitionerAsync(CancellationToken cancellationToken);

            /// <summary>
            ///     returns the snitch used by this cluster
            /// </summary>
            [OperationContract]
            Task<string> describe_snitchAsync(CancellationToken cancellationToken);

            /// <summary>
            ///     describe specified keyspace
            /// </summary>
            /// <param name="keyspace"></param>
            [OperationContract]
            [FaultContract(typeof(NotFoundExceptionFault))]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            Task<KsDef> describe_keyspaceAsync(string keyspace, CancellationToken cancellationToken);

            /// <summary>
            ///     experimental API for hadoop/parallel query support.
            ///     may change violently and without warning.
            ///     returns list of token strings such that first subrange is (list[0], list[1]],
            ///     next is (list[1], list[2]], etc.
            /// </summary>
            /// <param name="cfName"></param>
            /// <param name="start_token"></param>
            /// <param name="end_token"></param>
            /// <param name="keys_per_split"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            Task<List<string>> describe_splitsAsync(string cfName, string start_token, string end_token,
                int keys_per_split, CancellationToken cancellationToken);

            /// <summary>
            ///     adds a column family. returns the new schema id.
            /// </summary>
            /// <param name="cf_def"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            [FaultContract(typeof(SchemaDisagreementExceptionFault))]
            Task<string> system_add_column_familyAsync(CfDef cf_def, CancellationToken cancellationToken);

            /// <summary>
            ///     drops a column family. returns the new schema id.
            /// </summary>
            /// <param name="column_family"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            [FaultContract(typeof(SchemaDisagreementExceptionFault))]
            Task<string> system_drop_column_familyAsync(string column_family, CancellationToken cancellationToken);

            /// <summary>
            ///     adds a keyspace and any column families that are part of it. returns the new schema id.
            /// </summary>
            /// <param name="ks_def"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            [FaultContract(typeof(SchemaDisagreementExceptionFault))]
            Task<string> system_add_keyspaceAsync(KsDef ks_def, CancellationToken cancellationToken);

            /// <summary>
            ///     drops a keyspace and any column families that are part of it. returns the new schema id.
            /// </summary>
            /// <param name="keyspace"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            [FaultContract(typeof(SchemaDisagreementExceptionFault))]
            Task<string> system_drop_keyspaceAsync(string keyspace, CancellationToken cancellationToken);

            /// <summary>
            ///     updates properties of a keyspace. returns the new schema id.
            /// </summary>
            /// <param name="ks_def"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            [FaultContract(typeof(SchemaDisagreementExceptionFault))]
            Task<string> system_update_keyspaceAsync(KsDef ks_def, CancellationToken cancellationToken);

            /// <summary>
            ///     updates properties of a column family. returns the new schema id.
            /// </summary>
            /// <param name="cf_def"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            [FaultContract(typeof(SchemaDisagreementExceptionFault))]
            Task<string> system_update_column_familyAsync(CfDef cf_def, CancellationToken cancellationToken);

            /// <summary>
            ///     Executes a CQL (Cassandra Query Language) statement and returns a
            ///     CqlResult containing the results.
            /// </summary>
            /// <param name="query"></param>
            /// <param name="compression"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            [FaultContract(typeof(UnavailableExceptionFault))]
            [FaultContract(typeof(TimedOutExceptionFault))]
            [FaultContract(typeof(SchemaDisagreementExceptionFault))]
            Task<CqlResult> execute_cql_queryAsync(byte[] query, Compression compression,
                CancellationToken cancellationToken);

            /// <summary>
            ///     Prepare a CQL (Cassandra Query Language) statement by compiling and returning
            ///     - the type of CQL statement
            ///     - an id token of the compiled CQL stored on the server side.
            ///     - a count of the discovered bound markers in the statement
            /// </summary>
            /// <param name="query"></param>
            /// <param name="compression"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            Task<CqlPreparedResult> prepare_cql_queryAsync(byte[] query, Compression compression,
                CancellationToken cancellationToken);

            /// <summary>
            ///     Executes a prepared CQL (Cassandra Query Language) statement by passing an id token and  a list of variables
            ///     to bind and returns a CqlResult containing the results.
            /// </summary>
            /// <param name="itemId"></param>
            /// <param name="values"></param>
            [OperationContract]
            [FaultContract(typeof(InvalidRequestExceptionFault))]
            [FaultContract(typeof(UnavailableExceptionFault))]
            [FaultContract(typeof(TimedOutExceptionFault))]
            [FaultContract(typeof(SchemaDisagreementExceptionFault))]
            Task<CqlResult> execute_prepared_cql_queryAsync(int itemId, List<string> values,
                CancellationToken cancellationToken);
        }


        public class Client : TBaseClient, IDisposable, IAsync
        {
            public Client(TProtocol protocol) : this(protocol, protocol)
            {
            }

            public Client(TProtocol inputProtocol, TProtocol outputProtocol) : base(inputProtocol, outputProtocol)
            {
            }

            public async Task loginAsync(AuthenticationRequest auth_request, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("login", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new loginArgs();
                args.Auth_request = auth_request;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new loginResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.authnx)
                {
                    throw result.Authnx;
                }
                if (result.__isset.authzx)
                {
                    throw result.Authzx;
                }
                return;
            }

            public async Task set_keyspaceAsync(string keyspace, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("set_keyspace", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new set_keyspaceArgs();
                args.Keyspace = keyspace;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new set_keyspaceResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                return;
            }

            public async Task<ColumnOrSuperColumn> @getAsync(byte[] key, ColumnPath column_path,
                ConsistencyLevel consistency_level, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("get", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new getArgs();
                args.Key = key;
                args.Column_path = column_path;
                args.Consistency_level = consistency_level;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new getResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                if (result.__isset.nfe)
                {
                    throw result.Nfe;
                }
                if (result.__isset.ue)
                {
                    throw result.Ue;
                }
                if (result.__isset.te)
                {
                    throw result.Te;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "get failed: unknown result");
            }

            public async Task<List<ColumnOrSuperColumn>> get_sliceAsync(byte[] key, ColumnParent column_parent,
                SlicePredicate predicate, ConsistencyLevel consistency_level, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("get_slice", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new get_sliceArgs();
                args.Key = key;
                args.Column_parent = column_parent;
                args.Predicate = predicate;
                args.Consistency_level = consistency_level;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new get_sliceResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                if (result.__isset.ue)
                {
                    throw result.Ue;
                }
                if (result.__isset.te)
                {
                    throw result.Te;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "get_slice failed: unknown result");
            }

            public async Task<int> get_countAsync(byte[] key, ColumnParent column_parent, SlicePredicate predicate,
                ConsistencyLevel consistency_level, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("get_count", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new get_countArgs();
                args.Key = key;
                args.Column_parent = column_parent;
                args.Predicate = predicate;
                args.Consistency_level = consistency_level;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new get_countResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                if (result.__isset.ue)
                {
                    throw result.Ue;
                }
                if (result.__isset.te)
                {
                    throw result.Te;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "get_count failed: unknown result");
            }

            public async Task<Dictionary<byte[], List<ColumnOrSuperColumn>>> multiget_sliceAsync(List<byte[]> keys,
                ColumnParent column_parent, SlicePredicate predicate, ConsistencyLevel consistency_level,
                CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("multiget_slice", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new multiget_sliceArgs();
                args.Keys = keys;
                args.Column_parent = column_parent;
                args.Predicate = predicate;
                args.Consistency_level = consistency_level;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new multiget_sliceResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                if (result.__isset.ue)
                {
                    throw result.Ue;
                }
                if (result.__isset.te)
                {
                    throw result.Te;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "multiget_slice failed: unknown result");
            }

            public async Task<Dictionary<byte[], int>> multiget_countAsync(List<byte[]> keys, ColumnParent column_parent,
                SlicePredicate predicate, ConsistencyLevel consistency_level, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("multiget_count", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new multiget_countArgs();
                args.Keys = keys;
                args.Column_parent = column_parent;
                args.Predicate = predicate;
                args.Consistency_level = consistency_level;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new multiget_countResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                if (result.__isset.ue)
                {
                    throw result.Ue;
                }
                if (result.__isset.te)
                {
                    throw result.Te;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "multiget_count failed: unknown result");
            }

            public async Task<List<KeySlice>> get_range_slicesAsync(ColumnParent column_parent, SlicePredicate predicate,
                KeyRange range, ConsistencyLevel consistency_level, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("get_range_slices", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new get_range_slicesArgs();
                args.Column_parent = column_parent;
                args.Predicate = predicate;
                args.Range = range;
                args.Consistency_level = consistency_level;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new get_range_slicesResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                if (result.__isset.ue)
                {
                    throw result.Ue;
                }
                if (result.__isset.te)
                {
                    throw result.Te;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "get_range_slices failed: unknown result");
            }

            public async Task<List<KeySlice>> get_indexed_slicesAsync(ColumnParent column_parent,
                IndexClause index_clause, SlicePredicate column_predicate, ConsistencyLevel consistency_level,
                CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("get_indexed_slices", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new get_indexed_slicesArgs();
                args.Column_parent = column_parent;
                args.Index_clause = index_clause;
                args.Column_predicate = column_predicate;
                args.Consistency_level = consistency_level;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new get_indexed_slicesResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                if (result.__isset.ue)
                {
                    throw result.Ue;
                }
                if (result.__isset.te)
                {
                    throw result.Te;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "get_indexed_slices failed: unknown result");
            }

            public async Task insertAsync(byte[] key, ColumnParent column_parent, Column column,
                ConsistencyLevel consistency_level, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("insert", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new insertArgs();
                args.Key = key;
                args.Column_parent = column_parent;
                args.Column = column;
                args.Consistency_level = consistency_level;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new insertResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                if (result.__isset.ue)
                {
                    throw result.Ue;
                }
                if (result.__isset.te)
                {
                    throw result.Te;
                }
                return;
            }

            public async Task @addAsync(byte[] key, ColumnParent column_parent, CounterColumn column,
                ConsistencyLevel consistency_level, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("add", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new addArgs();
                args.Key = key;
                args.Column_parent = column_parent;
                args.Column = column;
                args.Consistency_level = consistency_level;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new addResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                if (result.__isset.ue)
                {
                    throw result.Ue;
                }
                if (result.__isset.te)
                {
                    throw result.Te;
                }
                return;
            }

            public async Task @removeAsync(byte[] key, ColumnPath column_path, long timestamp,
                ConsistencyLevel consistency_level, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("remove", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new removeArgs();
                args.Key = key;
                args.Column_path = column_path;
                args.Timestamp = timestamp;
                args.Consistency_level = consistency_level;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new removeResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                if (result.__isset.ue)
                {
                    throw result.Ue;
                }
                if (result.__isset.te)
                {
                    throw result.Te;
                }
                return;
            }

            public async Task remove_counterAsync(byte[] key, ColumnPath path, ConsistencyLevel consistency_level,
                CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("remove_counter", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new remove_counterArgs();
                args.Key = key;
                args.Path = path;
                args.Consistency_level = consistency_level;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new remove_counterResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                if (result.__isset.ue)
                {
                    throw result.Ue;
                }
                if (result.__isset.te)
                {
                    throw result.Te;
                }
                return;
            }

            public async Task batch_mutateAsync(Dictionary<byte[], Dictionary<string, List<Mutation>>> mutation_map,
                ConsistencyLevel consistency_level, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("batch_mutate", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new batch_mutateArgs();
                args.Mutation_map = mutation_map;
                args.Consistency_level = consistency_level;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new batch_mutateResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                if (result.__isset.ue)
                {
                    throw result.Ue;
                }
                if (result.__isset.te)
                {
                    throw result.Te;
                }
                return;
            }

            public async Task truncateAsync(string cfname, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("truncate", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new truncateArgs();
                args.Cfname = cfname;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new truncateResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                if (result.__isset.ue)
                {
                    throw result.Ue;
                }
                if (result.__isset.te)
                {
                    throw result.Te;
                }
                return;
            }

            public async Task<Dictionary<string, List<string>>> describe_schema_versionsAsync(
                CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(
                        new TMessage("describe_schema_versions", TMessageType.Call, SeqId), cancellationToken);

                var args = new describe_schema_versionsArgs();

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new describe_schema_versionsResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "describe_schema_versions failed: unknown result");
            }

            public async Task<List<KsDef>> describe_keyspacesAsync(CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("describe_keyspaces", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new describe_keyspacesArgs();

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new describe_keyspacesResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "describe_keyspaces failed: unknown result");
            }

            public async Task<string> describe_cluster_nameAsync(CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(
                        new TMessage("describe_cluster_name", TMessageType.Call, SeqId), cancellationToken);

                var args = new describe_cluster_nameArgs();

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new describe_cluster_nameResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "describe_cluster_name failed: unknown result");
            }

            public async Task<string> describe_versionAsync(CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("describe_version", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new describe_versionArgs();

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new describe_versionResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "describe_version failed: unknown result");
            }

            public async Task<List<TokenRange>> describe_ringAsync(string keyspace, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("describe_ring", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new describe_ringArgs();
                args.Keyspace = keyspace;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new describe_ringResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "describe_ring failed: unknown result");
            }

            public async Task<string> describe_partitionerAsync(CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(
                        new TMessage("describe_partitioner", TMessageType.Call, SeqId), cancellationToken);

                var args = new describe_partitionerArgs();

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new describe_partitionerResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "describe_partitioner failed: unknown result");
            }

            public async Task<string> describe_snitchAsync(CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("describe_snitch", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new describe_snitchArgs();

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new describe_snitchResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "describe_snitch failed: unknown result");
            }

            public async Task<KsDef> describe_keyspaceAsync(string keyspace, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("describe_keyspace", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new describe_keyspaceArgs();
                args.Keyspace = keyspace;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new describe_keyspaceResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                if (result.__isset.nfe)
                {
                    throw result.Nfe;
                }
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "describe_keyspace failed: unknown result");
            }

            public async Task<List<string>> describe_splitsAsync(string cfName, string start_token, string end_token,
                int keys_per_split, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("describe_splits", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new describe_splitsArgs();
                args.CfName = cfName;
                args.Start_token = start_token;
                args.End_token = end_token;
                args.Keys_per_split = keys_per_split;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new describe_splitsResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "describe_splits failed: unknown result");
            }

            public async Task<string> system_add_column_familyAsync(CfDef cf_def, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(
                        new TMessage("system_add_column_family", TMessageType.Call, SeqId), cancellationToken);

                var args = new system_add_column_familyArgs();
                args.Cf_def = cf_def;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new system_add_column_familyResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                if (result.__isset.sde)
                {
                    throw result.Sde;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "system_add_column_family failed: unknown result");
            }

            public async Task<string> system_drop_column_familyAsync(string column_family,
                CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(
                        new TMessage("system_drop_column_family", TMessageType.Call, SeqId), cancellationToken);

                var args = new system_drop_column_familyArgs();
                args.Column_family = column_family;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new system_drop_column_familyResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                if (result.__isset.sde)
                {
                    throw result.Sde;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "system_drop_column_family failed: unknown result");
            }

            public async Task<string> system_add_keyspaceAsync(KsDef ks_def, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(
                        new TMessage("system_add_keyspace", TMessageType.Call, SeqId), cancellationToken);

                var args = new system_add_keyspaceArgs();
                args.Ks_def = ks_def;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new system_add_keyspaceResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                if (result.__isset.sde)
                {
                    throw result.Sde;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "system_add_keyspace failed: unknown result");
            }

            public async Task<string> system_drop_keyspaceAsync(string keyspace, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(
                        new TMessage("system_drop_keyspace", TMessageType.Call, SeqId), cancellationToken);

                var args = new system_drop_keyspaceArgs();
                args.Keyspace = keyspace;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new system_drop_keyspaceResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                if (result.__isset.sde)
                {
                    throw result.Sde;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "system_drop_keyspace failed: unknown result");
            }

            public async Task<string> system_update_keyspaceAsync(KsDef ks_def, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(
                        new TMessage("system_update_keyspace", TMessageType.Call, SeqId), cancellationToken);

                var args = new system_update_keyspaceArgs();
                args.Ks_def = ks_def;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new system_update_keyspaceResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                if (result.__isset.sde)
                {
                    throw result.Sde;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "system_update_keyspace failed: unknown result");
            }

            public async Task<string> system_update_column_familyAsync(CfDef cf_def, CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(
                        new TMessage("system_update_column_family", TMessageType.Call, SeqId), cancellationToken);

                var args = new system_update_column_familyArgs();
                args.Cf_def = cf_def;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new system_update_column_familyResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                if (result.__isset.sde)
                {
                    throw result.Sde;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "system_update_column_family failed: unknown result");
            }

            public async Task<CqlResult> execute_cql_queryAsync(byte[] query, Compression compression,
                CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("execute_cql_query", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new execute_cql_queryArgs();
                args.Query = query;
                args.Compression = compression;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new execute_cql_queryResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                if (result.__isset.ue)
                {
                    throw result.Ue;
                }
                if (result.__isset.te)
                {
                    throw result.Te;
                }
                if (result.__isset.sde)
                {
                    throw result.Sde;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "execute_cql_query failed: unknown result");
            }

            public async Task<CqlPreparedResult> prepare_cql_queryAsync(byte[] query, Compression compression,
                CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(new TMessage("prepare_cql_query", TMessageType.Call, SeqId),
                        cancellationToken);

                var args = new prepare_cql_queryArgs();
                args.Query = query;
                args.Compression = compression;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new prepare_cql_queryResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "prepare_cql_query failed: unknown result");
            }

            public async Task<CqlResult> execute_prepared_cql_queryAsync(int itemId, List<string> values,
                CancellationToken cancellationToken)
            {
                await
                    OutputProtocol.WriteMessageBeginAsync(
                        new TMessage("execute_prepared_cql_query", TMessageType.Call, SeqId), cancellationToken);

                var args = new execute_prepared_cql_queryArgs();
                args.ItemId = itemId;
                args.Values = values;

                await args.WriteAsync(OutputProtocol, cancellationToken);
                await OutputProtocol.WriteMessageEndAsync(cancellationToken);
                await OutputProtocol.Transport.FlushAsync(cancellationToken);

                var msg = await InputProtocol.ReadMessageBeginAsync(cancellationToken);
                if (msg.Type == TMessageType.Exception)
                {
                    var x = await TApplicationException.ReadAsync(InputProtocol, cancellationToken);
                    await InputProtocol.ReadMessageEndAsync(cancellationToken);
                    throw x;
                }

                var result = new execute_prepared_cql_queryResult();
                await result.ReadAsync(InputProtocol, cancellationToken);
                await InputProtocol.ReadMessageEndAsync(cancellationToken);
                if (result.__isset.success)
                {
                    return result.Success;
                }
                if (result.__isset.ire)
                {
                    throw result.Ire;
                }
                if (result.__isset.ue)
                {
                    throw result.Ue;
                }
                if (result.__isset.te)
                {
                    throw result.Te;
                }
                if (result.__isset.sde)
                {
                    throw result.Sde;
                }
                throw new TApplicationException(TApplicationException.ExceptionType.MissingResult,
                    "execute_prepared_cql_query failed: unknown result");
            }
        }

        public class AsyncProcessor : ITAsyncProcessor
        {
            private IAsync _iAsync;
            protected Dictionary<string, ProcessFunction> processMap_ = new Dictionary<string, ProcessFunction>();

            public AsyncProcessor(IAsync iAsync)
            {
                if (iAsync == null) throw new ArgumentNullException(nameof(iAsync));

                _iAsync = iAsync;
                processMap_["login"] = login_ProcessAsync;
                processMap_["set_keyspace"] = set_keyspace_ProcessAsync;
                processMap_["get"] = get_ProcessAsync;
                processMap_["get_slice"] = get_slice_ProcessAsync;
                processMap_["get_count"] = get_count_ProcessAsync;
                processMap_["multiget_slice"] = multiget_slice_ProcessAsync;
                processMap_["multiget_count"] = multiget_count_ProcessAsync;
                processMap_["get_range_slices"] = get_range_slices_ProcessAsync;
                processMap_["get_indexed_slices"] = get_indexed_slices_ProcessAsync;
                processMap_["insert"] = insert_ProcessAsync;
                processMap_["add"] = add_ProcessAsync;
                processMap_["remove"] = remove_ProcessAsync;
                processMap_["remove_counter"] = remove_counter_ProcessAsync;
                processMap_["batch_mutate"] = batch_mutate_ProcessAsync;
                processMap_["truncate"] = truncate_ProcessAsync;
                processMap_["describe_schema_versions"] = describe_schema_versions_ProcessAsync;
                processMap_["describe_keyspaces"] = describe_keyspaces_ProcessAsync;
                processMap_["describe_cluster_name"] = describe_cluster_name_ProcessAsync;
                processMap_["describe_version"] = describe_version_ProcessAsync;
                processMap_["describe_ring"] = describe_ring_ProcessAsync;
                processMap_["describe_partitioner"] = describe_partitioner_ProcessAsync;
                processMap_["describe_snitch"] = describe_snitch_ProcessAsync;
                processMap_["describe_keyspace"] = describe_keyspace_ProcessAsync;
                processMap_["describe_splits"] = describe_splits_ProcessAsync;
                processMap_["system_add_column_family"] = system_add_column_family_ProcessAsync;
                processMap_["system_drop_column_family"] = system_drop_column_family_ProcessAsync;
                processMap_["system_add_keyspace"] = system_add_keyspace_ProcessAsync;
                processMap_["system_drop_keyspace"] = system_drop_keyspace_ProcessAsync;
                processMap_["system_update_keyspace"] = system_update_keyspace_ProcessAsync;
                processMap_["system_update_column_family"] = system_update_column_family_ProcessAsync;
                processMap_["execute_cql_query"] = execute_cql_query_ProcessAsync;
                processMap_["prepare_cql_query"] = prepare_cql_query_ProcessAsync;
                processMap_["execute_prepared_cql_query"] = execute_prepared_cql_query_ProcessAsync;
            }

            public async Task<bool> ProcessAsync(TProtocol iprot, TProtocol oprot)
            {
                return await ProcessAsync(iprot, oprot, CancellationToken.None);
            }

            public async Task<bool> ProcessAsync(TProtocol iprot, TProtocol oprot, CancellationToken cancellationToken)
            {
                try
                {
                    var msg = await iprot.ReadMessageBeginAsync(cancellationToken);

                    ProcessFunction fn;
                    processMap_.TryGetValue(msg.Name, out fn);

                    if (fn == null)
                    {
                        await TProtocolUtil.SkipAsync(iprot, TType.Struct, cancellationToken);
                        await iprot.ReadMessageEndAsync(cancellationToken);
                        var x = new TApplicationException(TApplicationException.ExceptionType.UnknownMethod,
                            "Invalid method name: '" + msg.Name + "'");
                        await
                            oprot.WriteMessageBeginAsync(new TMessage(msg.Name, TMessageType.Exception, msg.SeqID),
                                cancellationToken);
                        await x.WriteAsync(oprot, cancellationToken);
                        await oprot.WriteMessageEndAsync(cancellationToken);
                        await oprot.Transport.FlushAsync(cancellationToken);
                        return true;
                    }

                    await fn(msg.SeqID, iprot, oprot, cancellationToken);
                }
                catch (IOException)
                {
                    return false;
                }

                return true;
            }

            public async Task login_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new loginArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new loginResult();
                try
                {
                    try
                    {
                        await _iAsync.loginAsync(args.Auth_request, cancellationToken);
                    }
                    catch (AuthenticationException authnx)
                    {
                        result.Authnx = authnx;
                    }
                    catch (AuthorizationException authzx)
                    {
                        result.Authzx = authzx;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("login", TMessageType.Reply, seqid), cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("login", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task set_keyspace_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new set_keyspaceArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new set_keyspaceResult();
                try
                {
                    try
                    {
                        await _iAsync.set_keyspaceAsync(args.Keyspace, cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("set_keyspace", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("set_keyspace", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task get_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new getArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new getResult();
                try
                {
                    try
                    {
                        result.Success =
                            await
                                _iAsync.@getAsync(args.Key, args.Column_path, args.Consistency_level, cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    catch (NotFoundException nfe)
                    {
                        result.Nfe = nfe;
                    }
                    catch (UnavailableException ue)
                    {
                        result.Ue = ue;
                    }
                    catch (TimedOutException te)
                    {
                        result.Te = te;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("get", TMessageType.Reply, seqid), cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("get", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task get_slice_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new get_sliceArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new get_sliceResult();
                try
                {
                    try
                    {
                        result.Success =
                            await
                                _iAsync.get_sliceAsync(args.Key, args.Column_parent, args.Predicate,
                                    args.Consistency_level, cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    catch (UnavailableException ue)
                    {
                        result.Ue = ue;
                    }
                    catch (TimedOutException te)
                    {
                        result.Te = te;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("get_slice", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("get_slice", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task get_count_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new get_countArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new get_countResult();
                try
                {
                    try
                    {
                        result.Success =
                            await
                                _iAsync.get_countAsync(args.Key, args.Column_parent, args.Predicate,
                                    args.Consistency_level, cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    catch (UnavailableException ue)
                    {
                        result.Ue = ue;
                    }
                    catch (TimedOutException te)
                    {
                        result.Te = te;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("get_count", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("get_count", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task multiget_slice_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new multiget_sliceArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new multiget_sliceResult();
                try
                {
                    try
                    {
                        result.Success =
                            await
                                _iAsync.multiget_sliceAsync(args.Keys, args.Column_parent, args.Predicate,
                                    args.Consistency_level, cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    catch (UnavailableException ue)
                    {
                        result.Ue = ue;
                    }
                    catch (TimedOutException te)
                    {
                        result.Te = te;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("multiget_slice", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("multiget_slice", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task multiget_count_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new multiget_countArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new multiget_countResult();
                try
                {
                    try
                    {
                        result.Success =
                            await
                                _iAsync.multiget_countAsync(args.Keys, args.Column_parent, args.Predicate,
                                    args.Consistency_level, cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    catch (UnavailableException ue)
                    {
                        result.Ue = ue;
                    }
                    catch (TimedOutException te)
                    {
                        result.Te = te;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("multiget_count", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("multiget_count", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task get_range_slices_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new get_range_slicesArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new get_range_slicesResult();
                try
                {
                    try
                    {
                        result.Success =
                            await
                                _iAsync.get_range_slicesAsync(args.Column_parent, args.Predicate, args.Range,
                                    args.Consistency_level, cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    catch (UnavailableException ue)
                    {
                        result.Ue = ue;
                    }
                    catch (TimedOutException te)
                    {
                        result.Te = te;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("get_range_slices", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("get_range_slices", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task get_indexed_slices_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new get_indexed_slicesArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new get_indexed_slicesResult();
                try
                {
                    try
                    {
                        result.Success =
                            await
                                _iAsync.get_indexed_slicesAsync(args.Column_parent, args.Index_clause,
                                    args.Column_predicate, args.Consistency_level, cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    catch (UnavailableException ue)
                    {
                        result.Ue = ue;
                    }
                    catch (TimedOutException te)
                    {
                        result.Te = te;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("get_indexed_slices", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("get_indexed_slices", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task insert_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new insertArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new insertResult();
                try
                {
                    try
                    {
                        await
                            _iAsync.insertAsync(args.Key, args.Column_parent, args.Column, args.Consistency_level,
                                cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    catch (UnavailableException ue)
                    {
                        result.Ue = ue;
                    }
                    catch (TimedOutException te)
                    {
                        result.Te = te;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("insert", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("insert", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task add_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new addArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new addResult();
                try
                {
                    try
                    {
                        await
                            _iAsync.@addAsync(args.Key, args.Column_parent, args.Column, args.Consistency_level,
                                cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    catch (UnavailableException ue)
                    {
                        result.Ue = ue;
                    }
                    catch (TimedOutException te)
                    {
                        result.Te = te;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("add", TMessageType.Reply, seqid), cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("add", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task remove_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new removeArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new removeResult();
                try
                {
                    try
                    {
                        await
                            _iAsync.@removeAsync(args.Key, args.Column_path, args.Timestamp, args.Consistency_level,
                                cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    catch (UnavailableException ue)
                    {
                        result.Ue = ue;
                    }
                    catch (TimedOutException te)
                    {
                        result.Te = te;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("remove", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("remove", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task remove_counter_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new remove_counterArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new remove_counterResult();
                try
                {
                    try
                    {
                        await
                            _iAsync.remove_counterAsync(args.Key, args.Path, args.Consistency_level, cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    catch (UnavailableException ue)
                    {
                        result.Ue = ue;
                    }
                    catch (TimedOutException te)
                    {
                        result.Te = te;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("remove_counter", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("remove_counter", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task batch_mutate_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new batch_mutateArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new batch_mutateResult();
                try
                {
                    try
                    {
                        await _iAsync.batch_mutateAsync(args.Mutation_map, args.Consistency_level, cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    catch (UnavailableException ue)
                    {
                        result.Ue = ue;
                    }
                    catch (TimedOutException te)
                    {
                        result.Te = te;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("batch_mutate", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("batch_mutate", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task truncate_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new truncateArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new truncateResult();
                try
                {
                    try
                    {
                        await _iAsync.truncateAsync(args.Cfname, cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    catch (UnavailableException ue)
                    {
                        result.Ue = ue;
                    }
                    catch (TimedOutException te)
                    {
                        result.Te = te;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("truncate", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("truncate", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task describe_schema_versions_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new describe_schema_versionsArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new describe_schema_versionsResult();
                try
                {
                    try
                    {
                        result.Success = await _iAsync.describe_schema_versionsAsync(cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    await
                        oprot.WriteMessageBeginAsync(
                            new TMessage("describe_schema_versions", TMessageType.Reply, seqid), cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(
                            new TMessage("describe_schema_versions", TMessageType.Exception, seqid), cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task describe_keyspaces_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new describe_keyspacesArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new describe_keyspacesResult();
                try
                {
                    try
                    {
                        result.Success = await _iAsync.describe_keyspacesAsync(cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("describe_keyspaces", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("describe_keyspaces", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task describe_cluster_name_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new describe_cluster_nameArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new describe_cluster_nameResult();
                try
                {
                    result.Success = await _iAsync.describe_cluster_nameAsync(cancellationToken);
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("describe_cluster_name", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(
                            new TMessage("describe_cluster_name", TMessageType.Exception, seqid), cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task describe_version_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new describe_versionArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new describe_versionResult();
                try
                {
                    result.Success = await _iAsync.describe_versionAsync(cancellationToken);
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("describe_version", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("describe_version", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task describe_ring_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new describe_ringArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new describe_ringResult();
                try
                {
                    try
                    {
                        result.Success = await _iAsync.describe_ringAsync(args.Keyspace, cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("describe_ring", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("describe_ring", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task describe_partitioner_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new describe_partitionerArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new describe_partitionerResult();
                try
                {
                    result.Success = await _iAsync.describe_partitionerAsync(cancellationToken);
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("describe_partitioner", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(
                            new TMessage("describe_partitioner", TMessageType.Exception, seqid), cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task describe_snitch_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new describe_snitchArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new describe_snitchResult();
                try
                {
                    result.Success = await _iAsync.describe_snitchAsync(cancellationToken);
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("describe_snitch", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("describe_snitch", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task describe_keyspace_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new describe_keyspaceArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new describe_keyspaceResult();
                try
                {
                    try
                    {
                        result.Success = await _iAsync.describe_keyspaceAsync(args.Keyspace, cancellationToken);
                    }
                    catch (NotFoundException nfe)
                    {
                        result.Nfe = nfe;
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("describe_keyspace", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("describe_keyspace", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task describe_splits_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new describe_splitsArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new describe_splitsResult();
                try
                {
                    try
                    {
                        result.Success =
                            await
                                _iAsync.describe_splitsAsync(args.CfName, args.Start_token, args.End_token,
                                    args.Keys_per_split, cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("describe_splits", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("describe_splits", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task system_add_column_family_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new system_add_column_familyArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new system_add_column_familyResult();
                try
                {
                    try
                    {
                        result.Success = await _iAsync.system_add_column_familyAsync(args.Cf_def, cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    catch (SchemaDisagreementException sde)
                    {
                        result.Sde = sde;
                    }
                    await
                        oprot.WriteMessageBeginAsync(
                            new TMessage("system_add_column_family", TMessageType.Reply, seqid), cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(
                            new TMessage("system_add_column_family", TMessageType.Exception, seqid), cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task system_drop_column_family_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new system_drop_column_familyArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new system_drop_column_familyResult();
                try
                {
                    try
                    {
                        result.Success =
                            await _iAsync.system_drop_column_familyAsync(args.Column_family, cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    catch (SchemaDisagreementException sde)
                    {
                        result.Sde = sde;
                    }
                    await
                        oprot.WriteMessageBeginAsync(
                            new TMessage("system_drop_column_family", TMessageType.Reply, seqid), cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(
                            new TMessage("system_drop_column_family", TMessageType.Exception, seqid), cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task system_add_keyspace_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new system_add_keyspaceArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new system_add_keyspaceResult();
                try
                {
                    try
                    {
                        result.Success = await _iAsync.system_add_keyspaceAsync(args.Ks_def, cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    catch (SchemaDisagreementException sde)
                    {
                        result.Sde = sde;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("system_add_keyspace", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(
                            new TMessage("system_add_keyspace", TMessageType.Exception, seqid), cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task system_drop_keyspace_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new system_drop_keyspaceArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new system_drop_keyspaceResult();
                try
                {
                    try
                    {
                        result.Success = await _iAsync.system_drop_keyspaceAsync(args.Keyspace, cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    catch (SchemaDisagreementException sde)
                    {
                        result.Sde = sde;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("system_drop_keyspace", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(
                            new TMessage("system_drop_keyspace", TMessageType.Exception, seqid), cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task system_update_keyspace_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new system_update_keyspaceArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new system_update_keyspaceResult();
                try
                {
                    try
                    {
                        result.Success = await _iAsync.system_update_keyspaceAsync(args.Ks_def, cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    catch (SchemaDisagreementException sde)
                    {
                        result.Sde = sde;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("system_update_keyspace", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(
                            new TMessage("system_update_keyspace", TMessageType.Exception, seqid), cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task system_update_column_family_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new system_update_column_familyArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new system_update_column_familyResult();
                try
                {
                    try
                    {
                        result.Success = await _iAsync.system_update_column_familyAsync(args.Cf_def, cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    catch (SchemaDisagreementException sde)
                    {
                        result.Sde = sde;
                    }
                    await
                        oprot.WriteMessageBeginAsync(
                            new TMessage("system_update_column_family", TMessageType.Reply, seqid), cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(
                            new TMessage("system_update_column_family", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task execute_cql_query_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new execute_cql_queryArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new execute_cql_queryResult();
                try
                {
                    try
                    {
                        result.Success =
                            await _iAsync.execute_cql_queryAsync(args.Query, args.Compression, cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    catch (UnavailableException ue)
                    {
                        result.Ue = ue;
                    }
                    catch (TimedOutException te)
                    {
                        result.Te = te;
                    }
                    catch (SchemaDisagreementException sde)
                    {
                        result.Sde = sde;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("execute_cql_query", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("execute_cql_query", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task prepare_cql_query_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new prepare_cql_queryArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new prepare_cql_queryResult();
                try
                {
                    try
                    {
                        result.Success =
                            await _iAsync.prepare_cql_queryAsync(args.Query, args.Compression, cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("prepare_cql_query", TMessageType.Reply, seqid),
                            cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(new TMessage("prepare_cql_query", TMessageType.Exception, seqid),
                            cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            public async Task execute_prepared_cql_query_ProcessAsync(int seqid, TProtocol iprot, TProtocol oprot,
                CancellationToken cancellationToken)
            {
                var args = new execute_prepared_cql_queryArgs();
                await args.ReadAsync(iprot, cancellationToken);
                await iprot.ReadMessageEndAsync(cancellationToken);
                var result = new execute_prepared_cql_queryResult();
                try
                {
                    try
                    {
                        result.Success =
                            await _iAsync.execute_prepared_cql_queryAsync(args.ItemId, args.Values, cancellationToken);
                    }
                    catch (InvalidRequestException ire)
                    {
                        result.Ire = ire;
                    }
                    catch (UnavailableException ue)
                    {
                        result.Ue = ue;
                    }
                    catch (TimedOutException te)
                    {
                        result.Te = te;
                    }
                    catch (SchemaDisagreementException sde)
                    {
                        result.Sde = sde;
                    }
                    await
                        oprot.WriteMessageBeginAsync(
                            new TMessage("execute_prepared_cql_query", TMessageType.Reply, seqid), cancellationToken);
                    await result.WriteAsync(oprot, cancellationToken);
                }
                catch (TTransportException)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Error occurred in processor:");
                    Console.Error.WriteLine(ex.ToString());
                    var x = new TApplicationException(TApplicationException.ExceptionType.InternalError,
                        " Internal error.");
                    await
                        oprot.WriteMessageBeginAsync(
                            new TMessage("execute_prepared_cql_query", TMessageType.Exception, seqid), cancellationToken);
                    await x.WriteAsync(oprot, cancellationToken);
                }
                await oprot.WriteMessageEndAsync(cancellationToken);
                await oprot.Transport.FlushAsync(cancellationToken);
            }

            protected delegate Task ProcessFunction(
                int seqid, TProtocol iprot, TProtocol oprot, CancellationToken cancellationToken);
        }


        [DataContract(Namespace = "")]
        public partial class loginArgs : TBase
        {
            public loginArgs()
            {
            }

            public loginArgs(AuthenticationRequest auth_request) : this()
            {
                this.Auth_request = auth_request;
            }

            [DataMember(Order = 0)]
            public AuthenticationRequest Auth_request { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_auth_request = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Auth_request = new AuthenticationRequest();
                                    await Auth_request.ReadAsync(iprot, cancellationToken);
                                    isset_auth_request = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_auth_request)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("login_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "auth_request";
                    field.Type = TType.Struct;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Auth_request.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("login_args(");
                sb.Append(", Auth_request: ");
                sb.Append(Auth_request == null ? "<null>" : Auth_request.ToString());
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class loginResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private AuthenticationException _authnx;
            private AuthorizationException _authzx;

            public loginResult()
            {
            }

            [DataMember(Order = 0)]
            public AuthenticationException Authnx
            {
                get { return _authnx; }
                set
                {
                    __isset.authnx = true;
                    this._authnx = value;
                }
            }

            [DataMember(Order = 0)]
            public AuthorizationException Authzx
            {
                get { return _authzx; }
                set
                {
                    __isset.authzx = true;
                    this._authzx = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Authnx = new AuthenticationException();
                                    await Authnx.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Authzx = new AuthorizationException();
                                    await Authzx.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("login_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.authnx)
                    {
                        if (Authnx != null)
                        {
                            field.Name = "Authnx";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Authnx.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.authzx)
                    {
                        if (Authzx != null)
                        {
                            field.Name = "Authzx";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Authzx.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("login_result(");
                bool __first = true;
                if (Authnx != null && __isset.authnx)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Authnx: ");
                    sb.Append(Authnx == null ? "<null>" : Authnx.ToString());
                }
                if (Authzx != null && __isset.authzx)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Authzx: ");
                    sb.Append(Authzx == null ? "<null>" : Authzx.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool authnx;
                [DataMember] public bool authzx;
            }

            #region XmlSerializer support

            public bool ShouldSerializeAuthnx()
            {
                return __isset.authnx;
            }

            public bool ShouldSerializeAuthzx()
            {
                return __isset.authzx;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class set_keyspaceArgs : TBase
        {
            public set_keyspaceArgs()
            {
            }

            public set_keyspaceArgs(string keyspace) : this()
            {
                this.Keyspace = keyspace;
            }

            [DataMember(Order = 0)]
            public string Keyspace { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_keyspace = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.String)
                                {
                                    Keyspace = await iprot.ReadStringAsync(cancellationToken);
                                    isset_keyspace = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_keyspace)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("set_keyspace_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "keyspace";
                    field.Type = TType.String;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Keyspace, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("set_keyspace_args(");
                sb.Append(", Keyspace: ");
                sb.Append(Keyspace);
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class set_keyspaceResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;

            public set_keyspaceResult()
            {
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("set_keyspace_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            #region XmlSerializer support

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            #endregion XmlSerializer support

            public override string ToString()
            {
                var sb = new StringBuilder("set_keyspace_result(");
                bool __first = true;
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool ire;
            }
        }


        [DataContract(Namespace = "")]
        public partial class getArgs : TBase
        {
            public getArgs()
            {
                this.Consistency_level = ConsistencyLevel.ONE;
            }

            public getArgs(byte[] key, ColumnPath column_path, ConsistencyLevel consistency_level) : this()
            {
                this.Key = key;
                this.Column_path = column_path;
                this.Consistency_level = consistency_level;
            }

            [DataMember(Order = 0)]
            public byte[] Key { get; set; }

            [DataMember(Order = 0)]
            public ColumnPath Column_path { get; set; }

            /// <summary>
            ///     <seealso cref="ConsistencyLevel" />
            /// </summary>
            [DataMember(Order = 0)]
            public ConsistencyLevel Consistency_level { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_key = false;
                    bool isset_column_path = false;
                    bool isset_consistency_level = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.String)
                                {
                                    Key = await iprot.ReadBinaryAsync(cancellationToken);
                                    isset_key = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Column_path = new ColumnPath();
                                    await Column_path.ReadAsync(iprot, cancellationToken);
                                    isset_column_path = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.I32)
                                {
                                    Consistency_level = (ConsistencyLevel) await iprot.ReadI32Async(cancellationToken);
                                    isset_consistency_level = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_key)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_column_path)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_consistency_level)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("get_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "key";
                    field.Type = TType.String;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteBinaryAsync(Key, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "column_path";
                    field.Type = TType.Struct;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Column_path.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "consistency_level";
                    field.Type = TType.I32;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async((int) Consistency_level, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("get_args(");
                sb.Append(", Key: ");
                sb.Append(Key);
                sb.Append(", Column_path: ");
                sb.Append(Column_path == null ? "<null>" : Column_path.ToString());
                sb.Append(", Consistency_level: ");
                sb.Append(Consistency_level);
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class getResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private NotFoundException _nfe;
            private ColumnOrSuperColumn _success;
            private TimedOutException _te;
            private UnavailableException _ue;

            public getResult()
            {
            }

            [DataMember(Order = 0)]
            public ColumnOrSuperColumn Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            [DataMember(Order = 0)]
            public NotFoundException Nfe
            {
                get { return _nfe; }
                set
                {
                    __isset.nfe = true;
                    this._nfe = value;
                }
            }

            [DataMember(Order = 0)]
            public UnavailableException Ue
            {
                get { return _ue; }
                set
                {
                    __isset.ue = true;
                    this._ue = value;
                }
            }

            [DataMember(Order = 0)]
            public TimedOutException Te
            {
                get { return _te; }
                set
                {
                    __isset.te = true;
                    this._te = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.Struct)
                                {
                                    Success = new ColumnOrSuperColumn();
                                    await Success.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Nfe = new NotFoundException();
                                    await Nfe.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Ue = new UnavailableException();
                                    await Ue.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 4:
                                if (field.Type == TType.Struct)
                                {
                                    Te = new TimedOutException();
                                    await Te.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("get_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.Struct;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Success.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.nfe)
                    {
                        if (Nfe != null)
                        {
                            field.Name = "Nfe";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Nfe.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ue)
                    {
                        if (Ue != null)
                        {
                            field.Name = "Ue";
                            field.Type = TType.Struct;
                            field.ID = 3;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ue.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.te)
                    {
                        if (Te != null)
                        {
                            field.Name = "Te";
                            field.Type = TType.Struct;
                            field.ID = 4;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Te.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("get_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success == null ? "<null>" : Success.ToString());
                }
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                if (Nfe != null && __isset.nfe)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Nfe: ");
                    sb.Append(Nfe == null ? "<null>" : Nfe.ToString());
                }
                if (Ue != null && __isset.ue)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ue: ");
                    sb.Append(Ue == null ? "<null>" : Ue.ToString());
                }
                if (Te != null && __isset.te)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Te: ");
                    sb.Append(Te == null ? "<null>" : Te.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
                [DataMember] public bool ire;
                [DataMember] public bool nfe;
                [DataMember] public bool ue;
                [DataMember] public bool te;
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            public bool ShouldSerializeNfe()
            {
                return __isset.nfe;
            }

            public bool ShouldSerializeUe()
            {
                return __isset.ue;
            }

            public bool ShouldSerializeTe()
            {
                return __isset.te;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class get_sliceArgs : TBase
        {
            public get_sliceArgs()
            {
                this.Consistency_level = ConsistencyLevel.ONE;
            }

            public get_sliceArgs(byte[] key, ColumnParent column_parent, SlicePredicate predicate,
                ConsistencyLevel consistency_level) : this()
            {
                this.Key = key;
                this.Column_parent = column_parent;
                this.Predicate = predicate;
                this.Consistency_level = consistency_level;
            }

            [DataMember(Order = 0)]
            public byte[] Key { get; set; }

            [DataMember(Order = 0)]
            public ColumnParent Column_parent { get; set; }

            [DataMember(Order = 0)]
            public SlicePredicate Predicate { get; set; }

            /// <summary>
            ///     <seealso cref="ConsistencyLevel" />
            /// </summary>
            [DataMember(Order = 0)]
            public ConsistencyLevel Consistency_level { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_key = false;
                    bool isset_column_parent = false;
                    bool isset_predicate = false;
                    bool isset_consistency_level = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.String)
                                {
                                    Key = await iprot.ReadBinaryAsync(cancellationToken);
                                    isset_key = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Column_parent = new ColumnParent();
                                    await Column_parent.ReadAsync(iprot, cancellationToken);
                                    isset_column_parent = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Predicate = new SlicePredicate();
                                    await Predicate.ReadAsync(iprot, cancellationToken);
                                    isset_predicate = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 4:
                                if (field.Type == TType.I32)
                                {
                                    Consistency_level = (ConsistencyLevel) await iprot.ReadI32Async(cancellationToken);
                                    isset_consistency_level = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_key)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_column_parent)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_predicate)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_consistency_level)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("get_slice_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "key";
                    field.Type = TType.String;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteBinaryAsync(Key, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "column_parent";
                    field.Type = TType.Struct;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Column_parent.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "predicate";
                    field.Type = TType.Struct;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Predicate.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "consistency_level";
                    field.Type = TType.I32;
                    field.ID = 4;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async((int) Consistency_level, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("get_slice_args(");
                sb.Append(", Key: ");
                sb.Append(Key);
                sb.Append(", Column_parent: ");
                sb.Append(Column_parent == null ? "<null>" : Column_parent.ToString());
                sb.Append(", Predicate: ");
                sb.Append(Predicate == null ? "<null>" : Predicate.ToString());
                sb.Append(", Consistency_level: ");
                sb.Append(Consistency_level);
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class get_sliceResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private List<ColumnOrSuperColumn> _success;
            private TimedOutException _te;
            private UnavailableException _ue;

            public get_sliceResult()
            {
            }

            [DataMember(Order = 0)]
            public List<ColumnOrSuperColumn> Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            [DataMember(Order = 0)]
            public UnavailableException Ue
            {
                get { return _ue; }
                set
                {
                    __isset.ue = true;
                    this._ue = value;
                }
            }

            [DataMember(Order = 0)]
            public TimedOutException Te
            {
                get { return _te; }
                set
                {
                    __isset.te = true;
                    this._te = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.List)
                                {
                                    {
                                        Success = new List<ColumnOrSuperColumn>();
                                        TList _list83 = await iprot.ReadListBeginAsync(cancellationToken);
                                        for (int _i84 = 0; _i84 < _list83.Count; ++_i84)
                                        {
                                            ColumnOrSuperColumn _elem85;
                                            _elem85 = new ColumnOrSuperColumn();
                                            await _elem85.ReadAsync(iprot, cancellationToken);
                                            Success.Add(_elem85);
                                        }
                                        await iprot.ReadListEndAsync(cancellationToken);
                                    }
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Ue = new UnavailableException();
                                    await Ue.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Te = new TimedOutException();
                                    await Te.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("get_slice_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.List;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            {
                                await
                                    oprot.WriteListBeginAsync(new TList(TType.Struct, Success.Count), cancellationToken);
                                foreach (ColumnOrSuperColumn _iter86 in Success)
                                {
                                    await _iter86.WriteAsync(oprot, cancellationToken);
                                }
                                await oprot.WriteListEndAsync(cancellationToken);
                            }
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ue)
                    {
                        if (Ue != null)
                        {
                            field.Name = "Ue";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ue.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.te)
                    {
                        if (Te != null)
                        {
                            field.Name = "Te";
                            field.Type = TType.Struct;
                            field.ID = 3;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Te.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("get_slice_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success);
                }
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                if (Ue != null && __isset.ue)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ue: ");
                    sb.Append(Ue == null ? "<null>" : Ue.ToString());
                }
                if (Te != null && __isset.te)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Te: ");
                    sb.Append(Te == null ? "<null>" : Te.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
                [DataMember] public bool ire;
                [DataMember] public bool ue;
                [DataMember] public bool te;
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            public bool ShouldSerializeUe()
            {
                return __isset.ue;
            }

            public bool ShouldSerializeTe()
            {
                return __isset.te;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class get_countArgs : TBase
        {
            public get_countArgs()
            {
                this.Consistency_level = ConsistencyLevel.ONE;
            }

            public get_countArgs(byte[] key, ColumnParent column_parent, SlicePredicate predicate,
                ConsistencyLevel consistency_level) : this()
            {
                this.Key = key;
                this.Column_parent = column_parent;
                this.Predicate = predicate;
                this.Consistency_level = consistency_level;
            }

            [DataMember(Order = 0)]
            public byte[] Key { get; set; }

            [DataMember(Order = 0)]
            public ColumnParent Column_parent { get; set; }

            [DataMember(Order = 0)]
            public SlicePredicate Predicate { get; set; }

            /// <summary>
            ///     <seealso cref="ConsistencyLevel" />
            /// </summary>
            [DataMember(Order = 0)]
            public ConsistencyLevel Consistency_level { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_key = false;
                    bool isset_column_parent = false;
                    bool isset_predicate = false;
                    bool isset_consistency_level = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.String)
                                {
                                    Key = await iprot.ReadBinaryAsync(cancellationToken);
                                    isset_key = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Column_parent = new ColumnParent();
                                    await Column_parent.ReadAsync(iprot, cancellationToken);
                                    isset_column_parent = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Predicate = new SlicePredicate();
                                    await Predicate.ReadAsync(iprot, cancellationToken);
                                    isset_predicate = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 4:
                                if (field.Type == TType.I32)
                                {
                                    Consistency_level = (ConsistencyLevel) await iprot.ReadI32Async(cancellationToken);
                                    isset_consistency_level = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_key)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_column_parent)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_predicate)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_consistency_level)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("get_count_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "key";
                    field.Type = TType.String;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteBinaryAsync(Key, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "column_parent";
                    field.Type = TType.Struct;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Column_parent.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "predicate";
                    field.Type = TType.Struct;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Predicate.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "consistency_level";
                    field.Type = TType.I32;
                    field.ID = 4;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async((int) Consistency_level, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("get_count_args(");
                sb.Append(", Key: ");
                sb.Append(Key);
                sb.Append(", Column_parent: ");
                sb.Append(Column_parent == null ? "<null>" : Column_parent.ToString());
                sb.Append(", Predicate: ");
                sb.Append(Predicate == null ? "<null>" : Predicate.ToString());
                sb.Append(", Consistency_level: ");
                sb.Append(Consistency_level);
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class get_countResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private int _success;
            private TimedOutException _te;
            private UnavailableException _ue;

            public get_countResult()
            {
            }

            [DataMember(Order = 0)]
            public int Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            [DataMember(Order = 0)]
            public UnavailableException Ue
            {
                get { return _ue; }
                set
                {
                    __isset.ue = true;
                    this._ue = value;
                }
            }

            [DataMember(Order = 0)]
            public TimedOutException Te
            {
                get { return _te; }
                set
                {
                    __isset.te = true;
                    this._te = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.I32)
                                {
                                    Success = await iprot.ReadI32Async(cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Ue = new UnavailableException();
                                    await Ue.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Te = new TimedOutException();
                                    await Te.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("get_count_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        field.Name = "Success";
                        field.Type = TType.I32;
                        field.ID = 0;
                        await oprot.WriteFieldBeginAsync(field, cancellationToken);
                        await oprot.WriteI32Async(Success, cancellationToken);
                        await oprot.WriteFieldEndAsync(cancellationToken);
                    }
                    else if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ue)
                    {
                        if (Ue != null)
                        {
                            field.Name = "Ue";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ue.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.te)
                    {
                        if (Te != null)
                        {
                            field.Name = "Te";
                            field.Type = TType.Struct;
                            field.ID = 3;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Te.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("get_count_result(");
                bool __first = true;
                if (__isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success);
                }
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                if (Ue != null && __isset.ue)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ue: ");
                    sb.Append(Ue == null ? "<null>" : Ue.ToString());
                }
                if (Te != null && __isset.te)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Te: ");
                    sb.Append(Te == null ? "<null>" : Te.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
                [DataMember] public bool ire;
                [DataMember] public bool ue;
                [DataMember] public bool te;
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            public bool ShouldSerializeUe()
            {
                return __isset.ue;
            }

            public bool ShouldSerializeTe()
            {
                return __isset.te;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class multiget_sliceArgs : TBase
        {
            public multiget_sliceArgs()
            {
                this.Consistency_level = ConsistencyLevel.ONE;
            }

            public multiget_sliceArgs(List<byte[]> keys, ColumnParent column_parent, SlicePredicate predicate,
                ConsistencyLevel consistency_level) : this()
            {
                this.Keys = keys;
                this.Column_parent = column_parent;
                this.Predicate = predicate;
                this.Consistency_level = consistency_level;
            }

            [DataMember(Order = 0)]
            public List<byte[]> Keys { get; set; }

            [DataMember(Order = 0)]
            public ColumnParent Column_parent { get; set; }

            [DataMember(Order = 0)]
            public SlicePredicate Predicate { get; set; }

            /// <summary>
            ///     <seealso cref="ConsistencyLevel" />
            /// </summary>
            [DataMember(Order = 0)]
            public ConsistencyLevel Consistency_level { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_keys = false;
                    bool isset_column_parent = false;
                    bool isset_predicate = false;
                    bool isset_consistency_level = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.List)
                                {
                                    {
                                        Keys = new List<byte[]>();
                                        TList _list87 = await iprot.ReadListBeginAsync(cancellationToken);
                                        for (int _i88 = 0; _i88 < _list87.Count; ++_i88)
                                        {
                                            byte[] _elem89;
                                            _elem89 = await iprot.ReadBinaryAsync(cancellationToken);
                                            Keys.Add(_elem89);
                                        }
                                        await iprot.ReadListEndAsync(cancellationToken);
                                    }
                                    isset_keys = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Column_parent = new ColumnParent();
                                    await Column_parent.ReadAsync(iprot, cancellationToken);
                                    isset_column_parent = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Predicate = new SlicePredicate();
                                    await Predicate.ReadAsync(iprot, cancellationToken);
                                    isset_predicate = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 4:
                                if (field.Type == TType.I32)
                                {
                                    Consistency_level = (ConsistencyLevel) await iprot.ReadI32Async(cancellationToken);
                                    isset_consistency_level = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_keys)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_column_parent)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_predicate)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_consistency_level)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("multiget_slice_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "keys";
                    field.Type = TType.List;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await oprot.WriteListBeginAsync(new TList(TType.String, Keys.Count), cancellationToken);
                        foreach (byte[] _iter90 in Keys)
                        {
                            await oprot.WriteBinaryAsync(_iter90, cancellationToken);
                        }
                        await oprot.WriteListEndAsync(cancellationToken);
                    }
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "column_parent";
                    field.Type = TType.Struct;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Column_parent.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "predicate";
                    field.Type = TType.Struct;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Predicate.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "consistency_level";
                    field.Type = TType.I32;
                    field.ID = 4;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async((int) Consistency_level, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("multiget_slice_args(");
                sb.Append(", Keys: ");
                sb.Append(Keys);
                sb.Append(", Column_parent: ");
                sb.Append(Column_parent == null ? "<null>" : Column_parent.ToString());
                sb.Append(", Predicate: ");
                sb.Append(Predicate == null ? "<null>" : Predicate.ToString());
                sb.Append(", Consistency_level: ");
                sb.Append(Consistency_level);
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class multiget_sliceResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private Dictionary<byte[], List<ColumnOrSuperColumn>> _success;
            private TimedOutException _te;
            private UnavailableException _ue;

            public multiget_sliceResult()
            {
            }

            [DataMember(Order = 0)]
            public Dictionary<byte[], List<ColumnOrSuperColumn>> Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            [DataMember(Order = 0)]
            public UnavailableException Ue
            {
                get { return _ue; }
                set
                {
                    __isset.ue = true;
                    this._ue = value;
                }
            }

            [DataMember(Order = 0)]
            public TimedOutException Te
            {
                get { return _te; }
                set
                {
                    __isset.te = true;
                    this._te = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.Map)
                                {
                                    {
                                        Success = new Dictionary<byte[], List<ColumnOrSuperColumn>>();
                                        TMap _map91 = await iprot.ReadMapBeginAsync(cancellationToken);
                                        for (int _i92 = 0; _i92 < _map91.Count; ++_i92)
                                        {
                                            byte[] _key93;
                                            List<ColumnOrSuperColumn> _val94;
                                            _key93 = await iprot.ReadBinaryAsync(cancellationToken);
                                            {
                                                _val94 = new List<ColumnOrSuperColumn>();
                                                TList _list95 = await iprot.ReadListBeginAsync(cancellationToken);
                                                for (int _i96 = 0; _i96 < _list95.Count; ++_i96)
                                                {
                                                    ColumnOrSuperColumn _elem97;
                                                    _elem97 = new ColumnOrSuperColumn();
                                                    await _elem97.ReadAsync(iprot, cancellationToken);
                                                    _val94.Add(_elem97);
                                                }
                                                await iprot.ReadListEndAsync(cancellationToken);
                                            }
                                            Success[_key93] = _val94;
                                        }
                                        await iprot.ReadMapEndAsync(cancellationToken);
                                    }
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Ue = new UnavailableException();
                                    await Ue.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Te = new TimedOutException();
                                    await Te.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("multiget_slice_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.Map;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            {
                                await
                                    oprot.WriteMapBeginAsync(new TMap(TType.String, TType.List, Success.Count),
                                        cancellationToken);
                                foreach (byte[] _iter98 in Success.Keys)
                                {
                                    await oprot.WriteBinaryAsync(_iter98, cancellationToken);
                                    {
                                        await
                                            oprot.WriteListBeginAsync(new TList(TType.Struct, Success[_iter98].Count),
                                                cancellationToken);
                                        foreach (ColumnOrSuperColumn _iter99 in Success[_iter98])
                                        {
                                            await _iter99.WriteAsync(oprot, cancellationToken);
                                        }
                                        await oprot.WriteListEndAsync(cancellationToken);
                                    }
                                }
                                await oprot.WriteMapEndAsync(cancellationToken);
                            }
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ue)
                    {
                        if (Ue != null)
                        {
                            field.Name = "Ue";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ue.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.te)
                    {
                        if (Te != null)
                        {
                            field.Name = "Te";
                            field.Type = TType.Struct;
                            field.ID = 3;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Te.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("multiget_slice_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success);
                }
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                if (Ue != null && __isset.ue)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ue: ");
                    sb.Append(Ue == null ? "<null>" : Ue.ToString());
                }
                if (Te != null && __isset.te)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Te: ");
                    sb.Append(Te == null ? "<null>" : Te.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
                [DataMember] public bool ire;
                [DataMember] public bool ue;
                [DataMember] public bool te;
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            public bool ShouldSerializeUe()
            {
                return __isset.ue;
            }

            public bool ShouldSerializeTe()
            {
                return __isset.te;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class multiget_countArgs : TBase
        {
            public multiget_countArgs()
            {
                this.Consistency_level = ConsistencyLevel.ONE;
            }

            public multiget_countArgs(List<byte[]> keys, ColumnParent column_parent, SlicePredicate predicate,
                ConsistencyLevel consistency_level) : this()
            {
                this.Keys = keys;
                this.Column_parent = column_parent;
                this.Predicate = predicate;
                this.Consistency_level = consistency_level;
            }

            [DataMember(Order = 0)]
            public List<byte[]> Keys { get; set; }

            [DataMember(Order = 0)]
            public ColumnParent Column_parent { get; set; }

            [DataMember(Order = 0)]
            public SlicePredicate Predicate { get; set; }

            /// <summary>
            ///     <seealso cref="ConsistencyLevel" />
            /// </summary>
            [DataMember(Order = 0)]
            public ConsistencyLevel Consistency_level { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_keys = false;
                    bool isset_column_parent = false;
                    bool isset_predicate = false;
                    bool isset_consistency_level = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.List)
                                {
                                    {
                                        Keys = new List<byte[]>();
                                        TList _list100 = await iprot.ReadListBeginAsync(cancellationToken);
                                        for (int _i101 = 0; _i101 < _list100.Count; ++_i101)
                                        {
                                            byte[] _elem102;
                                            _elem102 = await iprot.ReadBinaryAsync(cancellationToken);
                                            Keys.Add(_elem102);
                                        }
                                        await iprot.ReadListEndAsync(cancellationToken);
                                    }
                                    isset_keys = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Column_parent = new ColumnParent();
                                    await Column_parent.ReadAsync(iprot, cancellationToken);
                                    isset_column_parent = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Predicate = new SlicePredicate();
                                    await Predicate.ReadAsync(iprot, cancellationToken);
                                    isset_predicate = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 4:
                                if (field.Type == TType.I32)
                                {
                                    Consistency_level = (ConsistencyLevel) await iprot.ReadI32Async(cancellationToken);
                                    isset_consistency_level = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_keys)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_column_parent)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_predicate)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_consistency_level)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("multiget_count_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "keys";
                    field.Type = TType.List;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await oprot.WriteListBeginAsync(new TList(TType.String, Keys.Count), cancellationToken);
                        foreach (byte[] _iter103 in Keys)
                        {
                            await oprot.WriteBinaryAsync(_iter103, cancellationToken);
                        }
                        await oprot.WriteListEndAsync(cancellationToken);
                    }
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "column_parent";
                    field.Type = TType.Struct;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Column_parent.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "predicate";
                    field.Type = TType.Struct;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Predicate.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "consistency_level";
                    field.Type = TType.I32;
                    field.ID = 4;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async((int) Consistency_level, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("multiget_count_args(");
                sb.Append(", Keys: ");
                sb.Append(Keys);
                sb.Append(", Column_parent: ");
                sb.Append(Column_parent == null ? "<null>" : Column_parent.ToString());
                sb.Append(", Predicate: ");
                sb.Append(Predicate == null ? "<null>" : Predicate.ToString());
                sb.Append(", Consistency_level: ");
                sb.Append(Consistency_level);
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class multiget_countResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private Dictionary<byte[], int> _success;
            private TimedOutException _te;
            private UnavailableException _ue;

            public multiget_countResult()
            {
            }

            [DataMember(Order = 0)]
            public Dictionary<byte[], int> Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            [DataMember(Order = 0)]
            public UnavailableException Ue
            {
                get { return _ue; }
                set
                {
                    __isset.ue = true;
                    this._ue = value;
                }
            }

            [DataMember(Order = 0)]
            public TimedOutException Te
            {
                get { return _te; }
                set
                {
                    __isset.te = true;
                    this._te = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.Map)
                                {
                                    {
                                        Success = new Dictionary<byte[], int>();
                                        TMap _map104 = await iprot.ReadMapBeginAsync(cancellationToken);
                                        for (int _i105 = 0; _i105 < _map104.Count; ++_i105)
                                        {
                                            byte[] _key106;
                                            int _val107;
                                            _key106 = await iprot.ReadBinaryAsync(cancellationToken);
                                            _val107 = await iprot.ReadI32Async(cancellationToken);
                                            Success[_key106] = _val107;
                                        }
                                        await iprot.ReadMapEndAsync(cancellationToken);
                                    }
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Ue = new UnavailableException();
                                    await Ue.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Te = new TimedOutException();
                                    await Te.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("multiget_count_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.Map;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            {
                                await
                                    oprot.WriteMapBeginAsync(new TMap(TType.String, TType.I32, Success.Count),
                                        cancellationToken);
                                foreach (byte[] _iter108 in Success.Keys)
                                {
                                    await oprot.WriteBinaryAsync(_iter108, cancellationToken);
                                    await oprot.WriteI32Async(Success[_iter108], cancellationToken);
                                }
                                await oprot.WriteMapEndAsync(cancellationToken);
                            }
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ue)
                    {
                        if (Ue != null)
                        {
                            field.Name = "Ue";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ue.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.te)
                    {
                        if (Te != null)
                        {
                            field.Name = "Te";
                            field.Type = TType.Struct;
                            field.ID = 3;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Te.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("multiget_count_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success);
                }
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                if (Ue != null && __isset.ue)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ue: ");
                    sb.Append(Ue == null ? "<null>" : Ue.ToString());
                }
                if (Te != null && __isset.te)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Te: ");
                    sb.Append(Te == null ? "<null>" : Te.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
                [DataMember] public bool ire;
                [DataMember] public bool ue;
                [DataMember] public bool te;
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            public bool ShouldSerializeUe()
            {
                return __isset.ue;
            }

            public bool ShouldSerializeTe()
            {
                return __isset.te;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class get_range_slicesArgs : TBase
        {
            public get_range_slicesArgs()
            {
                this.Consistency_level = ConsistencyLevel.ONE;
            }

            public get_range_slicesArgs(ColumnParent column_parent, SlicePredicate predicate, KeyRange range,
                ConsistencyLevel consistency_level) : this()
            {
                this.Column_parent = column_parent;
                this.Predicate = predicate;
                this.Range = range;
                this.Consistency_level = consistency_level;
            }

            [DataMember(Order = 0)]
            public ColumnParent Column_parent { get; set; }

            [DataMember(Order = 0)]
            public SlicePredicate Predicate { get; set; }

            [DataMember(Order = 0)]
            public KeyRange Range { get; set; }

            /// <summary>
            ///     <seealso cref="ConsistencyLevel" />
            /// </summary>
            [DataMember(Order = 0)]
            public ConsistencyLevel Consistency_level { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_column_parent = false;
                    bool isset_predicate = false;
                    bool isset_range = false;
                    bool isset_consistency_level = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Column_parent = new ColumnParent();
                                    await Column_parent.ReadAsync(iprot, cancellationToken);
                                    isset_column_parent = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Predicate = new SlicePredicate();
                                    await Predicate.ReadAsync(iprot, cancellationToken);
                                    isset_predicate = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Range = new KeyRange();
                                    await Range.ReadAsync(iprot, cancellationToken);
                                    isset_range = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 4:
                                if (field.Type == TType.I32)
                                {
                                    Consistency_level = (ConsistencyLevel) await iprot.ReadI32Async(cancellationToken);
                                    isset_consistency_level = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_column_parent)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_predicate)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_range)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_consistency_level)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("get_range_slices_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "column_parent";
                    field.Type = TType.Struct;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Column_parent.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "predicate";
                    field.Type = TType.Struct;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Predicate.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "range";
                    field.Type = TType.Struct;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Range.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "consistency_level";
                    field.Type = TType.I32;
                    field.ID = 4;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async((int) Consistency_level, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("get_range_slices_args(");
                sb.Append(", Column_parent: ");
                sb.Append(Column_parent == null ? "<null>" : Column_parent.ToString());
                sb.Append(", Predicate: ");
                sb.Append(Predicate == null ? "<null>" : Predicate.ToString());
                sb.Append(", Range: ");
                sb.Append(Range == null ? "<null>" : Range.ToString());
                sb.Append(", Consistency_level: ");
                sb.Append(Consistency_level);
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class get_range_slicesResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private List<KeySlice> _success;
            private TimedOutException _te;
            private UnavailableException _ue;

            public get_range_slicesResult()
            {
            }

            [DataMember(Order = 0)]
            public List<KeySlice> Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            [DataMember(Order = 0)]
            public UnavailableException Ue
            {
                get { return _ue; }
                set
                {
                    __isset.ue = true;
                    this._ue = value;
                }
            }

            [DataMember(Order = 0)]
            public TimedOutException Te
            {
                get { return _te; }
                set
                {
                    __isset.te = true;
                    this._te = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.List)
                                {
                                    {
                                        Success = new List<KeySlice>();
                                        TList _list109 = await iprot.ReadListBeginAsync(cancellationToken);
                                        for (int _i110 = 0; _i110 < _list109.Count; ++_i110)
                                        {
                                            KeySlice _elem111;
                                            _elem111 = new KeySlice();
                                            await _elem111.ReadAsync(iprot, cancellationToken);
                                            Success.Add(_elem111);
                                        }
                                        await iprot.ReadListEndAsync(cancellationToken);
                                    }
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Ue = new UnavailableException();
                                    await Ue.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Te = new TimedOutException();
                                    await Te.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("get_range_slices_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.List;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            {
                                await
                                    oprot.WriteListBeginAsync(new TList(TType.Struct, Success.Count), cancellationToken);
                                foreach (KeySlice _iter112 in Success)
                                {
                                    await _iter112.WriteAsync(oprot, cancellationToken);
                                }
                                await oprot.WriteListEndAsync(cancellationToken);
                            }
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ue)
                    {
                        if (Ue != null)
                        {
                            field.Name = "Ue";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ue.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.te)
                    {
                        if (Te != null)
                        {
                            field.Name = "Te";
                            field.Type = TType.Struct;
                            field.ID = 3;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Te.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("get_range_slices_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success);
                }
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                if (Ue != null && __isset.ue)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ue: ");
                    sb.Append(Ue == null ? "<null>" : Ue.ToString());
                }
                if (Te != null && __isset.te)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Te: ");
                    sb.Append(Te == null ? "<null>" : Te.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
                [DataMember] public bool ire;
                [DataMember] public bool ue;
                [DataMember] public bool te;
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            public bool ShouldSerializeUe()
            {
                return __isset.ue;
            }

            public bool ShouldSerializeTe()
            {
                return __isset.te;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class get_indexed_slicesArgs : TBase
        {
            public get_indexed_slicesArgs()
            {
                this.Consistency_level = ConsistencyLevel.ONE;
            }

            public get_indexed_slicesArgs(ColumnParent column_parent, IndexClause index_clause,
                SlicePredicate column_predicate, ConsistencyLevel consistency_level) : this()
            {
                this.Column_parent = column_parent;
                this.Index_clause = index_clause;
                this.Column_predicate = column_predicate;
                this.Consistency_level = consistency_level;
            }

            [DataMember(Order = 0)]
            public ColumnParent Column_parent { get; set; }

            [DataMember(Order = 0)]
            public IndexClause Index_clause { get; set; }

            [DataMember(Order = 0)]
            public SlicePredicate Column_predicate { get; set; }

            /// <summary>
            ///     <seealso cref="ConsistencyLevel" />
            /// </summary>
            [DataMember(Order = 0)]
            public ConsistencyLevel Consistency_level { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_column_parent = false;
                    bool isset_index_clause = false;
                    bool isset_column_predicate = false;
                    bool isset_consistency_level = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Column_parent = new ColumnParent();
                                    await Column_parent.ReadAsync(iprot, cancellationToken);
                                    isset_column_parent = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Index_clause = new IndexClause();
                                    await Index_clause.ReadAsync(iprot, cancellationToken);
                                    isset_index_clause = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Column_predicate = new SlicePredicate();
                                    await Column_predicate.ReadAsync(iprot, cancellationToken);
                                    isset_column_predicate = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 4:
                                if (field.Type == TType.I32)
                                {
                                    Consistency_level = (ConsistencyLevel) await iprot.ReadI32Async(cancellationToken);
                                    isset_consistency_level = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_column_parent)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_index_clause)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_column_predicate)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_consistency_level)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("get_indexed_slices_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "column_parent";
                    field.Type = TType.Struct;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Column_parent.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "index_clause";
                    field.Type = TType.Struct;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Index_clause.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "column_predicate";
                    field.Type = TType.Struct;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Column_predicate.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "consistency_level";
                    field.Type = TType.I32;
                    field.ID = 4;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async((int) Consistency_level, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("get_indexed_slices_args(");
                sb.Append(", Column_parent: ");
                sb.Append(Column_parent == null ? "<null>" : Column_parent.ToString());
                sb.Append(", Index_clause: ");
                sb.Append(Index_clause == null ? "<null>" : Index_clause.ToString());
                sb.Append(", Column_predicate: ");
                sb.Append(Column_predicate == null ? "<null>" : Column_predicate.ToString());
                sb.Append(", Consistency_level: ");
                sb.Append(Consistency_level);
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class get_indexed_slicesResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private List<KeySlice> _success;
            private TimedOutException _te;
            private UnavailableException _ue;

            public get_indexed_slicesResult()
            {
            }

            [DataMember(Order = 0)]
            public List<KeySlice> Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            [DataMember(Order = 0)]
            public UnavailableException Ue
            {
                get { return _ue; }
                set
                {
                    __isset.ue = true;
                    this._ue = value;
                }
            }

            [DataMember(Order = 0)]
            public TimedOutException Te
            {
                get { return _te; }
                set
                {
                    __isset.te = true;
                    this._te = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.List)
                                {
                                    {
                                        Success = new List<KeySlice>();
                                        TList _list113 = await iprot.ReadListBeginAsync(cancellationToken);
                                        for (int _i114 = 0; _i114 < _list113.Count; ++_i114)
                                        {
                                            KeySlice _elem115;
                                            _elem115 = new KeySlice();
                                            await _elem115.ReadAsync(iprot, cancellationToken);
                                            Success.Add(_elem115);
                                        }
                                        await iprot.ReadListEndAsync(cancellationToken);
                                    }
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Ue = new UnavailableException();
                                    await Ue.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Te = new TimedOutException();
                                    await Te.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("get_indexed_slices_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.List;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            {
                                await
                                    oprot.WriteListBeginAsync(new TList(TType.Struct, Success.Count), cancellationToken);
                                foreach (KeySlice _iter116 in Success)
                                {
                                    await _iter116.WriteAsync(oprot, cancellationToken);
                                }
                                await oprot.WriteListEndAsync(cancellationToken);
                            }
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ue)
                    {
                        if (Ue != null)
                        {
                            field.Name = "Ue";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ue.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.te)
                    {
                        if (Te != null)
                        {
                            field.Name = "Te";
                            field.Type = TType.Struct;
                            field.ID = 3;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Te.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("get_indexed_slices_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success);
                }
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                if (Ue != null && __isset.ue)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ue: ");
                    sb.Append(Ue == null ? "<null>" : Ue.ToString());
                }
                if (Te != null && __isset.te)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Te: ");
                    sb.Append(Te == null ? "<null>" : Te.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
                [DataMember] public bool ire;
                [DataMember] public bool ue;
                [DataMember] public bool te;
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            public bool ShouldSerializeUe()
            {
                return __isset.ue;
            }

            public bool ShouldSerializeTe()
            {
                return __isset.te;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class insertArgs : TBase
        {
            public insertArgs()
            {
                this.Consistency_level = ConsistencyLevel.ONE;
            }

            public insertArgs(byte[] key, ColumnParent column_parent, Column column, ConsistencyLevel consistency_level)
                : this()
            {
                this.Key = key;
                this.Column_parent = column_parent;
                this.Column = column;
                this.Consistency_level = consistency_level;
            }

            [DataMember(Order = 0)]
            public byte[] Key { get; set; }

            [DataMember(Order = 0)]
            public ColumnParent Column_parent { get; set; }

            [DataMember(Order = 0)]
            public Column Column { get; set; }

            /// <summary>
            ///     <seealso cref="ConsistencyLevel" />
            /// </summary>
            [DataMember(Order = 0)]
            public ConsistencyLevel Consistency_level { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_key = false;
                    bool isset_column_parent = false;
                    bool isset_column = false;
                    bool isset_consistency_level = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.String)
                                {
                                    Key = await iprot.ReadBinaryAsync(cancellationToken);
                                    isset_key = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Column_parent = new ColumnParent();
                                    await Column_parent.ReadAsync(iprot, cancellationToken);
                                    isset_column_parent = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Column = new Column();
                                    await Column.ReadAsync(iprot, cancellationToken);
                                    isset_column = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 4:
                                if (field.Type == TType.I32)
                                {
                                    Consistency_level = (ConsistencyLevel) await iprot.ReadI32Async(cancellationToken);
                                    isset_consistency_level = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_key)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_column_parent)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_column)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_consistency_level)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("insert_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "key";
                    field.Type = TType.String;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteBinaryAsync(Key, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "column_parent";
                    field.Type = TType.Struct;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Column_parent.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "column";
                    field.Type = TType.Struct;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Column.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "consistency_level";
                    field.Type = TType.I32;
                    field.ID = 4;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async((int) Consistency_level, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("insert_args(");
                sb.Append(", Key: ");
                sb.Append(Key);
                sb.Append(", Column_parent: ");
                sb.Append(Column_parent == null ? "<null>" : Column_parent.ToString());
                sb.Append(", Column: ");
                sb.Append(Column == null ? "<null>" : Column.ToString());
                sb.Append(", Consistency_level: ");
                sb.Append(Consistency_level);
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class insertResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private TimedOutException _te;
            private UnavailableException _ue;

            public insertResult()
            {
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            [DataMember(Order = 0)]
            public UnavailableException Ue
            {
                get { return _ue; }
                set
                {
                    __isset.ue = true;
                    this._ue = value;
                }
            }

            [DataMember(Order = 0)]
            public TimedOutException Te
            {
                get { return _te; }
                set
                {
                    __isset.te = true;
                    this._te = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Ue = new UnavailableException();
                                    await Ue.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Te = new TimedOutException();
                                    await Te.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("insert_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ue)
                    {
                        if (Ue != null)
                        {
                            field.Name = "Ue";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ue.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.te)
                    {
                        if (Te != null)
                        {
                            field.Name = "Te";
                            field.Type = TType.Struct;
                            field.ID = 3;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Te.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("insert_result(");
                bool __first = true;
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                if (Ue != null && __isset.ue)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ue: ");
                    sb.Append(Ue == null ? "<null>" : Ue.ToString());
                }
                if (Te != null && __isset.te)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Te: ");
                    sb.Append(Te == null ? "<null>" : Te.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool ire;
                [DataMember] public bool ue;
                [DataMember] public bool te;
            }

            #region XmlSerializer support

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            public bool ShouldSerializeUe()
            {
                return __isset.ue;
            }

            public bool ShouldSerializeTe()
            {
                return __isset.te;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class addArgs : TBase
        {
            public addArgs()
            {
                this.Consistency_level = ConsistencyLevel.ONE;
            }

            public addArgs(byte[] key, ColumnParent column_parent, CounterColumn column,
                ConsistencyLevel consistency_level) : this()
            {
                this.Key = key;
                this.Column_parent = column_parent;
                this.Column = column;
                this.Consistency_level = consistency_level;
            }

            [DataMember(Order = 0)]
            public byte[] Key { get; set; }

            [DataMember(Order = 0)]
            public ColumnParent Column_parent { get; set; }

            [DataMember(Order = 0)]
            public CounterColumn Column { get; set; }

            /// <summary>
            ///     <seealso cref="ConsistencyLevel" />
            /// </summary>
            [DataMember(Order = 0)]
            public ConsistencyLevel Consistency_level { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_key = false;
                    bool isset_column_parent = false;
                    bool isset_column = false;
                    bool isset_consistency_level = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.String)
                                {
                                    Key = await iprot.ReadBinaryAsync(cancellationToken);
                                    isset_key = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Column_parent = new ColumnParent();
                                    await Column_parent.ReadAsync(iprot, cancellationToken);
                                    isset_column_parent = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Column = new CounterColumn();
                                    await Column.ReadAsync(iprot, cancellationToken);
                                    isset_column = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 4:
                                if (field.Type == TType.I32)
                                {
                                    Consistency_level = (ConsistencyLevel) await iprot.ReadI32Async(cancellationToken);
                                    isset_consistency_level = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_key)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_column_parent)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_column)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_consistency_level)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("add_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "key";
                    field.Type = TType.String;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteBinaryAsync(Key, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "column_parent";
                    field.Type = TType.Struct;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Column_parent.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "column";
                    field.Type = TType.Struct;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Column.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "consistency_level";
                    field.Type = TType.I32;
                    field.ID = 4;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async((int) Consistency_level, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("add_args(");
                sb.Append(", Key: ");
                sb.Append(Key);
                sb.Append(", Column_parent: ");
                sb.Append(Column_parent == null ? "<null>" : Column_parent.ToString());
                sb.Append(", Column: ");
                sb.Append(Column == null ? "<null>" : Column.ToString());
                sb.Append(", Consistency_level: ");
                sb.Append(Consistency_level);
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class addResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private TimedOutException _te;
            private UnavailableException _ue;

            public addResult()
            {
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            [DataMember(Order = 0)]
            public UnavailableException Ue
            {
                get { return _ue; }
                set
                {
                    __isset.ue = true;
                    this._ue = value;
                }
            }

            [DataMember(Order = 0)]
            public TimedOutException Te
            {
                get { return _te; }
                set
                {
                    __isset.te = true;
                    this._te = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Ue = new UnavailableException();
                                    await Ue.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Te = new TimedOutException();
                                    await Te.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("add_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ue)
                    {
                        if (Ue != null)
                        {
                            field.Name = "Ue";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ue.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.te)
                    {
                        if (Te != null)
                        {
                            field.Name = "Te";
                            field.Type = TType.Struct;
                            field.ID = 3;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Te.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("add_result(");
                bool __first = true;
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                if (Ue != null && __isset.ue)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ue: ");
                    sb.Append(Ue == null ? "<null>" : Ue.ToString());
                }
                if (Te != null && __isset.te)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Te: ");
                    sb.Append(Te == null ? "<null>" : Te.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool ire;
                [DataMember] public bool ue;
                [DataMember] public bool te;
            }

            #region XmlSerializer support

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            public bool ShouldSerializeUe()
            {
                return __isset.ue;
            }

            public bool ShouldSerializeTe()
            {
                return __isset.te;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class removeArgs : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private ConsistencyLevel _consistency_level;

            public removeArgs()
            {
                this._consistency_level = ConsistencyLevel.ONE;
                this.__isset.consistency_level = true;
            }

            public removeArgs(byte[] key, ColumnPath column_path, long timestamp) : this()
            {
                this.Key = key;
                this.Column_path = column_path;
                this.Timestamp = timestamp;
            }

            [DataMember(Order = 0)]
            public byte[] Key { get; set; }

            [DataMember(Order = 0)]
            public ColumnPath Column_path { get; set; }

            [DataMember(Order = 0)]
            public long Timestamp { get; set; }

            /// <summary>
            ///     <seealso cref="ConsistencyLevel" />
            /// </summary>
            [DataMember(Order = 0)]
            public ConsistencyLevel Consistency_level
            {
                get { return _consistency_level; }
                set
                {
                    __isset.consistency_level = true;
                    this._consistency_level = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_key = false;
                    bool isset_column_path = false;
                    bool isset_timestamp = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.String)
                                {
                                    Key = await iprot.ReadBinaryAsync(cancellationToken);
                                    isset_key = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Column_path = new ColumnPath();
                                    await Column_path.ReadAsync(iprot, cancellationToken);
                                    isset_column_path = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.I64)
                                {
                                    Timestamp = await iprot.ReadI64Async(cancellationToken);
                                    isset_timestamp = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 4:
                                if (field.Type == TType.I32)
                                {
                                    Consistency_level = (ConsistencyLevel) await iprot.ReadI32Async(cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_key)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_column_path)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_timestamp)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("remove_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "key";
                    field.Type = TType.String;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteBinaryAsync(Key, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "column_path";
                    field.Type = TType.Struct;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Column_path.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "timestamp";
                    field.Type = TType.I64;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI64Async(Timestamp, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    if (__isset.consistency_level)
                    {
                        field.Name = "consistency_level";
                        field.Type = TType.I32;
                        field.ID = 4;
                        await oprot.WriteFieldBeginAsync(field, cancellationToken);
                        await oprot.WriteI32Async((int) Consistency_level, cancellationToken);
                        await oprot.WriteFieldEndAsync(cancellationToken);
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            #region XmlSerializer support

            public bool ShouldSerializeConsistency_level()
            {
                return __isset.consistency_level;
            }

            #endregion XmlSerializer support

            public override string ToString()
            {
                var sb = new StringBuilder("remove_args(");
                sb.Append(", Key: ");
                sb.Append(Key);
                sb.Append(", Column_path: ");
                sb.Append(Column_path == null ? "<null>" : Column_path.ToString());
                sb.Append(", Timestamp: ");
                sb.Append(Timestamp);
                if (__isset.consistency_level)
                {
                    sb.Append(", Consistency_level: ");
                    sb.Append(Consistency_level);
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool consistency_level;
            }
        }


        [DataContract(Namespace = "")]
        public partial class removeResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private TimedOutException _te;
            private UnavailableException _ue;

            public removeResult()
            {
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            [DataMember(Order = 0)]
            public UnavailableException Ue
            {
                get { return _ue; }
                set
                {
                    __isset.ue = true;
                    this._ue = value;
                }
            }

            [DataMember(Order = 0)]
            public TimedOutException Te
            {
                get { return _te; }
                set
                {
                    __isset.te = true;
                    this._te = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Ue = new UnavailableException();
                                    await Ue.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Te = new TimedOutException();
                                    await Te.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("remove_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ue)
                    {
                        if (Ue != null)
                        {
                            field.Name = "Ue";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ue.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.te)
                    {
                        if (Te != null)
                        {
                            field.Name = "Te";
                            field.Type = TType.Struct;
                            field.ID = 3;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Te.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("remove_result(");
                bool __first = true;
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                if (Ue != null && __isset.ue)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ue: ");
                    sb.Append(Ue == null ? "<null>" : Ue.ToString());
                }
                if (Te != null && __isset.te)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Te: ");
                    sb.Append(Te == null ? "<null>" : Te.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool ire;
                [DataMember] public bool ue;
                [DataMember] public bool te;
            }

            #region XmlSerializer support

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            public bool ShouldSerializeUe()
            {
                return __isset.ue;
            }

            public bool ShouldSerializeTe()
            {
                return __isset.te;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class remove_counterArgs : TBase
        {
            public remove_counterArgs()
            {
                this.Consistency_level = ConsistencyLevel.ONE;
            }

            public remove_counterArgs(byte[] key, ColumnPath path, ConsistencyLevel consistency_level) : this()
            {
                this.Key = key;
                this.Path = path;
                this.Consistency_level = consistency_level;
            }

            [DataMember(Order = 0)]
            public byte[] Key { get; set; }

            [DataMember(Order = 0)]
            public ColumnPath Path { get; set; }

            /// <summary>
            ///     <seealso cref="ConsistencyLevel" />
            /// </summary>
            [DataMember(Order = 0)]
            public ConsistencyLevel Consistency_level { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_key = false;
                    bool isset_path = false;
                    bool isset_consistency_level = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.String)
                                {
                                    Key = await iprot.ReadBinaryAsync(cancellationToken);
                                    isset_key = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Path = new ColumnPath();
                                    await Path.ReadAsync(iprot, cancellationToken);
                                    isset_path = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.I32)
                                {
                                    Consistency_level = (ConsistencyLevel) await iprot.ReadI32Async(cancellationToken);
                                    isset_consistency_level = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_key)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_path)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_consistency_level)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("remove_counter_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "key";
                    field.Type = TType.String;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteBinaryAsync(Key, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "path";
                    field.Type = TType.Struct;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Path.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "consistency_level";
                    field.Type = TType.I32;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async((int) Consistency_level, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("remove_counter_args(");
                sb.Append(", Key: ");
                sb.Append(Key);
                sb.Append(", Path: ");
                sb.Append(Path == null ? "<null>" : Path.ToString());
                sb.Append(", Consistency_level: ");
                sb.Append(Consistency_level);
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class remove_counterResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private TimedOutException _te;
            private UnavailableException _ue;

            public remove_counterResult()
            {
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            [DataMember(Order = 0)]
            public UnavailableException Ue
            {
                get { return _ue; }
                set
                {
                    __isset.ue = true;
                    this._ue = value;
                }
            }

            [DataMember(Order = 0)]
            public TimedOutException Te
            {
                get { return _te; }
                set
                {
                    __isset.te = true;
                    this._te = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Ue = new UnavailableException();
                                    await Ue.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Te = new TimedOutException();
                                    await Te.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("remove_counter_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ue)
                    {
                        if (Ue != null)
                        {
                            field.Name = "Ue";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ue.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.te)
                    {
                        if (Te != null)
                        {
                            field.Name = "Te";
                            field.Type = TType.Struct;
                            field.ID = 3;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Te.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("remove_counter_result(");
                bool __first = true;
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                if (Ue != null && __isset.ue)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ue: ");
                    sb.Append(Ue == null ? "<null>" : Ue.ToString());
                }
                if (Te != null && __isset.te)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Te: ");
                    sb.Append(Te == null ? "<null>" : Te.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool ire;
                [DataMember] public bool ue;
                [DataMember] public bool te;
            }

            #region XmlSerializer support

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            public bool ShouldSerializeUe()
            {
                return __isset.ue;
            }

            public bool ShouldSerializeTe()
            {
                return __isset.te;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class batch_mutateArgs : TBase
        {
            public batch_mutateArgs()
            {
                this.Consistency_level = ConsistencyLevel.ONE;
            }

            public batch_mutateArgs(Dictionary<byte[], Dictionary<string, List<Mutation>>> mutation_map,
                ConsistencyLevel consistency_level) : this()
            {
                this.Mutation_map = mutation_map;
                this.Consistency_level = consistency_level;
            }

            [DataMember(Order = 0)]
            public Dictionary<byte[], Dictionary<string, List<Mutation>>> Mutation_map { get; set; }

            /// <summary>
            ///     <seealso cref="ConsistencyLevel" />
            /// </summary>
            [DataMember(Order = 0)]
            public ConsistencyLevel Consistency_level { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_mutation_map = false;
                    bool isset_consistency_level = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.Map)
                                {
                                    {
                                        Mutation_map = new Dictionary<byte[], Dictionary<string, List<Mutation>>>();
                                        TMap _map117 = await iprot.ReadMapBeginAsync(cancellationToken);
                                        for (int _i118 = 0; _i118 < _map117.Count; ++_i118)
                                        {
                                            byte[] _key119;
                                            Dictionary<string, List<Mutation>> _val120;
                                            _key119 = await iprot.ReadBinaryAsync(cancellationToken);
                                            {
                                                _val120 = new Dictionary<string, List<Mutation>>();
                                                TMap _map121 = await iprot.ReadMapBeginAsync(cancellationToken);
                                                for (int _i122 = 0; _i122 < _map121.Count; ++_i122)
                                                {
                                                    string _key123;
                                                    List<Mutation> _val124;
                                                    _key123 = await iprot.ReadStringAsync(cancellationToken);
                                                    {
                                                        _val124 = new List<Mutation>();
                                                        TList _list125 =
                                                            await iprot.ReadListBeginAsync(cancellationToken);
                                                        for (int _i126 = 0; _i126 < _list125.Count; ++_i126)
                                                        {
                                                            Mutation _elem127;
                                                            _elem127 = new Mutation();
                                                            await _elem127.ReadAsync(iprot, cancellationToken);
                                                            _val124.Add(_elem127);
                                                        }
                                                        await iprot.ReadListEndAsync(cancellationToken);
                                                    }
                                                    _val120[_key123] = _val124;
                                                }
                                                await iprot.ReadMapEndAsync(cancellationToken);
                                            }
                                            Mutation_map[_key119] = _val120;
                                        }
                                        await iprot.ReadMapEndAsync(cancellationToken);
                                    }
                                    isset_mutation_map = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.I32)
                                {
                                    Consistency_level = (ConsistencyLevel) await iprot.ReadI32Async(cancellationToken);
                                    isset_consistency_level = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_mutation_map)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_consistency_level)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("batch_mutate_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "mutation_map";
                    field.Type = TType.Map;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await
                            oprot.WriteMapBeginAsync(new TMap(TType.String, TType.Map, Mutation_map.Count),
                                cancellationToken);
                        foreach (byte[] _iter128 in Mutation_map.Keys)
                        {
                            await oprot.WriteBinaryAsync(_iter128, cancellationToken);
                            {
                                await
                                    oprot.WriteMapBeginAsync(
                                        new TMap(TType.String, TType.List, Mutation_map[_iter128].Count),
                                        cancellationToken);
                                foreach (string _iter129 in Mutation_map[_iter128].Keys)
                                {
                                    await oprot.WriteStringAsync(_iter129, cancellationToken);
                                    {
                                        await
                                            oprot.WriteListBeginAsync(
                                                new TList(TType.Struct, Mutation_map[_iter128][_iter129].Count),
                                                cancellationToken);
                                        foreach (Mutation _iter130 in Mutation_map[_iter128][_iter129])
                                        {
                                            await _iter130.WriteAsync(oprot, cancellationToken);
                                        }
                                        await oprot.WriteListEndAsync(cancellationToken);
                                    }
                                }
                                await oprot.WriteMapEndAsync(cancellationToken);
                            }
                        }
                        await oprot.WriteMapEndAsync(cancellationToken);
                    }
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "consistency_level";
                    field.Type = TType.I32;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async((int) Consistency_level, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("batch_mutate_args(");
                sb.Append(", Mutation_map: ");
                sb.Append(Mutation_map);
                sb.Append(", Consistency_level: ");
                sb.Append(Consistency_level);
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class batch_mutateResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private TimedOutException _te;
            private UnavailableException _ue;

            public batch_mutateResult()
            {
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            [DataMember(Order = 0)]
            public UnavailableException Ue
            {
                get { return _ue; }
                set
                {
                    __isset.ue = true;
                    this._ue = value;
                }
            }

            [DataMember(Order = 0)]
            public TimedOutException Te
            {
                get { return _te; }
                set
                {
                    __isset.te = true;
                    this._te = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Ue = new UnavailableException();
                                    await Ue.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Te = new TimedOutException();
                                    await Te.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("batch_mutate_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ue)
                    {
                        if (Ue != null)
                        {
                            field.Name = "Ue";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ue.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.te)
                    {
                        if (Te != null)
                        {
                            field.Name = "Te";
                            field.Type = TType.Struct;
                            field.ID = 3;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Te.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("batch_mutate_result(");
                bool __first = true;
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                if (Ue != null && __isset.ue)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ue: ");
                    sb.Append(Ue == null ? "<null>" : Ue.ToString());
                }
                if (Te != null && __isset.te)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Te: ");
                    sb.Append(Te == null ? "<null>" : Te.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool ire;
                [DataMember] public bool ue;
                [DataMember] public bool te;
            }

            #region XmlSerializer support

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            public bool ShouldSerializeUe()
            {
                return __isset.ue;
            }

            public bool ShouldSerializeTe()
            {
                return __isset.te;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class truncateArgs : TBase
        {
            public truncateArgs()
            {
            }

            public truncateArgs(string cfname) : this()
            {
                this.Cfname = cfname;
            }

            [DataMember(Order = 0)]
            public string Cfname { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_cfname = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.String)
                                {
                                    Cfname = await iprot.ReadStringAsync(cancellationToken);
                                    isset_cfname = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_cfname)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("truncate_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "cfname";
                    field.Type = TType.String;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Cfname, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("truncate_args(");
                sb.Append(", Cfname: ");
                sb.Append(Cfname);
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class truncateResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private TimedOutException _te;
            private UnavailableException _ue;

            public truncateResult()
            {
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            [DataMember(Order = 0)]
            public UnavailableException Ue
            {
                get { return _ue; }
                set
                {
                    __isset.ue = true;
                    this._ue = value;
                }
            }

            [DataMember(Order = 0)]
            public TimedOutException Te
            {
                get { return _te; }
                set
                {
                    __isset.te = true;
                    this._te = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Ue = new UnavailableException();
                                    await Ue.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Te = new TimedOutException();
                                    await Te.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("truncate_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ue)
                    {
                        if (Ue != null)
                        {
                            field.Name = "Ue";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ue.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.te)
                    {
                        if (Te != null)
                        {
                            field.Name = "Te";
                            field.Type = TType.Struct;
                            field.ID = 3;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Te.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("truncate_result(");
                bool __first = true;
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                if (Ue != null && __isset.ue)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ue: ");
                    sb.Append(Ue == null ? "<null>" : Ue.ToString());
                }
                if (Te != null && __isset.te)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Te: ");
                    sb.Append(Te == null ? "<null>" : Te.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool ire;
                [DataMember] public bool ue;
                [DataMember] public bool te;
            }

            #region XmlSerializer support

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            public bool ShouldSerializeUe()
            {
                return __isset.ue;
            }

            public bool ShouldSerializeTe()
            {
                return __isset.te;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class describe_schema_versionsArgs : TBase
        {
            public describe_schema_versionsArgs()
            {
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("describe_schema_versions_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("describe_schema_versions_args(");
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class describe_schema_versionsResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private Dictionary<string, List<string>> _success;

            public describe_schema_versionsResult()
            {
            }

            [DataMember(Order = 0)]
            public Dictionary<string, List<string>> Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.Map)
                                {
                                    {
                                        Success = new Dictionary<string, List<string>>();
                                        TMap _map131 = await iprot.ReadMapBeginAsync(cancellationToken);
                                        for (int _i132 = 0; _i132 < _map131.Count; ++_i132)
                                        {
                                            string _key133;
                                            List<string> _val134;
                                            _key133 = await iprot.ReadStringAsync(cancellationToken);
                                            {
                                                _val134 = new List<string>();
                                                TList _list135 = await iprot.ReadListBeginAsync(cancellationToken);
                                                for (int _i136 = 0; _i136 < _list135.Count; ++_i136)
                                                {
                                                    string _elem137;
                                                    _elem137 = await iprot.ReadStringAsync(cancellationToken);
                                                    _val134.Add(_elem137);
                                                }
                                                await iprot.ReadListEndAsync(cancellationToken);
                                            }
                                            Success[_key133] = _val134;
                                        }
                                        await iprot.ReadMapEndAsync(cancellationToken);
                                    }
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("describe_schema_versions_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.Map;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            {
                                await
                                    oprot.WriteMapBeginAsync(new TMap(TType.String, TType.List, Success.Count),
                                        cancellationToken);
                                foreach (string _iter138 in Success.Keys)
                                {
                                    await oprot.WriteStringAsync(_iter138, cancellationToken);
                                    {
                                        await
                                            oprot.WriteListBeginAsync(new TList(TType.String, Success[_iter138].Count),
                                                cancellationToken);
                                        foreach (string _iter139 in Success[_iter138])
                                        {
                                            await oprot.WriteStringAsync(_iter139, cancellationToken);
                                        }
                                        await oprot.WriteListEndAsync(cancellationToken);
                                    }
                                }
                                await oprot.WriteMapEndAsync(cancellationToken);
                            }
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("describe_schema_versions_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success);
                }
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
                [DataMember] public bool ire;
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class describe_keyspacesArgs : TBase
        {
            public describe_keyspacesArgs()
            {
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("describe_keyspaces_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("describe_keyspaces_args(");
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class describe_keyspacesResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private List<KsDef> _success;

            public describe_keyspacesResult()
            {
            }

            [DataMember(Order = 0)]
            public List<KsDef> Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.List)
                                {
                                    {
                                        Success = new List<KsDef>();
                                        TList _list140 = await iprot.ReadListBeginAsync(cancellationToken);
                                        for (int _i141 = 0; _i141 < _list140.Count; ++_i141)
                                        {
                                            KsDef _elem142;
                                            _elem142 = new KsDef();
                                            await _elem142.ReadAsync(iprot, cancellationToken);
                                            Success.Add(_elem142);
                                        }
                                        await iprot.ReadListEndAsync(cancellationToken);
                                    }
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("describe_keyspaces_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.List;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            {
                                await
                                    oprot.WriteListBeginAsync(new TList(TType.Struct, Success.Count), cancellationToken);
                                foreach (KsDef _iter143 in Success)
                                {
                                    await _iter143.WriteAsync(oprot, cancellationToken);
                                }
                                await oprot.WriteListEndAsync(cancellationToken);
                            }
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("describe_keyspaces_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success);
                }
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
                [DataMember] public bool ire;
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class describe_cluster_nameArgs : TBase
        {
            public describe_cluster_nameArgs()
            {
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("describe_cluster_name_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("describe_cluster_name_args(");
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class describe_cluster_nameResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private string _success;

            public describe_cluster_nameResult()
            {
            }

            [DataMember(Order = 0)]
            public string Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.String)
                                {
                                    Success = await iprot.ReadStringAsync(cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("describe_cluster_name_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.String;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await oprot.WriteStringAsync(Success, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            #endregion XmlSerializer support

            public override string ToString()
            {
                var sb = new StringBuilder("describe_cluster_name_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success);
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
            }
        }


        [DataContract(Namespace = "")]
        public partial class describe_versionArgs : TBase
        {
            public describe_versionArgs()
            {
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("describe_version_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("describe_version_args(");
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class describe_versionResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private string _success;

            public describe_versionResult()
            {
            }

            [DataMember(Order = 0)]
            public string Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.String)
                                {
                                    Success = await iprot.ReadStringAsync(cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("describe_version_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.String;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await oprot.WriteStringAsync(Success, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            #endregion XmlSerializer support

            public override string ToString()
            {
                var sb = new StringBuilder("describe_version_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success);
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
            }
        }


        [DataContract(Namespace = "")]
        public partial class describe_ringArgs : TBase
        {
            public describe_ringArgs()
            {
            }

            public describe_ringArgs(string keyspace) : this()
            {
                this.Keyspace = keyspace;
            }

            [DataMember(Order = 0)]
            public string Keyspace { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_keyspace = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.String)
                                {
                                    Keyspace = await iprot.ReadStringAsync(cancellationToken);
                                    isset_keyspace = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_keyspace)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("describe_ring_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "keyspace";
                    field.Type = TType.String;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Keyspace, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("describe_ring_args(");
                sb.Append(", Keyspace: ");
                sb.Append(Keyspace);
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class describe_ringResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private List<TokenRange> _success;

            public describe_ringResult()
            {
            }

            [DataMember(Order = 0)]
            public List<TokenRange> Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.List)
                                {
                                    {
                                        Success = new List<TokenRange>();
                                        TList _list144 = await iprot.ReadListBeginAsync(cancellationToken);
                                        for (int _i145 = 0; _i145 < _list144.Count; ++_i145)
                                        {
                                            TokenRange _elem146;
                                            _elem146 = new TokenRange();
                                            await _elem146.ReadAsync(iprot, cancellationToken);
                                            Success.Add(_elem146);
                                        }
                                        await iprot.ReadListEndAsync(cancellationToken);
                                    }
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("describe_ring_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.List;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            {
                                await
                                    oprot.WriteListBeginAsync(new TList(TType.Struct, Success.Count), cancellationToken);
                                foreach (TokenRange _iter147 in Success)
                                {
                                    await _iter147.WriteAsync(oprot, cancellationToken);
                                }
                                await oprot.WriteListEndAsync(cancellationToken);
                            }
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("describe_ring_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success);
                }
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
                [DataMember] public bool ire;
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class describe_partitionerArgs : TBase
        {
            public describe_partitionerArgs()
            {
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("describe_partitioner_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("describe_partitioner_args(");
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class describe_partitionerResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private string _success;

            public describe_partitionerResult()
            {
            }

            [DataMember(Order = 0)]
            public string Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.String)
                                {
                                    Success = await iprot.ReadStringAsync(cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("describe_partitioner_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.String;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await oprot.WriteStringAsync(Success, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            #endregion XmlSerializer support

            public override string ToString()
            {
                var sb = new StringBuilder("describe_partitioner_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success);
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
            }
        }


        [DataContract(Namespace = "")]
        public partial class describe_snitchArgs : TBase
        {
            public describe_snitchArgs()
            {
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("describe_snitch_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("describe_snitch_args(");
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class describe_snitchResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private string _success;

            public describe_snitchResult()
            {
            }

            [DataMember(Order = 0)]
            public string Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.String)
                                {
                                    Success = await iprot.ReadStringAsync(cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("describe_snitch_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.String;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await oprot.WriteStringAsync(Success, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            #endregion XmlSerializer support

            public override string ToString()
            {
                var sb = new StringBuilder("describe_snitch_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success);
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
            }
        }


        [DataContract(Namespace = "")]
        public partial class describe_keyspaceArgs : TBase
        {
            public describe_keyspaceArgs()
            {
            }

            public describe_keyspaceArgs(string keyspace) : this()
            {
                this.Keyspace = keyspace;
            }

            [DataMember(Order = 0)]
            public string Keyspace { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_keyspace = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.String)
                                {
                                    Keyspace = await iprot.ReadStringAsync(cancellationToken);
                                    isset_keyspace = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_keyspace)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("describe_keyspace_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "keyspace";
                    field.Type = TType.String;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Keyspace, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("describe_keyspace_args(");
                sb.Append(", Keyspace: ");
                sb.Append(Keyspace);
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class describe_keyspaceResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private NotFoundException _nfe;
            private KsDef _success;

            public describe_keyspaceResult()
            {
            }

            [DataMember(Order = 0)]
            public KsDef Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            [DataMember(Order = 0)]
            public NotFoundException Nfe
            {
                get { return _nfe; }
                set
                {
                    __isset.nfe = true;
                    this._nfe = value;
                }
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.Struct)
                                {
                                    Success = new KsDef();
                                    await Success.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Nfe = new NotFoundException();
                                    await Nfe.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("describe_keyspace_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.Struct;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Success.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.nfe)
                    {
                        if (Nfe != null)
                        {
                            field.Name = "Nfe";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Nfe.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("describe_keyspace_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success == null ? "<null>" : Success.ToString());
                }
                if (Nfe != null && __isset.nfe)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Nfe: ");
                    sb.Append(Nfe == null ? "<null>" : Nfe.ToString());
                }
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
                [DataMember] public bool nfe;
                [DataMember] public bool ire;
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            public bool ShouldSerializeNfe()
            {
                return __isset.nfe;
            }

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class describe_splitsArgs : TBase
        {
            public describe_splitsArgs()
            {
            }

            public describe_splitsArgs(string cfName, string start_token, string end_token, int keys_per_split) : this()
            {
                this.CfName = cfName;
                this.Start_token = start_token;
                this.End_token = end_token;
                this.Keys_per_split = keys_per_split;
            }

            [DataMember(Order = 0)]
            public string CfName { get; set; }

            [DataMember(Order = 0)]
            public string Start_token { get; set; }

            [DataMember(Order = 0)]
            public string End_token { get; set; }

            [DataMember(Order = 0)]
            public int Keys_per_split { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_cfName = false;
                    bool isset_start_token = false;
                    bool isset_end_token = false;
                    bool isset_keys_per_split = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.String)
                                {
                                    CfName = await iprot.ReadStringAsync(cancellationToken);
                                    isset_cfName = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.String)
                                {
                                    Start_token = await iprot.ReadStringAsync(cancellationToken);
                                    isset_start_token = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.String)
                                {
                                    End_token = await iprot.ReadStringAsync(cancellationToken);
                                    isset_end_token = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 4:
                                if (field.Type == TType.I32)
                                {
                                    Keys_per_split = await iprot.ReadI32Async(cancellationToken);
                                    isset_keys_per_split = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_cfName)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_start_token)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_end_token)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_keys_per_split)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("describe_splits_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "cfName";
                    field.Type = TType.String;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(CfName, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "start_token";
                    field.Type = TType.String;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Start_token, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "end_token";
                    field.Type = TType.String;
                    field.ID = 3;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(End_token, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "keys_per_split";
                    field.Type = TType.I32;
                    field.ID = 4;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async(Keys_per_split, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("describe_splits_args(");
                sb.Append(", CfName: ");
                sb.Append(CfName);
                sb.Append(", Start_token: ");
                sb.Append(Start_token);
                sb.Append(", End_token: ");
                sb.Append(End_token);
                sb.Append(", Keys_per_split: ");
                sb.Append(Keys_per_split);
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class describe_splitsResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private List<string> _success;

            public describe_splitsResult()
            {
            }

            [DataMember(Order = 0)]
            public List<string> Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.List)
                                {
                                    {
                                        Success = new List<string>();
                                        TList _list148 = await iprot.ReadListBeginAsync(cancellationToken);
                                        for (int _i149 = 0; _i149 < _list148.Count; ++_i149)
                                        {
                                            string _elem150;
                                            _elem150 = await iprot.ReadStringAsync(cancellationToken);
                                            Success.Add(_elem150);
                                        }
                                        await iprot.ReadListEndAsync(cancellationToken);
                                    }
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("describe_splits_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.List;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            {
                                await
                                    oprot.WriteListBeginAsync(new TList(TType.String, Success.Count), cancellationToken);
                                foreach (string _iter151 in Success)
                                {
                                    await oprot.WriteStringAsync(_iter151, cancellationToken);
                                }
                                await oprot.WriteListEndAsync(cancellationToken);
                            }
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("describe_splits_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success);
                }
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
                [DataMember] public bool ire;
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class system_add_column_familyArgs : TBase
        {
            public system_add_column_familyArgs()
            {
            }

            public system_add_column_familyArgs(CfDef cf_def) : this()
            {
                this.Cf_def = cf_def;
            }

            [DataMember(Order = 0)]
            public CfDef Cf_def { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_cf_def = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Cf_def = new CfDef();
                                    await Cf_def.ReadAsync(iprot, cancellationToken);
                                    isset_cf_def = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_cf_def)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("system_add_column_family_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "cf_def";
                    field.Type = TType.Struct;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Cf_def.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("system_add_column_family_args(");
                sb.Append(", Cf_def: ");
                sb.Append(Cf_def == null ? "<null>" : Cf_def.ToString());
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class system_add_column_familyResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private SchemaDisagreementException _sde;
            private string _success;

            public system_add_column_familyResult()
            {
            }

            [DataMember(Order = 0)]
            public string Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            [DataMember(Order = 0)]
            public SchemaDisagreementException Sde
            {
                get { return _sde; }
                set
                {
                    __isset.sde = true;
                    this._sde = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.String)
                                {
                                    Success = await iprot.ReadStringAsync(cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Sde = new SchemaDisagreementException();
                                    await Sde.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("system_add_column_family_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.String;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await oprot.WriteStringAsync(Success, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.sde)
                    {
                        if (Sde != null)
                        {
                            field.Name = "Sde";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Sde.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("system_add_column_family_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success);
                }
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                if (Sde != null && __isset.sde)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Sde: ");
                    sb.Append(Sde == null ? "<null>" : Sde.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
                [DataMember] public bool ire;
                [DataMember] public bool sde;
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            public bool ShouldSerializeSde()
            {
                return __isset.sde;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class system_drop_column_familyArgs : TBase
        {
            public system_drop_column_familyArgs()
            {
            }

            public system_drop_column_familyArgs(string column_family) : this()
            {
                this.Column_family = column_family;
            }

            [DataMember(Order = 0)]
            public string Column_family { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_column_family = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.String)
                                {
                                    Column_family = await iprot.ReadStringAsync(cancellationToken);
                                    isset_column_family = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_column_family)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("system_drop_column_family_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "column_family";
                    field.Type = TType.String;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Column_family, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("system_drop_column_family_args(");
                sb.Append(", Column_family: ");
                sb.Append(Column_family);
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class system_drop_column_familyResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private SchemaDisagreementException _sde;
            private string _success;

            public system_drop_column_familyResult()
            {
            }

            [DataMember(Order = 0)]
            public string Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            [DataMember(Order = 0)]
            public SchemaDisagreementException Sde
            {
                get { return _sde; }
                set
                {
                    __isset.sde = true;
                    this._sde = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.String)
                                {
                                    Success = await iprot.ReadStringAsync(cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Sde = new SchemaDisagreementException();
                                    await Sde.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("system_drop_column_family_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.String;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await oprot.WriteStringAsync(Success, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.sde)
                    {
                        if (Sde != null)
                        {
                            field.Name = "Sde";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Sde.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("system_drop_column_family_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success);
                }
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                if (Sde != null && __isset.sde)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Sde: ");
                    sb.Append(Sde == null ? "<null>" : Sde.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
                [DataMember] public bool ire;
                [DataMember] public bool sde;
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            public bool ShouldSerializeSde()
            {
                return __isset.sde;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class system_add_keyspaceArgs : TBase
        {
            public system_add_keyspaceArgs()
            {
            }

            public system_add_keyspaceArgs(KsDef ks_def) : this()
            {
                this.Ks_def = ks_def;
            }

            [DataMember(Order = 0)]
            public KsDef Ks_def { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_ks_def = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ks_def = new KsDef();
                                    await Ks_def.ReadAsync(iprot, cancellationToken);
                                    isset_ks_def = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_ks_def)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("system_add_keyspace_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "ks_def";
                    field.Type = TType.Struct;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Ks_def.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("system_add_keyspace_args(");
                sb.Append(", Ks_def: ");
                sb.Append(Ks_def == null ? "<null>" : Ks_def.ToString());
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class system_add_keyspaceResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private SchemaDisagreementException _sde;
            private string _success;

            public system_add_keyspaceResult()
            {
            }

            [DataMember(Order = 0)]
            public string Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            [DataMember(Order = 0)]
            public SchemaDisagreementException Sde
            {
                get { return _sde; }
                set
                {
                    __isset.sde = true;
                    this._sde = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.String)
                                {
                                    Success = await iprot.ReadStringAsync(cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Sde = new SchemaDisagreementException();
                                    await Sde.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("system_add_keyspace_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.String;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await oprot.WriteStringAsync(Success, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.sde)
                    {
                        if (Sde != null)
                        {
                            field.Name = "Sde";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Sde.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("system_add_keyspace_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success);
                }
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                if (Sde != null && __isset.sde)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Sde: ");
                    sb.Append(Sde == null ? "<null>" : Sde.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
                [DataMember] public bool ire;
                [DataMember] public bool sde;
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            public bool ShouldSerializeSde()
            {
                return __isset.sde;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class system_drop_keyspaceArgs : TBase
        {
            public system_drop_keyspaceArgs()
            {
            }

            public system_drop_keyspaceArgs(string keyspace) : this()
            {
                this.Keyspace = keyspace;
            }

            [DataMember(Order = 0)]
            public string Keyspace { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_keyspace = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.String)
                                {
                                    Keyspace = await iprot.ReadStringAsync(cancellationToken);
                                    isset_keyspace = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_keyspace)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("system_drop_keyspace_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "keyspace";
                    field.Type = TType.String;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteStringAsync(Keyspace, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("system_drop_keyspace_args(");
                sb.Append(", Keyspace: ");
                sb.Append(Keyspace);
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class system_drop_keyspaceResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private SchemaDisagreementException _sde;
            private string _success;

            public system_drop_keyspaceResult()
            {
            }

            [DataMember(Order = 0)]
            public string Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            [DataMember(Order = 0)]
            public SchemaDisagreementException Sde
            {
                get { return _sde; }
                set
                {
                    __isset.sde = true;
                    this._sde = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.String)
                                {
                                    Success = await iprot.ReadStringAsync(cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Sde = new SchemaDisagreementException();
                                    await Sde.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("system_drop_keyspace_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.String;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await oprot.WriteStringAsync(Success, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.sde)
                    {
                        if (Sde != null)
                        {
                            field.Name = "Sde";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Sde.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("system_drop_keyspace_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success);
                }
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                if (Sde != null && __isset.sde)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Sde: ");
                    sb.Append(Sde == null ? "<null>" : Sde.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
                [DataMember] public bool ire;
                [DataMember] public bool sde;
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            public bool ShouldSerializeSde()
            {
                return __isset.sde;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class system_update_keyspaceArgs : TBase
        {
            public system_update_keyspaceArgs()
            {
            }

            public system_update_keyspaceArgs(KsDef ks_def) : this()
            {
                this.Ks_def = ks_def;
            }

            [DataMember(Order = 0)]
            public KsDef Ks_def { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_ks_def = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ks_def = new KsDef();
                                    await Ks_def.ReadAsync(iprot, cancellationToken);
                                    isset_ks_def = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_ks_def)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("system_update_keyspace_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "ks_def";
                    field.Type = TType.Struct;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Ks_def.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("system_update_keyspace_args(");
                sb.Append(", Ks_def: ");
                sb.Append(Ks_def == null ? "<null>" : Ks_def.ToString());
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class system_update_keyspaceResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private SchemaDisagreementException _sde;
            private string _success;

            public system_update_keyspaceResult()
            {
            }

            [DataMember(Order = 0)]
            public string Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            [DataMember(Order = 0)]
            public SchemaDisagreementException Sde
            {
                get { return _sde; }
                set
                {
                    __isset.sde = true;
                    this._sde = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.String)
                                {
                                    Success = await iprot.ReadStringAsync(cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Sde = new SchemaDisagreementException();
                                    await Sde.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("system_update_keyspace_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.String;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await oprot.WriteStringAsync(Success, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.sde)
                    {
                        if (Sde != null)
                        {
                            field.Name = "Sde";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Sde.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("system_update_keyspace_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success);
                }
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                if (Sde != null && __isset.sde)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Sde: ");
                    sb.Append(Sde == null ? "<null>" : Sde.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
                [DataMember] public bool ire;
                [DataMember] public bool sde;
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            public bool ShouldSerializeSde()
            {
                return __isset.sde;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class system_update_column_familyArgs : TBase
        {
            public system_update_column_familyArgs()
            {
            }

            public system_update_column_familyArgs(CfDef cf_def) : this()
            {
                this.Cf_def = cf_def;
            }

            [DataMember(Order = 0)]
            public CfDef Cf_def { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_cf_def = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Cf_def = new CfDef();
                                    await Cf_def.ReadAsync(iprot, cancellationToken);
                                    isset_cf_def = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_cf_def)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("system_update_column_family_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "cf_def";
                    field.Type = TType.Struct;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await Cf_def.WriteAsync(oprot, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("system_update_column_family_args(");
                sb.Append(", Cf_def: ");
                sb.Append(Cf_def == null ? "<null>" : Cf_def.ToString());
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class system_update_column_familyResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private SchemaDisagreementException _sde;
            private string _success;

            public system_update_column_familyResult()
            {
            }

            [DataMember(Order = 0)]
            public string Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            [DataMember(Order = 0)]
            public SchemaDisagreementException Sde
            {
                get { return _sde; }
                set
                {
                    __isset.sde = true;
                    this._sde = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.String)
                                {
                                    Success = await iprot.ReadStringAsync(cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Sde = new SchemaDisagreementException();
                                    await Sde.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("system_update_column_family_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.String;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await oprot.WriteStringAsync(Success, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.sde)
                    {
                        if (Sde != null)
                        {
                            field.Name = "Sde";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Sde.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("system_update_column_family_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success);
                }
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                if (Sde != null && __isset.sde)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Sde: ");
                    sb.Append(Sde == null ? "<null>" : Sde.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
                [DataMember] public bool ire;
                [DataMember] public bool sde;
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            public bool ShouldSerializeSde()
            {
                return __isset.sde;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class execute_cql_queryArgs : TBase
        {
            public execute_cql_queryArgs()
            {
            }

            public execute_cql_queryArgs(byte[] query, Compression compression) : this()
            {
                this.Query = query;
                this.Compression = compression;
            }

            [DataMember(Order = 0)]
            public byte[] Query { get; set; }

            /// <summary>
            ///     <seealso cref="Compression" />
            /// </summary>
            [DataMember(Order = 0)]
            public Compression Compression { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_query = false;
                    bool isset_compression = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.String)
                                {
                                    Query = await iprot.ReadBinaryAsync(cancellationToken);
                                    isset_query = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.I32)
                                {
                                    Compression = (Compression) await iprot.ReadI32Async(cancellationToken);
                                    isset_compression = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_query)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_compression)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("execute_cql_query_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "query";
                    field.Type = TType.String;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteBinaryAsync(Query, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "compression";
                    field.Type = TType.I32;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async((int) Compression, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("execute_cql_query_args(");
                sb.Append(", Query: ");
                sb.Append(Query);
                sb.Append(", Compression: ");
                sb.Append(Compression);
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class execute_cql_queryResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private SchemaDisagreementException _sde;
            private CqlResult _success;
            private TimedOutException _te;
            private UnavailableException _ue;

            public execute_cql_queryResult()
            {
            }

            [DataMember(Order = 0)]
            public CqlResult Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            [DataMember(Order = 0)]
            public UnavailableException Ue
            {
                get { return _ue; }
                set
                {
                    __isset.ue = true;
                    this._ue = value;
                }
            }

            [DataMember(Order = 0)]
            public TimedOutException Te
            {
                get { return _te; }
                set
                {
                    __isset.te = true;
                    this._te = value;
                }
            }

            [DataMember(Order = 0)]
            public SchemaDisagreementException Sde
            {
                get { return _sde; }
                set
                {
                    __isset.sde = true;
                    this._sde = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.Struct)
                                {
                                    Success = new CqlResult();
                                    await Success.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Ue = new UnavailableException();
                                    await Ue.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Te = new TimedOutException();
                                    await Te.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 4:
                                if (field.Type == TType.Struct)
                                {
                                    Sde = new SchemaDisagreementException();
                                    await Sde.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("execute_cql_query_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.Struct;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Success.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ue)
                    {
                        if (Ue != null)
                        {
                            field.Name = "Ue";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ue.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.te)
                    {
                        if (Te != null)
                        {
                            field.Name = "Te";
                            field.Type = TType.Struct;
                            field.ID = 3;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Te.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.sde)
                    {
                        if (Sde != null)
                        {
                            field.Name = "Sde";
                            field.Type = TType.Struct;
                            field.ID = 4;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Sde.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("execute_cql_query_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success == null ? "<null>" : Success.ToString());
                }
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                if (Ue != null && __isset.ue)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ue: ");
                    sb.Append(Ue == null ? "<null>" : Ue.ToString());
                }
                if (Te != null && __isset.te)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Te: ");
                    sb.Append(Te == null ? "<null>" : Te.ToString());
                }
                if (Sde != null && __isset.sde)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Sde: ");
                    sb.Append(Sde == null ? "<null>" : Sde.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
                [DataMember] public bool ire;
                [DataMember] public bool ue;
                [DataMember] public bool te;
                [DataMember] public bool sde;
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            public bool ShouldSerializeUe()
            {
                return __isset.ue;
            }

            public bool ShouldSerializeTe()
            {
                return __isset.te;
            }

            public bool ShouldSerializeSde()
            {
                return __isset.sde;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class prepare_cql_queryArgs : TBase
        {
            public prepare_cql_queryArgs()
            {
            }

            public prepare_cql_queryArgs(byte[] query, Compression compression) : this()
            {
                this.Query = query;
                this.Compression = compression;
            }

            [DataMember(Order = 0)]
            public byte[] Query { get; set; }

            /// <summary>
            ///     <seealso cref="Compression" />
            /// </summary>
            [DataMember(Order = 0)]
            public Compression Compression { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_query = false;
                    bool isset_compression = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.String)
                                {
                                    Query = await iprot.ReadBinaryAsync(cancellationToken);
                                    isset_query = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.I32)
                                {
                                    Compression = (Compression) await iprot.ReadI32Async(cancellationToken);
                                    isset_compression = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_query)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_compression)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("prepare_cql_query_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "query";
                    field.Type = TType.String;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteBinaryAsync(Query, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "compression";
                    field.Type = TType.I32;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async((int) Compression, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("prepare_cql_query_args(");
                sb.Append(", Query: ");
                sb.Append(Query);
                sb.Append(", Compression: ");
                sb.Append(Compression);
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class prepare_cql_queryResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private CqlPreparedResult _success;

            public prepare_cql_queryResult()
            {
            }

            [DataMember(Order = 0)]
            public CqlPreparedResult Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.Struct)
                                {
                                    Success = new CqlPreparedResult();
                                    await Success.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("prepare_cql_query_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.Struct;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Success.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("prepare_cql_query_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success == null ? "<null>" : Success.ToString());
                }
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
                [DataMember] public bool ire;
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            #endregion XmlSerializer support
        }


        [DataContract(Namespace = "")]
        public partial class execute_prepared_cql_queryArgs : TBase
        {
            public execute_prepared_cql_queryArgs()
            {
            }

            public execute_prepared_cql_queryArgs(int itemId, List<string> values) : this()
            {
                this.ItemId = itemId;
                this.Values = values;
            }

            [DataMember(Order = 0)]
            public int ItemId { get; set; }

            [DataMember(Order = 0)]
            public List<string> Values { get; set; }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    bool isset_itemId = false;
                    bool isset_values = false;
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 1:
                                if (field.Type == TType.I32)
                                {
                                    ItemId = await iprot.ReadI32Async(cancellationToken);
                                    isset_itemId = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.List)
                                {
                                    {
                                        Values = new List<string>();
                                        TList _list152 = await iprot.ReadListBeginAsync(cancellationToken);
                                        for (int _i153 = 0; _i153 < _list152.Count; ++_i153)
                                        {
                                            string _elem154;
                                            _elem154 = await iprot.ReadStringAsync(cancellationToken);
                                            Values.Add(_elem154);
                                        }
                                        await iprot.ReadListEndAsync(cancellationToken);
                                    }
                                    isset_values = true;
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                    if (!isset_itemId)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                    if (!isset_values)
                    {
                        throw new TProtocolException(TProtocolException.INVALID_DATA);
                    }
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("execute_prepared_cql_query_args");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();
                    field.Name = "itemId";
                    field.Type = TType.I32;
                    field.ID = 1;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    await oprot.WriteI32Async(ItemId, cancellationToken);
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    field.Name = "values";
                    field.Type = TType.List;
                    field.ID = 2;
                    await oprot.WriteFieldBeginAsync(field, cancellationToken);
                    {
                        await oprot.WriteListBeginAsync(new TList(TType.String, Values.Count), cancellationToken);
                        foreach (string _iter155 in Values)
                        {
                            await oprot.WriteStringAsync(_iter155, cancellationToken);
                        }
                        await oprot.WriteListEndAsync(cancellationToken);
                    }
                    await oprot.WriteFieldEndAsync(cancellationToken);
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("execute_prepared_cql_query_args(");
                sb.Append(", ItemId: ");
                sb.Append(ItemId);
                sb.Append(", Values: ");
                sb.Append(Values);
                sb.Append(")");
                return sb.ToString();
            }
        }


        [DataContract(Namespace = "")]
        public partial class execute_prepared_cql_queryResult : TBase
        {
            [DataMember(Order = 1)] public Isset __isset;

            private InvalidRequestException _ire;
            private SchemaDisagreementException _sde;
            private CqlResult _success;
            private TimedOutException _te;
            private UnavailableException _ue;

            public execute_prepared_cql_queryResult()
            {
            }

            [DataMember(Order = 0)]
            public CqlResult Success
            {
                get { return _success; }
                set
                {
                    __isset.success = true;
                    this._success = value;
                }
            }

            [DataMember(Order = 0)]
            public InvalidRequestException Ire
            {
                get { return _ire; }
                set
                {
                    __isset.ire = true;
                    this._ire = value;
                }
            }

            [DataMember(Order = 0)]
            public UnavailableException Ue
            {
                get { return _ue; }
                set
                {
                    __isset.ue = true;
                    this._ue = value;
                }
            }

            [DataMember(Order = 0)]
            public TimedOutException Te
            {
                get { return _te; }
                set
                {
                    __isset.te = true;
                    this._te = value;
                }
            }

            [DataMember(Order = 0)]
            public SchemaDisagreementException Sde
            {
                get { return _sde; }
                set
                {
                    __isset.sde = true;
                    this._sde = value;
                }
            }

            public async Task ReadAsync(TProtocol iprot, CancellationToken cancellationToken)
            {
                iprot.IncrementRecursionDepth();
                try
                {
                    TField field;
                    await iprot.ReadStructBeginAsync(cancellationToken);
                    while (true)
                    {
                        field = await iprot.ReadFieldBeginAsync(cancellationToken);
                        if (field.Type == TType.Stop)
                        {
                            break;
                        }

                        switch (field.ID)
                        {
                            case 0:
                                if (field.Type == TType.Struct)
                                {
                                    Success = new CqlResult();
                                    await Success.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 1:
                                if (field.Type == TType.Struct)
                                {
                                    Ire = new InvalidRequestException();
                                    await Ire.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 2:
                                if (field.Type == TType.Struct)
                                {
                                    Ue = new UnavailableException();
                                    await Ue.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 3:
                                if (field.Type == TType.Struct)
                                {
                                    Te = new TimedOutException();
                                    await Te.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            case 4:
                                if (field.Type == TType.Struct)
                                {
                                    Sde = new SchemaDisagreementException();
                                    await Sde.ReadAsync(iprot, cancellationToken);
                                }
                                else
                                {
                                    await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                }
                                break;
                            default:
                                await TProtocolUtil.SkipAsync(iprot, field.Type, cancellationToken);
                                break;
                        }

                        await iprot.ReadFieldEndAsync(cancellationToken);
                    }

                    await iprot.ReadStructEndAsync(cancellationToken);
                }
                finally
                {
                    iprot.DecrementRecursionDepth();
                }
            }

            public async Task WriteAsync(TProtocol oprot, CancellationToken cancellationToken)
            {
                oprot.IncrementRecursionDepth();
                try
                {
                    var struc = new TStruct("execute_prepared_cql_query_result");
                    await oprot.WriteStructBeginAsync(struc, cancellationToken);
                    var field = new TField();

                    if (this.__isset.success)
                    {
                        if (Success != null)
                        {
                            field.Name = "Success";
                            field.Type = TType.Struct;
                            field.ID = 0;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Success.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ire)
                    {
                        if (Ire != null)
                        {
                            field.Name = "Ire";
                            field.Type = TType.Struct;
                            field.ID = 1;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ire.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.ue)
                    {
                        if (Ue != null)
                        {
                            field.Name = "Ue";
                            field.Type = TType.Struct;
                            field.ID = 2;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Ue.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.te)
                    {
                        if (Te != null)
                        {
                            field.Name = "Te";
                            field.Type = TType.Struct;
                            field.ID = 3;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Te.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    else if (this.__isset.sde)
                    {
                        if (Sde != null)
                        {
                            field.Name = "Sde";
                            field.Type = TType.Struct;
                            field.ID = 4;
                            await oprot.WriteFieldBeginAsync(field, cancellationToken);
                            await Sde.WriteAsync(oprot, cancellationToken);
                            await oprot.WriteFieldEndAsync(cancellationToken);
                        }
                    }
                    await oprot.WriteFieldStopAsync(cancellationToken);
                    await oprot.WriteStructEndAsync(cancellationToken);
                }
                finally
                {
                    oprot.DecrementRecursionDepth();
                }
            }

            public override string ToString()
            {
                var sb = new StringBuilder("execute_prepared_cql_query_result(");
                bool __first = true;
                if (Success != null && __isset.success)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Success: ");
                    sb.Append(Success == null ? "<null>" : Success.ToString());
                }
                if (Ire != null && __isset.ire)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ire: ");
                    sb.Append(Ire == null ? "<null>" : Ire.ToString());
                }
                if (Ue != null && __isset.ue)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Ue: ");
                    sb.Append(Ue == null ? "<null>" : Ue.ToString());
                }
                if (Te != null && __isset.te)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Te: ");
                    sb.Append(Te == null ? "<null>" : Te.ToString());
                }
                if (Sde != null && __isset.sde)
                {
                    if (!__first)
                    {
                        sb.Append(", ");
                    }
                    __first = false;
                    sb.Append("Sde: ");
                    sb.Append(Sde == null ? "<null>" : Sde.ToString());
                }
                sb.Append(")");
                return sb.ToString();
            }

            [DataContract]
            public struct Isset
            {
                [DataMember] public bool success;
                [DataMember] public bool ire;
                [DataMember] public bool ue;
                [DataMember] public bool te;
                [DataMember] public bool sde;
            }

            #region XmlSerializer support

            public bool ShouldSerializeSuccess()
            {
                return __isset.success;
            }

            public bool ShouldSerializeIre()
            {
                return __isset.ire;
            }

            public bool ShouldSerializeUe()
            {
                return __isset.ue;
            }

            public bool ShouldSerializeTe()
            {
                return __isset.te;
            }

            public bool ShouldSerializeSde()
            {
                return __isset.sde;
            }

            #endregion XmlSerializer support
        }
    }
}