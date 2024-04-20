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
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Thrift
{
    public class TThriftUri
    {
        public const string THRIFT_URI_SCHEME = "thrift://";

        public readonly string Protocol;
        public readonly string EndpointTransport;
        public readonly List<string> LayeredTransports = new List<string>();
        public readonly Dictionary<string,string> QueryData = new Dictionary<string, string>();

        public TThriftUri(string protocol, string endpointTransport, Dictionary<string, string> data)
        {
            Protocol = (protocol ?? "").Trim();
            EndpointTransport = (endpointTransport ?? "").Trim();
            if(data!=null)
                foreach (var pair in data)
                    QueryData.Add(pair.Key, pair.Value);
        }

        public TThriftUri(string sThriftUri)
        {
            if (!sThriftUri.StartsWith(THRIFT_URI_SCHEME))
                throw new TApplicationException(TApplicationException.ExceptionType.ProtocolError, "Invalid URI: " + THRIFT_URI_SCHEME + " expected");

            // split path and query
            sThriftUri = sThriftUri.Remove(0, THRIFT_URI_SCHEME.Length);
            var pieces = sThriftUri.Split('?');
            var sPath = pieces[0];
            var sQuery = string.Join("?", pieces.Skip(1).ToArray());

            // analyze path
            // thrift://protocol/transport/layer/layer?data
            pieces = sPath.Split('/');
            if (pieces.Length < 2)
                throw new TApplicationException(TApplicationException.ExceptionType.ProtocolError, "Invalid URI: not enough data");
            Protocol = pieces[0].Trim();
            EndpointTransport = pieces[1].Trim();
            for (int i = 2; i < pieces.Length; i++)
                LayeredTransports.Add(pieces[i].Trim());

            // analyze query data
            if (!string.IsNullOrEmpty(sQuery))
            {
                pieces = sQuery.Split('&');
                foreach (var piece in pieces)
                {
                    var pair = piece.Split('=');
                    var sKey = Uri.UnescapeDataString(pair[0]);
                    var sValue = Uri.UnescapeDataString(string.Join("=", pair.Skip(1).ToArray()));
                    QueryData.Add(sKey, sValue);
                }
            }

            Validate();
        }

        public override string ToString()
        {
            var sb = new StringBuilder();
            sb.Append(THRIFT_URI_SCHEME);
            sb.Append(Protocol);
            sb.Append('/');
            sb.Append(EndpointTransport);

            foreach (var layer in LayeredTransports)
            {
                sb.Append('/');
                sb.Append(layer.Trim());
            }

            if (QueryData.Count > 0)
            {
                var kvpair = new List<string>();
                foreach (var pair in QueryData)
                {
                    var sTmp = Uri.EscapeDataString(pair.Key);
                    if (!string.IsNullOrEmpty(pair.Value))
                        sTmp += "=" + Uri.EscapeDataString(pair.Value);
                    kvpair.Add(sTmp);
                }
                sb.Append('?');
                sb.Append(string.Join("&", kvpair));
            }

            return sb.ToString();
        }


        internal void Validate()
        {
            if (string.IsNullOrEmpty(Protocol))
                throw new TApplicationException(TApplicationException.ExceptionType.ProtocolError, "Invalid URI: Protocol missing");
            if (string.IsNullOrEmpty(EndpointTransport))
                throw new TApplicationException(TApplicationException.ExceptionType.ProtocolError, "Invalid URI: Endpoint transport missing");
        }
    }

}
