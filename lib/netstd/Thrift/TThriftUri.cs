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
using System.Diagnostics;
using System.Linq;
using System.Text;

//#pragma warning disable IDE0028  // net8 only
//#pragma warning disable IDE0074  // net8 only

namespace Thrift
{
    public class TThriftUri
    {
        // RFC 3986: If a URI contains an authority component, then the path component
        // must either be empty or begin with a slash("/") character.If a URI
        // does not contain an authority component, then the path cannot begin
        // with two slash characters("//").  
        public const string THRIFT_URI_SCHEME = "thrift:";

        public readonly string Protocol;
        public readonly string Transport;
        public readonly List<KeyValuePair<string, Dictionary<string, string>>> Layers = new List<KeyValuePair<string, Dictionary<string, string>>>();
        public readonly Dictionary<string,string> QueryData = new Dictionary<string, string>();

        public TThriftUri(string protocol, string endpointTransport, Dictionary<string, string> data)
        {
            Protocol = (protocol ?? "").Trim();
            Transport = (endpointTransport ?? "").Trim();
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
            // - rather simple:  thrift://protocol/transport?data
            // - sophisticated:  thrift://protocol/transport/mplex:name/framed/zlib?data

            // it starts with protocol and endpoint transport
            pieces = sPath.Split('/');
            if (pieces.Length < 2)
                throw new TApplicationException(TApplicationException.ExceptionType.ProtocolError, "Invalid URI: not enough data");
            Protocol = pieces[0].Trim();
            Transport = pieces[1].Trim();

            // any layers on top, which can have parameters too
            // it is perfectly legal to mix transport layers and protocol decorators here
            // the canonical form is protocol decorators first, transport layers second
            for (int i = 2; i < pieces.Length; i++)
            {
                var layer = pieces[i].Trim().Split(':');
                var sKey = Uri.UnescapeDataString(layer[0]);
                var sValue = Uri.UnescapeDataString(string.Join(":", layer.Skip(1).ToArray()));
                var args = ParseArgumentList(sValue);
                Layers.Add(new KeyValuePair<string, Dictionary<string,string>>(sKey, args));
            }

            // analyze query data
            if (!string.IsNullOrEmpty(sQuery))
                ParseArgumentList(sQuery, QueryData);

            // validate the data
            Validate();
        }

        public override string ToString()
        {
            var sb = new StringBuilder();
            sb.Append(THRIFT_URI_SCHEME);
            sb.Append(Protocol);
            sb.Append('/');
            sb.Append(Transport);

            foreach (var layer in Layers)
            {
                sb.Append('/');
                sb.Append(Uri.EscapeDataString(layer.Key));
                if (layer.Value?.Count > 0)
                {
                    sb.Append(':');
                    sb.Append(ArgumentListToString(layer.Value));
                }
            }

            if (QueryData.Count > 0)
            {
                sb.Append('?');
                sb.Append(ArgumentListToString(QueryData));
            }

            return sb.ToString();
        }


        private static Dictionary<string, string> ParseArgumentList(string sQuery, Dictionary<string, string> args = null)
        {
            // args can be null or an existing dictionary
            if( args == null)
                args = new Dictionary<string, string>();
            Debug.Assert(args.Count == 0);

            if (!string.IsNullOrEmpty(sQuery))
            {
                var pieces = sQuery.Split('&');
                foreach (var piece in pieces)
                {
                    var pair = piece.Split('=');
                    var sKey = Uri.UnescapeDataString(pair[0]);
                    var sValue = Uri.UnescapeDataString(string.Join("=", pair.Skip(1).ToArray()));
                    args.Add(sKey, sValue);
                }
            }

            return args;
        }

        private static string ArgumentListToString(Dictionary<string, string> arguments)
        {
            var pieces = new List<string>();
            foreach (var pair in arguments)
            {
                var sTmp = Uri.EscapeDataString(pair.Key);
                if (!string.IsNullOrEmpty(pair.Value))
                    sTmp += "=" + Uri.EscapeDataString(pair.Value);
                pieces.Add(sTmp);
            }

            return string.Join("&", pieces);
        }


        internal void Validate()
        {
            if (string.IsNullOrEmpty(Protocol))
                throw new TApplicationException(TApplicationException.ExceptionType.ProtocolError, "Invalid URI: Protocol missing");
            if (string.IsNullOrEmpty(Transport))
                throw new TApplicationException(TApplicationException.ExceptionType.ProtocolError, "Invalid URI: Endpoint transport missing");
        }
    }

}
