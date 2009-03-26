#!/usr/bin/env python
import os
import BaseHTTPServer
import CGIHTTPServer

# chdir(2) into the tutorial directory.
os.chdir(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))

class Handler(CGIHTTPServer.CGIHTTPRequestHandler):
  cgi_directories  = ['/php']

BaseHTTPServer.HTTPServer(('', 8080), Handler).serve_forever()
