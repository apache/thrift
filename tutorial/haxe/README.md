Thrift Haxe Tutorial
==================================================
1) Compile the library

    make

2) TCP/Socket transport:

server:

    make tutorialserver 

or
    
    make tutorialserver_php  


client: 
    
    ```
    make tutorialclient
    make tutorialclient_php
    ```

3) http transport 

server:

    make tutorialserver_php_http

client:

    ```
    make tutorialclient_http
    make tutorialclient_nodejs_http_json
    make tutorialclient_nodejs_http
    make tutorialclient_php_http
    ```

for browser side javascript open http://localhost:9090/javascript.html


