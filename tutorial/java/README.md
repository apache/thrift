Thrift Java Tutorial
==================================================
1) Compile the Java library

    thrift/lib/java$ make
or:

    thrift/lib/java$ gradle assemble

4) Run the tutorial:

start server and client with one step:

    thrift/tutorial/java$ make tutorial
or:

    thrift/tutorial/java$ gradle tutorial

or run server and client in separate terminals:

    thrift/tutorial/java$ make tutorialserver
    thrift/tutorial/java$ make tutorialclient

or:

    thrift/tutorial/java$ gradle tutorialServer
    thrift/tutorial/java$ gradle tutorialClient
