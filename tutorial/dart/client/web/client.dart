/// Licensed to the Apache Software Foundation (ASF) under one
/// or more contributor license agreements. See the NOTICE file
/// distributed with this work for additional information
/// regarding copyright ownership. The ASF licenses this file
/// to you under the Apache License, Version 2.0 (the
/// "License"); you may not use this file except in compliance
/// with the License. You may obtain a copy of the License at
///
/// http://www.apache.org/licenses/LICENSE-2.0
///
/// Unless required by applicable law or agreed to in writing,
/// software distributed under the License is distributed on an
/// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
/// KIND, either express or implied. See the License for the
/// specific language governing permissions and limitations
/// under the License.

import 'dart:html';

import 'package:thrift/thrift.dart';
import 'package:thrift/thrift_browser.dart';
import 'package:tutorial/tutorial.dart';

/// Adapted from the AS3 tutorial
void main() {
  new CalculatorUI(querySelector('#output')).start();
}

class CalculatorUI {
  final DivElement output;

  CalculatorUI(this.output);

  TTransport _transport;
  Calculator _calculatorClient;

  void start() {
    _buildInterface();
    _initConnection();
  }

  void _buildInterface() {
    output.children.forEach((e) {
      e.remove();
    });

    output.append(new BRElement());
    ButtonElement pingButton = new ButtonElement()
      ..text = "PING"
      ..onClick.listen(_onPingClick);
    output.append(pingButton);
    output.append(new BRElement());

    output.append(new BRElement());
    InputElement num1 = new InputElement()
      ..id = "add1"
      ..width = 50;
    output.append(num1);
    InputElement num2 = new InputElement()
      ..id = "add2"
      ..width = 50;
    output.append(num2);
    ButtonElement addButton = new ButtonElement()
      ..text = "ADD"
      ..onClick.listen(_onAddClick);
    output.append(addButton);
    output.append(new BRElement());
  }

  void _onPingClick(MouseEvent e) {
    _validate();

    _calculatorClient.ping();
  }

  void _onAddClick(MouseEvent e) {
    _validate();

    InputElement add1 = querySelector("#add1");
    InputElement add2 = querySelector("#add2");

    _calculatorClient
        .add(int.parse(add1.value), int.parse(add2.value))
        .then((int result) {
      window.alert("The answer is $result");
    });
  }

  void _validate() {
    if (!_transport.isOpen) {
      window.alert("The transport is not open!");
    }
  }

  void _initConnection() {
    _transport = new TSocketTransport(
        new TWebSocket(Uri.parse('ws://127.0.0.1:9090/ws')));
    TProtocol protocol =
        new TBinaryProtocol(_transport, strictRead: false, strictWrite: false);
    _transport.open();

    _calculatorClient = new CalculatorClient(protocol);
  }
}
