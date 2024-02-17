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

import 'package:shared/shared.dart';
import 'package:tutorial/tutorial.dart';

import 'package:thrift/thrift.dart';
import 'package:thrift/thrift_browser.dart';

/// Adapted from the AS3 tutorial
void main() {
  new CalculatorUI(querySelector('#output') as DivElement).start();
}

class CalculatorUI {
  final DivElement output;

  CalculatorUI(this.output);

  late TTransport _transport;
  late Calculator _calculatorClient;

  void start() {
    _buildInterface();
    _initConnection();
  }

  void _validate() {
    if (!_transport.isOpen) {
      window.alert("The transport is not open!");
    }
  }

  void _initConnection() {
    _transport = new TAsyncClientSocketTransport(
        new TWebSocket(Uri.parse('ws://127.0.0.1:9090/ws')),
        new TMessageReader(new TBinaryProtocolFactory()));
    TProtocol protocol = new TBinaryProtocol(_transport);
    _transport.open();

    _calculatorClient = new CalculatorClient(protocol);
  }

  void _buildInterface() {
    output.children.forEach((e) {
      e.remove();
    });

    _buildPingComponent();

    _buildAddComponent();

    _buildCalculatorComponent();

    _buildGetStructComponent();
  }

  void _buildPingComponent() {
    output.append(new HeadingElement.h3()..text = "Ping");
    ButtonElement pingButton = new ButtonElement()
      ..text = "PING"
      ..onClick.listen(_onPingClick);
    output.append(pingButton);
  }

  void _onPingClick(MouseEvent e) {
    _validate();

    _calculatorClient.ping();
  }

  void _buildAddComponent() {
    output.append(new HeadingElement.h3()..text = "Add");
    InputElement num1 = new InputElement()
      ..id = "add1"
      ..type = "number"
      ..style.fontSize = "14px"
      ..style.width = "50px";
    output.append(num1);
    SpanElement op = new SpanElement()
      ..text = "+"
      ..style.fontSize = "14px"
      ..style.marginLeft = "10px";
    output.append(op);
    InputElement num2 = new InputElement()
      ..id = "add2"
      ..type = "number"
      ..style.fontSize = "14px"
      ..style.width = "50px"
      ..style.marginLeft = "10px";
    output.append(num2);
    ButtonElement addButton = new ButtonElement()
      ..text = "="
      ..style.fontSize = "14px"
      ..style.marginLeft = "10px"
      ..onClick.listen(_onAddClick);
    output.append(addButton);
    SpanElement result = new SpanElement()
      ..id = "addResult"
      ..style.fontSize = "14px"
      ..style.marginLeft = "10px";
    output.append(result);
  }

  void _onAddClick(MouseEvent e) {
    _validate();

    final num1 = querySelector("#add1") as InputElement;
    final num2 = querySelector("#add2") as InputElement;
    final result = querySelector("#addResult") as SpanElement;

    _calculatorClient
        .add(int.parse(num1.value ?? "0"), int.parse(num2.value ?? "0"))
        .then((int n) {
      result.text = "$n";
    });
  }

  void _buildCalculatorComponent() {
    output.append(new HeadingElement.h3()..text = "Calculator");
    InputElement num1 = new InputElement()
      ..id = "calc1"
      ..type = "number"
      ..style.fontSize = "14px"
      ..style.width = "50px";
    output.append(num1);
    SelectElement op = new SelectElement()
      ..id = "calcOp"
      ..multiple = false
      ..selectedIndex = 0
      ..style.fontSize = "16px"
      ..style.marginLeft = "10px"
      ..style.width = "50px";
    OptionElement addOp = new OptionElement()
      ..text = "+"
      ..value = Operation.ADD.toString();
    op.add(addOp, 0);
    OptionElement subtractOp = new OptionElement()
      ..text = "-"
      ..value = Operation.SUBTRACT.toString();
    op.add(subtractOp, 1);
    OptionElement multiplyOp = new OptionElement()
      ..text = "*"
      ..value = Operation.MULTIPLY.toString();
    op.add(multiplyOp, 2);
    OptionElement divideOp = new OptionElement()
      ..text = "/"
      ..value = Operation.DIVIDE.toString();
    op.add(divideOp, 3);
    output.append(op);
    InputElement num2 = new InputElement()
      ..id = "calc2"
      ..type = "number"
      ..style.fontSize = "14px"
      ..style.width = "50px"
      ..style.marginLeft = "10px";
    output.append(num2);
    ButtonElement calcButton = new ButtonElement()
      ..text = "="
      ..style.fontSize = "14px"
      ..style.marginLeft = "10px"
      ..onClick.listen(_onCalcClick);
    output.append(calcButton);
    SpanElement result = new SpanElement()
      ..id = "calcResult"
      ..style.fontSize = "14px"
      ..style.marginLeft = "10px";
    output.append(result);
    output.append(new BRElement());
    output.append(new BRElement());
    LabelElement logIdLabel = new LabelElement()
      ..text = "Log ID:"
      ..style.fontSize = "14px";
    output.append(logIdLabel);
    InputElement logId = new InputElement()
      ..id = "logId"
      ..type = "number"
      ..value = "1"
      ..style.fontSize = "14px"
      ..style.width = "50px"
      ..style.marginLeft = "10px";
    output.append(logId);
    LabelElement commentLabel = new LabelElement()
      ..text = "Comment:"
      ..style.fontSize = "14px"
      ..style.marginLeft = "10px";
    output.append(commentLabel);
    InputElement comment = new InputElement()
      ..id = "comment"
      ..style.fontSize = "14px"
      ..style.width = "100px"
      ..style.marginLeft = "10px";
    output.append(comment);
  }

  void _onCalcClick(MouseEvent e) {
    _validate();

    final num1 = querySelector("#calc1") as InputElement;
    final num2 = querySelector("#calc2") as InputElement;
    final op = querySelector("#calcOp") as SelectElement;
    final result = querySelector("#calcResult") as SpanElement;
    final logId = querySelector("#logId") as InputElement;
    final comment = querySelector("#comment") as InputElement;

    int logIdValue = int.parse(logId.value!);
    logId.value = (logIdValue + 1).toString();

    Work work = new Work();
    work.num1 = int.parse(num1.value!);
    work.num2 = int.parse(num2.value!);
    work.op = int.parse(op.options[op.selectedIndex!].value);
    work.comment = comment.value!;

    _calculatorClient.calculate(logIdValue, work).then((int n) {
      result.text = "$n";
    });
  }

  void _buildGetStructComponent() {
    output.append(new HeadingElement.h3()..text = "Get Struct");
    LabelElement logIdLabel = new LabelElement()
      ..text = "Struct Key:"
      ..style.fontSize = "14px";
    output.append(logIdLabel);
    InputElement logId = new InputElement()
      ..id = "structKey"
      ..type = "number"
      ..value = "1"
      ..style.fontSize = "14px"
      ..style.width = "50px"
      ..style.marginLeft = "10px";
    output.append(logId);
    ButtonElement getStructButton = new ButtonElement()
      ..text = "GET"
      ..style.fontSize = "14px"
      ..style.marginLeft = "10px"
      ..onClick.listen(_onGetStructClick);
    output.append(getStructButton);
    output.append(new BRElement());
    output.append(new BRElement());
    TextAreaElement result = new TextAreaElement()
      ..id = "getStructResult"
      ..style.fontSize = "14px"
      ..style.width = "300px"
      ..style.height = "50px"
      ..style.marginLeft = "10px";
    output.append(result);
  }

  void _onGetStructClick(MouseEvent e) {
    _validate();

    InputElement structKey = querySelector("#structKey") as InputElement;
    TextAreaElement result = querySelector("#getStructResult") as TextAreaElement;

    _calculatorClient
        .getStruct(int.parse(structKey.value!))
        .then((SharedStruct s) {
      result.text = "${s.toString()}";
    });
  }
}
