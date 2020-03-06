import Foundation
import Thrift
import Common

class TestServer {
  func run() throws {
    let parameters = try TestServerParameters(arguments: CommandLine.arguments)

    if parameters.showHelp {
      parameters.printHelp()
      return
    }

    let service = ThriftTestImpl()
    let processor = ThriftTestProcessor(service: service)
    
    switch parameters.proto {
      case .binary:
        let proto = TBinaryProtocol.self
      case .compact:
        let proto = TCompactProtocol.self
      default:
        throw ParserError.unsupportedOption
    }

    switch parameters.transport {
      case .buffered:
        let server = try TSocketServer(port: parameters.port!, inProtocol: proto, outProtocol: proto, processor: processor)
      case .framed:
        let server = try TFramedServer(port: parameters.port!, inProtocol: proto, outProtocol: proto, processor: processor)
      default:
        throw ParserError.unsupportedOption
    }

    //dispatchMain()
    //RunLoop.current.run()
    let theRL = RunLoop.current
    while theRL.run(mode: .default, before: .distantFuture) {}
  }
}

try TestServer().run()
