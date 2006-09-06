#ifndef _THRIFT_PROTOCOL_TBINARYPROTOCOL_H_
#define _THRIFT_PROTOCOL_TBINARYPROTOCOL_H_ 1

#include "TProtocol.h"

#include <boost/shared_ptr.hpp>

namespace facebook { namespace thrift { namespace protocol { 

using namespace boost;

/**
 * The default binary protocol for thrift. Writes all data in a very basic
 * binary format, essentially just spitting out the raw bytes.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
    class TBinaryProtocol : public TProtocol {
 public:
  TBinaryProtocol() {}
  ~TBinaryProtocol() {}

  /**
   * Writing functions.
   */

  virtual uint32_t writeMessageBegin(shared_ptr<TTransport> out,
				     const std::string name,
				     const TMessageType messageType,
				     const int32_t seqid) const;

  virtual uint32_t writeMessageEnd(shared_ptr<TTransport> out) const;


  uint32_t writeStructBegin(shared_ptr<TTransport> out,
			     const std::string& name) const;

  uint32_t writeStructEnd(shared_ptr<TTransport> out) const;

  uint32_t writeFieldBegin(shared_ptr<TTransport> out,
			    const std::string& name,
			    const TType fieldType,
			    const int16_t fieldId) const;

  uint32_t writeFieldEnd(shared_ptr<TTransport> out) const;

  uint32_t writeFieldStop(shared_ptr<TTransport> out) const;
                                       
  uint32_t writeMapBegin(shared_ptr<TTransport> out,
			  const TType keyType,
			  const TType valType,
			  const uint32_t size) const;

  uint32_t writeMapEnd(shared_ptr<TTransport> out) const;

  uint32_t writeListBegin(shared_ptr<TTransport> out,
			   const TType elemType,
			   const uint32_t size) const;

  uint32_t writeListEnd(shared_ptr<TTransport> out) const;

  uint32_t writeSetBegin(shared_ptr<TTransport> out,
			  const TType elemType,
			  const uint32_t size) const;

  uint32_t writeSetEnd(shared_ptr<TTransport> out) const;

  uint32_t writeBool(shared_ptr<TTransport> out,
		      const bool value) const;

  uint32_t writeByte(shared_ptr<TTransport> out,
		      const int8_t byte) const;

  uint32_t writeI16(shared_ptr<TTransport> out,
		     const int16_t i16) const;

  uint32_t writeI32(shared_ptr<TTransport> out,
		     const int32_t i32) const;

  uint32_t writeI64(shared_ptr<TTransport> out,
		     const int64_t i64) const;

  uint32_t writeDouble(shared_ptr<TTransport> out,
                       const double dub) const;


  uint32_t writeString(shared_ptr<TTransport> out,
			const std::string& str) const;

  /**
   * Reading functions
   */


  uint32_t readMessageBegin(shared_ptr<TTransport> in,
			    std::string& name,
			    TMessageType& messageType,
			    int32_t& seqid) const;

  uint32_t readMessageEnd(shared_ptr<TTransport> in) const;

  uint32_t readStructBegin(shared_ptr<TTransport> in,
			    std::string& name) const;

  uint32_t readStructEnd(shared_ptr<TTransport> in) const;

  uint32_t readFieldBegin(shared_ptr<TTransport> in,
			   std::string& name,
			   TType& fieldType,
			   int16_t& fieldId) const;
  
  uint32_t readFieldEnd(shared_ptr<TTransport> in) const;
 
  uint32_t readMapBegin(shared_ptr<TTransport> in,
			 TType& keyType,
			 TType& valType,
			 uint32_t& size) const;

  uint32_t readMapEnd(shared_ptr<TTransport> in) const;

  uint32_t readListBegin(shared_ptr<TTransport> in,
			  TType& elemType,
			  uint32_t& size) const;
  
  uint32_t readListEnd(shared_ptr<TTransport> in) const;

  uint32_t readSetBegin(shared_ptr<TTransport> in,
			 TType& elemType,
			 uint32_t& size) const;

  uint32_t readSetEnd(shared_ptr<TTransport> in) const;

  uint32_t readBool(shared_ptr<TTransport> in,
		      bool& value) const;

  uint32_t readByte(shared_ptr<TTransport> in,
                    int8_t& byte) const;

  uint32_t readI16(shared_ptr<TTransport> in,
		    int16_t& i16) const;

  uint32_t readI32(shared_ptr<TTransport> in,
		    int32_t& i32) const;

  uint32_t readI64(shared_ptr<TTransport> in,
		    int64_t& i64) const;

  uint32_t readDouble(shared_ptr<TTransport> in,
                      double& dub) const;

  uint32_t readString(shared_ptr<TTransport> in,
		       std::string& str) const;
};

}}} // facebook::thrift::protocol

#endif // #ifndef _THRIFT_PROTOCOL_TBINARYPROTOCOL_H_


