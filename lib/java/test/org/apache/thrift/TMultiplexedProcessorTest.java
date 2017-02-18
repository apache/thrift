package org.apache.thrift;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.apache.thrift.protocol.TMessage;
import org.apache.thrift.protocol.TMessageType;
import org.apache.thrift.protocol.TProtocol;
import org.junit.Before;
import org.junit.Test;

public class TMultiplexedProcessorTest {
	private TMultiplexedProcessor mp;
	private TProtocol iprot;
	private TProtocol oprot;

	@Before
	public void setUp() throws Exception {
		mp = new TMultiplexedProcessor();
		iprot = mock(TProtocol.class);
		oprot = mock(TProtocol.class);
	}

	@Test(expected = TException.class)
	public void noSuchService() throws TException {
		when(iprot.readMessageBegin()).thenReturn(new TMessage("service:func", TMessageType.CALL, 42));

		mp.process(iprot, oprot);
	}

	static class StubProcessor implements TProcessor {
		@Override
		public boolean process(TProtocol in, TProtocol out) throws TException {
			TMessage msg = in.readMessageBegin();
			if (!"func".equals(msg.name) || msg.type!=TMessageType.CALL || msg.seqid!=42) {
				throw new TException("incorrect parameters");
			}
			out.writeMessageBegin(new TMessage("func", TMessageType.REPLY, 42));
			return true;
		}
		
	}

	@Test
	public void existingService() throws TException {
		when(iprot.readMessageBegin()).thenReturn(new TMessage("service:func", TMessageType.CALL, 42));
		mp.registerProcessor("service", new StubProcessor());
		mp.process(iprot, oprot);
		verify(oprot).writeMessageBegin(any(TMessage.class));
	}

	@Test
	public void defaultService() throws TException {
		when(iprot.readMessageBegin()).thenReturn(new TMessage("func", TMessageType.CALL, 42));
		mp.registerDefault(new StubProcessor());
		mp.process(iprot, oprot);
		verify(oprot).writeMessageBegin(any(TMessage.class));
	}

}
