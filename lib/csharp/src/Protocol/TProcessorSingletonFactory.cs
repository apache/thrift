using System;

namespace Thrift.Protocol
{
  public class TProcessorSingletonFactory : TProcessorFactory
  {
    private readonly TProcessor _processor;

    public TProcessorSingletonFactory(TProcessor processor)
    {
      if (processor == null)
      {
          throw new ArgumentNullException("processor");
      }

      _processor = processor;
    }

    public TProcessor Create()
    {
      return _processor;
    }

    public void Release(TProcessor processor)
    {
    }
  }
}
