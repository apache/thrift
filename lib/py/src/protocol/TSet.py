from thrift.protocol.TList import TList 
class TSet(TList):
    
    def __init__(self, t=TList().elemType, s=TList().size):
        self.elemType = t
        self.size = s
