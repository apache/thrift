class TConfiguration:
    DEFAULT_MAX_MESSAGE_SIZE = 100*1024*1024
    DEFAULT_MAX_FRAME_SIZE = 16384000
    DEFAULT_RECURSION_DEPTH = 64

    def __init__(self, maxMessageSize = DEFAULT_MAX_MESSAGE_SIZE, maxFrameSize = DEFAULT_MAX_FRAME_SIZE, recursionLimit =DEFAULT_RECURSION_DEPTH):
        self.maxMessageSize = maxMessageSize
        self.maxFrameSize = maxFrameSize
        self.recursionLimit = recursionLimit

    def getMaxMessageSize(self):
        return self.maxMessageSize

    def getMaxFrameSize(self):
        return self.maxFrameSize

    def getRecursionLimit(self):
        return self.recursionLimit
  
    def setMaxMessageSize(self, maxMessageSize):
        self.maxMessageSize = maxMessageSize

    def setMaxFrameSize(self, maxFrameSize):
        self.maxFrameSize = maxFrameSize

    def setRecursionLimit(self, recursionLimit):
        self.recursionLimit = recursionLimit
