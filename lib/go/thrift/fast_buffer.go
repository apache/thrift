package thrift

import (
	"fmt"
	"unsafe"
)

type FastFrameBuffer struct {
	w         *TFramedTransport //
	b         []byte
	wIdx      int               // write-offset, reset when Flush
	frameSize int
	rIdx      int     // read-offset, reset when read-new-frame
	buf       [4]byte // only for write&read data-len
}

func NewFastBuffer(w *TFramedTransport, size int) *FastFrameBuffer {
	return &FastFrameBuffer{
		w:    w,
		b:    make([]byte, size),
		rIdx: 0,
		wIdx: 0,
	}
}

func (p *FastFrameBuffer) WritByte(b int8) {
	if p.wIdx+1 > p.Cap() {
		p.grow(2*p.Cap() + 1)
	}
	p.b[p.wIdx] = byte(b)
	p.wIdx += 1
}

func (p *FastFrameBuffer) WriteBytes(b []byte) {
	if p.wIdx+len(b) > p.Cap() {
		p.grow(2*p.Cap() + len(b))
	}
	copy(p.b[p.wIdx:], b)
	p.wIdx += len(b)
}

func (p *FastFrameBuffer) WriteI16(i uint16) {
	if p.wIdx+2 > p.Cap() {
		p.grow(2*p.Cap() + 2)
	}
	copy(p.b[p.wIdx:], []byte{byte(i >> 8), byte(i)})
	p.wIdx += 2
}

func (p *FastFrameBuffer) WriteI32(i uint32) {
	if p.wIdx+4 > p.Cap() {
		p.grow(2*p.Cap() + 4)
	}
	copy(p.b[p.wIdx:], []byte{byte(i >> 24), byte(i >> 16), byte(i >> 8), byte(i)})
	p.wIdx += 4
}

func (p *FastFrameBuffer) WriteI64(i uint64) {
	if p.wIdx+8 > p.Cap() {
		p.grow(2*p.Cap() + 8)
	}
	copy(p.b[p.wIdx:], []byte{byte(i >> 56), byte(i >> 48), byte(i >> 40), byte(i >> 32), byte(i >> 24), byte(i >> 16), byte(i >> 8), byte(i)})
	p.wIdx += 8
}

func (p *FastFrameBuffer) WriteString(s string) {
	if p.wIdx+len(s) > p.Cap() {
		p.grow(2*p.Cap() + len(s))
	}
	copy(p.b[p.wIdx:], str2bytes(s))
	p.wIdx += len(s)
}

func (p *FastFrameBuffer) Len() int {
	return len(p.b)
}

func (p *FastFrameBuffer) Cap() int {
	return cap(p.b)
}

func (p *FastFrameBuffer) Flush() error {
	p.buf[0] = byte(p.wIdx >> 24)
	p.buf[1] = byte(p.wIdx >> 16)
	p.buf[2] = byte(p.wIdx >> 8)
	p.buf[3] = byte(p.wIdx)
	_, err := p.w.transport.Write(p.buf[:4])

	if err != nil {
		return fmt.Errorf("Flush Write-Len failed, err: %v\n", err)
	}
	_, err = p.w.transport.Write(p.b[:p.wIdx])
	if err != nil {
		return fmt.Errorf("Flush Write-Dat failed, err: %v\n", err)
	}
	p.ResetWriter()
	p.w.transport.Flush()
	return nil
}

func (p *FastFrameBuffer) ResetWriter() {
	p.wIdx = 0
}

func (p *FastFrameBuffer) ResetReader() {
	p.rIdx = 0
}

func (p *FastFrameBuffer) grow(n int) {
	b := make([]byte, n)
	copy(b, p.b[0:])
	p.b = b
}

func (p *FastFrameBuffer) ReadByte() (c byte, err error) {
	if p.frameSize == 0 {
		p.frameSize, err = p.readFrameHeader()
		if err != nil {
			return
		}
		_, err = p.readAll(p.frameSize)
		if err != nil {
			return
		}
	}
	if p.frameSize < 1 {
		return 0, fmt.Errorf("Not enought frame size %d to read %d bytes", p.frameSize, 1)
	}
	c = p.b[p.rIdx]
	if err == nil {
		p.frameSize--
		p.rIdx += 1
	}
	return
}

// maybe read-bytes means ReadN
func (p *FastFrameBuffer) ReadN(num int) (b []byte, err error) {
	if p.frameSize == 0 {
		p.frameSize, err = p.readFrameHeader()
		if err != nil {
			return
		}
		_, err = p.readAll(p.frameSize)
		if err != nil {
			return
		}
	}
	if p.frameSize < num {
		return nil, fmt.Errorf("Not enought frame size %d to read %d bytes", p.frameSize, num)
	}
	b = p.b[p.rIdx : p.rIdx+num]
	p.frameSize = p.frameSize - num
	if p.frameSize < 0 {
		return nil, fmt.Errorf("Negative frame size")
	}
	p.rIdx += num
	return b, nil
}

func (p *FastFrameBuffer) ReadI64() (num int64, err error) {
	if p.frameSize == 0 {
		p.frameSize, err = p.readFrameHeader()
		if err != nil {
			return
		}
		_, err = p.readAll(p.frameSize)
		if err != nil {
			return
		}

	}
	if p.frameSize < 8 {
		return 0, fmt.Errorf("Not enought frame size %d to read %d bytes", p.frameSize, 2)
	}
	num = int64(uint64(p.b[p.rIdx+7]) | uint64(p.b[p.rIdx+6])<<8 | uint64(p.b[p.rIdx+5])<<16 | uint64(p.b[p.rIdx+4])<<24 | uint64(p.b[p.rIdx+3])<<32 | uint64(p.b[p.rIdx+2])<<40 | uint64(p.b[p.rIdx+1])<<48 | uint64(p.b[p.rIdx])<<56)
	p.frameSize = p.frameSize - 8
	p.rIdx += 8
	return num, nil
}

func (p *FastFrameBuffer) ReadI32() (num int32, err error) {
	if p.frameSize == 0 {
		p.frameSize, err = p.readFrameHeader()
		if err != nil {
			return
		}
		_, err = p.readAll(p.frameSize)
		if err != nil {
			return
		}
	}
	if p.frameSize < 4 {
		return 0, fmt.Errorf("Not enought frame size %d to read %d bytes", p.frameSize, 2)
	}
	num = int32(uint32(p.b[p.rIdx+3]) | uint32(p.b[p.rIdx+2])<<8 | uint32(p.b[p.rIdx+1])<<16 | uint32(p.b[p.rIdx])<<24)
	p.frameSize = p.frameSize - 4
	p.rIdx += 4
	return num, nil
}

func (p *FastFrameBuffer) ReadI16() (num int16, err error) {
	if p.frameSize == 0 {
		p.frameSize, err = p.readFrameHeader()
		if err != nil {
			return
		}
		_, err = p.readAll(p.frameSize)
		if err != nil {
			return
		}
	}
	if p.frameSize < 2 {
		return 0, fmt.Errorf("Not enought frame size %d to read %d bytes", p.frameSize, 2)
	}
	num = int16(uint16(p.b[p.rIdx+1]) | uint16(p.b[p.rIdx])<<8)
	p.frameSize = p.frameSize - 2
	p.rIdx += 2
	return num, nil
}

func (p *FastFrameBuffer) readFrameHeader() (int, error) {
	p.ResetReader()
	if _, err := p.w.transport.Read(p.buf[:4]); err != nil {
		return 0, err
	}
	frameSize := int(uint32(p.buf[3]) | uint32(p.buf[2])<<8 | uint32(p.buf[1])<<16 | uint32(p.buf[0])<<24)
	if frameSize < 0 || frameSize > DEFAULT_MAX_LENGTH {
		return 0, fmt.Errorf("Incorrect frame size (%d)", frameSize)
	}
	return frameSize, nil
}

// TODO 这个方法 是否会有问题？ block? or?
func (p *FastFrameBuffer) readAll(num int) (int, error) {
	// 当容量不足以装下所有数据的时候, 重新申请一下
	if cap(p.b) < num {
		rbNew := make([]byte, 2*cap(p.b)+num)
		copy(rbNew, p.b)
		p.b = rbNew
	}
	var i = 0
	for i < num {
		l, err := p.w.transport.Read(p.b[i:])
		if err != nil {
			return 0, err
		}
		i += l
	}
	return i, nil
}

func str2bytes(s string) []byte {
	x := (*[2]uintptr)(unsafe.Pointer(&s))
	h := [3]uintptr{x[0], x[1], x[1]}
	return *(*[]byte)(unsafe.Pointer(&h))
}
