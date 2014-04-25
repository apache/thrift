package common

import (
	"errors"
	. "gen/ThriftTest"
	"time"
)

var SimpleHandler = &simpleHandler{}

type simpleHandler struct{}

func (p *simpleHandler) TestVoid() (err error) {
	return nil
}

func (p *simpleHandler) TestString(thing string) (r string, err error) {
	return thing, nil
}

func (p *simpleHandler) TestByte(thing int8) (r int8, err error) {
	return thing, nil
}

func (p *simpleHandler) TestI32(thing int32) (r int32, err error) {
	return thing, nil
}

func (p *simpleHandler) TestI64(thing int64) (r int64, err error) {
	return thing, nil
}

func (p *simpleHandler) TestDouble(thing float64) (r float64, err error) {
	return thing, nil
}

func (p *simpleHandler) TestStruct(thing *Xtruct) (r *Xtruct, err error) {
	return r, err
}

func (p *simpleHandler) TestNest(nest *Xtruct2) (r *Xtruct2, err error) {
	return nest, nil
}

func (p *simpleHandler) TestMap(thing map[int32]int32) (r map[int32]int32, err error) {
	return thing, nil
}

func (p *simpleHandler) TestStringMap(thing map[string]string) (r map[string]string, err error) {
	return thing, nil
}

func (p *simpleHandler) TestSet(thing map[int32]bool) (r map[int32]bool, err error) {
	return thing, nil
}

func (p *simpleHandler) TestList(thing []int32) (r []int32, err error) {
	return thing, nil
}

func (p *simpleHandler) TestEnum(thing Numberz) (r Numberz, err error) {
	return thing, nil
}

func (p *simpleHandler) TestTypedef(thing UserId) (r UserId, err error) {
	return thing, nil
}

func (p *simpleHandler) TestMapMap(hello int32) (r map[int32]map[int32]int32, err error) {

	r = map[int32]map[int32]int32{
		-4: map[int32]int32{-4: -4, -3: -3, -2: -2, -1: -1},
		4:  map[int32]int32{4: 4, 3: 3, 2: 2, 1: 1},
	}
	return
}

func (p *simpleHandler) TestInsanity(argument *Insanity) (r map[UserId]map[Numberz]*Insanity, err error) {
	return nil, errors.New("No Insanity")
}

func (p *simpleHandler) TestMulti(arg0 int8, arg1 int32, arg2 int64, arg3 map[int16]string, arg4 Numberz, arg5 UserId) (r *Xtruct, err error) {
	r = NewXtruct()

	r.StringThing = "Hello2"
	r.ByteThing = arg0
	r.I32Thing = arg1
	r.I64Thing = arg2
	return
}

func (p *simpleHandler) TestException(arg string) (err error) {
	switch arg {
	case "Xception":
		e := NewXception()
		e.ErrorCode = 1001
		e.Message = arg
		return e
	case "TException":
		return errors.New("Just TException")
	}
	return
}

func (p *simpleHandler) TestMultiException(arg0 string, arg1 string) (r *Xtruct, err error) {
	switch arg0 {

	case "Xception":
		e := NewXception()
		e.ErrorCode = 1001
		e.Message = "This is an Xception"
		return nil, e
	case "Xception2":
		e := NewXception2()
		e.ErrorCode = 2002
		e.StructThing.StringThing = "This is an Xception2"
		return nil, e
	default:
		r = NewXtruct()
		r.StringThing = arg1
		return
	}
}

func (p *simpleHandler) TestOneway(secondsToSleep int32) (err error) {
	time.Sleep(time.Second * time.Duration(secondsToSleep))
	return
}
