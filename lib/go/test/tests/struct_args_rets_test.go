package tests

import (
	st "ServicesTest"
)

//this function is never called, it will fail to compile if check is failed
func staticCheckStructArgsResults() {
	//Check that struct args and results are passed by reference
	var sa *st.StructA = &st.StructA{}
	var iface st.AServ
	var err error

	sa, err = iface.StructAFunc_1structA(sa)
	_ = err
	_ = sa
}
