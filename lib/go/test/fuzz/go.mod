module github.com/apache/thrift/lib/go/test/fuzz

go 1.16

replace github.com/apache/thrift => ../../../../

replace shared => ./gen-go/shared

replace tutorial => ./gen-go/tutorial

require (
	github.com/apache/thrift v0.0.0-00010101000000-000000000000
	shared v0.0.0-00010101000000-000000000000
	tutorial v0.0.0-00010101000000-000000000000
)
