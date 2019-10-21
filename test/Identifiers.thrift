// THRIFT-4953
struct NoFieldIdentifiersTest {
  string field_without_id1,
  string field_without_id2,
  1: string field_with_id
}