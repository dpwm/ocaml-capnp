@0xc59224e99b6789a9;

using import "test/test_schema_2.capnp".LinkedList;

struct FooTest {
  ll @0 :LinkedList;
}

interface FooServer {
  get1 @0 () -> (result :BarServer);
  get2 @1 () -> (result :BarServer);
  get3 @2 () -> LinkedList;
}

interface BarServer {
  get @0 () -> (result :Text);
}
