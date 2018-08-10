@0xc59224e99b6789a9;

struct LinkedList {
  payload @0 :Int64;
  union {
    end @1 :Void;
    cons @2 :LinkedList;
  }
}

interface FooServer {
  get1 @0 () -> (result :BarServer);
  get2 @1 () -> (result :BarServer);
}

interface BarServer {
  get @0 () -> (result :Text);
}
