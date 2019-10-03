@0x8bc3ce8730f1232d;

struct Test {
  id @0 :UInt64;
  name @1 :Text;
}

const test :Test = (id=12111485094725311920, name="Test");

struct TestContainer {
  contents @0 :List(Test);
  list8 @1 :List(UInt8);
  list16 @2 :List(UInt16);
  list32 @3 :List(UInt32);
  list64 @4 :List(UInt64);
}

const test2 :Test = (id=12111485094725311921, name="Test2");
const test3 :Test = (id=12111485094725311922, name="Test3");
const testContainer :TestContainer = (contents=[.test, .test2, .test3], list8=[0,1,2,3,4], list16=[0,1,2,3,4], list32=[0,1,2,3,4], list64=[0,1,2,3,4]);
