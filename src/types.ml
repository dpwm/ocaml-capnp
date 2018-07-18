type 'a structure = {
  fields : field array;  
}
and field = Field : 'a -> field
and
'a t =
  | Int64 : Int64.t t
  | Int32 : Int32.t t
  | Int16 : int t
  | Int8  : int t
  | UInt64 : Unsigned.UInt64.t t
  | UInt32 : Unsigned.UInt32.t t
  | UInt16 : int t
  | UInt8  : int t 
  | Structure : 'a structure -> 'a t


