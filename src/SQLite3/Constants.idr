module SQLite3.Constants

import SQLite3.Bindings
import Data.Buffer

public export
data ErrorCode : Type where

||| Result Codes versus Error Codes
||| 
||| "Error codes" are a subset of "result codes" that indicate that something has gone wrong.
||| There are only a few non-error result codes: SQLITE_OK, SQLITE_ROW, and SQLITE_DONE.
||| The term "error code" means any result code other than these three.
||| -- from: https://sqlite.org/rescode.html
public export
data ResultCode : Type where
  ||| Value: 0   /* Successful result */
  Ok : ResultCode
  ||| Value: 1   /* Generic error */
  Error : ResultCode
  ||| Value: 2   /* Internal logic error in SQLite */
  Internal : ResultCode
  ||| Value: 3   /* Access permission denied */
  Perm : ResultCode
  ||| Value: 4   /* Callback routine requested an abort */
  Abort : ResultCode
  ||| Value: 5   /* The database file is locked */
  Busy : ResultCode
  ||| Value: 6   /* A table in the database is locked */
  Locked : ResultCode
  ||| Value: 7   /* A malloc() failed */
  NoMem : ResultCode
  ||| Value: 8   /* Attempt to write a readonly database */
  ReadOnly : ResultCode
  ||| Value: 9   /* Operation terminated by sqlite3_interrupt()*/
  Interrupt : ResultCode
  ||| Value: 10   /* Some kind of disk I/O error occurred */
  IoErr : ResultCode
  ||| Value: 11   /* The database disk image is malformed */
  Corrupt : ResultCode
  ||| Value: 12   /* Unknown opcode in sqlite3_file_control() */
  NotFound : ResultCode
  ||| Value: 13   /* Insertion failed because database is full */
  Full : ResultCode
  ||| Value: 14   /* Unable to open the database file */
  CantOpen : ResultCode
  ||| Value: 15   /* Database lock protocol error */
  Protocol : ResultCode
  ||| Value: 16   /* Internal use only */
  Empty : ResultCode
  ||| Value: 17   /* The database schema changed */
  Schema : ResultCode
  ||| Value: 18   /* String or BLOB exceeds size limit */
  TooBig : ResultCode
  ||| Value: 19   /* Abort due to constraint violation */
  Constraint : ResultCode
  ||| Value: 20   /* Data type mismatch */
  Mismatch : ResultCode
  ||| Value: 21   /* Library used incorrectly */
  Misuse : ResultCode
  ||| Value: 22   /* Uses OS features not supported on host */
  NoLFS : ResultCode
  ||| Value: 23   /* Authorization denied */
  Auth : ResultCode
  ||| Value: 24   /* Not used */
  Format : ResultCode
  ||| Value: 25   /* 2nd parameter to sqlite3_bind out of range */
  Range : ResultCode
  ||| Value: 26   /* File opened that is not a database file */
  NotADB : ResultCode
  ||| Value: 27   /* Notifications from sqlite3_log() */
  Notice : ResultCode
  ||| Value: 28   /* Warnings from sqlite3_log() */
  Warning : ResultCode
  ||| Value: 100  /* sqlite3_step() has another row ready */
  Row : ResultCode
  ||| Value: 101  /* sqlite3_step() has finished executing */
  Done : ResultCode

export
Show ResultCode where
  show Ok = "Ok"
  show Error = "Error"
  show Internal = "Internal"
  show Perm = "Perm"
  show Abort = "Abort"
  show Busy = "Busy"
  show Locked = "Locked"
  show NoMem = "NoMem"
  show ReadOnly = "ReadOnly"
  show Interrupt = "Interrupt"
  show IoErr = "IoErr"
  show Corrupt = "Corrupt"
  show NotFound = "NotFound"
  show Full = "Full"
  show CantOpen = "CantOpen"
  show Protocol = "Protocol"
  show Empty = "Empty"
  show Schema = "Schema"
  show TooBig = "TooBig"
  show Constraint = "Constraint"
  show Mismatch = "Mismatch"
  show Misuse = "Misuse"
  show NoLFS = "NoLFS"
  show Auth = "Auth"
  show Format = "Format"
  show Range = "Range"
  show NotADB = "NotADB"
  show Notice = "Notice"
  show Warning = "Warning"
  show Row = "Row"
  show Done = "Done"

namespace ResultCode
  export
  from_raw : Int -> Maybe ResultCode
  from_raw 0   = Just Ok
  from_raw 1   = Just Error
  from_raw 2   = Just Internal
  from_raw 3   = Just Perm
  from_raw 4   = Just Abort
  from_raw 5   = Just Busy
  from_raw 6   = Just Locked
  from_raw 7   = Just NoMem
  from_raw 8   = Just ReadOnly
  from_raw 9   = Just Interrupt
  from_raw 10  = Just IoErr
  from_raw 11  = Just Corrupt
  from_raw 12  = Just NotFound
  from_raw 13  = Just Full
  from_raw 14  = Just CantOpen
  from_raw 15  = Just Protocol
  from_raw 16  = Just Empty
  from_raw 17  = Just Schema
  from_raw 18  = Just TooBig
  from_raw 19  = Just Constraint
  from_raw 20  = Just Mismatch
  from_raw 21  = Just Misuse
  from_raw 22  = Just NoLFS
  from_raw 23  = Just Auth
  from_raw 24  = Just Format
  from_raw 25  = Just Range
  from_raw 26  = Just NotADB
  from_raw 27  = Just Notice
  from_raw 28  = Just Warning
  from_raw 100 = Just Row
  from_raw 101 = Just Done
  from_raw _   = Nothing

public export
data ConcreteType : Type where
  ||| #define SQLITE_INTEGER  1
  AInteger : ConcreteType
  ||| #define SQLITE_FLOAT    2
  AFloat : ConcreteType
  ||| #define SQLITE_TEXT     3
  AText : ConcreteType
  ||| #define SQLITE_BLOB     4
  ABlob : ConcreteType

export
Show ConcreteType where
  show AInteger = "Integer"
  show AFloat = "Float"
  show AText = "Text"
  show ABlob = "Blob"

export
Eq ConcreteType where
  AInteger == AInteger = True
  AFloat == AFloat = True
  AText == AText = True
  ABlob == ABlob = True
  _ == _ = False

public export
data DataType : Type where
  Concrete : ConcreteType -> DataType
  ||| #define SQLITE_NULL     5
  ANull : DataType

export
Show DataType where
  show ANull = "Null"
  show (Concrete t) = show t

export
Eq DataType where
  Concrete x == Concrete x' = x == x'
  ANull == ANull = True
  _ == _ = False

namespace DataType
  export
  from_raw : Int -> Maybe DataType
  from_raw 1 = Just $ Concrete AInteger
  from_raw 2 = Just $ Concrete AFloat
  from_raw 3 = Just $ Concrete AText
  from_raw 4 = Just $ Concrete ABlob
  from_raw 5 = Just ANull
  from_raw _ = Nothing
