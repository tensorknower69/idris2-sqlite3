module SQLite3.Bindings

import CUtils

libsqlite3 : String -> String
libsqlite3 x = "C:sqlite3_" <+> x <+> ",libsqlite3"

%foreign libsqlite3 "libversion"
sqlite3_libversion : PrimIO String

export data PrimHandle : Type where
export data PrimStmt : Type where

export %foreign libsqlite3 "errmsg"
sqlite3_errmsg : Ptr PrimHandle -> PrimIO String

export %foreign libsqlite3 "errcode"
sqlite3_errcode : Ptr PrimHandle -> PrimIO Int

export %foreign libsqlite3 "open"
sqlite3_open : (filename_utf8 : String) -> Ptr (Ptr PrimHandle) -> PrimIO Int

export %foreign libsqlite3 "close"
sqlite3_close : Ptr PrimHandle -> PrimIO Int

export %foreign libsqlite3 "prepare_v2"
sqlite3_prepare_v2 : Ptr PrimHandle -> (zsql : String) -> (zsql_max_len : Int) -> Ptr (Ptr PrimStmt) -> Ptr (Ptr String) -> PrimIO Int

export %foreign libsqlite3 "step"
sqlite3_step : Ptr PrimStmt -> PrimIO Int

export %foreign libsqlite3 "reset"
sqlite3_reset : Ptr PrimStmt -> PrimIO Int

export %foreign libsqlite3 "finalize"
sqlite3_finalize : Ptr PrimStmt -> PrimIO Int

export %foreign libsqlite3 "column_count"
sqlite3_column_count : Ptr PrimStmt -> PrimIO Int

export %foreign libsqlite3 "column_blob"
sqlite3_column_blob : Ptr PrimStmt -> (icol : Int) -> PrimIO AnyPtr

export %foreign libsqlite3 "column_text"
sqlite3_column_text : Ptr PrimStmt -> (icol : Int) -> PrimIO String

export %foreign libsqlite3 "column_double"
sqlite3_column_double : Ptr PrimStmt -> (icol : Int) -> PrimIO Double

||| sqlite3_column_int -> 32-bit INTEGER result
||| -- from: https://sqlite.org/c3ref/column_blob.html
||| tho the signature is `int sqlite3_column_int(sqlite3_stmt*, int iCol);`
export %foreign libsqlite3 "column_int"
sqlite3_column_int : Ptr PrimStmt -> (icol : Int) -> PrimIO Int32

export %foreign libsqlite3 "column_int64"
sqlite3_column_int64 : Ptr PrimStmt -> (icol : Int) -> PrimIO Int64

export %foreign libsqlite3 "column_bytes"
sqlite3_column_bytes : Ptr PrimStmt -> (icol : Int) -> PrimIO Int

export %foreign libsqlite3 "column_type"
sqlite3_column_type : Ptr PrimStmt -> (icol : Int) -> PrimIO Int
