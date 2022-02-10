module SQLite3.Direct

import CUtils
import Control.Monad.Either
import Control.Monad.Trans
import Data.Buffer
import SQLite3.Bindings
import Syntax.WithProof
import public Data.List.Quantifiers
import public SQLite3.Constants

export
record Handle where
  constructor MkHandle
  phandle : Ptr PrimHandle

export
record LibError where
  constructor MkLibError
  code : ResultCode
  msg : String

export
Show LibError where
  show err = show err.code <+> ": " <+> show err.msg

export
data Error : Type where
  ALibError : LibError -> Error
  GenericError : String -> Error
  ImpossibleError : String -> Error

export
Show Error where
  show (ALibError err) = "Library Error: " <+> show err
  show (GenericError msg) = msg
  show (ImpossibleError msg) = "IMPOSSIBLE ERROR: " <+> msg

mk_lib_error : HasIO m => Handle -> ResultCode -> m LibError
mk_lib_error handle code = do
  msg <- primIO $ sqlite3_errmsg handle.phandle
  pure $ MkLibError code msg

return_error : MonadError Error m => HasIO m => Handle -> ResultCode -> m a
return_error handle code = mk_lib_error handle code >>= throwError . ALibError

process_result_code : MonadError Error m => HasIO m => (rc : Int) -> m ResultCode
process_result_code rc = do
  case ResultCode.from_raw rc of
    Just x => pure x
    Nothing => throwError $ ImpossibleError $ "got unknown error code: " <+> show rc

check_lib_error' : MonadError Error m => HasIO m => Handle -> Int -> m ()
check_lib_error' handle rc = do
  process_result_code rc >>= \case
    Ok => pure ()
    code => return_error handle code

check_lib_error : MonadError Error m => HasIO m => Handle -> m ()
check_lib_error handle = do
  rc <- primIO $ sqlite3_errcode handle.phandle
  check_lib_error' handle rc

export
open_file : HasIO m => (path : String) -> m (Either Error Handle)
open_file path = runEitherT {m} $ do
  pphandle <- remember <$> malloc sizeof_ptr
  rc <- primIO $ sqlite3_open path pphandle
  handle <- MkHandle <$> join_ptr pphandle
  check_lib_error' handle rc
  pure handle

export
close : HasIO m => Handle -> m (Either Error ())
close handle = runEitherT {m} $ do
  rc <- primIO $ sqlite3_close handle.phandle
  check_lib_error' handle rc

export
record Stmt where
  constructor MkStmt
  pstmt : Ptr PrimStmt

export
prepare : HasIO m => Handle -> (zsql : String) -> m (Either Error Stmt)
prepare handle zsql = runEitherT {m} $ do
  ppstmt <- remember <$> malloc sizeof_ptr
  rc <- primIO $ sqlite3_prepare_v2 handle.phandle zsql (-1) ppstmt nullptr
  pstmt <- join_ptr ppstmt
  check_lib_error' handle rc
  pure $ MkStmt pstmt

||| returns True if succeeds
||| returns False if done
export
step : HasIO m => Handle -> Stmt -> m (Either Error Bool)
step handle stmt = runEitherT {m} $ do
  rc <- primIO $ sqlite3_step stmt.pstmt
  process_result_code rc >>= \case
    Row => pure True
    Done => pure False
    code => return_error handle code

export
reset : HasIO m => Handle -> Stmt -> m (Either Error ())
reset handle stmt = runEitherT {m} $ do
  rc <- primIO $ sqlite3_step stmt.pstmt
  check_lib_error' handle rc

export
finalize : HasIO m => Handle -> Stmt -> m (Either Error ())
finalize handle stmt = runEitherT {m} $ do
  rc <- primIO $ sqlite3_finalize stmt.pstmt
  check_lib_error' handle rc

check_column_lib_error : MonadError Error m => HasIO m => Handle -> m ()
check_column_lib_error handle = do
  rc <- primIO $ sqlite3_errcode handle.phandle
  process_result_code rc >>= \case
    Row => pure ()
    code => throwError $ GenericError $ "unexpected code: " <+> show code

export
column_count : HasIO m => Handle -> Stmt -> m (Either Error Nat)
column_count handle stmt = runEitherT {m} $ do
  x <- primIO $ sqlite3_column_count stmt.pstmt
  check_column_lib_error handle
  pure $ cast x

export
column_int : HasIO m => Handle -> Stmt -> (col : Nat) -> m (Either Error Int32)
column_int handle stmt col = runEitherT {m} $ do
  x <- primIO $ sqlite3_column_int stmt.pstmt (cast col)
  check_column_lib_error handle
  pure x

export
column_int64 : HasIO m => Handle -> Stmt -> (col : Nat) -> m (Either Error Int64)
column_int64 handle stmt col = runEitherT {m} $ do
  x <- primIO $ sqlite3_column_int64 stmt.pstmt (cast col)
  check_column_lib_error handle
  pure x

export
column_double : HasIO m => Handle -> Stmt -> (col : Nat) -> m (Either Error Double)
column_double handle stmt col = runEitherT {m} $ do
  x <- primIO $ sqlite3_column_double stmt.pstmt (cast col)
  check_column_lib_error handle
  pure x

public export
record Blob where
  constructor MkBlob
  ptr : AnyPtr
  size : Int

export
column_blob : HasIO m => Handle -> Stmt -> (col : Nat) -> m (Either Error Blob)
column_blob handle stmt col = runEitherT {m} $ do
  let col' = cast col
  x <- primIO $ sqlite3_column_blob stmt.pstmt col'
  check_column_lib_error handle
  size <- primIO $ sqlite3_column_bytes stmt.pstmt col'
  check_column_lib_error handle
  pure $ MkBlob x size

export
column_text : HasIO m => Handle -> Stmt -> (col : Nat) -> m (Either Error String)
column_text handle stmt col = runEitherT {m} $ do
  x <- primIO $ sqlite3_column_text stmt.pstmt (cast col)
  check_column_lib_error handle
  pure x

export
column_type : HasIO m => Handle -> Stmt -> (col : Nat) -> m (Either Error DataType)
column_type handle stmt col = runEitherT {m} $ do
  x <- primIO $ sqlite3_column_type stmt.pstmt (cast col)
  check_column_lib_error handle
  case DataType.from_raw x of
    Just t => pure t
    Nothing => throwError $ ImpossibleError $ "unknown data type: " <+> show x

export
interface ReadColumn a where
  get : HasIO m => Handle -> Stmt -> (col : Nat) -> m (Either Error a)
  concrete_type : ConcreteType

export
ReadColumn String where
  get = column_text
  concrete_type = AText

export
ReadColumn Int64 where
  get = column_int64
  concrete_type = AInteger

export
ReadColumn Int32 where
  get = column_int
  concrete_type = AInteger

export
ReadColumn Double where
  get = column_double
  concrete_type = AFloat

export
ReadColumn Blob where
  get = column_blob
  concrete_type = ABlob

public export
beta : Bool -> Type -> Type
beta False a = a
beta True a = Maybe a

check_get_column_error : MonadError Error m => (col : Nat) -> (got : ConcreteType) -> (expect : ConcreteType) -> m ()
check_get_column_error col got expect =
  unless (expect == got) $ do
    throwError $ GenericError $
      "expecting " <+> show expect <+> " but got " <+> show got <+> " at column " <+> show col

export
get_column : HasIO m => Handle -> Stmt
  -> (a : Type) -> {auto prf : ReadColumn a}
  -> (allow_null : Bool) -> (allow_cast : Bool)
  -> (col : Nat)
  -> m (Either Error (beta allow_null a))
get_column handle stmt a False allow_cast col = runEitherT {m} $ do
  MkEitherT (column_type handle stmt col) >>= \case
    ANull => throwError $ GenericError $
      "expected non-null " <+> show (concrete_type @{prf}) <+> " but encountered NULL at column " <+> show col
    Concrete t => do
      check_get_column_error col t (concrete_type @{prf})
      MkEitherT $ get handle stmt col
get_column handle stmt a True allow_cast col = runEitherT {m} $ do
  MkEitherT (column_type handle stmt col) >>= \case
    ANull => pure Nothing
    Concrete t => do
      check_get_column_error col t (concrete_type @{prf})
      map Just $ MkEitherT $ get handle stmt col

public export
data Column : Type -> Type where
  A : (a : Type)
    -> {default False cast : Bool}
    -> {default False null : Bool}
    -> {auto prf : ReadColumn a}
    -> Column (beta null a)

get_row' : HasIO m => Handle -> Stmt -> Nat -> {xs : _} -> All Column xs -> m (Either Error (HList xs))
get_row' handle stmt col [] = runEitherT {m} $ pure []
get_row' handle stmt col (A type {cast} {null} {prf} :: next) = runEitherT {m} $ do
  x <- MkEitherT $ get_column handle stmt type {prf} null cast col
  xs <- MkEitherT $ get_row' handle stmt (S col) next
  pure (x :: xs)

export
get_row : HasIO m => Handle -> Stmt -> {xs : _} -> All Column xs -> m (Either Error (HList xs))
get_row handle stmt = get_row' handle stmt Z

||| step then get_row
export
read_row : HasIO m => Handle -> Stmt -> {xs : _} -> All Column xs -> m (Either Error (HList xs))
read_row handle stmt columns = runEitherT {m} $ do
  True <- MkEitherT $ step handle stmt
  | False => throwError $ GenericError $ "already reached the end"
  MkEitherT $ get_row handle stmt columns
