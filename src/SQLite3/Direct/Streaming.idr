module SQLite3.Direct.Streaming

import public SQLite3.Direct
import public Streaming
import public Control.Monad.Either
import Control.Monad.Trans

export
get_rows : HasIO m => Handle -> Stmt -> {xs : _} -> All Column xs -> Stream (Of (HList xs)) (EitherT Error m) ()
get_rows handle stmt columns = do
  True <- lift $ MkEitherT $ step handle stmt
  | False => pure ()
  row <- lift $ MkEitherT $ get_row handle stmt columns
  yield row
  get_rows handle stmt columns
