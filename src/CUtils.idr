module CUtils

import public System.FFI

libcutils : String -> String
libcutils x = "C:" <+> x <+> ",libidris2_cutils.so"

export
read_cstring : Ptr String -> String
read_cstring = prim__getString

export
nullanyptr : AnyPtr
nullanyptr = prim__getNullAnyPtr

export
remember : AnyPtr -> Ptr a
remember = prim__castPtr

export
nullptr : Ptr a
nullptr = remember nullanyptr

export
forget : Ptr a -> AnyPtr
forget = prim__forgetPtr

export
is_null : Ptr a -> Bool
is_null ptr = prim__nullPtr ptr > 0

export
cast : Ptr a -> Ptr b
cast = remember . forget

export
%foreign libcutils "sizeof_ptr"
sizeof_ptr : Int

export
%foreign libcutils "join_ptr"
prim__join_ptr : AnyPtr -> PrimIO AnyPtr

export
join_ptr : HasIO m => Ptr (Ptr a) -> m (Ptr a)
join_ptr ptr = do
  x <- primIO $ prim__join_ptr (forget ptr)
  pure $ remember x
