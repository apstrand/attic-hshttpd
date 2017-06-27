
module OS_Types where

import SemiFFI

newtype IOHandle = IOH Int deriving (Eq,Show)

unIOH (IOH h) = h


type Data	    = (Int, String)
data OpenMode	    = Reading | Writing | Appending deriving (Eq,Enum,Show)

data FileType = File | Directory | Other | NotExist deriving (Eq,Enum,Show)

newtype Select = Select Addr

data WaitFor = Wait_Read IOHandle
	     | Wait_Write IOHandle
	     | Wait_Exception IOHandle
    deriving (Eq,Show)

unWF (Wait_Read h) = h
unWF (Wait_Write h) = h
unWF (Wait_Exception h) = h

data PosixError
  = EPERM
  | ENOENT
  | EINTR
  | EIO
  | ENOEXEC
  | EBADF
  | EAGAIN
  | ENOMEM
  | EACCES
  | EFAULT
  | EBUSY
  | ENODEV
  | ENOTDIR
  | EISDIR
  | EINVAL
  | EPIPE
  deriving (Eq,Show)

intToPosixError e = case e of
    1	-> EPERM
    2	-> ENOENT
    4   -> EINTR
    5   -> EIO	
    8	-> ENOEXEC
    9   -> EBADF
    11  -> EAGAIN
    12  -> ENOMEM
    13  -> EACCES
    14  -> EFAULT
    16  -> EBUSY
    19  -> ENODEV
    20	-> ENOTDIR
    21  -> EISDIR
    22  -> EINVAL
    32  -> EPIPE
    e	-> error ("Invalid errno: " ++ show e)




