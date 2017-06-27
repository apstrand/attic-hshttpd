
module IOExtra (ioeGetError, unsafePerformIO) where

import IO


import IOExts


ioeGetError = ioeGetErrorString

