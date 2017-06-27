
module MonadLib where


data StateMonadP m s a = ST { unST :: (s -> m (a,s)) }


instance Monad m => Monad (StateMonadP m s) where
    return v	= ST (\s -> return (v,s))
    ST m >>= f	= ST (\s -> do	(v,s') <- m s
				unST (f v) s')


type StateMonad s a = StateMonadP IdMonad s a
type IOState s a = StateMonadP IO s a


io :: IO a -> StateMonadP IO s a
io f = ST (\s -> do v <- f
		    return (v,s))

liftSt2 f a1 a2 = ST (\s -> f a1 (a2,s))

getst :: Monad m => StateMonadP m s s
getst		= ST (\s -> return (s,s))
getstp f	= ST (\s -> return (f s, s))
setst s		= ST (\_ -> return ((),s))
modst f		= ST (\s -> return ((), f s))

runst s i = unIdM ((unST s) i)
runstp s i = (unST s) i


data ErrorMonad a = Ok a | Fail String

instance Monad ErrorMonad where
	return v	= Ok v
	Ok   a >>= f	= f a
	Fail a >>= f	= Fail a

data IdMonad a = IdMonad { unIdM :: a }

instance Monad IdMonad where
	return a = IdMonad a
	IdMonad m >>= f = f m



