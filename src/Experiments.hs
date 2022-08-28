{-# LANGUAGE FlexibleContexts #-}
{-# HLINT ignore "Use join" #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Experiments where

-- import Control.Monad.Except
-- import Control.Monad.State
-- import Data.Word (Word8)

-- -- import Control.Monad.Reader (ReaderT (runReaderT))
-- -- import Control.Monad.Reader.Class

-- data PassError = PassError | SomeError deriving (Show)

-- data Context = Ctx {getA :: Int, getB :: Int} deriving (Show)

-- newtype Pass m a = Pass {runPass :: StateT Context (ExceptT PassError m) a}
--   deriving (Functor, Applicative, Monad, MonadState Context, MonadError PassError)

-- instance MonadState Int (Either PassError) where
--   get = undefined
--   put = undefined

-- pass1 :: (MonadError e m, MonadState s m, Monad m) => Pass m Int
-- pass1 = do
--   -- throwError "blah"
--   a <- gets getA
--   put (Ctx (a + 8) a)
--   return (a :: Int)

-- pass2 :: (MonadError e m, MonadState s m, Monad m) => Int -> Pass m String
-- pass2 a = do
--   -- throwError PassError
--   b <- gets getB
--   return (show a <> show b)

-- pass3 :: (MonadError e m, MonadState s m, Monad m) => String -> Pass m Word8
-- pass3 a = do
--   -- throwError PassError
--   b <- gets getA
--   return (fromIntegral b + fromIntegral (read a))

-- p :: Either PassError (Word8, Context)
-- p =
--   let passes = runPass $ pass1 >>= pass2 >>= pass3
--    in join . runExceptT $ runStateT passes (Ctx 0 0)

--- Fused effects

-- import Ast.Ast (Program)
-- import Control.Algebra
-- import Control.Carrier.Error.Either
-- import Control.Carrier.Fresh.Strict
-- import Control.Carrier.Lift (runM)
-- import Control.Carrier.State.Strict
-- import Control.Monad.Identity (Identity)
-- import Data.Text (Text, pack, unpack)
-- import Irs.X86 (X86Program)

-- type Locals = [Text]

-- type StackOffset = Int

-- data Context = Context {infoLocals :: Locals, infoStackOffset :: StackOffset} deriving (Show, Eq)

-- type PassM sig m = (Has (State Context) sig m, Has (Throw Text) sig m, Has Fresh sig m)

-- emptyContext :: Context
-- emptyContext = Context [] 0

-- type ProgramC m a = FreshC (StateC Context (ErrorC Text m)) a

-- runPass :: (Algebra sig m) => Context -> FreshC (StateC Context (ErrorC Text m)) a -> m (Either Text (Context, (Int, a)))
-- runPass initContext = runError . runState @Context initContext . runFresh 0

-- runProgram :: Context -> Either Text String
-- runProgram initContext = fmap (snd . snd) . run . runError . runState @Context initContext . runFresh 0 $ passes

-- ex :: (Has (State Context) sig m, Has (Throw Text) sig m, Has Fresh sig m) => m Int
-- ex = do
--   s <- get @Context
--   put (Context ["blah"] 1)
--   -- _x <- throwError (T.pack "nope")
--   pure 3

-- ex2 :: (Has (State Context) sig m, Has (Throw Text) sig m) => Int -> m String
-- ex2 num = do
--   s <- get @Context
--   -- _x <- throwError (pack "nope")
--   pure ("blah" <> show num <> unpack (head (infoLocals s)))

-- passes :: (PassM sig m) => m String
-- passes = do
--   x <- ex
--   ex2 x

-- -- pure (ex2 x)

-- -- liftEither $ fmap (snd . snd) x

-- -- p = runError . runState @Context (Context 0 "ouch") . runState @Int 9 . runFresh 0 $ ex3
