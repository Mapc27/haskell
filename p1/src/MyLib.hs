{-# LANGUAGE FlexibleContexts #-}

module MyLib where

import Control.Monad.IO.Class
import Control.Monad.State
import System.IO

-- Отделяем побочные эффекты "состояние" от "IO"
-- Можем комбинировать в рамках одной программы

getNextNumber :: MonadIO m => m Int
getNextNumber = do
  liftIO $ putStrLn "Next number, please"
  read <$> liftIO getLine

-- Вводить с клавиатуры числа
-- Суммировать в состояние (сумма, количество)
-- При вводе -1 вернём среднее
comp :: (MonadState (Int,Int) m, MonadIO m) => m Double
comp = do
  x <- getNextNumber
  (s,n) <- get
  case x of
    -1 -> pure $ fromIntegral s / fromIntegral n
    _ -> do
      put (s + x, n + 1)
      comp

someFunc :: IO ()
someFunc = do
  avg <- evalStateT comp (0,0)
  putStrLn $ "Average: " ++ show avg
