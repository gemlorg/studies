{-# LANGUAGE TemplateHaskell #-}

module Compiler.Optimizer.Environment where 
import Lens.Micro.TH (makeLenses)


data Env = Env {

}

makeLenses ''Env