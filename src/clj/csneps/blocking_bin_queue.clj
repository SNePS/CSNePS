(ns csneps.blocking-bin-queue)

(gen-class
  :name csneps.BlockingBinQueue
  :implements java.concurrent.BlockingQueue
  :state state
  :prefix "-"
  :main false
)