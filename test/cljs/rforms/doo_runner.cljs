(ns rforms.doo-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [rforms.core-test]))

(doo-tests 'rforms.core-test)
