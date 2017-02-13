module Nulan.Token where

import Nulan.Source (Source)


data Token
  = Integer String Source
  | Number String Source
  | Text String Source
  | Delimiter String Source
  | Symbol String Source
