module Nulan.ParseError where

import Nulan.Source (Source)


data ParseError = ParseError String Source
