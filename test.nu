#lang reader "nulan.rkt"

(var (= foo 1))

foo
#|
box net = require "net"

box server = net.create-server -> o
               | o.write "Echo server\r\n"
               | o.pipe o

server.listen 1337 "127.0.0.1"|#
