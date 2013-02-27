import System.Environment

main = do args <- getArgs
          if length args /= 2 then
            do print "Wrong number of arguments."
          else do let [arg1, arg2] = args
                  print arg1
                  print arg2
