{-
  * Scan

    ghci> listOfFiles ["var", "log", "httpd", "access_log"]
    ["/var","/var/log","/var/log/httpd","/var/log/httpd/access_log"]
-}

listOfFiles :: [String] -> [String]
listOfFiles s = tail $ scanl (\acc x -> acc ++ "/" ++ x) [] s
