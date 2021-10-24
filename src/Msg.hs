module Msg where

errorNotExist :: String -> String
errorNotExist path = "ERROR: `" ++ path ++ "` does not exist"

missingTargetFolder = "ERROR: missing target folder"

info :: String
info = "Lines is a recursive line-counting tool for the terminal\n\
\\n\
\Usage:\n\
\    lines [args]\n\
\\n\
\Examples:\n\
\    lines src/main.rs\n\
\    lines src/main.rs lib/iolib.rs\n\
\    lines src\n\
\    lines src pkg lib/iolib.rs\n\
\"