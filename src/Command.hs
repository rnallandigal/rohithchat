module Command
    ( Command (..)
    , readCommand
    ) where

import Options.Applicative

data Command = Run Int
             deriving (Show)

readCommand :: IO Command
readCommand = execParser $ info
    (commandP <**> helper)
     ( fullDesc
    <> progDesc "Launch the messaging server"
    <> header "Messaging Server" )

commandP :: Parser Command
commandP = Run <$> portP

portP :: Parser Int
portP = option auto $
       short 'p'
    <> long "port"
    <> help "the port number to serve the application on"
    <> metavar "PORT"
    <> value 8080
