module Options 
    ( getOptions
    , Options
    , optPort
    , optPgConnection
    ) where

import Data.Functor.Identity 
import Options.Applicative 
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Data.Maybe (isNothing)
import Data.List (intercalate)


data OptF f = OptF
    { port :: f Int
    , pgConnection :: f String
    } 

type OptM = OptF Maybe 
type Options = OptF Identity

instance Alternative f => Semigroup (OptF f) where
  (OptF p1 c1) <> (OptF p2 c2) = OptF (p1 <|> p2) (c1 <|> c2)

optPort :: Options -> Int
optPort (OptF x _) = runIdentity x

optPgConnection :: Options -> String
optPgConnection (OptF _ x) = runIdentity x

cmdLineParser :: Parser OptM
cmdLineParser = OptF 
    <$> optional ( option auto 
            ( long "port"
            <> short 'p'
            <> metavar "INT"
            <> help "Port to run http server"
            ))
    <*> optional ( strOption
            ( long "pgConnection"
            <> short 'c'
            <> help "Connection to database"
            ))

getCmdLineOptions :: IO OptM
getCmdLineOptions = execParser $ info 
    (cmdLineParser <**> helper) 
    (fullDesc <> progDesc "Runs wonsz server")

getOptions :: IO (Either String Options)
getOptions = do 
    cmdLineArgs <- getCmdLineOptions
    envVar <- getEnvOpts 
    return $ collapse (cmdLineArgs <> envVar <> defaults)

defaults :: OptM
defaults = OptF (Just 8080) Nothing

collapse :: OptM -> Either String Options
collapse (OptF (Just x) (Just y)) = Right $ OptF (Identity x) (Identity y)
collapse (OptF port pgConnection) = 
    Left . (<>) "Missing arguments: " . intercalate ", " . fmap snd . filter fst $ params
  where
    params = 
        [ (isNothing port, "Server port")
        , (isNothing pgConnection, "Database connection")]

getEnvOpts :: IO OptM
getEnvOpts = do
    port <- readEnvVar "PORT"
    pgConnection <- readEnvVar "DATABASE_URL"
    return $ OptF port pgConnection

readEnvVar :: Read a => String -> IO (Maybe a)
readEnvVar variableName = (>>= readMaybe) <$> lookupEnv variableName
