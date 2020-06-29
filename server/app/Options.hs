module Options 
    ( getOptions
    , Options
    , optPort
    , optPgConnection
    , optAllowedCorsOrigin
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
    , allowedCorsOrigin :: Maybe String
    } 

type OptM = OptF Maybe 
type Options = OptF Identity

instance Alternative f => Semigroup (OptF f) where
  (OptF p1 c1 o1) <> (OptF p2 c2 o2) = 
      OptF (p1 <|> p2) (c1 <|> c2) (o1 <|> o2)

optPort :: Options -> Int
optPort (OptF x _ _) = runIdentity x

optPgConnection :: Options -> String
optPgConnection (OptF _ x _) = runIdentity x

optAllowedCorsOrigin :: Options -> Maybe String
optAllowedCorsOrigin (OptF _ _ x) = x

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
    <*> optional ( strOption
            ( long "corsOrigin"
            <> help "Accepted CORS Origin (Default *)"
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
defaults = OptF (Just 8080) Nothing Nothing

collapse :: OptM -> Either String Options
collapse (OptF (Just x) (Just y) z) = Right $ OptF (Identity x) (Identity y) z
collapse (OptF port pgConnection _) = 
    Left . (<>) "Missing arguments: " . intercalate ", " . fmap snd . filter fst $ params
  where
    params = 
        [ (isNothing port, "Server port")
        , (isNothing pgConnection, "Database connection")
        ]

getEnvOpts :: IO OptM
getEnvOpts = do
    port <- readEnvVar "PORT"
    pgConnection <- readEnvVar "DATABASE_URL"
    return $ OptF port pgConnection Nothing

readEnvVar :: Read a => String -> IO (Maybe a)
readEnvVar variableName = (>>= readMaybe) <$> lookupEnv variableName
