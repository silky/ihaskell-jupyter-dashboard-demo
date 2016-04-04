module Control.Dashboard where

import Data.Time
import Network.Wreq
import Control.Monad (when)
import Control.Exception.Enclosed (catchAny)
import Control.Lens
import Data.Aeson.Lens
import Graphics.Rendering.Chart.Easy hiding (Vector)
import Graphics.Rendering.Chart.Backend.Cairo
import Data.ByteString.Char8 (ByteString)
import qualified Data.Text as T


type Username  = ByteString
type Password  = ByteString
type AuthDeets = Maybe (Username, Password)


-- References:
-- http://www.serpentine.com/wreq/tutorial.html
-- https://developer.atlassian.com/bitbucket/concepts/oauth2.html


-- | Repos endpoint; returns all the repos for a given team.
teamReposEndpoint :: T.Text -> String
teamReposEndpoint team =
    "https://bitbucket.org/!api/2.0/repositories/" ++ (T.unpack team)


-- | Internal endpoint for repos; this disagrees with the endpoint specified
--   by the bitbucket api (at the time of publishing); but there are problems
--   with the bitbucket API that await fixes (they've been reported).
repoEndpoint :: T.Text -> T.Text -> String
repoEndpoint owner repoId = 
    "https://bitbucket.org/!api/2.0/repositories/" ++
             (T.unpack owner) ++ "/" ++ (T.unpack repoId) ++ "/pullrequests?state=OPEN&pagelen=50"


-- | Given a team, find all the repos with more than zero pull requests. We
--   return a tuple consiting of the non-zero prs, as well as a list of all
--   the PRs and the time they were created.
nonzeroPrs :: AuthDeets -> T.Text -> IO ([(T.Text, Int)], [(T.Text, T.Text, UTCTime)])
nonzeroPrs deets team = do
    -- Get all the repositories for the team; starting on page 1.
    rs  <- repoList deets team 1

    -- For all the repos, find the PRs.
    prs <- mapM (\x -> let [a,b] = T.splitOn "/" x in getPrs 5 deets a b) rs

    let t = zipWith (\x y -> (last (T.splitOn "/" x), length y)) rs prs
        nonzero = filter (\(_, y) -> y > 0) t
    return $ (nonzero, concat prs)


wreqOpts :: AuthDeets -> Options
wreqOpts deets = 
    case deets of
         Nothing     -> defaults
         Just (u, p) -> defaults & auth ?~ basicAuth u p


-- | Get a list of all the repositories for a given team.
repoList :: AuthDeets -> T.Text -> Int -> IO [T.Text]
repoList deets team pageNo = do
    let url  = (teamReposEndpoint team) ++ "?pagelen=100&page=" ++ (show pageNo)
        opts = wreqOpts deets

    reposJson <- getWith opts url
    
    let rs = (reposJson ^.. responseBody
             . key "values"
             . _Array . each
                . key "full_name" . _String)

    let mnext = reposJson ^? responseBody . key "next" . _String
    case mnext of
         Just _ -> do
             xs <- repoList deets team (pageNo + 1)
             return $ xs ++ rs
         Nothing  -> return rs


-- | Grab all the PRs by running over every repo and pulling out all the PRs
--   from that repo. We need to retry because sometimes the SSL handshake fails.
getPrs :: Int -> AuthDeets -> T.Text -> T.Text -> IO [(T.Text, T.Text, UTCTime)]
getPrs tries deets team repoId = 
    flip catchAny (\_ -> getPrs (tries-1) deets team repoId) $ do
        let url  = repoEndpoint team repoId
            opts = wreqOpts deets

        when (tries <= 0) (error "Retry limit reached.")

        prs <- getWith opts url
        --
        -- Pull out the "title" field.
        --
        putStrLn $ "Getting PRs for: " ++ (show repoId)
        let vs = (prs ^.. responseBody .  key "values" . _Array . traverse)

        return $ map (\x -> (repoId, 
                        x ^. key "title" . _String, 
                        asDate $ x ^. key "created_on" . _String)) vs
    where
        -- | Truly this is horrible. But I can't figure out how to read
        --   the 8601 stuff in haskell completely properly, i.e. the below:
        --
        -- asDate str = read (T.unpack str) :: UTCTime
        --
        -- doesn't work. In any case, we only want to the time to sort; 
        -- we don't actually need all the timezone information (well,
        -- this does assume that all the reops are being reported by
        -- bitbucket in the same timezone).
        asDate str = parseTimeOrError True defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S"))
                        (take 19 (T.unpack str)) :: UTCTime


-- | Turn out data into a slice of pie.
mkPieSlice :: (T.Text, Int) -> PieItem
mkPieSlice (s,v) =   pitem_value .~ (fromIntegral v)
              $ pitem_label .~ (T.unpack s)
              $ def


-- | Returns a thing that will be rendered directly in a
--   jupyter cell.
renderablePrGraph :: [(T.Text, Int)] -> Renderable ()
renderablePrGraph dataPoints = toRenderable layout where
    layout = pie_title .~ "Open Pull Requests"
           $ pie_plot . pie_data .~ map mkPieSlice dataPoints
           $ def


-- | Writes the PR graph to a file.
makePrGraph :: String -> [(T.Text, Int)] -> IO ()
makePrGraph file dataPoints = toFile def file $ do
    pie_title .= "Open Pull Requests"
    pie_plot . pie_data .= map mkPieSlice dataPoints

