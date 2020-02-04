module Directory where


import qualified Post as Post
import System.Directory(getDirectoryContents,listDirectory,doesFileExist)
import Control.Monad(forM)
import System.FilePath((</>)) 



readArchive :: FilePath -> IO [Post.Post]
readArchive path = do
  dirs <- listDirectory path
  fmap concat $  forM dirs $ \dir -> do
    let postPath = path </> dir </> "post.md"
    exist <- doesFileExist postPath
    if exist
      then  (:[]) . Post.parse path dir <$> readFile postPath
    else do
      putStrLn  $ "warning: expected file at " ++ postPath ++ " but found none."
      return  []

  
