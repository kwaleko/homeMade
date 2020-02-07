module Directory where


import           Types
import qualified Post                          as Post
import qualified Template                      as Template
import           System.Directory               ( getDirectoryContents
                                                , listDirectory
                                                , doesFileExist
                                                , createDirectoryIfMissing
                                                )
import           Control.Monad                  ( forM
                                                , forM_
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           System.FilePath                ( (</>) )


data Config = Config {sourceDir :: FilePath
                     ,destDir   :: FilePath
                     ,templates :: String -- name of the folder holding all templates
                     ,markdown  :: String -- name of the folder hodling the markdown posts
                     }

readArchive :: FilePath -> IO [Post.Post]
readArchive mainDir = do
  dirs <- listDirectory mainDir
  fmap concat $ forM dirs $ \dir -> do
    let postPath = mainDir </> dir </> "post.md"
    exist <- doesFileExist postPath
    if exist
      then (: []) . Post.parse mainDir dir <$> readFile postPath
      else do
        putStrLn
          $  "warning: expected file at "
          ++ postPath
          ++ " but found none."
        return []

writePosts :: Config -> IO ()
writePosts config = do
  let sDir    = sourceDir config
      mdPosts = sDir </> markdown config
  posts <- readArchive mdPosts
  forM_ posts $ \post -> do
    writePost config post

writeArchive :: Config -> IO ()
writeArchive config = do
  let sDir        = sourceDir config
      dDir        = destDir config
      arcTemplate = sDir </> templates config </> "archive.html"
      mdPosts     = sDir </> markdown config
  posts    <- readArchive mdPosts
  template <- readFile arcTemplate
  writePage dDir template $ Post.archiveCtx posts

writePost :: Config -> Post.Post -> IO ()
writePost config post = do
  let dDir         = destDir config
      sDir         = sourceDir config
      fullPath     = dDir </> Post.url post
      postTemplatePath = sDir </> templates config </> "post.html"
      postCtx      = Post.postCtx post
  postTemplate <- readFile postTemplatePath 
  writePage fullPath postTemplate postCtx
-- Given a destination directory, a template and a context
-- generate an HTML page
writePage :: FilePath -> Template -> Template.Context -> IO ()
writePage destDir template context = do
  let file     = Template.parse template context
      destFile = destDir </> "index.html"
  createDirectoryIfMissing True destDir
  writeFile destFile file

defaultConfig = Config { sourceDir = "/Users/lambda/dev/source"
                       , destDir   = "/Users/lambda/dev/webserver"
                       , templates = "templates"
                       , markdown  = "posts"
                       }
