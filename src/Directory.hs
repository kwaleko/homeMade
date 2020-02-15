module Directory where


import           Types
import qualified Post                          as Post
import qualified Template                      as Template
import           System.Directory               ( getDirectoryContents
                                                , listDirectory
                                                , doesFileExist
                                                , createDirectoryIfMissing
                                                , removeFile
                                                , removeDirectoryRecursive
                                                , doesDirectoryExist
                                                , copyFile
                                               
                                                )
import           Control.Monad                  ( forM
                                                , forM_
                                                , when
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           System.FilePath                ( (</>) )
import           Control.Concurrent.Async       ( concurrently_ )
import qualified Data.Map                      as Map
import           Data.Text                      ( pack )

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

readTemplates :: Config -> IO (Map.Map String String)
readTemplates config = do
  let sDir = sourceDir config </> templates config
  --print sDir  
  templates <- listDirectory sDir 
  fmap Map.fromList $ forM templates $ \template -> do
    --print template 
    templateContent <- readFile $ sDir </> template
    return (template,templateContent)
   

mkGlobalContext :: Config -> IO Template.Context
mkGlobalContext config  = do
  css <-  readFile  $  sourceDir config   </> "styles.css"
  return $ Map.singleton "styles.css" (Template.Template css)


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
  globalTemplates <- readTemplates config 
  let allCtx = Map.union  (Post.archiveCtx posts) (Template.Template <$>  globalTemplates )
  writePage dDir template  allCtx 

writePost :: Config -> Post.Post -> IO ()
writePost config post = do
  let dDir             = destDir config
      sDir             = sourceDir config
      fullPath         = dDir </> Post.url post
      postTemplatePath = sDir </> templates config </> "post.html"
      postCtx          = Post.postCtx post
  postTemplate <- readFile postTemplatePath
  globalTemplates <- readTemplates config 
  let  allCtx = Map.union  (Post.postCtx post) (Template.Template <$>  globalTemplates )
  writePage fullPath postTemplate allCtx

-- Given a destination directory, a template and a context
-- generate an HTML page
writePage :: FilePath -> Template -> Template.Context -> IO ()
writePage destDir template context = do
  let file     = Template.parse template context
      destFile = destDir </> "index.html"
  createDirectoryIfMissing True destDir
  writeFile destFile file


-- Delete all files file from the output directory
-- migrate the css file to the destination directory
--
genHtml :: Config -> IO ()
genHtml config = do
  cleanOutputDir config
  migrateCss config 
  concurrently_ (writeArchive config) (writePosts config)

cleanOutputDir :: Config -> IO ()
cleanOutputDir config = do
  let dDir = destDir config
  putStrLn dDir
  exists <- doesDirectoryExist dDir
  when exists $ do
    files <- listDirectory dDir
    forM_ files $ \file -> do
      putStrLn $ " checking " ++ file ++ "..."
      let path = dDir </> file
      isFile <- doesFileExist path
      if isFile
        then do
          removeFile path
          putStrLn $ "file " ++ file ++ " is removed"
        else do
          removeDirectoryRecursive path
          putStrLn $ "directory " ++ file ++ " is removed"

migrateCss :: Config -> IO ()
migrateCss config = do
  let cssFile = "styles.css"
      sPath = sourceDir config </> templates config </> cssFile
      dPath = destDir config  </> cssFile
  print sPath
  print dPath 
  copyFile sPath dPath 
