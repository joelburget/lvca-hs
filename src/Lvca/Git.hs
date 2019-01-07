module Lvca.Git (writeTerm, writeRef, lookupTermBySha, readRef) where

import           Control.Monad (join)
import qualified Codec.Serialise as CBOR
import           Data.Bifunctor (bimap)
import           Data.Git hiding (withRepo, branchWrite)
import           Data.Git.Monad hiding (headGet)
import           Data.Git.Types (GitTime(GitTime))
import           Data.Text      (Text)
import           Data.Text.Encoding (encodeUtf8)
import qualified System.Hourglass as T
import qualified Data.ByteString.Lazy as LBS

import Lvca.Types
import           Data.Void                 (Void)

-- Use a git repository with layout:
-- .
--   /refs
--   /objects
--
-- * the filename for every ref is a sha256 hash of its contents
-- * objects can have any name, but their contents must be a valid sha256

identPath :: Sha256 -> EntPath
identPath (Sha256 ident) = ["objects", entName ident]

refPath :: Text -> EntPath
refPath ref = ["refs", entName $ encodeUtf8 ref]

timeCurrentGit :: GitM GitTime
timeCurrentGit = liftGit $ GitTime <$> T.timeCurrent <*> T.timezoneCurrent

mkPerson :: GitM Person
mkPerson = Person "autocommitter" "(no email)" <$> timeCurrentGit

readingMaster :: CommitAccessM (Either String a) -> IO (Either String a)
readingMaster action = fmap join $
  withRepo "repo" $
    withCommit @RefName "master" action

writingMaster :: EntPath -> LBS.ByteString -> IO (Either String ())
writingMaster path contents = withRepo "repo" $ do
  person <- mkPerson
  (r, ()) <- withNewCommit person (Just @RefName "master") $ do -- Nothing?
    setMessage "automatic commit"
    setFile path contents
  branchWrite "master" r

writeTerm :: Term Void -> IO (Either String ())
writeTerm tm = writingMaster (identPath (identify tm)) (CBOR.serialise tm)

writeRef :: Text -> Sha256 -> IO (Either String ())
writeRef ref (Sha256 sha) = writingMaster (refPath ref) (LBS.fromStrict sha)

lookupTermBySha :: Sha256 -> IO (Either String (Term Void))
lookupTermBySha ident = readingMaster $ maybe
  (Left "couldn't find term")
  (bimap show id . CBOR.deserialiseOrFail)
    <$> getFile (identPath ident)

readRef :: Text -> IO (Either String Sha256)
readRef ref = readingMaster $ maybe
  (Left "couldn't find term")
  (Right . Sha256 . LBS.toStrict)
    <$> getFile (refPath ref)
