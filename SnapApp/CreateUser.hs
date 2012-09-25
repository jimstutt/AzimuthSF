{-
Description: Create a snap user only from localhost.
Source: haskell-cafe Mon, 24 Sep 2012 23:39:17 +0200
Author: Mathias Fischmann
-}

data CreateUser = CreateUser T.Text ByteString
  deriving Show

-- | If called from localhost with appropriate GET values, call
-- @createUser@.  If called from a remote host, pass.  Any other
-- situation will just make the thread crash with a meaningful status
-- message (a little lazy, I know).
handleCreateUser :: Handler App (AuthManager App) ()
handleCreateUser = failIfNotLocal $
  do
    r <- getRequest
    case parse_ $ rqParams r of
      Just cu@(CreateUser nm pw) -> do
        createUser nm pw
        error $ "admin/create_user: creating " ++ show cu ++ " -- success!\n\n"
      Nothing -> do
        error $ "admin/create_user: invalid parameters:\n\n" ++ show r
  where
    parse_ :: M.Map ByteString [ByteString] -> Maybe CreateUser
    parse_ xs = case M.lookup "user" xs of
                  Just [user] -> case M.lookup "passwd" xs of
                                   Just [passwd] -> Just $ CreateUser (cs user) passwd
                                   _ -> Nothing
                  _ -> Nothing

-- | snap-0.9.0.1/src/Snap/Snaplet/Internal/Types.hs implements this,
-- but does not export it.
failIfNotLocal :: Handler a b () -> Handler a b ()
failIfNotLocal m =
  do
    r <- getRequest
    let warning = "enforceLocalClient from unauthorized client.\n" ++
                  "complete request:\n\n" ++
                  show r
        localhosts = ["127.0.0.1", "::1", "localhost"]

    if not $ elem (rqRemoteAddr r) localhosts
      then logError (cs warning) >> pass
      else m

{-
So in the end i use another convenience function that's hidden in
snap.  I am not sure whether this one should be exposed or re-written
because it is so short.

Creation of new users can now be cli-based:

#!/bin/sh

export _USER=$1
export _PASSWD=$2
export _CALL="http://localhost:8008/admin/create_user?user=$_USER&passwd=$_PASSWD"

echo "$_CALL"
curl "$_CALL"
echo

Finally, I have added another splice "<userName />" that is replaced
by the user name, so a button can read "logout mf":
-}

-- | bind user name to heist tag <userName /> on any App handler.
bindSpliceUserName :: Handler App b () -> Handler App b ()
bindSpliceUserName h =
  do
    m :: Maybe AuthUser <- withTop auth currentUser
    case userLogin <$> m of
      Nothing -> h
      Just n -> heistLocal (bindSplice "userName" (textSplice n)) h

