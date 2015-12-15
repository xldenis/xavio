{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DataKinds       #-}

module PostsAPI where
import Prelude hiding (show, index)

import API
import Servant hiding (Post)
import Control.Monad.Trans.Either

data Post = Post
type PostId = Int

type PostsAPI = APIFor Post PostId

postsApi :: Server PostsAPI
postsApi = serverFor index show create update destroy

index :: Response [Post]
index = undefined

show :: PostId -> Response Post
show = undefined

create :: Post -> Response ()
create = undefined

update :: PostId -> Post -> Response ()
update = undefined

destroy :: PostId -> Response ()
destroy = undefined
