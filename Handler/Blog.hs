module Handler.Blog where

import Import
import Data.Monoid
import Data.Time

import Yesod.Form.Nic (YesodNic, nicHtmlField)
instance YesodNic App

getBlogR :: Handler RepHtml
getBlogR = do
  posts <- runDB $ selectList [] [Desc PostTitle]
  defaultLayout $ do
    setTitle "Blog Index"
    $(widgetFile "blog")

postForm :: Maybe Post -> Form Post
postForm p = renderDivs $ Post 
  <$> areq textField "Name"  (postName  <$> p)
  <*> areq textField "Title" (postTitle <$> p)
  <*> areq nicHtmlField "Content" (postContent <$> p)
  <*> aformM (liftIO getCurrentTime)

getPostNewR :: Handler RepHtml
getPostNewR = do
  (postWidget, enctype) <- generateFormPost $ postForm Nothing
  defaultLayout $ do
    $(widgetFile "post_new")

postPostNewR :: Handler RepHtml
postPostNewR = do
    ((res,postWidget),enctype) <- runFormPost $ postForm Nothing
    case res of 
         FormSuccess post -> do 
            postId <- runDB $ insert post
            setMessage $ toHtml $ (postTitle post) <> " created"
            redirect $ BlogR 
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "post_new")

getPostViewR :: PostId -> Handler RepHtml
getPostViewR postId = do
  post <- runDB $ get404 postId
  comments <- runDB $ selectList [CommentPostId ==. postId] [Asc CommentId]
  (commentWidget, enctype) <- generateFormPost $ commentForm postId
  defaultLayout $ do
    setTitle $ toHtml $ postTitle post
    $(widgetFile "post")

getPostEditR :: PostId -> Handler RepHtml
getPostEditR postId = do
  post <- runDB $ get404 postId
  (postWidget, enctype) <- generateFormPost $ postForm $ Just post
  defaultLayout $ do
    $(widgetFile "post_new")

postPostEditR :: PostId -> Handler RepHtml
postPostEditR postId = do
  ((res, postWidget), enctype) <- runFormPost $ postForm Nothing
  case res of
       FormSuccess post -> do 
         runDB $ do 
           _post <- get404 postId
           update postId [ PostName    =. postName post
                         , PostTitle   =. postTitle post
                         , PostContent =. postContent post]
         setMessage $ toHtml $ (postTitle post) <> "updated"
         redirect $ PostViewR postId
       _ -> defaultLayout $ do
         setTitle "Please corrrect your entry form"
         $(widgetFile "post_new")
         
getPostDeleteR :: PostId -> Handler RepHtml
getPostDeleteR postId = do
  runDB $ do
    _post <- get404 postId
    delete postId
    deleteWhere [ CommentPostId ==. postId ]
  redirect $ BlogR

commentForm :: PostId -> Form Comment
commentForm postId = renderDivs $ Comment
  <$> areq textField     "Commenter" Nothing
  <*> areq textareaField "Body"      Nothing
  <*> aformM (liftIO getCurrentTime)
  <*> pure postId

postCommentNewR :: PostId -> Handler RepHtml
postCommentNewR postId = do
  _post <- runDB $ get404 postId
  ((res, commentWidget), enctype) <- runFormPost $ commentForm postId
  case res of
    FormSuccess comment -> do
      commentId <- runDB $ insert comment
      setMessage $ toHtml $ (commentCommenter comment)
      redirect $ PostViewR postId
    _ -> do
      setMessage "add correct comment"
      redirect $ PostViewR postId

