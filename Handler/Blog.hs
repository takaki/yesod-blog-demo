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

