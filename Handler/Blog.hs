module Handler.Blog where

import Import

import Yesod.Form.Nic (YesodNic, nicHtmlField)
instance YesodNic App

getBlogR :: Handler RepHtml
getBlogR = do
  posts <- runDB $ selectList [] [Desc PostTitle]
  defaultLayout $ do
    setTitle "Blog Index"
    $(widgetFile "blog")

