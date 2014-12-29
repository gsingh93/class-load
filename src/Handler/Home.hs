module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    let classes = ["EECS280", "EECS281"] :: [Text]
    defaultLayout $ do
        addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js"
        setTitle "Course Load Calculator"
--        mconcat $ map row classes
        $(widgetFile "homepage")
--      where row x = [julius|v[#{rawJS x}] = #{rawJS x};|]
