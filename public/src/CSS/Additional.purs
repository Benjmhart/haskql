module CSS.Additional where
  
import Prelude (($), (<<<), (<>), flip)
import Data.Int (toNumber)
import CSS.Stylesheet(select)
import CSS.Stylesheet (key)
import CSS.String (fromString)
import CSS (CSS, margin, padding, Size)
import CSS.Border (borderRadius)

textRendering :: String -> CSS
textRendering = key $ fromString "text-rendering"

webKitFontSmoothing :: String -> CSS
webKitFontSmoothing = key $ fromString "-webkit-font-smoothing"

borderNone :: CSS
borderNone = (key $ fromString "border") $ "none"

select_ :: String -> CSS -> CSS
select_ = select <<< fromString <<< ((<>) ".")

selectHover ::  String -> CSS -> CSS
selectHover = select <<< fromString <<< ((flip (<>)) ":hover")<<< ((<>)".")

applySelector :: (String -> CSS -> CSS) -> String -> CSS -> CSS
applySelector f s c = (f s) $ c 

applySelect_ :: String -> CSS -> CSS
applySelect_ = applySelector select_

applyHover :: String -> CSS -> CSS
applyHover = applySelector selectHover

infix 5 applySelect_ as ??
infix 5 applyHover as ?:

sizeFromInt :: forall a. Int -> (Number -> Size a)  -> Size a
sizeFromInt i f = f $ toNumber i

infix 5 sizeFromInt as ##


margin1 :: forall a. Size a -> CSS
margin1 s = margin s s s s

margin2 :: forall a. Size a -> Size a -> CSS
margin2 x y = margin x y x y

margin3 :: forall a. Size a -> Size a -> Size a -> CSS
margin3 x y z = margin x y z y

margin4 :: forall a. Size a -> Size a -> Size a -> Size a -> CSS
margin4 = margin

padding1 :: forall a. Size a -> CSS
padding1 s = padding s s s s

padding2 :: forall a. Size a -> Size a -> CSS
padding2 x y = padding x y x y

padding3 :: forall a. Size a -> Size a -> Size a -> CSS
padding3 x y z = padding x y z y

padding4 :: forall a. Size a -> Size a -> Size a -> Size a -> CSS
padding4 = padding

borderRadius_ :: forall a. Size a -> CSS
borderRadius_ x = borderRadius x x x x