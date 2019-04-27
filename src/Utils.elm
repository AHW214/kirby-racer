module Utils exposing
    ( window
    , halfWindow
    )

type alias Dimensions =
    { w : Float , h : Float }

window : Dimensions
window = { w = 1920, h = 1080 }

halfWindow : Dimensions
halfWindow = half window

half : Dimensions -> Dimensions
half ({ w, h } as dim) =
    { dim | h = h / 2, w = w /2 }