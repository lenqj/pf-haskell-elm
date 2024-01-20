module Model.PersonalDetails exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


type alias DetailWithName =
    { name : String
    , detail : String
    }


type alias PersonalDetails =
    { name : String
    , contacts : List DetailWithName
    , intro : String
    , socials : List DetailWithName
    }


view : PersonalDetails -> Html msg
view details =
    div []
    [ 
        h1 [ id "name" ] [ text details.name ],
        em [ id "intro" ] [ text details.intro ],
        ol []
        [
            li [] 
            [ 
                text "Contacts:", ul [] (details.contacts |> List.map (\curr -> li [ class "contact-detail" ] [ text (curr.name ++ " " ++ curr.detail) ])) 
            ],
            li [] 
            [ 
                text "Socials:", ul [] (details.socials |> List.map (\curr -> li [ class "social-link" ] [ a [ href curr.detail ] [ text curr.name ] ])) 
            ]
        ]
    ]
    --Debug.todo "Implement the Model.PersonalDetails.view function"
