module Model.Event exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Model.Event.Category exposing (EventCategory(..))
import Model.Interval as Interval exposing (Interval)
import Debug exposing(toString)


type alias Event =
    { title : String
    , interval : Interval
    , description : Html Never
    , category : EventCategory
    , url : Maybe String
    , tags : List String
    , important : Bool
    }


categoryView : EventCategory -> Html Never
categoryView category =
    case category of
        Academic ->
            text "Academic"

        Work ->
            text "Work"

        Project ->
            text "Project"

        Award ->
            text "Award"


sortByInterval : List Event -> List Event
sortByInterval events =
    events |> List.sortWith (\firstEvent secondEvent -> compareTwoIntervals firstEvent.interval secondEvent.interval)
    --Debug.todo "Implement Event.sortByInterval"

compareTwoIntervals : Interval -> Interval -> Order
compareTwoIntervals firstEvent secondEvent = Interval.compare firstEvent secondEvent


view : Event -> Html Never
view event = 
    div [ classList [ ("event", True), ("event-important", event.important ) ] ] 
    [
        h3 [ class "event-title" ] [ text event.title ],
        p  [ class "event-description" ]  [ event.description ],
        p  [ class "event-category" ] [ text (toString event.category) ],
        p  [ class "event-url" ] [ a [ href (Maybe.withDefault event.title event.url) ] [ text "URL"] ],
        p  [ class "event-interval" ] [ Interval.view event.interval ]
    ]
    --Debug.todo "Implement the Model.Event.view function"
