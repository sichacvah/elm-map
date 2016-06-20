module UrlTemplate exposing (getTileUrl)

import Regex exposing (HowMany, replace, regex, Regex, Match)
import Dict exposing (Dict)
import String


getTileUrl : { x : Float, y : Float } -> Float -> String -> String -> String
getTileUrl { x, y } z s url =
    let
        data =
            Dict.fromList
                [ ( "s", getTileSubdomain s x y )
                , ( "x", toString <| abs x )
                , ( "y", toString <| abs y )
                , ( "z", toString <| z )
                ]
    in
        template url data


type alias Data =
    Dict String String


getTileSubdomain : String -> Float -> Float -> String
getTileSubdomain subdomains x y =
    let
        subdomains' =
            String.toList subdomains

        index =
            (round (x + y)) % (List.length subdomains')

        subList =
            List.drop index subdomains'
    in
        case subList of
            [] ->
                "a"

            xs :: _ ->
                String.fromList [ xs ]


template : String -> Data -> String
template url data =
    let
        templateRe =
            regex "\\{ *([\\w_\\-]+) *\\}"
    in
        replace Regex.All templateRe (replaceStr data) url


replaceStr : Data -> Match -> String
replaceStr data { match, submatches } =
    case submatches of
        [] ->
            match

        xs :: _ ->
            case xs of
                Nothing ->
                    match

                Just key ->
                    Maybe.withDefault match (Dict.get key data)
