module Constants exposing (apiUrl)


dev : Bool
dev =
    True


apiUrl : String
apiUrl =
    if dev then
        "http://localhost:8080"

    else
        "https://cooperative-crosswords.onrender.com"
