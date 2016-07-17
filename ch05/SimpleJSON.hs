--file: ch05/SimpleJSON.hs

data JValue = JString String | JNumber Double | JBool Bool | JNull | JObject [ ( String, JValue ) ] | JArray [JValue] 
                                                                                                      deribing(Eq, Ord, Show)


