package sexy

type SxyThing interface {
    Answers     SxyBool
    Autos       SxyPair
    Default     SxyProc
    Messages    SxyPair
    Resends     SxyPair
    Send        SxyObj
    ToBool      SxyBool
    ToText      SxyText
    ToLiteral   SxyText
    Type        SxySymbol
    View        SxyText
}

