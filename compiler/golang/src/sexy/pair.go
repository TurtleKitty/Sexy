package sexy

type SxyPair struct {
    head SxyObj
    tail SxyObj
}

func (p SxyPair) Answers (m SxyObj) SxyBool {
    // return messages.has? m
}

func (p SxyPair) Autos SxyPair {
    // return autos
}

func (p SxyPair) Default SxyProc {
    // fn to accept ints and return vals
}

func (p SxyPair) Messages SxyPair {
    // ...
}

func (p SxyPair) Resends SxyPair {
    // return ()
}

func (p SxyPair) Send (m SxyObj) SxyObj {
    // the money shot
}

func (p SxyPair) ToBool SxyBool {
    // to-bool
}

func (p SxyPair) ToText SxyText {
    // to-bool
}

func (p SxyPair) ToLiteral SxyText {
    // to-bool
}

func (p SxyPair) Type SxySymbol {
    return SxySymbolTable("pair")
}

func (p SxyPair) View SxyText {
    // return (x . y) ?
}

