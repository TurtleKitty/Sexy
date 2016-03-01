
package sexy

type SxySymbol string

var symbolTable map[string]SxySymbol

func init() {
    symbolTable = make(map[string]SxySymbol)
}

func SxySymbolTable (s string) *SxySymbol {
    var v, has = symbolTable[s]

    if has {
        return &v
    }

    var noob SxySymbol = s

    symbolTable[s] = noob

    return &noob
}

func (s *SxySymbol) Send (msg *SxySymbol, ukont Sxyλ, ekont Sxyλ) SxyThing {
    
}

func (s SxySymbol) Answers (msg SxySymbol) SxyBool {

}

func (s SxySymbol) Autos () SxyPair {

}

func (s SxySymbol) Default () SxyProc {

}

func (s SxySymbol) Messages () SxyPair {

}

func (s SxySymbol) Resends () SxyPair {

}

func (s SxySymbol) Send () SxyObj {

}

func (s SxySymbol) ToBool () SxyBool {
    return SxyBool(true)
}

func (s SxySymbol) ToText () SxyText {
    return SxyText(s)
}

func (s SxySymbol) ToLiteral () SxyText {
    return SxyText(s)
}

func (s SxySymbol) Type () SxySymbol {
    return *(SxySymbolTable("symbol"))
}

func (s SxySymbol) View () SxyText {
    return SxyText(s)
}


