package sexy

type SxyObject struct {
    goValue   interface {}
    messages  map[*SxySymbol]SxyObj
    autos     map[*SxySymbol]bool
    resends   map[*SxySymbol]SxyObj
    defProc   SxyObj
}

var SxyObjBuiltIns map[string]bool{
    "answers?": true,
    "autos": true,
    "default": true,
    "messages": true,
    "resends": true,
    "to-bool": true,
    "to-text": true,
    "to-literal": true,
    "type": true,
    "view": true
}

func init () {

}

func CreateObject (msgs []SxyObject, ottos []SxyObject, rsends, dProc SxyObj) SxyObject {

}

func (obj SxyObject) Answers (msg SxySymbol) SxyBool {
    val, ok := messages[*msg]

    if ok {
        return ok
    }

    val, ok = resends[*msg]

    if ok {
        return ok
    }

    return false
}

func (obj SxyObject) Autos () SxyPair {
    for m, _a := range obj.autos {
        // make list of pairs and return
    }
}

func (obj SxyObject) Default () SxyProc {
    return obj.defProc
}

func (obj *SxyObject) Messages () (rv SxyPair) {
    // messages + resends
    for m, _ := range obj.messages {
        // make list of pairs
    }

    rs := obj.Resends()

    return ms + rs
}

func (obj SxyObject) Resends () (rv SxyPair) {
    for r, _ := range obj.resends {
        // make list of pairs
    }

    return rv
}

func (obj *SxyObject) Send (msg SxySymbol, ukont Sxyλ, ekont Sxyλ) SxyThing {
    val, ok := messages[*msg]

    if ok {
        _, isAuto = autos[*msg]

        if isAuto {
            // exec and return value
        }

        return val
    }

    val, ok = resends[*msg]

    if ok {
        return val
    }

    obj.defProc.apply(msg) // probably needs to be a list... we'll see
}

func (obj SxyObject) ToBool (rv SxyBool) {
    return true
}

func (obj SxyObject) ToText SxyText {
    return "object"
}

func (obj SxyObject) ToLiteral () SxyText {
    return "object"
}

func (obj SxyObject) Type () SxySymbol {
    return SxySymbolTable("object")
}

func (obj SxyObject) View () SxyText {
    return "object"
}


