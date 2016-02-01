package sexy

type SxyObject struct {
    messages  map[*SxySymbol]SxyObj
    autos     map[*SxySymbol]bool
    resends   map[*SxySymbol]SxyObj
    defProc   SxyProc
}

var SxyObjBuiltIns map[string]bool{
    "answers?": true,
    "autos": true,
    "default" true,
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

func CreateObject (msgs, ottos, rsends, dProc SxyObj) SxyObject {
    // FIXME needs to be a SxyProc
}

func BuiltInMessages (o SxyObj) {
    switch string(msg) {
    case: "answers?"
        return o.Answers // return function that takes message?
    case: "autos"
        return o.Autos()
    case: "default"
        return o.Default()
    case: "messages"
        return o.Messages()
    case: "resends"
        return o.Resends()
    case: "to-bool"
        return o.ToBool()
    case: "to-text"
        return o.ToText()
    case: "to-literal"
        return o.ToLiteral()
    case: "type"
        return o.Type()
    case: "view"
        return o.View()
    }
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

func (obj SxyObject) Send (msg SxySymbol) SxyObj {
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


