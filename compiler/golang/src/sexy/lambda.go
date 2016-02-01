
package sexy

/*
    proc / λ / operator

    arity
    code
    env
    formals
    (apply (args) (: opts))
*/

type Sxyλ struct {
    arity SxyInt
    formals []SxySymbol
    code SxyText
    env SxyEnv
    procedure func(
}


func (p Sxyλ) Answers (m SxyObj) SxyBool {
    // return messages.has? m
}

func (p Sxyλ) Autos SxyPair {
    // return arity code env formals
}

func (p Sxyλ) Default SxyProc {
    // fn to accept ints and return vals
}

func (p Sxyλ) Messages SxyPair {
    // arity code env formals apply
}

func (p Sxyλ) Resends SxyPair {
    // return ()
}

func (p Sxyλ) Send (m SxyObj) SxyObj {
    // the money shot
}

func (p Sxyλ) ToBool SxyBool {
    return true
}

func (p Sxyλ) ToLiteral SxyText {
    return p.code
}

func (p Sxyλ) ToText SxyText {
    return p.code
}

func (p Sxyλ) Type SxySymbol {
    return SxySymbolTable("λ")
}

func (p Sxyλ) View SxyText {
    return p.code
}

func (p Sxyλ) Apply (args SxyPair, opts SxyRecord, cont Sxyλ, err SxyProc) SxyObj {

}


