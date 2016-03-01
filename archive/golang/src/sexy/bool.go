package sexy

type SxyBool bool

func (b SxyBool) Answers     () SxyBool {}
func (b SxyBool) Autos       () SxyPair {}
func (b SxyBool) Default     () SxyProc {}
func (b SxyBool) Messages    () SxyPair {}
func (b SxyBool) Resends     () SxyPair {}
func (b SxyBool) Send        () SxyObj  {}
func (b SxyBool) ToBool      () SxyBool {}
func (b SxyBool) ToText      () SxyText {}
func (b SxyBool) ToLiteral   () SxyText {}

func (b SxyBool) Type () SxySymbol {
    return "bool"
}

func (b SxyBool) View () SxyText {

}

