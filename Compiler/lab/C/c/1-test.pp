Progr [
  Afunc (
    NewFunc [Type Tint]
    (NoPointer (NewFuncDec (Name (Ident "incr")) (AllSpec (ParamDec (TypeAndParam [Type Tint] (NoPointer (Name (Ident "a"))))))))
    (ScompTwo [JumpS (SjumpFive (Eplus (Evar (Ident "a")) (Econst (Eint 1))))])
  ),
  Afunc (
    NewFunc [Type Tint]
    (NoPointer (NewFuncDec (Name (Ident "main")) (AllSpec (ParamDec (OnlyType [Type Tvoid])))))
    (
      ScompFour
        [
          Declarators [Type Tint]
            [InitDecl
              (NoPointer (Name (Ident "a")))
              (InitExpr (Econst (Eint 1)))],
          Declarators [Type Tint]
            [InitDecl
              (NoPointer (Name (Ident "b")))
              (InitExpr (Efunkpar (Evar (Ident "incr")) [Evar (Ident "a")]))]
        ]
      [JumpS (SjumpFive (Evar (Ident "b")))]
    )
  )
]
