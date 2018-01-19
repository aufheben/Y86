Progr [
  Afunc (
    NewFunc [Type Tint]
      (NoPointer (NewFuncDec (Name (Ident "main")) (AllSpec (ParamDec (OnlyType [Type Tvoid])))))
      (ScompFour
        [
          Declarators [Type Tint] [InitDecl (NoPointer (Name (Ident "a"))) (InitExpr (Eplus (Econst (Eint 1)) (Econst (Eint 2))))]
        ]
        [
          JumpS (SjumpFive (Econst (Eoctal (Octal "0"))))
        ]
      )
  )
]

