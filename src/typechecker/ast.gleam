import gleam/option
import typechecker/types

pub type Loc =
  Int

pub type Prim {
  Pint(value: Int, loc: Loc)
  Pfloat(value: Float, loc: Loc)
  Pbool(value: Bool, loc: Loc)
}

pub type Top {
  Tdef(name: String, name_loc: Loc, expr: Expr, loc: Loc)
  Texpr(expr: Expr, loc: Loc)
  Tdeftype(
    name: String,
    name_loc: Loc,
    args: List(#(String, Loc)),
    constructors: List(#(String, Loc, List(types.Type), Loc)),
    loc: Loc,
  )
  Ttypealias(
    name: String,
    name_loc: Loc,
    args: List(#(String, Loc)),
    type_: types.Type,
    loc: Loc,
  )
}

pub type Cst {
  CstList(items: List(Cst), loc: Loc)
  CstArray(items: List(Cst), loc: Loc)
  CstSpread(value: Cst, loc: Loc)
  CstId(name: String, loc: Loc)
  CstString(value: String, templates: List(#(Cst, String, Loc)), loc: Loc)
}

pub type Quot {
  QuotExpr(Expr)
  QuotTop(Top)
  QuotType(types.Type)
  QuotPat(Pat)
  QuotQuot(Cst)
}

pub type Expr {
  Eprim(Prim, Loc)
  Evar(String, Loc)
  Estr(String, List(#(Expr, String, Loc)), Loc)
  Equot(Quot, Loc)
  Etuple(List(Expr), Loc)
  EtupleIndex(Expr, Int, Loc)
  Elambda(List(Pat), Expr, Loc)
  Eapp(Expr, List(Expr), Loc)
  Elet(List(#(Pat, Expr)), Expr, Loc)
  Ematch(Expr, List(#(Pat, Expr)), Loc)
}

pub type Pat {
  Pany(Loc)
  Pvar(String, Loc)
  Ptuple(List(Pat), Loc)
  Plist(List(Pat), option.Option(Pat), Loc)
  Pas(String, Pat, Loc)
  Pcon(String, Loc, List(Pat), Loc)
  Pstr(String, Loc)
  Pprim(Prim, Loc)
}
