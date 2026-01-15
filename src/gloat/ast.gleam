import glance as g
import gleam/option
import gloat/types

pub type Loc =
  g.Span

pub const unknown_loc: Loc = g.Span(-1, -1)

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
    constructors: List(
      #(String, Loc, List(#(option.Option(String), types.Type)), Loc),
    ),
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
  Elist(List(Expr), option.Option(Expr), Loc)
  Ebitstring(List(#(Expr, List(BitStringSegmentOption(Expr)))), Loc)
  Eecho(option.Option(Expr), option.Option(Expr), Loc)
  Erecord(
    module: option.Option(String),
    name: String,
    fields: List(#(option.Option(String), Expr)),
    loc: Loc,
  )
  ErecordUpdate(
    module: option.Option(String),
    name: String,
    record: Expr,
    fields: List(#(String, Expr)),
    loc: Loc,
  )
  Efield(Expr, String, Loc)
  Elambda(List(Pat), Expr, Loc)
  Eapp(Expr, List(Expr), Loc)
  Elet(List(#(Pat, Expr)), Expr, Loc)
  Ematch(Expr, List(#(Pat, option.Option(Expr), Expr)), Loc)
}

pub type Pat {
  Pany(Loc)
  Pvar(String, Loc)
  Ptuple(List(Pat), Loc)
  Plist(List(Pat), option.Option(Pat), Loc)
  Pas(String, Pat, Loc)
  Pconcat(String, option.Option(String), option.Option(String), Loc)
  Pbitstring(List(#(Pat, List(BitStringSegmentOption(Pat)))), Loc)
  Pcon(String, Loc, List(#(option.Option(String), Pat)), Loc)
  Pstr(String, Loc)
  Pprim(Prim, Loc)
}

pub type BitStringSegmentOption(t) {
  BytesOption
  IntOption
  FloatOption
  BitsOption
  Utf8Option
  Utf16Option
  Utf32Option
  Utf8CodepointOption
  Utf16CodepointOption
  Utf32CodepointOption
  SignedOption
  UnsignedOption
  BigOption
  LittleOption
  NativeOption
  SizeValueOption(t)
  SizeOption(Int)
  UnitOption(Int)
}
