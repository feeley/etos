; file: "grammar.scm"

; Copyright (C) 1998-1999 Universite de Montreal, All Rights Reserved.

; This is the grammar for Standard Erlang, in a format suitable for
; use with the LALR-SCM parser generator by Dominique Boucher.  The
; grammar is the one in Appendix H of the document "Specification of
; the Standard Erlang programming language, FINAL DRAFT (0.6), june
; 3, 1998".  Explicit definitions for optional categories have been
; added.  Other variations from the specification are marked with "*****".

;------------------------------------------------------------------------------

(define erlang-grammar '(

; ---------- Terminal symbols:

FullStop                          ; .
Variable                          ; Radius
UniversalPattern                  ; _
OneStringLiteral                  ; "hello"
UnsignedDecimalLiteral            ; 123
NotUnsignedDecimalIntegerLiteral  ; 2#100110
FloatLiteral                      ; 1.25
CharLiteral                       ; $x
NotQuotedAtomLiteral              ; hello
QuotedAtomLiteral                 ; 'Hello World'
AFTER ; "after"
ALL_TRUE ; "all_true"
AND ; "and"
BAND ; "band"
BEGIN ; "begin"
BNOT ; "bnot"
BOR ; "bor"
BSL ; "bsl"
BSR ; "bsr"
BXOR ; "bxor"
CASE ; "case"
CATCH ; "catch"
;COMPILE ; "compile"
COND ; "cond"
;DEFINE ; "define"
DIV ; "div"
;ELSE ; "else"
END ; "end"
;ENDIF ; "endif"
;EXPORT ; "export"
;FILE ; "file"
FUN ; "fun"
IF ; "if"
;IFDEF ; "ifdef"
;IFNDEF ; "ifndef"
;IMPORT ; "import"
;INCLUDE ; "include"
;INCLUDE_LIB ; "include_lib"
LET ; "let"
MOD ; "mod"
;MODULE ; "module"
NOT ; "not"
OF ; "of"
OR ; "or"
;QUERY ; "query"
RECEIVE ; "receive"
;RECORD ; "record"
REM ; "rem"
SOME_TRUE ; "some_true"
TRY ; "try"
;TRUE ; "true"
;UNDEF ; "undef"
WHEN ; "when"
XOR ; "xor"
BANG ; "!"
SHARP ; "#"
PAREN-OPEN ; "("
PAREN-CLOSE ; ")"
STAR ; "*"
PLUS ; "+"
PLUS-PLUS ; "++"
COMMA ; ","
MINUS ; "-"
MINUS-MINUS ; "--"
MINUS-GT ; "->"
PERIOD ; "."
SLASH ; "/"
SLASH-SLASH ; "//"
SLASH-EQUAL ; "/="
COLON ; ":"
SEMICOLON ; ";"
LT ; "<"
LT-MINUS ; "<-"
EQUAL ; "="
EQUAL-SLASH-EQUAL ; "=/="
EQUAL-COLON-EQUAL ; "=:="
EQUAL-LT ; "=<"
EQUAL-EQUAL ; "=="
GT ; ">"
GT-EQUAL ; ">="
QUESTION ; "?"
BRACK-OPEN ; "["
BRACK-CLOSE ; "]"
BRACE-OPEN ; "{"
BAR ; "|"
BAR-BAR ; "||"
BRACE-CLOSE ; "}"

MODULE ; "module"
EXPORT ; "export"
IMPORT ; "import"
COMPILE ; "compile"
RECORD ; "record"

; ---------- The main grammar:

(TopLevelForm ;***** added this so that parser can do better error recovery
  (ModuleAttribute)
    : $1
  (HeaderAttribute)
    : $1
  (RecordDeclaration)
    : $1
  (FunctionDeclaration)
    : $1
  ()
    : (make-end-of-source (token-location (preproc-read-token global-pps)))
)

(ModuleDeclaration
  (FileAttributes ModuleAttribute HeaderForms_opt ProgramForms_opt)
    : (make-prog $4)
  (ModuleAttribute HeaderForms_opt ProgramForms_opt) 
    : (make-prog $3)
)

(FileAttributes
  (FileAttribute)
  (FileAttributes FileAttribute)
)

(ModuleAttribute
  (MINUS MODULE PAREN-OPEN ModuleName PAREN-CLOSE FullStop)
    : (make-module-attribute (generic-location-join $1 $6) $4)
)

(ModuleName
  (AtomLiteral)
    : $1
)

(HeaderForms_opt
  ()
    : #f
  (HeaderForms)
    : #f
)

(HeaderForms
  (HeaderForm)
    : #f
  (HeaderForms HeaderForm)
    : #f
)

(HeaderForm
  (HeaderAttribute)
    : $1
  (AnywhereAttribute)
    : $1
)

(HeaderAttribute
  (ExportAttribute)
    : $1
  (ImportAttribute)
    : $1
  (CompileAttribute)
    : $1
  (WildAttribute)
    : $1
)

(AnywhereAttribute
  (FileAttribute)
    : $1
;  (Directive)
;  (MacroDefinition)
  (RecordDeclaration)
    : $1
)

(ExportAttribute
  (MINUS EXPORT PAREN-OPEN SymbolArityList PAREN-CLOSE FullStop)
    : (make-export-attribute (generic-location-join $1 $6) $4)
)

(SymbolArityList
  (BRACK-OPEN SymbolArities_opt BRACK-CLOSE)
    : $2
)

(SymbolArities_opt
  ()
    : '()
  (SymbolArities)
    : (reverse $1)
)

(SymbolArities
  (SymbolArity)
    : (list $1)
  (SymbolArities COMMA SymbolArity)
    : (cons $3 $1)
)

(SymbolArity
  (FunctionSymbol SLASH Arity)
    : (cons $1 $3)
)

(Arity
  (UnsignedDecimalLiteral)
    : (tok->constnode $1)
)

(ImportAttribute
  (MINUS IMPORT PAREN-OPEN ModuleName COMMA SymbolArityList PAREN-CLOSE FullStop)
    : (make-import-attribute (generic-location-join $1 $8) $4 $6)
)

(CompileAttribute
  (MINUS COMPILE PAREN-OPEN BRACK-OPEN Terms_opt BRACK-CLOSE PAREN-CLOSE FullStop)
    : (make-compile-attribute (generic-location-join $1 $8) $5)
)

(FileAttribute
;  (MINUS FILE PAREN-OPEN StringLiteral COMMA LineNumeral PAREN-CLOSE FullStop)
  ()
)

(LineNumeral
  (UnsignedDecimalLiteral)
)

(WildAttribute
  (MINUS WildAtomLiteral PAREN-OPEN Term PAREN-CLOSE FullStop)
  ;***** was (MINUS AtomLiteral PAREN-OPEN Term PAREN-CLOSE FullStop)
    : (make-wild-attribute (generic-location-join $1 $6) $2 $4)
)

(ProgramForms_opt
  ()
    : '()
  (ProgramForms)
    : (reverse $1)
)

(ProgramForms
  (FunctionDeclaration)
    : (list $1)
  (ProgramForms FunctionDeclaration)
    : (cons $2 $1)
  (ProgramForms AnywhereAttribute)
    : $1
)

(FunctionDeclaration
  (FunctionClauses FullStop)
    : (make-topfundef (reverse $1) $2)
)

(FunctionClauses
  (FunctionClause)
    : (list $1)
  (FunctionClauses SEMICOLON FunctionClause)
    : (cons $3 $1)
)

(FunctionClause
  (AtomLiteral FunClause)
    : (make-namedfunclause $1 $2)
)

(RecordDeclaration
  (MINUS RECORD PAREN-OPEN RecordType COMMA RecordDeclTuple PAREN-CLOSE FullStop)
    : (make-record-declaration (generic-location-join $1 $8) $4 $6)
)

(RecordDeclTuple
  (BRACE-OPEN RecordFieldDecls_opt BRACE-CLOSE)
    : $2
)

(RecordFieldDecls_opt
  ()
    : '()
  (RecordFieldDecls)
    : (reverse $1)
)

(RecordFieldDecls
  (RecordFieldDecl)
    : (list $1)
  (RecordFieldDecls COMMA RecordFieldDecl)
    : (cons $3 $1)
)

(RecordFieldDecl
  (RecordFieldName RecordFieldValue_opt)
    : (cons $1 $2)
)

(Pattern
  (Pattern EQUAL SimplePattern)
    : (make-match $1 $3)
  (SimplePattern)
    : $1
)

(Pattern2 ;***** as suggested in section 2.5.3
  (ApplicationExpr)
    : $1
)

(SimplePattern
  (AtomicLiteral)
    : $1
  (Variable)
    : (tok->varnode $1)
  (UniversalPattern)
    : (make-universal $1)
  (TuplePattern)
    : $1
  (ListPattern)
    : $1
)

(TuplePattern
  (BRACE-OPEN Patterns_opt BRACE-CLOSE)
    : (make-tupleast $1 $2 $3)
  (RecordPattern)
    : $1
)

(ListPattern
  (BRACK-OPEN BRACK-CLOSE)
    : (make-node 'const (generic-location-join $1 $2) (erl-nil))
  (BRACK-OPEN Patterns ListPatternTail_opt BRACK-CLOSE)
    : (make-cons $1 (reverse $2) $3 $4)
)

(ListPatternTail_opt
  ()
    : (newast-const (erl-nil))
  (ListPatternTail)
    : $1
)

(ListPatternTail
  (BAR Pattern)
    : $2
)

(Patterns_opt
  ()
    : '()
  (Patterns)
    : (reverse $1)
)

(Patterns
  (Pattern)
    : (list $1)
  (Patterns COMMA Pattern)
    : (cons $3 $1)
)

(RecordPattern
  (SHARP RecordType RecordPatternTuple)
    : (make-record-pat $1 $2 $3)
)

(RecordType
  (AtomLiteral)
    : $1
)

(RecordPatternTuple
  (BRACE-OPEN RecordFieldPatterns_opt BRACE-CLOSE)
    : $2
)

(RecordFieldPatterns_opt
  ()
    : '()
  (RecordFieldPatterns)
    : (reverse $1)
)

(RecordFieldPatterns
  (RecordFieldPattern)
    : (list $1)
  (RecordFieldPatterns COMMA RecordFieldPattern)
    : (cons $3 $1)
)

(RecordFieldPattern
  (RecordFieldName EQUAL Pattern)
    : (cons $1 $3)
)

(RecordFieldName
  (AtomLiteral)
    : $1
)

(Body
  (Exprs)
    : (make-body (reverse $1))
)

(Exprs_opt
  ()
    : '()
  (Exprs)
    : (reverse $1)
)

(Exprs
  (Expr)
    : (list $1)
  (Exprs COMMA Expr)
    : (cons $3 $1)
)

(Expr
  (CATCH Expr)
    : (make-catch $1 $2)
  (MatchExpr)
    : $1
)

(MatchExpr
  (Pattern2 EQUAL SendExpr)
    : (make-match $1 $3)
  (SendExpr)
    : $1
)

(SendExpr
  (DisjunctionExpr BANG SendExpr)
    : (make-send $1 $3)
  (DisjunctionExpr)
    : $1
)

(DisjunctionExpr
  (DisjunctionExpr OR ConjunctionExpr)
    : (make-binop 'or $1 $3)
  (DisjunctionExpr XOR ConjunctionExpr)
    : (make-binop 'xor $1 $3)
  (ConjunctionExpr)
    : $1
)

(ConjunctionExpr
  (ConjunctionExpr AND CompareExpr)
    : (make-binop 'and $1 $3)
  (CompareExpr)
    : $1
)

(CompareExpr
  (ListConcExpr RelationalOp ListConcExpr)
    : (make-compare $2 $1 $3)
  (ListConcExpr EqualityOp ListConcExpr)
    : (make-compare $2 $1 $3)
  (ListConcExpr)
    : $1
)

(RelationalOp
  (LT) 
    : '<
  (EQUAL-LT) 
    : '<=
  (GT) 
    : '>
  (GT-EQUAL) 
    : '>=
)

(EqualityOp
  (EQUAL-COLON-EQUAL)
    : '=:=
  (EQUAL-SLASH-EQUAL)
    : '=/=
  (EQUAL-EQUAL)
    : '==
  (SLASH-EQUAL)
    : '/=
)

(ListConcExpr
  (AdditionShiftExpr ListConcOp ListConcExpr)
    : (make-binop $2 $1 $3)
  (AdditionShiftExpr)
    : $1
)

(ListConcOp
  (PLUS-PLUS)
    : '++
  (MINUS-MINUS)
    : '--
)

(AdditionShiftExpr
  (AdditionShiftExpr AdditionOp MultiplicationExpr)
    : (make-binop $2 $1 $3)
  (AdditionShiftExpr ShiftOp MultiplicationExpr)
    : (make-binop $2 $1 $3)
  (MultiplicationExpr)
    : $1
)

(AdditionOp
  (PLUS)
    : '+
  (MINUS)
    : '-
  (BOR)
    : 'bor
  (BXOR)
    : 'bxor
)

(ShiftOp
  (BSL)
    : 'bsl
  (BSR)
    : 'bsr
)

(MultiplicationExpr
  (MultiplicationExpr MultiplicationOp PrefixOpExpr)
    : (make-binop $2 $1 $3)
  (PrefixOpExpr)
    : $1
)

(MultiplicationOp
  (STAR) : '*
  (SLASH) : '/
  (SLASH-SLASH) : '//
  (DIV) : 'div
  (MOD) : 'mod
  (REM) : 'rem
  (BAND): 'band
)

(PrefixOpExpr
  (PrefixOp RecordExpr)
    : (let ((optok $1))
	(make-unop (car optok) (cdr optok) $2))
  (RecordExpr)
    : $1
)

(PrefixOp
  (PLUS)
    : (cons '+ $1)
  (MINUS) 
    : (cons '- $1)
  (BNOT)
    : (cons 'bnot $1)
  (NOT)
    : (cons 'not $1)
)

(RecordExpr_opt
  ()
    : #f
  (RecordExpr)
    : $1
)

(RecordExpr
  (RecordExpr_opt SHARP RecordType PERIOD RecordFieldName)
    : (make-record-idx $1 $2 $3 $5)
  (RecordExpr_opt SHARP RecordType RecordUpdateTuple)
    : (make-record-upd $1 $3 $4)
  (ApplicationExpr)
    : $1
)

(RecordUpdateTuple
  (BRACE-OPEN RecordFieldUpdates_opt BRACE-CLOSE)
    : $2
)

(RecordFieldUpdates_opt
  ()
    : '()
  (RecordFieldUpdates)
    : (reverse $1)
)

(RecordFieldUpdates
  (RecordFieldUpdate)
    : (list $1)
  (RecordFieldUpdates COMMA RecordFieldUpdate)
    : (cons $3 $1)
)

(RecordFieldUpdate
  (RecordFieldName RecordFieldValue)
    : (cons $1 $2)
)

(RecordFieldValue_opt
  ()
    : (newast-const (erl-atom<-string "undefined"))
  (RecordFieldValue)
    : $1
)

(RecordFieldValue
  (EQUAL Expr)
    : $2
)

(ApplicationExpr
  (FunctionSymbol PAREN-OPEN Exprs_opt PAREN-CLOSE)
    : (make-apply-symb $1 $3 $4)
  (NonAtomLiteralPrimaryExpr PAREN-OPEN Exprs_opt PAREN-CLOSE) ;***** was (PrimaryExpr PAREN-OPEN Exprs_opt PAREN-CLOSE)
    : (make-apply $1 $3 $4)
  (PrimaryExpr COLON PrimaryExpr PAREN-OPEN Exprs_opt PAREN-CLOSE)
    : (make-remoteapply $1 $3 $5 $6)
  (PrimaryExpr)
    : $1
)

(FunctionSymbol
  (AtomLiteral) ;***** was (AtomicLiteral)
    : $1
)

(PrimaryExpr ;***** split into 2 to expose "NonAtomLiteralPrimaryExpr"
  (AtomLiteral) ;***** was (AtomicLiteral)
    : $1
  (NonAtomLiteralPrimaryExpr)
    : $1
)

(NonAtomLiteralPrimaryExpr
  (NonAtomAtomicLiteral)
    : $1
  (Variable)
    : (tok->varnode $1)
  (TupleSkeleton)
    : $1
  (ListSkeleton)
    : $1
  (ListComprehension)
    : $1
  (BlockExpr)
    : $1
  (AllTrueExpr)
    : $1
  (SomeTrueExpr)
    : $1
  (CondExpr)
    : $1
  (IfExpr)
    : $1
  (CaseExpr)
    : $1
  (ReceiveExpr)
    : $1
  (TryExpr)
    : $1
  (FunExpr)
    : $1
  (ParenthesizedExpr)
    : $1
  (UniversalPattern) ;***** added as suggested in section 2.5.3
    : (make-universal $1)
)

(TupleSkeleton
  (BRACE-OPEN Exprs_opt BRACE-CLOSE)
    : (make-tupleast $1 $2 $3)
)

(ListSkeleton
  (BRACK-OPEN BRACK-CLOSE)
    : (make-node 'const (generic-location-join $1 $2) (erl-nil))
  (BRACK-OPEN Exprs ListSkeletonTail_opt BRACK-CLOSE)
    : (make-cons $1 (reverse $2) $3 $4)
)

(ListSkeletonTail_opt
  ()
    : (newast-const (erl-nil))
  (ListSkeletonTail)
    : $1
)

(ListSkeletonTail
  (BAR Expr)
    : $2
)

(ListComprehension
  (BRACK-OPEN Expr BAR-BAR ListComprehensionExprs BRACK-CLOSE)
    : (make-node 'lc (generic-location-join $1 $5) $2 (reverse $4))
)

(ListComprehensionExprs
  (ListComprehensionExpr)
    : (list $1)
  (ListComprehensionExprs COMMA ListComprehensionExpr)
    : (cons $3 $1)
)

(ListComprehensionExpr
  (Generator)
    : $1
  (Filter)
    : $1
)

(Generator
  (Pattern2 LT-MINUS Expr)
    : (make-generator $1 $3)
)

(Filter
  (Expr)
    : $1
)

(BlockExpr
  (BEGIN Body END)
    : (make-block 'begin $1 $2 $3)
)

(AllTrueExpr
  (ALL_TRUE Exprs END)
    : (make-block 'all_true $1 (reverse $2) $3)
)

(SomeTrueExpr
  (SOME_TRUE Exprs END)
    : (make-block 'some_true $1 (reverse $2) $3)
)

(CondExpr
  (COND CondClauses END)
    : (make-block 'cond $1 (reverse $2) $3)
)

(CondClauses
  (CondClause)
    : (list $1)
  (CondClauses SEMICOLON CondClause)
    : (cons $3 $1)
)

(CondClause
  (Expr ClauseBody)
    : (make-cdclause $1 $2)
)

(ClauseBody
  (MINUS-GT Body)
    : $2
)

(IfExpr
  (IF IfClauses END)
    : (make-block 'if $1 (reverse $2) $3)
)

(IfClauses
  (IfClause)
    : (list $1)
  (IfClauses SEMICOLON IfClause)
    : (cons $3 $1)
)

(IfClause
  (Guard ClauseBody)
    : (make-ifclause $1 $2)
)

(CaseExpr
  (CASE Body OF CrtClauses END)
    : (make-case $1 $2 (reverse $4) $5)
)

(CrtClauses_opt
  ()
    : '()
  (CrtClauses)
    : (reverse $1)
)

(CrtClauses
  (CrtClause)
    : (list $1)
  (CrtClauses SEMICOLON CrtClause)
    : (cons $3 $1)
)

(CrtClause
  (Pattern ClauseGuard_opt ClauseBody)
    : (make-crtclause $1 $2 $3)
)

(ClauseGuard_opt
  ()
    : (list (truen))
  (ClauseGuard)
    : $1
)

(ClauseGuard
  (WHEN Guard)
    : $2
)

(Guard
  (Body)
    : (node-val2 $1)
)

(ReceiveExpr
  (RECEIVE CrtClauses END)
    : (make-receive $1
		    (reverse $2)
		    (newast-const (erl-atom<-string "infinity"))
		    (list (newast-const 0))
		    $3)
  (RECEIVE CrtClauses_opt AFTER Expr ClauseBody END)
    : (make-receive $1 $2 $4 $5 $6)
)

(TryExpr
  (TRY Body CATCH CrtClauses END)
    : (make-try $1 $2 (reverse $4) $5)
  (TRY Body END)
    : (let ((var (ast-genvar)))
	(make-try $1 
		  $2 
		  (list (make-crtclause (node-copy var)
					(list (truen))
					(node-copy var)))
		  $3))
)

(FunExpr
  (FUN SymbolArity) ;***** was (FUN NameArity)
    : (make-funref $1 $2)
  (FUN FunClauses END)
    : (make-fundef $1 (reverse $2) $3)
)

(FunClauses
  (FunClause)
    : (list $1)
  (FunClauses SEMICOLON FunClause)
    : (cons $3 $1)
)

(FunClause
  (PAREN-OPEN Patterns_opt PAREN-CLOSE ClauseGuard_opt ClauseBody)
    : (make-funclause $1 $2 $4 $5)
)

;(QueryExpr ;***** not defined by Standard
;  (QUERY ListComprehension END)
;)

(ParenthesizedExpr
  (PAREN-OPEN Expr PAREN-CLOSE)
    : $2
)

#|
;***** this section is not needed because a "Guard" is defined as a "Body" (see above)

(Guard
  (GuardTest)
  (Guard COMMA GuardTest)
)

(Guard ;***** duplicate?
  (GuardTest)
  (Guard COMMA GuardTest)
)

(GuardTest
  (TRUE)
  (GuardRecordTest)
  (GuardRecognizer)
  (GuardTermComparison)
  (ParenthesizedGuardTest)
)

(GuardRecordTest
  (RECORD PAREN-OPEN GuardExpr COMMA RecordType PAREN-CLOSE)
)

(GuardRecognizer
  (RecognizerBIF PAREN-OPEN GuardExpr PAREN-CLOSE)
)

(RecognizerBIF
  (AtomLiteral)
)

(GuardTermComparison
  (GuardExpr RelationalOp GuardExpr)
  (GuardExpr EqualityOp GuardExpr)
)

(ParenthesizedGuardTest
  (PAREN-OPEN GuardTest PAREN-CLOSE)
)

(GuardExpr
  (GuardAdditionShiftExpr)
)

(GuardAdditionShiftExpr
  (GuardAdditionShiftExpr AdditionOp GuardMultiplicationExpr)
  (GuardAdditionShiftExpr ShiftOp GuardMultiplicationExpr)
  (GuardMultiplicationExpr)
)

(GuardMultiplicationExpr
  (GuardMultiplicationExpr MultiplicationOp GuardPrefixOpExpr)
  (GuardPrefixOpExpr)
)

(GuardPrefixOpExpr
  (PrefixOp GuardApplicationExpr)
  (GuardApplicationExpr)
)

(GuardApplicationExpr
  (GuardBIF PAREN-OPEN GuardExprs_opt PAREN-CLOSE)
  (GuardRecordExpr)
  (GuardPrimaryExpr)
)

(GuardBIF
  (AtomLiteral)
)

(GuardExprs_opt
  ()
  (GuardExprs)
)

(GuardExprs
  (GuardExpr)
  (GuardExprs COMMA GuardExpr)
)

(GuardRecordExpr
  (GuardPrimaryExpr_opt SHARP AtomLiteral PERIOD AtomLiteral)
)

(GuardPrimaryExpr_opt
  ()
  (GuardPrimaryExpr)
)

(GuardPrimaryExpr
  (Variable)
  (AtomicLiteral)
  (GuardListSkeleton)
  (GuardTupleSkeleton)
  (ParenthesizedGuardExpr)
)

(GuardListSkeleton
  (BRACK-OPEN BRACK-CLOSE)
  (BRACK-OPEN GuardExprs GuardListSkeletonTail_opt BRACK-CLOSE)
)

(GuardListSkeletonTail_opt
  ()
  (GuardListSkeletonTail)
)

(GuardListSkeletonTail
  (BAR GuardExpr)
)

(GuardTupleSkeleton
  (BRACE-OPEN GuardExprs_opt BRACE-CLOSE)
)

(ParenthesizedGuardExpr
  (PAREN-OPEN GuardExpr PAREN-CLOSE)

)
|#

; ---------- The preprocessor grammar

#|
(Directive
  (MacroDefinition)
  (MacroUndefinition)
  (IncludeDirective)
  (IncludeLibDirective)
  (IfdefDirective)
  (IfndefDirective)
  (ElseDirective)
  (EndifDirective)
)

(MacroDefinition
  (MINUS DEFINE PAREN-OPEN MacroName MacroParams_opt COMMA MacroBodyRpar FullStop)
)

(MacroName
  (AtomLiteral)
  (Variable)
)

(MacroParams_opt
  ()
  (MacroParams)
)

(MacroParams
  (PAREN-OPEN Variables_opt PAREN-CLOSE)
)

(Variables_opt
  ()
  (Variables)
)

(Variables
  (Variable)
  (Variables COMMA Variable)
)

(MacroBodyRpar
  (NonemptyMacroBody_opt PAREN-CLOSE)
  (MacroBodyRpar PAREN-CLOSE)
)

(NonemptyMacroBody_opt
  ()
  (NonemptyMacroBody)
)

(NonemptyMacroBody
  (TokenNotRpar)
  (NonemptyMacroBody TokenNotRpar)
  (MacroBodyRpar TokenNotRpar)
)

(TokenNotRpar
;  (FullStop)
  (Variable)
  (UniversalPattern)
  (OneStringLiteral)
  (UnsignedDecimalLiteral)
  (NotUnsignedDecimalIntegerLiteral)
  (FloatLiteral)
  (CharLiteral)
  (NotSpecialAtomLiteral)
  (AFTER) ; "after"
  (ALL_TRUE) ; "all_true"
  (AND) ; "and"
  (BAND) ; "band"
  (BEGIN) ; "begin"
  (BNOT) ; "bnot"
  (BOR) ; "bor"
  (BSL) ; "bsl"
  (BSR) ; "bsr"
  (BXOR) ; "bxor"
  (CASE) ; "case"
  (CATCH) ; "catch"
  (COMPILE) ; "compile"
  (COND) ; "cond"
  (DEFINE) ; "define"
  (DIV) ; "div"
  (ELSE) ; "else"
  (END) ; "end"
  (ENDIF) ; "endif"
  (EXPORT) ; "export"
  (FUN) ; "fun"
  (IF) ; "if"
  (IFDEF) ; "ifdef"
  (IFNDEF) ; "ifndef"
  (IMPORT) ; "import"
  (INCLUDE) ; "include"
  (INCLUDE_LIB) ; "include_lib"
  (MOD) ; "mod"
  (MODULE) ; "module"
  (NOT) ; "not"
  (OF) ; "of"
  (OR) ; "or"
  (RECEIVE) ; "receive"
  (RECORD) ; "record"
  (REM) ; "rem"
  (SOME_TRUE) ; "some_true"
  (TRY) ; "try"
  (TRUE) ; "true"
  (UNDEF) ; "undef"
  (WHEN) ; "when"
  (XOR) ; "xor"
  (BANG) ; "!"
  (SHARP) ; "#"
  (PAREN-OPEN) ; "("
;  (PAREN-CLOSE) ; ")"
  (STAR) ; "*"
  (PLUS) ; "+"
  (PLUS-PLUS) ; "++"
  (COMMA) ; ","
  (MINUS) ; "-"
  (MINUS-MINUS) ; "--"
  (MINUS-GT) ; "->"
  (PERIOD) ; "."
  (SLASH) ; "/"
  (SLASH-SLASH) ; "//"
  (SLASH-EQUAL) ; "/="
  (COLON) ; ":"
  (SEMICOLON) ; ";"
  (LT) ; "<"
  (LT-MINUS) ; "<-"
  (EQUAL) ; "="
  (EQUAL-SLASH-EQUAL) ; "=/="
  (EQUAL-COLON-EQUAL) ; "=:="
  (EQUAL-LT) ; "=<"
  (EQUAL-EQUAL) ; "=="
  (GT) ; ">"
  (GT-EQUAL) ; ">="
  (QUESTION) ; "?"
  (BRACK-OPEN) ; "["
  (BRACK-CLOSE) ; "]"
  (BRACE-OPEN) ; "{"
  (BAR) ; "|"
  (BAR-BAR) ; "||"
  (BRACE-CLOSE) ; "}"
)

(MacroUndefinition
  (MINUS UNDEF PAREN-OPEN MacroName PAREN-CLOSE FullStop)
)

(MacroApplication
  (QUESTION MacroName MacroArguments_opt)
)

(MacroArguments_opt
  ()
  (MacroArguments)
)

(MacroArguments
  (PAREN-OPEN BalancedExprs_opt PAREN-CLOSE)
)

(BalancedExprs_opt
  ()
  (BalancedExprs)
)

(BalancedExprs
  (BalancedExpr)
  (BalancedExprs COMMA BalancedExpr)
)

(BalancedExpr
  (PAREN-OPEN BalancedExprs PAREN-CLOSE)
  (BRACK-OPEN BalancedExprs BRACK-CLOSE)
  (BRACE-OPEN BalancedExprs BRACE-CLOSE)
  (BEGIN BalancedExprs END)
  (ALL_TRUE BalancedExprs END)
  (SOME_TRUE BalancedExprs END)
  (COND BalancedExprs END)
  (IF BalancedExprs END)
  (CASE BalancedExprs END)
  (RECEIVE BalancedExprs END)
  (TRY BalancedExprs END)
  (FUN BalancedExprs END)
  (NotParenLikes_opt)
)

(NotParenLikes_opt
  ()
  (NotParenLikes)
)

(NotParenLikes
  (NotParenLike)
  (NotParenLikes NotParenLike)
)

(NotParenLike
;  (FullStop)
  (Variable)
  (UniversalPattern)
  (OneStringLiteral)
  (UnsignedDecimalLiteral)
  (NotUnsignedDecimalIntegerLiteral)
  (FloatLiteral)
  (CharLiteral)
  (NotSpecialAtomLiteral)
  (AFTER) ; "after"
;  (ALL_TRUE) ; "all_true"
  (AND) ; "and"
  (BAND) ; "band"
;  (BEGIN) ; "begin"
  (BNOT) ; "bnot"
  (BOR) ; "bor"
  (BSL) ; "bsl"
  (BSR) ; "bsr"
  (BXOR) ; "bxor"
;  (CASE) ; "case"
  (CATCH) ; "catch"
  (COMPILE) ; "compile"
;  (COND) ; "cond"
  (DEFINE) ; "define"
  (DIV) ; "div"
  (ELSE) ; "else"
;  (END) ; "end"
  (ENDIF) ; "endif"
  (EXPORT) ; "export"
;  (FUN) ; "fun"
;  (IF) ; "if"
  (IFDEF) ; "ifdef"
  (IFNDEF) ; "ifndef"
  (IMPORT) ; "import"
  (INCLUDE) ; "include"
  (INCLUDE_LIB) ; "include_lib"
  (MOD) ; "mod"
  (MODULE) ; "module"
  (NOT) ; "not"
  (OF) ; "of"
  (OR) ; "or"
;  (RECEIVE) ; "receive"
  (RECORD) ; "record"
  (REM) ; "rem"
;  (SOME_TRUE) ; "some_true"
;  (TRY) ; "try"
  (TRUE) ; "true"
  (UNDEF) ; "undef"
  (WHEN) ; "when"
  (XOR) ; "xor"
  (BANG) ; "!"
  (SHARP) ; "#"
;  (PAREN-OPEN) ; "("
;  (PAREN-CLOSE) ; ")"
  (STAR) ; "*"
  (PLUS) ; "+"
  (PLUS-PLUS) ; "++"
;  (COMMA) ; ","
  (MINUS) ; "-"
  (MINUS-MINUS) ; "--"
  (MINUS-GT) ; "->"
  (PERIOD) ; "."
  (SLASH) ; "/"
  (SLASH-SLASH) ; "//"
  (SLASH-EQUAL) ; "/="
  (COLON) ; ":"
  (SEMICOLON) ; ";"
  (LT) ; "<"
  (LT-MINUS) ; "<-"
  (EQUAL) ; "="
  (EQUAL-SLASH-EQUAL) ; "=/="
  (EQUAL-COLON-EQUAL) ; "=:="
  (EQUAL-LT) ; "=<"
  (EQUAL-EQUAL) ; "=="
  (GT) ; ">"
  (GT-EQUAL) ; ">="
  (QUESTION) ; "?"
;  (BRACK-OPEN) ; "["
;  (BRACK-CLOSE) ; "]"
;  (BRACE-OPEN) ; "{"
  (BAR) ; "|"
  (BAR-BAR) ; "||"
;  (BRACE-CLOSE) ; "}"
)

(IncludeDirective
  (MINUS INCLUDE PAREN-OPEN IncludeFileName PAREN-CLOSE FullStop)
)

(IncludeLibDirective
  (MINUS INCLUDE_LIB PAREN-OPEN IncludeFileName PAREN-CLOSE FullStop)
)

(IncludeFileName
  (OneStringLiteral)
)

(IfdefDirective
  (MINUS IFDEF PAREN-OPEN MacroName PAREN-CLOSE FullStop)
)

(IfndefDirective
  (MINUS IFNDEF PAREN-OPEN MacroName PAREN-CLOSE FullStop)
)

(ElseDirective
  (MINUS ELSE FullStop)
)

(EndifDirective
  (MINUS ENDIF FullStop)
)

|#






;***** from lexical grammar:

(AtomicLiteral ;***** split into 2 to expose "NonAtomAtomicLiteral"
  (AtomLiteral)
    : $1
  (NonAtomAtomicLiteral)
    : $1
)

(NonAtomAtomicLiteral
  (IntegerLiteral)
    : $1
  (FloatLiteral)
    : (tok->constnode $1)
  (CharLiteral)
    : (tok->constnode $1)
  (StringLiteral)
    : $1
)

(StringLiteral
  (OneStringLiteral)
    : (stringtok->ast $1)
  (StringLiteral OneStringLiteral)
    : (stringnode-append $1 (stringtok->ast $2))
)

;***** to handle identifiers with special meanings:

(KeywordOrOperatorOrAtomLiteral
  (AFTER)
    : (make-node 'const (token-location $1) (erl-atom<-string "after"))
  (ALL_TRUE)
    : (make-node 'const (token-location $1) (erl-atom<-string "all_true"))
  (AND)
    : (make-node 'const (token-location $1) (erl-atom<-string "and"))
  (BAND)
    : (make-node 'const (token-location $1) (erl-atom<-string "band"))
  (BEGIN)
    : (make-node 'const (token-location $1) (erl-atom<-string "begin"))
  (BNOT)
    : (make-node 'const (token-location $1) (erl-atom<-string "bnot"))
  (BOR)
    : (make-node 'const (token-location $1) (erl-atom<-string "bor"))
  (BSL)
    : (make-node 'const (token-location $1) (erl-atom<-string "bsl"))
  (BSR)
    : (make-node 'const (token-location $1) (erl-atom<-string "bsr"))
  (BXOR)
    : (make-node 'const (token-location $1) (erl-atom<-string "bxor"))
  (CASE)
    : (make-node 'const (token-location $1) (erl-atom<-string "case"))
  (CATCH)
    : (make-node 'const (token-location $1) (erl-atom<-string "catch"))
  (COND)
    : (make-node 'const (token-location $1) (erl-atom<-string "cond"))
  (DIV)
    : (make-node 'const (token-location $1) (erl-atom<-string "div"))
  (END)
    : (make-node 'const (token-location $1) (erl-atom<-string "end"))
  (FUN)
    : (make-node 'const (token-location $1) (erl-atom<-string "fun"))
  (IF)
    : (make-node 'const (token-location $1) (erl-atom<-string "if"))
  (LET)
    : (make-node 'const (token-location $1) (erl-atom<-string "let"))
  (MOD)
    : (make-node 'const (token-location $1) (erl-atom<-string "mod"))
  (NOT)
    : (make-node 'const (token-location $1) (erl-atom<-string "not"))
  (OF)
    : (make-node 'const (token-location $1) (erl-atom<-string "of"))
  (OR)
    : (make-node 'const (token-location $1) (erl-atom<-string "or"))
  (RECEIVE)
    : (make-node 'const (token-location $1) (erl-atom<-string "receive"))
  (REM)
    : (make-node 'const (token-location $1) (erl-atom<-string "rem"))
  (SOME_TRUE)
    : (make-node 'const (token-location $1) (erl-atom<-string "some_true"))
  (TRY)
    : (make-node 'const (token-location $1) (erl-atom<-string "try"))
  (WHEN)
    : (make-node 'const (token-location $1) (erl-atom<-string "when"))
  (XOR)
    : (make-node 'const (token-location $1) (erl-atom<-string "xor"))
  (AtomLiteral)
    : $1
)

(AtomLiteral
  (NotQuotedAtomLiteral)
    : (tok->constnode $1)
  (QuotedAtomLiteral)
    : (tok->constnode $1)
  (MODULE)
    : (make-node 'const (token-location $1) (erl-atom<-string "module"))
  (EXPORT)
    : (make-node 'const (token-location $1) (erl-atom<-string "export"))
  (IMPORT)
    : (make-node 'const (token-location $1) (erl-atom<-string "import"))
  (COMPILE)
    : (make-node 'const (token-location $1) (erl-atom<-string "compile"))
  (RECORD)
    : (make-node 'const (token-location $1) (erl-atom<-string "record"))
)

(WildAtomLiteral
  (NotQuotedAtomLiteral)
    : (tok->constnode $1)
  (QuotedAtomLiteral)
    : (tok->constnode $1)
)

(IntegerLiteral
  (UnsignedDecimalLiteral)
    : (tok->constnode $1)
  (NotUnsignedDecimalIntegerLiteral)
    : (tok->constnode $1)
)

;***** not defined in Standard Erlang specification.

(Term
  (AtomicLiteral)
    : (force-term-const $1)
  (ListTerm)
    : $1
  (TupleTerm)
    : $1
)

(ListTerm
  (BRACK-OPEN BRACK-CLOSE)
    : (make-node 'const
                 (generic-location-join $1 $2)
                 (erl-nil))
  (BRACK-OPEN Terms ListTermTail_opt BRACK-CLOSE)
    : (make-node 'const
                 (generic-location-join $1 $4)
                 (erl-append (erl-list<-list (map node-val1 $2)) $3))
)

(ListTermTail_opt
  ()
    : (erl-nil)
  (ListTermTail)
    : $1
)

(ListTermTail
  (BAR Term)
    : (node-val1 $2)
)

(TupleTerm
  (BRACE-OPEN Terms_opt BRACE-CLOSE)
    : (make-node 'const
                 (generic-location-join $1 $3)
                 (erl-tuple<-list (map node-val1 $2)))
)

(Terms_opt
  ()
    : '()
  (Terms)
    : (reverse $1)
)

(Terms
  (Term)
    : (list $1)
  (Terms COMMA Term)
    : (cons $3 $1)
)

))
