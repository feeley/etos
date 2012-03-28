% file: "test1.erl"
%
% Test handling of scanning errors and parsing errors.

% scanning errors:

   \ux                  % invalid Unicode escape
   $\^!                 % invalid control escape (and unexpected token)
   $\!                  % invalid escape sequence
   $                  % invalid character literal
   $                    % invalid character literal
   "start               % string literal spans more than one line
   'start               % atom literal spans more than one line
   "x"                % string literal contains a control character
   'x'                % atom literal contains a control character
                      % invalid character
   .

% preprocessing and parsing errors:

-define.                % opening parenthesis expected
-define(.               % macro name expected
-define(0.              % macro name expected
-define(if.             % macro name expected
-define('if'.           % comma expected
-define(MACRO0.         % comma expected
-define(MACRO0,.        % closing parenthesis expected
-define(MACRO0,123.     % closing parenthesis expected

-define(0,0).           % macro name expected
-define('if',0).
-define(if,0).          % macro name expected
-define(MODULE,0).
-define(FILE,0).        % macro previously defined
-define(LINE,0).
-define(MACRO0,f(1,2)).
-define(MACRO0,0).      % macro previously defined
-define(MACRO1(.        % variable expected
-define(MACRO1(0.       % variable expected
-define(MACRO1(x.       % variable expected
-define(MACRO1(X.       % closing parenthesis expected
-define(MACRO1(X).      % comma expected
-define(MACRO1(X),.     % closing parenthesis expected
-define(MACRO1(X),f(1)).
-define(MACRO2(x.       % variable expected
-define(MACRO2(X.       % closing parenthesis expected
-define(MACRO2(X,.      % variable expected
-define(MACRO2(X,y.     % variable expected
-define(MACRO2(X,Y.     % closing parenthesis expected
-define(MACRO2(X,Y).    % comma expected
-define(MACRO2(X,Y),.   % closing parenthesis expected
-define(MACRO2(X,Y),X "hi" Y X).
-define(M,0) foo.       % closing parenthesis expected

f() -> 0.               % module attribute expected

-module(if).            % unexpected token
-module(foobar).        % ill-placed module attribute (and module name and file name must match)

-import.                % unexpected token
-import(.               % unexpected token
-import(0.              % unexpected token
-import(if.             % unexpected token
-import('if'.           % unexpected token
-import(foo,.           % unexpected token
-import(foo,[.          % unexpected token
-import(foo,[].         % unexpected token
-import(foo,[]).
-import(foo,[f/1,g/1,size/0]).
-import(bar,[g/1,g/2]).    % can't change previous import of foo:g/2
-import(foo,[f/1,size/1]). % can't shadow implicit import of erlang:size/1

-export.                % unexpected token
-export(.               % unexpected token
-export([.              % unexpected token
-export([].             % unexpected token
-export([]).
-export([m1/0,m1/1]).
-export([m1/0]).

-record.                % unexpected token
-record(.               % unexpected token
-record(0.              % unexpected token
-record(if.             % unexpected token
-record('if'.           % unexpected token
-record(foo,.           % unexpected token
-record(foo,{.          % unexpected token
-record(foo,{}.         % unexpected token
-record(foo,{}).
-record(foo,{x}).       % record type already defined
-record(bar,{x=0}).
-record(baz,{x,y=X,x}). % duplicate record field name

r1(#bar{x=0}) -> 0.
r2(#bar{z=0}) -> 0.     % undefined record field
r3(#baz{x=0}) -> 0.     % undefined record type
r4(X) -> X#bar.x.
r5(X) -> X#bar.z.       % undefined record field
r6(X) -> X#baz.x.       % undefined record type
r7(X) -> X#bar{x=0}.
r8(X) -> X#bar{z=0}.    % undefined record field
r9(X) -> X#baz{x=0}.    % undefined record type

m1() -> ?.              % macro name expected (and unexpected token)
m2() -> ?0.             % macro name expected (and unexpected token)
m3() -> ?if.            % macro name expected (and unexpected token)
m3() -> ?'if'.
m4() -> ?MODULE.
m5() -> ?FILE.
m6() -> ?LINE.
m7() -> ?MACRO0.
m8() -> ?MACRO0().      % unexpected token
m9() -> ?MACRO0(1).     % unexpected token
m10() -> ?MACRO1.       % opening parenthesis expected (and unexpected token)
m11() -> ?MACRO1().
m12() -> ?MACRO1(1).
m13() -> ?MACRO1([1,2). % unbalanced macro argument (and unexpected token)
m14() -> ?MACRO1(1,2).  % wrong number of arguments (and unexpected token)
m15() -> ?MACRO2.       % opening parenthesis expected (and unexpected token)
m16() -> ?MACRO2("").   % wrong number of arguments (and unexpected token)
m17() -> ?MACRO2("a","b").
m18() -> ?MACRO2(,,).   % wrong number of arguments (and unexpected token)
m19() -> ?MACRO2(,).
m20() -> ?undefined.    % macro is not defined (and unexpected token)

-else.                  % unexpected "else" preprocessor directive

-endif.                 % unexpected "endif" preprocessor directive
-endif foo.             % full stop expected

-undef.                 % opening parenthesis expected
-undef(.                % macro name expected
-undef(0.               % macro name expected
-undef(if.              % macro name expected
-undef('if'.            % closing parenthesis expected
-undef(MODULE).
-undef(FILE).
-undef(LINE).
-undef(MACRO1).
-undef(MACRO1).
-undef(MACRO0) foo.     % full stop expected

-file.                  % opening parenthesis expected
-file(.                 % string literal expected
-file(0.                % string literal expected
-file("foo".            % comma expected
-file("foo",.           % unsigned decimal literal expected
-file("foo","bar".      % unsigned decimal literal expected
-file("foo",123).
-file("foo",1_2_3).
-file("foo",123) foo.   % full stop expected

-include.               % opening parenthesis expected
-include(.              % string literal expected
-include(0.             % string literal expected
-include("foo".         % closing parenthesis expected
-include("foo").        % file "foo" not found
-include("foo") bar.    % full stop expected

-include_lib.           % opening parenthesis expected
-include_lib(.          % string literal expected
-include_lib(0.         % string literal expected
-include_lib("foo".     % closing parenthesis expected
-include_lib("foo").    % file "foo" not found
-include_lib("foo") bar.% full stop expected

-if(LINE>100).          % "if" preprocessor directive is reserved

-elif(LINE>100).        % "elif" preprocessor directive is reserved

-ifdef.                 % opening parenthesis expected
-ifdef(.                % macro name expected
-ifdef(0.               % macro name expected
-ifdef(if.              % macro name expected
-ifdef('if'.            % closing parenthesis expected
-ifdef(MACRO0).

-ifndef.                % opening parenthesis expected
-ifndef(.               % macro name expected
-ifndef(0.              % macro name expected
-ifndef(if.             % macro name expected
-ifndef('if'.           % closing parenthesis expected
-ifndef(MACRO1).

f() -> ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))). % parsing stack overflow

-else.
-else.                  % unexpected "else" preprocessor directive
-else 0.                % full stop expected

last(X) -> 0            % token sequence is not terminated with full stop
