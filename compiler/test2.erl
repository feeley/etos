% file: "test2.erl"
%
% This file contains errors which are detected by the parser.

-ifdef(xx).

-include("foo.hrl").

-ifdef(yy).
-endif.

-else.

-include("bar.hrl").

-ifdef(zz).
-endif.

%zz.

-endif.

-define.
