-author("Sergey Loguntsov").

-compile([{parse_transform, lager_transform}]).

-define(DEBUG(Format, Data), lager:debug(Format, Data)).
-define(INFO(Format, Data), lager:info(Format, Data)).
-define(WARNINGL(Format, Data), lager:warning(Format, Data)).
-define(ERROR(Format, Data), lager:error(Format, Data)).
-define(CRITICAL(Format, Data), lager:critical(Format, Data)).

