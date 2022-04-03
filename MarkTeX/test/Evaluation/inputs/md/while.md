{{tSet "x" (1 :: Int)}}
{% tWhile (get "x" <= 3) %}  *{{ InsertVar "x" }}*  {{ tSet "x" (get "x" + 1) }}  {% end %}