{{set "x" (1 :: Int)}}
{% while (get "x" <= 3) %}  *{{ InsertVar "x" }}*  {{ set "x" (get "x" + 1) }}  {% end %}