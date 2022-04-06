{{ set "filepaths" (["data/2010.json", "data/2011.json"] :: [String]) }}


{% for "filepath" (get "filepaths") %}
{{ Insert $ (get "filepath" ++ "hello") }}
{{ ReadJsonQ (toString (get "filepath")) "yeardata" }}
The costs for this year are {{ insertVar "yeardata.costs" }}
The revenue for this year is {{ insertVar "yeardata.revenue" }}
{% end %}
