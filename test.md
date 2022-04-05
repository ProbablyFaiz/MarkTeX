{{ set "years" (["2010", "2011"] :: [String]) }}

{% for "year" (get "years") %}
{{ set "filepath" (concat ["data/", toString (get "year"), ".json"]) }}
{{ ReadJsonQ (toString (get "filepath")) "yeardata" }}
The costs for year {{ InsertVar "year" }} are {{ InsertVar "yeardata.costs" }}
The revenue for year {{ InsertVar "year" }} is {{ InsertVar "yeardata.revenue" }}
{% ifTrue (get "yeardata.revenue" > 2 * get "yeardata.costs") %}
We made a lot of profit this year
{% end %}
{% end %}
