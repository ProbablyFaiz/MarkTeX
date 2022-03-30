{{ InsertVar "devices[0].name" }}
{{ InsertVar "a.b.c[1][0].d[0][0]" }}

{% tFor "device" (get "devices") %}
- {{ InsertVar "device.name" }} is a thing
{% end %}