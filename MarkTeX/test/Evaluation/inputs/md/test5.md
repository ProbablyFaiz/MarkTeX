# A header

Some [formatting *inside* text]("http://google.com") and **bold??? please**

{% tFor "x" ([1, 2, 3, 10] :: [Int]) %}
- Can I do one more line? This *one* has *too*. say {{ tInsert (get "x")}} times whuut
{% end %}

Let's render an image: ![]("./image.png")

{{ SetVar "m" 0 }}

{% tWhile (get "m" < 5) %}

{{ tInsert (get "m") }}
{{ SetVar "m" (get "m" + 1) }}

{% end %}

$$ a^2 = b^2 + c^2 $$

It's the lists:
- Test
- Second item. Formatting *on* lists should work now.
- Another item
- Last item

1. Ordered list.
