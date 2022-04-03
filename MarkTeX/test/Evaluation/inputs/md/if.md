{% If (get "trueVar") %}
**Show**
{% end %}
{% If (get "falseVar") %}
**NotShow**
{% end %}

{% IfVar "trueVar" %}
**ShowVar**
{% end %}
{% IfVar "falseVar" %}
**NotShowVar**
{% end %}