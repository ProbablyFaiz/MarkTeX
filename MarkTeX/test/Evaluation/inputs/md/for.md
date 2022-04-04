{%for "x" (get "list1to3")%}*{{InsertVar "x"}}*{%end%}
{%for "x" (get "list1to3")%}{%ifTrue ((get "x") > 1)%}**{{InsertVar "x"}}**{%end%}{%end%}