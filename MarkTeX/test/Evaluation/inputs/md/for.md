{%For "x" (get "list1to3")%}*{{InsertVar "x"}}*{%end%}
{%For "x" (get "list1to3")%}{%tIf ((get "x") > 1)%}**{{InsertVar "x"}}**{%end%}{%end%}