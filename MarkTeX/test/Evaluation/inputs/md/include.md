Include file below without env:
Val={{Include "included.md"}}
Include file below with env:
Val={{IncludeWith "included.md" (M.fromList [("Val", TNumber 1337)])}}