Below is a regular import of json data
{{ReadJson "../json/import.json"}}
*{{InsertVar "imported"}}*
Below is a qualified import of json data
{{ReadJsonQ "../json/import.json" "qualified"}}
**{{InsertVar "qualified.imported"}}**