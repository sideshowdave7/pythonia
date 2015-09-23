# tests line join indentation
'start'
             \
          \
       \
                     \
    "this is fine, the indent is the whitespace before the first line join, but the next statement should result in a TabError"
    'this should be preceded by a TabError' 

