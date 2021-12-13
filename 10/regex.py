import re

r = "ab."
txt = "abcabdab"

print(re.search(r,txt).group())

