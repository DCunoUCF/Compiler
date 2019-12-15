import sys

for line in sys.stdin:
  s = line.strip().split(' ');
  name = s[0]
  args = ""
  init = ""
  delim = ""
  for i in range(1, len(s)):
    args = args + delim + s[i]
    init = init + ", .arg%d = (%s)" % (i, s[i])
    delim = ", "
  macro = """#define VM_%s(%s) (({ Instruction ins = { .op = OP_%s%s }; ins; })) """ % (name.upper(), args, name.upper(), init)
  print(macro)
