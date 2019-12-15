/* ANSI-C code produced by gperf version 3.1 */
/* Command-line: gperf -N vm_ops -t vm_ops.gperf  */
/* Computed positions: -k'1-3' */

#if !((' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) \
      && ('%' == 37) && ('&' == 38) && ('\'' == 39) && ('(' == 40) \
      && (')' == 41) && ('*' == 42) && ('+' == 43) && (',' == 44) \
      && ('-' == 45) && ('.' == 46) && ('/' == 47) && ('0' == 48) \
      && ('1' == 49) && ('2' == 50) && ('3' == 51) && ('4' == 52) \
      && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) \
      && ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) \
      && ('=' == 61) && ('>' == 62) && ('?' == 63) && ('A' == 65) \
      && ('B' == 66) && ('C' == 67) && ('D' == 68) && ('E' == 69) \
      && ('F' == 70) && ('G' == 71) && ('H' == 72) && ('I' == 73) \
      && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) \
      && ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) \
      && ('R' == 82) && ('S' == 83) && ('T' == 84) && ('U' == 85) \
      && ('V' == 86) && ('W' == 87) && ('X' == 88) && ('Y' == 89) \
      && ('Z' == 90) && ('[' == 91) && ('\\' == 92) && (']' == 93) \
      && ('^' == 94) && ('_' == 95) && ('a' == 97) && ('b' == 98) \
      && ('c' == 99) && ('d' == 100) && ('e' == 101) && ('f' == 102) \
      && ('g' == 103) && ('h' == 104) && ('i' == 105) && ('j' == 106) \
      && ('k' == 107) && ('l' == 108) && ('m' == 109) && ('n' == 110) \
      && ('o' == 111) && ('p' == 112) && ('q' == 113) && ('r' == 114) \
      && ('s' == 115) && ('t' == 116) && ('u' == 117) && ('v' == 118) \
      && ('w' == 119) && ('x' == 120) && ('y' == 121) && ('z' == 122) \
      && ('{' == 123) && ('|' == 124) && ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gperf@gnu.org>."
#endif

#line 1 "vm_ops.gperf"
struct opcode { char *name; uint8_t num; };

#define TOTAL_KEYWORDS 31
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 4
#define MIN_HASH_VALUE 2
#define MAX_HASH_VALUE 53
/* maximum key range = 52, duplicates = 0 */

#ifdef __GNUC__
__inline
#else
#ifdef __cplusplus
inline
#endif
#endif
static unsigned int
hash (register const char *str, register size_t len)
{
  static unsigned char asso_values[] =
    {
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54,  0,  5, 15,
       0,  5, 54,  6,  8,  0, 54, 54,  0, 15,
       3, 20, 15, 13,  5,  5, 10, 15,  5, 15,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
      54, 54, 54, 54, 54, 54
    };
  register unsigned int hval = len;

  switch (hval)
    {
      default:
        hval += asso_values[(unsigned char)str[2]];
      /*FALLTHROUGH*/
      case 2:
        hval += asso_values[(unsigned char)str[1]];
      /*FALLTHROUGH*/
      case 1:
        hval += asso_values[(unsigned char)str[0]];
        break;
    }
  return hval;
}

struct opcode *
vm_ops (register const char *str, register size_t len)
{
  static struct opcode wordlist[] =
    {
      {""}, {""},
#line 7 "vm_ops.gperf"
      {"ld", OP_LD},
#line 23 "vm_ops.gperf"
      {"add", OP_ADD},
#line 29 "vm_ops.gperf"
      {"addi", OP_ADDI},
      {""}, {""},
#line 20 "vm_ops.gperf"
      {"bl", OP_BL},
#line 26 "vm_ops.gperf"
      {"div", OP_DIV},
#line 32 "vm_ops.gperf"
      {"divi", OP_DIVI},
      {""}, {""},
#line 19 "vm_ops.gperf"
      {"br", OP_BR},
#line 16 "vm_ops.gperf"
      {"ble", OP_BLE},
#line 11 "vm_ops.gperf"
      {"read", OP_READ},
      {""},
#line 14 "vm_ops.gperf"
      {"bne", OP_BNE},
#line 8 "vm_ops.gperf"
      {"st", OP_ST},
#line 15 "vm_ops.gperf"
      {"blt", OP_BLT},
#line 18 "vm_ops.gperf"
      {"bge", OP_BGE},
      {""},
#line 4 "vm_ops.gperf"
      {"hlt", OP_HLT},
#line 12 "vm_ops.gperf"
      {"wr", OP_WR},
#line 21 "vm_ops.gperf"
      {"ret", OP_RET},
#line 17 "vm_ops.gperf"
      {"bgt", OP_BGT},
      {""},
#line 13 "vm_ops.gperf"
      {"beq", OP_BEQ},
      {""},
#line 24 "vm_ops.gperf"
      {"sub", OP_SUB},
#line 30 "vm_ops.gperf"
      {"subi", OP_SUBI},
      {""},
#line 9 "vm_ops.gperf"
      {"psh", OP_PSH},
      {""},
#line 25 "vm_ops.gperf"
      {"mul", OP_MUL},
#line 31 "vm_ops.gperf"
      {"muli", OP_MULI},
      {""}, {""}, {""},
#line 27 "vm_ops.gperf"
      {"mod", OP_MOD},
#line 33 "vm_ops.gperf"
      {"modi", OP_MODI},
      {""},
#line 3 "vm_ops.gperf"
      {"nop", OP_NOP},
      {""},
#line 5 "vm_ops.gperf"
      {"mov", OP_MOV},
#line 6 "vm_ops.gperf"
      {"movi", OP_MOVI},
      {""}, {""}, {""},
#line 22 "vm_ops.gperf"
      {"cmp", OP_CMP},
#line 28 "vm_ops.gperf"
      {"cmpi", OP_CMPI},
      {""}, {""}, {""},
#line 10 "vm_ops.gperf"
      {"pop", OP_POP}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register unsigned int key = hash (str, len);

      if (key <= MAX_HASH_VALUE)
        {
          register const char *s = wordlist[key].name;

          if (*str == *s && !strcmp (str + 1, s + 1))
            return &wordlist[key];
        }
    }
  return 0;
}
