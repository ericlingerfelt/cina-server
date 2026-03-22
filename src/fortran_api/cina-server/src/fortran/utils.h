
inline void fromFortran(string &s, const char *cstr, int len)
{
  int i;
  // Get trimmed length of cstr
  for (i = len - 1; i >= 0 && cstr[i] == ' '; i--);
  s.assign(cstr,i+1);
  //cout << i << " '" << s << "'" << endl;
}

// Replace all newlines
inline void toFortran(char *cstr, int len)
{
  int i = 0;
  // Find first NULL
  while (i < len && cstr[i] != '\0') i++;
  //cout << i << endl;
  for (; i < len; i++) cstr[i] = ' ';
}


