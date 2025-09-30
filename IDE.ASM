macro IDE.Write Value*
{
  local value, ofs, digit
  value = Value
  ofs = 15
  repeat 16
    digit = (value shr (ofs * 4)) and $F
    if digit > 9
      display digit + 'A' - 10
    else
      display digit + '0'
    end if
    if % = 8
      display ''''
    end if
    ofs = ofs - 1
  end repeat
}

IDE.Write 42