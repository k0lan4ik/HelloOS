SourceFileName equ 'ega.cpi'
FontIndex = 15

 format binary as '7.fnt'

virtual at 0
SourceFile::
  file SourceFileName
end virtual

load ofsFontInfo dword from SourceFile:19
load nCodePages word from SourceFile:ofsFontInfo

iFont = 0
ofsCPHeader = ofsFontInfo + 2
repeat nCodePages
  load nFonts word from SourceFile:ofsCPHeader + 28 + 2
  ofsFontHeader = ofsCPHeader + 28 + 6
  repeat nFonts
    load h byte from SourceFile:ofsFontHeader
    load w byte from SourceFile:ofsFontHeader + 1
    load nChars word from SourceFile:ofsFontHeader + 4

    display '0' + w, 'x', '0' + (h / 10), '0' + h mod 10, 13, 10
    if w = 8 & h = 16 & iFont = FontIndex
      repeat nChars * h * ((w + 7) / 8)
        load x byte from SourceFile:ofsFontHeader + 6 + % - 1
        db x
      end repeat

    end if
    iFont = iFont + 1
    ofsFontHeader = ofsFontHeader + 6 + nChars * h * ((w + 7) / 8)
  end repeat
  load ofsCPHeader dword from SourceFile:ofsCPHeader + 2
end repeat