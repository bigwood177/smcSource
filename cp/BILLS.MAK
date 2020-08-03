assign c:\mcba\source\COP\ SRC
assign c:\mcba\source\def\ def
assign c:\mcba\source\temp\ OBJ
assign c:\mcba\execute\ut\ ut
dbl /obj=OBJ:BILLS.TMP SRC:BILLS.COP
dbl /obj=OBJ:BLMNU.TMP/r:debug SRC:BLMNU.COP
dbl /obj=OBJ:ORDBL.TMP SRC:ORDBL.COP
dbl /obj=OBJ:LINBL.TMP/r:debug SRC:LINBL.COP
dbl /obj=OBJ:DUCBL.TMP/r:debug SRC:DUCBL.COP
dbl /obj=OBJ:DFITM.TMP/r:debug SRC:DFITM.COP
dbl /obj=OBJ:OE1.TMP/r:debug SRC:OE1.COP
dbl /obj=OBJ:OE10.TMP/r:debug SRC:OE10.COP
dbl /obj=OBJ:OE3B.TMP/r:debug SRC:OE3B.COP
dbl /obj=OBJ:OE4.TMP/r:debug SRC:OE4.COP
dbl /obj=OBJ:OE7.TMP/r:debug SRC:OE7.COP
dbl /obj=OBJ:DSPDU.TMP/r:debug SRC:DSPDU.COP
dbl /obj=OBJ:GETIM.TMP/r:debug SRC:GETIM.COP

DBLINK @BILLS.SSQ

