      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
        02  K08.
      **    02  K08-FIXED1.
              03  K08-PTNUM.
                04  K08-VALUE   PIC X(20).
              03  K08-KANANAME.
                04  K08-VALUE   PIC X(50).
              03  K08-NAME.
                04  K08-VALUE   PIC X(50).
              03  K08-SEX.
                04  K08-VALUE   PIC X(2).
              03  K08-HKNKUMI.
                04  K08-VALUE   PIC X(60).
              03  K08-RATE.
                04  K08-VALUE   PIC X(6).
              03  K08-LASTYMD.
                04  K08-VALUE   PIC X(9).
              03  K08-SYOSINYMD.
                04  K08-VALUE   PIC X(9).
              03  K08-MISYUMONEY.
                04  K08-VALUE   PIC X(15).
              03  K08-SRYKA.
                04  K08-VALUE   PIC X(10).
              03  K08-SRYYMD.
                04  K08-VALUE   PIC X(14).
              03  K08-GOKEITEN.
                04  K08-VALUE   PIC X(15).
              03  K08-ZAITENTOTAL.
                04  K08-VALUE   PIC X(16).
              03  K08-BIRTHDAY.
                04  K08-VALUE   PIC X(9).
              03  K08-NENREI.
                04  K08-VALUE   PIC X(6).
              03  K08-SYORIMEI.
                04  K08-SYORIMEI-STYLE   PIC X(20).
                04  K08-SYORIMEI-VALUE   PIC X(12).
              03  K08-SSUHKNTEN.
                04  K08-VALUE   PIC X(10).
              03  K08-SDOHKNTEN.
                04  K08-VALUE   PIC X(10).
              03  K08-ZTKHKNTEN.
                04  K08-VALUE   PIC X(10).
              03  K08-TYKHKNTEN.
                04  K08-VALUE   PIC X(10).
              03  K08-CSYHKNTEN.
                04  K08-VALUE   PIC X(10).
              03  K08-SYCHKNTEN.
                04  K08-VALUE   PIC X(10).
              03  K08-SJTHKNTEN.
                04  K08-VALUE   PIC X(10).
              03  K08-KNSHKNTEN.
                04  K08-VALUE   PIC X(10).
              03  K08-GZUHKNTEN.
                04  K08-VALUE   PIC X(10).
              03  K08-ETCHKNTEN.
                04  K08-VALUE   PIC X(10).
              03  K08-SSNHKNTEN.
                04  K08-VALUE   PIC X(10).
              03  K08-HOUHKNTEN.
                04  K08-VALUE   PIC X(10).
              03  K08-NYUHKNTEN.
                04  K08-VALUE   PIC X(10).
              03  K08-MSIHKNTEN.
                04  K08-VALUE   PIC X(10).
              03  K08-RYOHKNTEN.
                04  K08-VALUE   PIC X(10).
              03  K08-BYRHKNTEN.
                04  K08-VALUE   PIC X(10).
              03  K08-SELNUM    PIC S9(3)  OCCURS  10.
              03  K08-SCROLLEDWINDOW1.
                04  K08-MEILIST.
                  05  K08-COUNT   PIC S9(9)   BINARY.
                  05  K08-ROW     PIC S9(9)   BINARY.
                  05  K08-ROWATTR PIC S9(9)   BINARY.
                  05  K08-ITEM        OCCURS  800 TIMES.
                    06  K08-BANGO  PIC X(4).
                    06  K08-DELFLG  PIC X(2).
                    06  K08-MEISAI  PIC X(150).
                  05  K08-SELECT  PIC X       OCCURS  800 TIMES.
                  05  K08-MEI-FGCOLOR     PIC X(16)   OCCURS  800 TIMES.
                  05  K08-MEI-BGCOLOR     PIC X(16)   OCCURS  800 TIMES.
                  05  K08-MEI-STYLE   PIC X(20).
              03  K08-MSGDATA.
                04  K08-MSGDATA-STYLE   PIC X(20).
                04  K08-MSGDATA-VALUE   PIC X(60).
              03  K08-SRYMEI15.
                04  K08-VALUE   PIC X(8).
              03  K08-SRYMEI14.
                04  K08-VALUE   PIC X(8).
              03  K08-B10.
                04  K08-B10-STATE   PIC S9(9)   BINARY.
                04  K08-B10-LABEL   PIC X(10).
              03  K08-USERPG-COMB.
                04  K08-USERPG.
                  05  K08-VALUE   PIC X(20).
                04  K08-USERPG-COUNT   PIC S9(9)   BINARY.
                04  K08-USERPG-ITEM
                        PIC X(20)   OCCURS  2 TIMES.
