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
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
        02  O04.
              03  O04-B05-LABEL PIC X(10).
              03  O04-B06-STATE PIC S9(9)   BINARY.
              03  O04-B07-STATE PIC S9(9)   BINARY.
              03  O04-LBLGEMNEN-G.
                04  O04-LBLGENMEN-STYLE
                                         PIC X(20).
                04  O04-LBLGENMEN        PIC X(30).
              03  O04-PTNUM     PIC X(20).
              03  O04-SRYYM     PIC X(15).
              03  O04-KANANAME  PIC X(50).
              03  O04-NAME      PIC X(50).
              03  O04-SEX       PIC X(4).
              03  O04-BIRTHDAY  PIC X(9).
              03  O04-HKNCOMBI  PIC X(50).
              03  O04-RATE      PIC X(6).
              03  O04-SRYKA     PIC X(10).
              03  O04-NAIYO     PIC X(10).
              03  O04-DENPNUM   PIC X(10).
              03  O04-DENPJTI   PIC X(10).
              03  O04-HAKYMD    PIC X(9).
              03  O04-SSUHKNTEN PIC S9(8).
              03  O04-SDOHKNTEN PIC S9(8).
              03  O04-ZTKHKNTEN PIC S9(8).
              03  O04-TYKHKNTEN PIC S9(8).
              03  O04-CSYHKNTEN PIC S9(8).
              03  O04-SYCHKNTEN PIC S9(8).
              03  O04-SJTHKNTEN PIC S9(8).
              03  O04-MSIHKNTEN PIC S9(8).
              03  O04-KNSHKNTEN PIC S9(8).
              03  O04-GZUHKNTEN PIC S9(8).
              03  O04-ETCHKNTEN PIC S9(8).
              03  O04-SSNHKNTEN PIC S9(8).
              03  O04-HOUHKNTEN PIC S9(8).
              03  O04-BYRHKNTEN PIC S9(8).
              03  O04-NYUHKNTEN PIC S9(8).
              03  O04-RYOHKNTEN PIC S9(8).
              03  O04-TOTALHKNTEN
                                PIC S9(8).
              03  O04-SSUTGMONEY
                                PIC S9(8).
              03  O04-SDOTGMONEY
                                PIC S9(8).
              03  O04-ZTKTGMONEY
                                PIC S9(8).
              03  O04-TYKTGMONEY
                                PIC S9(8).
              03  O04-CSYTGMONEY
                                PIC S9(8).
              03  O04-SYCTGMONEY
                                PIC S9(8).
              03  O04-SJTGMONEY PIC S9(8).
              03  O04-MSITGMONEY PIC S9(8).
              03  O04-KNSTGMONEY
                                PIC S9(8).
              03  O04-GZUTGMONEY
                                PIC S9(8).
              03  O04-ETCTGMONEY
                                PIC S9(8).
              03  O04-SSNTGMONEY PIC S9(8).
              03  O04-HOUTGMONEY PIC S9(8).
              03  O04-BYRTGMONEY PIC S9(8).
              03  O04-NYUTGMONEY
                                PIC S9(8).
              03  O04-RYOTGMONEY PIC S9(8).
              03  O04-HKNTEKMONEY
                                PIC S9(8).
              03  O04-HKNTEKGAI PIC S9(8).
              03  O04-JIHIMSG-G OCCURS  10.
                04  O04-JIHIMSG PIC X(14).
              03  O04-JIHI1     PIC S9(8).
              03  O04-JIHI2     PIC S9(8).
              03  O04-JIHI3     PIC S9(8).
              03  O04-JIHI4     PIC S9(8).
              03  O04-JIHI5     PIC S9(8).
              03  O04-JIHI6     PIC S9(8).
              03  O04-JIHI7     PIC S9(8).
              03  O04-JIHI8     PIC S9(8).
              03  O04-JIHI9     PIC S9(8).
              03  O04-JIHI10    PIC S9(8).
              03  O04-JIHITOTAL PIC S9(8).
              03  O04-JIHI1TAX  PIC S9(8).
              03  O04-JIHI2TAX  PIC S9(8).
              03  O04-JIHI3TAX  PIC S9(8).
              03  O04-JIHI4TAX  PIC S9(8).
              03  O04-JIHI5TAX  PIC S9(8).
              03  O04-JIHI6TAX  PIC S9(8).
              03  O04-JIHI7TAX  PIC S9(8).
              03  O04-JIHI8TAX  PIC S9(8).
              03  O04-JIHI9TAX  PIC S9(8).
              03  O04-JIHI10TAX PIC S9(8).
              03  O04-JIHITAXTOTAL
                                PIC S9(8).
              03  O04-JIHITAX   PIC S9(8).
              03  O04-SKJRYOYO  PIC S9(8).
              03  O04-LIFERYOYO PIC S9(8).
              03  O04-SKJFTN    PIC S9(8).
              03  O04-LIFEFTN   PIC S9(8).
              03  O04-RJNFTN    PIC S9(8).
              03  O04-KOHFTN    PIC S9(8).
              03  O04-FTNTOTAL  PIC S9(8).
              03  O04-RSISHOSHIN
                                PIC X(10).
              03  O04-RSISAISHIN
                                PIC X(10).
              03  O04-RSISDO    PIC X(10).
              03  O04-RSIETC    PIC X(10).
              03  O04-RMSAGAKU  PIC S9(8).
              03  O04-CHOSEI1   PIC S9(8).
              03  O04-CHOSEI2   PIC S9(8).
              03  O04-SYKMONEY  PIC S9(8).
              03  O04-NYUKINGK  PIC S9(8).
              03  O04-MISYUZAN  PIC S9(8).
              03  O04-ZENMISYU-GAI  PIC S9(8).
              03  O04-ZENMISYU-NYU  PIC S9(8).
              03  O04-GOKMISYU  PIC S9(8).
              03  O04-BTNCHOSEI1-STATE PIC S9(9)   BINARY.
              03  O04-BTNCHOSEI2-STATE PIC S9(9)   BINARY.
              03  O04-CHOSEI1IN-G.
                04  O04-CHOSEI1IN       PIC X(10).
                04  O04-CHOSEI1IN-STATE PIC S9(9)   BINARY.
              03  O04-CHOSEI2IN-G.
                04  O04-CHOSEI2IN       PIC X(10).
                04  O04-CHOSEI2IN-STATE PIC S9(9)   BINARY.
              03  O04-SKYMONEYIN-G.
                04  O04-SKYMONEYIN-STYLE 
                                        PIC X(20).
                04  O04-SKYMONEYIN      PIC X(10).
                04  O04-SKYMONEYIN-STATE PIC S9(9)   BINARY.
              03  O04-KINGAKU   PIC X(10).
              03  O04-NYUKINYMD.
                04  O04-VALUE   PIC X(10).
              03  O04-CMB-NYUKINHOUHOU.
                04  O04-NYUKINHOUHOU PIC X(33).
                04  O04-NYUKINHOUHOU-STATE
                                 PIC S9(9)   BINARY.
                04  O04-NYUKINHOUHOU-COUNT   PIC S9(9)   BINARY.
                04  O04-NYUKINHOUHOULST      PIC X(33) OCCURS 50 TIMES.
                04  O04-NYUKINHOUHOULST-STATE
                                 PIC S9(9)   BINARY.
              03  O04-CMB-SKYPRT.
                04  O04-SKYPRT PIC X(24).
                04  O04-SKYPRT-COUNT   PIC S9(9)   BINARY.
                04  O04-SKYPRTLST      PIC X(24) OCCURS 5 TIMES.
              03  O04-CMB-SRYPRT.
                04  O04-SRYPRT PIC X(24).
                04  O04-SRYPRT-COUNT   PIC S9(9)   BINARY.
                04  O04-SRYPRTLST      PIC X(24) OCCURS 5 TIMES.
              03  O04-RIREKIIN PIC S9(2).
              03  O04-SCROLLEDWINDOW1.
                04  O04-CLIST1.
                  05  O04-CLIST1-COUNT       PIC S9(9)   BINARY.
                  05  O04-CLIST1-ROW         PIC S9(9)   BINARY.
                  05  O04-CLIST1-ROWATTR     PIC S9(9)   BINARY.
                  05  O04-ITEM        OCCURS  99 TIMES.
                      06  O04-TRENNUM PIC X(4).
                      06  O04-TSKYMONY    PIC X(13).
                      06  O04-TNYUKINMONEY    PIC X(12).
                      06  O04-TSYORIYMD   PIC X(11).
                      06  O04-TJTI    PIC X(12).
                      06  O04-TNYKN-NAME    PIC X(10).
                  05  O04-SELECT  PIC X       OCCURS  99 TIMES.
              03  O04-LBLSKY1ST   PIC X(4).
              03  O04-SKY1ST      PIC X(10).
              03  O04-LBLSKY2ND   PIC X(4).
              03  O04-SKY2ND      PIC X(10).
              03  O04-LBLSKY3RD   PIC X(4).
              03  O04-SKY3RD      PIC X(10).
              03  O04-LBLSKYSGK-STYLE
                                  PIC X(20).
              03  O04-LBLSKYSGK   PIC X(4).
              03  O04-SKYSGK-STYLE
                                  PIC X(20).
              03  O04-SKYSGK      PIC X(10).
              03  O04-SKYSELNUM      PIC S9(2).
              03  O04-SCROLLEDWINDOW2.
                04  O04-SKYLIST.
                  05  O04-SKYLIST-COUNT    PIC S9(9)   BINARY.
                  05  O04-SKYLIST-ROW      PIC S9(9)   BINARY.
                  05  O04-SKYLIST-ROWATTR  PIC S9(9)   BINARY.
                  05  O04-SKYLIST-ITEM     OCCURS  50 TIMES.
                      06  O04-TSK-RENNUM   PIC X(2).
                      06  O04-TSK-SKYKKN   PIC X(10).
                      06  O04-TSK-SRYKA    PIC X(2).
                      06  O04-TSK-HKNCOMBI PIC X(4).
                  05  O04-SKYLIST-SELECT   PIC X
                                           OCCURS  50 TIMES.
