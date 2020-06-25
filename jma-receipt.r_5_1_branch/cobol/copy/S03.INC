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
        02  S03.
              03  S03-B03-LABEL    PIC X(10).
              03  S03-B05-LABEL    PIC X(10).
              03  S03-B12-STATE PIC S9(9)   BINARY.
              03  S03-B12S-STATE PIC S9(9)  BINARY.
              03  S03-LBLGEMNEN-G.
                04  S03-LBLGENMEN-STYLE
                                         PIC X(20).
                04  S03-LBLGENMEN       PIC X(30).
              03  S03-PTNUM     PIC X(20).
              03  S03-SRYYMD    PIC X(9).
              03  S03-KANANAME  PIC X(50).
              03  S03-NAME      PIC X(50).
              03  S03-SEX       PIC X(4).
              03  S03-BIRTHDAY  PIC X(9).
              03  S03-HKNCOMBI  PIC X(50).
              03  S03-RATE      PIC X(6).
              03  S03-SRYKA     PIC X(10).
              03  S03-NYUGAIKBN PIC X(6).
              03  S03-NAIYO-COMBO.
                04  S03-NAIYO PIC X(12).
                04  S03-NAIYO-STATE   PIC S9(9)   BINARY.
                04  S03-NAIYO-COUNT   PIC S9(9)   BINARY.
                04  S03-NAIYOLST      PIC X(12) OCCURS 5 TIMES.
                04  S03-NAIYOLST-STATE   PIC S9(9)   BINARY.
              03  S03-DENPNUM   PIC X(10).
              03  S03-DENPJTI   PIC X(10).
              03  S03-HAKYMD    PIC X(9).
              03  S03-SSUHKNTEN PIC S9(10).
              03  S03-SDOHKNTEN PIC S9(10).
              03  S03-ZTKHKNTEN PIC S9(10).
              03  S03-TYKHKNTEN PIC S9(10).
              03  S03-CSYHKNTEN PIC S9(10).
              03  S03-SYCHKNTEN PIC S9(10).
              03  S03-SJTHKNTEN PIC S9(10).
              03  S03-MSIHKNTEN PIC S9(10).
              03  S03-KNSHKNTEN PIC S9(10).
              03  S03-GZUHKNTEN PIC S9(10).
              03  S03-ETCHKNTEN PIC S9(10).
              03  S03-SSNHKNTEN PIC S9(10).
              03  S03-HOUHKNTEN PIC S9(10).
              03  S03-BYRHKNTEN PIC S9(10).
              03  S03-NYUHKNTEN PIC S9(10).
              03  S03-RYOHKNTEN PIC S9(10).
              03  S03-TOTALHKNTEN
                                PIC S9(10).
              03  S03-SSUTGMONEY
                                PIC S9(10).
              03  S03-SDOTGMONEY
                                PIC S9(10).
              03  S03-ZTKTGMONEY
                                PIC S9(10).
              03  S03-TYKTGMONEY
                                PIC S9(10).
              03  S03-CSYTGMONEY
                                PIC S9(10).
              03  S03-SYCTGMONEY
                                PIC S9(10).
              03  S03-SJTGMONEY PIC S9(10).
              03  S03-MSITGMONEY PIC S9(10).
              03  S03-KNSTGMONEY
                                PIC S9(10).
              03  S03-GZUTGMONEY
                                PIC S9(10).
              03  S03-ETCTGMONEY
                                PIC S9(10).
              03  S03-SSNTGMONEY PIC S9(10).
              03  S03-HOUTGMONEY PIC S9(10).
              03  S03-BYRTGMONEY PIC S9(10).
              03  S03-NYUTGMONEY
                                PIC S9(10).
              03  S03-RYOTGMONEY PIC S9(10).
              03  S03-HKNTEKMONEY
                                PIC S9(10).
              03  S03-HKNTEKGAI PIC S9(10).
              03  S03-JIHIMSG-G OCCURS 10.
                04  S03-JIHIMSG PIC X(12).
              03  S03-JIHI1     PIC S9(10).
              03  S03-JIHI2     PIC S9(10).
              03  S03-JIHI3     PIC S9(10).
              03  S03-JIHI4     PIC S9(10).
              03  S03-JIHI5     PIC S9(10).
              03  S03-JIHI6     PIC S9(10).
              03  S03-JIHI7     PIC S9(10).
              03  S03-JIHI8     PIC S9(10).
              03  S03-JIHI9     PIC S9(10).
              03  S03-JIHI10    PIC S9(10).
              03  S03-JIHITOTAL PIC S9(10).
              03  S03-JIHI1TAX  PIC S9(10).
              03  S03-JIHI2TAX  PIC S9(10).
              03  S03-JIHI3TAX  PIC S9(10).
              03  S03-JIHI4TAX  PIC S9(10).
              03  S03-JIHI5TAX  PIC S9(10).
              03  S03-JIHI6TAX  PIC S9(10).
              03  S03-JIHI7TAX  PIC S9(10).
              03  S03-JIHI8TAX  PIC S9(10).
              03  S03-JIHI9TAX  PIC S9(10).
              03  S03-JIHI10TAX PIC S9(10).
              03  S03-JIHITAXTOTAL
                                 PIC S9(10).
              03  S03-JIHITAX   PIC S9(10).
      *
              03  S03-FR2-VISIBLE  PIC X(01).
              03  S03-S03N-SKJRYOYO  PIC S9(10).
              03  S03-S03N-LIFERYOYO PIC S9(10).
              03  S03-S03N-SKJFTN    PIC S9(10).
              03  S03-S03N-LIFEFTN   PIC S9(10).
              03  S03-S03N-RJNFTN    PIC S9(10).
              03  S03-S03N-KOHFTN    PIC S9(10).
              03  S03-S03N-FTNTOTAL  PIC S9(10).
              03  S03-S03N-RSISHOSHIN
                                PIC S9(10).
              03  S03-S03N-RSISAISHIN
                                PIC S9(10).
              03  S03-S03N-RSISDO    PIC S9(10).
              03  S03-S03N-RSIETC    PIC S9(10).
              03  S03-S03N-RMSAGAKU  PIC S9(10).
              03  S03-S03N-CHOSEI1   PIC S9(10).
              03  S03-S03N-CHOSEI2   PIC S9(10).
              03  S03-S03N-SKYMONEY  PIC S9(10).
              03  S03-S03N-NYUKINGK1 PIC S9(10).
              03  S03-S03N-MISYUZAN  PIC S9(10).
              03  S03-S03N-ZENMISYU-GAI  PIC S9(10).
              03  S03-S03N-ZENMISYU-NYU  PIC S9(10).
              03  S03-S03N-GOKMISYU  PIC S9(10).
      *
              03  S03-FR3-VISIBLE    PIC X(01).
              03  S03-S03G-YKZFTN    PIC S9(10).
              03  S03-S03G-RJNFTN    PIC S9(10).
              03  S03-S03G-KOHFTN    PIC S9(10).
              03  S03-S03G-FTNTOTAL  PIC S9(10).
              03  S03-S03G-RSISHOSHIN
                                PIC S9(10).
              03  S03-S03G-RSISAISHIN
                                PIC S9(10).
              03  S03-S03G-RSISDO    PIC S9(10).
              03  S03-S03G-RSIETC    PIC S9(10).
              03  S03-S03G-CHOSEI1   PIC S9(10).
              03  S03-S03G-CHOSEI2   PIC S9(10).
              03  S03-S03G-SKYMONEY  PIC S9(10).
              03  S03-S03G-NYUKINGK1 PIC S9(10).
              03  S03-S03G-MISYUZAN  PIC S9(10).
              03  S03-S03G-ZENMISYU  PIC S9(10).
              03  S03-S03G-GOKMISYU  PIC S9(10).
      *
              03  S03-CHOSEI1IN  PIC X(10).
              03  S03-CHOSEI1IN-STATE
                                 PIC S9(9)   BINARY.
              03  S03-CHOSEI2IN  PIC X(10).
              03  S03-CHOSEI2IN-STATE
                                 PIC S9(9)   BINARY.
              03  S03-SKYMONEYIN-STYLE  PIC X(20).
              03  S03-SKYMONEYIN  PIC X(10).
              03  S03-SKYMONEYIN-STATE
                                 PIC S9(9)   BINARY.
              03  S03-NYUKINGK2 PIC X(10).
              03  S03-NYUKINGK2-STATE
                                 PIC S9(9)   BINARY.
              03  S03-NYUKINYMD.
                04  S03-VALUE   PIC X(10).
              03  S03-NYUKINYMD-STATE
                                 PIC S9(9)   BINARY.
              03  S03-NYUKINHOUHOU-COMBO.
                04  S03-NYUKINHOUHOU PIC X(33).
                04  S03-NYUKINHOUHOU-STATE
                                 PIC S9(9)   BINARY.
                04  S03-NYUKINHOUHOU-COUNT   PIC S9(9)   BINARY.
                04  S03-NYUKINHOUHOULST      PIC X(33) OCCURS 50 TIMES.
                04  S03-NYUKINHOUHOULST-STATE
                                 PIC S9(9)   BINARY.
              03  S03-SKYPRT-COMBO.
                04  S03-SKYPRT PIC X(24).
                04  S03-SKYPRT-STATE   PIC S9(9)   BINARY.
                04  S03-SKYPRT-COUNT   PIC S9(9)   BINARY.
                04  S03-SKYPRTLST      PIC X(24) OCCURS 5 TIMES.
                04  S03-SKYPRTLST-STATE   PIC S9(9)   BINARY.
              03  S03-SRYPRT-COMBO.
                04  S03-SRYPRT PIC X(24).
                04  S03-SRYPRT-STATE   PIC S9(9)   BINARY.
                04  S03-SRYPRT-COUNT   PIC S9(9)   BINARY.
                04  S03-SRYPRTLST      PIC X(24) OCCURS 5 TIMES.
                04  S03-SRYPRTLST-STATE   PIC S9(9)   BINARY.
              03  S03-RIREKIIN PIC S9(2).
              03  S03-SCROLLEDWINDOW1.
                04  S03-CLIST1.
                  05  S03-CLIST1-COUNT       PIC S9(9)   BINARY.
                  05  S03-CLIST1-ROW         PIC S9(9)   BINARY.
                  05  S03-CLIST1-ROWATTR     PIC S9(9)   BINARY.
                  05  S03-ITEM        OCCURS  99 TIMES.
                      06  S03-TRENNUM PIC X(4).
                      06  S03-TSKYMONY    PIC X(12).
                      06  S03-TNYUKINMONEY    PIC X(12).
                      06  S03-TSYORIYMD   PIC X(11).
                      06  S03-TJTI    PIC X(12).
                      06  S03-TNYKN-NAME    PIC X(10).
                  05  S03-SELECT  PIC X       OCCURS  99 TIMES.
