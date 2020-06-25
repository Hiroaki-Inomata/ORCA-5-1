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
         02  U02.
      *** 02  U02-FIXED1.
              03  U02-SRYYMD.
                04  U02-VALUE   PIC X(30).
              03  U02-PTNUM-G.
                04  U02-PTNUM-STATE   PIC S9(9)   BINARY.
                04  U02-PTNUM        PIC X(20).
              03  U02-NAME-G.
                04  U02-NAME-STATE   PIC S9(9)   BINARY.
                04  U02-NAME         PIC X(50).
              03  U02-BIRTHDAY.
                04  U02-VALUE   PIC X(9).
              03  U02-SRYKA-COMBO.
                04  U02-SRYKA.
                  05  U02-VALUE   PIC X(21).
                04  U02-SRYKA-COUNT   PIC S9(9)   BINARY.
                04  U02-SRYKA-ITEM    PIC X(21)   OCCURS  99 TIMES.
              03  U02-DRNAME-COMBO.
                04  U02-DRNAME.
                  05  U02-VALUE   PIC X(29).
                04  U02-DRNAME-COUNT   PIC S9(9)   BINARY.
                04  U02-DRNAME-ITEM    PIC X(29)   OCCURS  99 TIMES.
              03  U02-SRYNAIYO-COMBO.
                04  U02-SRYNAIYO.
                  05  U02-VALUE   PIC X(21).
                04  U02-SRYNAIYO-COUNT   PIC S9(9)   BINARY.
                04  U02-SRYNAIYO-ITEM    PIC X(21)   OCCURS  99 TIMES.
              03  U02-KARUTE-COMBO.
                04  U02-KARUTE.
                  05  U02-VALUE   PIC X(30).
                04  U02-KARUTE-COUNT   PIC S9(9)   BINARY.
                04  U02-KARUTE-ITEM    PIC X(30)   OCCURS  5 TIMES.
              03  U02-SYOHO-COMBO.
                04  U02-SYOHO.
                  05  U02-VALUE   PIC X(14).
                04  U02-SYOHO-COUNT   PIC S9(9)   BINARY.
                04  U02-SYOHO-ITEM    PIC X(14)   OCCURS  2 TIMES.
              03  U02-HKNCOMBI.
                04  U02-VALUE   PIC S9(4).
              03  U02-SELNUM.
                04  U02-VALUE   PIC S9(4).
              03  U02-SCROLLEDWINDOW3.
                04  U02-HKNCOMBI-LIST.
                  05  U02-HKNCOMBI-COUNT   PIC S9(9)   BINARY.
                  05  U02-HKNCOMBI-ROW     PIC S9(9)   BINARY.
                  05  U02-HKNCOMBI-ROWATTR  PIC S9(9)   BINARY.
                  05  U02-ITEM        OCCURS  30 TIMES.
                    07  U02-HCNUM   PIC X(4).
                    07  U02-HCHKN   PIC X(10).
                    07  U02-HCKOH1  PIC X(10).
                    07  U02-HCKOH2  PIC X(10).
                    07  U02-HCKOH3  PIC X(10).
                    07  U02-HCKOH4  PIC X(10).
                  05  U02-HKNCOMBI-SELECT  PIC X       OCCURS  30 TIMES.
              03  U02-BHK1.
                04  U02-BHK1-VALUE   PIC X.
                04  U02-BHK1-LABEL   PIC X(2).
              03  U02-BHK2.
                04  U02-BHK2-VALUE   PIC X.
                04  U02-BHK2-LABEL   PIC X(2).
              03  U02-BHK3.
                04  U02-BHK3-VALUE   PIC X.
                04  U02-BHK3-LABEL   PIC X(2).
              03  U02-BHK4.
                04  U02-BHK4-VALUE   PIC X.
                04  U02-BHK4-LABEL   PIC X(2).
              03  U02-BHK5.
                04  U02-BHK5-VALUE   PIC X.
                04  U02-BHK5-LABEL   PIC X(2).
              03  U02-SCROLLEDWINDOW4.
                04  U02-UKETUKE-LIST.
                  05  U02-UKETUKE-COUNT   PIC S9(9)   BINARY.
                  05  U02-UKETUKE-ROW     PIC S9(9)   BINARY.
                  05  U02-UKETUKE-ROWATTR  PIC S9(9)   BINARY.
                  05  U02-ITEM        OCCURS  400 TIMES.
                    07  U02-ULNUM         PIC X(4).
                    07  U02-ULYYKTIME     PIC X(6).
                    07  U02-ULUKETUKETIME PIC X(8).
                    07  U02-ULPTNUM       PIC X(20).
                    07  U02-ULNAME        PIC X(50).
                    07  U02-ULSEX         PIC X(02).
                    07  U02-ULNENREI      PIC X(06).
                    07  U02-ULSRYKA       PIC X(21).
                    07  U02-ULDRNAME      PIC X(29).
                    07  U02-ULSRYNAIYO    PIC X(21).
                    07  U02-ULHKNCOMBI    PIC X(55).
                    07  U02-ULRRKYMD      PIC X(10).
                    07  U02-ULSRFLG       PIC X(04).
                  05  U02-UKETUKE-SELECT  PIC X       OCCURS  400 TIMES.
              03  U02-MIKAKUNINN-LIST.
                04  U02-MIKAKUNINN-COUNT   PIC S9(9)   BINARY.
                04  U02-MIKAKUNINN-ROW     PIC S9(9)   BINARY.
                04  U02-MIKAKUNINN-ROWATTR  PIC S9(9)   BINARY.
                04  U02-ITEM        OCCURS  5 TIMES.
                  05  U02-MLSEIDONAME       PIC X(22).
                  05  U02-MLKIGONUM         PIC X(40).
                  05  U02-MLLASTYMD         PIC X(10).
                04  U02-MIKAKUNINN-SELECT  PIC X       OCCURS  5 TIMES.
              03  U02-USERPG-COMB.
                04  U02-USERPG-STATE   PIC S9(9)   BINARY.
                04  U02-USERPG.
                  05  U02-VALUE   PIC X(20).
                04  U02-USERPG-COUNT   PIC S9(9)   BINARY.
                04  U02-USERPG-ITEM
                        PIC X(20)   OCCURS  2 TIMES.
              03  U02-NENREI.
                04  U02-VALUE   PIC X(6).
              03  U02-LASTYMD.
                04  U02-VALUE   PIC X(16).
              03  U02-MEMOC.
                04  U02-MEMOC-STYLE  PIC X(20).
                04  U02-MEMOC-VALUE   PIC X(06).
              03  U02-KOHMSG.
                04  U02-KOHMSG-STYLE  PIC X(20).
                04  U02-KOHMSG-VALUE   PIC X(50).
              03  U02-LABEL.
                04  U02-LABEL-STYLE  PIC X(20).
                04  U02-LABEL-VALUE   PIC X(50).
              03  U02-SEX.
                04  U02-VALUE   PIC X(2).
              03  U02-CHKMSG.
                04  U02-CHKMSG-STYLE  PIC X(20).
                04  U02-CHKMSG-VALUE   PIC X(20).
              03  U02-B05.
                04  U02-B05-STATE   PIC S9(9)   BINARY.
              03  U02-B06.
                04  U02-B06-STATE   PIC S9(9)   BINARY.
              03  U02-B07.
                04  U02-B07-STATE   PIC S9(9)   BINARY.
              03  U02-B08.
                04  U02-B08-STATE   PIC S9(9)   BINARY.
              03  U02-B09S.
                04  U02-B09S-STATE   PIC S9(9)   BINARY.
              03  U02-B10S.
                04  U02-B10S-STATE   PIC S9(9)   BINARY.
              03  U02-B03S.
                04  U02-B03S-STATE   PIC S9(9)   BINARY.
                04  U02-B03S-LABEL   PIC X(10).
              03  U02-UPDMSG.
                04  U02-UPMSG-STYLE   PIC X(20).
                04  U02-UPMSG-VALUE   PIC X(20).
