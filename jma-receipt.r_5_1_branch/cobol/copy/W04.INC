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
        02  W04.
            03  W04-FIXED2.
              04  W04-NEN.
                05  W04-VALUE   PIC X(4).
              04  W04-RENNUM-G.
                05  W04-RENNUM-STATE   PIC S9(9)   BINARY.
                05  W04-RENNUM   PIC X(10).
              04  W04-PTID.
                05  W04-VALUE   PIC X(10).
              04  W04-STYUKYMD.
                05  W04-VALUE   PIC X(9).
              04  W04-EDYUKYMD.
                05  W04-VALUE   PIC X(9).
              04  W04-PTNUMKSIKBN.
                05  W04-VALUE   PIC X(1).
              03  W04-NENTUKI.
                04  W04-VALUE   PIC X(2).
              04  W04-NOTEBOOK1.
                05  W04-PAGENO  PIC S9(9)   BINARY.
                05  W04-FIXED5.
                  06  W04-JIYKSIKBN.
                    07  W04-VALUE   PIC X(1).
                  06  W04-JIYKSIKETA.
                    07  W04-VALUE   PIC 9(2).
                05  W04-FIXED4.
                  06  W04-HJNKSIKBN.
                    07  W04-VALUE   PIC X(1).
                  06  W04-HJNKSINENKBN.
                    07  W04-VALUE   PIC X(1).
                  06  W04-HJNKSIRENNUMKBN.
                    07  W04-VALUE   PIC X(1).
                  06  W04-HJNKSIRENNUMKETA.
                    07  W04-VALUE   PIC 9(2).
                05  W04-FIXED3.
                  06  W04-KKCKSIKBN.
                    07  W04-VALUE   PIC X(1).
                  06  W04-KKCKSIMAEKETA.
                    07  W04-VALUE   PIC 9(2).
                  06  W04-KKCKSIRENNUMKETA.
                    07  W04-VALUE   PIC 9(2).
                  06  W04-KKCKSIATOKETA.
                    07  W04-VALUE   PIC 9(2).
