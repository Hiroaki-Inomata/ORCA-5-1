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
          03  Z91.
                04  Z91-FIXED2.
                    05  Z91-B11.
                        07  Z91-B11-STATE      PIC S9(9)   BINARY.
                    05  Z91-SRYCHK             PIC X.
                    05  Z91-SRY                PIC X        OCCURS  2.
                    05  Z91-SRYSTRCOMBO.
                        06  Z91-SRYSTR         PIC X(17).
                        06  Z91-SRYSTR-STATE   PIC S9(9)   BINARY.
                        06  Z91-SRYSTR-COUNT   PIC S9(9)   BINARY.
                        06  Z91-SRYSTR-ITEM    PIC X(17)   OCCURS  14.
                    05  Z91-SRYENDCOMBO.
                        06  Z91-SRYEND         PIC X(17).
                        06  Z91-SRYEND-STATE   PIC S9(9)   BINARY.
                        06  Z91-SRYEND-COUNT   PIC S9(9)   BINARY.
                        06  Z91-SRYEND-ITEM    PIC X(17)   OCCURS  14.
                    05  Z91-IYKCHKALL          PIC X.
                    05  Z91-IPNCHKALL          PIC X.
                    05  Z91-IYK-G.
                        06  Z91-IYKCHK1        PIC X.
                        06  Z91-IYKCHK1-STATE  PIC S9(9)   BINARY.
                        06  Z91-IYKCHK2        PIC X.
                        06  Z91-IYKCHK2-STATE  PIC S9(9)   BINARY.
                        06  Z91-IYKCHK3        PIC X.
                        06  Z91-IYKCHK3-STATE  PIC S9(9)   BINARY.
                        06  Z91-IYKCHK4        PIC X.
                        06  Z91-IYKCHK4-STATE  PIC S9(9)   BINARY.
                        06  Z91-IYKCHK5        PIC X.
                        06  Z91-IYKCHK5-STATE  PIC S9(9)   BINARY.
                        06  Z91-IYKCHK6        PIC X.
                        06  Z91-IYKCHK6-STATE  PIC S9(9)   BINARY.
                        06  Z91-IYKCHK7        PIC X.
                        06  Z91-IYKCHK7-STATE  PIC S9(9)   BINARY.
                    05  Z91-TOKCHK             PIC X.
                    05  Z91-HOKACHK            PIC X.
                    05  Z91-KZICHK             PIC X.
                    05  Z91-COMCHKALL          PIC X.
                    05  Z91-COMCHK             PIC X       OCCURS  3.
                    05  Z91-JIHCHK             PIC X.
                    05  Z91-MSTYMD             PIC X(10).
                    05  Z91-KEI                PIC X       OCCURS  2.
                    05  Z91-CYU-G.
                        06  Z91-CYU1           PIC X.
                        06  Z91-CYU1-STATE     PIC S9(9)   BINARY.
                        06  Z91-CYU2           PIC X.
                        06  Z91-CYU3           PIC X.
                    05  Z91-IPN                PIC X.
                    05  Z91-IPN-STATE          PIC S9(9)   BINARY.
                    05  Z91-SYU-G.
                        06  Z91-SYU1           PIC X.
                        06  Z91-SYU2           PIC X.
                        06  Z91-SYU3           PIC X.
                        06  Z91-SYU4           PIC X.
                        06  Z91-SYU4-STATE     PIC S9(9)   BINARY.

