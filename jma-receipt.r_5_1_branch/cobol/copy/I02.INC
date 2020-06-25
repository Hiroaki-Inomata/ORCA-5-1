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
      *****************************************************************
        02  I02.
            03  I02-FIXED4.
              04  I02-B05-STT              PIC S9(9)   BINARY.
              04  I02-SHOKAI               PIC X(4).
              04  I02-JYOTAI               PIC X(40).
              04  I02-BTUNAME              PIC X(20).
              04  I02-BRMNUM               PIC X(6).
              04  I02-NYUINYMD             PIC X(9).
              04  I02-LBLTIMEKBN-IN        PIC X(10).
              04  I02-TAIINYMD             PIC X(9).
              04  I02-TAIINCD              PIC X(10).
              04  I02-LBLTIMEKBN-OT        PIC X(10).
              04  I02-NYUINKA              PIC X(20).
              04  I02-DR                   PIC X(24).
              04  I02-SANTEI               PIC X(60).
              04  I02-SCROLLEDWINDOW2.
                05  I02-TENKALST.
                  06  I02-TENKALST-CNT     PIC S9(9)   BINARY.
                  06  I02-TENKALST-ROW     PIC S9(9)   BINARY.
                  06  I02-TENKALST-ROWATTR PIC S9(9)   BINARY.
                  06  I02-TENKALST-ITM     OCCURS  100.
                    07  I02-TNYUINKA       PIC X(20).
                    07  I02-TBTUNAME       PIC X(20).
                    07  I02-TBRMNUM        PIC X(6).
                    07  I02-TTENNYUYMD     PIC X(9).
                    07  I02-TTENSTUYMD     PIC X(9).
                    07  I02-TNISSU         PIC X(5).
                    07  I02-TTUSAN         PIC X(5).
                    07  I02-TIDONAIYO      PIC X(40).
                  06  I02-TENKALST-SELECT  PIC X
                                           OCCURS  100.
              04  I02-CMB-NYUGAI.
                05  I02-NYUGAI             PIC X(8).
                05  I02-NYUGAI-STT         PIC S9(9)   BINARY.
                05  I02-NYUGAI-CNT         PIC S9(9)
                                           BINARY.
                05  I02-NYUGAI-ITM         PIC X(8)
                                           OCCURS  5.
                05  I02-NYUGAI-ITM-STT     PIC S9(9)   BINARY.
              04  I02-SCROLLEDWINDOW1.
                05  I02-BYOMEILST.
                  06  I02-BYOMEILST-CNT    PIC S9(9)   BINARY.
                  06  I02-BYOMEILST-ROW    PIC S9(9)   BINARY.
                  06  I02-BYOMEILST-ROWATTR PIC S9(9)   BINARY.
                  06  I02-BYOMEILST-ITM    OCCURS  500 TIMES.
                    07  I02-TSYUBYO        PIC X(2).
                    07  I02-TNYUGAI        PIC X(4).
                    07  I02-TSRYKA         PIC X(20).
                    07  I02-TBYOMEI        PIC X(50).
                    07  I02-TSTSRYYMD      PIC X(9).
                    07  I02-TTENKIYMD      PIC X(9).
                    07  I02-TTENKI         PIC X(4).
                  06  I02-BYOMEILST-SELECT PIC X
                                           OCCURS  500.
