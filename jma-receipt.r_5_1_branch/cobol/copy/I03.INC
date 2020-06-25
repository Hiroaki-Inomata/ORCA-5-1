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
        02  I03.
            03  I03-FIXED4.
              04  I03-B02-STT              PIC S9(9)   BINARY.
              04  I03-B03-STT              PIC S9(9)   BINARY.
              04  I03-B04-STT              PIC S9(9)   BINARY.
              04  I03-CMB-SHOKAI.
                05  I03-SHOKAI             PIC X(8).
                05  I03-SHOKAI-STT         PIC S9(9)   BINARY.
                05  I03-SHOKAI-CNT         PIC S9(9)
                                           BINARY.
                05  I03-SHOKAI-ITM         PIC X(8)
                                           OCCURS  2.
                05  I03-SHOKAI-ITM-STT     PIC S9(9)   BINARY.
              04  I03-SHONUM               PIC S9(3).
              04  I03-SHONUM-STT           PIC S9(9)   BINARY.
              04  I03-COMMENT              PIC X(20).
              04  I03-NYUINYMD             PIC X(10).
              04  I03-NYUINYMD-STT         PIC S9(9)   BINARY.
              04  I03-TAIINYMD             PIC X(10).
              04  I03-TAIINYMD-STT         PIC S9(9)   BINARY.
              04  I03-TAISYONISSU          PIC S9(5).
              04  I03-TAISYONISSU-STT      PIC S9(9)   BINARY.
              04  I03-BYOMEI               PIC X(50)
                                           OCCURS  5.
              04  I03-KHNRYO               PIC X(50).
              04  I03-CMB-RRKKBN.
                05  I03-RRKKBN             PIC X(40).
                05  I03-RRKKBN-STT         PIC S9(9)   BINARY.
                05  I03-RRKKBN-CNT         PIC S9(9)
                                           BINARY.
                05  I03-RRKKBN-ITM         PIC X(40)
                                           OCCURS  3.
                05  I03-RRKKBN-ITM-STT     PIC S9(9)   BINARY.
              04  I03-CMB-NYUINCHUKBN.
                05  I03-NYUINCHUKBN        PIC X(10).
                05  I03-NYUINCHUKBN-STT    PIC S9(9)   BINARY.
                05  I03-NYUINCHUKBN-CNT    PIC S9(9)
                                           BINARY.
                05  I03-NYUINCHUKBN-ITM    PIC X(10)
                                           OCCURS  2.
                05  I03-NYUINCHUKBN-ITM-STT
                                           PIC S9(9)   BINARY.
              04  I03-TOKUTAISYONISSU      PIC S9(5).
              04  I03-TOKUTAISYONISSU-STT  PIC S9(9)   BINARY.
