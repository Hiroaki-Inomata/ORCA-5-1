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
      ******************************************************************
      *    ＡＰＩリクエスト入院病棟情報    COPY (CPHSCONFWARDV2REQ.INC)
      ******************************************************************
        01  XML-HSCONFWARDV2REQ.
            03  HSCWREQ-CONTEXT          PIC S9(9)   BINARY.
            03  HSCWREQ-OBJECT           PIC X(36).
            03  HSCWREQ-MODE             PIC S9(9)   BINARY.
            03  HSCWREQ-RECORDNAME       PIC X(128).
      *
            03  HSCWREQ-PRIVATE-OBJECTS.
                05  HSCWREQ-REQUEST-NUMBER PIC X(1).
                05  HSCWREQ-BASE-DATE    PIC X(10).
                05  HSCWREQ-BTUNUM       PIC X(2).
                05  HSCWREQ-BRMNUM       PIC X(6).
