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
      *    ＡＰＩリクエスト受付一覧　　      COPY (CPACCPTLSTV2REQ.INC)
      ******************************************************************
        01  XML-ACCEPTLSTREQ.
            02  ACPLSTREQ-CONTEXT          PIC S9(9)   BINARY.
            02  ACPLSTREQ-OBJECT           PIC X(36).
            02  ACPLSTREQ-MODE             PIC S9(9)   BINARY.
            02  ACPLSTREQ-RECORDNAME       PIC X(128).
            02  ACPLSTREQ-APPOINT.
      *       受付日
              03  ACPLSTREQ-ACCEPT-DATE        PIC X(10).
      *       診療科
              03  ACPLSTREQ-ACCEPT-DEP-CODE    PIC X(2).
      *       ドクターコード
              03  ACPLSTREQ-ACCEPT-DR-CODE     PIC X(5).
      *       診療内容コード
              03  ACPLSTREQ-ACCEPT-DEP-INFO    PIC X(2).
      *       受付順SORT区分
              03  ACPLSTREQ-DISPLAY-SORT       PIC X(5).
