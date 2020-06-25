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
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      ******************************************************************
      *    ＡＰＩリクエスト来院一覧情報取得 (xml2)
      *                                        COPY (CPVISITPTLSTEQ.INC)
      ******************************************************************
        01  XML-VISITPTLSTREQ.
            02  VISREQ-CONTEXT           PIC S9(9)   BINARY.
            02  VISREQ-OBJECT            PIC X(36).
            02  VISREQ-MODE              PIC S9(9)   BINARY.
            02  VISREQ-RECORDNAME        PIC X(128).
            02  VISREQ-VISITPTREQ.
      *       処理区分
              03  VISREQ-REQUEST-NUMBER    PIC X(02).
      *       診療日付
              03  VISREQ-VISIT-DATE        PIC X(10).
      *       診療科
              03  VISREQ-DEP-CODE          PIC X(02).
      *
