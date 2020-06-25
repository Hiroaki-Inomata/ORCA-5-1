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
      *    ＡＰＩリクエスト併用禁忌薬剤チェック情報
      *                                    COPY (CPCONTRACHECKV2REQ.INC)
      ******************************************************************
       01  XML-CONTRA-CHECKV2REQ.
           03  CHKREQ-CONTEXT          PIC S9(9)   BINARY.
           03  CHKREQ-OBJECT           PIC X(36).
           03  CHKREQ-MODE             PIC S9(9)   BINARY.
           03  CHKREQ-RECORDNAME       PIC X(128).
           03  CHKREQ-CONTRACHKREQ.
      *       リクエスト番号
              05  CHKREQ-REQUEST-NUMBER    PIC X(2).
      *       患者番号
              05  CHKREQ-PATIENTID         PIC X(20).
      *       診療日付
              05  CHKREQ-PERFORM-YM        PIC X(07).
      *       対象期間月数
              05  CHKREQ-CHECK-TUKI        PIC X(02).
      *
      *       薬剤情報
              05  CHKREQ-MDC-INFO-G.
                  07  CHKREQ-PRESCRIPTION-INFO    OCCURS   30.
      *               薬剤コード
                      09  CHKREQ-PRSCRPT-CODE          PIC X(9).
                      09  CHKREQ-PRSCRPT-NAME          PIC X(80).
      *
