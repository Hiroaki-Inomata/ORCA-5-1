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
      ******************************************************************
      *    ＡＰＩレスポンスＣＬＡＩＭ中途データ等取得 (xml2)
      *                                      COPY (CPCLAIMRECEIVEV2RES.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  XML-CLAIM-RECEIVEV2RES.
           02  CLRERES-CONTEXT           PIC S9(9)   BINARY.
           02  CLRERES-OBJECT            PIC X(36).
           02  CLRERES-MODE              PIC S9(9)   BINARY.
           02  CLRERES-RECORDNAME        PIC X(128).
           02  CLRERES-CLAIM-FRONT.
      *        実施日
               03  CLRERES-INFORMATION-DATE    PIC X(10).
      *        実施時間
               03  CLRERES-INFORMATION-TIME    PIC X(8).
      *        返却コード
               03  CLRERES-API-RESULT          PIC X(2).
      *        返却メッセージ
               03  CLRERES-API-RESULT-MSG      PIC X(100).
      *        患者番号
               03  CLRERES-PATIENTID           PIC X(20).
      *        処理日
               03  CLRERES-PERFORM-DATE        PIC X(19).
      *        メッセージ
               03  CLRERES-MESSAGE-G.
      *            メッセージ
                   05  CLRERES-WARNING-MSG-TBL   OCCURS  50.
      *                メッセージコード
                       07  CLRERES-WARNING        PIC X(03).
      *                メッセージ
                       07  CLRERES-WARNING-MSG    PIC X(100).
