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
      ************************************************************
      *    ユーザー一覧サブ　COPY (CPORCSLDUSER.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SLDUSER-AREA.
           03  SLDUSER-MY-LD               PIC 9(03).
           03  SLDUSER-OTHERS              PIC 9(05).
           03  SLDUSER-MAX                 PIC 9(05).
           03  SLDUSER-ID                  PIC X(64).
           03  SLDUSER-KBN                 PIC 9(02).
      *        KBN 1:一括情報／2:個別情報／
      *            3:一括ポップアップ／4:個別ポップアップ／
      *            5:一括ダイアログ／6:個別ダイアログ／
      *            7:一括アボート／8:個別アボート
           03  SLDUSER-MESSAGE             PIC X(256).
           03  SLDUSER-OCC                 OCCURS  100.
               05  SLDUSER-HOSPNUM         PIC 9(02).
               05  SLDUSER-TERMID          PIC X(64).
               05  SLDUSER-LD              PIC 9(03).
               05  SLDUSER-IN-PROCESS      PIC X(64).
               05  SLDUSER-WINDOW          PIC X(64).
               05  SLDUSER-USER            PIC X(64).
               05  SLDUSER-EVENT           PIC X(64).
               05  SLDUSER-ACCESS-TIME     PIC X(64).
               05  SLDUSER-PROCESS-TIME    PIC X(64).
               05  SLDUSER-TOTAL-PROC      PIC X(64).
               05  SLDUSER-AGENT           PIC X(64).
               05  SLDUSER-HOST            PIC X(64).
               05  SLDUSER-WIDGET          PIC X(64).
               05  SLDUSER-CREATE-TIME     PIC X(64).
