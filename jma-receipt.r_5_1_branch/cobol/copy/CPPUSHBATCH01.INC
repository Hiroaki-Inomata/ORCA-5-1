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
       01  PUSHBA01-REC.
           03  PUSHBA01-EVENT                  PIC X(50).
           03  PUSHBA01-BODY.
      *        カルテUID
               05  PUSHBA01API-KARTE-UID       PIC X(36).
      *        オルカUID
               05  PUSHBA01API-ORCA-UID        PIC X(36).
      *        処理結果
               05  PUSHBA01API-RESULT          PIC X(3).
      *        診療年月
               05  PUSHBA01-PERFORM-MONTH      PIC X(7).
      *        基準日
               05  PUSHBA01-PERFORM-DATE       PIC X(10).
      *        処理区分
               05  PUSHBA01-REQUEST-MODE       PIC X(1).
      *        開始日
               05  PUSHBA01-START-DATE         PIC X(10).
      *        終了日
               05  PUSHBA01-END-DATE           PIC X(10).
      *        処理件数
               05  PUSHBA01-OUTPUT-COUNT       PIC X(10).
      *        処理件数（データ）
               05  PUSHBA01-NUMBER-OF-RECORDS  PIC X(10).
      *        処理件数（帳票）
               05  PUSHBA01-NUMBER-OF-SHEETS   PIC X(10).
