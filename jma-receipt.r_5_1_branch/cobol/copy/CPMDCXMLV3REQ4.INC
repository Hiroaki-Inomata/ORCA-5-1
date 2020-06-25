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
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      ******************************************************************
      *    ＡＰＩリクエスト診療行為登録情報 (一体化)
      *                                       COPY (CPMDCXMLV3REQ4.INC)
      *         診療行為削除
      ******************************************************************
       01  XML-MEDICALV3REQ4.
           03  MDCREQ-CONTEXT          PIC S9(9)   BINARY.
           03  MDCREQ-OBJECT           PIC X(36).
           03  MDCREQ-MODE             PIC S9(9)   BINARY.
           03  MDCREQ-RECORDNAME       PIC X(128).
           03  MDCREQ-MEDICALREQ.
      *        リクエスト番号
               05  MDCREQ-REQUEST-NUMBER    PIC X(2).
      *        UID（電子カルテ）
               05  MDCREQ-KARTE-UID         PIC X(36).
      *        UID（オルカ）
               05  MDCREQ-ORCA-UID          PIC X(36).
      *        患者番号
               05  MDCREQ-PATIENTID         PIC X(20).
      *        診療日付
               05  MDCREQ-PERFORM-DATE      PIC X(10).
      *        処理区分
               05  MDCREQ-PATIENT-MODE      PIC X(06).
      *        伝票番号
               05  MDCREQ-INVOICD-NUMBER    PIC X(07).
      *
      *        診療科
               05  MDCREQ-APPOINT-DEP-CODE  PIC X(2).
      *        保険組合せ番号
               05  MDCREQ-COMBINATION-NUMBER   PIC X(04).
      *        連番
               05  MDCREQ-NUMBER            PIC X(01).
      *
      *        確認メッセージ返答
               05  MDCREQ-SELECT-ANSWER     PIC X(02).
      *
      *
