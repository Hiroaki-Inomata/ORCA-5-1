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
      *    HAORIリクエスト　保険一括変更入力情報 (一体化)
      *                                     COPY (CPMDCXMLV3REQ6.INC)
      ******************************************************************
       01  XML-MEDICALV3REQ6.
           03  MDCREQ-CONTEXT          PIC S9(9)   BINARY.
           03  MDCREQ-OBJECT           PIC X(36).
           03  MDCREQ-MODE             PIC S9(9)   BINARY.
           03  MDCREQ-RECORDNAME       PIC X(128).
           03  MDCREQ-MEDICALREQ.
      *       リクエスト番号
              05  MDCREQ-REQUEST-NUMBER    PIC X(2).
      *       UID（電子カルテ）
              05  MDCREQ-KARTE-UID         PIC X(36).
      *       基準日付
      *****   05  MDCREQ-BASE-DATE         PIC X(10).
      *       UID（オルカ）
              05  MDCREQ-ORCA-UID          PIC X(36).
      *
      *       患者番号
              05  MDCREQ-PATIENTID         PIC X(20).
      *       診療年月
              05  MDCREQ-PERFORM-YM        PIC X(07).
      *       診療科
              05  MDCREQ-APPOINT-DEP-CODE  PIC X(02).
      *H30.6
      *       入外区分（１：入院）
              05  MDCREQ-OUTPATIENT-CLASS  PIC X(1).
      *
      *       一括変更内容（リクエスト＝０２）
              05  MDCREQ-CHENGE-INFO.
      *           変更前　保険組合せ番号
                  07  MDCREQ-IN-COMBINATION-NUMBER    PIC X(04).
      *           変更後　保険組合せ番号
                  07  MDCREQ-OT-COMBINATION-NUMBER    PIC X(04).
      *           変更期間
                  07  MDCREQ-START-DAY         PIC X(02).
                  07  MDCREQ-END-DAY           PIC X(02).
      *           収納一括更新あり
                  07  MDCREQ-INCOME-CHANGE     PIC X(03).
      *
