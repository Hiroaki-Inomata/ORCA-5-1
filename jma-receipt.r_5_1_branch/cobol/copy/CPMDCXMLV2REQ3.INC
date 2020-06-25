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
      *    APIリクエスト　初診算定日登録 
      *                                     COPY (CPMDCXMLV2REQ3.INC)
      ******************************************************************
       01  XML-MEDICALV2REQ3.
           03  MDCREQ-CONTEXT          PIC S9(9)   BINARY.
           03  MDCREQ-OBJECT           PIC X(36).
           03  MDCREQ-MODE             PIC S9(9)   BINARY.
           03  MDCREQ-RECORDNAME       PIC X(128).
           03  MDCREQ-MEDICALREQ.
      *       リクエスト番号
              05  MDCREQ-REQUEST-NUMBER    PIC X(2).
      *       患者番号
              05  MDCREQ-PATIENTID         PIC X(20).
      *
      *       初診算定日
              05  MDCREQ-FIRST-DATE        PIC X(10).
      *       最終来院日（履歴なし患者のみ）
              05  MDCREQ-LAST-DATE         PIC X(10).
      *       診療科
              05  MDCREQ-DEP-CODE          PIC X(2).
      *
      *
