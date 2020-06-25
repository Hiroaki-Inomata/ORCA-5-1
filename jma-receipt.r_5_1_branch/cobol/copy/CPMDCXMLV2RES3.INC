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
      *    API 初診算定日登録情報(xml2)   COPY (CPMDCXMLV2RES3.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  XML-MEDICALV2RES3.
           03  MDCRES-CONTEXT             PIC S9(9)   BINARY.
           03  MDCRES-OBJECT              PIC X(36).
           03  MDCRES-MODE                PIC S9(9)   BINARY.
           03  MDCRES-RECORDNAME          PIC X(128).
           03  MDCRES-MEDICALRES.
      *        施日
               05  MDCRES-INFORMATION-DATE  PIC X(10).
      *        実施時間
               05  MDCRES-INFORMATION-TIME  PIC X(8).
      *        返却コード
               05  MDCRES-API-RESULT        PIC X(3).
      *        返却メッセージ
               05  MDCRES-API-RESULT-MSG    PIC X(100).
      *
               05  MDCRES-RESKEY            PIC X(100).
      *        患者情報
               05  MDCRES-PATIENT-INFOMATION.
      *            患者番号
                   07  MDCRES-PATIENTID        PIC X(20).
      *            患者氏名
                   07  MDCRES-NAME             PIC X(100).
                   07  MDCRES-KANANAME         PIC X(100).
                   07  MDCRES-BIRTHDAY         PIC X(10).
                   07  MDCRES-SEX              PIC X(1).
      *        初診算定日
               05  MDCRES-FIRST-DATE       PIC X(10).
      *        最終来院日
               05  MDCRES-LAST-DATE        PIC X(10).
      *        診療科
               05  MDCRES-DEP-CODE         PIC X(2).
               05  MDCRES-DEP-NAME         PIC X(80).
      *        受診履歴初回日（リクエスト＝00 のみ）
               05  MDCRES-FIRST-VISIT-DATE  PIC X(10).
      *
