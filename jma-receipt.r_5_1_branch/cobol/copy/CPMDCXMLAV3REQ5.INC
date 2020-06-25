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
      *    HAORIリクエスト　入院調剤料一括変更情報 (一体化)
      *                                     COPY (CPMDCXMLAV3REQ5.INC)
      *         診療行為内容（入院）
      ******************************************************************
       01  XML-MEDICALAV3REQ5.
           03  MDCREQ-CONTEXT          PIC S9(9)   BINARY.
           03  MDCREQ-OBJECT           PIC X(36).
           03  MDCREQ-MODE             PIC S9(9)   BINARY.
           03  MDCREQ-RECORDNAME       PIC X(128).
           03  MDCREQ-MEDICALREQ.
      *       リクエスト番号
               05  MDCREQ-REQUEST-NUMBER    PIC X(2).
      *        UID（電子カルテ）
               05  MDCREQ-KARTE-UID         PIC X(36).
      *        UID（オルカ）
               05  MDCREQ-ORCA-UID          PIC X(36).
      *
      *        患者番号
               05  MDCREQ-PATIENTID         PIC X(20).
      *        診療年月
               05  MDCREQ-PERFORM-YM        PIC X(07).
      *
      *リクエスト＝０２
      *        入院調剤料
               05  MDCREQ-MEDICAL-CHOZAIINF-G.
      *             診療科
                    09  MDCREQ-CHZ-DEP-CODE        PIC X(2).
      *             保険組合せ番号
                    09  MDCREQ-CHZ-COMBINATION     PIC X(4).
      *
      *             剤番号
                    09  MDCREQ-CHZ-ZAINUM          PIC X(8).
      *
      *             診療種別区分
                    09  MDCREQ-CHZ-CLASS           PIC X(3).
      *             診療種別名称
                    09  MDCREQ-CHZ-CLASS-NAME      PIC X(40).
      *             算定日
                    09  MDCREQ-CHZ-DAY-AREA.
      *                 算定履歴
                        11  MDCREQ-CHZ-RRK-G.
      *                 回数
                            13  MDCREQ-CHZ-CLASS-NUMBER  PIC X(03)
                                                     OCCURS   31.
      *             同日再入院の算定
                    09  MDCREQ-CHZ-REDAY-G.
                        11  MDCREQ-CHZ-REDAY-OCC    OCCURS   31.
      *                     同日再入院日
                            13  MDCREQ-CHZ-REDAY        PIC X(02).
      *                     回数
                            13  MDCREQ-CHZ-RECLASS-NUMBER  PIC X(03).
      *
                    09  MDCREQ-CHZ-INFO-G.
                        11  MDCREQ-CHZ-INFO    OCCURS   5.
      *                     診療コード
                            13  MDCREQ-CHZ-CODE        PIC X(9).
      *                     診療名称
                            13  MDCREQ-CHZ-NAME        PIC X(80).
      *                     自動区分
                            13  MDCREQ-CHZ-AUTOKBN     PIC X(01).
      *
