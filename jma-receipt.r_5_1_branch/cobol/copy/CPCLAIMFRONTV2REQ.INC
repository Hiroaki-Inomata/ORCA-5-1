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
      *    ＡＰＩリクエストＣＬＡＩＭ受付送信取得 (xml2)
      *                                      COPY (CPCLAIMFRONTV02REQ.INC)
      ******************************************************************
       01  XML-CLAIM-FRONTV2REQ.
           02  CLFRREQ-CONTEXT           PIC S9(9)   BINARY.
           02  CLFRREQ-OBJECT            PIC X(36).
           02  CLFRREQ-MODE              PIC S9(9)   BINARY.
           02  CLFRREQ-RECORDNAME        PIC X(128).
           02  CLFRREQ-CLAIM-FRONT.
      *        リクエスト番号
               03  CLFRREQ-REQUEST-NUMBER    PIC X(02).
      *        患者番号
               03  CLFRREQ-PATIENTID         PIC X(20).
      *        受付日付
               03  CLFRREQ-ACCEPT-DATE       PIC X(10).
      *        受付時間
               03  CLFRREQ-ACCEPT-TIME       PIC X(8).
      *        診療科
               03  CLFRREQ-DEP-CODE          PIC X(02).
      *        ドクター
               03  CLFRREQ-DR-CODE           PIC X(05).
      *        保険組合せ番号
               03  CLFRREQ-COMBINATION-NUMBER      PIC X(04).
      *        診療内容名称
               03  CLFRREQ-MEDICAL-NAME       PIC X(16).
      *        メモ送信区分
               03  CLFRREQ-MEMO-CODE          PIC X(02).
      *        受付済み送信区分（リクエスト番号＝０２　のみ）
               03  CLFRREQ-SEND-STATUS       PIC X(02).
      *
