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
      *    ＡＰＩリクエスト入院診療行為入力情報 (一体化)  COPY (CPMDCXMLAV3REQ.INC)
      ******************************************************************
       01  XML-MEDICALAV3REQ1.
           03  MDCREQ-CONTEXT          PIC S9(9)   BINARY.
           03  MDCREQ-OBJECT           PIC X(36).
           03  MDCREQ-MODE             PIC S9(9)   BINARY.
           03  MDCREQ-RECORDNAME       PIC X(128).
           03  MDCREQ-MEDICALREQ.
      *       リクエスト番号
              05  MDCREQ-REQUEST-NUMBER    PIC X(2).
      *       UID（電子カルテ）
              05  MDCREQ-KARTE-UID         PIC X(36).
      *       患者番号
              05  MDCREQ-PATIENTID         PIC X(20).
      *       診療日付
              05  MDCREQ-PERFORM-DATE      PIC X(10).
      *       診療時間
              05  MDCREQ-PERFORM-TIME      PIC X(8).
      *       UID（オルカ）
              05  MDCREQ-ORCA-UID          PIC X(36).
      *
      *       診療科
              05  MDCREQ-APPOINT-DEP-CODE    PIC X(2).
      *       同日再入院分
              05  MDCREQ-DOUJITSUKBN         PIC X(1).
      *         ドクターコード
      *****     07  MDCREQ-APPOINT-DR-CODE     PIC X(5).
      *       保険情報
              05  MDCREQ-INSURANCE-INFO.
      *           保険組合せ番号
                  09  MDCREQ-COMBINATION-NUMBER      PIC X(04).
      *           保険の種類
                  09  MDCREQ-MAIN-INSURANCE-CLASS    PIC X(03).
      *           保険者番号
                  09  MDCREQ-MAIN-INSURANCE-NUMBER   PIC X(08).
      *           保険の種類名称
                  09  MDCREQ-MAIN-INSURANCE-NAME     PIC X(20).
      *           記号
                  09  MDCREQ-MAIN-MARK               PIC X(80).
      *           番号
                  09  MDCREQ-MAIN-NUMBER             PIC X(80).
      *           継続区分
                  09  MDCREQ-MAIN-CONTINUATION       PIC X(01).
      *           補助区分
                  09  MDCREQ-MAIN-ASSISTANCE         PIC X(01).
      *           本人家族区分
                  09  MDCREQ-MAIN-FAMILY             PIC X(01).
      *           被保険者名
                  09  MDCREQ-MAIN-POLICYHOLDER       PIC X(100).
      *           有効開始日
                  09  MDCREQ-MAIN-START-DATE         PIC X(10).
      *           有効終了日
                  09  MDCREQ-MAIN-END-DATE           PIC X(10).
      *           公費情報
                  09  MDCREQ-PUBLIC-INSURANCE-INF  OCCURS  4 TIMES.
      *             公費の種類
                    11  MDCREQ-INSURANCE-CLASS       PIC X(03).
      *             公費の種類名称
                    11  MDCREQ-INSURANCE-NAME        PIC X(20).
      *             負担者番号
                    11  MDCREQ-PROVIDER-NUMBER       PIC X(08).
      *             受給者番号
                    11  MDCREQ-RECIPIENT-NUMBER      PIC X(20).
      *             有効開始日
                    11  MDCREQ-INSURANCE-START-DATE  PIC X(10).
      *             有効終了日
                    11  MDCREQ-INSURANCE-END-DATE    PIC X(10).
      *
      *
