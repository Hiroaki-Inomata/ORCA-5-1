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
      *    ＡＰＩリクエスト負担金計算情報
      *                                    COPY (CPACSIMULATEV2REQ.INC)
      ******************************************************************
        01  XML-ACSIMULATEREQ.
            02  ACSREQ-CONTEXT          PIC S9(9)   BINARY.
            02  ACSREQ-OBJECT           PIC X(36).
            02  ACSREQ-MODE             PIC S9(9)   BINARY.
            02  ACSREQ-RECORDNAME       PIC X(128).
            02  ACSREQ-MEDICALREQ.
      *       患者番号
              03  ACSREQ-PATIENTID         PIC X(20).
      *       診療日付
              03  ACSREQ-PERFORM-DATE      PIC X(10).
      *       時間外区分（診察料）
              03  ACSREQ-TIME-CLASS        PIC X(1).
      *
              03  ACSREQ-MEDICAL-INFO.
      *         診療科
                04  ACSREQ-APPOINT-DEP-CODE    PIC X(2).
      *         保険組合せ番号
                04  ACSREQ-COMBINATION-NUMBER  PIC X(4).
      *         保険情報
                04  ACSREQ-INSURANCE-INFO.
      *           保険の種類
                  05  ACSREQ-MAIN-INSURANCE-CLASS    PIC X(03).
      *           保険者番号
                  05  ACSREQ-MAIN-INSURANCE-NUMBER   PIC X(08).
      *           保険の種類名称
                  05  ACSREQ-MAIN-INSURANCE-NAME     PIC X(20).
      *           記号
                  05  ACSREQ-MAIN-MARK               PIC X(80).
      *           番号
                  05  ACSREQ-MAIN-NUMBER             PIC X(80).
      *           継続区分
                  05  ACSREQ-MAIN-CONTINUATION       PIC X(01).
      *           補助区分
                  05  ACSREQ-MAIN-ASSISTANCE         PIC X(01).
      *           本人家族区分
                  05  ACSREQ-MAIN-FAMILY             PIC X(01).
      *           被保険者名
                  05  ACSREQ-MAIN-POLICYHOLDER       PIC X(100).
      *           有効開始日
                  05  ACSREQ-MAIN-START-DATE         PIC X(10).
      *           有効終了日
                  05  ACSREQ-MAIN-END-DATE           PIC X(10).
      *           公費情報
                  05  ACSREQ-PUBLIC-INSURANCE-INF  OCCURS  4 TIMES.
      *             公費の種類
                    06  ACSREQ-INSURANCE-CLASS       PIC X(03).
      *             公費の種類名称
                    06  ACSREQ-INSURANCE-NAME        PIC X(20).
      *             負担者番号
                    06  ACSREQ-PROVIDER-NUMBER       PIC X(08).
      *             受給者番号
                    06  ACSREQ-RECIPIENT-NUMBER      PIC X(20).
      *             有効開始日
                    06  ACSREQ-INSURANCE-START-DATE  PIC X(10).
      *             有効終了日
                    06  ACSREQ-INSURANCE-END-DATE    PIC X(10).
      *         診療内容
                04  ACSREQ-MDC-INFO         OCCURS  40 TIMES.
      *           診療種別区分
                  05  ACSREQ-MDC-CLASS               PIC X(3).
      *??
                  05  ACSREQ-MDC-CLASS-NAME          PIC X(50).
      *           回数
                  05  ACSREQ-MDC-CLASS-NUMBER        PIC X(50).
      *           剤内容
                  05  ACSREQ-PRESCRIPTION-INFO     OCCURS  40 TIMES.
      *             診療コード
                    06  ACSREQ-PRSCRPT-CODE          PIC X(9).
      *             診療名称（コメント）
                    06  ACSREQ-PRSCRPT-NAME          PIC X(80).
      *             数量
                    06  ACSREQ-PRSCRPT-NUMBER        PIC X(40).
      *             処方せん一般名記載
                    06  ACSREQ-GENERIC-FLG           PIC X(03).
