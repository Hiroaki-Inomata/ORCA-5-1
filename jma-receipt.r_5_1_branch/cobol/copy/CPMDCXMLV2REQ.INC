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
      *    ＡＰＩリクエスト中途終了情報 (xml2)  COPY (CPMDCXMLV2REQ.INC)
      ******************************************************************
        01  XML-MEDICALREQ.
            02  MDCREQ-CONTEXT          PIC S9(9)   BINARY.
            02  MDCREQ-OBJECT           PIC X(36).
            02  MDCREQ-MODE             PIC S9(9)   BINARY.
            02  MDCREQ-RECORDNAME       PIC X(128).
            02  MDCREQ-MEDICALREQ.
      *       入外区分（１：入院）
              03  MDCREQ-OUTPATIENT-CLASS  PIC X(1).
      *       患者番号
              03  MDCREQ-PATIENTID         PIC X(20).
      *       診療日付
              03  MDCREQ-PERFORM-DATE      PIC X(10).
      *       診療時間
              03  MDCREQ-PERFORM-TIME      PIC X(8).
      *       UID
              03  MDCREQ-MEDICAL-UID       PIC X(36).
      *       入院日
              03  MDCREQ-ADMISSION-DATE    PIC X(10).
      *
              03  MDCREQ-MEDICAL-INFO.
      *         診療科
                04  MDCREQ-APPOINT-DEP-CODE PIC X(2).
      *         ドクターコード
                04  MDCREQ-APPOINT-DR-CODE  PIC X(5).
      *         保険情報
                04  MDCREQ-INSURANCE-INFO.
      *           保険組合せ番号
                  05  MDCREQ-COMBINATION-NUMBER      PIC X(04).
      *           保険の種類
                  05  MDCREQ-MAIN-INSURANCE-CLASS    PIC X(03).
      *           保険者番号
                  05  MDCREQ-MAIN-INSURANCE-NUMBER   PIC X(08).
      *           保険の種類名称
                  05  MDCREQ-MAIN-INSURANCE-NAME     PIC X(20).
      *           記号
                  05  MDCREQ-MAIN-MARK               PIC X(80).
      *           番号
                  05  MDCREQ-MAIN-NUMBER             PIC X(80).
      *           継続区分
                  05  MDCREQ-MAIN-CONTINUATION       PIC X(01).
      *           補助区分
                  05  MDCREQ-MAIN-ASSISTANCE         PIC X(01).
      *           本人家族区分
                  05  MDCREQ-MAIN-FAMILY             PIC X(01).
      *           被保険者名
                  05  MDCREQ-MAIN-POLICYHOLDER       PIC X(100).
      *           有効開始日
                  05  MDCREQ-MAIN-START-DATE         PIC X(10).
      *           有効終了日
                  05  MDCREQ-MAIN-END-DATE           PIC X(10).
      *           公費情報
                  05  MDCREQ-PUBLIC-INSURANCE-INF  OCCURS  4 TIMES.
      *             公費の種類
                    06  MDCREQ-INSURANCE-CLASS       PIC X(03).
      *             公費の種類名称
                    06  MDCREQ-INSURANCE-NAME        PIC X(20).
      *             負担者番号
                    06  MDCREQ-PROVIDER-NUMBER       PIC X(08).
      *             受給者番号
                    06  MDCREQ-RECIPIENT-NUMBER      PIC X(20).
      *             有効開始日
                    06  MDCREQ-INSURANCE-START-DATE  PIC X(10).
      *             有効終了日
                    06  MDCREQ-INSURANCE-END-DATE    PIC X(10).
      *         診療内容
                04  MDCREQ-MDC-INFO         OCCURS  40 TIMES.
      *           診療種別区分
                  05  MDCREQ-MDC-CLASS               PIC X(3).
      *??
                  05  MDCREQ-MDC-CLASS-NAME          PIC X(50).
      *           回数
                  05  MDCREQ-MDC-CLASS-NUMBER        PIC X(50).
      *           剤内容
                  05  MDCREQ-PRESCRIPTION-INFO     OCCURS  40 TIMES.
      *             診療コード
                    06  MDCREQ-PRSCRPT-CODE          PIC X(9).
      *             診療名称（コメント）
      *R02.4        06  MDCREQ-PRSCRPT-NAME          PIC X(80).
                    06  MDCREQ-PRSCRPT-NAME          PIC X(140).
      *             数量
                    06  MDCREQ-PRSCRPT-NUMBER        PIC X(40).
      *             処方せん一般名記載
                    06  MDCREQ-GENERIC-FLG           PIC X(03).
      *H29.4
      *             継続コメント指示区分
                    06  MDCREQ-CONTKBN               PIC X(01).
      *             内服１種類区分
                    06  MDCREQ-NAIFKBN               PIC X(01).
      *
      *         病名
                04  MDCREQ-DIAGNOSIS-INFO          OCCURS  50 TIMES.
                  05  MDCREQ-DIAGNOSIS-NUMBER        PIC X(50).
      *ver.4.7    入外区分
                  05  MDCREQ-DIAGNOSIS-INOUT         PIC X(01).
                  05  MDCREQ-DIAGNOSIS-NAME          PIC X(80).
                  05  MDCREQ-DIAGNOSIS-SINGLE      OCCURS  6  TIMES.
                    07 MDCREQ-DIAGNOSIS-SINGLE-NUMBER
                                                     PIC X(10). 
                    07 MDCREQ-DIAGNOSIS-SINGLE-NAME
                                                     PIC X(20). 
      *           補足コメント
                  05  MDCREQ-DIAGNOSIS-SUPPLEMENT.
                      07  MDCREQ-DIAGNOSIS-SCODE1        PIC X(10).
                      07  MDCREQ-DIAGNOSIS-SCODE2        PIC X(10).
                      07  MDCREQ-DIAGNOSIS-SCODE3        PIC X(10).
                      07  MDCREQ-DIAGNOSIS-SNAME         PIC X(40).
      *
                  05  MDCREQ-DIAGNOSIS-CATEGORY      PIC X(2).
                  05  MDCREQ-DIAGNOSIS-SUSPECTEDFLAG PIC X(2).
                  05  MDCREQ-DIAGNOSIS-START-DATE    PIC X(10).
                  05  MDCREQ-DIAGNOSIS-END-DATE      PIC X(10).
                  05  MDCREQ-DIAGNOSIS-OUTCOME       PIC X(2).
