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
      *    ＡＰＩ症状詳記登録情報取得 (xml2)
      *                                   COPY (CPSUBJECTIVEV2REQ.INC)
      ******************************************************************
        01  XML-SUBJECTIVESREQ.
            03  SUBJETREQ-CONTEXT           PIC S9(9)   BINARY.
            03  SUBJETREQ-OBJECT            PIC X(36).
            03  SUBJETREQ-MODE              PIC S9(9)   BINARY.
            03  SUBJETREQ-RECORDNAME        PIC X(128).
            03  SUBJETREQ-MEDICALREQ.
      *       入外区分
              05  SUBJETREQ-OUTPATIENT-CLASS  PIC X(1).
      *       患者番号
              05  SUBJETREQ-PATIENTID         PIC X(20).
      *       診療年月
              05  SUBJETREQ-PERFORM-DATE      PIC X(10).
      *       診療科
              05  SUBJETREQ-DEP-CODE          PIC X(02).
      *       保険組合せ番号
              05  SUBJETREQ-COMBINATION-NUMBER     PIC X(04).
      *       保険組合せ情報
              05  SUBJETREQ-INSURANCE-INFO.
      *           保険の種類
                  07  SUBJETREQ-MAIN-INSURANCE-CLASS    PIC X(03).
      *           保険の種類
                  07  SUBJETREQ-MAIN-INSURANCE-NAME     PIC X(20).
      *           保険者番号
                  07  SUBJETREQ-MAIN-INSURANCE-NUMBER   PIC X(08).
      *           記号
                  07  SUBJETREQ-MAIN-MARK               PIC X(80).
      *           番号
                  07  SUBJETREQ-MAIN-NUMBER             PIC X(80).
      *           継続区分
                  07  SUBJETREQ-MAIN-CONTINUATION      PIC X(01).
      *           補助区分
                  07  SUBJETREQ-MAIN-ASSISTANCE        PIC X(01).
      *           本人家族区分
                  07  SUBJETREQ-MAIN-FAMILY            PIC X(01).
      *           被保険者名
                  07  SUBJETREQ-MAIN-POLICYHOLDER      PIC X(100).
      *           有効開始日
                  07  SUBJETREQ-MAIN-START-DATE        PIC X(10).
      *           有効終了日
                  07  SUBJETREQ-MAIN-END-DATE          PIC X(10).
      *           公費情報
                  07  SUBJETREQ-PUBLIC-INSURANCE-INF  OCCURS  4 TIMES.
      *             公費の種類
                    09  SUBJETREQ-INSURANCE-CLASS       PIC X(03).
      *             公費の種類
                    09  SUBJETREQ-INSURANCE-NAME        PIC X(20).
      *             負担者番号
                    09  SUBJETREQ-PROVIDER-NUMBER       PIC X(08).
      *             受給者番号
                    09  SUBJETREQ-RECIPIENT-NUMBER      PIC X(20).
      *             有効開始日
                    09  SUBJETREQ-INSURANCE-START-DATE  PIC X(10).
      *             有効終了日
                    09  SUBJETREQ-INSURANCE-END-DATE    PIC X(10).
      *       症状詳記区分コード
              05  SUBJETREQ-SUBJECTIVES-CODE       PIC X(02).
      *       症状詳記
              05  SUBJETREQ-DETAIL-RECORD          PIC X(4800).
      *
