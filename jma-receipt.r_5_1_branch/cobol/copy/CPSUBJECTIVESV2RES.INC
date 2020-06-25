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
      *    ＡＰＩレスポンス症状詳記登録情報取得(xml2)
      *                                    COPY (CPSUBJECTIVEV2RES.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
        01  XML-SUBJECTIVESRES.
            03  SUBJETRES-CONTEXT           PIC S9(9)   BINARY.
            03  SUBJETRES-OBJECT            PIC X(36).
            03  SUBJETRES-MODE              PIC S9(9)   BINARY.
            03  SUBJETRES-RECORDNAME        PIC X(128).
            03  SUBJETRES-MEDICALRES.
      *       実施日
              05  SUBJETRES-INFORMATION-DATE    PIC X(10).
      *       実施時間
              05  SUBJETRES-INFORMATION-TIME    PIC X(8).
      *       返却コード
              05  SUBJETRES-API-RESULT          PIC X(2).
      *       返却メッセージ
              05  SUBJETRES-API-RESULT-MSG      PIC X(100).
      *       返却ワーニングメッセージ
              05  SUBJETRES-API-WARNING-MSG-G.
                  07  SUBJETRES-API-WARNING-MSG      PIC X(100)
                                                OCCURS   5.
      *
              05  SUBJETRES-RESKEY              PIC X(100).
      *       登録情報
              05  SUBJETRES-PATIENT-INFOMATION.
      *           入外区分
                  07  SUBJETRES-OUTPATIENT-CLASS  PIC X(01).
      *           患者番号
                  07  SUBJETRES-PATIENTID         PIC X(20).
      *           患者氏名
                  07  SUBJETRES-NAME              PIC X(100).
      *           患者カナ氏名
                  07  SUBJETRES-KANANAME          PIC X(100).
      *           誕生日
                  07  SUBJETRES-BIRTHDAY          PIC X(10).
      *           性別
                  07  SUBJETRES-SEX               PIC X(1).
      *           診療年月
                  07  SUBJETRES-PERFORM-DATE      PIC X(10).
      *           診療科
                  07  SUBJETRES-DEP-CODE            PIC X(02).
      *           診療科短縮名称
                  07  SUBJETRES-DEP-NAME            PIC X(80).
      *           保険組合せ番号
                  07  SUBJETRES-COMBINATION-NUMBER  PIC X(04).
      *           保険組合せ情報
                  07  SUBJETRES-INSURANCE-INFO.
      *             保険の種類
                    09  SUBJETRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *             保険の種類名称
                    09  SUBJETRES-MAIN-INSURANCE-NAME    PIC X(20).
      *             保険者番号
                    09  SUBJETRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *             記号
                    09  SUBJETRES-MAIN-MARK              PIC X(80).
      *             番号
                    09  SUBJETRES-MAIN-NUMBER            PIC X(80).
      *             継続区分
                    09  SUBJETRES-MAIN-CONTINUATION      PIC X(01).
      *             補助区分
                    09  SUBJETRES-MAIN-ASSISTANCE        PIC X(01).
      *             補助区分名称
                    09  SUBJETRES-MAIN-ASSISTANCE-NAME   PIC X(30).
      *             本人家族区分
                    09  SUBJETRES-MAIN-FAMILY            PIC X(01).
      *             被保険者名
                    09  SUBJETRES-MAIN-POLICYHOLDER      PIC X(100).
      *             有効開始日
                    09  SUBJETRES-MAIN-START-DATE        PIC X(10).
      *             有効終了日
                    09  SUBJETRES-MAIN-END-DATE          PIC X(10).
      *             公費情報
                    09  SUBJETRES-PUBLIC-INSURANCE-INFO
                                              OCCURS  4 TIMES.
      *               公費の種類
                      11  SUBJETRES-INSURANCE-CLASS         PIC X(03).
      *               公費の種類名称
                      11  SUBJETRES-INSURANCE-NAME          PIC X(20).
      *               負担者番号
                      11  SUBJETRES-PROVIDER-NUMBER         PIC X(08).
      *               受給者番号
                      11  SUBJETRES-RECIPIENT-NUMBER        PIC X(20).
      *               入院−負担率（割）
                      11  SUBJETRES-RATE-ADMISSION          PIC X(04).
      *               入院−固定額
                      11  SUBJETRES-MONEY-ADMISSION         PIC X(06).
      *               外来−負担率（割）
                      11  SUBJETRES-RATE-OUTPATIENT         PIC X(04).
      *               外来−固定額
                      11  SUBJETRES-MONEY-OUTPATIENT        PIC X(06).
      *               有効開始日
                      11  SUBJETRES-INSURANCE-START-DATE    PIC X(10).
      *               有効終了日
                      11  SUBJETRES-INSURANCE-END-DATE      PIC X(10).
      *
      *           連番
                  07  SUBJETRES-NUMBER                 PIC X(02).
      *           症状詳記区分コード
                  07  SUBJETRES-SUBJECTIVES-CODE       PIC X(02).
      *           症状詳記区分名称
                  07  SUBJETRES-SUBJECTIVES-NAME       PIC X(50).
      *           症状詳記
                  07  SUBJETRES-DETAIL-RECORD          PIC X(4800).
      *
      *
