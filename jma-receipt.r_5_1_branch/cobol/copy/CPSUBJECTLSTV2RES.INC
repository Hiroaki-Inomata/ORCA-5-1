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
      *                                    COPY (CPSUBJECTLSTV2RES.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
        01  XML-SUBJECTLSTRES.
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
              05  SUBJETRES-API-RESULT          PIC X(3).
      *       返却メッセージ
              05  SUBJETRES-API-RESULT-MSG      PIC X(100).
      *
              05  SUBJETRES-RESKEY              PIC X(100).
      *       登録情報
              05  SUBJETRES-PATIENT-INFOMATION.
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
      *        診療年月
               05  SUBJETRES-PERFORM-DATE      PIC X(07).
      *        症状リスト
               05  SUBJETRES-SUBJECTIVES-AREA.
                   07  SUBJETRES-SUBJECTIVES-INFO     OCCURS   50.
      *               入外区分
                      09  SUBJETRES-OUTPATIENT-CLASS    PIC X(01).
      *               診療科
                      09  SUBJETRES-DEP-CODE            PIC X(02).
      *               診療科短縮名称
                      09  SUBJETRES-DEP-NAME            PIC X(80).
      *               保険組合せ情報
                      09  SUBJETRES-INSURANCE-INFO.
      *                   保険組合せ番号
                          11  SUBJETRES-COMBINATION-NUMBER  PIC X(04).
      *                   保険組合せ有効期間
                          11  SUBJETRES-COMB-START-DATE       PIC X(10).
                          11  SUBJETRES-COMB-END-DATE         PIC X(10).
      *
      *                   保険の種類
                          11  SUBJETRES-MAIN-INSURANCE-CLASS  PIC X(03).
      *                   保険の種類名称
                          11  SUBJETRES-MAIN-INSURANCE-NAME   PIC X(20).
      *                   保険者番号
                          11  SUBJETRES-MAIN-INSURANCE-NUMBER PIC X(08).
      *                   記号
                          11  SUBJETRES-MAIN-MARK             PIC X(80).
      *                   番号
                          11  SUBJETRES-MAIN-NUMBER           PIC X(80).
      *                   補助区分
                          11  SUBJETRES-MAIN-ASSISTANCE       PIC X(01).
      *                   補助区分名称
                          11  SUBJETRES-MAIN-ASSISTANCE-NAME  PIC X(30).
      *                   本人家族区分
                          11  SUBJETRES-MAIN-FAMILY           PIC X(01).
      *                   公費情報
                          11  SUBJETRES-PUBLIC-INSURANCE-INFO
                                                    OCCURS  4 TIMES.
      *                       公費の種類
                              13  SUBJETRES-INSURANCE-CLASS   PIC X(03).
      *                       公費の種類名称
                              13  SUBJETRES-INSURANCE-NAME    PIC X(20).
      *                       負担者番号
                              13  SUBJETRES-PROVIDER-NUMBER   PIC X(08).
      *                       受給者番号
                              13  SUBJETRES-RECIPIENT-NUMBER  PIC X(20).
      *                   保険組合せ状態
                          11  SUBJETRES-COMBINATION-MODE      PIC X(01).
      *               診療日
                      09  SUBJETRES-PERFORM-DAY            PIC X(02).
      *               症状詳記区分コード
                      09  SUBJETRES-SUBJECTIVES-CODE       PIC X(02).
      *               症状詳記区分名称
                      09  SUBJETRES-SUBJECTIVES-NAME       PIC X(50).
      *               連番
                      09  SUBJETRES-RENNUM                 PIC X(02).
      *        症状詳記
               05  SUBJETRES-DETAIL-AREA.
      *           症状詳記内容
                  07  SUBJETRES-DETAIL-RECORD          PIC X(4800).
      *
      *
