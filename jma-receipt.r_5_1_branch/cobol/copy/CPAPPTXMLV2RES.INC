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
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ******************************************************************
      *    ＡＰＩレスポンス予約情報           COPY (CPACCPTXMLV2RES.INC)
      ******************************************************************
        01  XML-APPOINTRES.
            02  APNTRES-CONTEXT            PIC S9(9)   BINARY.
            02  APNTRES-OBJECT             PIC X(36).
            02  APNTRES-MODE               PIC S9(9)   BINARY.
            02  APNTRES-RECORDNAME         PIC X(128).
            02  APNTRES-APPOINTRES.
      *       実施日
              03  APNTRES-INFORMATION-DATE    PIC X(10).
      *       実施時間
              03  APNTRES-INFORMATION-TIME    PIC X(8).
      *       返却コード
              03  APNTRES-API-RESULT          PIC X(2).
      *       返却メッセージ
              03  APNTRES-API-RESULT-MSG      PIC X(100).
      *       ワーニングメッセージ
              03  APNTRES-API-WARNING-MSG-G.
                  05  APNTRES-API-WARNING-MSG-TBL     OCCURS   5.
                      07  APNTRES-API-WARNING-MSG     PIC X(100).
      *
              03  APNTRES-RESKEY              PIC X(100).
      *       予約日
              03  APNTRES-APPOINT-DATE        PIC X(10).
      *       予約時間
              03  APNTRES-APPOINT-TIME        PIC X(8).
      *       予約ＩＤ
              03  APNTRES-APPOINT-ID          PIC X(5).
      *       診療科
              03  APNTRES-APPOINT-DEP-CODE    PIC X(2).
      *       診療科名称
              03  APNTRES-APPOINT-DEP-NAME    PIC X(80).
      *       ドクターコード
              03  APNTRES-APPOINT-DR-CODE     PIC X(5).
      *       ドクター氏名
              03  APNTRES-APPOINT-DR-NAME     PIC X(80).
      *       診療内容区分
              03  APNTRES-APPOINT-DEP-INFO    PIC X(2).
      *       予約区分
              03  APNTRES-APPOINT-INFO        PIC X(2).
      *       予約メモ
              03  APNTRES-APPOINT-MEMO        PIC X(100).
      *       診療内容区分
              03  APNTRES-PATIENT-INFOMATION.
      *         患者番号
                04  APNTRES-PATIENTID         PIC X(20).
      *         患者氏名
                04  APNTRES-NAME              PIC X(100).
      *         患者カナ氏名
                04  APNTRES-KANANAME          PIC X(100).
      *         誕生日
                04  APNTRES-BIRTHDAY          PIC X(10).
      *         性別
                04  APNTRES-SEX               PIC X(1).
      *         自宅情報
                04  APNTRES-ADDRESS-INFO.
      *           郵便番号
                  05  APNTRES-HOME-ZIP-CODE   PIC X(7).
      *           住所
                  05  APNTRES-HOME-ADDRESSES  PIC X(100).
      *         保険情報
                04  APNTRES-INSURANCE-G.
                  05  APNTRES-INSURANCE-INFO          OCCURS 20 TIMES.
      *             保険の種類
                    06  APNTRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *             保険者番号
                    06  APNTRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *             保険の種類名称
                    06  APNTRES-MAIN-INSURANCE-NAME    PIC X(20).
      *             記号
                    06  APNTRES-MAIN-MARK              PIC X(80).
      *             番号
                    06  APNTRES-MAIN-NUMBER            PIC X(80).
      *             継続区分
                    06  APNTRES-MAIN-CONTINUATION      PIC X(01).
      *             補助区分
                    06  APNTRES-MAIN-ASSISTANCE        PIC X(01).
      *             本人家族区分
                    06  APNTRES-MAIN-FAMILY            PIC X(01).
      *             被保険者名
                    06  APNTRES-MAIN-POLICYHOLDER      PIC X(100).
      *             有効開始日
                    06  APNTRES-MAIN-START-DATE        PIC X(10).
      *             有効終了日
                    06  APNTRES-MAIN-END-DATE          PIC X(10).
      *             公費情報
                    06  APNTRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *               公費の種類
                      07  APNTRES-INSURANCE-CLASS         PIC X(03).
      *               公費の種類名称
                      07  APNTRES-INSURANCE-NAME          PIC X(20).
      *               負担者番号
                      07  APNTRES-PROVIDER-NUMBER         PIC X(08).
      *               受給者番号
                      07  APNTRES-RECIPIENT-NUMBER        PIC X(20).
      *               入院−負担率（割）
                      07  APNTRES-RATE-ADMISSION          PIC X(04).
      *               入院−固定額
                      07  APNTRES-MONEY-ADMISSION         PIC X(06).
      *               外来−負担率（割）
                      07  APNTRES-RATE-OUTPATIENT         PIC X(04).
      *               外来−固定額
                      07  APNTRES-MONEY-OUTPATIENT        PIC X(06).
      *               有効開始日
                      07  APNTRES-INSURANCE-START-DATE    PIC X(10).
      *               有効終了日
                      07  APNTRES-INSURANCE-END-DATE      PIC X(10).
      *
