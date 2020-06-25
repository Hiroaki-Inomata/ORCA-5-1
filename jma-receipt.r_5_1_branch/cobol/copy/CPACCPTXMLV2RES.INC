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
      *    ＡＰＩレスポンス受付情報           COPY (CPACCPTXMLV2RES.INC)
      ******************************************************************
        01  XML-ACCEPTRES.
            02  ACCPTRES-CONTEXT          PIC S9(9)   BINARY.
            02  ACCPTRES-OBJECT           PIC X(36).
            02  ACCPTRES-MODE             PIC S9(9)   BINARY.
            02  ACCPTRES-RECORDNAME       PIC X(128).
            02  ACCPTRES-REGISTRES.
      *       実施日
              03  ACCPTRES-INFORMATION-DATE           PIC X(10).
      *       実施時間
              03  ACCPTRES-INFORMATION-TIME           PIC X(8).
      *       返却コード
              03  ACCPTRES-API-RESULT                 PIC X(2).
      *       返却メッセージ
              03  ACCPTRES-API-RESULT-MSG             PIC X(100).
      *       ワーニングメッセージ
              03  ACCPTRES-API-WARNING-MSG-G.
                  05  ACCPTRES-API-WARNING-MSG-TBL    OCCURS  5.
                      07  ACCPTRES-API-WARNING-MSG    PIC X(100).
      *
              03  ACCPTRES-RESKEY                     PIC X(100).
      *       受付日
              03  ACCPTRES-REGIST-DATE                PIC X(10).
      *       受付時間
              03  ACCPTRES-REGIST-TIME                PIC X(8).
      *       受付ＩＤ
              03  ACCPTRES-REGIST-ID                  PIC X(5).
      *       診療科
              03  ACCPTRES-REGIST-DEP-CODE            PIC X(2).
      *       診療科名称
              03  ACCPTRES-REGIST-DEP-NAME            PIC X(80).
      *       ドクターコード
              03  ACCPTRES-REGIST-DR-CODE             PIC X(5).
      *       ドクター氏名
              03  ACCPTRES-REGIST-DR-NAME             PIC X(80).
      *       診療内容区分
              03  ACCPTRES-REGIST-DEP-INFO            PIC X(2).
      *
              03  ACCPTRES-PATIENT-INFOMATION.
      *         患者番号
                04  ACCPTRES-PATIENTID                PIC X(20).
      *         患者氏名
                04  ACCPTRES-NAME                     PIC X(100).
      *         患者カナ氏名
                04  ACCPTRES-KANANAME                 PIC X(100).
      *         生年月日
                04  ACCPTRES-BIRTHDAY                 PIC X(10).
      *         性別
                04  ACCPTRES-SEX                      PIC X(1).
      *         自宅情報
                04  ACCPTRES-ADDRESS-INFO.
      *           郵便番号
                  05  ACCPTRES-HOME-ZIP-CODE          PIC X(7).
      *           住所
                  05  ACCPTRES-HOME-ADDRESSES         PIC X(100).
      *         保険情報
                04  ACCPTRES-INSURANCE-G.
                  05  ACCPTRES-INSURANCE-INFO          OCCURS 30 TIMES.
      *             保険組合せ番号
                    06  ACCPTRES-COMBINATION-NUMBER     PIC X(04).
      *H29.04
      *             非表示区分
                    06  ACCPTRES-COMBINATION-HYOJI      PIC X(01).
      *
      *             保険の種類
                    06  ACCPTRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *             保険者番号
                    06  ACCPTRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *             保険の種類名称
                    06  ACCPTRES-MAIN-INSURANCE-NAME    PIC X(20).
      *             記号
                    06  ACCPTRES-MAIN-MARK              PIC X(80).
      *             番号
                    06  ACCPTRES-MAIN-NUMBER            PIC X(80).
      *             継続区分
                    06  ACCPTRES-MAIN-CONTINUATION      PIC X(01).
      *             補助区分
                    06  ACCPTRES-MAIN-ASSISTANCE        PIC X(01).
      *             本人家族区分
                    06  ACCPTRES-MAIN-FAMILY            PIC X(01).
      *             被保険者名
                    06  ACCPTRES-MAIN-POLICYHOLDER      PIC X(100).
      *             有効開始日
                    06  ACCPTRES-MAIN-START-DATE        PIC X(10).
      *             有効終了日
                    06  ACCPTRES-MAIN-END-DATE          PIC X(10).
      *             公費情報
                    06  ACCPTRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *               公費の種類
                      07  ACCPTRES-INSURANCE-CLASS         PIC X(03).
      *               公費の種類名称
                      07  ACCPTRES-INSURANCE-NAME          PIC X(20).
      *               負担者番号
                      07  ACCPTRES-PROVIDER-NUMBER         PIC X(08).
      *               受給者番号
                      07  ACCPTRES-RECIPIENT-NUMBER        PIC X(20).
      *               入院−負担率（割）
                      07  ACCPTRES-RATE-ADMISSION          PIC X(04).
      *               入院−固定額
                      07  ACCPTRES-MONEY-ADMISSION         PIC X(06).
      *               外来−負担率（割）
                      07  ACCPTRES-RATE-OUTPATIENT         PIC X(04).
      *               外来−固定額
                      07  ACCPTRES-MONEY-OUTPATIENT        PIC X(06).
      *               有効開始日
                      07  ACCPTRES-INSURANCE-START-DATE    PIC X(10).
      *               有効終了日
                      07  ACCPTRES-INSURANCE-END-DATE      PIC X(10).
      *
