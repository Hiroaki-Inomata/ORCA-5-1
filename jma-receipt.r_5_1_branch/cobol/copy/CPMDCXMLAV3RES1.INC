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
      *    ＡＰＩレスポンス入院診療行為入力情報 (一体化) COPY (CPMDCXMLAV3RES.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
        01  XML-MEDICALAV3RES1.
            03  MDCRES-CONTEXT             PIC S9(9)   BINARY.
            03  MDCRES-OBJECT              PIC X(36).
            03  MDCRES-MODE                PIC S9(9)   BINARY.
            03  MDCRES-RECORDNAME          PIC X(128).
            03  MDCRES-MEDICALRES.
      *       リクエスト番号
              05  MDCRES-REQUEST-NUMBER    PIC X(2).
      *       レスポンス番号
              05  MDCRES-RESPONSE-NUMBER   PIC X(2).
      *       UID（電子カルテ）
              05  MDCRES-KARTE-UID         PIC X(36).
      *       UID（オルカ）
              05  MDCRES-ORCA-UID          PIC X(36).
      *       実施日
              05  MDCRES-INFORMATION-DATE  PIC X(10).
      *       実施時間
              05  MDCRES-INFORMATION-TIME  PIC X(8).
      *       返却コード
              05  MDCRES-API-RESULT        PIC X(03).
      *       返却メッセージ
              05  MDCRES-API-RESULT-MSG    PIC X(100).
      *
              05  MDCRES-RESKEY            PIC X(100).
      *       診療日付
              05  MDCRES-PERFORM-DATE      PIC X(10).
              05  MDCRES-PERFORM-TIME      PIC X(8).
      *       中途データ存在区分
              05  MDCRES-TEMP-MEDICAL-FLAG PIC X(05).
      *       初診算定日なし区分
              05  MDCRES-DOCTORS-FEE-INITIAL   PIC X(05).
      *       入院日
              05  MDCRES-NYUIN-DATE        PIC  X(10).
      *       退院日
              05  MDCRES-TAIIN-DATE        PIC  X(10).
      *       同日再入院分（連番）
              05  MDCRES-DOUJITSUKBN       PIC X(1).
      *       診療科
              05  MDCRES-APPOINT-DEP-CODE  PIC X(2).
              05  MDCRES-APPOINT-DEP-NAME  PIC X(80).
      ***     ドクター（入院履歴のドクター１つ目）
      ***     05  MDCRES-APPOINT-DR-CODE   PIC X(5).
      ***     05  MDCRES-APPOINT-DR-NAME   PIC X(80).
      *
              05  MDCRES-PATIENT-INFOMATION.
      *         患者情報
                07  MDCRES-PATIENTID       PIC X(20).
                07  MDCRES-NAME            PIC X(100).
                07  MDCRES-KANANAME        PIC X(100).
                07  MDCRES-BIRTHDAY        PIC X(10).
                07  MDCRES-SEX             PIC X(1).
      *         保険情報
                07  MDCRES-INSURANCE-G.
      *             保険組合せ番号
                    09  MDCRES-COMBINATION-NUMBER     PIC X(04).
      *             保険の種類
                    09  MDCRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *             保険者番号
                    09  MDCRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *             保険の種類名称
                    09  MDCRES-MAIN-INSURANCE-NAME    PIC X(20).
      *             記号
                    09  MDCRES-MAIN-MARK              PIC X(80).
      *             番号
                    09  MDCRES-MAIN-NUMBER            PIC X(80).
      *             継続区分
                    09  MDCRES-MAIN-CONTINUATION      PIC X(01).
      *             補助区分
                    09  MDCRES-MAIN-ASSISTANCE        PIC X(01).
      *             補助区分
                    09  MDCRES-MAIN-ASSI-NAME         PIC X(20).
      *             本人家族区分
                    09  MDCRES-MAIN-FAMILY            PIC X(01).
      *             被保険者名
                    09  MDCRES-MAIN-POLICYHOLDER      PIC X(100).
      *             有効開始日
                    09  MDCRES-MAIN-START-DATE        PIC X(10).
      *             有効終了日
                    09  MDCRES-MAIN-END-DATE          PIC X(10).
      *             公費情報
                    09  MDCRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *               公費の種類
                      11  MDCRES-INSURANCE-CLASS         PIC X(03).
      *               公費の種類名称
                      11  MDCRES-INSURANCE-NAME          PIC X(20).
      *               負担者番号
                      11  MDCRES-PROVIDER-NUMBER         PIC X(08).
      *               受給者番号
                      11  MDCRES-RECIPIENT-NUMBER        PIC X(20).
      *               入院−負担率（割）
                      11  MDCRES-RATE-ADMISSION          PIC X(04).
      *               入院−固定額
                      11  MDCRES-MONEY-ADMISSION         PIC X(06).
      *               外来−負担率（割）
                      11  MDCRES-RATE-OUTPATIENT         PIC X(04).
      *               外来−固定額
                      11  MDCRES-MONEY-OUTPATIENT        PIC X(06).
      *               有効開始日
                      11  MDCRES-INSURANCE-START-DATE    PIC X(10).
      *               有効終了日
                      11  MDCRES-INSURANCE-END-DATE      PIC X(10).
      *
              05  MDCRES-MEDICAL-MSG1-G.
      *           エラーコード
                  07  MDCRES-MEDI-RESULT        PIC X(3).
      *           エラーメッセージ
                  07  MDCRES-MEDI-RESULT-MSG    PIC X(100).
      *           ワーニングメッセージ
                  07  MDCRES-MEDI-WARNING-TBL   OCCURS  50.
      *               ワーニングコード
                      09  MDCRES-MEDI-WARNING        PIC X(03).
      *               ワーニングメッセージ
                      09  MDCRES-MEDI-WARNING-MSG    PIC X(100).
      *               ワーニングコード
                      09  MDCRES-MEDI-CODE           PIC X(09).
      *
