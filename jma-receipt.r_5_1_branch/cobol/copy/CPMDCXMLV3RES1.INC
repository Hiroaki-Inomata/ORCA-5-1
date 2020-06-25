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
      *    ＡＰＩレスポンス診療行為入力情報 (一体化)   COPY (CPMDCXMLV3RES.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.00.00     NACL-多々納  20/03/XX  Ｒ０２年４月改正対応
      *****************************************************************
        01  XML-MEDICALV3RES1.
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
      *       複数返却あり
              05  MDCRES-MEDICAL-SELECT-FLAG   PIC X(05).
      *       最終来院日（退院日）
              05  MDCRES-LAST-DATE             PIC  X(10).
      *       診療科
              05  MDCRES-APPOINT-DEP-CODE  PIC X(2).
              05  MDCRES-APPOINT-DEP-NAME  PIC X(80).
      *       ドクター
              05  MDCRES-APPOINT-DR-CODE   PIC X(5).
              05  MDCRES-APPOINT-DR-NAME   PIC X(80).
      *
      *       送信設定診察料
              05  MDCRES-IN-MEDICAL-INF-G.
      *           診療種別区分
                  07  MDCRES-IN-MDC-CLASS        PIC X(3).
      *           診療種別名称
                  07  MDCRES-IN-MDC-CLASS-NAME   PIC X(40).
      *           診療コード
                  07  MDCRES-IN-PRSCRPT-CODE     PIC X(9).
      *           診療名称
                  07  MDCRES-IN-PRSCRPT-NAME     PIC X(140).
      *
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
      *        返却時間外区分
               05  MDCRES-OFFTIME               PIC X(01).
      *        返却診察料
               05  MDCRES-MEDICAL-INF-G.
                   07  MDCRES-MEDICAL-INF        OCCURS   10.
                     08  MDCRES-MEDICAL-INF2       OCCURS   10.
      *               診療種別区分
                      09  MDCRES-MDC-CLASS             PIC X(3).
      *               診療種別名称
                      09  MDCRES-MDC-CLASS-NAME        PIC X(40).
      *               診療回数（１のみ）
                      09  MDCRES-MDC-CLASS-NUMBER      PIC X(03).
      *               剤内容
                      09  MDCRES-PRESCRIPTION-INFO     OCCURS  10 TIMES.
      *                 診療コード
                        11  MDCRES-PRSCRPT-CODE          PIC X(9).
      *                 診療名称（コメント）
                        11  MDCRES-PRSCRPT-NAME          PIC X(140).
      *                 数量（１のみ）
                        11  MDCRES-PRSCRPT-NUMBER        PIC X(11).
      *                 他科受診の診療科
      ******            11  MDCRES-DEP-CODE              PIC X(02).
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
      *登録済み中途データ
      *        診療内容返却
               05  MDCRES-WKMEDICAL-INF-G.
                 07  MDCRES-WKMDC-INFO          OCCURS   110.
      *             診療区分
                    09  MDCRES-WKMDC-CLASS-KBN       PIC X(2).
      *             診療種別区分
                    09  MDCRES-WKMDC-CLASS           PIC X(3).
      *             診療種別名称
                    09  MDCRES-WKMDC-CLASS-NAME      PIC X(40).
      *             回数
                    09  MDCRES-WKMDC-CLASS-NUMBER    PIC X(03).
      *
                    09  MDCRES-WKPRSCRPT-INFO-G.
                      10  MDCRES-WKPRESCRIPTION-INFO    OCCURS   50.
      *                 診療コード
                        11  MDCRES-WKPRSCRPT-CODE          PIC X(9).
      *                 診療名称
                        11  MDCRES-WKPRSCRPT-NAME          PIC X(140).
      *                 数量
                        11  MDCRES-WKPRSCRPT-NUMBER        PIC X(11).
      *                 自費金額
                        11  MDCRES-WKPRSCRPT-MONEY         PIC X(07).
      *                 コメント埋め込み値
                        11  MDCRES-WKPRSCRPT-ATAI-G.
                            13  MDCRES-WKPRSCRPT-ATAI      PIC X(10)
                                                         OCCURS   5.
      *                 継続指示区分
                        11  MDCRES-WKPRSCRPT-CONTKBN       PIC X(01).
      *                 内服種類数指示区分
                        11  MDCRES-WKPRSCRPT-NAIFKBN       PIC X(01).
      *                 自動区分
                        11  MDCRES-WKPRSCRPT-AUTOKBN       PIC X(01).
      *
      *
