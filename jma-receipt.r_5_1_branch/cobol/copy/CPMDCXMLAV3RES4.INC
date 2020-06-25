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
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.00.00     NACL-多々納  20/03/XX  Ｒ０２年４月改正対応
      *****************************************************************
      ******************************************************************
      *    ＡＰＩレスポンス診療行為登録情報 (一体化)
      *                                      COPY (CPMDCXMLAV3RE4.INC)
      *         入院診療行為削除
      ******************************************************************
       01  XML-MEDICALAV3RES4.
            03  MDCRES-CONTEXT             PIC S9(9)   BINARY.
            03  MDCRES-OBJECT              PIC X(36).
            03  MDCRES-MODE                PIC S9(9)   BINARY.
            03  MDCRES-RECORDNAME          PIC X(128).
            03  MDCRES-MEDICALRES.
      *        リクエスト番号
               05  MDCRES-REQUEST-NUMBER    PIC X(2).
      *        レスポンス番号
               05  MDCRES-RESPONSE-NUMBER   PIC X(2).
      *        UID（電子カルテ）
               05  MDCRES-KARTE-UID         PIC X(36).
      *        UID（オルカ）
               05  MDCRES-ORCA-UID          PIC X(36).
      *        実施日
               05  MDCRES-INFORMATION-DATE  PIC X(10).
      *        実施時間
               05  MDCRES-INFORMATION-TIME  PIC X(8).
      *        返却コード
               05  MDCRES-API-RESULT        PIC X(3).
      *        返却メッセージ
               05  MDCRES-API-RESULT-MSG    PIC X(100).
      *
               05  MDCRES-RESKEY            PIC X(100).
      *        患者情報
               05  MDCRES-PATIENTID        PIC X(20).
               05  MDCRES-NAME             PIC X(100).
               05  MDCRES-KANANAME         PIC X(100).
               05  MDCRES-BIRTHDAY         PIC X(10).
               05  MDCRES-SEX              PIC X(1).
      *        選択返却あり
               05  MDCRES-MEDICAL-SELECT-FLAG   PIC X(05).
      *        診療日付
               05  MDCRES-PERFORM-DATE      PIC X(10).
      *        診療科
               05  MDCRES-APPOINT-DEP-CODE  PIC X(2).
               05  MDCRES-APPOINT-DEP-NAME  PIC X(80).
      *        連番(同日再入院分)
               05  MDCRES-NUMBER            PIC X(01).
      *        入院日
               05  MDCRES-NYUIN-DATE        PIC  X(10).
      *        退院日
               05  MDCRES-TAIIN-DATE        PIC  X(10).
      *        ドクター
      *        05  MDCRES-APPOINT-DR-CODE   PIC X(5).
      *        05  MDCRES-APPOINT-DR-NAME   PIC X(80).
      *        保険情報
               05  MDCRES-INSURANCE-G.
      *            保険組合せ番号
                   09  MDCRES-COMBINATION-NUMBER     PIC X(04).
      *            保険組合せ負担割合
                   09  MDCRES-COMBINATION-RATE       PIC X(03).
      *            保険の種類
                   09  MDCRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *            保険者番号
                   09  MDCRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *            保険の種類名称
                   09  MDCRES-MAIN-INSURANCE-NAME    PIC X(20).
      *            記号
                   09  MDCRES-MAIN-MARK              PIC X(80).
      *            番号
                   09  MDCRES-MAIN-NUMBER            PIC X(80).
      *            継続区分
                   09  MDCRES-MAIN-CONTINUATION      PIC X(01).
      *            補助区分
                   09  MDCRES-MAIN-ASSISTANCE        PIC X(01).
      *            補助区分名称
                   09  MDCRES-MAIN-ASSI-NAME         PIC X(20).
      *            本人家族区分
                   09  MDCRES-MAIN-FAMILY            PIC X(01).
      *            公費情報
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
      *               外来−負担率（割）
      *               11  MDCRES-RATE-OUTPATIENT         PIC X(04).
      *               外来−固定額
      *               11  MDCRES-MONEY-OUTPATIENT        PIC X(06).
      *
      *        診療内容返却
               05  MDCRES-MEDICAL-INF-G.
                 07  MDCRES-MDC-INFO          OCCURS   120.
      *             診療区分
                    09  MDCRES-MDC-CLASS-KBN         PIC X(2).
      *             診療種別区分
                    09  MDCRES-MDC-CLASS             PIC X(3).
      *             診療種別名称
                    09  MDCRES-MDC-CLASS-NAME        PIC X(40).
      *             回数
                    09  MDCRES-MDC-CLASS-NUMBER      PIC X(03).
      *             剤点数
                    09  MDCRES-MDC-POINT             PIC X(08).
      *             剤金額
                    09  MDCRES-MDC-MONEY             PIC X(08).
      *             包括区分（True：包括剤）
                    09  MDCRES-INCLUSION-CLASS       PIC X(05).
      *
      *             包括検査の検査数
                    09  MDCRES-HKTKNSA-CNT           PIC X(02).
      *
                    09  MDCRES-PRSCRPT-INFO-G.
                      10  MDCRES-PRESCRIPTION-INFO    OCCURS   50.
      *                 診療コード
                        11  MDCRES-PRSCRPT-CODE          PIC X(9).
      *                 診療名称
                        11  MDCRES-PRSCRPT-NAME          PIC X(140).
      *                 数量
                        11  MDCRES-PRSCRPT-NUMBER        PIC X(11).
      *                 単位コード
                        11  MDCRES-PRSCRPT-TANICD        PIC X(03).
      *                 単位名称
                        11  MDCRES-PRSCRPT-TANICD-NAME   PIC X(24).
      *                 自費金額
                        11  MDCRES-PRSCRPT-MONEY         PIC X(07).
      *                 コメント埋め込み値
                        11  MDCRES-PRSCRPT-ATAI-G.
                            13  MDCRES-PRSCRPT-ATAI      PIC X(10)
                                                         OCCURS   5.
      *                 フィルム分画数
                        11  MDCRES-PRSCRPT-FILMNUM       PIC X(03).
      *                 一般名記載区分（１：一般名、３：処方名（一般名）
      *                 11  MDCRES-PRSCRPT-GENERIC       PIC X(01).
      *                 一般名名称
      *                 11  MDCRES-PRSCRPT-GENNAME       PIC X(200).
      *                 継続指示区分
                        11  MDCRES-PRSCRPT-CONTKBN       PIC X(01).
      *                 内服種類数指示区分
                        11  MDCRES-PRSCRPT-NAIFKBN       PIC X(01).
      *
      *                 自動算定なし区分（在宅療養実績加算）
                        11  MDCRES-PRSCRPT-NOAUTO        PIC X(03).
      *
      *                 自動区分
                        11  MDCRES-PRSCRPT-AUTOKBN       PIC X(01).
      *
      *        診療内容エラー
               05  MDCRES-MEDICAL-MSG1-G.
      *           ワーニングメッセージ
                  07  MDCRES-MEDI-WARNING-TBL    OCCURS  40.
      *               ワーニングコード
                      09  MDCRES-MEDI-WARNING        PIC X(03).
      *               ワーニングメッセージ
                      09  MDCRES-MEDI-WARNING-MSG    PIC X(100).
      *
      *        確認メッセージコード（送信コード）
               05  MDCRES-SELECT-CODE         PIC X(04).
      *        確認メッセージ内容
               05  MDCRES-SELECT-MSG          PIC X(100).
      *
