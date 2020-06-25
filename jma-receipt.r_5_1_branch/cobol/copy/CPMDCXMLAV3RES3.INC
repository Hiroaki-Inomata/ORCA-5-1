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
      *****************************************************************
      *****************************************************************
      *    ＡＰＩレスポンス診療行為登録情報 (一体化)
      *                                      COPY (CPMDCXMLAV3RE3.INC)
      *         入院診療行為内容
      *****************************************************************
       01  XML-MEDICALAV3RES3.
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
      *        診療日付
               05  MDCRES-PERFORM-DATE      PIC X(10).
               05  MDCRES-PERFORM-TIME      PIC X(8).
      *        入院日
               05  MDCRES-NYUIN-DATE        PIC  X(10).
      *        退院日
               05  MDCRES-TAIIN-DATE        PIC  X(10).
      *        同日再入院分
               05  MDCRES-DOUJITSUKBN       PIC X(1).
      *        訂正診療科・保険組合せ番号
               05  MDCRES-MOD-DEP-CODE       PIC X(2).
               05  MDCRES-MOD-COMBINATION    PIC X(4).
      *
      *        診療科
               05  MDCRES-APPOINT-DEP-CODE  PIC X(2).
               05  MDCRES-APPOINT-DEP-NAME  PIC X(80).
      *        ドクター（主治医）
               05  MDCRES-APPOINT-DR-CODE   PIC X(5).
               05  MDCRES-APPOINT-DR-NAME   PIC X(80).
      *        患者情報
               05  MDCRES-PATIENTID        PIC X(20).
               05  MDCRES-NAME             PIC X(100).
               05  MDCRES-KANANAME         PIC X(100).
               05  MDCRES-BIRTHDAY         PIC X(10).
               05  MDCRES-SEX              PIC X(1).
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
      *             算定日
                    09  MDCRES-MDC-DAY-G.
                        11  MDCRES-MDC-DAY           PIC X(01)
                                                     OCCURS   31.
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
      *             剤削除可能連番
                    09  MDCRES-DEL-NUMBER            PIC X(02).
      *
      *             剤削除区分
                    09  MDCRES-DEL-INF               PIC X(06).
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
                        11  MDCRES-PRSCRPT-GENERIC       PIC X(01).
      *                 一般名名称
                        11  MDCRES-PRSCRPT-GENNAME       PIC X(200).
      *                 継続指示区分
                        11  MDCRES-PRSCRPT-CONTKBN       PIC X(01).
      *                 内服種類数指示区分
                        11  MDCRES-PRSCRPT-NAIFKBN       PIC X(01).
      *                 自動算定なし区分（在宅療養実績加算）
                        11  MDCRES-PRSCRPT-NOAUTO        PIC X(03).
      *                 自動区分
                        11  MDCRES-PRSCRPT-AUTOKBN       PIC X(01).
      *
      *         診療内容エラー
                05  MDCRES-MEDICAL-MSG1-G.
      *           エラーコード
                  07  MDCRES-MEDI-RESULT         PIC X(04).
      *           エラーメッセージ
                  07  MDCRES-MEDI-RESULT-MSG     PIC X(100).
      *
      *        入院調剤料返却（登録後）
               05  MDCRES-MEDICAL-CHOZAIINF-G.
                   07  MDCRES-MDC-CHOZA-INF        OCCURS   10.
      *             診療科
                    09  MDCRES-CHZ-DEP-CODE        PIC X(2).
      *             保険組合せ番号
                    09  MDCRES-CHZ-COMBINATION     PIC X(4).
      *             診療区分
                    09  MDCRES-CHZ-CLASS-KBN         PIC X(2).
      *             診療種別区分
                    09  MDCRES-CHZ-CLASS             PIC X(3).
      *             診療種別名称
                    09  MDCRES-CHZ-CLASS-NAME        PIC X(40).
      *             剤点数
                    09  MDCRES-CHZ-POINT             PIC X(08).
      *             算定日
                    09  MDCRES-CHZ-DAY-G.
                        11  MDCRES-CHZ-DAY           PIC X(01)
                                                     OCCURS   31.
      *             包括区分（True：包括剤）
                    09  MDCRES-CHZ-INCLUSION-CLASS       PIC X(05).
      *
                    09  MDCRES-CHZ-INFO-G.
                        11  MDCRES-CHZ-INFO    OCCURS   5.
      *                 診療コード
                        13  MDCRES-CHZ-CODE        PIC X(9).
      *                 診療名称
                        13  MDCRES-CHZ-NAME        PIC X(140).
      *                 自動区分
                        13  MDCRES-CHZ-AUTOKBN     PIC X(01).
      *
      *   帳票印刷指示区分
              05  MDCRES-PRINT-AREA.
      *           院内処方せん印刷
                  07  MDCRES-PRINT-SYOHOU-G.
                      11  MDCRES-PRINT-SYOHOU      PIC X(01).
                      11  MDCRES-PRINT-SYOHOU-MEI  PIC X(20).
      *           注射処方せん印刷
                  07  MDCRES-PRINT-CHUUSHA-G.
                      11  MDCRES-PRINT-CHUUSHA     PIC X(01).
                      11  MDCRES-PRINT-CHUUSHA-MEI PIC X(20).
      *           指示せん印刷
                  07  MDCRES-PRINT-SIJISEN-G.
                      11  MDCRES-PRINT-SIJISEN     PIC X(01).
                      11  MDCRES-PRINT-SIJISEN-MEI PIC X(20).
      *           発行医師
                  07  MDCRES-PRINT-DRCD         PIC X(5).
                  07  MDCRES-PRINT-DR-NAME      PIC X(80).
      *
      *        DATA-ID 2019/09/19 Added
               05  MDCRES-DATA-ID               PIC X(37).
