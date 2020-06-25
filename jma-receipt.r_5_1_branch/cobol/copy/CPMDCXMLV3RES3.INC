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
      * 05.00.00     NACL-多々納  19/01/XX  複数科保険対応
      *****************************************************************
      ******************************************************************
      *    ＡＰＩレスポンス診療行為登録情報 (一体化)
      *                                      COPY (CPMDCXMLV3RE3.INC)
      *         診療行為内容
      ******************************************************************
       01  XML-MEDICALV3RES3.
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
      *        基準日付（伝票発行日）
               05  MDCRES-BASE-DATE         PIC X(10).
      *        診療科
               05  MDCRES-APPOINT-DEP-CODE  PIC X(2).
               05  MDCRES-APPOINT-DEP-NAME  PIC X(80).
      *        ドクター
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
      *               外来−負担率（割）
                      11  MDCRES-RATE-OUTPATIENT         PIC X(04).
      *               外来−固定額
                      11  MDCRES-MONEY-OUTPATIENT        PIC X(06).
      *
      *        診療内容返却
               05  MDCRES-MEDICAL-INF-G.
                 07  MDCRES-MDC-INFO          OCCURS   120.
      *!!!
      *2019.1
      *             保険・診療科切替
                    09  MDCRES-CHENGE-REC.
      *                 データ区分（DEP：複数診療科、COM:保険組合せ）
                        11  MDCRES-MDC-DATA-KBN            PIC X(03).
      *                 保険組合せ番号
                        11  MDCRES-MDC-COMBINATION-NUMBER  PIC X(04).
      *                 保険の種類名称
                        11  MDCRES-MDC-MAIN-INS-NAME       PIC X(20).
                        11  MDCRES-MDC-PUBLIC-INS-INFO.
      *                 公費の種類名称
                            13  MDCRES-MDC-INSURANCE-NAME  PIC X(20)
                                                     OCCURS  4 TIMES.
      *                 診療科
                        11  MDCRES-MDC-DEP-CODE            PIC X(2).
                        11  MDCRES-MDC-DEP-CODE-NAME       PIC X(20).
      *                 ドクター
                        11  MDCRES-MDC-DR-CODE             PIC X(5).
                        11  MDCRES-MDC-DR-CODE-NAME        PIC X(80).
      *!!!
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
      *        伝票番号（主たる）
               05  MDCRES-INVOICD-NUMBER     PIC  X(07).
      *
      *2019.1
      *        送信収納情報返却
               05  MDCRES-IN-CD-INF.
                   07  MDCRES-IN-CD-CHNGE-INF      OCCURS   15.
      *                保険組合せ番号
                       09  MDCRES-IN-CD-COMB-NUMBER    PIC X(04).
      *                診療科
                       09  MDCRES-IN-CD-DEP-CODE       PIC X(02).
      *
      *                収納入金額
                       09  MDCRES-IN-CD-IC-MONEY       PIC  X(10).
      *                調整金額１
                       09  MDCRES-IN-CD-AD-MONEY1      PIC  X(10).
      *                調整金額２
                       09  MDCRES-IN-CD-AD-MONEY2      PIC  X(10).
      *            調整金額１（単独）
                   07  MDCRES-IN-AD-MONEY1         PIC  X(10).
      *            調整金額２（単独）
                   07  MDCRES-IN-AD-MONEY2         PIC  X(10).
      *            入金額
                   07  MDCRES-IN-IC-MONEY          PIC  X(10).
      *            返金額
                   07  MDCRES-IN-RE-MONEY          PIC  X(10).
      *
               05  MDCRES-IC-INF.
      *            入金方法区分
                   07  MDCRES-IC-CODE           PIC  X(02).
                   07  MDCRES-IC-CODE-NAME      PIC  X(20).
      *!!
      *H29.7
      *            入金取り扱い区分
                   07  MDCRES-IC-REQUEST           PIC X(01).
                   07  MDCRES-IC-REQUEST-NAME      PIC X(40).
      *
      *            今回合計請求額  （調整金・充当含む。訂正は差額）
                   07  MDCRES-TOTAL-AC-MONEY      PIC  X(10).
      *
      *            今回入金額（合計）
                   07  MDCRES-TOTAL-IC-MONEY      PIC  X(10).
      *            今回返金額（合計）
                   07  MDCRES-TOTAL-RE-MONEY      PIC  X(10).
      *
      *            今回入金額上限額・今回返金額（新規のみ）
                   07  MDCRES-MAX-IC-MONEY        PIC X(10).
      *
      *
      *        収納情報返却
               05  MDCRES-CD-INF.
      *!!!
      *2019.1
                   07  MDCRES-CD-CHNGE-INF.
      *                保険組合せ番号
                       09  MDCRES-CD-COMBINATION-NUMBER    PIC X(04).
                       09  MDCRES-CD-COMBINATION-NAME      PIC X(40).
      *                診療科
                       09  MDCRES-CD-DEP-CODE              PIC X(02).
                       09  MDCRES-CD-DEP-CODE-NAME         PIC X(20).
      *!!!
      *            今回入金金額
                   07  MDCRES-IC-MONEY          PIC  X(10).
      *
      *            調整金額１
                   07  MDCRES-AD-MONEY1         PIC  X(10).
      *            調整金額２
                   07  MDCRES-AD-MONEY2         PIC  X(10).
      *            請求金額（調整金を含まない）
                   07  MDCRES-AC-MONEY          PIC  X(10).
      *
      *            今回返金額（訂正時以外は充当）
                   07  MDCRES-RE-MONEY          PIC  X(10).
      *
      *            収納情報返却（請求額内訳）
                   07  MDCRES-SYUNOU-AREA.
      *            保険適用金額
                       09  MDCRES-AI-MONEY         PIC  X(10).
      *             自費金額
                       09  MDCRES-OE-MONEY         PIC  X(10).
      *             薬剤一部負担金
                       09  MDCRES-DG-SMONEY        PIC  X(10).
      *             老人一部負担金
                       09  MDCRES-OM-SMONEY           PIC  X(10).
      *             公費一部負担金
                       09  MDCRES-PI-SMONEY           PIC  X(10).
      *             労災金額合計
                       09  MDCRES-LSI-TOTALMONEY      PIC  X(10).
      *
      *         請求点数情報
                    07  MDCRES-AC-POINT-INF.
      *             保険合計点数
                        09  MDCRES-AC-TOTAL-POINT         PIC  X(10).
      *             保険点数
                        09  MDCRES-AC-POINT-G.
                            11  MDCRES-AC-POINT-TBL      OCCURS   16.
                                13  MDCRES-AC-POINT-NAME   PIC  X(30).
      *                         保険点数
                                13  MDCRES-AC-POINT        PIC  X(10).
      *                         適用外金額
                                13  MDCRES-AC-TGMONEY      PIC  X(10).
      *              自費明細
                     07  MDCRES-JIHI-G.
                         09  MDCRES-JIHI-TBL      OCCURS   10.
                             11  MDCRES-JIHI-NAME       PIC  X(20).
      *                      自費金額
                             11  MDCRES-JIHI-MONEY      PIC  X(10).
      *                      自費金額（消費税あり）
                             11  MDCRES-JIHI-MONEY-TAX  PIC  X(10).
      *                  自費消費税（再掲）
                         09  MDCRES-JIHI-TAX    PIC X(10).
      *
      *            訂正前収納情報返却
                   07  MDCRES-OLD-CD-INF.
      *                前回入金金額
                       09  MDCRES-OLD-IC-MONEY         PIC  X(10).
      *                前回請求金額
                       09  MDCRES-OLD-AC-MONEY         PIC  X(10).
      *                前回調整金額１
                       09  MDCRES-OLD-AD-MONEY1        PIC  X(10).
      *                前回調整金額２
                       09  MDCRES-OLD-AD-MONEY2        PIC  X(10).
      *
      *H29.6
      *        減免情報
               05  MDCRES-DISCOUNT-INF.
                   07  MDCRES-DISCOUNT-NAME        PIC X(12).
               05  MDCRES-MAE-CD-INF.
      *            前回までの未収額
                   07  MDCRES-MAE-MISYU             PIC X(10).
      *            前回までの超過額
                   07  MDCRES-MAE-CHOUKA            PIC X(10).
      *
      *2019.1
      * 複数科保険
      *        収納情報返却（明細）
               05  MDCRES-SUB-CD-INF.
                 06  MDCRES-SUB-CD-INF-OCC          OCCURS   15.
                   07  MDCRES-SUB-CHNGE-INF.
      *                保険組合せ番号
                       09  MDCRES-SUB-COMBINATION-NUMBER    PIC X(04).
      *                診療科
                       09  MDCRES-SUB-DEP-CODE         PIC X(02).
                       09  MDCRES-SUB-DEP-CODE-NAME    PIC X(20).
      *                ドクター
                       09  MDCRES-SUB-DR-CODE          PIC X(5).
      *                伝票番号
                       09  MDCRES-SUB-INVOICD-NUMBER   PIC  X(07).
      *            今回入金金額
                   07  MDCRES-SUB-IC-MONEY          PIC  X(10).
      *
      *            調整金額１
                   07  MDCRES-SUB-AD-MONEY1         PIC  X(10).
      *            調整金額２
                   07  MDCRES-SUB-AD-MONEY2         PIC  X(10).
      *            請求金額（調整金を含まない）
                   07  MDCRES-SUB-AC-MONEY          PIC  X(10).
      *
      *            今回返金額（訂正時以外は充当）
                   07  MDCRES-SUB-RE-MONEY          PIC  X(10).
      *
      *            収納情報返却（請求額内訳）
                   07  MDCRES-SUB-SYUNOU-AREA.
      *            保険適用金額
                       09  MDCRES-SUB-AI-MONEY         PIC  X(10).
      *             自費金額
                       09  MDCRES-SUB-OE-MONEY         PIC  X(10).
      *             薬剤一部負担金
                       09  MDCRES-SUB-DG-SMONEY        PIC  X(10).
      *             老人一部負担金
                       09  MDCRES-SUB-OM-SMONEY        PIC  X(10).
      *             公費一部負担金
                       09  MDCRES-SUB-PI-SMONEY        PIC  X(10).
      *             労災金額合計
                       09  MDCRES-SUB-LSI-TOTALMONEY   PIC  X(10).
      *
      *         請求点数情報
                    07  MDCRES-SUB-AC-POINT-INF.
      *             保険合計点数
                        09  MDCRES-SUB-AC-TOTAL-POINT      PIC  X(10).
      *             保険点数
                        09  MDCRES-SUB-AC-POINT-G.
                            11  MDCRES-SUB-AC-POINT-TBL    OCCURS   16.
                                13  MDCRES-SUB-AC-POINT-NAME
                                                           PIC  X(30).
      *                         保険点数
                                13  MDCRES-SUB-AC-POINT    PIC  X(10).
      *                         適用外金額
                                13  MDCRES-SUB-AC-TGMONEY  PIC  X(10).
      *              自費明細
                     07  MDCRES-SUB-JIHI-G.
                         09  MDCRES-SUB-JIHI-TBL      OCCURS   10.
                             11  MDCRES-SUB-JIHI-NAME       PIC  X(20).
      *                      自費金額
                             11  MDCRES-SUB-JIHI-MONEY      PIC  X(10).
      *                      自費金額（消費税あり）
                             11  MDCRES-SUB-JIHI-MONEY-TAX  PIC  X(10).
      *                  自費消費税（再掲）
                         09  MDCRES-SUB-JIHI-TAX    PIC X(10).
      *
      *            訂正前収納情報返却
                   07  MDCRES-SUB-OLD-CD-INF.
      *                前回入金金額
                       09  MDCRES-SUB-OLD-IC-MONEY         PIC  X(10).
      *                前回請求金額
                       09  MDCRES-SUB-OLD-AC-MONEY         PIC  X(10).
      *                前回調整金額１
                       09  MDCRES-SUB-OLD-AD-MONEY1        PIC  X(10).
      *                前回調整金額２
                       09  MDCRES-SUB-OLD-AD-MONEY2        PIC  X(10).
      *
      *            まとめ入力差額 （診療科＝００以外）
                   07  MDCRES-SUB-GRP-SGKMONEY-INF.
                       09  MDCRES-SUB-GRP-SGKMONEY         PIC X(10).
      *!!!!
      *2019.1
              05  MDCRES-PRINT-INIT-AREA.
      *           請求書兼領収書・診療費明細書　発行区分
                  07  MDCRES-HAKKOU-KBN        PIC X(01).
                  07  MDCRES-HAKKOU-KBN-MEI    PIC X(30).
      *H29.6
      *   帳票印刷指示区分
              05  MDCRES-PRINT-AREA.
      *           処方せん印刷
                  07  MDCRES-PRINT-SHOHOU      PIC X(01).
                  07  MDCRES-PRINT-SHOHOU-MEI  PIC X(30).
      *           請求書兼領収書印刷
                  07  MDCRES-PRINT-SEIKYU      PIC X(01).
                  07  MDCRES-PRINT-SEIKYU-MEI  PIC X(30).
      *           診療費明細書印刷
                  07  MDCRES-PRINT-MEISAI      PIC X(01).
                  07  MDCRES-PRINT-MEISAI-MEI  PIC X(30).
      *           薬剤情報印刷
                  07  MDCRES-PRINT-YAKUJYO     PIC X(01).
                  07  MDCRES-PRINT-YAKUJYO-MEI PIC X(30).
      *           お薬手帳印刷
                  07  MDCRES-PRINT-TECHOU      PIC X(01).
                  07  MDCRES-PRINT-TECHOU-MEI  PIC X(30).
      *           予約票印刷
                  07  MDCRES-PRINT-YOYAKU      PIC X(01).
                  07  MDCRES-PRINT-YOYAKU-MEI  PIC X(30).
      *           DATA-ID 2019/09/19 Added
              05  MDCRES-DATA-ID               PIC X(37).
      *
