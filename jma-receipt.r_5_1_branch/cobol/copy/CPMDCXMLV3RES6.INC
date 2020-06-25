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
      *  05.00.00    NACL-多々納  18/06/XX  入院対応追加
      *****************************************************************
      ******************************************************************
      *    ＡＰＩレスポンス保険一括変更情報 (一体化)
      *                                      COPY (CPMDCXMLV3RE6.INC)
      *         診療行為内容
      ******************************************************************
       01  XML-MEDICALV3RES6.
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
      *        診療年月
               05  MDCRES-PERFORM-YM        PIC X(07).
      *        診療科
               05  MDCRES-APPOINT-DEP-CODE  PIC X(2).
               05  MDCRES-APPOINT-DEP-NAME  PIC X(80).
      *        患者情報
               05  MDCRES-PATIENTID        PIC X(20).
               05  MDCRES-NAME             PIC X(100).
               05  MDCRES-KANANAME         PIC X(100).
               05  MDCRES-BIRTHDAY         PIC X(10).
               05  MDCRES-SEX              PIC X(1).
      * リクエスト＝０１
      *        受診履歴情報
               05  MDCRES-MEDICAL-LIST-G.
                   07  MDCRES-MEDICAL-LIST-TBL     OCCURS   150.
      *                診療日付
                       09  MDCRES-PERFORM-DATE     PIC X(10).
      *                伝票番号
                       09  MDCRES-INVOICD-NUMBER        PIC  X(07).
      *                保険組合せ番号
                       09  MDCRES-MED-COMBINATION-NUMBER   PIC X(04).
      *                連番
                       09  MDCRES-MED-RENNUM             PIC X(01).
      *                保険組合せ負担割合（外来）
                       09  MDCRES-MED-COMBINATION-RATE  PIC X(03).
      *                保険組合せ 有効開始日
                       09  MDCRES-MED-START-DATE        PIC X(10).
      *                保険組合せ 有効終了日
                       09  MDCRES-MED-END-DATE          PIC X(10).
      *                保険の種類
                       09  MDCRES-MED-INSURANCE-CLASS   PIC X(03).
      *                保険の種類名称
                       09  MDCRES-MED-INSURANCE-NAME    PIC X(20).
      *                保険者番号
                       09  MDCRES-MED-INSURANCE-NUMBER  PIC X(08).
      *                公費情報
                       09  MDCRES-MED-INSURANCE-INFO 
                                              OCCURS  4 TIMES.
      *                 公費の種類
                        11  MDCRES-MED-INSU-CLASS     PIC X(03).
      *                 公費の種類名称
                        11  MDCRES-MED-INSU-NAME      PIC X(20).
      *                 負担者番号
                        11  MDCRES-MED-PROVIDER-NUMBER     PIC X(08).
      *                 受給者番号
                        11  MDCRES-MED-RECIPIENT-NUMBER    PIC X(20).
      *                保険組合せ削除区分（１：削除、２：期間外
      *                                    ３：外来非表示）
                       09  MDCRES-MED-COMBINATION-MODE  PIC X(1).
      *H30.6
      *                入院調剤料のみ算定
                       09  MDCRES-CHOZAI-AREA.
                           11  MDCRES-CHOZAI-ONLY      PIC X(10).
      *!!
      * リクエスト＝０１
      *        保険組合せ一覧返却
               05  MDCRES-INSURANCE-INFO-G.
      *           保険情報（保険組合せ）
                  06  MDCRES-INSURANCE-INFO     OCCURS  100 TIMES.
      *               保険組合せ番号
                      07  MDCRES-COMBINATION-NUMBER     PIC X(04).
      *               保険組合せ負担割合
                      07  MDCRES-COMBINATION-RATE       PIC X(03).
      *               保険組合せ 有効開始日
                      07  MDCRES-COMB-START-DATE        PIC X(10).
      *               保険組合せ 有効終了日
                      07  MDCRES-COMB-END-DATE          PIC X(10).
      *
      *               保険の種類
                      07  MDCRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *               保険の種類名称
                      07  MDCRES-MAIN-INSURANCE-NAME    PIC X(20).
      *               保険者番号
                      07  MDCRES-MAIN-INSURANCE-NUMBER   PIC X(08).
      *               記号
                      07  MDCRES-MAIN-MARK              PIC X(80).
      *               番号
                      07  MDCRES-MAIN-NUMBER            PIC X(80).
      *               継続区分
                      07  MDCRES-MAIN-CONTINUATION      PIC X(01).
      *               補助区分
                      07  MDCRES-MAIN-ASSISTANCE        PIC X(01).
                      07  MDCRES-MAIN-ASSISTANCE-NAME   PIC X(20).
      *               本人家族区分
                      07  MDCRES-MAIN-FAMILY            PIC X(01).
      *               開始日
                      07  MDCRES-MAIN-START-DATE        PIC X(10).
                      07  MDCRES-MAIN-END-DATE          PIC X(10).
      *               公費情報
                      07  MDCRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *                 公費の種類
                        09  MDCRES-INSURANCE-CLASS         PIC X(03).
      *                 公費の種類名称
                        09  MDCRES-INSURANCE-NAME          PIC X(20).
      *                 負担者番号
                        09  MDCRES-PROVIDER-NUMBER         PIC X(08).
      *                 受給者番号
                        09  MDCRES-RECIPIENT-NUMBER        PIC X(20).
      *                 外来－負担率（割）
                        09  MDCRES-RATE-OUTPATIENT         PIC X(04).
      *                 外来－固定額
                        09  MDCRES-MONEY-OUTPATIENT        PIC X(06).
      *                 開始日
                        09  MDCRES-INSURANCE-START-DATE    PIC X(10).
                        09  MDCRES-INSURANCE-END-DATE      PIC X(10).
      * リクエスト＝０２
      *       一括変更内容（リクエスト＝０２）
               05  MDCRES-CHENGE-INFO.
      *           変更前　保険組合せ番号
                  07  MDCRES-IN-COMBINATION-NUMBER    PIC X(04).
      *           変更後　保険組合せ番号
                  07  MDCRES-OT-COMBINATION-NUMBER    PIC X(04).
      *           変更期間
                  07  MDCRES-START-DAY         PIC X(02).
                  07  MDCRES-END-DAY           PIC X(02).
      *           収納一括更新あり
                  07  MDCRES-INCOME-CHANGE     PIC X(03).
      *        変更後、収納情報
               05  MDCRES-CD-INF-G.
                   07  MDCRES-CD-INF-TBL     OCCURS   100.
      *                診療日付
                       09  MDCRES-CD-PERFORM-DATE       PIC X(10).
      *                伝票番号
                       09  MDCRES-CD-INVOICD-NUMBER    PIC  X(07).
      *                保険組合せ番号
                       09  MDCRES-CD-COMBINATION-NUMBER PIC X(04).
      *                収納負担割合
                       09  MDCRES-CD-RATE               PIC X(03).
      *                伝票発行日付
                       09  MDCRES-CD-DENPPRTYMD         PIC X(10).
      *                変更前　請求金額
                       09  MDCRES-MAE-AC-MONEY          PIC X(07).
      *                変更後　請求金額
                       09  MDCRES-ATO-AC-MONEY          PIC X(07).
      *                更新不可区分
                       09  MDCRES-DC-ERR                PIC X(06).
      *        警告メッセージ
               05  MDCRES-WARING-G.
                   07  MDCRES-WARING-OCC       OCCURS  20.
                       09  MDCRES-WARNING              PIC X(03).
                       09  MDCRES-WARNIG-MESG          PIC X(200).
      *
