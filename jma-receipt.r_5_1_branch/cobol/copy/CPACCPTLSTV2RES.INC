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
      *    ＡＰＩレスポンス受付一覧           COPY (CPACCPTLSTRES.INC)
      ******************************************************************
        01  XML-ACCEPTLSTRES.
            02  ACPLSTRES-CONTEXT          PIC S9(9)   BINARY.
            02  ACPLSTRES-OBJECT           PIC X(36).
            02  ACPLSTRES-MODE             PIC S9(9)   BINARY.
            02  ACPLSTRES-RECORDNAME       PIC X(128).
            02  ACPLSTRES-APPOINTRES.
      *       実施日
              03  ACPLSTRES-INFORMATION-DATE    PIC X(10).
      *       実施時間
              03  ACPLSTRES-INFORMATION-TIME    PIC X(8).
      *       返却コード
              03  ACPLSTRES-API-RESULT          PIC X(2).
      *       返却メッセージ
              03  ACPLSTRES-API-RESULT-MSG      PIC X(100).
      *
              03  ACPLSTRES-RESKEY              PIC X(100).
      *       受付日
              03  ACPLSTRES-ACCEPT-DATE         PIC X(10).
      *       受付内容
              03  ACPLSTRES-ACCEPT-LST-G.
                  05  ACPLSTRES-ACCEPT-LST      OCCURS   500.
      *               受付時間
                      07  ACPLSTRES-ACCEPT-TIME        PIC X(8).
      *               受付ＩＤ
                      07  ACPLSTRES-ACCEPT-ID          PIC X(5).
      *               診療科
                      07  ACPLSTRES-ACCEPT-DEP-CODE    PIC X(2).
      *               診療科名称
                      07  ACPLSTRES-ACCEPT-DEP-NAME    PIC X(80).
      *               ドクターコード
                      07  ACPLSTRES-ACCEPT-DR-CODE     PIC X(5).
      *               ドクター氏名
                      07  ACPLSTRES-ACCEPT-DR-NAME     PIC X(80).
      *               診療内容区分
                      07  ACPLSTRES-ACCEPT-DEP-INFO    PIC X(2).
      *               送受信情報
                      07  ACPLSTRES-CLAIM-INFO         PIC X(1).
      *               会計時間
                      07  ACPLSTRES-ACCOUNT-TIME       PIC X(8).
      *H29.4
      *               予約時間
                      07  ACPLSTRES-APPOINT-TIME       PIC X(8).
      *               予約ID
                      07  ACPLSTRES-APPOINT-ID         PIC X(2).
      *               患者情報
                      07  ACPLSTRES-PATIENT-INFOMATION.
      *                   患者番号
                          09  ACPLSTRES-PATIENTID         PIC X(20).
      *                   患者氏名
                          09  ACPLSTRES-NAME              PIC X(100).
      *                   患者カナ氏名
                          09  ACPLSTRES-KANANAME          PIC X(100).
      *                   誕生日
                          09  ACPLSTRES-BIRTHDAY          PIC X(10).
      *                   性別
                          09  ACPLSTRES-SEX               PIC X(1).
      *               保険情報
                      07  ACPLSTRES-INSURANCE-INFO.
      *                 保険組合せ番号
                        09  ACPLSTRES-COMBINATION-NUMBER     PIC X(04).
      *H29.4            非表示区分
                        09  ACPLSTRES-COMBINATION-HYOJI      PIC X(01).
      *                 保険の種類
                        09  ACPLSTRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *                 保険者番号
                        09  ACPLSTRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *                 保険の種類名称
                        09  ACPLSTRES-MAIN-INSURANCE-NAME    PIC X(20).
      *                 記号
                        09  ACPLSTRES-MAIN-MARK              PIC X(80).
      *                 番号
                        09  ACPLSTRES-MAIN-NUMBER            PIC X(80).
      *                 継続区分
                        09  ACPLSTRES-MAIN-CONTINUATION      PIC X(01).
      *                 補助区分
                        09  ACPLSTRES-MAIN-ASSISTANCE        PIC X(01).
      *                 本人家族区分
                        09  ACPLSTRES-MAIN-FAMILY            PIC X(01).
      *                 被保険者名
                        09  ACPLSTRES-MAIN-POLICYHOLDER      PIC X(100).
      *                 有効開始日
                        09  ACPLSTRES-MAIN-START-DATE        PIC X(10).
      *                 有効終了日
                        09  ACPLSTRES-MAIN-END-DATE          PIC X(10).
      *                 公費情報
                        09  ACPLSTRES-PUBLIC-INSURANCE-INFO           
                                                  OCCURS  4 TIMES.
      *                    公費の種類
                           11  ACPLSTRES-INSURANCE-CLASS     PIC X(03).
      *                    公費の種類名称
                           11  ACPLSTRES-INSURANCE-NAME      PIC X(20).
      *                    負担者番号
                           11  ACPLSTRES-PROVIDER-NUMBER     PIC X(08).
      *                    受給者番号
                           11  ACPLSTRES-RECIPIENT-NUMBER    PIC X(20).
      *                    入院−負担率（割）
                           11  ACPLSTRES-RATE-ADMISSION      PIC X(04).
      *                    入院−固定額
                           11  ACPLSTRES-MONEY-ADMISSION     PIC X(06). 
      *                    外来−負担率（割）
                           11  ACPLSTRES-RATE-OUTPATIENT     PIC X(04).
      *                    外来−固定額
                           11  ACPLSTRES-MONEY-OUTPATIENT    PIC X(06).
      *                    有効開始日
                           11  ACPLSTRES-INSURANCE-START-DATE
                                                           PIC X(10).
      *                    有効終了日
                           11  ACPLSTRES-INSURANCE-END-DATE  PIC X(10).
      *
      *H30.11
      *               メモ登録情報
                      07  ACPLSTRES-MEMO-INF.
                          09  ACPLSTRES-MEMO-00   PIC X(01).
                          09  ACPLSTRES-MEMO-KA   PIC X(01).
      *R01.5
      *               画面受付順
                      07  ACPLSTRES-DISPLAY-SORT.
                          09  ACPLSTRES-SORT-NUMBER      PIC X(4).
      *                   受付順変更区分
                          09  ACPLSTRES-SORT-MARK        PIC X(5).
