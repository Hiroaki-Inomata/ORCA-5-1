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
        01  FMSEIKYU-REC.
            03  FMSEIKYU-CONTEXT                   PIC S9(9)   BINARY.
            03  FMSEIKYU-OBJECT                    PIC X(36).
            03  FMSEIKYU-MODE                      PIC S9(9)   BINARY.
            03  FMSEIKYU-RECORDNAME                PIC X(128).
            03  FMSEIKYU-DATA.
              05  FMSEIKYU-FORM-ID               PIC X(30).
              05  FMSEIKYU-CUSTOM-ID               PIC X(30).
              05  FMSEIKYU-PRINTER                 PIC X(20).
      *
              05  FMSEIKYU-MOTOID                  PIC X(02).
      *
      *       患者番号
              05  FMSEIKYU-PTNUM                   PIC X(20).
      *       患者氏名
              05  FMSEIKYU-NAME                    PIC X(50).
      *       患者カナ氏名
              05  FMSEIKYU-KANANAME                PIC X(50).
      *
      *       医療機関情報
              05  FMSEIKYU-HOSP-AREA.
      *         医療機関名称
                07  FMSEIKYU-HOSP-NAME-G.
                  09  FMSEIKYU-HOSP-NAME           PIC X(120)
                                                   OCCURS   3.
      *         医療機関郵便番号
                07  FMSEIKYU-HOSP-POST             PIC X(7).
      *         医療機関住所
                07  FMSEIKYU-HOSP-ADRS-G.
                  09  FMSEIKYU-HOSP-ADRS           PIC X(200)
                                                   OCCURS   3.
      *         医療機関電話番号
                07  FMSEIKYU-HOSP-TEL              PIC X(15).
      *
      *       実日数
              05  FMSEIKYU-JNISSU                  PIC X(03).
      *       診療期間区分
              05  FMSEIKYU-SRYKIKANKBN             PIC X(01).
      *       診療年月
              05  FMSEIKYU-SRYYM                   PIC X(07).
      *       開始診療日
              05  FMSEIKYU-STSRYYMD                PIC X(10).
      *       終了診療日
              05  FMSEIKYU-EDSRYYMD                PIC X(10).
      *       伝票発行日
              05  FMSEIKYU-DENPPRTYMD              PIC X(10).
      *
      *       診療科
              05  FMSEIKYU-SRYKANAME                   PIC X(30).
      *       保険情報
              05  FMSEIKYU-HOKENNAME                   PIC X(40).
              05  FMSEIKYU-FUTAN-G.
      *         負担割合
                07  FMSEIKYU-FUTAN                     PIC X(04).
      *         負担割合
                07  FMSEIKYU-FUTANMEI                  PIC X(02).
      *       伝票番号（一括以外）
              05  FMSEIKYU-DENPNUM                     PIC X(07).
      *       訂正表示区分
              05  FMSEIKYU-TEISEI-KBN                  PIC X(01).
      *       請求項目
              05  FMSEIKYU-MEISAI-OCC                  OCCURS 16.
      *           請求項目区分
                  07  FMSEIKYU-HKNTEN-KBN              PIC X(03).
      *           請求項目名
                  07  FMSEIKYU-HKNTEN-NAME             PIC X(20).
      *           請求点数
                  07  FMSEIKYU-HKNTEN                  PIC X(08).
      *           請求金額（保険適用外）
                  07  FMSEIKYU-TGMONEY                 PIC X(08).
      *       点数合計
              05  FMSEIKYU-TOTAL-TEN                   PIC X(08).
      *       患者一部負担金
              05  FMSEIKYU-HKNTEKMONEY                 PIC X(08).
      *       患者一部負担金ラベル（労災診療料等再掲）
              05  FMSEIKYU-LBL-RSI-TOTAL               PIC X(20).
      *       患者一部負担金（労災診療料等再掲）
              05  FMSEIKYU-RSI-TOTAL                   PIC X(08).
      *       公費一部負担金
              05  FMSEIKYU-KOH-FTN                     PIC X(08).
      *       保険適用外合計
              05  FMSEIKYU-TOTAL-TGMONEY               PIC X(08).
      *       保険適用外合計消費税再掲
              05  FMSEIKYU-TOTAL-TGMONEY-TAX           PIC X(08).
      *       その他自費
              05  FMSEIKYU-JIHI-OCC                    OCCURS  10.
      *             その他自費名称
                07  FMSEIKYU-JIHI-NAME                 PIC X(20).
      *             その他自費金額
                07  FMSEIKYU-JIHI                      PIC X(08).
      *       その他自費合計金額
              05  FMSEIKYU-JIHI-TOTAL                  PIC X(08).
      *       その他自費合計金額消費税再掲
              05  FMSEIKYU-JIHI-TOTAL-TAX              PIC X(08).
      *       調整金１
              05  FMSEIKYU-CHOSEI1                     PIC X(08).
      *       調整金２
              05  FMSEIKYU-CHOSEI2                     PIC X(08).
      *       今回請求額
              05  FMSEIKYU-SKYMONEY                    PIC X(08).
      *       今回請求額消費税再掲
              05  FMSEIKYU-SKYMONEY-TAX                PIC X(08).
      *       前回請求額
              05  FMSEIKYU-SKYMONEY-LAST               PIC X(08).
      *       合計請求額
              05  FMSEIKYU-SKYMONEY-TOTAL              PIC X(08).
      *       今回入金額
              05  FMSEIKYU-NYUKIN                      PIC X(08).
      *
      *       注意書き（請求書部分）
              05  FMSEIKYU-SKY-CAUTION                 PIC X(100).
      *
      *       メモ
              05  FMSEIKYU-MEMO                        PIC X(80)
                                                       OCCURS 10.
      *
      *       明細情報
              05   FMSEIKYU-SRY-ITEM-AREA              OCCURS 2000.
      *         グループヘッダ（１：伝票番号が変わる度の先頭）
                07   FMSEIKYU-GRPHEAD                  PIC X(01).
      *         連番（伝票番号が変わる度に増加）（999：伝票情報行）
                07   FMSEIKYU-GRPNUM                   PIC X(03).
      *         診療区分名
                07   FMSEIKYU-SRYKBN                   PIC X(10).
      *         名称
                07  FMSEIKYU-SRYNAME-G.
      *           剤開始（＊など
                  09  FMSEIKYU-ZAIHEN                  PIC X(02).
      *           診療内容名称
                  09  FMSEIKYU-SRYNAME                 PIC X(148).
      *         点数記載円立てフラグ
                07   FMSEIKYU-TENSU-MARK               PIC X(1).
      *         点数
                07   FMSEIKYU-TENSU                    PIC X(09).
      *         回数
                07   FMSEIKYU-KAISU                    PIC X(03).
      *         合計
                07   FMSEIKYU-TOTAL                    PIC X(11).
      *         剤終了（１：剤最終行）
                07  FMSEIKYU-ZAIEND                    PIC X(01).
      *
      *       明細情報金額編集フラグ
              05  FMSEIKYU-YEN-FLG                     PIC X(1).
              05  FMSEIKYU-SRY-CAUTION                 PIC X(100).
