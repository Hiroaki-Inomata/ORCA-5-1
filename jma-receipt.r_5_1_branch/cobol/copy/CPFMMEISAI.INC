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
        01  FMMEISAI-REC.
            03  FMMEISAI-CONTEXT               PIC S9(9)   BINARY.
            03  FMMEISAI-OBJECT                PIC X(36).
            03  FMMEISAI-MODE                  PIC S9(9)   BINARY.
            03  FMMEISAI-RECORDNAME            PIC X(128).
            03  FMMEISAI-DATA.
              05  FMMEISAI-FORM-ID           PIC X(30).
              05  FMMEISAI-CUSTOM-ID           PIC X(30).
              05  FMMEISAI-PRINTER             PIC X(20).
      *
      *       患者番号
              05  FMMEISAI-PTNUM             PIC X(20).
      *       患者カナ氏名
              05  FMMEISAI-KANANAME          PIC X(50).
      *       患者氏名
              05  FMMEISAI-NAME              PIC X(50).
      *
      *       医療機関情報
              05  FMMEISAI-HOSP-AREA.
      *         医療機関名称
                07  FMMEISAI-HOSP-NAME-G.
                  09  FMMEISAI-HOSP-NAME       PIC X(120)
                                               OCCURS   3.
      *         医療機関郵便番号
                07  FMMEISAI-HOSP-POST         PIC X(7).
      *         医療機関住所
                07  FMMEISAI-HOSP-ADRS-G.
                  09  FMMEISAI-HOSP-ADRS       PIC X(200)
                                               OCCURS   3.
      *         医療機関電話番号
                07  FMMEISAI-HOSP-TEL          PIC X(15).
      *
      *       開始診療日
              05  FMMEISAI-STSRYYMD          PIC X(10).
      *       終了診療日
              05  FMMEISAI-EDSRYYMD          PIC X(10).
      *       伝票発行日
              05  FMMEISAI-DENPPRTYMD        PIC X(10).
      *
      *       診療科
              05  FMMEISAI-SRYKANAME         PIC X(30).
      *       保険情報
              05  FMMEISAI-HOKENNAME         PIC X(40).
              05  FMMEISAI-FUTAN-G.
      *         負担割合
                07  FMMEISAI-FUTAN         PIC X(04).
      *         負担割合
                07  FMMEISAI-FUTANMEI      PIC X(02).
      *       伝票番号（一括以外）
              05  FMMEISAI-DENPNUM           PIC X(07).
      *
      *       明細情報
              05   FMMEISAI-SRY-ITEM-AREA  OCCURS   2000.
      *         グループヘッダ（１：伝票番号が変わる度の先頭）
                07   FMMEISAI-GRPHEAD        PIC X(01).
      *         連番（伝票番号が変わる度に増加）（999：伝票情報行）
                07   FMMEISAI-GRPNUM         PIC X(03).
      *         診療区分名
                07   FMMEISAI-SRYKBN         PIC X(10).
      *         名称
                07  FMMEISAI-SRYNAME-G.
      *           剤開始（＊など
                  09  FMMEISAI-ZAIHEN        PIC X(02).
      *           診療内容名称
                  09  FMMEISAI-SRYNAME       PIC X(148).
      *         点数記載円立てフラグ
                07   FMMEISAI-TENSU-MARK     PIC X(1).
      *         点数
                07   FMMEISAI-TENSU          PIC X(09).
      *         回数
                07   FMMEISAI-KAISU          PIC X(03).
      *         合計
                07   FMMEISAI-TOTAL          PIC X(11).
      *         剤終了（１：剤最終行）
                07  FMMEISAI-ZAIEND          PIC X(01).
      *
      *       明細情報金額編集フラグ
              05  FMMEISAI-YEN-FLG           PIC X(1).
      *
      *       注意書き
              05  FMMEISAI-CAUTION           PIC X(100).
