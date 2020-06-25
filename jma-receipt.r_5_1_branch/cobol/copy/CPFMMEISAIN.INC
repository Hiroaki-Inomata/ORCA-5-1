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
        01  FMMEISAIN-REC.
            03  FMMEISAIN-CONTEXT              PIC S9(9)   BINARY.
            03  FMMEISAIN-OBJECT               PIC X(36).
            03  FMMEISAIN-MODE                 PIC S9(9)   BINARY.
            03  FMMEISAIN-RECORDNAME           PIC X(128).
            03  FMMEISAIN-DATA.
              05  FMMEISAIN-FORM-ID            PIC X(30).
              05  FMMEISAIN-CUSTOM-ID          PIC X(30).
              05  FMMEISAIN-PRINTER            PIC X(20).
      *
      *       患者番号
              05  FMMEISAIN-PTNUM             PIC X(20).
      *       患者氏名
              05  FMMEISAIN-NAME              PIC X(50).
      *       患者カナ氏名
              05  FMMEISAIN-KANANAME          PIC X(50).
      *
      *       医療機関情報
              05  FMMEISAIN-HOSP-AREA.
      *         医療機関名称
                07  FMMEISAIN-HOSP-NAME-G.
                  09  FMMEISAIN-HOSP-NAME      PIC X(120)
                                               OCCURS   3.
      *         医療機関郵便番号
                07  FMMEISAIN-HOSP-POST        PIC X(7).
      *         医療機関住所
                07  FMMEISAIN-HOSP-ADRS-G.
                  09  FMMEISAIN-HOSP-ADRS      PIC X(200)
                                               OCCURS   3.
      *         医療機関電話番号
                07  FMMEISAIN-HOSP-TEL         PIC X(15).
      *
      *       開始診療日
              05  FMMEISAIN-STSRYYMD           PIC X(10).
      *       終了診療日
              05  FMMEISAIN-EDSRYYMD           PIC X(10).
      *       伝票発行日
              05  FMMEISAIN-DENPPRTYMD         PIC X(10).
      *       病棟番号
              05  FMMEISAIN-BTUNUM             PIC X(2).
      *       病棟名称
              05  FMMEISAIN-BTUNAME            PIC X(20).
      *       病室番号
              05  FMMEISAIN-BRMNUM             PIC X(6).
      *       診療科
              05  FMMEISAIN-SRYKANAME          PIC X(30).
      *       保険情報
              05  FMMEISAIN-HOKENNAME          PIC X(40).
              05  FMMEISAIN-FUTAN-G.
      *         負担割合
                07  FMMEISAIN-FUTAN            PIC X(04).
      *         負担割合
                07  FMMEISAIN-FUTANMEI         PIC X(02).
      *       伝票番号（一括以外）
              05  FMMEISAIN-DENPNUM            PIC X(07).
      *
      *       明細情報
              05   FMMEISAIN-SRY-ITEM-AREA     OCCURS   2000.
      *         グループヘッダ（１：伝票番号が変わる度の先頭）
                07   FMMEISAIN-GRPHEAD         PIC X(01).
      *         連番（伝票番号が変わる度に増加）（999：伝票情報行）
                07   FMMEISAIN-GRPNUM          PIC X(03).
      *         診療区分名
                07   FMMEISAIN-SRYKBN          PIC X(10).
      *         名称
                07  FMMEISAIN-SRYNAME-G.
      *           剤開始（＊など
                  09  FMMEISAIN-ZAIHEN         PIC X(02).
      *           診療内容名称
                  09  FMMEISAIN-SRYNAME        PIC X(148).
      *         点数記載円立てフラグ
                07   FMMEISAIN-TENSU-MARK      PIC X(1).
      *         点数
                07   FMMEISAIN-TENSU           PIC X(09).
      *         回数
                07   FMMEISAIN-KAISU           PIC X(03).
      *         合計
                07   FMMEISAIN-TOTAL           PIC X(11).
      *         剤終了（１：剤最終行）
                07  FMMEISAIN-ZAIEND           PIC X(01).
      *
      *       明細情報金額編集フラグ
              05  FMMEISAIN-YEN-FLG            PIC X(1).
      *
      *       注意書き
              05  FMMEISAIN-CAUTION            PIC X(100).
