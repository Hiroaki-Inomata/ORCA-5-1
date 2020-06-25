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
        01  SC07.
      *印刷年月
            03  SC07-PRTYM                PIC X(16).
      *請求年月日
            03  SC07-SEIYMD               PIC X(22).
      *市町村名
            03  SC07-CITYNAME             PIC X(20).
      *医療機関住所
            03  SC07-ADRS                 PIC X(100).
      *医療機関名
            03  SC07-HOSPNAME             PIC X(100).
      *開設者名
            03  SC07-KAISETU              PIC X(40).
      *医療機関コード
            03  SC07-HOSPCD               PIC X(14).
      *負担割合
            03  SC07-FTNWARI
                                          PIC X(02)    OCCURS  3 TIMES.
      *明細テーブル
            03  SC07-MEISAI-TBL           OCCURS  20 TIMES.
      *長期
                05  SC07-TYOKI1           PIC X(02).
                05  SC07-TYOKI2           PIC X(02).
      *氏名
                05  SC07-NAME             PIC X(24).
      *誕生年月
                05  SC07-TANJOYM          PIC X(10).
      *負担者番号
                05  SC07-FTNJANUM         PIC X(08).
      *受給者番号
                05  SC07-JKYSNUM          PIC X(14).
      *診療年月
                05  SC07-SRYYM            PIC X(08).
      *入外区分
                05  SC07-NYUGAIKBN
                                          PIC X(02)    OCCURS  2 TIMES.
      *日数
                05  SC07-NISSU            PIC X(04).
      *点数
                05  SC07-TENSU            PIC X(12).
      *一部負担金(老健)
                05  SC07-RITBFTN          PIC X(12).
      *一部負担金(高齢身)
                05  SC07-ITBFTN           PIC X(10).
      *備考
                05  SC07-BIKO1            PIC X(02).
                05  SC07-BIKO2            PIC X(02).
      *合計テーブル
            03  SC07-GOKEI-TBL.
      *合計件数
                05  SC07-GKENSU           PIC X(04).
      *合計点数
                05  SC07-GTENSU           PIC X(14).
      *合計一部負担金(老健)
                05  SC07-GRITBFTN         PIC X(14).
      *合計一部負担金(高齢身)
                05  SC07-GITBFTN          PIC X(14).
