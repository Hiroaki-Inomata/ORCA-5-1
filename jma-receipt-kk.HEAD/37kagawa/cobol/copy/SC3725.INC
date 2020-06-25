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
        01  SC25.
      *診療年月
            03  SC25-SRYYM      PIC X(06).
      *請求日付
            03  SC25-SEIYMD     PIC X(22).
      *合計請求金額
            03  SC25-SEIKYUGK-G PIC X(09).
      *医療機関名称
            03  SC25-HOSPNAME   PIC X(100).
      *医療機関所在地
            03  SC25-ADRS       PIC X(100).
      *開設者氏名
            03  SC25-KAISETU    PIC X(40).
      *銀行名
            03  SC25-GINKO      PIC X(12).
      *支店名
            03  SC25-SITEN      PIC X(12).
      *口座番号
            03  SC25-KOUZA      PIC X(08).
      *合計ページ
            03  SC25-GPAGE      PIC X(03). 
      *ページ
            03  SC25-PAGE       PIC X(03).
            03  SC25-SEIKYU-TBL         OCCURS  13 TIMES.
      *受給者番号
                05  SC25-JKYSNUM    PIC X(16).
      *氏名
                05  SC25-NAME       PIC X(32).
      *保険種別
      *(１：社本 ２：社家 ３：国保 ４：退本 ５：退家)
                05  SC25-HKNSYUBETU PIC X(02)    OCCURS  5 TIMES.
      *入外区分(１：入院 ２：入院外)
                05  SC25-NYUGAIKBN  PIC X(02)    OCCURS  2 TIMES.
      *医療費総点数
                05  SC25-TENSU      PIC X(07).
      *他法負担額
                05  SC25-TAHO       PIC X(07).
      *負担金
                05  SC25-FTNGK      PIC X(07).
      *薬剤一部負担金
                05  SC25-YKZFTN     PIC X(07).
      *食事日数
                05  SC25-SNISSU     PIC X(02).
      *食事療養費
                05  SC25-SKINGK     PIC X(06).
      *入院年月日
                05  SC25-NYUINYY    PIC X(03).
                05  SC25-NYUINMM    PIC X(02).
                05  SC25-NYUINDD    PIC X(02).
      *入院日数
                05  SC25-NNISSU     PIC X(02).
      *請求金額
                05  SC25-SEIKYUGK   PIC X(09).
            03  SC25-GOKEI-TBL.
      *合計点数
                05  SC25-GTENSU     PIC X(09).
      *合計他法負担額
                05  SC25-GTAHO      PIC X(09).
                05  SC25-GFTNGK     PIC X(09).
                05  SC25-GSEIKYUGK  PIC X(09).
