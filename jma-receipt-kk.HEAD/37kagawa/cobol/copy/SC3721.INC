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
        01  SC21.
      *診療年月
            03  SC21-SRYYM      PIC X(06).
      *請求日付
            03  SC21-SEIYMD     PIC X(22).
      *合計請求金額
            03  SC21-SEIKYUGK-G PIC X(09).
      *医療機関名称
            03  SC21-HOSPNAME   PIC X(100).
      *医療機関所在地
            03  SC21-ADRS       PIC X(100).
      *開設者氏名
            03  SC21-KAISETU    PIC X(40).
      *銀行名
            03  SC21-GINKO      PIC X(12).
      *支店名
            03  SC21-SITEN      PIC X(12).
      *口座番号
            03  SC21-KOUZA      PIC X(08).
      *合計ページ
            03  SC21-GPAGE      PIC X(03). 
      *ページ
            03  SC21-PAGE       PIC X(03).
            03  SC21-SEIKYU-TBL         OCCURS  12 TIMES.
      *受給者番号
                05  SC21-JKYSNUM    PIC X(16).
      *氏名
                05  SC21-NAME       PIC X(32).
      *保険種別(１：社家 ２：国保 ３：退家)
                05  SC21-HKNSYUBETU PIC X(02)    OCCURS  3 TIMES.
      *入外区分(１：入院 ２：入院外)
                05  SC21-NYUGAIKBN  PIC X(02)    OCCURS  2 TIMES.
      *医療費総点数
                05  SC21-TENSU      PIC X(07).
      *負担区分(１：２割 ２：３割)
                05  SC21-FTNRATE    PIC X(02)    OCCURS  2 TIMES.
      *他法負担額
                05  SC21-TAHO       PIC X(07).
      *高額療養費
                05  SC21-KOGAKU     PIC X(07).
      *負担金
                05  SC21-FTNGK      PIC X(07).
      *入院年月日
                05  SC21-NYUINYY    PIC X(03).
                05  SC21-NYUINMM    PIC X(02).
                05  SC21-NYUINDD    PIC X(02).
      *入院日数
                05  SC21-NNISSU     PIC X(02).
      *請求金額
                05  SC21-SEIKYUGK   PIC X(09).
            03  SC21-GOKEI-TBL.
      *合計請求額
                05  SC21-GSEIKYUGK  PIC X(09).
