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
        01  SC19.
      *医療機関ID
            03  SC19-HOSPCD     PIC X(20).
      *医療機関名称
            03  SC19-HOSPNAME   PIC X(60).
      *医療機関所在地
            03  SC19-ADRS       PIC X(60).
      *医療機関電話番号
            03  SC19-TEL        PIC X(15).
      *代表者
            03  SC19-KAISETU    PIC X(60).
      *銀行名
            03  SC19-GINKOU     PIC X(20).
      *支店名
            03  SC19-SITEN      PIC X(20).
      *口座区分
            03  SC19-KOUZAKBN   PIC X(02)    OCCURS  2 TIMES.
      *口座番号
            03  SC19-KOUZANUM   PIC X(10).
      *診療年月
            03  SC19-SRYYM      PIC X(16).
      *請求年月日
            03  SC19-SEIYMD     PIC X(22).
      *公費区分
            03  SC19-KOHKBN     PIC X(02)    OCCURS  2 TIMES.
      *合計請求額
            03  SC19-SEIKYUGK-G PIC X(09).
      *合計ページ
            03  SC19-GPAGE      PIC X(03).
      *ページ
            03  SC19-PAGE       PIC X(03).
      *明細エリア
            03  SC19-SEIKYU-TBL         OCCURS  15 TIMES.
      *受給者
                05  SC19-JKYSNUM    PIC X(16).
      *氏名
                05  SC19-NAME       PIC X(32).
      *誕生日
                05  SC19-TANJOYY    PIC X(03).
                05  SC19-TANJOMM    PIC X(02).
                05  SC19-TANJODD    PIC X(02).
      *入外区分
                05  SC19-NYUGAIKBN  PIC X(02)    OCCURS  2 TIMES.
      *診療科目
                05  SC19-SRYKA      PIC X(08).
      *負担割合
                05  SC19-FTNRATE    PIC X(02)    OCCURS  2 TIMES.
      *処方種別
                05  SC19-SHOHOUKBN  PIC X(02)    OCCURS  2 TIMES.
      *入院日数
                05  SC19-NISSU      PIC X(02).
      *点数
                05  SC19-TENSU      PIC X(09).
      *医療費請求金額
                05  SC19-SEIKYUGK   PIC X(09).
      *小計・合計エリア
            03  SC19-GOKEI-TBL          OCCURS  2 TIMES.
                05  SC19-GKENSU     PIC X(03).
                05  SC19-GSEIKYUGK  PIC X(10).
