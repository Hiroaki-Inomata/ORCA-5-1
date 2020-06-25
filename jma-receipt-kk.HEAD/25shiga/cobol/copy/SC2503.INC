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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.01    吉川         16/03/11  長期欄を2バイトに変更
      *  04.08.02    井上         18/12/12  修正履歴コメント削除
      *  04.08.02    井上         18/12/12  修正履歴コメント削除
      *  05.01.01    門間         19/04/23  改元対応
      *  05.01.02    門間         20/03/11  岐阜県の記載仕様に対応
      ********************************************************************
        01  SC03.
      *医療機関コード
            03  SC03-HOSPCD     PIC X(07).
      *医療機関−住所
            03  SC03-ADRS       PIC X(100).
      *医療機関−名称
            03  SC03-HOSPNAME   PIC X(100).
      *医療機関−開設者名
            03  SC03-KAISETU    PIC X(40).
      *医療機関−電話番号
            03  SC03-HOSPTEL    PIC X(15).
      *診療年月
            03  SC03-PRTGG      PIC X(04).
            03  SC03-PRTYY      PIC X(02).
            03  SC03-PRTMM      PIC X(02).
      *請求年月日
            03  SC03-SEIGG      PIC X(04).
            03  SC03-SEIYY      PIC X(02).
            03  SC03-SEIMM      PIC X(02).
            03  SC03-SEIDD      PIC X(02).
      *点数表区分
            03  SC03-TENSUHYO   PIC X(01).
      *ページ数
            03  SC03-PAGE       PIC X(03).
            03  SC03-KANJA-TBL          OCCURS  6 TIMES.
      *氏名
                05  SC03-NAME       PIC X(20).
      *性別
                05  SC03-SEX        PIC X(01).
      *生年月日
                05  SC03-TANJOYMD   PIC X(07).
      *保険者番号
                05  SC03-HKNJANUM   PIC X(08).
      *公費番号(負担者番号)
                05  SC03-FTNJANUM   PIC X(08)    OCCURS  2 TIMES.
      *受給者番号
                05  SC03-JKYSNUM    PIC X(07)    OCCURS  2 TIMES.
      *記号
                05  SC03-KIGO       PIC X(16).
      *番号
                05  SC03-NUM        PIC X(24). 
      *診療年月
                05  SC03-SRYYY      PIC X(02).
                05  SC03-SRYMM      PIC X(02).
      *給付割合
                05  SC03-KYURATE    PIC X(02).
      *本人家族区分
                05  SC03-HONKZKKBN  PIC X(01).
      *日数
                05  SC03-NISSU      PIC X(02).
      *点数
                05  SC03-TENSU      PIC X(07).
      *一部負担金
                05  SC03-ITBFTN     PIC X(07).
      *公費分点数
                05  SC03-KOHTENSU   PIC X(07)    OCCURS  2 TIMES.
      *公費対象患者負担額
                05  SC03-KOHPTFTN   PIC X(07)    OCCURS  2 TIMES.
      *長期
                05  SC03-TYOKI      PIC X(02).
      *老人区分
                05  SC03-RJNKBN     PIC X(02).
      *他法公費番号
                05  SC03-TAHONUM    PIC X(02).
      *科別番号
                05  SC03-SRYKA      PIC X(02).
      *合計エリア
            03  SC03-GOKEI-TBL. 
                05  SC03-GKENSU     PIC X(03).
                05  SC03-GTENSU     PIC X(07).
                05  SC03-GITBFTN    PIC X(07).
                05  SC03-GKOHTENSU  PIC X(07)    OCCURS  2 TIMES.
                05  SC03-GKOHPTFTN  PIC X(07)    OCCURS  2 TIMES.
      *医療機関コード（岐阜県用）
            03  SC03-HOSPCD-P21     PIC X(03).
