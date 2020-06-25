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
      *表示年月
              03  SC07-PRTYM            PIC  X(16).
      *定率／定額
              03  SC07-RITU             PIC  X(02).
              03  SC07-GAKU             PIC  X(02).
      *県番号
              03  SC07-KENNUM           PIC  X(02).
      *医療機関コード
              03  SC07-HOSPCDN          PIC  X(20).
      *請求年月日
              03  SC07-SEIYMD           PIC  X(22).
      *住所
              03  SC07-ADRS             PIC  X(100).
      *医療機関名
              03  SC07-HOSPNAME         PIC  X(100).
      *管理者名
              03  SC07-KAISETUNAME      PIC  X(60).
      *電話番号
              03  SC07-TEL              PIC  X(15).
      *診療テーブル
              03  SC07-SRYTBL           OCCURS  10.
      *受給者番号
                  05  SC07-JKYSNUM      PIC  X(06).
      *受診者氏名
                  05  SC07-NAME         PIC  X(20).
      *入外区分
                  05  SC07-NYUGAIKBN-TBL  OCCURS  2.
                      07  SC07-NYUGAIKBN  PIC  X(02).
      *保険区分：国保／社保
                  05  SC07-SYAHO        PIC  X(02).
                  05  SC07-KOKUHO       PIC  X(02).
                  05  SC07-TAI          PIC  X(02).
      *点数
                  05  SC07-TENSU7       PIC  X(07).
                  05  SC07-KOHTENSU7    PIC  X(06).
                  05  SC07-TENSU8       PIC  X(07).
                  05  SC07-KOHTENSU8    PIC  X(06).
      *薬剤一部負担金
                  05  SC07-YKZFTN       PIC  X(07).
      *実日数
                  05  SC07-NISSU        PIC  X(02).
      *患者負担額
                  05  SC07-ITBFTN       PIC  X(07).
      *請求算定額
                  05  SC07-SEIKYU       PIC  X(09).
      *合計テーブル
              03  SC07-SUM-TBL  OCCURS  2.
      *合計：件数／点数／公費点数
                  05  SC07-GKENSU7      PIC  X(02).
                  05  SC07-GTENSU7      PIC  X(07).
                  05  SC07-GKOHTEN7     PIC  X(07).
                  05  SC07-GKENSU8      PIC  X(02).
                  05  SC07-GTENSU8      PIC  X(07).
                  05  SC07-GKOHTEN8     PIC  X(07).
      *合計：薬剤一部負担金
                  05  SC07-GYKZFTN      PIC  X(07).
      *合計：実日数
                  05  SC07-GNISSU       PIC  X(03).
      *合計：患者負担額
                  05  SC07-GITBFTN      PIC  X(07).
      *合計：請求算定額
                  05  SC07-GSEIKYU      PIC  X(09).
