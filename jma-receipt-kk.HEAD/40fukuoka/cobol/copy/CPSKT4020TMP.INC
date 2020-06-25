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
      ************************************************************
      *    診療報酬請求書     COPY (CPSKT4020TMP.INC)              *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者     日付        内容
      *  01.00.01    高嶋       05/10/31    ７割追加対応
      *                                     薬剤一部負担金削除対応
      *  01.00.02    門間       08/10/29    シス管「2007」公費まとめ対応
      *  01.00.03    内田       09/10/14    中国九州北部豪雨対応
      *  01.00.04    門間       19/01/10    修正履歴コメント削除
      ************************************************************
       01  SKT4020TMP-REC.
           03  SKT4020TMP-KEY.
               05  SKT4020TMP-SYUTURYOKU            PIC  9(01).
               05  SKT4020TMP-HKNJANUM              PIC  X(06).
               05  SKT4020TMP-ZAITAKU               PIC  X(01).
               05  SKT4020TMP-GENMEN                PIC  X(01).
      *
           03  SKT4020TMP-PAGE1-FLG                 PIC  9(01).
           03  SKT4020TMP-PAGE2-FLG                 PIC  9(01).
           03  SKT4020TMP-PAGE3-FLG                 PIC  9(01).
           03  SKT4020TMP-KYURATE-TBL  OCCURS 2.
               05  SKT4020TMP-KYURATE               PIC  9(02).
      *
           03  SKT4020TMP-HKN-AREA  OCCURS  10.
               05  SKT4020TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT4020TMP-KENSU             PIC  9(05).
                   07  SKT4020TMP-NISSU             PIC  9(05).
                   07  SKT4020TMP-TENSU             PIC  9(09).
                   07  SKT4020TMP-YKZFTN            PIC  9(09).
                   07  SKT4020TMP-ITBFTN            PIC  9(09).
               05  SKT4020TMP-S-KENSU               PIC  9(05).
               05  SKT4020TMP-S-NISSU               PIC  9(05).
               05  SKT4020TMP-S-KINGK               PIC  9(13).
               05  SKT4020TMP-S-FTNGK               PIC  9(09).
           03  SKT4020TMP-KOH-AREA  OCCURS  20.
               05  SKT4020TMP-KOHNUM                PIC  X(03).
               05  SKT4020TMP-KOHNAME               PIC  X(10).
               05  SKT4020TMP-KOH-NYUGAI-TBL OCCURS 2.
                   07  SKT4020TMP-KKENSU            PIC  9(05).
                   07  SKT4020TMP-KNISSU            PIC  9(05).
                   07  SKT4020TMP-KTENSU            PIC  9(09).
                   07  SKT4020TMP-KYKZFTN           PIC  9(09).
                   07  SKT4020TMP-KITBFTN           PIC  9(09).
                   07  SKT4020TMP-KPTFTN            PIC  9(09).
               05  SKT4020TMP-S-KKENSU              PIC  9(05).
               05  SKT4020TMP-S-KNISSU              PIC  9(05).
               05  SKT4020TMP-S-KKINGK              PIC  9(13).
               05  SKT4020TMP-S-KFTNGK              PIC  9(09).
           03  SKT4020TMP-KOH2-AREA  OCCURS  20.
               05  SKT4020TMP-KOHNUM2                PIC  X(03).
               05  SKT4020TMP-KOH2-NYUGAI-TBL OCCURS 2.
                   07  SKT4020TMP-KKENSU2            PIC  9(05).
                   07  SKT4020TMP-KNISSU2            PIC  9(05).
                   07  SKT4020TMP-KTENSU2            PIC  9(09).
                   07  SKT4020TMP-KYKZFTN2           PIC  9(09).
                   07  SKT4020TMP-KITBFTN2           PIC  9(09).
                   07  SKT4020TMP-KPTFTN2            PIC  9(09).
               05  SKT4020TMP-S-KKENSU2              PIC  9(05).
               05  SKT4020TMP-S-KNISSU2              PIC  9(05).
               05  SKT4020TMP-S-KKINGK2              PIC  9(13).
               05  SKT4020TMP-S-KFTNGK2              PIC  9(09).
           03  SKT4020TMP-GENBAKU-AREA.
               05  SKT4020TMP-KOH-NYUGAI-TBL OCCURS 2.
                   07  SKT4020TMP-GKENSU            PIC  9(05).
                   07  SKT4020TMP-GNISSU            PIC  9(05).
                   07  SKT4020TMP-GTENSU            PIC  9(09).
                   07  SKT4020TMP-GYKZFTN           PIC  9(09).
                   07  SKT4020TMP-GITBFTN           PIC  9(09).
                   07  SKT4020TMP-GPTFTN            PIC  9(09).
               05  SKT4020TMP-S-GKENSU              PIC  9(05).
               05  SKT4020TMP-S-GNISSU              PIC  9(05).
               05  SKT4020TMP-S-GKINGK              PIC  9(13).
               05  SKT4020TMP-S-GFTNGK              PIC  9(09).
