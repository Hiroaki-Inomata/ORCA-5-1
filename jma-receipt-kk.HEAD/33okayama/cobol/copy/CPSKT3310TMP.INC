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
      *    診療報酬請求書     COPY (CPSKT3310TMP.INC)          *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者   日付        内容
      * 01.00.01    門間    2008/11/20  シス管「2007」公費まとめ対応
      * 01.00.02    内田    2009/09/29  平成21年台風第9号対応（対象者別集計）
      * 01.00.03    吉川    2010/12/09  open-cobol1.0対応
      * 01.00.04    吉川    2011/04/19  東北地方太平洋沖地震対応
      * 01.00.05    井上    2019/01/10  修正履歴コメント削除
      ************************************************************
       01  SKT3310TMP-REC.
           03  SKT3310TMP-KEY.
               05  SKT3310TMP-SYUTURYOKU             PIC  9(01).
               05  SKT3310TMP-KOKUKUMI-KBN           PIC  9(01).
               05  SKT3310TMP-HKNJANUM               PIC  X(06).
               05  SKT3310TMP-SRYYM                  PIC  9(06).
      *    平成21年台風第9号対応
               05  SKT3310TMP-GENMEN                 PIC  X(05).
           03  SKT3310TMP-HKN-AREA.
               05  SKT3310TMP-HKN-TBL  OCCURS  11.      
                   07  SKT3310TMP-NYUGAI-TBL  OCCURS  2.
                       09  SKT3310TMP-KENSU              PIC  9(04).
                       09  SKT3310TMP-NISSU              PIC  9(04).
                       09  SKT3310TMP-TENSU              PIC  9(09).
                       09  SKT3310TMP-YKZFTN             PIC  9(07).
                       09  SKT3310TMP-ITBFTN             PIC  9(09).
                   07  SKT3310TMP-SKENSU                 PIC  9(04).
                   07  SKT3310TMP-SNISSU                 PIC  9(04).
                   07  SKT3310TMP-SKINGK                 PIC  9(09).
                   07  SKT3310TMP-SFTNGK                 PIC  9(09).
               05  SKT3310TMP-ZAITAKU-TBL OCCURS 6.
                   07  SKT3310TMP-ZKENSU                 PIC  9(04).
                   07  SKT3310TMP-ZNISSU                 PIC  9(04).
                   07  SKT3310TMP-ZTENSU                 PIC  9(09).
                   07  SKT3310TMP-ZITBFTN                PIC  9(09).
           03  SKT3310TMP-KOH-AREA.
               05  SKT3310TMP-KOH-TBL      OCCURS  20.
                   07  SKT3310TMP-KOHNUM                 PIC  X(03).
                   07  SKT3310TMP-KOHNAME                PIC  X(10).
                   07  SKT3310TMP-K-NYUGAI-TBL  OCCURS  2.
                       09  SKT3310TMP-K-KENSU            PIC  9(05).
                       09  SKT3310TMP-K-NISSU            PIC  9(05).
                       09  SKT3310TMP-K-TENSU            PIC  9(09).
                       09  SKT3310TMP-K-YKZFTN           PIC  9(07).
                       09  SKT3310TMP-K-ITBFTN           PIC  9(07).
                   07  SKT3310TMP-K-SKENSU               PIC  9(05).
                   07  SKT3310TMP-K-SNISSU               PIC  9(05).
                   07  SKT3310TMP-K-SKINGK               PIC  9(09).
                   07  SKT3310TMP-K-SFTNGK               PIC  9(09).
           03  SKT3310TMP-BIKO-AREA.
               05  SKT3310TMP-BIKO-TBL      OCCURS  20.
                   07  SKT3310TMP-BKOHNUM                PIC  X(03).
                   07  SKT3310TMP-BKOHNAME               PIC  X(10).
                   07  SKT3310TMP-BNYUGAIKBN             PIC  X(01).
                   07  SKT3310TMP-BKENSU                 PIC  9(05).
                   07  SKT3310TMP-BITBFTN                PIC  9(09).
                   07  SKT3310TMP-BYKZFTN                PIC  9(09).
               05  SKT3310TMP-BIKO-TBL-R.
                   07  SKT3310TMP-BIKO-TBL-RR  OCCURS  20.
                       09  SKT3310TMP-BKOHNUM-R          PIC  X(04).
                       09  SKT3310TMP-BKOHNAME-R         PIC  X(10).
                       09  SKT3310TMP-BKENSU-R           PIC  9(05).
                       09  SKT3310TMP-BITBFTN-R          PIC  9(09).
                       09  SKT3310TMP-BYKZFTN-R          PIC  9(09).
           03  SKT3310TMP-BIKO-AREA2.
               05  SKT3310TMP-BIKO-TBL2      OCCURS  20.
                   07  SKT3310TMP-BSKOHNUM               PIC  X(03).
                   07  SKT3310TMP-BSKOHNAME              PIC  X(10).
                   07  SKT3310TMP-BSNYUGAIKBN            PIC  X(01).
                   07  SKT3310TMP-BSKENSU                PIC  9(05).
                   07  SKT3310TMP-BSKINGK                PIC  9(09).
                   07  SKT3310TMP-BSFTNGK                PIC  9(09).
           03  SKT3310TMP-41-TBL.
               05  SKT3310TMP-41-KOHNUM                  PIC  X(03).
               05  SKT3310TMP-41-KOHNAME                 PIC  X(10).
               05  SKT3310TMP-41-NYUGAI-TBL  OCCURS  2.
                   07  SKT3310TMP-K41-KENSU              PIC  9(05).
                   07  SKT3310TMP-K41-NISSU              PIC  9(05).
                   07  SKT3310TMP-K41-TENSU              PIC  9(09).
                   07  SKT3310TMP-K41-YKZFTN             PIC  9(07).
                   07  SKT3310TMP-K41-ITBFTN             PIC  9(07).
               05  SKT3310TMP-K41-SKENSU                 PIC  9(05).
               05  SKT3310TMP-K41-SNISSU                 PIC  9(05).
               05  SKT3310TMP-K41-SKINGK                 PIC  9(09).
               05  SKT3310TMP-K41-SFTNGK                 PIC  9(09).
