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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    門間    2018/02/05  保険番号063、072〜075は組合健保に〇を印字
      *  01.00.02    井上    2018/11/01  修正履歴コメント削除
      *  01.00.03    門間    2019/05/09  元号改正対応
      ************************************************************
        01  SC05.
            03  SC05-SRYYM               PIC  X(16).
            03  SC05-FTNJANUM            PIC  X(08).
            03  SC05-JKYSNUM             PIC  X(20).
            03  SC05-TEKSTYMD            PIC  X(22).
            03  SC05-TEKEDYMD            PIC  X(22).
            03  SC05-JGNGAK              PIC  X(09).
            03  SC05-NAME                PIC  X(50).
            03  SC05-KANANAME            PIC  X(40).
            03  SC05-KANANAME2           PIC  X(50).
            03  SC05-SEX                 PIC  X(02)  OCCURS  2.
            03  SC05-NENGOU              PIC  X(02)  OCCURS  4.
            03  SC05-BIRTHDAY            PIC  X(08).
            03  SC05-HOSPNAME            PIC  X(100).
            03  SC05-HOSPADRS            PIC  X(100).
            03  SC05-KAISETU             PIC  X(40).
            03  SC05-HONKZKKBN           PIC  X(02)  OCCURS  2.
            03  SC05-HKNKBN              PIC  X(02)  OCCURS  11.
            03  SC05-HKNKBN2             PIC  X(04).
            03  SC05-BYOMEI              PIC  X(54)  OCCURS  5.
            03  SC05-SRYYMD              PIC  X(26)  OCCURS  5.
            03  SC05-TENKI               PIC  X(10)  OCCURS  5.
            03  SC05-TENKIYMD            PIC  X(06)  OCCURS  5.
            03  SC05-JINISSU             PIC  X(02).
            03  SC05-SHOSHINTIME         PIC  X(10).
            03  SC05-TENSU               PIC  X(07)  OCCURS  35.
            03  SC05-KAISU               PIC  X(03)  OCCURS  35.
            03  SC05-KOUTEN              PIC  X(07)  OCCURS  35.
            03  SC05-KHNTEN              PIC  X(07)  OCCURS  5.
            03  SC05-KHNTEN1             PIC  X(07)  OCCURS  2.
            03  SC05-KHNTEN2             PIC  X(07)  OCCURS  4.
            03  SC05-KHNTEN3             PIC  X(07).
            03  SC05-SONOTA              PIC  X(10).
            03  SC05-SRYKBN              PIC  X(02)  OCCURS  35.
            03  SC05-TEKIYO              PIC  X(52)  OCCURS  35.
            03  SC05-BODY                PIC  X(52).
            03  SC05-TOTALTEN            PIC  X(07).
            03  SC05-SEIKYUGK            PIC  X(10)  OCCURS  4.
            03  SC05-YKZFTN              PIC  X(10)  OCCURS  4.
            03  SC05-ITBFTN              PIC  X(10)  OCCURS  4.
            03  SC05-BIRTHDAY2           PIC  X(10).
