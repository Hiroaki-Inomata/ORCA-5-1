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
      *    公費一時ファイル     COPY (CPSKT3900TMP.INC)            *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     山口       05/04/12  KENSUのバイト数変更
      *                                   GENKENSU追加
      *                                   SKT3900TMP-HONKZKKBNをキーから外す
      * 01.00.02     門間       19/01/07  修正履歴コメント削除
      ************************************************************
       01  SKT3900TMP-REC.
           03  SKT3900TMP-KEY.
               05  SKT3900TMP-SEIDOKBN          PIC  9(01).
               05  SKT3900TMP-SYUTURYOKU        PIC  9(01).
               05  SKT3900TMP-HKNJANUM          PIC  X(06).
               05  SKT3900TMP-ZAITAKU           PIC  X(01).
           03  SKT3900TMP-HONKZKKBN         PIC  9(01).
           03  SKT3900TMP-DATA.
               05  SKT3900TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT3900TMP-KENSU         PIC  9(05).
                   07  SKT3900TMP-TENSU         PIC  9(10).
                   07  SKT3900TMP-KOHTENSU      PIC  9(10).
                   07  SKT3900TMP-YKZFTN        PIC  9(10).
                   07  SKT3900TMP-RJNFTN        PIC  9(10).
                   07  SKT3900TMP-GENKENSU      PIC  9(03).
               05  SKT3900TMP-SKJ-KENSU         PIC  9(05).
               05  SKT3900TMP-SKJ-KINGAKU       PIC  9(10).
               05  SKT3900TMP-SKJ-FTNGAKU       PIC  9(10).
