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
      *    総括表一時ファイル                                    *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2009/08/25  SKT1000TMP-TYOKI追加
      *  01.00.02    吉川    2011/03/10  KBNを数値から文字に変更
      *  01.00.03    吉川    2011/04/08  東北地方太平洋沖地震対応
      *  01.00.04    吉川    2011/10/28  市町村毎で改頁されないように修正
      *  01.00.05    吉川    2011/11/04  印字順の修正
      *  01.00.06    吉川    2012/01/20  災害がある場合、県外の国保と
      *                                  組合国保が別帳票になるのを修正
      *  01.00.07    門間    2018/10/02  不要コメントを削除
      ************************************************************
       01  SKT1000TMP-REC.
           03  SKT1000TMP-KEY.
               05  SKT1000TMP-SRYYM          PIC  X(06).
               05  SKT1000TMP-PREFKBN        PIC  9(01).
               05  SKT1000TMP-TYOKI          PIC  X(01).
               05  SKT1000TMP-GENMEN         PIC  X(01).
               05  SKT1000TMP-ADRS           PIC  X(40).
               05  SKT1000TMP-KUMIAIKBN      PIC  X(01).
               05  SKT1000TMP-HKNJANUM       PIC  X(08).
               05  SKT1000TMP-ZAITAKU        PIC  X(01).
               05  SKT1000TMP-SYUBETU        PIC  9(01).
               05  SKT1000TMP-JYUNJO         PIC  9(02).
               05  SKT1000TMP-KBN            PIC  X(01).
               05  SKT1000TMP-KYURATE        PIC  9(02).
           03  SKT1000TMP-DATA.
               05  SKT1000TMP-KENSU          PIC  9(06).
               05  SKT1000TMP-KKENSU         PIC  9(06).
               05  SKT1000TMP-FKENSU         PIC  9(06).
               05  SKT1000TMP-TENSU          PIC  9(10).
               05  SKT1000TMP-NYUGAIKBN      PIC  9(01).
      *
