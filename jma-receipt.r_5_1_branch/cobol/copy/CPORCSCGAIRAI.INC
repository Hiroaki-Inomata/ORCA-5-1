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
      *    入院診療行為更新パラメタ　　COPY (CPORCSCGAIRAI.INC)
      *                           使用プログラム (ORCGK08,ORCGK03)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSCGAIRAIAREA.
      *
      *区分（１：診療行為作成、２：診療行為削除、３：初診算定日のみ）
           03  ORCSCGAIRAI-KBN                 PIC  X(01).
      *Ｋ０３のみ
           03  ORCSCGAIRAI-K03-AREA.
      *    調整金
               05  ORCSCGAIRAI-CHOSEI          PIC S9(07).
      *    請求書作成区分
               05  ORCSCGAIRAI-HAKFLG          PIC X(01).
      *    今回請求額
               05  ORCSCGAIRAI-KON-SKYMONEY    PIC S9(07).
      *    入金額
               05  ORCSCGAIRAI-NYUKIN          PIC S9(07).
      *    入金方法
               05  ORCSCGAIRAI-NYUKIN-HOHO     PIC X(02).
      *   （１：まとめ継続、２：まとめ確定）
               05  ORCSCGAIRAI-SYORI           PIC 9(01).
      *    発行方法
               05  ORCSCGAIRAI-HAKHOUFLG       PIC 9(01).
      *    発行日
               05  ORCSCGAIRAI-HAKYMD          PIC X(08).
      *
      *    伝票番号
      *!       05  ORCSCGAIRAI-DENPNUM         PIC 9(07).
      *    受診履歴番号
      *!       05  ORCSCGAIRAI-JYURRK-RENNUM   PIC 9(01).
      *    収納領域
               05  ORCSCGAIRAI-SYUNOU-AREA.
                   07  ORCSCGAIRAI-SYUNOU-MAX  PIC 9(04).
                   07  ORCSCGAIRAI-SYUNOU-TBL  OCCURS  15.
                       09  ORCSCGAIRAI-SYUNOU-REC  PIC X(2000).
      *                受診履歴番号
                       09  ORCSCGAIRAI-JYURRK-RENNUM   PIC 9(01).
      *H18/9
      *                今回返金額
                       09  ORCSCGAIRAI-SYU-HENKIN      PIC S9(07).
                       09  ORCSCGAIRAI-SYU-HENKBN      PIC  X(01).
      *
