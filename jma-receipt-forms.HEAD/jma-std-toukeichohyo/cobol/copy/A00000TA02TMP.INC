      ******************************************************************
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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ******************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 診療行為別集計表
      *  管理者            : 
      *  Maj/Min/Rev  修正者  日付        内容
      *   01.00.00    田中    2011/02/14  新規作成
      *   01.00.01    門間    2014/05/22  薬価／点数、件数、使用量を小数点第2位まで対応
      *   01.00.02    門間    2017/06/30  外来金額、入院金額欄の桁数を変更
      *******************************************************************
       01  TA02TMP-REC.
      *    KEY情報
           03  TA02TMP-KEY.
      *    処理区分
             05  TA02TMP-PRTKBN           PIC 9(05).
      *    印字順序
             05  TA02TMP-SORTKBN          PIC 9(02).
      *    診療区分
             05  TA02TMP-SRYKBN           PIC X(02).
      *    診療名
             05  TA02TMP-SRYNAME          PIC X(40).
      *    診療コード
             05  TA02TMP-SRYCD            PIC X(09).
      *    薬価
             05  TA02TMP-YAKKA            PIC S9(07)V9(02).
      *    単価
             05  TA02TMP-TANKA            PIC S9(07)V9(02).
      *    明細テーブル
           03  TA02TMP-SRY-TBL.
      *    外来件数
             05  TA02TMP-GAIRAI-KEN       PIC 9(07)V9(02).
      *    入院件数
             05  TA02TMP-NYUIN-KEN        PIC 9(07)V9(02).
      *    合計件数
             05  TA02TMP-TOTAL-KEN        PIC 9(08)V9(02).
      *    外来使用量
             05  TA02TMP-GAIRAI-SIYO      PIC 9(07)V9(02).
      *    入院使用量
             05  TA02TMP-NYUIN-SIYO       PIC 9(07)V9(02).
      *    合計使用量
             05  TA02TMP-TOTAL-SIYO       PIC 9(08)V9(02).
      *    外来金額
             05  TA02TMP-GAIRAI-KIN       PIC 9(09).
      *    入院金額
             05  TA02TMP-NYUIN-KIN        PIC 9(09).
      *    合計金額
             05  TA02TMP-TOTAL-KIN        PIC 9(09).
