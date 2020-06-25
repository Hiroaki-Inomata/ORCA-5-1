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
      *    請求管理           COPY (CPRCF010.INC)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     門間         10/12/15  コメント履歴削除
      * 01.00.02     門間         18/05/18  公費単独の集計を修正
      ********************************************************************
       01  L200TMP-REC.
      *ＫＥＹ
           03  L200TMP-KEY.
               05  L200TMP-NYUGAIKBN-KEY    PIC  X(01).
               05  L200TMP-SRYKA-KEY        PIC  X(02).
               05  L200TMP-SORT-KEY         PIC  X(50).
               05  L200TMP-KEY2.
      *医療機関ＩＤ
                   07  L200TMP-HOSPNUM          PIC  9(2).
      *診療年月
                   07  L200TMP-SRYYM            PIC  9(06).
      *入外区分
                   07  L200TMP-NYUGAIKBN        PIC  X(01).
      *患者ＩＤ
                   07  L200TMP-PTID             PIC  9(10).
      *レセプト科（空白の時ゼロをセット）
                   07  L200TMP-RECEKA           PIC  X(02).
      *審査支払機関
                   07  L200TMP-TEISYUTUSAKI     PIC  9(01).
      *レセプト種別
                   07  L200TMP-RECESYUBETU      PIC  X(04).
      *保険者番号
                   07  L200TMP-HKNJANUM         PIC  X(08).
      *補助区分（船員保険のときのみ）
                   07  L200TMP-HOJOKBN-KEY      PIC  X(01).
      *公費ＩＤ（医保：公費単独のみ、労災、自賠責：保険ＩＤ）
                   07  L200TMP-KOHID-KEY        PIC  9(10).
      *適用開始年月日
                   07  L200TMP-TEKSTYMD         PIC  X(08).
      *分割区分
                   07  L200TMP-RECEIPT-KBN      PIC  X(10).
      *患者番号
           03  L200TMP-PTNUM                PIC  X(20).
      *年齢
           03  L200TMP-AGE                  PIC  9(03).
      *給付割合
           03  L200TMP-KYURATE              PIC  9(03).
      *在総診区分
           03  L200TMP-ZAITAKU              PIC  X(01).
      *主保険−保険番号
           03  L200TMP-HKNNUM               PIC  X(03).
      *主保険−本人家族区分
           03  L200TMP-HONKZKKBN           PIC  X(01).
      *老人保険−保険番号
           03  L200TMP-RJNHKNNUM            PIC  X(03).
      *氏名
           03  L200TMP-NAME                 PIC  X(50).
      *公費負担者番号
           03  L200TMP-FTNJANUM1            PIC  X(20).
           03  L200TMP-FTNJANUM2            PIC  X(20).
      *公費受給者番号
           03  L200TMP-JKYSNUM1             PIC  X(20).
           03  L200TMP-JKYSNUM2             PIC  X(20).
      *誕生日
           03  L200TMP-TANJO                PIC  X(08).
      *実日数
           03  L200TMP-NISSU                PIC  9(02).
      *総点数
           03  L200TMP-TENSU                PIC  9(09).
      *負担額
           03  L200TMP-ITBFTN               PIC  9(09).
      *食事日数
           03  L200TMP-SNISSU               PIC  9(02).
      *食事療養費
           03  L200TMP-KINGK                PIC  9(09).
      *標準負担額
           03  L200TMP-FTNGK                PIC  9(09).
