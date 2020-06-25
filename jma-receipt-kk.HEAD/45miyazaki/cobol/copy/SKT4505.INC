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
      *  後期高齢者医療 請求添付票（宮崎県）     COPY (SKT4505.INC)
      *
      *  作成日付    作業者        記述
      *  08/05/28    吉川          新規作成
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
       01  SKT4505.
      *    診療年月
           03  SKT4505-SRYYM              PIC X(16).
      *    標榜科名
           03  SKT4505-HYOUBOUKA1         PIC X(20).
           03  SKT4505-HYOUBOUKA2         PIC X(20).
           03  SKT4505-HYOUBOUKA3         PIC X(20).
      *    医療機関コード
           03  SKT4505-HOSPCD             PIC X(14).
      *    郵便番号
           03  SKT4505-POST3              PIC X(03).
           03  SKT4505-POST4              PIC X(04).
      *    医療機関住所
           03  SKT4505-ADRS               PIC X(100).
      *    医療機関名
           03  SKT4505-HOSPNAME           PIC X(100).
      *    開設者氏名
           03  SKT4505-KAISETUNAME        PIC X(40).
      *    電話番号
           03  SKT4505-TEL1               PIC X(05).
           03  SKT4505-TEL2               PIC X(04).
           03  SKT4505-TEL3               PIC X(06).
      *    県内県外区分
           03  SKT4505-KENNAI             PIC X(02).
           03  SKT4505-KENGAI             PIC X(02).
