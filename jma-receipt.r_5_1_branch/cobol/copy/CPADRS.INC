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
      *****************************************************************
      *    住所                COPY (CPADRS.INC)        *
      *****************************************************************
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *01  ADRS-REC.
           03  ADRS-KEY.
               05  ADRS-HOSPNUM        PIC 9(2).
      *地方公共団体コード
               05  ADRS-LPUBCD         PIC X(05).
      *郵便番号
               05  ADRS-POST           PIC X(07).
      *枝番
               05  ADRS-EDANUM         PIC 9(02).
      *都道府県名カナ
           03  ADRS-PREFKANA           PIC X(40).
      *市町村名カナ
           03  ADRS-CITYKANA           PIC X(200).
      *町域名カナ(200->400)4.6.0
           03  ADRS-TOWNKANA           PIC X(400).
      *都道府県名
           03  ADRS-PREFNAME           PIC X(20).
      *市町村名
           03  ADRS-CITYNAME           PIC X(100).
      *町域名(100->200)4.6.0
           03  ADRS-TOWNNAME           PIC X(200).
      *編集後住所カナ(440->640)4.6.0
           03  ADRS-EDITADRS-KANA      PIC X(640).
      *編集後住所(220->320)4.6.0
           03  ADRS-EDITADRS-NAME      PIC X(320).
      *町域分割フラグ
           03  ADRS-TOWNDIVFLG         PIC X(01).
      *複数町域フラグ
           03  ADRS-MANYTOWNFLG        PIC X(01).
      *補足（通り名）4.6.0
           03  ADRS-TOWNNAME2          PIC X(1200).
      *
