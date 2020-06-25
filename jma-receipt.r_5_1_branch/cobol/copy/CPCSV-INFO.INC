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
      *    ＣＳＶ用ＤＢ                   (CPCSV-INFO.INC)       *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  CSV-INFO-REC.
      *
          03  CSV-INFO-KEY.
      *
               05  CSV-INFO-HOSPNUM        PIC 9(2).
      *ＫＥＹ
               05  CSV-INFO-TBL-KEY        PIC X(08).
      *診療年月
               05  CSV-INFO-SRYYM          PIC X(06).
      *シェルＩＤ
               05  CSV-INFO-SHELLID        PIC X(20).
      *連番
               05  CSV-INFO-RENNUM         PIC 9(03).
      *処理内連番
               05  CSV-INFO-SHORI-RENNUM   PIC 9(03).
      *出力連番
               05  CSV-INFO-SEQ-NO         PIC 9(06).
      *入外区分
           03  CSV-INFO-NYUGAIKBN          PIC X(01).
      *請求年月日
           03  CSV-INFO-SKYYMD             PIC X(08).
      *審査支払機関
           03  CSV-INFO-TEISYUTUSAKI       PIC X(01).
      *ＰＴＩＤ
           03  CSV-INFO-PTID               PIC 9(10).
      *ＣＳＶデータ
           03  CSV-INFO-CSVDATA            PIC X(2000).
      *作成日
           03  CSV-INFO-CREYMD             PIC X(08).
      *作成時間
           03  CSV-INFO-CREHMS             PIC X(06).
