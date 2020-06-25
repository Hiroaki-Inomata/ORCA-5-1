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
      *************************************************************
      *    ＣＳＶ用ＤＢ制御サブパラメタ    CPORCSCSVINFO.INC      *
      *************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSCSVINFOAREA.
      *    処理区分                              
           03  SCSVINFO-MODE           PIC X(03).
      *    復帰コード     
           03  SCSVINFO-RETURN         PIC S9(03).
      *    ＫＥＹ
           03  SCSVINFO-TBL-KEY        PIC X(08).
      *    シェルＩＤ
           03  SCSVINFO-SHELLID        PIC X(20).
      *    処理内連番
           03  SCSVINFO-SHORI-RENNUM   PIC 9(03).
      *    連番
           03  SCSVINFO-RENNUM         PIC 9(03).
      *    データ連番
           03  SCSVINFO-SEQ-NO         PIC 9(06).
      *    入外区分
           03  SCSVINFO-NYUGAIKBN      PIC X(01).
      *    診療年月
           03  SCSVINFO-SRYYM          PIC X(06).
      *    請求年月日
           03  SCSVINFO-SKYYMD         PIC X(08).
      *    審査支払機関
           03  SCSVINFO-TEISYUTUSAKI   PIC X(01).
      *    ＰＴＩＤ
           03  SCSVINFO-PTID           PIC 9(10).
      *    ＣＳＶデータ
           03  SCSVINFO-CSVDATA        PIC X(2000).
