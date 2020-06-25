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
      *    チェック確認データ振り分け   COPY (CPRECORCGD05.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.10    ひさなが     01/10/15  印刷順を患者番号へ変更
      *
      ************************************************************
      *01  DID5-REC.
           03  DID5-KEY.
      *メッセージＩＤ
               05  DID5-MESGID       PIC  X(006).
      *医療機関ＩＤ
               05  DID5-HOSPID       PIC  X(024).
      *修正/*開始*01.10.15********************************************
      *患者ＩＤ
      *         05  DID5-PTID         PIC  9(010).
      *患者番号
               05  DID5-PTNUM        PIC  X(020).
      *修正/*終了*01.10.15********************************************
      *データ入力順
               05  DID5-SEQNO        PIC  9(009).
      *患者漢字氏名
           03  DID5-NAME             PIC  X(100).
      *患者年齢
           03  DID5-NENREI           PIC  9(003).
      *メッセージ
           03  DID5-MESG             PIC  X(200).
