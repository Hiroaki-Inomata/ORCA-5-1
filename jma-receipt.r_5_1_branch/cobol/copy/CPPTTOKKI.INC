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
      *  患者特記事項テーブル       (CPPTTOKKI.INC)
      *****************************************************************
      *  修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
      *01  PTTOKKI-REC.
           03  PTTOKKI-KEY.
      *医療機関ＩＤ
               05  PTTOKKI-HOSPNUM     PIC 9(2).
      *入外区分
               05  PTTOKKI-NYUGAIKBN   PIC X(01).
      *患者ＩＤ
               05  PTTOKKI-PTID        PIC 9(10).
      *連番号
               05  PTTOKKI-RENNUM      PIC 9(02).
      *特記事項コード
           03  PTTOKKI-CD              PIC X(02).
      *特記事項内容
           03  PTTOKKI-NAIYO           PIC X(12).
      *開始年月
           03  PTTOKKI-STYM            PIC X(06).
      *終了年月
           03  PTTOKKI-EDYM            PIC X(06).
      *端末ＩＤ
           03  PTTOKKI-TERMID          PIC X(16).
      *オペーレータＩＤ
           03  PTTOKKI-OPID            PIC X(16).
      *作成年月日
           03  PTTOKKI-CREYMD          PIC X(08).
      *更新年月日
           03  PTTOKKI-UPYMD           PIC X(08).
      *更新時間
           03  PTTOKKI-UPHMS           PIC X(06).
