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
      *         レセプト編綴順詳細設定 (CPRECE-PUTB.INC)         *
      *         ＢＯＤＹ                                         *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.02.01    NACL-藤原    08/03/31  主キー更新用項目追加
      ************************************************************
      *01  RECE-PUTB-REC.
      *医療機関ＩＤ
            02  RECE-PUTB-HOSPNUM      PIC 9(2).
      *
            02  RECE-PUTB-PREFKBN      PIC X(1).
      *
            02  RECE-PUTB-SYUBETU-SRT1 PIC S9(9)   BINARY.
      *
            02  RECE-PUTB-GRP-NO1      PIC X(3).
      *
            02  RECE-PUTB-SYUBETU-SRT2 PIC S9(9)   BINARY.
      *
            02  RECE-PUTB-GRP-NO2      PIC X(3).
      *
            02  RECE-PUTB-SYUBETU-GAI  PIC X(4).
      *
            02  RECE-PUTB-SYUBETU-NYU  PIC X(4).
      *
            02  RECE-PUTB-SYUBETU-UNDER-NO1
                                       PIC X(2).
      *
            02  RECE-PUTB-SYUBETU-UNDER-NO2
                                       PIC X(2).
      *
            02  RECE-PUTB-KOUHI-NO     PIC S9(3).
      *
            02  RECE-PUTB-KOUHI-NUM    PIC X(3).
      *
            02  RECE-PUTB-SYUBETU-GAI-SRT1
                                       PIC X(4).
      *
            02  RECE-PUTB-SYUBETU-NYU-SRT1
                                       PIC X(4).
      *有効開始年月日
            02  RECE-PUTB-STYUKYMD     PIC X(8).
      *有効終了年月日
            02  RECE-PUTB-EDYUKYMD     PIC X(8).
      *端末ＩＤ
            02  RECE-PUTB-TERMID       PIC X(64).
      *オペレータＩＤ
            02  RECE-PUTB-OPID         PIC X(16).
      *作成年月日
            02  RECE-PUTB-CREYMD       PIC X(8).
      *更新年月日
            02  RECE-PUTB-UPYMD        PIC X(8).
      *更新時間
            02  RECE-PUTB-UPHMS        PIC X(6).
      *
      *主キー更新用項目
            02  RECE-PUTB-UPSTYUKYMD   PIC X(08).
            02  RECE-PUTB-UPEDYUKYMD   PIC X(08).
