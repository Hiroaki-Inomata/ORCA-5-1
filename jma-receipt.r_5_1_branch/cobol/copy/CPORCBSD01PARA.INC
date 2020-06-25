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
      *----------------------------------------------------------------*
      *    データチェック一括処理固有パラメータ
      *                         COPY (CPORCBSD01PARA.INC)              *
      *    ※注意事項※
      *    CHK004(2000)でORCBSD01PARAの内容をそのまま記録している為、
      *    項目追加は必ず既存項目の最後尾から行うこと。
      *----------------------------------------------------------------*
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  ORCBSD01PARA.
           03  ORCBSD01-IKTKBT         PIC  X(01).
           03  ORCBSD01-SRYYM          PIC  X(06).
           03  ORCBSD01-NYUGAIKBN      PIC  X(01).
           03  ORCBSD01-CHKKBN-G.
               05  ORCBSD01-CHKKBN     PIC  X(01)  OCCURS 50.
           03  ORCBSD01-SYAHOPROCKBN   PIC  X(01).
           03  ORCBSD01-KOKUHOPROCKBN  PIC  X(01).
           03  ORCBSD01-RSIPROCKBN     PIC  X(01).
           03  ORCBSD01-JBIPROCKBN     PIC  X(01).
           03  ORCBSD01-JIHIPROCKBN    PIC  X(01).
           03  ORCBSD01-INGAIPROCKBN   PIC  X(01).
           03  ORCBSD01-PRTSRTKBN      PIC  X(01).
           03  ORCBSD01-PRTPTLSTKBN    PIC  X(01).
           03  ORCBSD01-SRYYMDKBN      PIC  X(01).
           03  ORCBSD01-STSRYYMD       PIC  9(02).
           03  ORCBSD01-EDSRYYMD       PIC  9(02).
           03  ORCBSD01-KGIPROCKBN     PIC  X(01).
           03  ORCBSD01-KOUKIPROCKBN   PIC  X(01).
           03  ORCBSD01-CHIKENPROCKBN  PIC  X(01).
           03  ORCBSD01-DAISANPROCKBN  PIC  X(01).
           03  ORCBSD01-FROM-API       PIC  X(01).
           03  ORCBSD01-KARTE-UID      PIC  X(36).
           03  ORCBSD01-ORCA-UID       PIC  X(36).
           03  ORCBSD01-DATA-ID        PIC  X(36).
           03  ORCBSD01-PRINT-ID       PIC  X(36).
