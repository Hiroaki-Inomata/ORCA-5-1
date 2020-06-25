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
      *  感染症プルーフ情報                          (CPINFECTION.INC)*
      *****************************************************************
      *  修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
      *01  INFECTION-REC.
           03  INFECT-KEY.
      *医療機関ＩＤ
               05  INFECT-HOSPNUM      PIC 9(2).
      *報告日
               05  INFECT-SENDYMD      PIC X(8).
      *患者ＩＤ
               05  INFECT-PTID         PIC 9(10).
      *病原体区分
               05  INFECT-DISEASE      PIC 9(02).
      *診療日
               05  INFECT-SRYYMD       PIC X(08).
      *診療コード（診療行為）
           03  INFECT-SRYCD            PIC X(09).
      *診療コード（医薬品）
           03  INFECT-MEDSRYCD         PIC X(09).
      *初診再診区分
           03  INFECT-SYOSAI           PIC 9(01).
      *初診算定日
           03  INFECT-SYOSINYMD        PIC X(08).
      *病名コード
           03  INFECT-BYOMEICD         PIC X(07).
      *診療開始日
           03  INFECT-BYOSRYYMD        PIC X(08).
      *転帰区分
           03  INFECT-TENKIKBN         PIC X(01).
      *転帰日
           03  INFECT-TENKIYMD         PIC X(08).
      *端末ＩＤ
           03  INFECT-TERMID           PIC X(16).
      *オペレータＩＤ
           03  INFECT-OPID             PIC X(16).
      *作成年月日
           03  INFECT-CREYMD           PIC X(08).
      *更新年月日
           03  INFECT-UPYMD            PIC X(08).
      *更新時間
           03  INFECT-UPHMS            PIC X(06).
