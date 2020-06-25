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
      *    お薬手帳データ連携       COPY (CPOKUSURIDATA.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  OKUSURI-DATA-REC.
      *キー
           03  OKUDAT-KEY.
      *データキー
               05  OKUDAT-DATA-KEY          PIC  9(16).
      *ＲＰ番号
               05  OKUDAT-RP                PIC  9(03).
      *ＲＰ内連番
               05  OKUDAT-RP-SEQ            PIC  9(03).
      *データ識別
           03  OKUDAT-DATA-CLASS            PIC  9(03).
      *名称
           03  OKUDAT-NAME                  PIC  X(200).
      *数量
           03  OKUDAT-SURYO                 PIC  9(5)V9(5).
      *単位名
           03  OKUDAT-TANINAME              PIC  X(20).
      *コード区分
           03  OKUDAT-CODE-CLASS            PIC  9(02).
      *コード
           03  OKUDAT-CODE                  PIC  X(20).
      *端末ＩＤ
           03  OKUDAT-TERMID                PIC  X(16).
      *オペーレータＩＤ
           03  OKUDAT-OPID                  PIC  X(16).
      *作成年月日
           03  OKUDAT-CREYMD                PIC  X(08).
      *更新年月日
           03  OKUDAT-UPYMD                 PIC  X(08).
      *更新時間
           03  OKUDAT-UPHMS                 PIC  X(06).
