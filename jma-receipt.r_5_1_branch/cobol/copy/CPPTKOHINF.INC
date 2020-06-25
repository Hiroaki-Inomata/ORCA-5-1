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
      *    患者公費情報　    　 COPY  (CPPTKOHINF.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  02/09/09  支払区分追加
      ************************************************************
      *01  PTKOHINF-REC.
      *キー
           03  PTKOH-KEY.
      *医療機関ＩＤ
               05  PTKOH-HOSPNUM            PIC 9(2).
      *患者ＩＤ
               05  PTKOH-PTID              PIC  9(10).
      *公費ＩＤ
               05  PTKOH-KOHID             PIC  9(10).
      *保険番号（公費）
           03  PTKOH-KOHNUM                PIC  X(03).
      *支払区分
           03  PTKOH-PAYKBN                PIC  X(02).
      *負担者番号
           03  PTKOH-FTNJA-G.
      *負担者番号
               05  PTKOH-FTNJANUM          PIC  X(08).
      *受給者番号
           03  PTKOH-JKYSNUM               PIC  X(20).
      *適用開始年月日
           03  PTKOH-TEKSTYMD              PIC  X(08).
      *適用終了年月日
           03  PTKOH-TEKEDYMD              PIC  X(08).
      *確認年月日
           03  PTKOH-KAKUNINYMD            PIC  X(08).
      *削除区分
           03  PTKOH-SAKJOKBN              PIC  X(01).
      *期限切れ区分
           03  PTKOH-TEKEDCHKKBN           PIC  X(01).
      *端末ＩＤ
           03  PTKOH-TERMID                PIC  X(16).
      *オペーレータＩＤ
           03  PTKOH-OPID                  PIC  X(16).
      *作成年月日
           03  PTKOH-CREYMD                PIC  X(08).
      *更新年月日
           03  PTKOH-UPYMD                 PIC  X(08).
      *更新時間
           03  PTKOH-UPHMS                 PIC  X(06).
