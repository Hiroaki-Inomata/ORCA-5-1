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
      *    入院帳票オーダ      COPY (CPORDERPRT.INC)                 *
      *****************************************************************
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.00.00     NACL-多々納  20/02/XX  コメント値桁数変更
      *****************************************************************
      *01  ORDERPRT-REC.
           03  ORDERPRT-KEY.
      *  医療機関ＩＤ
               05  ORDERPRT-HOSPNUM        PIC 9(2).
      *  患者ＩＤ
               05  ORDERPRT-PTID          PIC  9(10).
      *  患者番号
               05  ORDERPRT-PTNUM         PIC  X(20).
      *  入外区分
               05  ORDERPRT-NYUGAIKBN     PIC  X(01).
      *  診療科
               05  ORDERPRT-SRYKA         PIC  X(02).
      *  保険組合せ
               05  ORDERPRT-HKNCOMBI      PIC  X(04).
      *  システム日付
               05  ORDERPRT-SYSYMD        PIC  X(08).
      *  発行日
               05  ORDERPRT-PRINTYMD      PIC X(08).
      *  発行時間
               05  ORDERPRT-PRINTHMS      PIC X(06).
      *  帳票種類  １：入院処方箋、２：入院麻薬処方、３：臨時投薬
      *            ４：注射処方箋、５：注射麻薬処方、６：指示箋
               05  ORDERPRT-PRTKBN        PIC  X(01).
      *  番号
               05  ORDERPRT-ORDERNUM      PIC 9(02).
      *  枝番（剤毎に１からの連番）
               05  ORDERPRT-RENNUM        PIC 9(02).
      *同一番号はすべて同一内容
      *  ドクターコード
           03  ORDERPRT-DRCD              PIC X(05).
      *  診療区分
           03  ORDERPRT-SRYKBN            PIC X(02).
      *  診療種別
           03  ORDERPRT-SRYSYUKBN         PIC X(03).
      *  診療日付
           03  ORDERPRT-SRYYMD            PIC X(08).
      *枝番＝０１の時設定
      *  回数
           03  ORDERPRT-DAY-G.
               05  ORDERPRT-DAY           PIC 9(03)
                                       OCCURS   31.
      *診療内容（必須）
           03  ORDERPRT-SINRYO-G.
      *  診療コード
               05  ORDERPRT-SRYCD             PIC X(09).
      *  数量
               05  ORDERPRT-SRYSURYO          PIC 9(05)V9(5).
      *  数量（外用の個別数量）
               05  ORDERPRT-SRYSURYO2         PIC 9(05)V9(5).
      *  撮影回数・外用の個別回数
               05  ORDERPRT-SRYKAISU          PIC 9(03).
      *  入力コメント（フリーコメント等）
      *R02.4   05  ORDERPRT-INPUTCOMENT       PIC X(80).
               05  ORDERPRT-INPUTCOMENT       PIC X(140).
      *  換算値数量
               05  ORDERPRT-KANSURYO          PIC 9(05)V9(5).
      *
      *端末ＩＤ
           03  ORDERPRT-TERMID            PIC  X(64).
      *オペーレータＩＤ
           03  ORDERPRT-OPID              PIC  X(16).
      *作成年月日
           03  ORDERPRT-CREYMD            PIC  X(08).
      *更新年月日
           03  ORDERPRT-UPYMD             PIC  X(08).
      *更新時間
           03  ORDERPRT-UPHMS             PIC  X(06).

