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
      *----------------------------------------------------------*
      *    入院薬剤情報出力パラメタ          COPY (CPORCHCN30.INC)
      *                                          (ORCHCN30 で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.05.00     NACL-森脇    09/06/22  小数点以下５桁
      * 04.05.00     NACL-多々納  09/07/23  在宅薬剤、コメント追加
      * 04.06.00     NACL-多々納  11/02/22  用法コメント対応追加
      * 04.08.00     NACL-多々納  16/11/XX  用法コード数量追加
      * 05.00.00     NACL-多々納  17/05/16  病棟番号追加
      ************************************************************
      *
       01  ORCHCN30AREA.
      *    ドクター
           03  ORCHCN30-DRCD            PIC X(05).
      *    病棟
           03  ORCHCN30-BTUNAME         PIC X(20).
      *    病室
           03  ORCHCN30-ROOMNO          PIC X(06).
      *H29.5
      *    病棟番号
           03  ORCHCN30-BTUNUM          PIC X(02).
      *    最大件数
           03  ORCHCN30-MAX             PIC 9(04).
      *
           03  ORCHCN30-DATA-TBL        OCCURS   99.
      *        薬剤コード
               05  ORCHCN30-SRYCD       PIC X(09).
      *        用法コード（空白あり）
               05  ORCHCN30-YSRYCD      PIC X(09).
      *        用法コメント名称
      *R02.4   05  ORCHCN30-YSRYCD-NAME PIC X(80).
               05  ORCHCN30-YSRYCD-NAME PIC X(140).
      *        数量（１日量／外用は総量）
               05  ORCHCN30-SURYO       PIC 9(05)V9(05).
      *        日数（回数）（外用は１回）
               05  ORCHCN30-KAISU       PIC 9(03).
      *        （１：内服、２：頓服、３：外用、４：在宅）
               05  ORCHCN30-ZAIKBN      PIC X(01).
      *        用法毎の用量（１：朝、２：昼、３：夜、４：寝、５：起床）
               05  ORCHCN30-YOURYOU-TBL.
                   07  ORCHCN30-YOURYOU     PIC 9(05)V9(05)
                                           OCCURS   5.
      *        外用日数（日数入力があった時のみ）
               05  ORCHCN30-SRYKAISU        PIC 9(03).
      *        換算値数量（１日量）
               05  ORCHCN30-KANSURYO    PIC 9(05)V9(05).
      *H28.11
      *        用法コードの数量
      *           （１：朝、２：昼、３：夜、４：寝、５：起床）
               05  ORCHCN30-INPUTSURYOU-TBL.
                   07  ORCHCN30-INPUTSURYOU    PIC 9(05)V9(05)
                                               OCCURS   5.
      *        用法コード２（表示のみ）
               05  ORCHCN30-YOHOU-G.
                   07  ORCHCN30-YOHOU-TBL      OCCURS  5.
                       09  ORCHCN30-YOHOU       PIC X(09).
      *                用法コメント名称
      *R02.4           09  ORCHCN30-YOHOU-NAME  PIC X(80).
                       09  ORCHCN30-YOHOU-NAME  PIC X(140).
      *        コメントコード
               05  ORCHCN30-COMMENT-G.
                   07  ORCHCN30-COMMENT-OCCURS  OCCURS  10.
                       09  ORCHCN30-COMMENT-CD   PIC X(09).
      *R02.4           09  ORCHCN30-COMMENT-MEI  PIC X(80).
                       09  ORCHCN30-COMMENT-MEI  PIC X(140).
      *H28.10
      *                1:用法コードの数量編集分
                       09  ORCHCN30-COMHENKBN    PIC X(01).
      *
