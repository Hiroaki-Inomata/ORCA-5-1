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
      *    公害レセプト・点数情報         COPY (CPRCF121.INC) 
      *                       診療科違いの同一剤のまとめ用         
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.01    NACL-藤原    09/07/14  小数点以下５桁対応
      *  04.05.02    NACL-門脇    12/04/10  改正対応項目追加
      *  04.07.01    NACL-門脇    13/01/22  データ領域拡張
      *  04.08.01    NACL-門脇    13/01/22  日数回数テーブル１０回対応
      *  04.08.02    NACL-門脇    13/01/25  包括診療分の点検用レセプト記載対応
      *  05.00.01    NACL-門脇    20/03/30  令和２年４月改正対応
      ************************************************************
      *01  RECE121-REC.
           03  RECE121-KEY.
               05  RECE121-KEY1.
      *医療機関番号
                   07  RECE121-HOSPNUM     PIC 9(2).
      *診療年月
                   07  RECE121-SRYYM       PIC 9(06).
      *入外区分
                   07  RECE121-NYUGAIKBN   PIC X(01).
      *患者ＩＤ
                   07  RECE121-PTID        PIC 9(10).
      *レセプト診療科
                   07  RECE121-RECEKA      PIC X(02).
      *公費ＩＤ
                   07  RECE121-HKNID       PIC 9(10).
      *適用開始年月日
                   07  RECE121-TEKSTYMD    PIC X(08). 
      *診療区分
               05  RECE121-SRYKBN          PIC X(02).
      *診療種別区分
               05  RECE121-SRYSYUKBN       PIC X(03).
      *剤点数
               05  RECE121-ZAITEN          PIC 9(08).
      *診療コード計
               05  RECE121-SRYCDTOTAL      PIC 9(14).
      *数量計
               05  RECE121-SURYOUTOTAL     PIC 9(07)V9(05).
      *診療コード＊数量の累計
               05  RECE121-RUIKEITOTAL     PIC 9(13)V9(05).
      *明細数
               05  RECE121-MEISAISU        PIC 9(04).
      *在宅患者訪問点滴注射管理指導料算定時の点滴注射
               05  RECE121-FLG-HOUTEN-COMMENT
                                           PIC X(01).
      *自保区分
               05  RECE121-JIHOKBN         PIC X(01).
      *コメント剤連番
               05  RECE121-COMRENNUM       PIC 9(05).
      *剤回数
           03  RECE121-ZAIKAISU            PIC 9(04).
      *日数回数テーブル
           03  RECE121-DAY-AREA.
      *        診療回数テーブル（1:合計、2-9:１回〜９回）
               05  RECE121-DAY-TBL         OCCURS  10.
                   07  RECE121-KAISUINF    OCCURS  31.
                       09  RECE121-KAISU   PIC 9(03).
      *データ
           03  RECE121-REC1                PIC X(2000).
      *コメントデータ
           03  RECE121-COM-AREA.
               05  RECE121-COM             PIC X(300)    OCCURS  10.
      *初診（同日複数診療科）算定科
           03  RECE121-SYODOU-SRYKA-AREA.
               05  RECE121-SYODOU-SRYKA    PIC X(02)     OCCURS  10.
      *再診（同一日２科目）算定科
           03  RECE121-SAIDOU-SRYKA-AREA.
               05  RECE121-SAIDOU-SRYKA    PIC X(02)     OCCURS  10.
