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
      *    労災レセプト明細書（入院）     COPY (CPRCF0842.INC)   *
      *                       診療科違いの同一剤のまとめ用       *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    04.02.05  診療コード＊数量の累計追加
      *  01.00.02    NACL-門脇    04.08.17  レセプト科追加
      *  01.00.03    NACL-門脇    05.07.05  診療種別区分追加
      *  01.00.04    NACL-門脇    05.11.11  コメントデータ追加
      *  02.09.01    NACL-門脇    05.04.17  ２００６年４月改正対応
      *  03.05.01    NACL-門脇    07.06.01  グループ診療対応
      *
      *  04.05.01    NACL-藤原    09/07/14  小数点以下５桁対応
      *
      *  04.07.01    NACL-門脇    13/01/21  労災レセ電対応
      *  04.08.01    NACL-門脇    13/01/25  包括診療分の点検用レセプト記載対応
      *  04.08.02    NACL-門脇    16/08/23  持参薬対応
      *  05.00.01    NACL-門脇    20/03/30  令和２年４月改正対応
      ************************************************************
      *01  RECE842-REC.
           03  RECE842-KEY.
      *医療機関ＩＤ
               05  RECE842-HOSPNUM         PIC  9(02).
      *診療年月
               05  RECE842-SRYYM           PIC  9(06).
      *入外区分
               05  RECE842-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  RECE842-PTID            PIC  9(10).
      *レセプト科
               05  RECE842-RECEKA          PIC  X(02).
      *保険ＩＤ        
               05  RECE842-HKNID           PIC  9(10).
      *適用開始年月日
               05  RECE842-TEKSTYMD        PIC  X(08).
      *診療区分
               05  RECE842-SRYKBN          PIC  X(02).
      *診療種別区分
               05  RECE842-SRYSYUKBN       PIC  X(03).
      *剤点数
               05  RECE842-ZAITEN          PIC  9(08).
      *診療コード計
               05  RECE842-SRYCDTOTAL      PIC  9(14).
      *数量計
               05  RECE842-SURYOUTOTAL     PIC  9(07)V9(05).
      *診療コード＊数量の累計
               05  RECE842-RUIKEITOTAL     PIC  9(13)V9(05).
      *明細数
               05  RECE842-MEISAISU        PIC  9(04).
      *自保区分
               05  RECE842-JIHOKBN         PIC  X(01).
      *持参薬区分
               05  RECE842-JISANYAKKBN     PIC  X(01).
      *コメント剤連番
               05  RECE842-COMRENNUM       PIC  9(05).
      *剤回数
           03  RECE842-ZAIKAISU            PIC  9(04).
      *日数回数テーブル
           03  RECE842-DAY-AREA.
      *        診療回数テーブル（1:合計,2:１回,3:２回, 4:３回以上）
               05  RECE842-DAY-TBL         OCCURS  4.
                   07  RECE842-KAISUINF    OCCURS  31.
                       09  RECE842-KAISU   PIC  9(03).
      *データ
           03  RECE842-REC1                PIC  X(1000).
      *コメントデータ
           03  RECE842-COM-AREA.
               05  RECE842-COM             PIC  X(300)    OCCURS  10.
      *初診（同日複数診療科）算定科
           03  RECE842-SYODOU-SRYKA-AREA.
               05  RECE842-SYODOU-SRYKA    PIC  X(02)     OCCURS  10.
