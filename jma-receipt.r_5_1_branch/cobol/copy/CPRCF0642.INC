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
      *    レセプト明細書     COPY (CPRCF0642.INC)               *
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
      *  03.05.01    NACL-小豆沢  07/05/30  グループ診療対応
      *
      *  04.05.01    NACL-藤原    09/07/14  小数点以下５桁対応
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.02    NACL-藤原    11/01/05  機能追加（注加算、同時併施等）
      *
      *  04.07.01    NACL-藤原    12/12/17  複数枚レセプト分割対応
      *  04.07.02    NACL-門脇    15/08/10  日別保険組合せ番号追加
      *
      *  04.08.01    NACL-門脇    13/01/25  包括診療分の点検用レセプト記載対応
      *  04.08.02    NACL-門脇    16/08/23  持参薬対応
      *  05.00.01    NACL-門脇    20/03/30  令和２年４月改正対応
      ************************************************************
      *01  RECE642-REC.
           03  RECE642-KEY.
               05  RECE642-KEY1.
      *医療機関ＩＤ
                   07  RECE642-HOSPNUM     PIC 9(2).
      *診療年月
                   07  RECE642-SRYYM       PIC 9(06).
      *入外区分
                   07  RECE642-NYUGAIKBN   PIC X(01).
      *患者ＩＤ
                   07  RECE642-PTID        PIC 9(10).
      *レセプト科
                   07  RECE642-RECEKA      PIC X(02).
      *審査支払機関
                   07  RECE642-TEISYUTUSAKI
                                           PIC 9(01).
      *レセプト種別
                   07  RECE642-RECESYUBETU PIC 9(04).
      *保険者番号        
                   07  RECE642-HKNJANUM    PIC X(08).
      *補助区分
                   07  RECE642-HOJOKBN     PIC X(01).
      *公費ＩＤ
                   07  RECE642-KOHID       PIC 9(10).
      *適用開始年月日
                   07  RECE642-TEKSTYMD    PIC X(08). 
      *分割区分
                   07  RECE642-RECEIPT-KBN PIC X(10).
      *公費分離区分
               05  RECE642-KOHBUNRI        PIC  9(01).
      *診療区分
               05  RECE642-SRYKBN          PIC  X(02).
      *診療種別区分
               05  RECE642-SRYSYUKBN       PIC  X(03).
      *剤点数
               05  RECE642-ZAITEN          PIC  9(08).
      *診療コード計
               05  RECE642-SRYCDTOTAL      PIC  9(14).
      *数量計
               05  RECE642-SURYOUTOTAL     PIC  9(07)V9(05).
      *診療コード＊数量の累計
               05  RECE642-RUIKEITOTAL     PIC  9(13)V9(05).
      *明細数
               05  RECE642-MEISAISU        PIC  9(04).
      *自保区分
               05  RECE642-JIHOKBN         PIC  X(01).
      *持参薬区分
               05  RECE642-JISANYAKKBN     PIC  X(01).
      *コメント剤連番
               05  RECE642-COMRENNUM       PIC  9(05).
      *剤回数
           03  RECE642-ZAIKAISU            PIC  9(04).
      *日数回数テーブル
           03  RECE642-DAY-AREA.
      *        診療回数テーブル（1:合計,2:１回,3:２回, 4:３回以上）
               05  RECE642-DAY-TBL         OCCURS  4.
                   07  RECE642-KAISUINF    OCCURS  31.
                       09  RECE642-KAISU   PIC  9(03).
      *日別保険組合せ番号
           03  RECE642-HKNCOMBI-DAY-AREA.
               05  RECE642-HKNCOMBI-DAY    PIC  9(04)     OCCURS  31.
      *データ
           03  RECE642-REC1                PIC  X(2000).
      *コメントデータ
           03  RECE642-COM-AREA.
               05  RECE642-COM             PIC  X(300)    OCCURS  10.
      *初診（同日複数診療科）算定科
           03  RECE642-SYODOU-SRYKA-AREA.
               05  RECE642-SYODOU-SRYKA    PIC  X(02)     OCCURS  10.
