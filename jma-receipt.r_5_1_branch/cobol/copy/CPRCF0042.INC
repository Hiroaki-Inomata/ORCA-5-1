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
      *    レセプト明細書     COPY (CPRCF0042.INC)               *
      *                       診療科違いの同一剤のまとめ用       *  
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    04.02.05  診療コード＊数量の累計追加
      *  ２００４年４月改正対応
      *  01.01.01    NACL-門脇    04.04.10  在宅患者訪問点滴注射管理指
      *                                     導料算定時の点滴注射の追加
      *  01.01.02    NACL-門脇    04.08.17  レセプト科追加
      *  01.01.03    NACL-門脇    05.07.05  診療種別区分追加
      *  01.01.04    NACL-門脇    05.11.11  コメントデータ追加
      *  02.09.01    NACL-門脇    05.04.17  ２００６年４月改正対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.05.01    NACL-藤原    09/07/14  小数点以下５桁対応
      *  04.05.02    NACL-門脇    12/04/10  改正対応項目追加
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.02    NACL-藤原    11/01/05  機能追加（注加算、同時併施等）
      *  04.06.03    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *  04.08.01    NACL-門脇    13/01/22  日数回数テーブル１０回対応
      *  04.08.02    NACL-門脇    13/01/25  包括診療分の点検用レセプト記載対応
      *  04.08.03    NACL-門脇    16/02/05  医保レセプト「他保険にて診察料算定
      *                                     済」コメント不記載対応
      *  05.00.01    NACL-門脇    20/03/30  令和２年４月改正対応
      ************************************************************
      *01  RECE042-REC.
           03  RECE042-KEY.
               05  RECE042-KEY1.
      *医療機関ＩＤ
                   07  RECE042-HOSPNUM     PIC 9(2).
      *診療年月
                   07  RECE042-SRYYM       PIC 9(06).
      *入外区分
                   07  RECE042-NYUGAIKBN   PIC X(01).
      *患者ＩＤ
                   07  RECE042-PTID        PIC 9(10).
      *レセプト科
                   07  RECE042-RECEKA      PIC X(02).
      *審査支払機関
                   07  RECE042-TEISYUTUSAKI
                                           PIC 9(01).
      *レセプト種別
                   07  RECE042-RECESYUBETU PIC 9(04).
      *保険者番号        
                   07  RECE042-HKNJANUM    PIC X(08).
      *補助区分
                   07  RECE042-HOJOKBN     PIC X(01).
      *公費ＩＤ
                   07  RECE042-KOHID       PIC 9(10).
      *適用開始年月日
                   07  RECE042-TEKSTYMD    PIC X(08). 
      *分割区分
                   07  RECE042-RECEIPT-KBN PIC X(10).
      *公費分離区分
               05  RECE042-KOHBUNRI        PIC  9(01).
      *診療区分
               05  RECE042-SRYKBN          PIC  X(02).
      *診療種別区分
               05  RECE042-SRYSYUKBN       PIC  X(03).
      *剤点数
               05  RECE042-ZAITEN          PIC  9(08).
      *診療コード計
               05  RECE042-SRYCDTOTAL      PIC  9(14).
      *数量計
               05  RECE042-SURYOUTOTAL     PIC  9(07)V9(05).
      *診療コード＊数量の累計
               05  RECE042-RUIKEITOTAL     PIC  9(13)V9(05).
      *明細数
               05  RECE042-MEISAISU        PIC  9(04).
      *在宅患者訪問点滴注射管理指導料算定時の点滴注射
               05  RECE042-FLG-HOUTEN-COMMENT
                                           PIC  X(01).
      *他保険にて診察料算定済不記載フラグ
               05  RECE042-TAHOKENNGFLG    PIC  X(01).
      *自保区分
               05  RECE042-JIHOKBN         PIC  X(01).
      *コメント剤連番
               05  RECE042-COMRENNUM       PIC  9(05).
      *剤回数
           03  RECE042-ZAIKAISU            PIC  9(04).
      *日数回数テーブル
           03  RECE042-DAY-AREA.
      *        診療回数テーブル（1:合計、2-9:１回〜９回）
               05  RECE042-DAY-TBL         OCCURS  10.
                   07  RECE042-KAISUINF    OCCURS  31.
                       09  RECE042-KAISU   PIC  9(03).
      *データ
           03  RECE042-REC1                PIC  X(2000).
      *コメントデータ
           03  RECE042-COM-AREA.
               05  RECE042-COM             PIC  X(300)    OCCURS  10.
      *初診（同日複数診療科）算定科
           03  RECE042-SYODOU-SRYKA-AREA.
               05  RECE042-SYODOU-SRYKA    PIC  X(02)     OCCURS  10.
      *再診（同一日２科目）算定科
           03  RECE042-SAIDOU-SRYKA-AREA.
               05  RECE042-SAIDOU-SRYKA    PIC  X(02)     OCCURS  10.
