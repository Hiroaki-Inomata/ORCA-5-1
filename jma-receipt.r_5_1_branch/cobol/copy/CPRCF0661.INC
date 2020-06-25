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
      *    レセプト明細書（入院） 適用欄並び替え (CPRCF066.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    05.05.25  診療種別区分、診療日の追加
      *  03.05.01    NACL-小豆沢  07/05/30  グループ診療対応
      *  04.04.01    NACL-門脇    10/03/29  診療日２の追加
      *  04.05.01    NACL-門脇    10/01/12  剤区分追加
      *  04.05.02    NACL-門脇    11/06/20  保険組合せ追加
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *
      *  04.07.01    NACL-藤原    12/12/17  複数枚レセプト分割対応
      *
      *  04.08.01    NACL-門脇    13/01/25  包括診療分の点検用レセプト記載対応
      ************************************************************
      *01  RECE661-REC.
           03  RECE661-KEY.
               05  RECE661-KEY1.
      *医療機関ＩＤ
                   07  RECE661-HOSPNUM     PIC 9(2).
      *診療年月
                   07  RECE661-SRYYM       PIC 9(06).
      *入外区分
                   07  RECE661-NYUGAIKBN   PIC X(01).
      *患者ＩＤ
                   07  RECE661-PTID        PIC 9(10).
      *レセプト科
                   07  RECE661-RECEKA      PIC X(02).
      *審査支払機関
                   07  RECE661-TEISYUTUSAKI
                                           PIC 9(01).
      *レセプト種別
                   07  RECE661-RECESYUBETU PIC 9(04).
      *保険者番号        
                   07  RECE661-HKNJANUM    PIC X(08).
      *補助区分
                   07  RECE661-HOJOKBN     PIC X(01).
      *公費ＩＤ
                   07  RECE661-KOHID       PIC 9(10).
      *適用開始年月日
                   07  RECE661-TEKSTYMD    PIC X(08). 
      *分割区分
                   07  RECE661-RECEIPT-KBN PIC X(10).
      *公費分離区分
               05  RECE661-KOHBUNRI        PIC  9(01).
      *診療区分
               05  RECE661-SRYKBN          PIC  X(02).
      *自保区分
               05  RECE661-JIHOKBN         PIC  X(01).
      *剤区分
               05  RECE661-ZAIKBN          PIC  9(03).
      *剤識別区分
               05  RECE661-ZAISKBKBN       PIC  X(01).
      *剤識別内番号
               05  RECE661-ZAISKBBAN       PIC  9(08).
      *点滴出力区分
               05  RECE661-TENTEKIKBN      PIC  9(02).
      *点滴出力区分別内番号
               05  RECE661-TENTEKIBAN      PIC  9(08).
      *検査実施区分
               05  RECE661-KNSJISKBN       PIC  9(01).
      *診療種別区分
               05  RECE661-SRYSYUKBN       PIC  X(03).
      *診療日
               05  RECE661-DAY             PIC  9(02).
      *表用番号
               05  RECE661-CDKBN-SYO           PIC  9(01).
               05  RECE661-CDKBN-BU            PIC  9(02).
               05  RECE661-CDKBN-KBN           PIC  X(01).
               05  RECE661-CDKBN-KBNNUM        PIC  9(03).
               05  RECE661-CDKBN-KBNNUM-EDA    PIC  9(02).
               05  RECE661-CDKBN-KOUBAN        PIC  9(02).
               05  RECE661-KOUHYOJUNNUM        PIC  9(09).
      *診療日２
               05  RECE661-DAY2            PIC  9(02).
      *剤点数
               05  RECE661-ZAITEN          PIC  9(08).
      *剤番号
               05  RECE661-ZAINUM          PIC  9(08).
      *連番号
               05  RECE661-LINENUM         PIC  9(05).
      *剤明細数
           03  RECE661-ZAIMEISAISU         PIC  9(03).
      *患者番号
           03  RECE661-PTNUM               PIC  X(20).
      *診療科
           03  RECE661-SRYKA               PIC  X(02).
      *適要欄情報
           03  RECE661-TEKIYO-INF.
      *剤区切（＊）
               05  RECE661-ASTER           PIC  X(02).
      *摘要
               05  RECE661-TEKIYO          PIC  X(100).
      *保険組合せ番号
           03  RECE661-HKNCOMBI            PIC  9(04).
