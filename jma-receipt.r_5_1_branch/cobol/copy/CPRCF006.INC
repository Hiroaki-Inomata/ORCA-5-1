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
      *    レセプト明細書     COPY (CPRCF06.INC)        *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01.06.29  保険者番号・
      *                                     適用開始年月日の追加
      *  01.00.02    NACL-藤原    03/03/12  補助区分の追加
      *  01.00.03    NACL-門脇    03.03.03  剤番号桁修正
      *  01.00.04    NACL-門脇    05.02.28  （サ）フラグの追加
      *  01.00.05    NACL-門脇    05.05.25  診療種別区分、診療日の追加
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  04.02.01    NACL-門脇    08/04/21  処方せん判別フラグ追加
      *  04.04.01    NACL-門脇    10/03/29  診療日２の追加
      *  04.05.01    NACL-門脇    10/01/12  剤区分追加
      *  04.05.02    NACL-門脇    11/06/20  保険組合せ追加
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.02    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *
      *  04.08.01    NACL-門脇    13/01/25  包括診療分の点検用レセプト記載対応
      ************************************************************
      *01  RECE06-REC.
           03  RECE06-KEY.
               05  RECE06-KEY1.
      *医療機関ＩＤ
                   07  RECE06-HOSPNUM      PIC 9(2).
      *診療年月
                   07  RECE06-SRYYM        PIC 9(06).
      *入外区分
                   07  RECE06-NYUGAIKBN    PIC X(01).
      *患者ＩＤ
                   07  RECE06-PTID         PIC 9(10).
      *レセプト科
                   07  RECE06-RECEKA       PIC X(02).
      *審査支払機関
                   07  RECE06-TEISYUTUSAKI
                                           PIC 9(01).
      *レセプト種別
                   07  RECE06-RECESYUBETU  PIC 9(04).
      *保険者番号        
                   07  RECE06-HKNJANUM     PIC X(08).
      *補助区分
                   07  RECE06-HOJOKBN      PIC X(01).
      *公費ＩＤ
                   07  RECE06-KOHID        PIC 9(10).
      *適用開始年月日
                   07  RECE06-TEKSTYMD     PIC X(08). 
      *分割区分
                   07  RECE06-RECEIPT-KBN  PIC X(10).
      *公費分離区分
               05  RECE06-KOHBUNRI        PIC  9(01).
      *剤番号
               05  RECE06-ZAINUM          PIC  9(08).
      *連番号
               05  RECE06-LINENUM         PIC  9(05).
      *剤明細数
           03  RECE06-ZAIMEISAISU         PIC  9(03).
      *患者番号
           03  RECE06-PTNUM               PIC  X(20).
      *診療科
           03  RECE06-SRYKA               PIC  X(02).
      *診療区分
           03  RECE06-SRYKBN              PIC  X(02).
      *適要欄情報
           03  RECE06-TEKIYO-INF.
      *剤区切（＊）
               05  RECE06-ASTER           PIC  X(02).
      *摘要
               05  RECE06-TEKIYO          PIC  X(100).
      *剤点数
           03  RECE06-ZAITEN              PIC  9(08).
      *検査実施区分
           03  RECE06-KNSJISKBN           PIC  9(01).
      *表用番号
           03  RECE06-CDKBN-SYO           PIC  9(01).
           03  RECE06-CDKBN-BU            PIC  9(02).
           03  RECE06-CDKBN-KBN           PIC  X(01).
           03  RECE06-CDKBN-KBNNUM        PIC  9(03).
           03  RECE06-CDKBN-KBNNUM-EDA    PIC  9(02).
           03  RECE06-CDKBN-KOUBAN        PIC  9(02).
           03  RECE06-KOUHYOJUNNUM        PIC  9(09).
      *（サ）フラグ
           03  RECE06-SAFLG               PIC  X(01).
      *診療種別区分
           03  RECE06-SRYSYUKBN           PIC  X(03).
      *診療日
           03  RECE06-DAY                 PIC  9(02).
      *処方せん判別フラグ
           03  RECE06-SYOHOUFLG           PIC  9(01).
      *剤区分
           03  RECE06-ZAIKBN              PIC  9(03).
      *診療日２
           03  RECE06-DAY2                PIC  9(02).
      *保険組合せ番号
           03  RECE06-HKNCOMBI            PIC  9(04).
      *自保区分
           03  RECE06-JIHOKBN             PIC  X(01).
