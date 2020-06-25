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
      *    レセプト明細書     COPY (CPRCF064.INC)                *
      *    (レセプト電算処理システムのレコード並び替え用コピー句 *      
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  02.09.01    NACL-藤原    05.04.17  ２００６年４月改正対応
      *  03.03.01    NACL-藤原    06/09/29  検査の判断料の記載方法の変更
      *                                     検査等実施判断の追加
      *  03.05.01    NACL-小豆沢  07/05/30  グループ診療対応
      * 
      *  04.02.01    NACL-門脇    08/04/10  レセ電用入院料加算フラグ追加
      *
      *  04.03.01    NACL-門脇    08/07/14  仮レセ作成（院外処方含む）対応
      *                                     投薬剤点０フラグの追加
      *
      *  04.05.01    NACL-藤原    09/06/10  摘要欄下部へのコメント記載対応
      *                                     診療種別区分の追加
      *  04.05.02    NACL-藤原    09/07/14  小数点以下５桁対応
      *  04.05.03    NACL-藤原    09/11/27  レセ電コメント拡張対応
      *                                     関係コメント存在フラグ追加
      *  04.05.03    NACL-藤原    10/01/13  治験対応
      *                                     剤区分の追加
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.02    NACL-藤原    11/01/05  機能追加（注加算、同時併施等）
      *  04.06.03    NACL-藤原    11/01/28  キーに剤連番追加
      *
      *  04.07.01    NACL-藤原    12/12/17  複数枚レセプト分割対応
      *  04.07.02    NACL-門脇    15/08/10  日別保険組合せ番号追加
      *  04.08.01    NACL-門脇    16/08/02  １５円以下薬剤の提出用レセプト記載
      *                                     （記録）対応
      *  04.08.02    NACL-門脇    16/08/23  持参薬対応
      ************************************************************
      *01  RECE641-REC.
           03  RECE641-KEY.
               05  RECE641-KEY1.
      *医療機関ＩＤ
                   07  RECE641-HOSPNUM     PIC 9(2).
      *診療年月
                   07  RECE641-SRYYM       PIC 9(06).
      *入外区分
                   07  RECE641-NYUGAIKBN   PIC X(01).
      *患者ＩＤ
                   07  RECE641-PTID        PIC 9(10).
      *レセプト科
                   07  RECE641-RECEKA      PIC X(02).
      *審査支払機関
                   07  RECE641-TEISYUTUSAKI
                                           PIC 9(01).
      *レセプト種別
                   07  RECE641-RECESYUBETU PIC 9(04).
      *保険者番号        
                   07  RECE641-HKNJANUM    PIC X(08).
      *補助区分
                   07  RECE641-HOJOKBN     PIC X(01).
      *公費ＩＤ
                   07  RECE641-KOHID       PIC 9(10).
      *適用開始年月日
                   07  RECE641-TEKSTYMD    PIC X(08). 
      *分割区分
                   07  RECE641-RECEIPT-KBN PIC X(10).
      *診療区分
               05  RECE641-SRYKBN          PIC  X(02).
      *コード表用番号−区分番号
               05  RECE641-CDKBN-KBNNUM    PIC  9(03).
      *剤番号
               05  RECE641-ZAINUM          PIC  9(08).
      *剤連番
               05  RECE641-ZAIREN          PIC  9(02).
      *公費分離区分
           03  RECE641-KOHBUNRI            PIC  9(01).
      *患者番号
           03  RECE641-PTNUM               PIC  X(20).
      *診療種別区分
           03  RECE641-SRYSYUKBN           PIC  X(03).
      *診療科
           03  RECE641-SRYKA               PIC  X(02).
      *保険組合せ番号
           03  RECE641-HKNCOMBI            PIC  9(04).
      *自保区分
           03  RECE641-JIHOKBN             PIC  X(01).
      *剤点数
           03  RECE641-ZAITEN              PIC  9(08).
      *診療コード計
           03  RECE641-SRYCDTOTAL          PIC  9(14).
      *数量計
           03  RECE641-SURYOUTOTAL         PIC  9(07)V9(05).
      *診療コード＊数量の累計
           03  RECE641-RUIKEITOTAL         PIC  9(13)V9(05).
      *明細数
           03  RECE641-MEISAISU            PIC  9(04).
      *剤回数
           03  RECE641-ZAIKAISU            PIC  9(04).
      *手技点数１
           03  RECE641-SYUTEN1             PIC  9(07).
      *手技点数２
           03  RECE641-SYUTEN2             PIC  9(07).
      *薬剤点数
           03  RECE641-YKZTEN              PIC  9(07).
      *器材点数
           03  RECE641-KIZAITEN            PIC  9(07).
      *ドクターコード
           03  RECE641-DRCD                PIC  X(05).
      *剤請求フラグ
           03  RECE641-ZAIREQFLG           PIC  X(01).
      *逓減率
           03  RECE641-TEIGENRATE          PIC  X(01).
      *投薬剤点０（１又は２（コメントのみ））
           03  RECE641-TOYAKUFLG           PIC  X(01).
      *関係コメント存在フラグ
           03  RECE641-SETCOMFLG           PIC  X(01).
      *剤区分
           03  RECE641-ZAIKBN              PIC  9(03).
      *検査等実施判断
           03  RECE641-KNSJISKBN           PIC  9(01).
      *検査等実施判断グループ
           03  RECE641-KNSJISGRPKBN        PIC  9(02).
      *レセ電用入院料加算フラグ
           03  RECE641-NYUKSNFLG           PIC  9(01).
      *日数回数テーブル
           03  RECE641-DAY-AREA.
      *        診療回数テーブル（1:合計,2:１回,3:２回, 4:３回以上）
             04  RECE641-DAY-TBL           OCCURS  4.
                 05  RECE641-KAISUINF      OCCURS  31.
                     07  RECE641-KAISU     PIC  9(03).
      *
      *手技毎の内容（手術・画像診断・処置）
           03  RECE641-ACCTP-SYUGI-TBL-G.
               05  RECE641-ACCTP-SYUGI-TBL  OCCURS  5.
      *手技開始コード
                   07  RECE641-ACCTP-SRYCD  PIC X(09).
      *手技点数
                   07  RECE641-ACCTP-SYUTEN PIC 9(07).
      *薬剤点数
                   07  RECE641-ACCTP-YKZTEN PIC 9(07).
      *その他器材点数（フィルム・酸素・窒素以外）
                   07  RECE641-ACCTP-KIZAITEN
                                           PIC 9(07).
      *フィルム点数
                   07  RECE641-ACCTP-FIRTEN PIC 9(07).
      *酸素点数
           03  RECE641-ACCTP-SANSOTEN       PIC 9(07).
      *窒素点数
           03  RECE641-ACCTP-CHISOTEN       PIC 9(07).
      *
      *日別保険組合せ番号
           03  RECE641-HKNCOMBI-DAY-AREA.
               05  RECE641-HKNCOMBI-DAY     PIC 9(04)     OCCURS  31.
      *
      *再診加算（入院）の剤
           03  RECE641-NYUSAIFLG            PIC X(01).
      *１５円以下薬剤レセプト記載フラグ
           03  RECE641-YKZKISAIFLG          PIC X(01).
      *持参薬区分
           03  RECE641-JISANYAKKBN          PIC X(01).
