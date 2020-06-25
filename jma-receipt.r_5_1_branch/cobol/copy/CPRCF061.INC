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
      *    レセプト明細書・医療費請求書（入院） COPY (CPRCF061.INC)      *
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    03.03.03  診療科数１０から２０へ変更
      *  01.00.02    NACL-藤原    03.03.11  レセプト種別不明情報の追加
      *  01.00.03    NACL-藤原    03.07.28  診療回数テーブルの追加
      *  01.00.04    NACL-藤原    04.06.15  印刷順の追加（個別作成時のみ）
      *  01.00.05    NACL-藤原    04.10.31  患者情報・県番号の追加
      *  01.00.06    NACL-藤原    04.11.01  公費レセプト請求区分の追加
      *
      *  ver.2.6.0 
      *  02.06.01    NACL-藤原    05.07.13  保険組合せ・主保険情報の追加 
      *  02.06.02    NACL-藤原    05.07.28  レセプト請求しない公費の追加
      *
      *  03.04.01    NACL-藤原    06/12/05  未請求レセ対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      * 
      *  04.02.01    NACL-藤原    08/03/04  平成２０年４月改正対応
      *                                     就学前区分の追加
      *
      *  04.05.01    NACL-藤原    11/03/02  徳島県対応
      *                                     乳児４５、４８が月途中で変更
      *                                     になった場合の記載順
      *                                     公費適用開始日を追加
      *  04.05.02    NACL-藤原    11/03/25  災害対応
      *                                     連絡先情報追加
      *  04.05.03    NACL-藤原    11/11/22  同日再入院対応
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.02    NACL-藤原    11/12/12  点検レセ０点公費印字対応
      *  04.06.03    NACL-藤原    12/10/04  同一レセプトの公費が４つを超
      *                                     えた場合の種別不明対応
      *  04.06.04    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *  04.06.05    NACL-藤原    12/12/21  入院料を自費で算定した場合の
      *                                     入退院歴記載対応
      *
      *  04.07.01    NACL-藤原    14/12/08  月途中に公費の負担者番号等の
      *                                     変更があった場合の記載対応
      *                                     法別番号、適用開始日の順とする
      *                                     （地方公費） 
      *                                     法別番号追加
      *  04.07.02    NACL-藤原    14/11/13  生保の負担者番号、受給者番号
      *                                     が同じときはまとめる
      *
      *  05.00.01    NACL-藤原    19/04/04  印刷順の桁数変更
      ********************************************************************
      *01  RECE61-REC.
           03  RECE61-KEY.
      *医療機関ＩＤ
               05  RECE61-HOSPNUM         PIC 9(2).
      *診療年月
               05  RECE61-SRYYM           PIC  9(06).
      *入外区分
               05  RECE61-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  RECE61-PTID            PIC  9(10).
      *分割区分
               05  RECE61-RECEIPT-KBN     PIC  X(10).
      *レセプト診療科
               05  RECE61-RECEKA          PIC  X(02).
      *主保険保険番号
               05  RECE61-HKNNUM          PIC  X(03).
      *公費ＩＤ（老人保険）
               05  RECE61-KOHID           PIC  9(10).
      *保険者番号        
               05  RECE61-HKNJANUM        PIC  X(08).
      *本人家族区分
               05  RECE61-HONKZKKBN       PIC  X(01).
      *補助区分
               05  RECE61-HOJOKBN         PIC  X(01).
      *公費ＩＤ（公費単独時使用）
               05  RECE61-KOH-KOHID       PIC  9(10).
      *適用開始年月日
               05  RECE61-TEKSTYMD        PIC  X(08). 
      *公費領域
           03 RECE61-KOHI-AREA.
               05  RECE61-KOHI-TBL        OCCURS  10.
      *    公費ＩＤ
                   07  RECE61-KOHID-O     PIC  9(10).
      *    公費保険番号
                   07  RECE61-KOHHKNNUM-O PIC  X(03).
      *    公費支払区分
                   07  RECE61-KOHPAYKBN-O PIC  X(02).
      *    公費レセプト請求区分（空白：レセ記載のみ、４：記載なし）
                   07  RECE61-RECESKYKBN-O
                                          PIC  X(01).
      *    公費適用開始年月日
                   07  RECE61-KOHTEKSTYMD-O
                                          PIC  X(08).
      *    公費法別番号
                   07  RECE61-KOHHBTNUM-O PIC  X(02).
      *    公費負担者番号
                   07  RECE61-KOHFTNJANUM-O
                                          PIC  X(08).
      *    公費受給者番号
                   07  RECE61-KOHJKYSNUM-O
                                          PIC  X(20).
      *
      *主保険情報
           03  RECE61-HKN.
      *主保険−保険ＩＤ
               05  RECE61-HKNID           PIC  9(10).
      *主保険−法別番号
               05  RECE61-HKNHBTNUM       PIC  X(02).
      *主保険−保険者番号
               05  RECE61-HKN-HKNJANUM    PIC  X(08).
      *主保険−本人家族区分
               05  RECE61-HKN-HONKZKKBN   PIC  X(01).
      *主保険−補助区分
               05  RECE61-HKN-HOJOKBN     PIC  X(01).
      *主保険−継続区分
               05  RECE61-CONTKBN         PIC  X(01).
      *主保険−記号
               05  RECE61-KIGO            PIC  X(80).
      *主保険−番号
               05  RECE61-NUM             PIC  X(80).
      *主保険−証明書番号
               05  RECE61-CERTNUM         PIC  X(03).
      *
      *保険組合せ番号領域
           03 RECE61-HKNCOMBI-AREA.
               05  RECE61-HKNCOMBI-TBL    OCCURS  10.
      *保険組合せ番号
                   07  RECE61-HKNCOMBI-O  PIC  9(04).
      *主保険番号
                   07  RECE61-HKNNUMHC    PIC  X(03).
      *組合せ公費保険番号情報
                   07  RECE61-KOHNUMHCINF OCCURS  4.
      *組合せ公費保険番号
                       09  RECE61-KOHNUMHC
                                          PIC  X(03).
      *組合せ公費保険ＩＤ
                       09  RECE61-KOHIDHC PIC  9(10).
      *
      *診療科情報
           03 RECE61-SRYKAINF.
               05  RECE61-SRYKA           PIC  X(02)  OCCURS  20.
      *適用年月日領域
           03 RECE61-TEKYMD-AREA.
               05  RECE61-TEKYMD-TBL      OCCURS  10.
      *適用開始年月日
                   07  RECE61-TEKSTYMDX   PIC  X(08). 
      *適用終了年月日
                   07  RECE61-TEKEDYMDX   PIC  X(08). 
      *
      *レセプト請求しない公費領域
           03 RECE61-KOHSKY-KOHI-AREA.
               05  RECE61-KOHSKY-KOHI-TBL OCCURS  4.
      *    公費ＩＤ
                   07  RECE61-KOHSKY-KOHID-O
                                          PIC  9(10).
      *    公費保険番号
                   07  RECE61-KOHSKY-KOHHKNNUM-O
                                          PIC  X(03).
      *診療回数テーブル
           03  RECE61-DAY-AREA.
               05  RECE61-DAY             PIC  9(03)
                                               OCCURS   31.
      *診療回数テーブル（入院基本料を算定していないとき）
           03  RECE61-DAY-1-AREA.
               05  RECE61-DAY-1           PIC  9(03)
                                               OCCURS   31.
      *患者情報
           03  RECE61-PTINF.
      *患者番号  
               05  RECE61-PTNUM           PIC  X(20).
      *カナ氏名
               05  RECE61-KANANAME        PIC  X(50).
      *漢字氏名
               05  RECE61-NAME            PIC  X(100).
      *性別
               05  RECE61-SEX             PIC  X(01).
      *生年月日
               05  RECE61-BIRTHDAY        PIC  9(08).
      *死亡区分
               05  RECE61-DEATHKBN        PIC  X(01).
      *年齢
               05  RECE61-AGE             PIC  9(03).
      *就学前区分
               05  RECE61-SYUGAKUKBN      PIC 9(01).
      *テスト患者番号区分
               05  RECE61-TSTPTNUMKBN     PIC  X(01).
      *自宅−郵便番号
               05  RECE61-HOME-POST       PIC X(07).
      *自宅−住所
               05  RECE61-HOME-ADRS       PIC X(200).
      *自宅−番地方書
               05  RECE61-HOME-BANTI      PIC X(200).
      *連絡先−名称
               05  RECE61-RENRAKU-NAME    PIC X(100).
      *連絡先−郵便番号
               05  RECE61-RENRAKU-POST    PIC X(07).
      *連絡先−住所
               05  RECE61-RENRAKU-ADRS    PIC X(200).
      *連絡先−番地方書
               05  RECE61-RENRAKU-BANTI   PIC X(200).
      *連絡先−昼電話番号
               05  RECE61-RENRAKU-TEL1    PIC X(15).
      *連絡先−夜電話番号
               05  RECE61-RENRAKU-TEL2    PIC X(15).
      *県番号
           03  RECE61-PREFNUM             PIC  9(02).
      *入院料情報取得フラグ
           03  RECE61-DOUJITSU-FLG        PIC 9(01).
      *入院料情報
           03  RECE61-DOUJITSU-AREA.
      *同日再入院（入院分）の保険組合せ
               05  RECE61-DOUJITSU-HKNCOMBI
                                          PIC 9(04)   OCCURS  31.
      *同日再入院（入院分）の食事開始時間区分
               05  RECE61-DOUJITSU-SKJKBN PIC X(01)   OCCURS  31.
      *入院区分
               05  RECE61-NYUINDAY-TBL.
                   07  RECE61-NYUINDAY-KBN
                                          PIC  X(01)  OCCURS  31.
      *印刷順
           03  RECE61-PRTSEQ              PIC  9(04).
      *未請求フラグ
           03  RECE61-MISEIKYU-FLG        PIC X(02).
      *未訂正区分
           03  RECE61-TEISEIKBN           PIC  X(01).
      *点検レセ０点公費フラグ
           03  RECE61-TENKEN-FLG          PIC X(01).
      *レセプト種別不明エラー情報
           03  RECE61-ERR-AREA.
               05  RECE61-ERR-ERRKBN      PIC 9(01).
               05  RECE61-ERR-DAY         PIC 9(02).
               05  RECE61-ERR-HKNCOMBINUM PIC 9(04).
               05  RECE61-ERR-TEKSTYMD    PIC 9(08).
               05  RECE61-ERR-TEKEDYMD    PIC 9(08).
      
