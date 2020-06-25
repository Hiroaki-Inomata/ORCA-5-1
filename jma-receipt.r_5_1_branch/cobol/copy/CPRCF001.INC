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
      *    レセプト明細書・医療費請求書   COPY (CPRCF001.INC)    *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01.06.27  継続区分の追加
      *  01.00.02    MCC-藤原     01.06.29  適用終了年月日の追加
      *  01.01.03    MCC-藤原     02.02.01  補足区分の追加
      *  01.01.04    MCC-藤原     02.02.13  ｋｅｙを保険ＩＤから
      *                                     保険番号に変更
      *                                     本人家族区分の追加
      *                                     保険組合せを３から１０に変更
      *  01.01.05    MCC-藤原     02.03.13  適用年月日を複数個に変更
      *  01.01.06    MCC-藤原     02.05.24  保険変更時の未訂正フラグの追加
      *  01.01.07    NACL-藤原    03.02.12  補助区分の追加
      *  01.01.08    NACL-藤原    03.03.03  診療科数１０から２０へ変更
      *  01.01.09    NACL-藤原    03.03.11  レセプト種別不明情報の追加
      *  01.01.10    NACL-藤原    03.10.27  患者情報の追加
      *                                     県番号の追加
      *  01.01.11    NACL-藤原    04.06.15  印刷順の追加（個別作成時のみ）
      *  01.01.12    NACL-藤原    04.11.01  公費レセプト請求区分・主保険
      *                                     情報の追加
      *
      *  ver.2.6.0 
      *  02.06.01    NACL-藤原    05.07.13  保険組合せの追加
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
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.02    NACL-藤原    11/12/12  点検レセ０点公費印字対応
      *  04.06.03    NACL-藤原    12/10/04  同一レセプトの公費が４つを超
      *                                     えた場合の種別不明対応
      *  04.06.04    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
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
      ************************************************************
      *01  RECE01-REC.
           03  RECE01-KEY.
      *医療機関ＩＤ
               05  RECE01-HOSPNUM         PIC 9(2).
      *診療年月
               05  RECE01-SRYYM           PIC  9(06).
      *入外区分
               05  RECE01-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  RECE01-PTID            PIC  9(10).
      *分割区分
               05  RECE01-RECEIPT-KBN     PIC  X(10).
      *レセプト診療科
               05  RECE01-RECEKA          PIC  X(02).
      *主保険保険番号
               05  RECE01-HKNNUM          PIC  X(03).
      *公費ＩＤ
               05  RECE01-KOHID           PIC  9(10).
      *保険者番号        
               05  RECE01-HKNJANUM        PIC  X(08).
      *本人家族区分
               05  RECE01-HONKZKKBN       PIC  X(01).
      *補助区分
               05  RECE01-HOJOKBN         PIC  X(01).
      *公費ＩＤ（公費単独時使用）
               05  RECE01-KOH-KOHID       PIC  9(10).
      *適用開始年月日
               05  RECE01-TEKSTYMD        PIC  X(08). 
      *公費領域
           03 RECE01-KOHI-AREA.
               05  RECE01-KOHI-TBL        OCCURS  10.
      *    公費ＩＤ
                   07  RECE01-KOHID-O     PIC  9(10).
      *    公費保険番号
                   07  RECE01-KOHHKNNUM-O PIC  X(03).
      *    公費支払区分
                   07  RECE01-KOHPAYKBN-O PIC  X(02).
      *    公費レセプト請求区分（空白：レセ記載のみ、４：記載なし）
                   07  RECE01-RECESKYKBN-O
                                          PIC  X(01).
      *    公費適用開始年月日
                   07  RECE01-KOHTEKSTYMD-O
                                          PIC  X(08).
      *    法別番号
                   07  RECE01-KOHHBTNUM-O PIC  X(02).
      *    公費負担者番号
                   07  RECE01-KOHFTNJANUM-O
                                          PIC  X(08).
      *    公費受給者番号
                   07  RECE01-KOHJKYSNUM-O
                                          PIC  X(20).
      *
      *主保険情報
           03  RECE01-HKN.
      *主保険−保険ＩＤ
               05  RECE01-HKNID           PIC  9(10).
      *主保険−法別番号
               05  RECE01-HKNHBTNUM       PIC  X(02).
      *主保険−保険者番号
               05  RECE01-HKN-HKNJANUM    PIC  X(08).
      *主保険−本人家族区分
               05  RECE01-HKN-HONKZKKBN   PIC  X(01).
      *主保険−補助区分
               05  RECE01-HKN-HOJOKBN     PIC  X(01).
      *主保険−継続区分
               05  RECE01-CONTKBN         PIC  X(01).
      *主保険−記号
               05  RECE01-KIGO            PIC  X(80).
      *主保険−番号
               05  RECE01-NUM             PIC  X(80).
      *主保険−証明書番号
               05  RECE01-CERTNUM         PIC  X(03).
      *
      *保険組合せ番号領域
           03 RECE01-HKNCOMBI-AREA.
               05  RECE01-HKNCOMBI-TBL    OCCURS  10.
      *保険組合せ番号
                   07  RECE01-HKNCOMBI-O  PIC  9(04).
      *主保険番号
                   07  RECE01-HKNNUMHC    PIC  X(03).
      *組合せ公費保険番号情報
                   07  RECE01-KOHNUMHCINF OCCURS  4.
      *組合せ公費保険番号
                       09  RECE01-KOHNUMHC
                                          PIC  X(03).
      *組合せ公費保険ＩＤ
                       09  RECE01-KOHIDHC PIC  9(10).
      *
      *診療科情報
           03 RECE01-SRYKAINF.
               05  RECE01-SRYKA           PIC  X(02)  OCCURS  20.
      *適用年月日領域
           03 RECE01-TEKYMD-AREA.
               05  RECE01-TEKYMD-TBL      OCCURS  10.
      *適用開始年月日
                   07  RECE01-TEKSTYMDX   PIC  X(08). 
      *適用終了年月日
                   07  RECE01-TEKEDYMDX   PIC  X(08). 
      *
      *レセプト請求しない公費領域
           03 RECE01-KOHSKY-KOHI-AREA.
               05  RECE01-KOHSKY-KOHI-TBL OCCURS  4.
      *    公費ＩＤ
                   07  RECE01-KOHSKY-KOHID-O
                                          PIC  9(10).
      *    公費保険番号
                   07  RECE01-KOHSKY-KOHHKNNUM-O
                                          PIC  X(03).
      *患者情報
           03  RECE01-PTINF.
      *患者番号  
               05  RECE01-PTNUM           PIC  X(20).
      *カナ氏名
               05  RECE01-KANANAME        PIC  X(50).
      *漢字氏名
               05  RECE01-NAME            PIC  X(100).
      *性別
               05  RECE01-SEX             PIC  X(01).
      *生年月日
               05  RECE01-BIRTHDAY        PIC  9(08).
      *死亡区分
               05  RECE01-DEATHKBN        PIC  X(01).
      *年齢
               05  RECE01-AGE             PIC  9(03).
      *就学前区分
               05  RECE01-SYUGAKUKBN      PIC 9(01).
      *テスト患者番号区分
               05  RECE01-TSTPTNUMKBN     PIC  X(01).
      *自宅−郵便番号
               05  RECE01-HOME-POST       PIC X(07).
      *自宅−住所
               05  RECE01-HOME-ADRS       PIC X(200).
      *自宅−番地方書
               05  RECE01-HOME-BANTI      PIC X(200).
      *連絡先−名称
               05  RECE01-RENRAKU-NAME    PIC X(100).
      *連絡先−郵便番号
               05  RECE01-RENRAKU-POST    PIC X(07).
      *連絡先−住所
               05  RECE01-RENRAKU-ADRS    PIC X(200).
      *連絡先−番地方書
               05  RECE01-RENRAKU-BANTI   PIC X(200).
      *連絡先−昼電話番号
               05  RECE01-RENRAKU-TEL1    PIC X(15).
      *連絡先−夜電話番号
               05  RECE01-RENRAKU-TEL2    PIC X(15).
      *県番号
           03  RECE01-PREFNUM             PIC  9(02).
      *印刷順
           03  RECE01-PRTSEQ              PIC  9(04).
      *未請求フラグ
           03  RECE01-MISEIKYU-FLG        PIC X(02).
      *未訂正区分
           03  RECE01-TEISEIKBN           PIC  X(01).
      *点検レセ０点公費フラグ
           03  RECE01-TENKEN-FLG          PIC X(01).
      *レセプト種別不明エラー情報
           03  RECE01-ERR-AREA.
               05  RECE01-ERR-ERRKBN      PIC 9(01).
               05  RECE01-ERR-DAY         PIC 9(02).
               05  RECE01-ERR-HKNCOMBINUM PIC 9(04).
               05  RECE01-ERR-TEKSTYMD    PIC 9(08).
               05  RECE01-ERR-TEKEDYMD    PIC 9(08).
