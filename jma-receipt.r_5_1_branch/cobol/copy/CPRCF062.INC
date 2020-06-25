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
      *    レセプト明細書     COPY (CPRCF062.INC)                *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    03.02.06  コメントカウントフラグの追加
      *  01.00.02    NACL-藤原    03.02.12  テスト患者番号区分の追加
      *  01.00.03    NACL-藤原    03.03.03  診療科数１０から２０へ変更
      *  01.00.04    NACL-藤原    03/03/07  レセプト記載外公費情報追加
      *  01.00.05    NACL-藤原    03.03.11  レセプト種別不明情報の追加
      *  01.00.06    NACL-門脇    03.04.16  船員保険（被保険者）＋通勤災害
      *                                     フラグの追加
      *  01.00.07    NACL-門脇    03.05.28  病棟番号・種別の追加
      *  01.00.08    NACL-門脇    03.06.19  実日数テーブルの追加
      *  01.00.09    NACL-門脇    04.01.16  公費分離区分最小値の追加
      *  01.00.10    NACL-門脇    04.01.19  病床数編集フラグの追加
      *  01.00.11    NACL-門脇    04.01.20  自費レセ摘要欄編集レコードフラグ
      *                                     の追加
      *  01.00.12    NACL-門脇    04.03.17  福祉編集フラグの追加
      *                                     福祉編集フラグ（食事療養）の追加
      *                                     合計点数情報（ぶん点・公費分離
      *                                     区分編集用）の追加
      *  01.00.13    NACL-藤原    04.06.08  適用終了日の追加
      *  01.00.14    NACL-藤原    04.06.15  印刷順の追加（個別作成時のみ）
      *  01.00.15    NACL-藤原    04.07.06  組合せ公費保険ＩＤの追加
      *  01.00.16    NACL-藤原    04.09.13  ＯＣＲエリアを追加
      *  01.00.17    NACL-藤原    04.10.31  県番号の追加
      *  01.00.18    NACL-門脇    04.12.15  公費一部負担金レセ記載フラグ
      *                                     の追加
      *                                     公費一部負担金０円記載フラグ
      *                                     の追加
      *  01.00.19    NACL-門脇    05.03.11  負担金額情報に項目追加
      *                                     レセプト記載外公費情報に項目追加
      *  01.00.20    NACL-藤原    05.03.24  レセプト特記事項編集情報の追加
      *  01.00.21    NACL-小豆沢  05.06.17  福岡公費の給付外点数の追加
      *
      *  ver.2.6.0 
      *  02.06.01    NACL-藤原    05.07.13  老人保険−支払区分
      *                                     本人−入院−内容
      *                                     保険組合せ公費フラグの追加
      *  02.06.02    NACL-藤原    05.07.28  レセプト請求しない公費の追加
      *  02.06.03    NACL-藤原    05.08.05  記載外公費−負担者番号の追加
      *
      *  02.07.01    NACL-藤原    05/08/20  特別療養費対応
      *                                     レセプト記載用レセプト種別の追加
      *  02.07.02    NACL-門脇    06/02/07  食事基本料情報（その他）追加
      *
      *  02.08.01    NACL-藤原    06/02/20  主科対応
      *                                     主科追加 
      *
      *  02.09.01    NACL-藤原    06/04/14  ２００６年４月改正対応
      *                                     外来迅速検体検査加算フラグ追加
      *  03.03.01    NACL-門脇    06/10/10  入院時生活療養費食事対応
      *
      *  03.04.01    NACL-藤原    06/12/05  未請求レセ対応
      *  03.04.02    NACL-門脇    07/02/19  公費の一部負担金再掲追加
      *                                     公費の一部負担金再掲フラグ追加
      *                                     公費−本人−入院−回−負担割合追加
      *  03.04.03    NACL-門脇    07/03/27  高齢者一部負担金再掲２追加
      *  03.05.01    NACL-小豆沢  07/05/30  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/19  最終病棟・病室番号追加
      *  03.05.03    NACL-門脇    07/08/01  所得情報取扱い機能強化対応
      *  03.05.04    NACL-門脇    07/08/01  標準負担額減額認定証設定対応
      *  03.05.05    NACL-門脇    07/08/01  月途中の所得情報変更対応
      *  04.01.01    NACL-門脇    07/12/19  入院料未算定日対応
      *
      *  04.01.01    NACL-藤原    08/01/10  レセプト特記事項編集情報
      *                                     適用欄コメント対応
      *
      *  04.02.01    NACL-藤原    08/03/25  平成２０年４月改正対応
      *                                     就学前区分の追加
      *
      *  04.03.01    NACL-藤原    08/06/19  医師別出力対応
      *  04.03.02    NACL-門脇    08/07/07  実日数・請求点記載フラグ追加
      *  04.03.03    NACL-藤原    08/08/05  患者登録特記事項入力対応
      *                                     患者特記事項追加
      *  04.03.04    NACL-門脇    08/09/05  レセ電用負担区分編集フラグ追加
      *                                     予備追加
      *  04.03.05    NACL-門脇    08/10/16  公費−低２・低１項目追加
      *
      *  04.04.01    NACL-門脇    09/02/16  地方公費のレセ記載対応
      *  04.04.02    NACL-藤原    09/03/10  特別な関係にある他院歴を初回
      *                                     入院日として起算しレセプト記
      *                                     載を行なう対応
      *  04.04.03    NACL-藤原    09/03/25  生保まとめフラグ追加
      *
      *  04.05.01    NACL-門脇    09/12/04  食事療養費情報（ぶん点・
      *                                     公費分離区分編集用）追加
      *  04.05.02    NACL-門脇    10/02/08  全国公費併用分請求点集計追加
      *                                     福祉編集フラグにインデックス追加
      *  04.05.03    NACL-藤原    11/02/22  レセ電データの記号番号区切り
      *                                     文字設定対応
      *  04.05.04    NACL-藤原    11/03/25  災害対応
      *                                     連絡先情報追加
      *  04.05.05    NACL-藤原    11/11/22  同日再入院対応
      *  04.05.06    NACL-門脇    12/04/09  入院略称の文字数拡張
      *  04.05.07    NACL-門脇    12/08/16  レセ電用負担区分編集フラグ（食事）
      *                                     追加
      *
      *  04.06.01    NACL-藤原    10/06/23  レセ電請求時の受付エラー分の
      *                                     種別不明レセ対応
      *                                     レセプト種別不明エラー情報変更
      *  04.06.02    NACL-門脇    10/12/13  請求額情報の追加
      *  04.06.03    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.04    NACL-藤原    11/12/12  点検レセ０点公費印字対応
      *  04.06.05    NACL-門脇    13/06/14  食事標準負担額記録判定用フラグ追加
      *
      *  04.07.01    NACL-門脇    11/09/01  システム管理２０１０機能追加対応
      *                                     一部負担金金額集計フラグ追加
      *                                     保険欄−一部負担金記載
      *                                     （高齢者）（70歳未満）フラグ追加
      *                                     食事区分追加
      *  04.07.02    NACL-藤原    12/01/06  予備を２０に変更
      *  04.07.03    NACL-門脇    12/08/27  システム管理２０１０機能追加対応
      *  04.07.04    NACL-門脇    12/10/02  高齢者判定フラグ追加
      *  04.07.05    NACL-藤原    12/12/17  複数枚レセプト分割対応
      *  04.07.06    NACL-藤原    13/12/09  医療情報連携基盤対応
      *  04.07.07    NACL-門脇    13/07/10  自賠責（第三者行為）対応
      *  04.07.08    NACL-門脇    14/01/08  請求点数がない保険組み合わせ有り
      *                                     フラグ追加
      *  04.07.09    NACL-門脇    14/05/13  超重症児（者）入院診療加算等の
      *                                     算定フラグ追加
      *  04.07.10    NACL-門脇    14/07/31  第三者行為に関するフラグ追加
      *
      *  04.08.01    NACL-門脇    14/07/29  ３２文字以上の文字列修正
      *                                     （コンパイル時のワーニング修正）
      *                                     RECE62-FUKUSHIFLG-SKJIPA-AREA
      *                                     RECE62-FUKUSHIFLG-SKJSEI-AREA
      *  04.08.02    NACL-門脇    15/12/02  ３歳児視覚聴覚精密健康診査
      *                                     （地域公費）対応
      *  04.08.03    NACL-門脇    16/02/08  予備項目使用分について、
      *                                     個々に項目を設ける。
      *                                     第３第４公費の括弧再掲記載対応
      *  04.08.04    NACL-門脇    16/03/15  生保まとめ用項目追加
      *  04.08.05    NACL-門脇    16/03/31  平成２８年４月改正対応
      *  04.08.06    NACL-門脇    16/05/30  １９原爆一般の特殊計算対応
      *  04.08.07    NACL-門脇    17/03/17  保険別請求チェック表の機能追加
      *                                     （公費請求書に関わる対応）
      *  04.08.08    NACL-門脇    17/03/27  請求金額０円レセプト作成対応
      *  04.08.09    NACL-門脇    17/09/26  平成２９年１０月改正
      *                                     （入院時生活療養費の見直し）
      *  04.08.10    NACL-門脇    17/10/18  入院料と異なる保険組み合わせ
      *                                     での食事算定対応
      *  04.08.11    NACL-門脇    18/03/27  平成３０年４月改正対応
      *
      *  05.00.01    NACL-門脇    19/03/13  システム管理２０１０機能追加対応
      *  05.00.02    NACL-藤原    19/04/04  印刷順の桁数変更
      *  05.00.03    NACL-門脇    19/06/04  生保に関わる保険組み合わせ作成
      *                                     対応（主保険なしの場合）
      *  05.00.04    NACL-門脇    19/07/01  リハビリ発症日レセプト記載対応
      *                                     （機能追加）
      *  05.00.05    NACL-門脇    19/10/01  括弧再掲記載に関わるフラグ追加
      *  05.00.06    NACL-門脇    19/11/15  マル長レセプト記載処理区分追加
      ************************************************************
      *01  RECE62-REC.
           03  RECE62-KEY.
      *医療機関ＩＤ
               05  RECE62-HOSPNUM         PIC  9(2).
      *診療年月
               05  RECE62-SRYYM           PIC  9(06).
      *入外区分
               05  RECE62-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  RECE62-PTID            PIC  9(10).
      *レセプト科
               05  RECE62-RECEKA          PIC  X(02).
      *審査支払機関
               05  RECE62-TEISYUTUSAKI    PIC  9(01).
      *レセプト種別
               05  RECE62-RECESYUBETU     PIC  9(04).
      *保険者番号        
               05  RECE62-HKNJANUM-KEY    PIC  X(08).
      *補助区分
               05  RECE62-HOJOKBN-KEY     PIC  X(01).
      *公費ＩＤ
               05  RECE62-KOHID-KEY       PIC  9(10).
      *適用開始年月日
               05  RECE62-TEKSTYMD        PIC  X(08). 
      *分割区分
               05  RECE62-RECEIPT-KBN     PIC  X(10).
      *患者番号
           03  RECE62-PTNUM               PIC  X(20).
      *診療科情報
           03  RECE62-SRYKAINF            OCCURS  20.
      *診療科
               05  RECE62-SRYKA           PIC  X(02).
      *カナ氏名
           03  RECE62-KANANAME            PIC  X(50).
      *漢字氏名
           03  RECE62-NAME                PIC  X(100).
      *性別
           03  RECE62-SEX                 PIC  X(01).
      *生年月日
           03  RECE62-BIRTHDAY            PIC  9(08).
      *年齢
           03  RECE62-AGE                 PIC  9(03).
      *就学前区分
           03  RECE62-SYUGAKUKBN          PIC 9(01).
      *死亡区分
           03  RECE62-DEATHKBN            PIC  X(01).
      *自宅−郵便番号
           03  RECE62-HOME-POST           PIC X(07).
      *自宅−住所
           03  RECE62-HOME-ADRS           PIC X(200).
      *自宅−番地方書
           03  RECE62-HOME-BANTI          PIC X(200).
      *連絡先−名称
           03  RECE62-RENRAKU-NAME.
               05  RECE62-RENRAKU-NAME-R  PIC X(50)
                                              OCCURS  2.
      *連絡先−郵便番号
           03  RECE62-RENRAKU-POST        PIC X(07).
      *連絡先−住所
           03  RECE62-RENRAKU-ADRS        PIC X(200).
      *連絡先−番地方書
           03  RECE62-RENRAKU-BANTI       PIC X(200).
      *連絡先−昼電話番号
           03  RECE62-RENRAKU-TEL1        PIC X(15).
      *連絡先−夜電話番号
           03  RECE62-RENRAKU-TEL2        PIC X(15).
      *自宅−編集郵便番号
           03  RECE62-HOME-POST1          PIC X(14).
      *自宅−編集住所（住所＋番地）
           03  RECE62-HOME-ADRS1.
               05  RECE62-HOME-ADRS-R     PIC X(50)
                                              OCCURS  4.
      *連絡先−編集郵便番号
           03  RECE62-RENRAKU-POST1       PIC X(14).
      *連絡先−編集住所（住所＋番地）
           03  RECE62-RENRAKU-ADRS1.
               05  RECE62-RENRAKU-ADRS-R  PIC X(50)
                                              OCCURS  4.
      *連絡先−編集電話番号（昼電話番号全角）
           03  RECE62-TEL                 PIC X(30).
      *レセプト対象年月日情報
           03  RECE62-TEKYMD.
      *適用開始年月日
               05  RECE62-TEKSTYMD-X      PIC  X(08). 
      *適用終了年月日
               05  RECE62-TEKEDYMD-X      PIC  X(08). 
      *記載用レセプト種別（特別療養費）
           03  RECE62-RECESYUBETU-X       PIC  X(04).
      *記載用レセプト種別（紙レセプト）
           03  RECE62-RECESYUBETU-Y       PIC  X(04).
      *主科情報
           03  RECE62-SYUKA-AREA.
      *主科
               05  RECE62-SYUKA           PIC  X(02).
      *主科（レセ電算診療科）
               05  RECE62-RECESYUKA       PIC  X(02).
      *主科診療科名
               05  RECE62-SYUKANAME       PIC  X(80).
      *医師別出力情報
           03  RECE62-DRCD-AREA.
      *医師コード
               05  RECE62-DRCD            PIC  X(05).
      *医師別診療科
               05  RECE62-DRCD-SRYKA      PIC  X(02).
      *主保険情報
           03  RECE62-HKN.
      *主保険−保険ＩＤ
               05  RECE62-HKNID           PIC  9(10).
      *主保険−保険番号
               05  RECE62-HKNNUM          PIC  X(03).
      *主保険−法別番号
               05  RECE62-HKNHBTNUM       PIC  X(02).
      *主保険−保険者番号
               05  RECE62-HKNJANUM        PIC  X(08).
      *主保険−本人家族区分
               05  RECE62-HONKZKKBN       PIC  X(01).
      *主保険−補助区分
               05  RECE62-HOJOKBN         PIC  X(01).
      *主保険−継続区分
               05  RECE62-CONTKBN         PIC  X(01).
      *主保険−記号
               05  RECE62-KIGO            PIC  X(80).
      *主保険−番号
               05  RECE62-NUM             PIC  X(80).
      *主保険−給付割合
               05  RECE62-KYURATE         PIC  9(03).
      *主保険−証明書番号
               05  RECE62-CERTNUM         PIC  X(03).
      *老人保健情報
           03  RECE62-RJNHKN.
      *老人保険−保険ＩＤ
               05  RECE62-RJNHKNID        PIC  9(10).
      *老人保険−保険番号
               05  RECE62-RJNHKNNUM       PIC  X(03).
      *老人保険−法別番号
               05  RECE62-RJNHBTNUM       PIC  X(02).
      *老人保険−市町村番号
               05  RECE62-RJNFTNJANUM     PIC  X(08).
      *老人保険−受給者番号
               05  RECE62-RJNJKYSNUM      PIC  X(07).
      *老人保険−支払区分
               05  RECE62-RJNPAYKBN       PIC  X(02).
      *
      *公費情報
           03  RECE62-KOHINF-AREA.
               05  RECE62-KOHINF          OCCURS  4.
      *公費−保険ＩＤ
                   07  RECE62-KOHID       PIC  9(10).
      *公費−保険番号
                   07  RECE62-KOHNUM      PIC  X(03).
      *公費−法別番号
                   07  RECE62-KOHHBTNUM   PIC  X(02).
      *公費−負担者番号
                   07  RECE62-KOHFTNJANUM PIC  X(08).
      *公費−受給者番号
                   07  RECE62-KOHJKYSNUM  PIC  X(20).
      *公費−任意給付区分
                   07  RECE62-KOHNINIKYUKBN
                                          PIC  X(01).
      *公費−件数
                   07  RECE62-KOHKENSU    PIC  9(03).
      *公費−レセプト請求区分
                   07  RECE62-RECESKYKBN  PIC  X(01).
      *公費−適用開始年月日
                   07  RECE62-KOHTEKSTYMD PIC  X(08).
      *公費−適用終了年月日
                   07  RECE62-KOHTEKEDYMD PIC  X(08).
      *
      *公費−本人−入院−負担区分
                   07  RECE62-HON-NYUFTNKBN     PIC  X(01).
      *公費−本人−入院−回−負担割合
                   07  RECE62-HON-NYUKFTNRATE   PIC  9(03).
      *公費−本人−入院−日−上限額
                   07  RECE62-HON-NYUDJGNGAK    PIC  9(06).
      *公費−本人−入院−月−上限額
                   07  RECE62-HON-NYUMJGNGAK    PIC  9(06).
      *公費−本人−入院−月−上限回数
                   07  RECE62-HON-NYUMJGNCNT    PIC  9(02).
      *公費−本人−入院−食事療養区分
                   07  RECE62-HON-NYUSKJRYYKBN  PIC  X(01).
      *
      *公費−低２−入院−負担区分
                   07  RECE62-TSY-NYUFTNKBN     PIC  X(01).
      *公費−低２−入院−回−負担割合
                   07  RECE62-TSY-NYUKFTNRATE   PIC  9(03).
      *公費−低２−入院−日−上限額
                   07  RECE62-TSY-NYUDJGNGAK    PIC  9(06).
      *公費−低２−入院−月−上限額
                   07  RECE62-TSY-NYUMJGNGAK    PIC  9(06).
      *公費−低２−入院−月−上限回数
                   07  RECE62-TSY-NYUMJGNCNT    PIC  9(02).
      *公費−低２−入院−食事療養区分
                   07  RECE62-TSY-NYUSKJRYYKBN  PIC  X(01).
      *
      *公費−低１−入院−負担区分
                   07  RECE62-TNK-NYUFTNKBN     PIC  X(01).
      *公費−低１−入院−回−負担割合
                   07  RECE62-TNK-NYUKFTNRATE   PIC  9(03).
      *公費−低１−入院−日−上限額
                   07  RECE62-TNK-NYUDJGNGAK    PIC  9(06).
      *公費−低１−入院−月−上限額
                   07  RECE62-TNK-NYUMJGNGAK    PIC  9(06).
      *公費−低１−入院−月−上限回数
                   07  RECE62-TNK-NYUMJGNCNT    PIC  9(02).
      *公費−低１−入院−食事療養区分
                   07  RECE62-TNK-NYUSKJRYYKBN  PIC  X(01).
      *
      *保険組合せ番号情報
           03  RECE62-HKNCOMBIINF-AREA.
               05  RECE62-HKNCOMBIINF     OCCURS  10.
      *保険組合せ番号
                   07  RECE62-HKNCOMBI    PIC  9(04).
      *主保険番号
                   07  RECE62-HKNNUMHC    PIC  X(03).
      *組合せ公費保険番号情報
                   07  RECE62-KOHNUMHCINF OCCURS  4.
      *組合せ公費保険番号
                       09  RECE62-KOHNUMHC
                                          PIC  X(03).
      *組合せ公費保険ＩＤ
                       09  RECE62-KOHIDHC PIC  9(10).
      *全国公費併用分請求点集計区分
                       09  RECE62-SYUKEIKBN
                                          PIC  X(01).
      *全国公費併用分請求点集計対象区分
                       09  RECE62-SYUKEI-TAIKBN
                                          PIC  X(01).
      *レセ電用負担区分編集フラグ
                       09  RECE62-FTNKBNFLG
                                          PIC  9(01).
      *レセ電用負担区分編集フラグ（食事）
                       09  RECE62-FTNKBNFLG-SKJ
                                          PIC  9(01).
      *保険組合せ公費フラグ（１：全国公費あり）
                   07  RECE62-HKNCOMBIFLG PIC  9(01).
      *
      *診療区分別点数情報
           03  RECE62-SRYKBNINF           OCCURS  30.
      *基本点数
               05  RECE62-KHNTEN          PIC  9(08).
      *回数
               05  RECE62-KAISU           PIC  9(05).
      *明細点数情報
               05  RECE62-TENSUINF-AREA.
                   07  RECE62-TENSUINF    OCCURS  5.
      *明細点数
                       09  RECE62-TENSU   PIC  9(08).
      *
      *合計点数情報
           03  RECE62-TOTALTENINF-AREA.
               05  RECE62-TOTALTENINF     OCCURS  5.
      *合計点数
                   07  RECE62-TOTALTEN    PIC  9(08).
      *
      *負担金額情報
           03  RECE62-FTNMONEYINF-AREA.
               05  RECE62-FTNMONEYINF     OCCURS  5.
      *負担金額
                   07  RECE62-FTNMONEY    PIC  9(09).
      *負担金額（一部負担額）
                   07  RECE62-FTNMONEY-FTN    PIC  9(09).
      *負担金額（控除額）
                   07  RECE62-FTNMONEY-KOJ    PIC  9(09).
      *
      *実日数情報
           03  RECE62-JNISSUINF-AREA.
               05  RECE62-JNISSUINF       OCCURS  5.
      *実日数
                   07  RECE62-JNISSU      PIC  9(02).
      *
      *入院基本料種別情報
           03  RECE62-NYUSYUINF           OCCURS  20.
      *入院基本料種別
               05  RECE62-NYUSYUBETU      PIC  X(20).
      *
      *入院基本料情報
           03  RECE62-NYUKHNINF           OCCURS  20.
      *入院基本料・加算
               05  RECE62-NYUKHNRYO       PIC  9(09).
      *入院日数
               05  RECE62-NYUKHNNISSU     PIC  9(02).
      *合計金額情報
               05  RECE62-NYUKHNKEIINF-AREA.
                   07  RECE62-NYUKHNKEIINF    OCCURS  5.
      *合計金額
                       09  RECE62-NYUKHNKEI   PIC  9(09).
      *入院基本料情報２
           03  RECE62-NYUKHNINF2.
      *合計金額情報２
               05  RECE62-NYUKHNKEIINF2   OCCURS  5.
      *合計金額２
                   07  RECE62-NYUKHNKEI2  PIC S9(09).
      *
      *特定入院基本料情報
           03  RECE62-NYUTOKINF           OCCURS  20.
      *特定入院料・その他（点）
               05  RECE62-NYUTOKRYO       PIC  9(09).
      *特定入院料・その他（略称）
               05  RECE62-NYUTOKRYO-RYAKU PIC  X(20).
      *入院日数
               05  RECE62-NYUTOKNISSU     PIC  9(02).
      *合計金額情報
               05  RECE62-NYUTOKKEIINF-AREA.
                   07  RECE62-NYUTOKKEIINF    OCCURS  5.
      *合計金額
                       09  RECE62-NYUTOKKEI   PIC  9(09).
      *
      *特定入院基本料情報２
           03  RECE62-NYUTOKINF2.
      *合計金額情報２
               05  RECE62-NYUTOKKEIINF2   OCCURS  5.
      *合計金額２
                   07  RECE62-NYUTOKKEI2  PIC S9(09).
      *
      *食事療養費情報
           03  RECE62-SHOKUJIINF-AREA.
               05  RECE62-SHOKUJIINF      OCCURS  5.
      *食事日数
                   07  RECE62-SHOKUJINISSU
                                          PIC  9(02).
      *食事療養費
                   07  RECE62-SHOKUJIRYOYOHI
                                          PIC  9(09).
      *標準負担額
                   07  RECE62-SHOKUJIFTN  PIC  9(09).
      *
      *食事療養費情報２
           03  RECE62-SHOKUJIINF2-AREA.
               05  RECE62-SHOKUJIINF2     OCCURS  5.
      *食事区分
                   07  RECE62-SKJRYYKBN   PIC  X(01).
      *
      *一部負担金・食事療養費標準負担額区分
           03  RECE62-SHOKUJIFTNKBN       PIC  X(01).
      *一部負担金・食事療養費標準負担額区分２（地方公費判定用）
           03  RECE62-SHOKUJIFTNKBN2-INF.
               05  RECE62-SHOKUJIFTNKBN2  PIC  X(01)
                                          OCCURS  4.
      *
      *食事種別情報‐基本
           03  RECE62-SHOKUJISYUBETU-KHN  PIC  X(02).
      *
      *食事種別情報
           03  RECE62-SHOKUJISYUINF       OCCURS   3.
      *食事種別
               05  RECE62-SHOKUJISYUBETU  PIC  X(04).
      *
      *食事基本料情報
           03  RECE62-SHOKUJIKHNINF       OCCURS   4.
      *食事基本料
               05  RECE62-SHOKUJIKHNRYO   PIC  9(09).
      *食事日数
               05  RECE62-SHOKUJIKHNNISSU PIC  9(02).
      *
      *食事種別情報‐基本（生活）
           03  RECE62-SHOKUJISYUBETU-KHNSEI   PIC  X(02).
      *
      *食事基本料情報（生活）
           03  RECE62-SHOKUJIKHNINF-SEI   OCCURS   2.
      *食事基本料
               05  RECE62-SHOKUJIKHNRYO-SEI   PIC  9(09).
      *食事日数
               05  RECE62-SHOKUJIKHNNISSU-SEI PIC  9(02).
      *
      *食事基本料情報（その他）
           03  RECE62-SHOKUJIKHNINF-ETC       OCCURS   3.
      *食事基本料
               05  RECE62-SHOKUJIKHNRYO-ETC   PIC  9(09).
      *食事日数
               05  RECE62-SHOKUJIKHNNISSU-ETC PIC  9(02).
      *
      *入院時生活療養費に係る生活療養食事算定フラグ
           03  RECE62-SEIKATURYOYO        PIC  X(01).
      *
      *最終診療科
           03  RECE62-LASTSRYKA           PIC  X(02).
      *最終病棟番号
           03  RECE62-LASTBTUNUM          PIC  X(02).
      *最終病室番号
           03  RECE62-LASTBRMNUM          PIC  X(08).
      *
      *入院年月日
           03  RECE62-NYUINYMD            PIC  X(08).
      *
      *退院年月日
           03  RECE62-TAIINYMD            PIC  X(08).
      *
      *医療機関種別
           03  RECE62-HOSPSBT             PIC  X(01).
      *特記事項情報
           03  RECE62-TOKKIINF            OCCURS  5.
      *特記事項
               05  RECE62-TOKKI           PIC  X(02).
      *病棟区分情報
           03  RECE62-BYOTOKBNINF         OCCURS  4.
      *病棟区分
               05  RECE62-BYOTOKBN        PIC  X(02).
      *病棟種別情報
           03  RECE62-BYOTOSYUBETUINF     OCCURS 31.
      *病棟番号
               05  RECE62-BYOTONUM        PIC  X(02).
      *病棟種別
               05  RECE62-BYOTOSYUBETU    PIC  X(02).
      *レセプトＯＣＲ１
           03  RECE62-OCR1                PIC  X(67).
      *レセプトＯＣＲ２
           03  RECE62-OCR2                PIC  X(67).
      *レセプトＯＣＲ３
           03  RECE62-OCR3                PIC  X(67).
      *
      *レセプト記載外公費情報
           03  RECE62-ETC-KOH-G.
               05  RECE62-ETC-KOHINF      OCCURS  4.
      *記載外公費−保険番号
                   07  RECE62-ETC-KOHNUM  PIC  X(03).
      *記載外公費−負担者番号
                   07  RECE62-ETC-KOHFTNJANUM
                                          PIC  X(08).
      *記載外公費−実日数
                   07  RECE62-ETC-JNISSU  PIC  9(02).
      *記載外公費−合計点数
                   07  RECE62-ETC-TOTALTEN
                                          PIC  9(08).
      *記載外公費−負担金額
                   07  RECE62-ETC-FTNMONEY
                                          PIC  9(09).
      *記載外公費−負担金額（一部負担額）
                   07  RECE62-ETC-FTNMONEY-FTN
                                          PIC  9(09).
      *記載外公費−負担金額（控除額）
                   07  RECE62-ETC-FTNMONEY-KOJ
                                          PIC  9(09).
      *記載外公費−食事日数
                   07  RECE62-ETC-SHOKUJINISSU
                                          PIC  9(02).
      *記載外公費−食事療養費
                   07  RECE62-ETC-SHOKUJIRYOYOHI
                                          PIC  9(09).
      *記載外公費−標準負担額
                   07  RECE62-ETC-SHOKUJIFTN
                                          PIC  9(09).
      *
      *レセプト請求しない公費領域
           03 RECE62-KOHSKY-KOHI-AREA.
               05  RECE62-KOHSKY-KOHI-TBL OCCURS  4.
      *    公費ＩＤ
                   07  RECE62-KOHSKY-KOHID
                                          PIC  9(10).
      *    公費保険番号
                   07  RECE62-KOHSKY-KOHNUM
                                          PIC  X(03).
      *    公費負担者番号
                   07  RECE62-KOHSKY-KOHFTNJANUM
                                          PIC  X(08).
      *低所得フラグ
           03  RECE62-TEISYOTOKU          PIC  X(01).
      *低所得フラグ２（地方公費判定用）
           03  RECE62-TEISYOTOKU2-INF.
               05  RECE62-TEISYOTOKU2     PIC  X(01)
                                          OCCURS  4.
      *低所得フラグ３（食事判定用）
           03  RECE62-TEISYOTOKU3         PIC  X(01).
           03  RECE62-TEISYOTOKU3-KKSKBN  PIC  X(01).
           03  RECE62-TEISYOTOKU3-SKJKBN  PIC  X(01)  OCCURS 31.
      *高齢者フラグ
           03  RECE62-KOUREI              PIC  X(01).
      *高齢者一部負担金再掲
           03  RECE62-KOUREI-SAIKEI       PIC  9(09).
      *高齢者一部負担金再掲２
           03  RECE62-KOUREI-SAIKEI2      PIC  9(09).
      *再掲３
           03  RECE62-KOUREI-SAIKEI3      PIC  9(09).
      *再掲４
           03  RECE62-KOUREI-SAIKEI4      PIC  9(09).
      *公費の一部負担金再掲
           03  RECE62-KOH-SAIKEI          PIC  9(09).
      *公費の一部負担金再掲フラグ
           03  RECE62-KOH-SAIKEIFLG       PIC  9(01).
      *船員保険（被保険者）＋通勤災害フラグ
           03  RECE62-SENIN-TUUSAI        PIC  X(01).
      *初診時間区分
           03  RECE62-SHOSHINTIME         PIC  X(01).
      *老人減免区分（高齢者経過措置フラグとして使用）
           03  RECE62-RJNGENMENKBN        PIC  X(01).
      *老人外来金額（請求点数合計として使用）
           03  RECE62-RJNGAIFTN           PIC  9(09).
      *老人入院金額（未使用）
           03  RECE62-RJNNYUFTN           PIC  9(09).
      *減免区分
           03  RECE62-GENMENKBN           PIC  9(01).
      *減額割合
           03  RECE62-GENGAKURATE         PIC  9(03).
      *減額金額
           03  RECE62-GENGAKU             PIC  9(07).
      *割引点数単価（７０歳未満現物給付の所得区分として使用）
           03  RECE62-DISCOUNTTENTNK      PIC  X(02).
      *老人公費負担金額情報
           03  RECE62-RJNKOHFTNINF        OCCURS  3.
      *老人外来公費負担額（（１）（２）（３）使用）
               05  RECE62-RJNGAIKOHFTN    PIC  9(09).
      *老人入院公費負担額（（１）（２）（３）使用）
               05  RECE62-RJNNYUKOHFTN    PIC  9(09).
      *コメントカウントフラグ
           03  RECE62-COMCNTFLG           PIC  X(01).
      *テスト患者番号区分
           03  RECE62-TSTPTNUMKBN         PIC  X(01).
      *レセプト種別不明エラー情報
           03  RECE62-ERR-AREA.
               05  RECE62-ERR-ERRKBN      PIC 9(02).
               05  RECE62-ERR-DAY         PIC 9(02).
               05  RECE62-ERR-HKNCOMBINUM PIC 9(04).
               05  RECE62-ERR-TEKSTYMD    PIC 9(08).
               05  RECE62-ERR-TEKEDYMD    PIC 9(08).
      *他保険入院料算定済みフラグ
           03  RECE62-NYUINRYOFLG         PIC  X(01).
      *
      *実日数テーブル情報
           03  RECE62-JNISSUTBLINF        OCCURS 31.
      *実日数テーブル
               05  RECE62-JNISSUTBL       PIC  X(01).
      *
      *公費実日数テーブル情報
           03  RECE62-KOHJNISSUTBLINF.
      *公費実日数テーブル
               05  RECE62-KOHJNISSUTBL    PIC  X(01)
                                          OCCURS 31.
      *県番号
           03  RECE62-PREFNUM             PIC  9(02).
      *県内・県外区分（１：県内　２：県外　）
           03  RECE62-PREFKBN             PIC  X(01).
      *県番号（保険者）
           03  RECE62-HKNJANUM-PREFNUM    PIC  X(02).
      *公費分離区分最小値
           03  RECE62-KOHBUNRI-MIN        PIC  9(01).
      *病床数編集フラグ（特定疾患療養指導料等）
           03  RECE62-BEDSUFLG            PIC  X(01).
      *自費レセ摘要欄編集レコードフラグ
           03  RECE62-JIHTEKFLG           PIC  X(01).
      *外来迅速検体検査加算フラグ
           03  RECE62-KENTAIKASANFLG      PIC  X(01).
      *福祉編集フラグ
           03  RECE62-FUKUSHIFLGINF-AREA.
               05  RECE62-FUKUSHIFLG      PIC  X(01)   OCCURS 5.
      *特別な関係にある他院歴ありフラグ
           03  RECE62-TAINFLG             PIC  X(01).
      *生保まとめフラグ
           03  RECE62-SEIHOFLG            PIC  X(01).
      *
      *レセプト用：食事療養編集情報
           03  RECE62-FUKUSHIFLG-SKJINF-AREA.
               05  RECE62-FUKUSHIFLG-SKJ  PIC  X(01)   OCCURS 4.
      *
      *レセ電用：食事療養（一般）編集情報
           03  RECE62-FUKUSHIFLG-SKJIPA-AREA.
               05  RECE62-FUKUSHIFLG-SKJIPA
                                          PIC  X(01)   OCCURS 4.
      *レセ電用：食事療養（生活）編集情報
           03  RECE62-FUKUSHIFLG-SKJSEI-AREA.
               05  RECE62-FUKUSHIFLG-SKJSEI
                                          PIC  X(01)   OCCURS 4.
      *
      *合計点数情報（ぶん点・公費分離区分編集用）
           03  RECE62-HENTOTALTENINF-AREA.
               05  RECE62-HENTOTALTENINF  OCCURS  5.
      *合計点数（ぶん点・公費分離区分編集用）
                   07  RECE62-HENTOTALTEN PIC  9(08).
      *
      *食事療養費情報（ぶん点・公費分離区分編集用）
           03  RECE62-HENSHOKUJIINF-AREA.
               05  RECE62-HENSHOKUJIINF   OCCURS  5.
      *食事療養費（ぶん点・公費分離区分編集用）
                   07  RECE62-HENSHOKUJIRYOYOHI
                                          PIC  9(09).
      *
      *印刷順
           03  RECE62-PRTSEQ              PIC  9(04).
      *
      *その他記載フラグ
           03  RECE62-ETCKISAIFLGINF-AREA.
               05  RECE62-ETCKISAIFLGINF  OCCURS  5.
                   07  RECE62-KISAIFLG         PIC  X(01).
                   07  RECE62-ZEROKISAIFLG     PIC  X(01).
      *
                   07  RECE62-EKF-JNISSUFLG    PIC  X(01).
                   07  RECE62-EKF-TOTALTENFLG  PIC  X(01).
                   07  RECE62-EKF-SKJFLG       PIC  X(01).
                   07  RECE62-EKF-OCRFLG       PIC  X(01).
                   07  RECE62-EKF-FTNKEIFLG    PIC  X(01).
                   07  RECE62-EKF-HKNFTNFLG-K  PIC  X(01).
                   07  RECE62-EKF-HKNFTNFLG-K2 PIC  X(01).
                   07  RECE62-EKF-HKNFTNFLG-Y  PIC  X(01).
                   07  RECE62-EKF-HKNFTNFLG-Y2 PIC  X(01).
      *
                   07  RECE62-EKF-KAKKBN-K     PIC  X(01).
                   07  RECE62-EKF-KAK21KBN-K   PIC  X(01).
                   07  RECE62-EKF-KAK22KBN-K   PIC  X(01).
                   07  RECE62-EKF-KAK23KBN-K   PIC  X(01).
                   07  RECE62-EKF-KAK31KBN-K   PIC  X(01).
                   07  RECE62-EKF-KAK32KBN-K   PIC  X(01).
                   07  RECE62-EKF-KAK41KBN-K   PIC  X(01).
                   07  RECE62-EKF-KAK42KBN-K   PIC  X(01).
                   07  RECE62-EKF-KAK43KBN-K   PIC  X(01).
                   07  RECE62-EKF-KAKKBN-Y     PIC  X(01).
                   07  RECE62-EKF-KAK21KBN-Y   PIC  X(01).
                   07  RECE62-EKF-KAK22KBN-Y   PIC  X(01).
                   07  RECE62-EKF-KAK23KBN-Y   PIC  X(01).
                   07  RECE62-EKF-KAK31KBN-Y   PIC  X(01).
                   07  RECE62-EKF-KAK32KBN-Y   PIC  X(01).
                   07  RECE62-EKF-KAK41KBN-Y   PIC  X(01).
                   07  RECE62-EKF-KAK42KBN-Y   PIC  X(01).
                   07  RECE62-EKF-KAK43KBN-Y   PIC  X(01).
                   07  RECE62-EKF-KSINGKBN-K   PIC  X(01).
                   07  RECE62-EKF-KSINGKBN-Y   PIC  X(01).
                   07  RECE62-EKF-ENCHGKBN-K   PIC  X(01).
                   07  RECE62-EKF-ENCHGKBN-Y   PIC  X(01).
                   07  RECE62-EKF-KOH2SKJKBN   PIC  X(01).
                   07  RECE62-EKF-FTNHASUKBN   PIC  X(01).
      *
                   07  RECE62-EKF-SAIKEIKSI    PIC  X(01).
                   07  RECE62-EKF-KINGAKKSN    PIC  X(01).
      *
                   07  RECE62-SKJRYOKBN-HON    PIC  X(01).
      *
                   07  RECE62-MJK-ZEROKISAIFLG    PIC X(01).
                   07  RECE62-MJK-JNISSUFLG       PIC X(01).
                   07  RECE62-MJK-TOTALTENFLG     PIC X(01).
                   07  RECE62-MJK-KISAIFLG        PIC X(01).
                   07  RECE62-MJK-OCRFLG          PIC X(01).
      *
      *法定外現物給付フラグ
           03  RECE62-HOTEIGAIFLGINF-AREA.
               05  RECE62-HOTEIGAIFLGINF  OCCURS  5.
                   07  RECE62-HOTEIGAIFLG         PIC X(01).
      *
      *マル長レセプト記載処理区分
           03  RECE62-CYOKI-KSIKBN        PIC  X(01).
      *
      *実日数・請求点記載フラグ
           03  RECE62-JSKISAIFLG          PIC  X(01).
      *未請求フラグ
           03  RECE62-MISEIKYU-FLG        PIC  X(02).
      *レセプト特記事項編集情報
           03  RECE62-2006-AREA.
      *        特記事項
               05  RECE62-2006-TOKKIINF.
                   07  RECE62-2006-TOKKI  PIC  X(12)
                                               OCCURS  4.
      *        給付割合
               05  RECE62-2006-KYURATE    PIC  9(03).
      *        レセ電：特記事項
               05  RECE62-2006-RECEDEN-TOKKIINF.
                   07  RECE62-2006-RECEDEN-TOKKI
                                          PIC  X(02)
                                               OCCURS  5.
      *        レセ電：給付割合
               05  RECE62-2006-RECEDEN-KYURATE
                                          PIC  9(03).
      *        公費情報
               05  RECE62-2006-KOHINF.
                   07  RECE62-2006-KOH-OCC     OCCURS  4. 
      *                負担者番号記載区分
                       09  RECE62-2006-FTNJANUMKBN
                                          PIC  X(02).
      *                負担者番号
                       09  RECE62-2006-FTNJANUM
                                          PIC  X(08).
      *                受給者番号記載区分
                       09  RECE62-2006-JKYSNUMKBN
                                          PIC  X(02).
      *                受給者番号
                       09  RECE62-2006-JKYSNUM
                                          PIC  X(07).
      *                レセ電：公費レコード区分
                       09  RECE62-2006-RECEDEN-KORECKBN
                                          PIC  X(01).
      *                レセ電：負担者番号記載区分
                       09  RECE62-2006-RECEDEN-FTNJANUMKBN
                                          PIC  X(02).
      *                レセ電：負担者番号
                       09  RECE62-2006-RECEDEN-FTNJANUM
                                          PIC  X(08).
      *                レセ電：受給者番号記載区分
                       09  RECE62-2006-RECEDEN-JKYSNUMKBN
                                          PIC  X(02).
      *                レセ電：受給者番号
                       09  RECE62-2006-RECEDEN-JKYSNUM
                                          PIC  X(07).
      *        適用欄コメント情報
               05  RECE62-2006-TEKIYOU-COMMENTINF.
                   07  RECE62-2006-TEKIYOU-COMMENT-OCC OCCURS  5. 
      *                適用欄上段コメント
                       09  RECE62-2006-TEKIYOU-COMMENT1
                                          PIC  X(52).
      *                適用欄下段コメント
                       09  RECE62-2006-TEKIYOU-COMMENT2
                                          PIC  X(52).
      *                適用欄上段コメントレセ電編集区分
                       09  RECE62-2006-TEKIYOU-COMMENT1KBN
                                          PIC  X(01).
      *                適用欄下段コメントレセ電編集区分
                       09  RECE62-2006-TEKIYOU-COMMENT2KBN
                                          PIC  X(01).
      *
      *福岡県公費給付外点数
           03  RECE62-KYUFUGAITEN         PIC  9(06).
      *
      *患者特記事項情報
           03  RECE62-PTTOKKI-AREA.
               05  RECE62-PTTOKKINF           OCCURS  5.
                   07  RECE62-PTTOKKI-NAIYO
                                          PIC  X(12).
                   07  RECE62-PTTOKKI-CD  PIC  X(02).
      *
      *請求額情報（計算用）
           03  RECE62-FTNMONEY-SKY             PIC 9(09).
      *
      *請求額情報（医療費）
           03  RECE62-SKYIRYO-INF.
      *実日数
               05  RECE62-SKYIRYO-NISSU        PIC 9(02).
      *請求点
               05  RECE62-SKYIRYO-TOTALTEN     PIC 9(08).
      *患者負担
               05  RECE62-SKYIRYO-ICHIBUFTN    PIC 9(09).
      *保険者負担
               05  RECE62-SKYIRYO-HOKENJA      PIC 9(09).
      *公費負担
               05  RECE62-SKYIRYO-KOHFTN       PIC 9(09).
      *
      *請求額情報（食事）
           03  RECE62-SKYSKJ-INF.
      *回数
               05  RECE62-SKYSKJ-KAISU         PIC 9(02).
      *請求円
               05  RECE62-SKYSKJ-RYOYOHI       PIC 9(09).
      *患者負担
               05  RECE62-SKYSKJ-ICHIBUFTN     PIC 9(09).
      *保険者負担
               05  RECE62-SKYSKJ-HOKENJA       PIC 9(09).
      *公費負担
               05  RECE62-SKYSKJ-KOHFTN        PIC 9(09).
      *
      *請求額情報（公費負担のうちレセプト請求しない地方公費分）
           03  RECE62-SKYIRYO-KOHFTN-KOHSKY    PIC 9(09).
           03  RECE62-SKYSKJ-KOHFTN-KOHSKY     PIC 9(09).
      *
      *入院料情報
           03  RECE62-DOUJITSU-AREA.
      *同日再入院（入院分）の保険組合せ
               05  RECE62-DOUJITSU-HKNCOMBI
                                          PIC 9(04)   OCCURS  31.
      *同日再入院（入院分）の食事開始時間区分
               05  RECE62-DOUJITSU-SKJKBN PIC X(01)   OCCURS  31.
      *入院区分
               05  RECE62-NYUINDAY-TBL.
                   07  RECE62-NYUINDAY-KBN
                                          PIC  X(01)  OCCURS  31.
      *
      *点検レセ０点公費フラグ
           03  RECE62-TENKEN-FLG          PIC X(01).
      *
      *高齢者判定フラグ
           03  RECE62-KOUREI-HANTEIFLG    PIC X(01).
      *
      *食事標準負担額記録判定用フラグ（主保険がない場合）（レセ電用）
           03  RECE62-SKJFTNFLG-AREA.
               05  RECE62-SKJFTNFLG       PIC X(01)   OCCURS   4.
               05  RECE62-SKJFTNFLG2      PIC X(01).
      *
      *第三者行為情報（インデックスの情報は保険組合せ番号情報とリンク）
           03  RECE62-DAISANINF-AREA.
             04  RECE62-DAISANINF               OCCURS 10.
      *第三者行為保険組合せフラグ
               05  RECE62-DAISANFLG             PIC 9(01).
      *第三者行為公費ＩＤ
               05  RECE62-DAISAN-KOHID          PIC 9(10).
      *第三者行為保険組合せ現物給付フラグ
               05  RECE62-DAISAN-GENBUTUFLG     PIC 9(01).
      *第三者行為請求点数又は食事あり
               05  RECE62-DAISAN-SKYFLG         PIC 9(01).
      *
      *リハビリ情報（インデックスの情報は保険組合せ番号情報とリンク）
           03  RECE62-RIHAINF-AREA.
             04  RECE62-RIHAINF                 OCCURS 10.
      *リハビリ開始日編集用公費分離区分
               05  RECE62-RIHA-KOHBUNRI         PIC 9(01).
      *リハビリ算定テーブル
               05  RECE62-RIHA-DAY-AREA.
                 06  RECE62-RIHA-DAY-OCC        OCCURS 31.
                   07  RECE62-RIHA-SINDAI       PIC 9(01).
                   07  RECE62-RIHA-KOKYUKI      PIC 9(01).
                   07  RECE62-RIHA-NOUKE        PIC 9(01).
                   07  RECE62-RIHA-UNDOKI       PIC 9(01).
                   07  RECE62-RIHA-NANBYO       PIC 9(01).
                   07  RECE62-RIHA-SYOGAI       PIC 9(01).
                   07  RECE62-RIHA-SESSYOKU     PIC 9(01).
                   07  RECE62-RIHA-HAIYO        PIC 9(01).
      *リハビリ加算算定テーブル
               05  RECE62-RIHA-DAY9-AREA.
                 06  RECE62-RIHA-DAY9-OCC       OCCURS 31.
                   07  RECE62-RIHA-SINDAI9      PIC 9(01).
                   07  RECE62-RIHA-KOKYUKI9     PIC 9(01).
                   07  RECE62-RIHA-HAIYO9       PIC 9(01).
      *リハビリ算定テーブル２
               05  RECE62-RIHA2-DAY-AREA.
                   07  RECE62-RIHA2-SINDAI      PIC 9(01).
                   07  RECE62-RIHA2-SINYMD-OCC1.
                     08  RECE62-RIHA2-SINYMD1   PIC X(08)  OCCURS  2.
                   07  RECE62-RIHA2-SINYMD-OCC2.
                     08  RECE62-RIHA2-SINYMD2   PIC 9(01)  OCCURS 31.
      *
                   07  RECE62-RIHA2-KOKYUKI     PIC 9(01).
                   07  RECE62-RIHA2-KOKYMD-OCC1.
                     08  RECE62-RIHA2-KOKYMD1   PIC X(08)  OCCURS  2.
                   07  RECE62-RIHA2-KOKYMD-OCC2.
                     08  RECE62-RIHA2-KOKYMD2   PIC 9(01)  OCCURS 31.
      *
                   07  RECE62-RIHA2-NOUKE       PIC 9(01).
                   07  RECE62-RIHA2-NOUYMD-OCC1.
                     08  RECE62-RIHA2-NOUYMD1   PIC X(08)  OCCURS  2.
                   07  RECE62-RIHA2-NOUYMD-OCC2.
                     08  RECE62-RIHA2-NOUYMD2   PIC 9(01)  OCCURS 31.
      *
                   07  RECE62-RIHA2-UNDOKI      PIC 9(01).
                   07  RECE62-RIHA2-UNDYMD-OCC1.
                     08  RECE62-RIHA2-UNDYMD1   PIC X(08)  OCCURS  2.
                   07  RECE62-RIHA2-UNDYMD-OCC2.
                     08  RECE62-RIHA2-UNDYMD2   PIC 9(01)  OCCURS 31.
      *
                   07  RECE62-RIHA2-NANBYO      PIC 9(01).
                   07  RECE62-RIHA2-NANYMD-OCC1.
                     08  RECE62-RIHA2-NANYMD1   PIC X(08)  OCCURS  2.
                   07  RECE62-RIHA2-NANYMD-OCC2.
                     08  RECE62-RIHA2-NANYMD2   PIC 9(01)  OCCURS 31.
      *
                   07  RECE62-RIHA2-SYOGAI      PIC 9(01).
                   07  RECE62-RIHA2-SYOYMD-OCC1.
                     08  RECE62-RIHA2-SYOYMD1   PIC X(08)  OCCURS  2.
                   07  RECE62-RIHA2-SYOYMD-OCC2.
                     08  RECE62-RIHA2-SYOYMD2   PIC 9(01)  OCCURS 31.
      *
                   07  RECE62-RIHA2-SESSYOKU    PIC 9(01).
                   07  RECE62-RIHA2-SESYMD-OCC1.
                     08  RECE62-RIHA2-SESYMD1   PIC X(08)  OCCURS  2.
                   07  RECE62-RIHA2-SESYMD-OCC2.
                     08  RECE62-RIHA2-SESYMD2   PIC 9(01)  OCCURS 31.
      *
                   07  RECE62-RIHA2-HAIYO       PIC 9(01).
                   07  RECE62-RIHA2-HAIYMD-OCC1.
                     08  RECE62-RIHA2-HAIYMD1   PIC X(08)  OCCURS  2.
                   07  RECE62-RIHA2-HAIYMD-OCC2.
                     08  RECE62-RIHA2-HAIYMD2   PIC 9(01)  OCCURS 31.
      *リハビリ加算算定テーブル２
               05  RECE62-RIHA2-DAY9-AREA.
                   07  RECE62-RIHA2-SINDAI9     PIC 9(01).
                   07  RECE62-RIHA2-SIN9YMD-OCC1.
                     08  RECE62-RIHA2-SIN9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE62-RIHA2-SIN9YMD-OCC2.
                     08  RECE62-RIHA2-SIN9YMD2  PIC 9(01)  OCCURS 31.
      *
                   07  RECE62-RIHA2-KOKYUKI9    PIC 9(01).
                   07  RECE62-RIHA2-KOK9YMD-OCC1.
                     08  RECE62-RIHA2-KOK9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE62-RIHA2-KOK9YMD-OCC2.
                     08  RECE62-RIHA2-KOK9YMD2  PIC 9(01)  OCCURS 31.
      *
                   07  RECE62-RIHA2-HAIYO9      PIC 9(01).
                   07  RECE62-RIHA2-HAI9YMD-OCC1.
                     08  RECE62-RIHA2-HAI9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE62-RIHA2-HAI9YMD-OCC2.
                     08  RECE62-RIHA2-HAI9YMD2  PIC 9(01)  OCCURS 31.
      *
      *第三者行為レセプト該当フラグ
      *（０：第三者行為の請求点なし）
      *（１：第三者行為のみ請求点あり）
      *（２：第三者行為以外と第三者行為の両方に請求点あり）
      *（３：第三者行為の請求点なし　（第三者行為に院外処方あり））
      *（４：第三者行為のみ請求点あり（第三者行為以外に院外処方あり））
      *（９：第三者行為の請求点なし　（患者登録で第三者行為の登録なし））
           03  RECE62-DAISAN-GAITO              PIC 9(01).
      *第三者行為請求点数合計
           03  RECE62-DAISAN-TOTALTEN           PIC 9(08).
      *第三者行為一部負担金相当額合計
           03  RECE62-DAISAN-FTNMONEY           PIC 9(09).
      *第三者行為食事合計
           03  RECE62-DAISAN-SKJ-NISSU-KEI      PIC 9(02).
           03  RECE62-DAISAN-SKJ-RYOYOHI-KEI    PIC 9(08).
           03  RECE62-DAISAN-SKJ-FTN-KEI        PIC 9(08).
      *第三者行為高齢者フラグ（第三者行為のみ＋高齢者）
           03  RECE62-DAISAN-KOUREI             PIC 9(01).
      *第三者行為レセプト現物給付該当フラグ
           03  RECE62-DAISAN-GENBUTU-GAITO      PIC 9(01).
      *第三者行為一部負担金金額置換フラグ
           03  RECE62-DAISAN-CHIKAN             PIC 9(01).
      *第三者行為特記事項有無フラグ
           03  RECE62-DAISAN-TOKKI              PIC 9(01).
      *
      *請求点数がない保険組み合わせ有りフラグ
           03  RECE62-SKYTENFLG                 PIC 9(01).
      *
      *超重症児（者）入院診療加算等の算定フラグ
           03  RECE62-CYOJKSNFLG                PIC X(01).
      *
      *３歳児視覚聴覚精密健康診査（地域公費）
           03  RECE62-SEIKENFLG                 PIC 9(01).
           03  RECE62-SEIKENTEN                 PIC 9(07).
      *
      *高齢非該当フラグ
           03  RECE62-KOREIHIGAITO              PIC 9(01).
      *公費分離区分置換フラグ
           03  RECE62-KOHBUNRICHIKAN            PIC 9(01).
      *非減免対象分扱い一部負担金
           03  RECE62-GENMEN-NASI-KIN           PIC 9(09).
      *非減免対象分扱い食事標準負担額
           03  RECE62-GENMEN-NASI-SKJ           PIC 9(09).
      *括弧再掲上の０円記載フラグ
           03  RECE62-SAIKEIZEROKSI1            PIC 9(01).
      *括弧再掲下の０円記載フラグ
           03  RECE62-SAIKEIZEROKSI2            PIC 9(01).
      *括弧再掲３の０円記載フラグ
           03  RECE62-SAIKEIZEROKSI3            PIC 9(01).
      *括弧再掲４の０円記載フラグ
           03  RECE62-SAIKEIZEROKSI4            PIC 9(01).
      *東大阪市減免コメントフラグ
           03  RECE62-GENMEN-COM                PIC 9(01).
      *【レセプト】保険欄−一部負担金記載なしフラグ
           03  RECE62-ICBKINKSI-NASI            PIC 9(01).
      *【レセプト】保険欄−一部負担金記載ありフラグ
           03  RECE62-ICBKINKSI-ARI             PIC 9(01).
      *【レセプト】公費欄−一部負担金金額置換フラグ
           03  RECE62-KOHCHIKAN                 PIC 9(01).
      *「（再掲上）：」置換金額
           03  RECE62-SAIKEI-CHIKAN-KIN1        PIC 9(09).
      *「（再掲下）：」置換金額
           03  RECE62-SAIKEI-CHIKAN-KIN2        PIC 9(09).
      *「（再掲３）：」置換金額
           03  RECE62-SAIKEI-CHIKAN-KIN3        PIC 9(09).
      *「（再掲４）：」置換金額
           03  RECE62-SAIKEI-CHIKAN-KIN4        PIC 9(09).
      *「保険負担金：」置換金額
           03  RECE62-HKN-CHIKAN-KIN            PIC 9(09).
      *「公１負担金：」置換金額
           03  RECE62-KOH-CHIKAN-KIN1           PIC 9(09).
      *「公２負担金：」置換金額
           03  RECE62-KOH-CHIKAN-KIN2           PIC 9(09).
      *「公３負担金：」置換金額
           03  RECE62-KOH-CHIKAN-KIN3           PIC 9(09).
      *「公４負担金：」置換金額
           03  RECE62-KOH-CHIKAN-KIN4           PIC 9(09).
      *「公費請求書公１負担金：」置換金額
           03  RECE62-SEI-CHIKAN-KIN1           PIC 9(09).
      *「公費請求書公２負担金：」置換金額
           03  RECE62-SEI-CHIKAN-KIN2           PIC 9(09).
      *「公費請求書公３負担金：」置換金額
           03  RECE62-SEI-CHIKAN-KIN3           PIC 9(09).
      *「公費請求書公４負担金：」置換金額
           03  RECE62-SEI-CHIKAN-KIN4           PIC 9(09).
      *
      *生保まとめ用（主保険単独分診区別点数）
           03  RECE62-SYUTANTEN-AREA.
               05  RECE62-SYUTANTEN             PIC 9(08)  OCCURS 30.
               05  RECE62-SYUTANTEN-NYUKHN      PIC 9(09)  OCCURS 20.
               05  RECE62-SYUTANTEN-NYUKHN2     PIC S9(09).
               05  RECE62-SYUTANTEN-NYUTOK      PIC 9(09)  OCCURS 20.
               05  RECE62-SYUTANTEN-NYUTOK2     PIC S9(09).
           03  RECE62-SYUTANARIFLG              PIC 9(01).
      *
      *標準負担額経過措置（精神）
      *（インデックスの情報は保険組合せ番号情報とリンク）
           03  RECE62-SEISIN-SKJFLG             PIC 9(01).
           03  RECE62-SEISIN-NYUINYMD           PIC X(08).
           03  RECE62-SEISIN-SKJSAN-AREA.
             04  RECE62-SEISIN-SKJSAN-OCC       OCCURS 10.
               05  RECE62-SEISIN-SKJSAN-OCC2    OCCURS 31.
                 06  RECE62-SEISIN-SKJSAN       PIC 9(01).
      *
      *再診加算（入院）情報
           03  RECE62-NYUSAIINF.
               05  RECE62-NYUSAI-TENSU          PIC 9(08)  OCCURS  5.
      *入院料日別診療区分情報
           03  RECE62-NYUDAYINF.
               05  RECE62-NYUDAY-SRYKBN         PIC X(02)  OCCURS 31.
      *
      *１９原爆一般特殊計算フラグ（患者７割負担）
      *（インデックスの情報は保険組合せ番号情報とリンク）
           03  RECE62-GENBAKUINF.
               05  RECE62-GENBAKU-TOKFLG        PIC 9(01).
               05  RECE62-GENBAKU-OCC           OCCURS 10.
                   07  RECE62-GENBAKU-FTNKIN    PIC 9(07)V9(01).
                   07  RECE62-GENBAKU-IDX       PIC 9(01).
      *
      *請求点数０円レセプト作成フラグ
           03  RECE62-SKYZEROFLG                PIC 9(01).
      *
      *入院料算定日テーブル情報（ＥＦ用）
           03  RECE62-EF-NYUINDAY-TBL.
               05  RECE62-EF-NYUINDAY-KBN       PIC X(01)  OCCURS 31.
      *
      *夜間看護体制特定日減算情報
           03  RECE62-KANGOGENINF.
               05  RECE62-KANGOGEN-SRYCD        PIC X(09)  OCCURS 31.
      *
      *妊婦フラグ
           03  RECE62-NINPUFLG                  PIC 9(01).
      *
      *検査フラグ
           03  RECE62-KENSA-BNPFLG              PIC 9(01).
           03  RECE62-KENSA-NTPROFLG            PIC 9(01).
           03  RECE62-KENSA-ANPFLG              PIC 9(01).
      *
      *診療区分別点数情報（点数合計）
           03  RECE62-ALLTENSU-AREA.
               05  RECE62-ALLTENSU              PIC 9(08)  OCCURS 30.
               05  RECE62-ALLTENSU-NYUKHN       PIC 9(09)  OCCURS 20.
               05  RECE62-ALLTENSU-NYUKHN2      PIC S9(09).
               05  RECE62-ALLTENSU-NYUTOK       PIC 9(09)  OCCURS 20.
               05  RECE62-ALLTENSU-NYUTOK2      PIC S9(09).
      *
      *公費一部負担金情報
      *（インデックスの情報は保険組合せ番号情報とリンク）
           03  RECE62-KOHFTNKININF-AREA.
               05  RECE62-KOHFTNKININF          OCCURS  10.
                   07  RECE62-KOHFTNKIN         PIC 9(09)  OCCURS  4.
      *
      *排尿自立支援加算の該当月最初の算定日
           03  RECE62-HAIJSKYMD                 PIC X(08).
      *
      *予備
           03  RECE62-YOBIETC-AREA.
               05  RECE62-YOBIETCINF            OCCURS  20.
                   07  RECE62-YOBIETC           PIC 9(09).
      *
