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
      *    レセプト明細書     COPY (CPRCF02.INC)        *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.01.01    MCC-藤原     02.02.01  補足区分の追加
      *  01.01.04    MCC-藤原     02.02.13  保険組合せを３から１０に変更
      *
      *  ２００２年４月改正対応
      *  01.02.01    MCC-小豆沢   02.04.22  レセプト摘要欄の印刷区分を追加
      *                                     再診料、外来管理加算について
      *  ２００２年１０月改正対応
      *  01.02.02    NACL-小豆沢  02.09.24  年齢項目の追加
      *  01.02.03    NACL-門脇    02.10.07  在宅総合・末期フラグの追加
      *                                     低所得フラグの追加
      *                                     一部負担金再掲の追加
      *
      *  01.02.04    NACL-門脇    03.02.06  コメントカウントフラグの追加
      *  01.02.05    NACL-藤原    03/02/12  テスト患者番号区分・補助区分の追加
      *  01.02.06    NACL-藤原    03/03/07  レセプト記載外公費情報追加
      *                                     診療科数１０から２０へ変更
      *  01.02.07    NACL-藤原    03.03.11  レセプト種別不明情報の追加
      *  01.02.08    NACL-門脇    03.04.16  船員保険（被保険者）＋通勤災害
      *                                     フラグの追加
      *  01.02.09    NACL-門脇    03.06.19  実日数テーブルの追加
      *  01.02.10    NACL-藤原    03.10.27  県番号の追加
      *  01.02.11    NACL-門脇    04.01.16  公費分離区分最小値の追加
      *  01.02.12    NACL-門脇    04.01.19  病床数編集フラグの追加
      *  01.02.13    NACL-門脇    04.01.20  自費レセ摘要欄編集レコードフラグ
      *                                     の追加
      *  01.02.14    NACL-門脇    04.03.17  福祉編集フラグの追加
      *                                     合計点数情報（ぶん点・公費分離
      *                                     区分編集用）の追加
      *  01.02.15    NACL-藤原    04.06.08  適用終了日の追加
      *  01.02.16    NACL-藤原    04.06.15  印刷順の追加（個別作成時のみ）
      *  01.02.17    NACL-藤原    04.07.06  組合せ公費保険ＩＤの追加
      *
      *  ver.2.5.0 
      *  01.03.01    NACL-門脇    04.12.15  公費一部負担金レセ記載フラグの追加
      *                                     公費一部負担金０円記載フラグの追加
      *  01.03.02    NACL-門脇    05.03.11  負担金額情報に項目追加
      *                                     レセプト記載外公費情報に項目追加
      *  01.03.03    NACL-藤原    05.03.14  レセプト特記事項編集情報の追加
      *
      *  ver.2.6.0 
      *  02.06.01    NACL-藤原    05.07.13  老人保険−支払区分
      *                                     本人−外来−内容
      *                                     保険組合せ公費フラグの追加
      *  02.06.02    NACL-藤原    05.07.28  レセプト請求しない公費の追加
      *  02.06.03    NACL-藤原    05.08.05  記載外公費−負担者番号の追加
      *
      *  02.07.01    NACL-藤原    05/08/20  特別療養費対応
      *                                     レセプト記載用レセプト種別の追加
      *
      *  02.08.01    NACL-藤原    06/02/20  主科対応
      *                                     主科追加
      *
      *  02.09.01    NACL-藤原    06/04/14  ２００６年４月改正対応
      *                                     外来迅速検体検査加算フラグ追加
      *  3.0.0
      *  03.00.01    NACL-藤原    06/05/15  入退院履歴記載対応
      *                                     入退院履歴記載フラグの追加
      *
      *  03.04.01    NACL-藤原    06/12/05  未請求レセ対応
      *  03.04.02    NACL-門脇    07/02/19  公費の一部負担金再掲追加
      *                                     公費の一部負担金再掲フラグ追加
      *                                     公費−本人−外来−回−上限額追加
      *  03.04.03    NACL-門脇    07/03/27  在宅総合・在宅末期一部負担金
      *                                     再掲２追加
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-門脇    07/08/01  所得情報取扱い機能強化対応
      *  03.05.03    NACL-門脇    07/08/01  月途中の所得情報変更対応
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
      *  04.04.02    NACL-藤原    09/03/25  生保まとめフラグ追加
      *
      *  04.05.01    NACL-門脇    10/02/08  全国公費併用分請求点集計追加
      *                                     福祉編集フラグにインデックス追加
      *
      *  04.05.02    NACL-藤原    10/04/14  平成２２年４月改正対応
      *                                     請求に係わる記録条件仕様の
      *                                     変更に伴う修正
      *                                     診療科２、３を追加
      *  04.05.03    NACL-藤原    11/02/22  レセ電データの記号番号区切り
      *                                     文字設定対応
      *  04.05.04    NACL-藤原    11/03/25  災害対応
      *                                     連絡先情報追加
      *  04.05.05    NACL-門脇    12/04/10  改正対応
      *                                     在医総算定フラグ
      *                                     （平成２４年４月診療〜）追加
      *
      *  04.06.01    NACL-門脇    10/07/02  小児特定疾患カウンセリング料
      *                                     （１回目）算定フラグ追加
      *  04.06.02    NACL-藤原    10/06/23  レセ電請求時の受付エラー分の
      *                                     種別不明レセ対応
      *                                     レセプト種別不明エラー情報変更
      *  04.06.03    NACL-門脇    10/12/13  請求額情報の追加
      *  04.06.04    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.05    NACL-藤原    11/12/12  点検レセ０点公費印字対応
      *  04.06.05    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *
      *  04.07.01    NACL-門脇    11/09/01  システム管理２０１０機能追加対応
      *                                     一部負担金金額集計フラグ追加
      *                                     保険欄−一部負担金記載
      *                                     （高齢者）（70歳未満）フラグ追加
      *  04.07.02    NACL-藤原    12/01/06  予備を２０に変更
      *  04.07.03    NACL-門脇    12/08/27  システム管理２０１０機能追加対応
      *  04.07.04    NACL-門脇    12/10/02  高齢者判定フラグ追加
      *  04.07.05    NACL-門脇    13/07/10  自賠責（第三者行為）対応
      *  04.07.06    NACL-門脇    14/01/08  請求点数がない保険組み合わせ有り
      *                                     フラグ追加
      *  04.07.07    NACL-門脇    14/07/31  第三者行為に関するフラグ追加
      *  04.07.08    NACL-門脇    15/04/27  特記事項０９施がある場合の
      *                                     【配】回数記載対応
      *  04.08.01    NACL-門脇    15/10/08  システム管理２０１０機能追加対応
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
      *  04.08.09    NACL-門脇    18/04/06  平成３０年４月改正対応
      *  04.08.10    NACL-門脇    18/05/22  平成３０年４月改正対応
      *  04.08.11    NACL-門脇    18/06/11  逓減点数集計のサイン対応
      *
      *  05.00.01    NACL-門脇    19/03/13  システム管理２０１０機能追加対応
      *  05.00.02    NACL-藤原    19/04/04  印刷順の桁数変更
      *  05.00.03    NACL-門脇    19/06/04  生保に関わる保険組み合わせ作成
      *                                     対応（主保険なしの場合）
      *  05.00.04    NACL-門脇    19/07/01  リハビリ発症日レセプト記載対応
      *                                     （機能追加）
      *  05.00.05    NACL-門脇    19/10/01  括弧再掲記載に関わるフラグ追加
      *  05.00.06    NACL-門脇    19/11/15  マル長レセプト記載処理区分追加
      *  05.00.07    NACL-門脇    20/04/07  令和２年４月改正対応
      ************************************************************
      *01  RECE02-REC.
           03  RECE02-KEY.
      *医療機関ＩＤ
               05  RECE02-HOSPNUM         PIC 9(2).
      *診療年月
               05  RECE02-SRYYM           PIC  9(06).
      *入外区分
               05  RECE02-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  RECE02-PTID            PIC  9(10).
      *レセプト科
               05  RECE02-RECEKA          PIC  X(02).
      *審査支払機関
               05  RECE02-TEISYUTUSAKI    PIC  9(01).
      *レセプト種別
               05  RECE02-RECESYUBETU     PIC  9(04).
      *保険者番号        
               05  RECE02-HKNJANUM-KEY    PIC  X(08).
      *補助区分
               05  RECE02-HOJOKBN-KEY     PIC  X(01).
      *公費ＩＤ
               05  RECE02-KOHID-KEY       PIC  9(10).
      *適用開始年月日
               05  RECE02-TEKSTYMD        PIC  X(08). 
      *分割区分
               05  RECE02-RECEIPT-KBN     PIC  X(10).
      *患者番号
           03  RECE02-PTNUM               PIC  X(20).
      *診療科情報
           03  RECE02-SRYKAINF            OCCURS  20.
      *診療科
               05  RECE02-SRYKA           PIC  X(02).
      *カナ氏名
           03  RECE02-KANANAME            PIC  X(50).
      *漢字氏名
           03  RECE02-NAME                PIC  X(100).
      *性別
           03  RECE02-SEX                 PIC  X(01).
      *生年月日
           03  RECE02-BIRTHDAY            PIC  9(08).
      *年齢
           03  RECE02-AGE                 PIC  9(03).
      *就学前区分
           03  RECE02-SYUGAKUKBN          PIC 9(01).
      *死亡区分
           03  RECE02-DEATHKBN            PIC  X(01).
      *自宅−郵便番号
           03  RECE02-HOME-POST           PIC X(07).
      *自宅−住所
           03  RECE02-HOME-ADRS           PIC X(200).
      *自宅−番地方書
           03  RECE02-HOME-BANTI          PIC X(200).
      *連絡先−名称
           03  RECE02-RENRAKU-NAME.
               05  RECE02-RENRAKU-NAME-R  PIC X(50)
                                              OCCURS  2.
      *連絡先−郵便番号
           03  RECE02-RENRAKU-POST        PIC X(07).
      *連絡先−住所
           03  RECE02-RENRAKU-ADRS        PIC X(200).
      *連絡先−番地方書
           03  RECE02-RENRAKU-BANTI       PIC X(200).
      *連絡先−昼電話番号
           03  RECE02-RENRAKU-TEL1        PIC X(15).
      *連絡先−夜電話番号
           03  RECE02-RENRAKU-TEL2        PIC X(15).
      *自宅−編集郵便番号
           03  RECE02-HOME-POST1          PIC X(14).
      *自宅−編集住所（住所＋番地）
           03  RECE02-HOME-ADRS1.
               05  RECE02-HOME-ADRS-R     PIC X(50)
                                              OCCURS  4.
      *連絡先−編集郵便番号
           03  RECE02-RENRAKU-POST1       PIC X(14).
      *連絡先−編集住所（住所＋番地）
           03  RECE02-RENRAKU-ADRS1.
               05  RECE02-RENRAKU-ADRS-R  PIC X(50)
                                              OCCURS  4.
      *連絡先−編集電話番号（昼電話番号全角）
           03  RECE02-TEL                 PIC X(30).
      *レセプト対象年月日情報
           03  RECE02-TEKYMD.
      *適用開始年月日
               05  RECE02-TEKSTYMD-X      PIC  X(08). 
      *適用終了年月日
               05  RECE02-TEKEDYMD-X      PIC  X(08). 
      *記載用レセプト種別（特別療養費）
           03  RECE02-RECESYUBETU-X       PIC  X(04).
      *記載用レセプト種別（紙レセプト）
           03  RECE02-RECESYUBETU-Y       PIC  X(04).
      *主科情報
           03  RECE02-SYUKA-AREA.
      *主科１
               05  RECE02-SYUKA           PIC  X(02).
      *主科１（レセ電算診療科）
               05  RECE02-RECESYUKA       PIC  X(02).
      *主科１診療科名
               05  RECE02-SYUKANAME       PIC  X(80).
      *主科２
               05  RECE02-SYUKA2          PIC  X(02).
      *主科２（レセ電算診療科）
               05  RECE02-RECESYUKA2      PIC  X(02).
      *主科３
               05  RECE02-SYUKA3          PIC  X(02).
      *主科３（レセ電算診療科）
               05  RECE02-RECESYUKA3      PIC  X(02).
      *医師別出力情報
           03  RECE02-DRCD-AREA.
      *医師コード
               05  RECE02-DRCD            PIC  X(05).
      *医師別診療科
               05  RECE02-DRCD-SRYKA      PIC  X(02).
      *
      *主保険情報
           03  RECE02-HKN.
      *主保険−保険ＩＤ
               05  RECE02-HKNID           PIC  9(10).
      *主保険−保険番号
               05  RECE02-HKNNUM          PIC  X(03).
      *主保険−法別番号
               05  RECE02-HKNHBTNUM       PIC  X(02).
      *主保険−保険者番号
               05  RECE02-HKNJANUM        PIC  X(08).
      *主保険−本人家族区分
               05  RECE02-HONKZKKBN       PIC  X(01).
      *主保険−補助区分
               05  RECE02-HOJOKBN         PIC  X(01).
      *主保険−継続区分
               05  RECE02-CONTKBN         PIC  X(01).
      *主保険−記号
               05  RECE02-KIGO            PIC  X(80).
      *主保険−番号
               05  RECE02-NUM             PIC  X(80).
      *主保険−給付割合
               05  RECE02-KYURATE         PIC  9(03).
      *主保険−証明書番号
               05  RECE02-CERTNUM         PIC  X(03).
      *老人保健情報
           03  RECE02-RJNHKN.
      *老人保険−保険ＩＤ
               05  RECE02-RJNHKNID        PIC  9(10).
      *老人保険−保険番号
               05  RECE02-RJNHKNNUM       PIC  X(03).
      *老人保険−法別番号
               05  RECE02-RJNHBTNUM       PIC  X(02).
      *老人保険−市町村番号
               05  RECE02-RJNFTNJANUM     PIC  X(08).
      *老人保険−受給者番号
               05  RECE02-RJNJKYSNUM      PIC  X(07).
      *老人保険−支払区分
               05  RECE02-RJNPAYKBN       PIC  X(02).
      *
      *公費情報
           03  RECE02-KOHINF-AREA.
               05  RECE02-KOHINF          OCCURS  4.
      *公費−保険ＩＤ
                   07  RECE02-KOHID       PIC  9(10).
      *公費−保険番号
                   07  RECE02-KOHNUM      PIC  X(03).
      *公費−法別番号
                   07  RECE02-KOHHBTNUM   PIC  X(02).
      *公費−負担者番号
                   07  RECE02-KOHFTNJANUM PIC  X(08).
      *公費−受給者番号
                   07  RECE02-KOHJKYSNUM  PIC  X(20).
      *公費−任意給付区分
                   07  RECE02-KOHNINIKYUKBN
                                          PIC  X(01).
      *公費−件数
                   07  RECE02-KOHKENSU    PIC  9(03).
      *公費−レセプト請求区分
                   07  RECE02-RECESKYKBN  PIC  X(01).
      *公費−適用開始年月日
                   07  RECE02-KOHTEKSTYMD PIC  X(08).
      *公費−適用終了年月日
                   07  RECE02-KOHTEKEDYMD PIC  X(08).
      *
      *公費−本人−外来−負担区分
                   07  RECE02-HON-GAIFTNKBN      PIC  X(01).
      *公費−本人−外来−回−負担割合
                   07  RECE02-HON-GAIKFTNRATE    PIC  9(03).
      *公費−本人−外来−回−上限額
                   07  RECE02-HON-GAIKJGNGAK     PIC  9(06).
      *公費−本人−外来−日−上限額
                   07  RECE02-HON-GAIDJGNGAK     PIC  9(06).
      *公費−本人−外来−日−上限回数
                   07  RECE02-HON-GAIDJGNCNT     PIC  9(02).
      *公費−本人−外来−月−院内−上限額
                   07  RECE02-HON-GAIMNAIJGNGAK  PIC  9(06).
      *公費−本人−外来−月−院外−上限額
                   07  RECE02-HON-GAIMGAIJGNGAK  PIC  9(06).
      *公費−本人−外来−月−上限回数
                   07  RECE02-HON-GAIMJGNCNT     PIC  9(02).
      *
      *公費−低２−外来−負担区分
                   07  RECE02-TSY-GAIFTNKBN      PIC  X(01).
      *公費−低２−外来−回−負担割合
                   07  RECE02-TSY-GAIKFTNRATE    PIC  9(03).
      *公費−低２−外来−回−上限額
                   07  RECE02-TSY-GAIKJGNGAK     PIC  9(06).
      *公費−低２−外来−日−上限額
                   07  RECE02-TSY-GAIDJGNGAK     PIC  9(06).
      *公費−低２−外来−日−上限回数
                   07  RECE02-TSY-GAIDJGNCNT     PIC  9(02).
      *公費−低２−外来−月−院内−上限額
                   07  RECE02-TSY-GAIMNAIJGNGAK  PIC  9(06).
      *公費−低２−外来−月−院外−上限額
                   07  RECE02-TSY-GAIMGAIJGNGAK  PIC  9(06).
      *公費−低２−外来−月−上限回数
                   07  RECE02-TSY-GAIMJGNCNT     PIC  9(02).
      *
      *公費−低１−外来−負担区分
                   07  RECE02-TNK-GAIFTNKBN      PIC  X(01).
      *公費−低１−外来−回−負担割合
                   07  RECE02-TNK-GAIKFTNRATE    PIC  9(03).
      *公費−低１−外来−回−上限額
                   07  RECE02-TNK-GAIKJGNGAK     PIC  9(06).
      *公費−低１−外来−日−上限額
                   07  RECE02-TNK-GAIDJGNGAK     PIC  9(06).
      *公費−低１−外来−日−上限回数
                   07  RECE02-TNK-GAIDJGNCNT     PIC  9(02).
      *公費−低１−外来−月−院内−上限額
                   07  RECE02-TNK-GAIMNAIJGNGAK  PIC  9(06).
      *公費−低１−外来−月−院外−上限額
                   07  RECE02-TNK-GAIMGAIJGNGAK  PIC  9(06).
      *公費−低１−外来−月−上限回数
                   07  RECE02-TNK-GAIMJGNCNT     PIC  9(02).
      *
      *保険組合せ番号情報
           03  RECE02-HKNCOMBIINF-AREA.
               05  RECE02-HKNCOMBIINF     OCCURS  10.
      *保険組合せ番号
                   07  RECE02-HKNCOMBI    PIC  9(04).
      *主保険番号
                   07  RECE02-HKNNUMHC    PIC  X(03).
      *組合せ公費保険番号情報
                   07  RECE02-KOHNUMHCINF OCCURS  4.
      *組合せ公費保険番号
                       09  RECE02-KOHNUMHC
                                          PIC  X(03).
      *組合せ公費保険ＩＤ
                       09  RECE02-KOHIDHC PIC  9(10).
      *全国公費併用分請求点集計区分
                       09  RECE02-SYUKEIKBN
                                          PIC  X(01).
      *全国公費併用分請求点集計対象区分
                       09  RECE02-SYUKEI-TAIKBN
                                          PIC  X(01).
      *レセ電用負担区分編集フラグ
                       09  RECE02-FTNKBNFLG
                                          PIC  9(01).
      *保険組合せ公費フラグ（１：全国公費あり）
                   07  RECE02-HKNCOMBIFLG PIC  9(01).
      *
      *特記事項情報
           03  RECE02-TOKKIINF            OCCURS  5.
      *特記事項
               05  RECE02-TOKKI           PIC  X(02).
      *
      *診療区分別点数情報
           03  RECE02-SRYKBNINF           OCCURS  50.
      *基本点数
               05  RECE02-KHNTEN          PIC  9(08).
      *回数
               05  RECE02-KAISU           PIC  9(05).
      *明細点数情報
               05  RECE02-TENSUINF-AREA.
                   07  RECE02-TENSUINF    OCCURS  5.
      *明細点数
                       09  RECE02-TENSU   PIC S9(08).
      *
      *実日数情報
           03  RECE02-JNISSUINF-AREA.
               05  RECE02-JNISSUINF       OCCURS  5.
      *実日数
                   07  RECE02-JNISSU      PIC  9(02).
      *
      *合計点数情報
           03  RECE02-TOTALTENINF-AREA.
               05  RECE02-TOTALTENINF     OCCURS  5.
      *合計点数
                   07  RECE02-TOTALTEN    PIC S9(08).
      *
      *負担金額情報
           03  RECE02-FTNMONEYINF-AREA.
               05  RECE02-FTNMONEYINF     OCCURS  5.
      *負担金額
                   07  RECE02-FTNMONEY    PIC  9(09).
      *負担金額（一部負担額）
                   07  RECE02-FTNMONEY-FTN    PIC  9(09).
      *負担金額（控除額）
                   07  RECE02-FTNMONEY-KOJ    PIC  9(09).
      *
      *老人減免区分（高齢者経過措置フラグとして使用）
           03  RECE02-RJNGENMENKBN        PIC  X(01).
      *老人外来金額（請求点数合計として使用）
           03  RECE02-RJNGAIFTN           PIC  9(09).
      *老人入院金額（老人減免一部負担金記載フラグとして使用）
           03  RECE02-RJNNYUFTN           PIC  9(09).
      *減免区分
           03  RECE02-GENMENKBN           PIC  9(01).
      *減額割合
           03  RECE02-GENGAKURATE         PIC  9(03).
      *減額金額
           03  RECE02-GENGAKU             PIC  9(07).
      *
      *割引点数単価（７０歳未満現物給付の所得区分として使用）
           03  RECE02-DISCOUNTTENTNK      PIC  X(02).
      *食事療養費情報
           03  RECE02-SHOKUJIINF          OCCURS  5.
      *食事日数
               05  RECE02-SHOKUJINISSU    PIC  9(02).
      *食事療養費
               05  RECE02-SHOKUJIRYOYOHI  PIC  9(09).
      *標準負担額
               05  RECE02-SHOKUJIFTN      PIC  9(09).
      *
      *一部負担金・食事療養費標準負担額区分
           03  RECE02-SHOKUJIFTNKBN       PIC  X(01).
      *一部負担金・食事療養費標準負担額区分２（地方公費判定用）
           03  RECE02-SHOKUJIFTNKBN2-INF.
               05  RECE02-SHOKUJIFTNKBN2  PIC  X(01)
                                          OCCURS  4.
      *
      *老人公費負担金額情報
           03  RECE02-RJNKOHFTNINF        OCCURS  3.
      *老人外来公費負担額（（１）（２）（３）使用）
               05  RECE02-RJNGAIKOHFTN    PIC  9(09).
      *老人入院公費負担額（（１）（２）（３）使用）
               05  RECE02-RJNNYUKOHFTN    PIC  9(09).
      *
      *薬剤一部負担金情報
           03  RECE02-YKZFTNINF-AREA.
               05  RECE02-YKZFTNINF       OCCURS  5.
      *薬剤一部負担金
                   07  RECE02-YKZFTN      PIC  9(05).
      *レセプトＯＣＲ１
           03  RECE02-OCR1                PIC  X(67).
      *レセプトＯＣＲ２
           03  RECE02-OCR2                PIC  X(67).
      *レセプトＯＣＲ３
           03  RECE02-OCR3                PIC  X(67).
      *
      *レセプト記載外公費情報
           03  RECE02-ETC-KOH-G.
               05  RECE02-ETC-KOHINF      OCCURS  4.
      *記載外公費−保険番号
                   07  RECE02-ETC-KOHNUM  PIC  X(03).
      *記載外公費−負担者番号
                   07  RECE02-ETC-KOHFTNJANUM
                                          PIC  X(08).
      *記載外公費−実日数
                   07  RECE02-ETC-JNISSU  PIC  9(02).
      *記載外公費−合計点数
                   07  RECE02-ETC-TOTALTEN
                                          PIC  9(08).
      *記載外公費−負担金額
                   07  RECE02-ETC-FTNMONEY
                                          PIC  9(09).
      *記載外公費−負担金額（一部負担額）
                   07  RECE02-ETC-FTNMONEY-FTN
                                          PIC  9(09).
      *記載外公費−負担金額（控除額）
                   07  RECE02-ETC-FTNMONEY-KOJ
                                          PIC  9(09).
      *記載外公費−薬剤一部負担金情報
                   07  RECE02-ETC-YKZFTN  PIC  9(05).
      *
      *レセプト請求しない公費領域
           03 RECE02-KOHSKY-KOHI-AREA.
               05  RECE02-KOHSKY-KOHI-TBL OCCURS  4.
      *    公費ＩＤ
                   07  RECE02-KOHSKY-KOHID
                                          PIC  9(10).
      *    公費保険番号
                   07  RECE02-KOHSKY-KOHNUM
                                          PIC  X(03).
      *    公費負担者番号
                   07  RECE02-KOHSKY-KOHFTNJANUM
                                          PIC  X(08).
      *
      *初診時間区分
           03  RECE02-SHOSHINTIME         PIC  X(01).
      *再診摘要印刷区分
           03  RECE02-SAISHIN-PRTKBN      PIC  X(01).
      *外来管理加算摘要印刷区分
           03  RECE02-GAIRAIKSN-PRTKBN    PIC  X(01).
      *低所得フラグ
           03  RECE02-TEISYOTOKU          PIC  X(01).
      *低所得フラグ２（地方公費判定用）
           03  RECE02-TEISYOTOKU2-INF.
               05  RECE02-TEISYOTOKU2     PIC  X(01)
                                          OCCURS  4.
      *在宅総合・在宅末期フラグ
           03  RECE02-ZAITAKU             PIC  X(01).
      *在宅総合・在宅末期一部負担金再掲
           03  RECE02-ZAITAKU-SAIKEI      PIC  9(09).
      *在宅総合・在宅末期一部負担金再掲２
           03  RECE02-ZAITAKU-SAIKEI2     PIC  9(09).
      *再掲３
           03  RECE02-ZAITAKU-SAIKEI3     PIC  9(09).
      *再掲４
           03  RECE02-ZAITAKU-SAIKEI4     PIC  9(09).
      *公費の一部負担金再掲
           03  RECE02-KOH-SAIKEI          PIC  9(09).
      *公費の一部負担金再掲フラグ
           03  RECE02-KOH-SAIKEIFLG       PIC  9(01).
      *船員保険（被保険者）＋通勤災害フラグ
           03  RECE02-SENIN-TUUSAI        PIC  X(01).
      *コメントカウントフラグ
           03  RECE02-COMCNTFLG           PIC  X(01).
      *テスト患者番号区分
           03  RECE02-TSTPTNUMKBN         PIC  X(01).
      *レセプト種別不明エラー情報
           03  RECE02-ERR-AREA.
               05  RECE02-ERR-ERRKBN      PIC 9(02).
               05  RECE02-ERR-DAY         PIC 9(02).
               05  RECE02-ERR-HKNCOMBINUM PIC 9(04).
               05  RECE02-ERR-TEKSTYMD    PIC 9(08).
               05  RECE02-ERR-TEKEDYMD    PIC 9(08).
      *
      *実日数テーブル情報
           03  RECE02-JNISSUTBLINF        OCCURS 31.
      *実日数テーブル
               05  RECE02-JNISSUTBL       PIC  X(01).
      *乳幼児・幼児のコメントフラグ
           03  RECE02-NYUYOJIFLG          PIC  X(01).
      *小児特定疾患カウンセリング料（１回目）算定フラグ
           03  RECE02-SYOUNIFLG           PIC  X(01).
      *乳腺炎重症化予防ケア・指導料（２回目から
      *４回目まで）算定フラグ
           03  RECE02-NYUSENFLG           PIC  X(01).
      *
      *公費実日数テーブル情報
           03  RECE02-KOHJNISSUTBLINF.
      *公費実日数テーブル
               05  RECE02-KOHJNISSUTBL    PIC  X(01)
                                          OCCURS 31.
      *県番号
           03  RECE02-PREFNUM             PIC  9(02).
      *県内・県外区分（１：県内　２：県外　）
           03  RECE02-PREFKBN             PIC  X(01).
      *県番号（保険者）
           03  RECE02-HKNJANUM-PREFNUM    PIC  X(02).
      *公費分離区分最小値
           03  RECE02-KOHBUNRI-MIN        PIC  9(01).
      *公費分離区分最大値
           03  RECE02-KOHBUNRI-MAX        PIC  9(01).
      *病床数編集フラグ（特定疾患療養指導料等）
           03  RECE02-BEDSUFLG            PIC  X(01).
      *自費レセ摘要欄編集レコードフラグ
           03  RECE02-JIHTEKFLG           PIC  X(01).
      *外来迅速検体検査加算フラグ
           03  RECE02-KENTAIKASANFLG      PIC  X(01).
      *福祉編集フラグ
           03  RECE02-FUKUSHIFLGINF-AREA.
               05  RECE02-FUKUSHIFLG      PIC  X(01)   OCCURS 5.
      *入退院履歴記載フラグ
           03  RECE02-NYUINRRK-FLG        PIC  X(01).
      *生保まとめフラグ
           03  RECE02-SEIHOFLG            PIC  X(01).
      *合計点数情報（ぶん点・公費分離区分編集用）
           03  RECE02-HENTOTALTENINF-AREA.
               05  RECE02-HENTOTALTENINF  OCCURS  5.
      *合計点数（ぶん点・公費分離区分編集用）
                   09  RECE02-HENTOTALTEN PIC S9(08).
      *印刷順
           03  RECE02-PRTSEQ              PIC  9(04).
      *
      *その他記載フラグ
           03  RECE02-ETCKISAIFLGINF-AREA.
               05  RECE02-ETCKISAIFLGINF  OCCURS  5.
                   09  RECE02-KISAIFLG         PIC  X(01).
                   09  RECE02-ZEROKISAIFLG     PIC  X(01).
      *
                   09  RECE02-EKF-JNISSUFLG    PIC  X(01).
                   09  RECE02-EKF-TOTALTENFLG  PIC  X(01).
                   09  RECE02-EKF-OCRFLG       PIC  X(01).
                   09  RECE02-EKF-FTNKEIFLG    PIC  X(01).
                   09  RECE02-EKF-HKNFTNFLG-K  PIC  X(01).
                   09  RECE02-EKF-HKNFTNFLG-K2 PIC  X(01).
                   09  RECE02-EKF-HKNFTNFLG-Y  PIC  X(01).
                   09  RECE02-EKF-HKNFTNFLG-Y2 PIC  X(01).
      *
                   09  RECE02-EKF-KAKKBN-K     PIC  X(01).
                   09  RECE02-EKF-KAK21KBN-K   PIC  X(01).
                   09  RECE02-EKF-KAK22KBN-K   PIC  X(01).
                   09  RECE02-EKF-KAK23KBN-K   PIC  X(01).
                   09  RECE02-EKF-KAK31KBN-K   PIC  X(01).
                   09  RECE02-EKF-KAK32KBN-K   PIC  X(01).
                   09  RECE02-EKF-KAK41KBN-K   PIC  X(01).
                   09  RECE02-EKF-KAK42KBN-K   PIC  X(01).
                   09  RECE02-EKF-KAK43KBN-K   PIC  X(01).
                   09  RECE02-EKF-KAKKBN-Y     PIC  X(01).
                   09  RECE02-EKF-KAK21KBN-Y   PIC  X(01).
                   09  RECE02-EKF-KAK22KBN-Y   PIC  X(01).
                   09  RECE02-EKF-KAK23KBN-Y   PIC  X(01).
                   09  RECE02-EKF-KAK31KBN-Y   PIC  X(01).
                   09  RECE02-EKF-KAK32KBN-Y   PIC  X(01).
                   09  RECE02-EKF-KAK41KBN-Y   PIC  X(01).
                   09  RECE02-EKF-KAK42KBN-Y   PIC  X(01).
                   09  RECE02-EKF-KAK43KBN-Y   PIC  X(01).
                   09  RECE02-EKF-KSINGKBN-K   PIC  X(01).
                   09  RECE02-EKF-KSINGKBN-Y   PIC  X(01).
                   09  RECE02-EKF-ENCHGKBN-K   PIC  X(01).
                   09  RECE02-EKF-ENCHGKBN-Y   PIC  X(01).
                   09  RECE02-EKF-FTNHASUKBN   PIC  X(01).
      *
                   09  RECE02-EKF-SAIKEIKSI    PIC  X(01).
                   09  RECE02-EKF-KINGAKKSN    PIC  X(01).
      *
                   09  RECE02-GNBKEIKBN-K-G    PIC  X(01).
      *
                   09  RECE02-MJK-ZEROKISAIFLG    PIC X(01).
                   09  RECE02-MJK-JNISSUFLG       PIC X(01).
                   09  RECE02-MJK-TOTALTENFLG     PIC X(01).
                   09  RECE02-MJK-KISAIFLG        PIC X(01).
                   09  RECE02-MJK-OCRFLG          PIC X(01).
      *
      *法定外現物給付フラグ
           03  RECE02-HOTEIGAIFLGINF-AREA.
               05  RECE02-HOTEIGAIFLGINF  OCCURS  5.
                   09  RECE02-HOTEIGAIFLG         PIC X(01).
      *
      *マル長レセプト記載処理区分
           03  RECE02-CYOKI-KSIKBN        PIC  X(01).
      *
      *実日数・請求点記載フラグ
           03  RECE02-JSKISAIFLG          PIC  X(01).
      *未請求フラグ
           03  RECE02-MISEIKYU-FLG        PIC  X(02).
      *レセプト特記事項編集情報
           03  RECE02-2006-AREA.
      *        特記事項
               05  RECE02-2006-TOKKIINF.
                   07  RECE02-2006-TOKKI  PIC  X(12)
                                               OCCURS  4.
      *        給付割合
               05  RECE02-2006-KYURATE    PIC  9(03).
      *        レセ電：特記事項
               05  RECE02-2006-RECEDEN-TOKKIINF.
                   07  RECE02-2006-RECEDEN-TOKKI
                                          PIC  X(02)
                                               OCCURS  5.
      *        レセ電：給付割合
               05  RECE02-2006-RECEDEN-KYURATE
                                          PIC  9(03).
      *        公費情報
               05  RECE02-2006-KOHINF.
                   07  RECE02-2006-KOH-OCC     OCCURS  4. 
      *                負担者番号記載区分
                       09  RECE02-2006-FTNJANUMKBN
                                          PIC  X(02).
      *                負担者番号
                       09  RECE02-2006-FTNJANUM
                                          PIC  X(08).
      *                受給者番号記載区分
                       09  RECE02-2006-JKYSNUMKBN
                                          PIC  X(02).
      *                受給者番号
                       09  RECE02-2006-JKYSNUM
                                          PIC  X(07).
      *                レセ電：公費レコード区分
                       09  RECE02-2006-RECEDEN-KORECKBN
                                          PIC  X(01).
      *                レセ電：負担者番号記載区分
                       09  RECE02-2006-RECEDEN-FTNJANUMKBN
                                          PIC  X(02).
      *                レセ電：負担者番号
                       09  RECE02-2006-RECEDEN-FTNJANUM
                                          PIC  X(08).
      *                レセ電：受給者番号記載区分
                       09  RECE02-2006-RECEDEN-JKYSNUMKBN
                                          PIC  X(02).
      *                レセ電：受給者番号
                       09  RECE02-2006-RECEDEN-JKYSNUM
                                          PIC  X(07).
      *        適用欄コメント情報
               05  RECE02-2006-TEKIYOU-COMMENTINF.
                   07  RECE02-2006-TEKIYOU-COMMENT-OCC OCCURS  5. 
      *                適用欄上段コメント
                       09  RECE02-2006-TEKIYOU-COMMENT1
                                          PIC  X(52).
      *                適用欄下段コメント
                       09  RECE02-2006-TEKIYOU-COMMENT2
                                          PIC  X(52).
      *                適用欄上段コメントレセ電編集区分
                       09  RECE02-2006-TEKIYOU-COMMENT1KBN
                                          PIC  X(01).
      *                適用欄下段コメントレセ電編集区分
                       09  RECE02-2006-TEKIYOU-COMMENT2KBN
                                          PIC  X(01).
      *
      *患者特記事項情報
           03  RECE02-PTTOKKI-AREA.
               05  RECE02-PTTOKKINF           OCCURS  5.
                   07  RECE02-PTTOKKI-NAIYO
                                          PIC  X(12).
                   07  RECE02-PTTOKKI-CD  PIC  X(02).
      *
      *請求額情報（計算用）
           03  RECE02-FTNMONEY-SKY             PIC 9(09).
      *
      *請求額情報（医療費）
           03  RECE02-SKYIRYO-INF.
      *実日数
               05  RECE02-SKYIRYO-NISSU        PIC 9(02).
      *請求点
               05  RECE02-SKYIRYO-TOTALTEN     PIC 9(08).
      *患者負担
               05  RECE02-SKYIRYO-ICHIBUFTN    PIC 9(09).
      *保険者負担
               05  RECE02-SKYIRYO-HOKENJA      PIC 9(09).
      *公費負担
               05  RECE02-SKYIRYO-KOHFTN       PIC 9(09).
      *
      *請求額情報（公費負担のうちレセプト請求しない地方公費分）
           03  RECE02-SKYIRYO-KOHFTN-KOHSKY    PIC 9(09).
      *
      *点検レセ０点公費フラグ
           03  RECE02-TENKEN-FLG               PIC X(01).
      *
      *在医総算定フラグ（平成２４年４月診療〜）
           03  RECE02-ZAITAKU-SANTEI-FLG       PIC X(01).
      *
      *高齢者判定フラグ
           03  RECE02-KOUREI-HANTEIFLG         PIC X(01).
      *
      *第三者行為情報（インデックスの情報は保険組合せ番号情報とリンク）
           03  RECE02-DAISANINF-AREA.
             04  RECE02-DAISANINF               OCCURS 10.
      *第三者行為保険組合せフラグ
               05  RECE02-DAISANFLG             PIC 9(01).
      *第三者行為公費ＩＤ
               05  RECE02-DAISAN-KOHID          PIC 9(10).
      *第三者行為保険組合せ現物給付フラグ
               05  RECE02-DAISAN-GENBUTUFLG     PIC 9(01).
      *第三者行為請求点数あり
               05  RECE02-DAISAN-SKYFLG         PIC 9(01).
      *
      *リハビリ情報（インデックスの情報は保険組合せ番号情報とリンク）
           03  RECE02-RIHAINF-AREA.
             04  RECE02-RIHAINF                 OCCURS 10.
      *リハビリ開始日編集用公費分離区分
               05  RECE02-RIHA-KOHBUNRI         PIC 9(01).
      *リハビリ算定テーブル
               05  RECE02-RIHA-DAY-AREA.
                 06  RECE02-RIHA-DAY-OCC        OCCURS 31.
                   07  RECE02-RIHA-SINDAI       PIC 9(01).
                   07  RECE02-RIHA-KOKYUKI      PIC 9(01).
                   07  RECE02-RIHA-NOUKE        PIC 9(01).
                   07  RECE02-RIHA-UNDOKI       PIC 9(01).
                   07  RECE02-RIHA-NANBYO       PIC 9(01).
                   07  RECE02-RIHA-SYOGAI       PIC 9(01).
                   07  RECE02-RIHA-SESSYOKU     PIC 9(01).
                   07  RECE02-RIHA-HAIYO        PIC 9(01).
      *リハビリ加算算定テーブル
               05  RECE02-RIHA-DAY9-AREA.
                 06  RECE02-RIHA-DAY9-OCC       OCCURS 31.
                   07  RECE02-RIHA-SINDAI9      PIC 9(01).
                   07  RECE02-RIHA-KOKYUKI9     PIC 9(01).
                   07  RECE02-RIHA-HAIYO9       PIC 9(01).
      *リハビリ算定テーブル２
               05  RECE02-RIHA2-DAY-AREA.
                   07  RECE02-RIHA2-SINDAI      PIC 9(01).
                   07  RECE02-RIHA2-SINYMD-OCC1.
                     08  RECE02-RIHA2-SINYMD1   PIC X(08)  OCCURS  2.
                   07  RECE02-RIHA2-SINYMD-OCC2.
                     08  RECE02-RIHA2-SINYMD2   PIC 9(01)  OCCURS 31.
      *
                   07  RECE02-RIHA2-KOKYUKI     PIC 9(01).
                   07  RECE02-RIHA2-KOKYMD-OCC1.
                     08  RECE02-RIHA2-KOKYMD1   PIC X(08)  OCCURS  2.
                   07  RECE02-RIHA2-KOKYMD-OCC2.
                     08  RECE02-RIHA2-KOKYMD2   PIC 9(01)  OCCURS 31.
      *
                   07  RECE02-RIHA2-NOUKE       PIC 9(01).
                   07  RECE02-RIHA2-NOUYMD-OCC1.
                     08  RECE02-RIHA2-NOUYMD1   PIC X(08)  OCCURS  2.
                   07  RECE02-RIHA2-NOUYMD-OCC2.
                     08  RECE02-RIHA2-NOUYMD2   PIC 9(01)  OCCURS 31.
      *
                   07  RECE02-RIHA2-UNDOKI      PIC 9(01).
                   07  RECE02-RIHA2-UNDYMD-OCC1.
                     08  RECE02-RIHA2-UNDYMD1   PIC X(08)  OCCURS  2.
                   07  RECE02-RIHA2-UNDYMD-OCC2.
                     08  RECE02-RIHA2-UNDYMD2   PIC 9(01)  OCCURS 31.
      *
                   07  RECE02-RIHA2-NANBYO      PIC 9(01).
                   07  RECE02-RIHA2-NANYMD-OCC1.
                     08  RECE02-RIHA2-NANYMD1   PIC X(08)  OCCURS  2.
                   07  RECE02-RIHA2-NANYMD-OCC2.
                     08  RECE02-RIHA2-NANYMD2   PIC 9(01)  OCCURS 31.
      *
                   07  RECE02-RIHA2-SYOGAI      PIC 9(01).
                   07  RECE02-RIHA2-SYOYMD-OCC1.
                     08  RECE02-RIHA2-SYOYMD1   PIC X(08)  OCCURS  2.
                   07  RECE02-RIHA2-SYOYMD-OCC2.
                     08  RECE02-RIHA2-SYOYMD2   PIC 9(01)  OCCURS 31.
      *
                   07  RECE02-RIHA2-SESSYOKU    PIC 9(01).
                   07  RECE02-RIHA2-SESYMD-OCC1.
                     08  RECE02-RIHA2-SESYMD1   PIC X(08)  OCCURS  2.
                   07  RECE02-RIHA2-SESYMD-OCC2.
                     08  RECE02-RIHA2-SESYMD2   PIC 9(01)  OCCURS 31.
      *
                   07  RECE02-RIHA2-HAIYO       PIC 9(01).
                   07  RECE02-RIHA2-HAIYMD-OCC1.
                     08  RECE02-RIHA2-HAIYMD1   PIC X(08)  OCCURS  2.
                   07  RECE02-RIHA2-HAIYMD-OCC2.
                     08  RECE02-RIHA2-HAIYMD2   PIC 9(01)  OCCURS 31.
      *リハビリ加算算定テーブル２
               05  RECE02-RIHA2-DAY9-AREA.
                   07  RECE02-RIHA2-SINDAI9     PIC 9(01).
                   07  RECE02-RIHA2-SIN9YMD-OCC1.
                     08  RECE02-RIHA2-SIN9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE02-RIHA2-SIN9YMD-OCC2.
                     08  RECE02-RIHA2-SIN9YMD2  PIC 9(01)  OCCURS 31.
      *
                   07  RECE02-RIHA2-KOKYUKI9    PIC 9(01).
                   07  RECE02-RIHA2-KOK9YMD-OCC1.
                     08  RECE02-RIHA2-KOK9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE02-RIHA2-KOK9YMD-OCC2.
                     08  RECE02-RIHA2-KOK9YMD2  PIC 9(01)  OCCURS 31.
      *
                   07  RECE02-RIHA2-HAIYO9      PIC 9(01).
                   07  RECE02-RIHA2-HAI9YMD-OCC1.
                     08  RECE02-RIHA2-HAI9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE02-RIHA2-HAI9YMD-OCC2.
                     08  RECE02-RIHA2-HAI9YMD2  PIC 9(01)  OCCURS 31.
      *
      *第三者行為レセプト該当フラグ
      *（０：第三者行為の請求点なし）
      *（１：第三者行為のみ請求点あり）
      *（２：第三者行為以外と第三者行為の両方に請求点あり）
      *（３：第三者行為の請求点なし　（第三者行為に院外処方あり））
      *（４：第三者行為のみ請求点あり（第三者行為以外に院外処方あり））
      *（９：第三者行為の請求点なし　（患者登録で第三者行為の登録なし））
           03  RECE02-DAISAN-GAITO              PIC 9(01).
      *第三者行為請求点数合計
           03  RECE02-DAISAN-TOTALTEN           PIC S9(08).
      *第三者行為一部負担金相当額合計
           03  RECE02-DAISAN-FTNMONEY           PIC 9(09).
      *第三者行為レセプト現物給付該当フラグ
           03  RECE02-DAISAN-GENBUTU-GAITO      PIC 9(01).
      *第三者行為一部負担金金額置換フラグ
           03  RECE02-DAISAN-CHIKAN             PIC 9(01).
      *第三者行為特記事項有無フラグ
           03  RECE02-DAISAN-TOKKI              PIC 9(01).
      *
      *請求点数がない保険組み合わせ有りフラグ
           03  RECE02-SKYTENFLG                 PIC 9(01).
      *
      *特記事項０９施有りフラグ
           03  RECE02-TOKKI09FLG                PIC 9(01).
      *【配】コメント有りフラグ
           03  RECE02-HAICOMFLG                 PIC 9(01).
      *初診ダミー・再診ダミーの剤回数
           03  RECE02-DUMMYKAISU                PIC 9(03).
      *
      *３歳児視覚聴覚精密健康診査（地域公費）
           03  RECE02-SEIKENFLG                 PIC 9(01).
           03  RECE02-SEIKENTEN                 PIC 9(07).
      *
      *高齢非該当フラグ
           03  RECE02-KOREIHIGAITO              PIC 9(01).
      *公費分離区分置換フラグ
           03  RECE02-KOHBUNRICHIKAN            PIC 9(01).
      *非減免対象分扱い一部負担金
           03  RECE02-GENMEN-NASI-KIN           PIC 9(09).
      *括弧再掲上の０円記載フラグ
           03  RECE02-SAIKEIZEROKSI1            PIC 9(01).
      *括弧再掲下の０円記載フラグ
           03  RECE02-SAIKEIZEROKSI2            PIC 9(01).
      *括弧再掲３の０円記載フラグ
           03  RECE02-SAIKEIZEROKSI3            PIC 9(01).
      *括弧再掲４の０円記載フラグ
           03  RECE02-SAIKEIZEROKSI4            PIC 9(01).
      *東大阪市減免コメントフラグ
           03  RECE02-GENMEN-COM                PIC 9(01).
      *【レセプト】保険欄−一部負担金記載なしフラグ
           03  RECE02-ICBKINKSI-NASI            PIC 9(01).
      *【レセプト】保険欄−一部負担金記載ありフラグ
           03  RECE02-ICBKINKSI-ARI             PIC 9(01).
      *【レセプト】公費欄−一部負担金金額置換フラグ
           03  RECE02-KOHCHIKAN                 PIC 9(01).
      *「（再掲上）：」置換金額
           03  RECE02-SAIKEI-CHIKAN-KIN1        PIC 9(09).
      *「（再掲下）：」置換金額
           03  RECE02-SAIKEI-CHIKAN-KIN2        PIC 9(09).
      *「（再掲３）：」置換金額
           03  RECE02-SAIKEI-CHIKAN-KIN3        PIC 9(09).
      *「（再掲４）：」置換金額
           03  RECE02-SAIKEI-CHIKAN-KIN4        PIC 9(09).
      *「保険負担金：」置換金額
           03  RECE02-HKN-CHIKAN-KIN            PIC 9(09).
      *「公１負担金：」置換金額
           03  RECE02-KOH-CHIKAN-KIN1           PIC 9(09).
      *「公２負担金：」置換金額
           03  RECE02-KOH-CHIKAN-KIN2           PIC 9(09).
      *「公３負担金：」置換金額
           03  RECE02-KOH-CHIKAN-KIN3           PIC 9(09).
      *「公４負担金：」置換金額
           03  RECE02-KOH-CHIKAN-KIN4           PIC 9(09).
      *「公費請求書公１負担金：」置換金額
           03  RECE02-SEI-CHIKAN-KIN1           PIC 9(09).
      *「公費請求書公２負担金：」置換金額
           03  RECE02-SEI-CHIKAN-KIN2           PIC 9(09).
      *「公費請求書公３負担金：」置換金額
           03  RECE02-SEI-CHIKAN-KIN3           PIC 9(09).
      *「公費請求書公４負担金：」置換金額
           03  RECE02-SEI-CHIKAN-KIN4           PIC 9(09).
      *
      *生保まとめ用（主保険単独分診区別点数）
           03  RECE02-SYUTANTEN-AREA.
               05  RECE02-SYUTANTEN             PIC S9(08)  OCCURS 50.
           03  RECE02-SYUTANARIFLG              PIC 9(01).
      *
      *湿布薬制限枚数解除及び院外湿布薬７０枚超理由コメント情報
           03  RECE02-SIPPUCHKFLG               PIC 9(01).
           03  RECE02-SIPPUCHKINF.
             04  RECE02-SIPPUCHK-OCC            OCCURS  10.
               05  RECE02-SIPPUCHK-OCC2         OCCURS  31.
                   07  RECE02-SIPPUCHK          PIC 9(03).
      *
      *１９原爆一般特殊計算フラグ（患者７割負担）
      *（インデックスの情報は保険組合せ番号情報とリンク）
           03  RECE02-GENBAKUINF.
               05  RECE02-GENBAKU-TOKFLG        PIC 9(01).
               05  RECE02-GENBAKU-OCC           OCCURS 10.
                   07  RECE02-GENBAKU-FTNKIN    PIC 9(07)V9(01).
                   07  RECE02-GENBAKU-IDX       PIC 9(01).
      *
      *請求点数０円レセプト作成フラグ
           03  RECE02-SKYZEROFLG                PIC 9(01).
      *
      *妊婦フラグ
           03  RECE02-NINPUFLG                  PIC 9(01).
      *
      *検査フラグ
           03  RECE02-KENSA-BNPFLG              PIC 9(01).
           03  RECE02-KENSA-NTPROFLG            PIC 9(01).
           03  RECE02-KENSA-ANPFLG              PIC 9(01).
      *
      *診療区分別点数情報（点数合計）
           03  RECE02-ALLTENSU-AREA.
               05  RECE02-ALLTENSU              PIC S9(08)  OCCURS 50.
      *
      *公費一部負担金情報
      *（インデックスの情報は保険組合せ番号情報とリンク）
           03  RECE02-KOHFTNKININF-AREA.
               05  RECE02-KOHFTNKININF          OCCURS  10.
                   07  RECE02-KOHFTNKIN         PIC 9(09)   OCCURS  4.
      *
      *外来排尿自立指導料の該当月最初の算定日
           03  RECE02-GAIJIRYMD                 PIC X(08).
      *
      *予備
           03  RECE02-YOBIETC-AREA.
               05  RECE02-YOBIETCINF            OCCURS  20.
                   07  RECE02-YOBIETC           PIC 9(09).
      *
