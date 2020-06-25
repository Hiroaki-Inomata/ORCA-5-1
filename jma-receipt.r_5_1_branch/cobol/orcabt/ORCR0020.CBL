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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCR0020.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 保険情報編集
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−小豆沢   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-小豆沢   01.12.10  自費保険のレセプト種別対応
      *                                     追加
      *  01.00.02    MCC-藤原     02.02.08  氏名が変更されたときの
      *                                     編集の追加
      *  01.01.03    MCC-藤原     02.02.13  同一保険者で記号番号が変わっ
      *                                     たとき任継・継の変更の時一枚に
      *                                     印刷するように修正
      *                                     保険組合せを３から１０に変更
      *  01.00.04   MCC-藤原      02.02.15  特記事項（老保）の編集の追加
      *  01.00.05   MCC-藤原      02.03.13  同一保険者で記号番号が変わった
      *                                     とき保険の履歴を表示するように修正
      *  01.00.06   MCC-小豆沢    02.03.25  長期特定疾患の場合、レセプトに
      *                                     印字しないように修正
      *  01.00.07   MCC-藤原      02.03.26  老人保険情報の編集の修正
      *  01.00.08   MCC-藤原      02.04.02  県単用項目ファイルのクリア処理追加
      *  01.00.09   MCC-藤原      02.05.28  訂正区分＝１のときレセプト種別は
      *                                     不明分とする
      *  01.00.10   MCC-藤原      02.07.18  生保の受給者番号を月代わり受給者
      *                                     番号マスタから編集する
      *  01.00.11   NACL-藤原     02.08.23  自費を不明分と分ける
      *  01.00.12   NACL-藤原     02.09.05  請求書用ファイルのみ端末ＩＤを固定
      *                                     にする
      *  01.00.13   NACL-小豆沢   02.09.24  （平成１４年１０月改正対応）
      *                                     レセプト種別コード追加
      *  01.00.14   NACL-小豆沢   02.09.27  （平成１４年１０月改正対応）
      *                                     老人保険の場合も給付割合のセットを
      *                                     を行う
      *  01.00.15   NACL-門脇     02.10.07  （平成１４年１０月改正対応）
      *                                     低所得フラグのセットを行う
      *  01.00.16   NACL-小豆沢   02.10.16  （平成１４年１０月改正対応）
      *                                     国保退職者の給付割合セット
      *  01.00.17   NACL-藤原     02.11.06  主保険が自費のとき保険履歴を作成
      *                                     しないようにする 
      *  01.00.18   NACL-藤原     02.11.25  前期高齢者の船員保険のときの
      *                                     レセプト種別の編集を修正 
      *                                     前期高齢者の共済組合の下船３ヶ月の
      *                                     ときのレセプト種別の編集を修正 
      *  01.00.19   NACL-小豆沢   02.12.04  退職者国保の３才未満の給付割合表示
      *                                     を修正
      *  01.00.20   NACL-藤原     03.01.17  特記事項０４老保の判定に年齢判定
      *                                     を追加する
      *  01.00.21   NACL-藤原     03.01.20  月代り受給者番号に入外区分を追加
      *  01.00.22   NACL-藤原     03.01.30  特記事項０４老保の判定にを７０歳
      *                                     を追加
      *  01.00.23    NACL-藤原    03/02/12  テスト患者番号区分・補助区分の追加
      *  01.00.24    NACL-藤原    03/03/07  レセプト記載外公費情報追加
      *                                     診療科数１０から２０へ変更
      *  01.00.25    NACL-門脇    03/03/07  在総診フラグセット
      *  01.00.26    NACL-藤原    03/03/11  種別不明の情報を編集
      *  01.00.27    NACL-門脇    03/03/18  在総診フラグセットの修正
      *  01.00.28    NACL-門脇    03/04/16  船員保険（被保険者）＋通勤災害
      *                                     フラグセット
      *  01.00.29    NACL-藤原    03/04/21  地方公費データのとき主保険が
      *                                     無い場合は不明分とする
      *  01.00.30    NACL-小豆沢  03/05/30  山形県の地方公費は主保険が
      *                                     国保の場合は公費カウントしない
      *  01.00.31    NACL-藤原    03/10/07  福岡県の地方公費は主保険が
      *                                     国保の場合は公費カウントしない
      *                                     （法別番号４１以外のとき）
      *  01.00.32    NACL-藤原    03/10/27  患者情報の編集の修正
      *                                     県番号の追加
      *  01.00.33    NACL-門脇    03/11/27  ”老保”編集条件修正
      *  01.00.34    NACL-門脇    04/01/14  診療行為の包括分は除く
      *                                     SRYACCT5→31
      *  01.00.35    NACL-藤原    04/02/04  月途中に老保の変更があった場合
      *                                     のＫＥＹの編集の修正
      *  01.00.36    NACL-門脇    04/02/12  年齢判定文修正
      *  01.00.37    NACL-藤原    04/03/08  シス管による生保受給者番号の編集
      *  01.00.38    NACL-門脇    04/04/06  公費単独７０歳未満で老人保険入力
      *                                     のある方の在総診フラグ編集修正
      *  01.00.39    NACL-藤原    04/05/27  同一開始日の複数自費レセプトが
      *                                     別々に作成されるように修正
      *  01.00.40    NACL-藤原    04/06/08  適用終了日の追加
      *  01.00.41    NACL-藤原    04/06/15  印刷順の追加（個別作成時のみ）
      *  01.00.42    NACL-藤原    04/07/12  生保の受給者番号を月代わり受給者DBに
      *                                     無い場合は公費DBより編集する
      *                                     保険組合せの公費ＩＤを追加
      *  01.00.43    NACL-藤原    04/07/20  特例退職者＋老人のとき不明分
      *                                     とする
      *  01.00.44    NACL-藤原    04/07/23  補助区分の追加に伴うレセプト
      *                                     種別・給付割合の編集を変更
      *  01.00.45    NACL-藤原    04/10/04  特例退職者＋老人のとき本人の
      *                                     み不明分とする
      *  01.00.46    NACL-門脇    04/10/05  公費単独の在総診フラグセットの修正
      *  中止処理追加
      *  01.01.01    NACL-藤原    04/09/28  中止処理の追加
      *  01.03.02    NACL-藤原    04/11/01  記載なしの公費の処理
      *  01.03.03    NACL-門脇    05/02/25  特記事項”老保”編集修正
      *
      *  02.06.01    NACL-藤原    05/05/11  沖縄県対応
      *                                     精神障害者特別措置(121)の場合
      *                                     主保険なしでも不明分としない
      *  02.06.02    NACL-藤原    05/05/17  神奈川県の障害(180､280)は
      *                                     国保で負担者番号が空白の場合
      *                                     公費カウントしない
      *  02.06.03    NACL-門脇    05/06/09  低所得フラグ編集修正
      *  02.06.04    NACL-竹田    05/05/30  realtime preview 処理追加
      *  02.06.05    NACL-藤原    05/07/05  本人−外来−内容の追加
      *  02.06.06    NACL-藤原    05/07/28  レセプト特記事項編集情報のため
      *                                     のレセプト請求しない地方公費の
      *                                     を追加
      *
      *  02.07.01    NACL-藤原    05/08/20  特別療養費対応
      *  02.07.02    NACL-藤原    05/10/06  公費単独の三歳未満の割合設定
      *                                     をしないようにする
      *  02.07.03    NACL-藤原    06/01/05  山形県のレセプト様式変更対応
      *                                     平成１８年１月分から標準的な
      *                                     レセプト様式へ変更
      *
      *  02.08.01    NACL-藤原    06/02/20  主科対応
      *
      *  02.09.01    NACL-門脇    06/04/13  ２００６年４月改正対応
      *  02.09.02    NACL-門脇    06/05/18  岡山県西粟倉村退職者国保２割対応
      *  3.0.0
      *  03.00.01    NACL-藤原    06/05/15  入退院履歴記載対応
      *
      *  03.01.01    NACL-藤原    06/07/03  旧総合病院の科表示
      *
      *  03.02.01    NACL-藤原    06/07/31  国保主科対応
      *  03.02.02    NACL-藤原    06/09/07  ２００６年１０月改正対応
      *                                     福岡地方公費対応
      *
      *  03.03.01    NACL-門脇    06/09/27  ２００６年１０月改正対応
      *                                     特記事項１６長２対応
      *                                     レセプト種別
      *                                     老齢福祉年金 
      *  03.03.02    NACL-藤原    06/09/19  月途中での氏名変更対応
      *  03.03.03    NACL-藤原    06/12/13  公費分で種別が設定できなかっ
      *                                     たときの対応
      *
      *  03.04.01    NACL-藤原    06/12/05  未請求レセ対応
      *  03.04.02    NACL-藤原    07/02/06  月途中の受給者番号変更に対応
      *  03.04.03    NACL-門脇    07/02/19  本人−外来−回−上限額のセット
      *  03.04.04    NACL-門脇    07/03/23  月途中医療機関コード変更時対応
      *  03.04.05    NACL-門脇    07/04/01  現物給付対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-門脇    07/07/31  ７０歳未満現物給付保険者毎で計算
      *  03.05.03    NACL-門脇    07/08/01  所得情報取扱い機能強化対応
      *  03.05.04    NACL-門脇    07/08/01  月途中の所得情報変更対応
      *  03.05.05    NACL-藤原    07/09/04  宮崎県「公費９０」対応
      *                                     ２００７年９月以降のみ対象 
      *
      *  04.01.01    NACL-藤原    07/01/21  月途中の負担者番号変更対応
      *                                     （生保の公費単独のみ） 
      *  04.01.02    NACL-藤原    08/02/12  宮崎県「公費９０」対応
      *                                     通常の記載方法に戻す
      *
      *  04.02.01    NACL-藤原    08/03/04  平成２０年４月改正対応
      *  04.02.02    NACL-門脇    08/03/24  在医総管の診療行為コード追加
      *  04.02.03    NACL-藤原    08/03/25  広域連合対応
      *                                     中国残留法人等
      *                                     レセプト種別
      *  04.02.04    NACL-藤原    08/04/14  月途中の負担者番号変更対応
      *                                     （中国残留法人等の公費単独） 
      *  04.02.05    NACL-藤原    08/04/18  神奈川県の障害は
      *                                     国保で負担者番号が空白の場合
      *                                     法別番号８０で判定して
      *                                     公費カウントしない
      *  04.02.06    NACL-藤原    08/04/25  保険履歴の記載変更
      *  04.02.07    NACL-門脇    08/05/22  在総診判定条件修正
      *  04.02.08    NACL-門脇    08/06/24  後期高齢者の現物給付対象編集修正
      *
      *  04.03.01    NACL-藤原    08/05/07  公費４種までの併用の記録対応
      *  04.03.02    NACL-藤原    08/06/19  医師別出力対応
      *  04.03.03    NACL-藤原    08/08/05  患者登録特記事項入力対応
      *  04.03.04    NACL-門脇    08/10/16  公費−低２・低１項目編集
      *
      *  04.04.01    NACL-藤原    09/01/14  月途中の生保変更時の保険履歴
      *                                     記載対応
      *                                     月途中に医保と生保の併用から
      *                                     生保単独になったときのまとめ
      *                                     公費分点欄に生保単独分記載
      *  04.04.02    NACL-藤原    09/10/21  月途中の負担者番号変更対応
      *                                     （自立支援の公費単独） 
      *  04.04.03    NACL-藤原    09/12/10  同月に医保と生保の併用と生保
      *                                     単独が混在する場合の複数診療
      *                                     科による入力時の記載対応
      *  04.04.04    NACL-藤原    09/12/17  月途中に医保と自立支援の併用
      *                                     から自立支援単独になったとき
      *                                     のまとめ対応
      *  04.04.05    NACL-門脇    10/08/23  高齢非該当対応
      *  04.04.06    NACL-藤原    10/09/14  医療観察法対応
      *                                     公費単独のレセプトとする
      *
      *  04.05.01    NACL-藤原    09/07/23  主科未設定患者の主科再取得対応
      *  04.05.02    NACL-藤原    10/01/05  治験対応
      *                                     平成２０年４月分より自費レセ
      *                                     とする 
      *  04.05.03    NACL-藤原    10/04/14  平成２２年４月改正対応
      *                                     請求に係わる記録条件仕様の
      *                                     変更に伴う修正
      *                                     診療科２、３を追加
      *  04.05.04    NACL-藤原    11/03/25  災害対応
      *                                     連絡先情報追加
      *  04.05.05    NACL-藤原    11/04/18  災害対応
      *                                     主保険情報が同じで適用期間が
      *                                     区切られている場合の保険履歴 
      *  04.05.06    NACL-藤原    11/08/25  公費単独対応
      *                                     公費を最大５０に変更
      *  04.05.07    NACL-門脇    11/10/05  岡山県西粟倉村退職者国保２割対応
      *                                     修正（平成２３年９月まで）
      *  04.05.08    NACL-藤原    11/11/18  他保険での同日再入院時の保険
      *                                     履歴対応
      *  04.05.09    NACL-門脇    12/04/10  平成２４年４月改正
      *                                     現物給付化対応
      *  04.05.10    NACL-門脇    12/05/23  在医総算定フラグ編集修正
      *  04.05.11    NACL-門脇    12/11/19  ９７２と９７４の混在時対応
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.02    NACL-藤原    11/02/01  主保険がない場合、児童保護(53)
      *                                     は公費単独のレセプトとする
      *  04.06.03    NACL-藤原    11/02/24  月途中に医保と児童保護(53)の
      *                                     併用から単独になったときのまとめ
      *  04.06.04    NACL-藤原    11/12/12  点検レセ０点公費印字対応
      *  04.06.05    NACL-藤原    12/08/01  保険履歴ファイルのエラー処理
      *                                     中止 
      *  04.06.06    NACL-藤原    12/10/04  同一レセプトの公費が４つを超
      *                                     えた場合の種別不明対応
      *  04.06.07    NACL-藤原    12/09/27  生活保護期間重複対応
      *  04.06.08    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *
      *  04.07.01    NACL-藤原    11/06/02  入退院履歴記載対応
      *                                     「2自費分は含まない」を追加
      *  04.07.02    NACL-門脇    12/10/02  高齢者判定フラグ編集
      *  04.07.03    NACL-藤原    12/12/17  複数枚レセプト分割対応
      *  04.07.04    NACL-門脇    13/07/10  自賠責（第三者行為）対応
      *  04.07.05    NACL-門脇    14/04/04  平成２６年４月改正対応
      *  04.07.06    NACL-門脇    14/04/08  長期の特記事項重複修正
      *  04.07.07    NACL-藤原    14/03/30  平成２６年４月改正対応
      *  04.07.08    NACL-藤原    14/04/30  治験が複数あるときの対応
      *  04.07.09    NACL-藤原    14/06/05  月途中のまとめのとき第二公費
      *                                     マル長のときも対象とする
      *  04.07.10    NACL-門脇    14/07/31  第三者行為の現物給付対応
      *  04.07.11    NACL-藤原    14/08/18  ７５歳以上の海外居住者２割対応
      *  04.07.12    NACL-藤原    14/09/10  月途中のまとめのとき第二公費
      *                                     マル長のときの修正
      *  04.07.13    NACL-門脇    14/10/21  低所得フラグ編集修正
      *  04.07.14    NACL-藤原    15/01/24  同日再入院時の入退院履歴の記録
      *                                     を修正
      *                                    （入院日と退院日が同じとき）
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.02    NACL-門脇    15/10/21  現物給付対象外の低所得（食事）対応
      *  04.08.03    NACL-藤原    15/10/08  複数枚レセプト分割対応
      *                                     公費単独の２回以上の分割対応
      *  04.08.04    NACL-門脇    16/02/08  RECE02の予備項目使用分について、
      *                                     個々に項目を設ける事に伴う修正
      *  04.08.05    NACL-門脇    16/03/28  平成２８年４月改正対応
      *  04.08.06    NACL-門脇    17/10/18  入院料と異なる保険組み合わせ
      *                                     での食事算定対応
      *  04.08.07    NACL-藤原    18/03/28  平成３０年４月改正対応
      *
      *  05.00.01    NACL-藤原    18/06/11  Warning 対応
      *  05.00.02    NACL-門脇    19/07/03  在医総算定フラグ編集を
      *                                     別のプログラムへ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE01-FILE     ASSIGN  RECE01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE01-KEY
                                   FILE    STATUS  IS  STS-RECE01.
           SELECT  RECE02-FILE     ASSIGN  RECE02PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE02-KEY
                                   FILE    STATUS  IS  STS-RECE02.
           SELECT  RECE09-FILE     ASSIGN  RECE09PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE09-KEY
                                   FILE    STATUS  IS  STS-RECE09.
      *    レセプト（県単用項目）データ
           SELECT  RECE11-FILE     ASSIGN  RECE11PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE11-KEY
                                   FILE    STATUS  IS  STS-RECE11.
           SELECT  RECE12-FILE     ASSIGN  RECE12PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE12-KEY
                                   FILE    STATUS  IS  STS-RECE12.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE01-FILE.
       01  RECE01-REC. 
           COPY    "CPRCF001.INC".
      *
       FD  RECE02-FILE.
       01  RECE02-REC. 
           COPY    "CPRCF002.INC".
      *
       FD  RECE09-FILE.
       01  RECE09-REC. 
           COPY    "CPRCF009.INC".
      *
      *    レセプト（県単用項目）データ
       FD  RECE11-FILE.
       01  RECE11-REC. 
           COPY    "CPRCF011.INC".
      *
       FD  RECE12-FILE.
       01  RECE12-REC. 
           COPY    "CPRCF068.INC"  REPLACING  //RECE68//
                                   BY         //RECE12//.
      *     
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE01PARA//.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE02PARA//.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE09PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE11PARA//.
      *
      *     入退院履歴情報ファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE12PARA//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECEERR//.
      *
      *    ファイル名取得
           COPY    "CPRECEDAT1.INC".
           COPY    "CPTEMPFL.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE01                              PIC X(02).
           03  STS-RECE02                              PIC X(02).
           03  STS-RECE09                              PIC X(02).
           03  STS-RECE11                              PIC X(02).
           03  STS-RECE12                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-RECE01                              PIC 9(01). 
           03  FLG-RECE09                              PIC 9(01). 
           03  FLG-RECE12                              PIC 9(01). 
           03  FLG-PTHKNINF                            PIC 9(01).
           03  FLG-PTKOHINF                            PIC 9(01).
           03  FLG-HKNNUM                              PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-CHK                                 PIC 9(01).
           03  FLG-MONTHLYNUM                          PIC 9(01).
           03  FLG-PTNYUINRRK                          PIC 9(01).
           03  FLG-PTNYUINRRK12                        PIC 9(01).
           03  FLG-PTNYUINRRK22                        PIC 9(01).
           03  FLG-PTNYUINRRK23                        PIC 9(01).
           03  FLG-NYUINACCT                           PIC 9(01).
           03  FLG-HKNCOMBI                            PIC 9(01).
           03  FLG-KYUSEIRRK                           PIC 9(01).
           03  FLG-PTTOKKI                             PIC 9(01).
      *
           03  FLG-SET                                 PIC 9(01).
           03  FLG-ROUJINHOKEN                         PIC 9(01).
           03  FLG-GENBUTU70                           PIC 9(01).
           03  FLG-TAISYO                              PIC 9(01).
           03  FLG-SEIHO                               PIC 9(01).
           03  FLG-RIREKI-HEN                          PIC 9(01).
           03  FLG-PTRSIINF                            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE12                              PIC 9(06). 
           03  CNT-RECE09                              PIC 9(06). 
           03  CNT-IN-CANCEL                           PIC 9(06).
           03  CNT-TBL-OCC                             OCCURS  2. 
               05  CNT-IN                              PIC 9(06).
               05  CNT-OUT                             PIC 9(06).
      *
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-HKNCOMBI                      PIC 9(04).
           03  KEY-OLD.
               05  KEY-O-HKNCOMBI                      PIC 9(04).
      *
       01  KEY-AREA1                           VALUE   LOW-VALUE.
           03  KEY-NEW1.
               05  KEY-NEW11.
                   07  KEY-N-SRYYM                     PIC 9(06).
                   07  KEY-N-PTID                      PIC 9(10).
               05  KEY-N-RECEIPT-KBN                   PIC X(10).
           03  KEY-OLD1.
               05  KEY-OLD11.
                   07  KEY-O-SRYYM                     PIC 9(06).
                   07  KEY-O-PTID                      PIC 9(10).
               05  KEY-O-RECEIPT-KBN                   PIC X(10).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDY                                     PIC 9(03).
           03  IDX1                                    PIC 9(03).
           03  IDX2                                    PIC 9(03).
           03  IDX3                                    PIC 9(03).
           03  IDX4                                    PIC 9(03).
           03  IDX5                                    PIC 9(03).
           03  IDX6                                    PIC 9(03).
           03  IDY1                                    PIC 9(03).
           03  IDY2                                    PIC 9(03).
           03  IDZ                                     PIC 9(02).
           03  IDZ1                                    PIC 9(02).
           03  IDZ2                                    PIC 9(02).
           03  IDVV                                    PIC 9(03).
           03  IDWW                                    PIC 9(03).
           03  IDXX                                    PIC 9(03).
           03  IDYY                                    PIC 9(03).
           03  IDZZ                                    PIC 9(03).
           03  TOKKI-IDX                               PIC 9(02).
           03  TOKKI-IDX2                              PIC 9(02).
           03  SYSKAN-IDX                              PIC 9(03).
           03  SYOTOKU-IDX                             PIC 9(02).
           03  SYOTOKU-IDX2                            PIC 9(02)
                                                       OCCURS 4.
           03  SYOTOKU-IDX9                            PIC 9(02).
           03  KOHFTN-IDX                              PIC 9(02).
      *
      *    パス名領域
       01  WK-PATH                                     PIC X(20).
      *
      *    レセプト種別判定用領域
       01  WK-RECESYUBETU-AREA.
           03  WK-KOKUHO                               PIC X(01).
           03  WK-IHO                                  PIC X(01).
           03  WK-RJN                                  PIC X(01).
           03  WK-KOHI-CNT                             PIC 9(01).
           03  WK-HONKZKKBN                            PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC  X(06).
               05  WRK-PARA-TERMID                     PIC  X(16).
               05  WRK-PARA-SYSYMD                     PIC  X(08).
               05  WRK-PARA-RECEKBN                    PIC  X(01).
               05  WRK-PARA-JOBID                      PIC  9(07).
               05  WRK-PARA-SHELLID                    PIC  X(08).
           03  WRK-PARA-HOSPNUM                        PIC  9(02).
      *
           03  WRK-SYS-SRYYMD                          PIC  X(08).
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYYMM                         PIC  9(06).
               05  WRK-SRYDD                           PIC  9(02).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-HKNNUM                PIC X(03).
      *
           03  WRK-JIHI-FLG              PIC X(01).
      *
           03  WRK-CHOKI                 PIC X(03).
           03  WRK-CHOKI2                PIC X(03).
      *
           03  WRK-ZAITAKU               PIC X(01).
      *
           03  RJN-RATE-KBN              PIC X(01).
      *
           03  WRK-RECESKYKBN            PIC  X(01).
      *
           03  WK-PTNYUINRRK-TENSTUYMD   PIC  X(08).
           03  WK-PTNYUINRRK-TENSTUYMD2  PIC  X(08).
      *
           03  WRK-ZAITAKU-SRYCD         PIC  X(09).
      *
           03  WRK-SEIHO-FLG             PIC 9(01).
      *
      *    主科別情報
           03  WRK-SYUACC-IO-SYUKA-AREA.
               05  WRK-SYUACC-IO-SRYKATBL  OCCURS  50.
      *            主科
                   07  WRK-SYUACC-IO-SRYKA PIC X(02).
      *            請求点数合計
                   07  WRK-SYUACC-IO-TENSU PIC S9(09).
      *            受診回数合計
                   07  WRK-SYUACC-IO-KAISU PIC 9(03).
      *            レセ電算診療科（コード）
                   07  WRK-SYUACC-IO-RECESYUKA
                                           PIC X(02).
      *    主科別情報
           03  WRK-SYUACC-REC.
      *        主科
               05  WRK-SYUACC-SRYKA        PIC X(02).
      *        請求点数合計
               05  WRK-SYUACC-TENSU        PIC S9(09).
      *        受診回数合計
               05  WRK-SYUACC-KAISU        PIC 9(03).
      *        レセ電算診療科（コード）
               05  WRK-SYUACCRECESYUKA     PIC X(02).
      *              
      *    診療回数テーブル（入院会計用　１：基本　２：組合せ）
           03  WRK-DAY-AREA.
               05  WRK-DAY-TBL         OCCURS  2.
                   07  WRK-DAY         PIC 9(03)
                                           OCCURS   31.
      *    保険組合せ番号情報
           03  WRK-HKNCOMBIINF-AREA.
               05  WRK-HKNCOMBIINF     OCCURS  10.
      *            保険組合せ番号
                   07  WRK-HKNCOMBI    PIC 9(04).
      *            適用開始年月日
                   07  WRK-TEKSTYMDX   PIC X(08). 
      *            適用終了年月日
                   07  WRK-TEKEDYMDX   PIC X(08).
      *        適用開始年月日
               05  WRK-TEKSTYMD-X      PIC X(08). 
      *        適用終了年月日
               05  WRK-TEKEDYMD-X      PIC X(08). 
      *
      *    患者年齢算出用エリア
       01  WRK-NENREI-AREA.
           03  WRK-NENREI                PIC 9(03).
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR         PIC X(200).
           03  WRK-ERRMSG          PIC X(300).
           03  WRK-MCP-RC          PIC 9(9).
           03  WRK-ERR-FILE-STS    PIC X(02).
           03  WRK-ERR-FILE-MSG    PIC X(100).
           03  WRK-ERR-FILE-MSG1   PIC X(17).
           03  WRK-ERR-FLG         PIC 9(01).
      *    エラーファイル名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *    FULLNAME内のBASENAME開始位置
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *
      *    初回番号(最新の入院歴分)                                                                 
       01  WRK-SHONUM                  PIC 9(03).                     
      *
      *    ワーク入力ファイル退避
       01  WRK-RECE01-REC.
           COPY    "CPRCF001.INC"      REPLACING  //RECE01-//
                                       BY         //WRK-RECE01-//.
      *
      *    ワーク生保まとめ
       01  WRK-SEIHO-RECE01-AREA.
           02  WRK-SEIHO-RECE01-REC    OCCURS  50. 
           COPY    "CPRCF001.INC"      REPLACING  //RECE01-//
                                       BY         //WRK-SEH-01-//.
           02  WRK-SEIHO-RECE01-PRTFLG PIC 9(01)  OCCURS  50.
           02  WRK-SEIHO-RECE01-MAX    PIC 9(03).
      *
           02  WRK-KOH-RECE01-REC. 
           COPY    "CPRCF001.INC"      REPLACING  //RECE01-//
                                       BY         //WRK-KOH-01-//.
      *
      *    ワーク保険組合せ（入院会計用） 
       01  WRK-HKNCOMBI-TABLE.
           02  WRK-HKNCOMBI-REC        OCCURS  10. 
           COPY    "CPHKNCOMBI.INC"    REPLACING  //COMB-//
                                       BY         //WRK-COMB-//.
           03  WRK-HKNCOMBI-SYOKBN     PIC X(01).   
      *
      *    ワーク入退院履歴
       01  WRK-RECE12-REC. 
           COPY    "CPRCF068.INC"  REPLACING  //RECE12//
                                   BY         //WRK-RECE12//.
      *
      *    ワーク入院履歴
       01  WRK-PTNRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //WRK-PTNRRK-//.
      *
      *    ワーク保険履歴
       01  TABLE-RECE09-AREA.
           02  TBL-RECE09-OCC          OCCURS  100.
           COPY    "CPRCF009.INC"      REPLACING  //RECE09//
                                       BY         //TBL-RECE09//.
      * 
      *    保険番号マスタ退避ワーク
       01  WRK-HKNNUM-AREA.
           03  WRK-HKNNUM-IDX                  PIC 9(05).
           03  WRK-HKNNUM-MAX                  PIC 9(05).
           03  WRK-CHK-HKNNUM                  PIC X(03).
      *
       01  TABLE-HKNNUM-AREA.
         02  TBL-HKNNUM-REC                    OCCURS  2000.
           COPY    "CPHKNNUM.INC"  REPLACING   //HKN-//
                                   BY          //TBL-HKN-//.        
       01  TBL-HKNNUM-CONST-MAX                PIC 9(05)
                                               VALUE   2000.
      *
      *    主保険用保険番号情報
       01  WRK-HKNNUM-REC.
           COPY    "CPHKNNUM.INC"  REPLACING  //HKN-//
                                   BY         //WRK-HKN-//.
      *
      *    患者特記事項情報
       01  TABLE-PTTOKKI-AREA.
           03  TBL-PTTOKKI-OCC                 OCCURS  100.
               05  TBL-PTTOKKI-CD              PIC X(02).
               05  TBL-PTTOKKI-NAIYO           PIC X(12).
           03  TBL-PTTOKKI-MAX                 PIC 9(03).
      *
       01  WRK-CONS-AREA.
      *    
      *    生活保護保険番号 
           03  WRK-CONS-SEIHO-HKNNUM
                                   PIC X(03)   VALUE   "012". 
      *    
      *    中国残留邦人等保険番号 
           03  WRK-CONS-ZANRYUU-HKNNUM
                                   PIC X(03)   VALUE   "025". 
      *    
      *    自費保険番号 
           03  WRK-CONS-JIHI-HKNNUM
                                   PIC X(03)   VALUE   "980". 
      *
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM
                                   PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM
                                   PIC X(03)   VALUE   "973".       
      *    公害保険番号 
           03  WRK-CONS-KOUGAI-HKNNUM
                                   PIC X(03)   VALUE   "975". 
      *    
      *    固定端末ＩＤ
           03  WRK-CONS-TERMID     PIC X(32)   VALUE
                                   "0100007FZZZZZZZZZZZZZZZZZZZZZZZZ". 
      *
           03  WRK-CONS-SRYYM-200609
                                   PIC 9(06)   VALUE   200609.      
           03  WRK-CONS-SRYYM-200804
                                   PIC 9(06)   VALUE   200804.      
      *
      *    在総診テーブル
       01  TBL-ZAITAKU-AREA.
      *    寝たきり老人在宅総合診療料（院内処方せんを交付する）
           03  FILLER          PIC X(09)   VALUE   "114701210".
      *    寝たきり老人在宅総合診療料（院内処方せんを交付しない）
           03  FILLER          PIC X(09)   VALUE   "114701710".
      *    在宅末期医療総合診療料（院外）
           03  FILLER          PIC X(09)   VALUE   "114007610".
      *    在宅末期医療総合診療料（院内）
           03  FILLER          PIC X(09)   VALUE   "114007710".
      *
       01  TBL-ZAITAKU-R       REDEFINES   TBL-ZAITAKU-AREA.
            03  TBL-ZAITAKU    PIC X(09)   OCCURS   4.       
      *
       01  TBL-ZAITAKU-MAX     PIC 9(02)   VALUE    4.  
      *
      *    在総診テーブル（２００６年４月診療分〜）
       01  TBL-ZAITAKU2-AREA.
      *    在宅末期医療総合診療料（院外）
           03  FILLER          PIC X(09)   VALUE   "114007610".
      *    在宅末期医療総合診療料（院内）
           03  FILLER          PIC X(09)   VALUE   "114007710".
      *    在宅時医学総合管理料１（院外処方せん）
           03  FILLER          PIC X(09)   VALUE   "114012210".
      *    在宅時医学総合管理料１（院内処方せん）
           03  FILLER          PIC X(09)   VALUE   "114012310".
      *    在宅時医学総合管理料２（院内処方せん）
           03  FILLER          PIC X(09)   VALUE   "114012410".
      *    在宅時医学総合管理料２（院外処方せん）
           03  FILLER          PIC X(09)   VALUE   "114007510".
      *    在医総・在医総管（ＤＵＭＭＹ）
           03  FILLER          PIC X(09)   VALUE   "099140003".
      *
       01  TBL-ZAITAKU2-R      REDEFINES   TBL-ZAITAKU2-AREA.
            03  TBL-ZAITAKU2   PIC X(09)   OCCURS   7.
      *
       01  TBL-ZAITAKU2-MAX    PIC 9(02)   VALUE    7.
      *
      *    在総診テーブル（２００８年４月診療分〜）
       01  TBL-ZAITAKU3-AREA.
      *    在宅末期医療総合診療料（院外）
           03  FILLER          PIC X(09)   VALUE   "114007610".
      *    在宅末期医療総合診療料（院内）
           03  FILLER          PIC X(09)   VALUE   "114007710".
      *    在宅時医学総合管理料１（院外処方せん）
           03  FILLER          PIC X(09)   VALUE   "114012210".
      *    在宅時医学総合管理料１（院内処方せん）
           03  FILLER          PIC X(09)   VALUE   "114012310".
      *    在宅時医学総合管理料２（院内処方せん）
           03  FILLER          PIC X(09)   VALUE   "114012410".
      *    在宅時医学総合管理料２（院外処方せん）
           03  FILLER          PIC X(09)   VALUE   "114007510".
      *    在医総・在医総管（ＤＵＭＭＹ）
           03  FILLER          PIC X(09)   VALUE   "099140003".
      *    特定施設入居時等医学総合管理料１（処方せんを交付）
           03  FILLER          PIC X(09)   VALUE   "114013010".
      *    特定施設入居時等医学総合管理料１（処方せんを交付しない）
           03  FILLER          PIC X(09)   VALUE   "114013110".
      *    特定施設入居時等医学総合管理料２（処方せんを交付）
           03  FILLER          PIC X(09)   VALUE   "114013210".
      *    特定施設入居時等医学総合管理料２（処方せんを交付しない）
           03  FILLER          PIC X(09)   VALUE   "114013310".
      *
       01  TBL-ZAITAKU3-R      REDEFINES   TBL-ZAITAKU3-AREA.
            03  TBL-ZAITAKU3   PIC X(09)   OCCURS  11.
      *
       01  TBL-ZAITAKU3-MAX    PIC 9(02)   VALUE   11.
      *
      *    在総診テーブル（２０１２年４月診療分〜）
       01  TBL-ZAITAKU4-AREA.
      *    在がん医総（在支診等）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114007610".
      *    在がん医総（在支診等）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114007710".
      *    在医総管（在支診等）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114012210".
      *    在医総管（在支診等）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114012310".
      *    在医総管（在支診以外）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114012410".
      *    在医総管（在支診以外）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114007510".
      *    在医総・在医総管（ＤＵＭＭＹ）
           03  FILLER          PIC X(09)   VALUE   "099140003".
      *    特医総管（在支診等）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114013010".
      *    特医総管（在支診等）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114013110".
      *    特医総管（在支診等以外）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114013210".
      *    特医総管（在支診等以外）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114013310".
      *
      *    在がん医総（機能強化した在支診等）（病床あり）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114019510".
      *    在がん医総（機能強化した在支診等）（病床あり）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114019610".
      *    在がん医総（機能強化した在支診等）（病床なし）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114019710".
      *    在がん医総（機能強化した在支診等）（病床なし）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114019810".
      *    在医総管（機能強化した在支診等）（病床あり）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114018710".
      *    在医総管（機能強化した在支診等）（病床あり）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114018810".
      *    在医総管（機能強化した在支診等）（病床なし）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114018910".
      *    在医総管（機能強化した在支診等）（病床なし）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114019010".
      *    特医総管（機能強化した在支診等）（病床あり）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114019110".
      *    特医総管（機能強化した在支診等）（病床あり）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114019210".
      *    特医総管（機能強化した在支診等）（病床なし）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114019310".
      *    特医総管（機能強化した在支診等）（病床なし）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114019410".
      *
       01  TBL-ZAITAKU4-R      REDEFINES   TBL-ZAITAKU4-AREA.
            03  TBL-ZAITAKU4   PIC X(09)   OCCURS  23.
      *
       01  TBL-ZAITAKU4-MAX    PIC 9(02)   VALUE   23.
      *
      *    在総診テーブル（２０１４年４月診療分〜）
       01  TBL-ZAITAKU5-AREA.
      *    在がん医総（在支診等）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114007610".
      *    在がん医総（在支診等）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114007710".
      *    在医総・在医総管（ＤＵＭＭＹ）
           03  FILLER          PIC X(09)   VALUE   "099140003".
      *
      *    在がん医総（機能強化した在支診等）（病床あり）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114019510".
      *    在がん医総（機能強化した在支診等）（病床あり）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114019610".
      *    在がん医総（機能強化した在支診等）（病床なし）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114019710".
      *    在がん医総（機能強化した在支診等）（病床なし）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114019810".
      *
      *    在医総管（機能強化在支診等）（病床有・処方せん有・同一建物以外）
           03  FILLER          PIC X(09)   VALUE   "114023110".
      *    在医総管（機能強化在支診等）（病床有・処方せん有・同一建物居住）
           03  FILLER          PIC X(09)   VALUE   "114023210".
      *    在医総管（機能強化在支診等）（病床有・処方せん無・同一建物以外）
           03  FILLER          PIC X(09)   VALUE   "114023310".
      *    在医総管（機能強化在支診等）（病床有・処方せん無・同一建物居住）
           03  FILLER          PIC X(09)   VALUE   "114023410".
      *    在医総管（機能強化在支診等）（病床無・処方せん有・同一建物以外）
           03  FILLER          PIC X(09)   VALUE   "114023510".
      *    在医総管（機能強化在支診等）（病床無・処方せん有・同一建物居住）
           03  FILLER          PIC X(09)   VALUE   "114023610".
      *    在医総管（機能強化在支診等）（病床無・処方せん無・同一建物以外）
           03  FILLER          PIC X(09)   VALUE   "114023710".
      *    在医総管（機能強化在支診等）（病床無・処方せん無・同一建物居住）
           03  FILLER          PIC X(09)   VALUE   "114023810".
      *    在医総管（在支診等）（処方せんあり・同一建物居住者以外）
           03  FILLER          PIC X(09)   VALUE   "114023910".
      *    在医総管（在支診等）（処方せんあり・同一建物居住者）
           03  FILLER          PIC X(09)   VALUE   "114024010".
      *    在医総管（在支診等）（処方せんなし・同一建物居住者以外）
           03  FILLER          PIC X(09)   VALUE   "114024110".
      *    在医総管（在支診等）（処方せんなし・同一建物居住者）
           03  FILLER          PIC X(09)   VALUE   "114024210".
      *    在医総管（在支診等以外）（処方せんあり・同一建物居住者以外）
           03  FILLER          PIC X(09)   VALUE   "114024310".
      *    在医総管（在支診等以外）（処方せんあり・同一建物居住者）
           03  FILLER          PIC X(09)   VALUE   "114024410".
      *    在医総管（在支診等以外）（処方せんなし・同一建物居住者以外）
           03  FILLER          PIC X(09)   VALUE   "114024510".
      *    在医総管（在支診等以外）（処方せんなし・同一建物居住者）
           03  FILLER          PIC X(09)   VALUE   "114024610".
      *
      *    特医総管（機能強化在支診等）（病床有・処方せん有・同一建物以外）
           03  FILLER          PIC X(09)   VALUE   "114024910".
      *    特医総管（機能強化在支診等）（病床有・処方せん有・同一建物居住）
           03  FILLER          PIC X(09)   VALUE   "114025010".
      *    特医総管（機能強化在支診等）（病床有・処方せん無・同一建物以外）
           03  FILLER          PIC X(09)   VALUE   "114025110".
      *    特医総管（機能強化在支診等）（病床有・処方せん無・同一建物居住）
           03  FILLER          PIC X(09)   VALUE   "114025210".
      *    特医総管（機能強化在支診等）（病床無・処方せん有・同一建物以外）
           03  FILLER          PIC X(09)   VALUE   "114025310".
      *    特医総管（機能強化在支診等）（病床無・処方せん有・同一建物居住）
           03  FILLER          PIC X(09)   VALUE   "114025410".
      *    特医総管（機能強化在支診等）（病床無・処方せん無・同一建物以外）
           03  FILLER          PIC X(09)   VALUE   "114025510".
      *    特医総管（機能強化在支診等）（病床無・処方せん無・同一建物居住）
           03  FILLER          PIC X(09)   VALUE   "114025610".
      *    特医総管（在支診等）（処方せんあり・同一建物居住者以外）
           03  FILLER          PIC X(09)   VALUE   "114025710".
      *    特医総管（在支診等）（処方せんあり・同一建物居住者）
           03  FILLER          PIC X(09)   VALUE   "114025810".
      *    特医総管（在支診等）（処方せんなし・同一建物居住者以外）
           03  FILLER          PIC X(09)   VALUE   "114025910".
      *    特医総管（在支診等）（処方せんなし・同一建物居住者）
           03  FILLER          PIC X(09)   VALUE   "114026010".
      *    特医総管（在支診等以外）（処方せんあり・同一建物居住者以外）
           03  FILLER          PIC X(09)   VALUE   "114026110".
      *    特医総管（在支診等以外）（処方せんあり・同一建物居住者）
           03  FILLER          PIC X(09)   VALUE   "114026210".
      *    特医総管（在支診等以外）（処方せんなし・同一建物居住者以外）
           03  FILLER          PIC X(09)   VALUE   "114026310".
      *    特医総管（在支診等以外）（処方せんなし・同一建物居住者）
           03  FILLER          PIC X(09)   VALUE   "114026410".
      *
       01  TBL-ZAITAKU5-R      REDEFINES   TBL-ZAITAKU5-AREA.
            03  TBL-ZAITAKU5   PIC X(09)   OCCURS  39.
      *
       01  TBL-ZAITAKU5-MAX    PIC 9(02)   VALUE   39.
      *
      *    在総診テーブル（２０１６年４月診療分〜）
       01  TBL-ZAITAKU6-AREA.
      *    在がん医総（在支診等）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114007610".
      *    在がん医総（在支診等）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114007710".
      *    在医総・在医総管（ＤＵＭＭＹ）
           03  FILLER          PIC X(09)   VALUE   "099140003".
      *
      *    在がん医総（機能強化した在支診等）（病床あり）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114019510".
      *    在がん医総（機能強化した在支診等）（病床あり）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114019610".
      *    在がん医総（機能強化した在支診等）（病床なし）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114019710".
      *    在がん医総（機能強化した在支診等）（病床なし）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114019810".
      *
      *    在医総管（機能強化在支診等・病床有・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114030710".
      *    在医総管（機能強化在支診等・病床有・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114030810".
      *    在医総管（機能強化在支診等・病床有・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114030910".
      *    在医総管（機能強化在支診等・病床有・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114031010".
      *    在医総管（機能強化在支診等・病床有・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114031110".
      *    在医総管（機能強化在支診等・病床有・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114031210".
      *    在医総管（機能強化在支診等・病床有・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114031310".
      *    在医総管（機能強化在支診等・病床有・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114031410".
      *    在医総管（機能強化在支診等・病床有・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114031510".
      *    在医総管（機能強化在支診等・病床無・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114031610".
      *    在医総管（機能強化在支診等・病床無・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114031710".
      *    在医総管（機能強化在支診等・病床無・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114031810".
      *    在医総管（機能強化在支診等・病床無・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114031910".
      *    在医総管（機能強化在支診等・病床無・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114032010".
      *    在医総管（機能強化在支診等・病床無・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114032110".
      *    在医総管（機能強化在支診等・病床無・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114032210".
      *    在医総管（機能強化在支診等・病床無・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114032310".
      *    在医総管（機能強化在支診等・病床無・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114032410".
      *    在医総管（在支診等・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114032510".
      *    在医総管（在支診等・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114032610".
      *    在医総管（在支診等・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114032710".
      *    在医総管（在支診等・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114032810".
      *    在医総管（在支診等・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114032910".
      *    在医総管（在支診等・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114033010".
      *    在医総管（在支診等・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114033110".
      *    在医総管（在支診等・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114033210".
      *    在医総管（在支診等・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114033310".
      *    在医総管（在支診等以外・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114033410".
      *    在医総管（在支診等以外・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114033510".
      *    在医総管（在支診等以外・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114033610".
      *    在医総管（在支診等以外・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114033710".
      *    在医総管（在支診等以外・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114033810".
      *    在医総管（在支診等以外・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114033910".
      *    在医総管（在支診等以外・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114034010".
      *    在医総管（在支診等以外・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114034110".
      *    在医総管（在支診等以外・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114034210".
      *
      *    施医総管（機能強化在支診等・病床有・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114035510".
      *    施医総管（機能強化在支診等・病床有・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114035610".
      *    施医総管（機能強化在支診等・病床有・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114035710".
      *    施医総管（機能強化在支診等・病床有・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114035810".
      *    施医総管（機能強化在支診等・病床有・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114035910".
      *    施医総管（機能強化在支診等・病床有・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114036010".
      *    施医総管（機能強化在支診等・病床有・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114036110".
      *    施医総管（機能強化在支診等・病床有・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114036210".
      *    施医総管（機能強化在支診等・病床有・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114036310".
      *    施医総管（機能強化在支診等・病床無・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114036410".
      *    施医総管（機能強化在支診等・病床無・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114036510".
      *    施医総管（機能強化在支診等・病床無・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114036610".
      *    施医総管（機能強化在支診等・病床無・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114036710".
      *    施医総管（機能強化在支診等・病床無・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114036810".
      *    施医総管（機能強化在支診等・病床無・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114036910".
      *    施医総管（機能強化在支診等・病床無・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114037010".
      *    施医総管（機能強化在支診等・病床無・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114037110".
      *    施医総管（機能強化在支診等・病床無・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114037210".
      *    施医総管（在支診等・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114037310".
      *    施医総管（在支診等・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114037410".
      *    施医総管（在支診等・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114037510".
      *    施医総管（在支診等・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114037610".
      *    施医総管（在支診等・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114037710".
      *    施医総管（在支診等・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114037810".
      *    施医総管（在支診等・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114037910".
      *    施医総管（在支診等・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114038010".
      *    施医総管（在支診等・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114038110".
      *    施医総管（在支診等以外・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114038210".
      *    施医総管（在支診等以外・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114038310".
      *    施医総管（在支診等以外・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114038410".
      *    施医総管（在支診等以外・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114038510".
      *    施医総管（在支診等以外・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114038610".
      *    施医総管（在支診等以外・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114038710".
      *    施医総管（在支診等以外・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114038810".
      *    施医総管（在支診等以外・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114038910".
      *    施医総管（在支診等以外・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114039010".
      *
       01  TBL-ZAITAKU6-R      REDEFINES   TBL-ZAITAKU6-AREA.
            03  TBL-ZAITAKU6   PIC X(09)   OCCURS  79.
      *
       01  TBL-ZAITAKU6-MAX    PIC 9(02)   VALUE   79.
      *
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *    レセ電診療科コードテーブル
           COPY    "CMRECEDENSRYKACD.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *    
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK2005.INC".
           COPY    "CPSK200501.INC"
                                   REPLACING  //SYS-200501//
                                   BY         //SYS-25//.
      *
           COPY    "CPSK2009.INC".
      *
      *    月代わり受給者番号
       01  MONTHLYNUM-REC.
           COPY    "CPMONTHLYNUM.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    旧姓履歴情報
       01  KYUSEIRRK-REC.
           COPY    "CPKYUSEIRRK.INC".
      *
      *    患者特記事項情報
       01  PTTOKKI-REC.
           COPY    "CPPTTOKKI.INC".
      *
      *    労災患者保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    主科取得サブ
       01  ORCSSYUACCAREA.
           COPY    "CPORCSSYUACC.INC".
      *
      *    レセプト種別返却サブ
           COPY    "CPORCSSYUBETUGET.INC".
      *
      *    患者所得情報編集サブ
           COPY    "CPORCSSYOTOKU.INC".
      *
      *    医師コード取得サブ
       01  ORCSDRCDGETAREA.
           COPY    "CPORCSDRCDGET.INC".
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1 
      *
           PERFORM 300-TERM-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       SPA-AREA
                                       WRK-AREA
                                       WRK-HKNNUM-AREA
                                       TABLE-HKNNUM-AREA  
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           MOVE    ZERO                TO  FLG-END
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *    ＤＢオープン処理
               PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0020"      TO  JOB-PGID
               MOVE    "保険情報編集"  TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *    処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *         
      *    レセプト編集区分情報
           IF      FLG-END             =   ZERO
               MOVE    SPACE           TO  SYSKANRI-REC
               INITIALIZE                  SYSKANRI-REC
               MOVE    "2009"          TO  SYS-KANRICD
               MOVE    "*"             TO  SYS-KBNCD
               MOVE    WRK-PARA-SRYYM  TO  SYS-STYUKYMD (1:6)
               MOVE    "01"            TO  SYS-STYUKYMD (7:2)
               MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
               MOVE    "tbl_syskanri"  TO  WRK-MCP-TABLE
               MOVE    "key10"         TO  WRK-MCP-PATHNAME
               PERFORM 960-KANRIMST-READ-SEC
               IF      FLG-SYSKANRI    =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-2009-REC
               ELSE    
                   INITIALIZE                  SYS-2009-REC
               END-IF
               IF      SYS-2009-SYUKA-KBN
                                       =   SPACE
                   MOVE    "0"             TO  SYS-2009-SYUKA-KBN
               END-IF
               IF    ( SYS-2009-SYUKA-KBN
                                       =   "1"   )
               AND   ( SYS-2009-SYUKA-SYAHO
                                       =   SPACE )
                   MOVE    "1"             TO  SYS-2009-SYUKA-SYAHO
               END-IF      
               IF    ( SYS-2009-SYUKA-KBN
                                       =   "1"   )
               AND   ( SYS-2009-SYUKA-KOKUHO
                                       =   SPACE )
                   MOVE    "0"             TO  SYS-2009-SYUKA-KOKUHO
               END-IF
               IF    ( SYS-2009-SYUKA-KBN
                                       =   "1"   )
               AND   ( SYS-2009-SYUKA-KOUIKI
                                       =   SPACE )
                   MOVE    "0"             TO  SYS-2009-SYUKA-KOUIKI
               END-IF
      *
               DISPLAY "SYS-2009-SYUKA-KBN   =" SYS-2009-SYUKA-KBN
               DISPLAY "SYS-2009-SYUKA-SYAHO =" SYS-2009-SYUKA-SYAHO
               DISPLAY "SYS-2009-SYUKA-KOKUHO=" SYS-2009-SYUKA-KOKUHO
               DISPLAY "SYS-2009-SYUKA-KOUIKI=" SYS-2009-SYUKA-KOUIKI
      *         
      *        レセプト編集情報
               MOVE    SPACE           TO  SYSKANRI-REC
               INITIALIZE                  SYSKANRI-REC
               MOVE    "2005"          TO  SYS-KANRICD
               MOVE    "*"             TO  SYS-KBNCD
               MOVE    WRK-PARA-SRYYM  TO  SYS-STYUKYMD (1:6)
               MOVE    "01"            TO  SYS-STYUKYMD (7:2)
               MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
               MOVE    "tbl_syskanri"  TO  WRK-MCP-TABLE
               MOVE    "key10"         TO  WRK-MCP-PATHNAME
               PERFORM 960-KANRIMST-READ-SEC
               IF      FLG-SYSKANRI    =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-2005-REC
               ELSE    
                   INITIALIZE                  SYS-2005-REC
               END-IF
               IF      SYS-2005-NYUTAIINREKI-KSIKBN
                                       =   SPACE
                   MOVE    "0"         TO  SYS-2005-NYUTAIINREKI-KSIKBN
               END-IF
      *
               MOVE    SPACE           TO  SYSKANRI-REC
               INITIALIZE                  SYSKANRI-REC
               MOVE    "2005"          TO  SYS-KANRICD
               MOVE    "01"            TO  SYS-KBNCD
               MOVE    WRK-PARA-SRYYM  TO  SYS-STYUKYMD (1:6)
               MOVE    "01"            TO  SYS-STYUKYMD (7:2)
               MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
               MOVE    "tbl_syskanri"  TO  WRK-MCP-TABLE
               MOVE    "key10"         TO  WRK-MCP-PATHNAME
               PERFORM 960-KANRIMST-READ-SEC
               IF      FLG-SYSKANRI    =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-25-REC
               ELSE    
                   INITIALIZE                  SYS-25-REC
               END-IF
               IF      SYS-25-DRCD-PRTKBN
                                       =   SPACE
                   MOVE    "0"         TO  SYS-25-DRCD-PRTKBN
               END-IF
      *
               DISPLAY "SYS-2005-NYUTAIINREKI-KSIKBN=" 
                                   SYS-2005-NYUTAIINREKI-KSIKBN " "
               DISPLAY "SYS-200501-DRCD-PRTKBN      =" 
                                   SYS-25-DRCD-PRTKBN " "
          END-IF
      *
          .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "RECE090"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (1)
           MOVE    "RECE110"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (2)
           MOVE    "RECE120"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (3)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECE09PARA
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE11PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE12PARA
      *
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       2
                   OR      FLG-END     =   1
      *
               IF      IDZ             =   1               
      *    レセプト処理
                   INITIALIZE                      SGETTEMP-AREA
                   MOVE    "RECE010"           TO  RECEDAT1PARA-FILE-ID
                   PERFORM 800-FILENM-SET-SEC
                   MOVE    RECEDAT1PARA-BASENAME
                                               TO  SGETTEMP-BASENAMES(1)
                   MOVE    "RECE020"           TO  RECEDAT1PARA-FILE-ID
                   PERFORM 800-FILENM-SET-SEC
                   MOVE    RECEDAT1PARA-BASENAME
                                               TO  SGETTEMP-BASENAMES(2)
               ELSE
      *    請求書処理
                   INITIALIZE                      SGETTEMP-AREA
                   MOVE    "RECE01X"           TO  RECEDAT1PARA-FILE-ID
                   PERFORM 800-FILENM-SET-SEC
                   MOVE    RECEDAT1PARA-BASENAME
                                               TO  SGETTEMP-BASENAMES(1)
                   MOVE    "RECE02X"           TO  RECEDAT1PARA-FILE-ID
                   PERFORM 800-FILENM-SET-SEC
                   IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
                       MOVE    WRK-CONS-TERMID     TO
                                                   RECEDAT1PARA-TERMID
                   END-IF
                   MOVE    RECEDAT1PARA-BASENAME
                                               TO  SGETTEMP-BASENAMES(2)
               END-IF
               CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
               MOVE    SGETTEMP-FULLNAMES (1)
                                           TO  RECE01PARA
               MOVE    SGETTEMP-FULLNAMES (2)
                                           TO  RECE02PARA
      *
               PERFORM 200-HENSYU-SEC
      *        
               IF      WRK-PARA-SHELLID    =   "RECEPTX"
                   MOVE    1                   TO  FLG-END
               END-IF
           END-PERFORM 
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-HENSYU-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-RECE01
           MOVE    LOW-VALUE           TO  KEY-AREA1
      *
           OPEN    OUTPUT              RECE02-FILE
           CLOSE                       RECE02-FILE
      *
           IF      IDZ             =   1               
               OPEN    OUTPUT              RECE09-FILE
               CLOSE                       RECE09-FILE
               OPEN    I-O                 RECE09-FILE
      *
               OPEN    OUTPUT              RECE12-FILE
               CLOSE                       RECE12-FILE
               OPEN    I-O                 RECE12-FILE
      *
               OPEN    OUTPUT              RECE11-FILE
               CLOSE                       RECE11-FILE
      *
           END-IF    
      *
           OPEN    INPUT               RECE01-FILE
           IF      STS-RECE01          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE01PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE01          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    I-O                 RECE02-FILE
      *
           PERFORM     RECE01-READ-SEC
      *
           PERFORM                     UNTIL   FLG-RECE01  =   1
                                       OR      FLG-END     =   1
      *
               MOVE    ZERO            TO  FLG-SEIHO
               INITIALIZE                  WRK-SEIHO-RECE01-AREA
                                           TABLE-RECE09-AREA
      *
               MOVE    KEY-NEW11       TO  KEY-OLD11
               PERFORM         UNTIL   FLG-RECE01      =   1
                               OR      FLG-END         =   1 
                               OR      KEY-NEW11   NOT =   KEY-OLD11
                   MOVE    ZERO            TO  FLG-SEIHO
                   INITIALIZE                  WRK-SEIHO-RECE01-AREA
      *
                   MOVE    KEY-NEW1        TO  KEY-OLD1
                   PERFORM         UNTIL   FLG-RECE01      =   1
                                   OR      FLG-END         =   1 
                                   OR      KEY-NEW1    NOT =   KEY-OLD1
                       IF      IDZ             =   1
      *                    保険履歴情報編集処理
                           PERFORM 230-RECE09-HENSYU-SEC
      *                    患者特記情報取得処理
                           PERFORM 230-PTTOKKI-HENSYU-SEC
                       END-IF
      *
      *                編集処理
      *                処理区分（社保・国保）により読み飛ばす
                       EVALUATE    WRK-PARA-RECEKBN
                           WHEN    "0"
                               PERFORM 210-HENSYU-SEC
                           WHEN    "1"
                               IF      RECE01-HKNNUM   NOT =
                                                       "060" AND "067"
                                                   AND "068" AND "069"
                                                   AND "039" AND "040"
                                   PERFORM 210-HENSYU-SEC
                               END-IF
                           WHEN    "2"
                               IF      RECE01-HKNNUM       =
                                                       "060" OR  "067"
                                                   OR  "068" OR  "069"
                                   PERFORM 210-HENSYU-SEC
                               END-IF
                           WHEN    "7"
                               IF      RECE01-HKNNUM       =
                                                       "039" OR  "040"
                                   PERFORM 210-HENSYU-SEC
                               END-IF
                       END-EVALUATE
      *
                       PERFORM     RECE01-READ-SEC
      *
      *                処理中止設定処理
                       IF      CNT-IN-CANCEL       =   50
                           MOVE    ZERO                TO  CNT-IN-CANCEL
                           PERFORM 500-CANCEL-HENSYU-SEC
                       END-IF
                   END-PERFORM
      *
                   IF      IDZ             =   1               
      *                生保単独レセの出力処理
                       IF    ( WRK-SEIHO-RECE01-MAX    >   ZERO )
                       AND   ( FLG-END                 =   ZERO )
                           IF      FLG-RECE01          =   ZERO
                               MOVE    RECE01-REC  TO  WRK-RECE01-REC
                           END-IF
                           PERFORM 250-SEIHO-RECE01-WRITE-SEC
                           IF      FLG-RECE01          =   ZERO
                               MOVE    WRK-RECE01-REC  TO  RECE01-REC
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
      *
               IF      IDZ             =   1               
      *            保険履歴情報編集処理
                   PERFORM VARYING IDYY    FROM    1   BY  1
                           UNTIL   IDYY    >       100
                           OR      TBL-RECE09-HOSPNUM (IDYY)  =   ZERO
                       MOVE    TBL-RECE09-OCC (IDYY)   TO  RECE09-REC
                       WRITE   RECE09-REC
      *
                       IF      STS-RECE09      =   "00"
                           ADD     1               TO  CNT-RECE09
                       ELSE
                           MOVE    SPACE           TO  WRK-ERR-AREA
                           INITIALIZE                  WRK-ERR-AREA
                           MOVE    3               TO  WRK-ERR-FLG
                           MOVE    RECE09PARA      TO  WRK-ERR-FILE-NM
                           MOVE    STS-RECE09      TO  WRK-ERR-FILE-STS
      ****                 PERFORM 500-FILE-ERR-ABORT-SEC
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           CLOSE   RECE01-FILE
                   RECE02-FILE
      *
           IF      IDZ             =   1               
               CLOSE   RECE09-FILE
                       RECE12-FILE
           END-IF    
           .
       200-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    編集処理
      *****************************************************************
       210-HENSYU-SEC                   SECTION.
      *
           MOVE    ZERO                TO  WRK-SEIHO-FLG
      *
      *    公費単独の場合（生保、中国残留邦人等、自立支援）
           IF      RECE01-HKNNUM        =   SPACE
      *******IF      RECE01-KOHHKNNUM-O (1)  =   "030" OR  "053" 
             IF      RECE01-KOHHKNNUM-O (1)  =   "030"
                 CONTINUE
             ELSE 
               IF      RECE01-TEKSTYMD      =   ZERO
                   IF    ( RECE01-KOHHKNNUM-O (1)  =   "012" OR  "025"
                                                   OR  "015" OR  "016"
                                                   OR  "021" OR  SPACE )
      *                同じ公費の公費単独の場合
                       IF    ( RECE01-KOHHKNNUM-O  (2)
                                           =   RECE01-KOHHKNNUM-O (1) 
                                           OR  SPACE                  )
                       AND   ( RECE01-KOHHKNNUM-O  (3)
                                           =   RECE01-KOHHKNNUM-O (1) 
                                           OR  SPACE                  )
                       AND   ( RECE01-KOHHKNNUM-O  (4)
                                           =   RECE01-KOHHKNNUM-O (1) 
                                           OR  SPACE                  )
                       AND   ( RECE01-KOHHKNNUM-O  (5)
                                           =   RECE01-KOHHKNNUM-O (1) 
                                           OR  SPACE                  )
                       AND   ( RECE01-KOHHKNNUM-O  (6)
                                           =   RECE01-KOHHKNNUM-O (1) 
                                           OR  SPACE                  )
                       AND   ( RECE01-KOHHKNNUM-O  (7)
                                           =   RECE01-KOHHKNNUM-O (1) 
                                           OR  SPACE                  )
                       AND   ( RECE01-KOHHKNNUM-O  (8)
                                           =   RECE01-KOHHKNNUM-O (1) 
                                           OR  SPACE                  )
                       AND   ( RECE01-KOHHKNNUM-O  (9)
                                           =   RECE01-KOHHKNNUM-O (1) 
                                           OR  SPACE                  )
                       AND   ( RECE01-KOHHKNNUM-O  (10)
                                           =   RECE01-KOHHKNNUM-O (1) 
                                           OR  SPACE                  )
                           GO  TO  210-HENSYU-EXT
                       ELSE
      *********************IF    ( RECE01-KOHHKNNUM-O  (2)
      *********************                    =   "012"   OR  "025"
      *********************                    OR  "015"   OR  "016"
      *********************                    OR  "021"   OR  SPACE )
      *********************AND   ( RECE01-KOHHKNNUM-O  (3)
      *********************                    =   "012"   OR  "025"
      *********************                    OR  "015"   OR  "016"
      *********************                    OR  "021"   OR  SPACE )
      *********************AND   ( RECE01-KOHHKNNUM-O  (4)
      *********************                    =   "012"   OR  "025"
      *********************                    OR  "015"   OR  "016"
      *********************                    OR  "021"   OR  SPACE )
      *********************    生保、中国残留邦人等、自立支援のみの
      *********************    公費単独の場合退避
      *********************    MOVE    RECE01-REC      TO
      *********************                        WRK-KOH-RECE01-REC
      *********************    GO  TO  210-HENSYU-EXT
      *********************ELSE
                               MOVE    1               TO  FLG-SEIHO
      *********************END-IF
                       END-IF
                   ELSE
                       MOVE    1               TO  FLG-SEIHO
                   END-IF
               ELSE
                   IF    ( FLG-SEIHO            =   1     )
                   AND   ( RECE01-KOHHKNNUM-O (1)
                                            NOT =   "053" )
                       GO  TO  210-HENSYU-EXT
                   ELSE
      *                公費単独の場合退避
                       ADD     1                TO  WRK-SEIHO-RECE01-MAX
                       MOVE    RECE01-REC       TO  WRK-SEIHO-RECE01-REC
                                                  (WRK-SEIHO-RECE01-MAX)
                       GO  TO  210-HENSYU-EXT
                   END-IF
               END-IF
             END-IF
           END-IF
      *
      *    社保で公費併用の場合（生保、中国残留邦人等、自立支援、児童保護）
           IF    ( RECE01-HKNNUM   NOT =   SPACE )
           AND   ( RECE01-KOHHKNNUM-O (1)
                                       =   "012"   OR  "025"   OR 
                                           "015"   OR  "016"   OR
                                           "021"   OR  "053"      )
           AND   ( RECE01-KOHHKNNUM-O (2)
                                       =   SPACE   OR  "972"   OR
                                           "974"                  )
               IF      RECE01-HKNNUM       =   "060"   OR  "067"   OR
                                               "068"   OR  "069"   OR
                                               "039"   OR  "040"
                   CONTINUE
               ELSE
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       WRK-SEIHO-RECE01-MAX
      *              分割区分毎にまとめる
                     IF      RECE01-RECEIPT-KBN    =
                                   WRK-SEH-01-RECEIPT-KBN (IDXX)
                       PERFORM VARYING IDYY    FROM    1   BY  1
                               UNTIL   IDYY    >       10
                               OR      RECE01-KOHID-O (IDYY)
                                               =       ZERO
                           IF      WRK-SEH-01-KOHID-O (IDXX 1)
                                       =   RECE01-KOHID-O (IDYY)
                               MOVE    1           TO
                                           WRK-SEIHO-RECE01-PRTFLG(IDXX)
                               MOVE    1           TO  WRK-SEIHO-FLG
                               PERFORM VARYING IDZZ    FROM    1   BY  1
                                       UNTIL   IDZZ    >       10
                                   IF      RECE01-HKNCOMBI-O (IDZZ)
                                               =   ZERO
                                       MOVE
                                           WRK-SEH-01-HKNCOMBI-TBL
                                                              (IDXX 1)
                                                           TO
                                               RECE01-HKNCOMBI-TBL(IDZZ)
                                   END-IF
                                   IF      RECE01-HKNCOMBI-O (IDZZ)
                                               =
                                             WRK-SEH-01-HKNCOMBI-O
                                                              (IDXX 1)
                                       MOVE    10          TO  IDZZ
                                   END-IF
                               END-PERFORM    
                               PERFORM VARYING IDVV    FROM    1   BY  1
                                       UNTIL   IDVV    >       20
                                       OR      WRK-SEH-01-SRYKA
                                                          (IDXX IDVV)
                                                       =   SPACE
                                   PERFORM VARYING IDZZ    FROM    1
                                                                   BY  1
                                           UNTIL   IDZZ    >       20
                                       IF      RECE01-SRYKA (IDZZ)
                                                   =   SPACE
                                           MOVE
                                               WRK-SEH-01-SRYKA
                                                             (IDXX IDVV)
                                                           TO
                                                   RECE01-SRYKA (IDZZ)
                                       END-IF
                                       IF      RECE01-SRYKA (IDZZ)
                                                   =
                                             WRK-SEH-01-SRYKA
                                                             (IDXX IDVV)
                                           MOVE    20          TO  IDZZ
                                       END-IF
                                   END-PERFORM
                               END-PERFORM
                               MOVE    10          TO  IDYY
                               MOVE    WRK-SEIHO-RECE01-MAX
                                                   TO  IDXX
                           END-IF
                       END-PERFORM
                     END-IF
                   END-PERFORM
               END-IF                     
           END-IF
      *
      *    ファイル編集処理
           PERFORM 2101-RECE02-HENSYU-SEC
           .
       210-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ファイル編集処理
      *****************************************************************
       2101-RECE02-HENSYU-SEC           SECTION.
      *
           MOVE    SPACE                TO  WK-RECESYUBETU-AREA
           INITIALIZE                       WK-RECESYUBETU-AREA
      *     
           MOVE    ZERO                 TO  TOKKI-IDX
      *
           MOVE    SPACE                TO  RECE02-REC
           INITIALIZE                       RECE02-REC
      *
           MOVE    SPACE                TO  RJN-RATE-KBN
      *
           MOVE    RECE01-HOSPNUM       TO  RECE02-HOSPNUM
           MOVE    RECE01-SRYYM         TO  RECE02-SRYYM
           MOVE    RECE01-NYUGAIKBN     TO  RECE02-NYUGAIKBN
           MOVE    RECE01-PTID          TO  RECE02-PTID
           MOVE    RECE01-RECEKA        TO  RECE02-RECEKA
           IF      RECE01-HKNNUM (1:2)  =   "98"    OR  "90"    OR
                                            "91"
               MOVE    RECE01-HKNNUM        TO  RECE02-HKNJANUM-KEY
           ELSE  
               MOVE    RECE01-HKNJANUM      TO  RECE02-HKNJANUM-KEY
           END-IF
           MOVE    RECE01-HOJOKBN       TO  RECE02-HOJOKBN-KEY
           IF    ( RECE01-HKNNUM        =   SPACE            )
           AND   ( RECE01-KOHHKNNUM-O (1)
                                        =   "030"  OR  "053" )
               MOVE    RECE01-KOH-KOHID TO  RECE02-KOHID-KEY
           END-IF
           IF    ( RECE01-HKNNUM        =   SPACE            )
           AND   ( RECE01-KOHHKNNUM-O (1)
                                        =   "012"            )
               MOVE    RECE01-KOH-KOHID TO  RECE02-KOHID-KEY
           END-IF
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
                   OR      RECE01-TEKSTYMDX (IDX)  =   SPACE
               IF      RECE02-TEKSTYMD     =   SPACE 
                   MOVE    RECE01-TEKSTYMDX (IDX)  TO  RECE02-TEKSTYMD
               ELSE
                   IF      RECE02-TEKSTYMD <   RECE01-TEKSTYMDX (IDX)
                       MOVE    RECE01-TEKSTYMDX (IDX)  
                                               TO  RECE02-TEKSTYMD
                   END-IF
               END-IF
               IF      RECE02-TEKSTYMD-X   =   SPACE 
                   MOVE    RECE01-TEKSTYMDX (IDX)  TO  RECE02-TEKSTYMD-X
                   MOVE    RECE01-TEKEDYMDX (IDX)  TO  RECE02-TEKEDYMD-X
               ELSE
                   IF      RECE02-TEKSTYMD-X >   RECE01-TEKSTYMDX (IDX)
                       MOVE    RECE01-TEKSTYMDX (IDX)  
                                               TO  RECE02-TEKSTYMD-X
                   END-IF
                   IF      RECE02-TEKEDYMD-X <   RECE01-TEKEDYMDX (IDX)
                       MOVE    RECE01-TEKEDYMDX (IDX)  
                                               TO  RECE02-TEKEDYMD-X
                   END-IF
               END-IF
           END-PERFORM
           MOVE    RECE01-RECEIPT-KBN  TO  RECE02-RECEIPT-KBN
      *    患者番号
           MOVE    RECE01-PTNUM        TO  RECE02-PTNUM
      *    診療科情報のセット
           PERFORM   VARYING   IDX   FROM   1   BY   1
                     UNTIL    (IDX      >   20)    OR
                              (RECE01-SRYKA(IDX)  =   SPACE)
               MOVE  RECE01-SRYKA(IDX)    TO   RECE02-SRYKA(IDX)
           END-PERFORM
      *    審査支払機関
           IF      RECE01-HKNNUM        =   "060"   OR    "067"   OR
                                            "068"   OR    "069"
               MOVE    2                TO  RECE02-TEISYUTUSAKI
           ELSE
               IF      RECE01-HKNNUM        =   "039"   OR    "040"
                   MOVE    6                TO  RECE02-TEISYUTUSAKI
               ELSE
                   MOVE    1                TO  RECE02-TEISYUTUSAKI
               END-IF
           END-IF
      *    患者情報の取得
           MOVE    RECE01-KANANAME     TO  RECE02-KANANAME
           MOVE    RECE01-NAME         TO  RECE02-NAME
           MOVE    RECE01-SEX          TO  RECE02-SEX
           MOVE    RECE01-BIRTHDAY     TO  RECE02-BIRTHDAY
           MOVE    RECE01-DEATHKBN     TO  RECE02-DEATHKBN
           MOVE    RECE01-TSTPTNUMKBN  TO  RECE02-TSTPTNUMKBN
           MOVE    RECE01-AGE          TO  WRK-NENREI
                                           RECE02-AGE
           MOVE    RECE01-SYUGAKUKBN   TO  RECE02-SYUGAKUKBN
      *
           MOVE    RECE01-HOME-POST    TO  RECE02-HOME-POST
           MOVE    RECE01-HOME-ADRS    TO  RECE02-HOME-ADRS
           MOVE    RECE01-HOME-BANTI   TO  RECE02-HOME-BANTI
           MOVE    RECE01-RENRAKU-POST TO  RECE02-RENRAKU-POST
           MOVE    RECE01-RENRAKU-NAME TO  RECE02-RENRAKU-NAME
           MOVE    RECE01-RENRAKU-ADRS TO  RECE02-RENRAKU-ADRS
           MOVE    RECE01-RENRAKU-BANTI
                                       TO  RECE02-RENRAKU-BANTI
           MOVE    RECE01-RENRAKU-TEL1 TO  RECE02-RENRAKU-TEL1
           MOVE    RECE01-RENRAKU-TEL2 TO  RECE02-RENRAKU-TEL2
      *    旧姓履歴の取得（月途中で保険変更があるとき）
           IF    ( RECE01-TEISEIKBN    NOT =   "1"   )
           AND   ( RECE02-TEKSTYMD-X   NOT =   SPACE )
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE    RECE02-SRYYM        TO  LNK-DAY7-YM
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
               IF    ( RECE02-TEKEDYMD-X   <   LNK-DAY7-KOYOMI )
                   INITIALIZE                      KYUSEIRRK-REC
grpsys             MOVE    RECE02-HOSPNUM         TO  KYUSEI-HOSPNUM
                   MOVE    RECE02-PTID         TO  KYUSEI-PTID
                   MOVE    RECE02-TEKEDYMD-X   TO  KYUSEI-CHGYMD
                   MOVE    KYUSEIRRK-REC       TO  MCPDATA-REC
                   PERFORM 900-KYUSEIRRK-READ-SEC
                   IF       FLG-KYUSEIRRK      =   ZERO
                       MOVE    KYUSEI-NAME         TO  RECE02-NAME
                       MOVE    KYUSEI-KANANAME     TO  RECE02-KANANAME
                   END-IF
               END-IF
           END-IF
      *    県番号
           MOVE    RECE01-PREFNUM      TO  RECE02-PREFNUM
      *    患者保険情報テーブルから主保険情報の取得
           INITIALIZE                      WRK-HKNNUM-REC
      *
           IF      RECE01-HKNID        NOT =   ZERO
               MOVE    RECE01-HKNNUM   TO  RECE02-HKNNUM
               MOVE    RECE01-HKNID    TO  RECE02-HKNID
      *        法別番号のセットと給付割合情報の取得
      *        （退職特別療養費の場合は退職国保から取得する）
               INITIALIZE                  HKNNUM-REC
grpsys         MOVE   RECE02-HOSPNUM   TO  HKN-HOSPNUM
               IF     RECE02-HKNNUM    =   "069"
                   MOVE    "067"           TO  HKN-HKNNUM
               ELSE
                   MOVE   RECE02-HKNNUM    TO  HKN-HKNNUM
               END-IF
               PERFORM 900-HKNNUM-CHK-SEC
               IF      FLG-HKNNUM      =   ZERO
                   MOVE    HKNNUM-REC      TO  WRK-HKNNUM-REC
                   MOVE    HKN-HBTNUM      TO  RECE02-HKNHBTNUM
                   MOVE    SPACE           TO  WRK-JIHI-FLG
                   IF      HKN-HKNKOHSBTKBN    =   "8"
                       MOVE   "1"              TO  WRK-JIHI-FLG
                   END-IF
                   IF      HKN-HKNKOHSBTKBN    =   "9"
                       MOVE   "2"              TO  WRK-JIHI-FLG
                   END-IF
               END-IF
               MOVE    RECE01-HKN-HKNJANUM  TO  RECE02-HKNJANUM
               MOVE    RECE01-HKN-HONKZKKBN TO  RECE02-HONKZKKBN
                                                WK-HONKZKKBN
               MOVE    RECE01-HKN-HOJOKBN   TO  RECE02-HOJOKBN
               MOVE    RECE01-CONTKBN       TO  RECE02-CONTKBN
               IF      RECE02-HKNNUM    NOT  =   "039"
                   MOVE    RECE01-KIGO          TO  RECE02-KIGO
               END-IF
               MOVE    RECE01-NUM           TO  RECE02-NUM
               MOVE    RECE01-CERTNUM       TO  RECE02-CERTNUM
           END-IF
      *    患者公費情報テーブルから老人保険情報の取得
           IF      RECE01-SRYYM            <   WRK-CONS-SRYYM-200804
             IF    ( RECE01-KOHHKNNUM-O (1)  =   "027" )
             OR    ( RECE01-KOHID            >   ZERO  )
               INITIALIZE                      PTKOHINF-REC
grpsys         MOVE    RECE01-HOSPNUM      TO  PTKOH-HOSPNUM
               MOVE    RECE01-PTID         TO  PTKOH-PTID
               MOVE    RECE01-KOHID        TO  PTKOH-KOHID
               MOVE    PTKOHINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM PTKOHINF-READ-SEC
                   IF      FLG-PTKOHINF        =   ZERO
                       MOVE    PTKOH-KOHID         TO  RECE02-RJNHKNID
                       MOVE    PTKOH-KOHNUM        TO  RECE02-RJNHKNNUM
                       IF    ( PTKOH-TEKSTYMD (1:6)
                                                   =   RECE01-SRYYM    )
                       AND   ( PTKOH-TEKSTYMD      >   RECE02-TEKSTYMD )
                           MOVE    PTKOH-TEKSTYMD  TO  RECE02-TEKSTYMD
                       END-IF
      *                法別番号のセット
                       INITIALIZE                  HKNNUM-REC
grpsys                 MOVE   RECE01-HOSPNUM   TO  HKN-HOSPNUM
                       MOVE   PTKOH-KOHNUM     TO  HKN-HKNNUM
                       PERFORM 900-HKNNUM-CHK-SEC
                       IF       FLG-HKNNUM     =   ZERO
                           MOVE   HKN-HBTNUM TO RECE02-RJNHBTNUM
                       END-IF
                       MOVE    PTKOH-FTNJANUM   TO  RECE02-RJNFTNJANUM
                       MOVE    PTKOH-JKYSNUM    TO  RECE02-RJNJKYSNUM
                       MOVE    PTKOH-PAYKBN     TO  RECE02-RJNPAYKBN
      *----(01.00.14) LINE ADD START --------------------------
      *                平成１４年１０月対応
      *                老人保険の患者について負担割合を退避
                       IF   RECE02-SRYYM   >=   "200210"
                            EVALUATE    PTKOH-PAYKBN
                                WHEN    "01"
                                    MOVE      "1"   TO  RJN-RATE-KBN
                                WHEN    "02"
                                    MOVE      "2"   TO  RJN-RATE-KBN
                                WHEN    "03"
                                    MOVE      "3"   TO  RJN-RATE-KBN
                            END-EVALUATE
                       END-IF
      *----(01.00.14) LINE ADD END   --------------------------
                   ELSE
                        DISPLAY  "PTKOHINF FETCH ERR" 
                   END-IF
               ELSE
                   DISPLAY   "PTKOHINF SELECT ERR" 
               END-IF
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
             END-IF
           END-IF  
      *
      *    長期高額療養費の編集
           MOVE    SPACE              TO   WRK-CHOKI
           MOVE    SPACE              TO   WRK-CHOKI2
           PERFORM   VARYING   IDX   FROM    1   BY   1
                     UNTIL    (IDX     >   10)    
                     OR       (RECE01-KOHHKNNUM-O(IDX)  =   SPACE)
               IF    RECE01-KOHHKNNUM-O(IDX)     =    "027"
                   CONTINUE
               ELSE
                 INITIALIZE                        HKNNUM-REC
grpsys           MOVE    RECE01-HOSPNUM        TO  HKN-HOSPNUM
                 MOVE    RECE01-KOHHKNNUM-O(IDX) TO  HKN-HKNNUM
                 MOVE    "00"                  TO  HKN-PAYKBN
                 MOVE    RECE01-SRYYM          TO  HKN-TEKSTYMD(1:6)
                 MOVE    "01"                  TO  HKN-TEKSTYMD(7:2)
                 MOVE    RECE01-SRYYM          TO  HKN-TEKEDYMD(1:6)
                 MOVE    "01"                  TO  HKN-TEKEDYMD(7:2)
                 MOVE    HKNNUM-REC            TO  MCPDATA-REC
                 MOVE    "tbl_hknnum"          TO  MCP-TABLE
                 MOVE    "key"                 TO  MCP-PATHNAME
grpsys           PERFORM 900-DBSELECT-SEC
                 IF      MCP-RC                =   ZERO
                   MOVE    "tbl_hknnum"            TO  MCP-TABLE
                   MOVE    "key"                   TO  MCP-PATHNAME
                   PERFORM  HKNNUM-READ-SEC
                   IF       FLG-HKNNUM    =   0
                       IF   HKN-HKNKOHSBTKBN   =   "2"
                           EVALUATE  HKN-HKNNUM
                            WHEN     "972"
                              PERFORM VARYING TOKKI-IDX2 FROM 1 BY 1
                                       UNTIL  TOKKI-IDX2   >  5
                                IF  RECE02-TOKKI(TOKKI-IDX2) = SPACE
                                  MOVE "02" TO RECE02-TOKKI(TOKKI-IDX2)
                                  MOVE   5  TO TOKKI-IDX2
                                ELSE
                                  IF  RECE02-TOKKI(TOKKI-IDX2) = "02"
                                    MOVE 5  TO TOKKI-IDX2
                                  END-IF
                                END-IF
                              END-PERFORM
                              MOVE    RECE01-KOHHKNNUM-O(IDX)
                                                   TO  WRK-CHOKI
                            WHEN     "974"
                              PERFORM VARYING TOKKI-IDX2 FROM 1 BY 1
                                       UNTIL  TOKKI-IDX2   >  5
                                IF  RECE02-TOKKI(TOKKI-IDX2) = SPACE
                                  MOVE "16" TO RECE02-TOKKI(TOKKI-IDX2)
                                  MOVE   5  TO TOKKI-IDX2
                                ELSE
                                  IF  RECE02-TOKKI(TOKKI-IDX2) = "16"
                                    MOVE 5  TO TOKKI-IDX2
                                  END-IF
                                END-IF
                              END-PERFORM
                              MOVE    RECE01-KOHHKNNUM-O(IDX)
                                                   TO  WRK-CHOKI2
                           END-EVALUATE
                       END-IF     
                   END-IF
                 END-IF
                 MOVE    "tbl_hknnum"          TO  MCP-TABLE
                 MOVE    "key"                 TO  MCP-PATHNAME
                 PERFORM 900-CLOSE-SEC
               END-IF
           END-PERFORM
      *
           PERFORM VARYING TOKKI-IDX2 FROM 1 BY 1
                    UNTIL (TOKKI-IDX2               >  5   )
                     OR   (RECE02-TOKKI(TOKKI-IDX2) = SPACE)
               MOVE   TOKKI-IDX2  TO  TOKKI-IDX
           END-PERFORM
      *
      *    公費情報のセット
           MOVE    ZERO        TO  IDX2
           PERFORM   VARYING   IDX    FROM   1   BY   1
                     UNTIL    (IDX          >     10)
                     OR       (RECE01-KOHID-O(IDX)  =   ZERO)  
               IF  (RECE01-KOHHKNNUM-O (IDX)  =       "027")
                     CONTINUE
               ELSE
                   IF      IDX                >   4
      *
                       DISPLAY "err=" IDX " " RECE01-KOHHKNNUM-O (IDX)
                                          " " RECE01-KOHID-O (IDX)
      *
                       MOVE    "9"                TO  RECE01-TEISEIKBN
                       MOVE    10                 TO  IDX
                   ELSE
                       IF      RECE01-KOHHKNNUM-O (IDX)  =   WRK-CHOKI
                                                        OR   WRK-CHOKI2
                           MOVE   "4"             TO  WRK-RECESKYKBN
                       ELSE
                           MOVE   RECE01-RECESKYKBN-O (IDX)
                                                 TO  WRK-RECESKYKBN
                       END-IF
                       PERFORM   KOHI-SET-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
      *    高齢非該当編集
           PERFORM   2101-KOREI-HIGAITO-HENSYU-SEC
      *
      *    レセプト種別
           PERFORM   RECESYUBETU-SET-SEC
      *
      *    主保険給付割合のセット
           PERFORM   KYURATE-SET-SEC
      *
      *    保険組合せ番号情報
           PERFORM VARYING IDX    FROM   1   BY   1
                   UNTIL   IDX          >     10
                   OR          RECE01-HKNCOMBI-O(IDX)  =   ZERO
               MOVE    RECE01-HKNCOMBI-O(IDX)
                                       TO  RECE02-HKNCOMBI(IDX)
               MOVE    RECE01-HKNNUMHC (IDX)
                                       TO  RECE02-HKNNUMHC(IDX)
               PERFORM VARYING IDY2    FROM    1   BY  1
                       UNTIL   IDY2    >       4
                   IF   ( RECE01-KOHNUMHC (IDX IDY2)
                                       NOT =   "027"       )
                   AND  ( RECE01-KOHNUMHC (IDX IDY2) (1:1)
                                           =   "0" OR  "9" )
                   AND  ( RECE01-KOHNUMHC (IDX IDY2) (1:2)
                                       NOT =   "99"        )
                   AND  ( RECE01-KOHNUMHC (IDX IDY2)
                                       NOT =   "970"       )
                       MOVE    1           TO  RECE02-HKNCOMBIFLG(IDX)
                       MOVE    4           TO  IDY2
                   END-IF
               END-PERFORM
      *
               MOVE    ZERO     TO   FLG-TAISYO
               PERFORM VARYING IDY2    FROM    1   BY  1
                       UNTIL   IDY2    >       4
                   IF   RECE01-KOHNUMHC (IDX IDY2) NOT = SPACE
                     IF RECE01-KOHNUMHC (IDX IDY2) = "972" OR "974"
                        IF   FLG-TAISYO  =  ZERO
                           MOVE   1     TO   FLG-TAISYO
                        END-IF
                     ELSE
                        MOVE      9     TO   FLG-TAISYO
                     END-IF
                   END-IF
               END-PERFORM
               IF   FLG-TAISYO  =  1
                        MOVE      2     TO   RECE02-HKNCOMBIFLG(IDX)
               END-IF
      *
               PERFORM VARYING IDY2    FROM    1   BY  1
                       UNTIL   IDY2    >       4
                   IF   RECE01-KOHNUMHC(IDX IDY2) =  "970"
                       MOVE    1        TO   RECE02-DAISANFLG(IDX)
                       MOVE    RECE01-KOHIDHC(IDX IDY2)
                                        TO   RECE02-DAISAN-KOHID(IDX)
      *                第三者行為現物給付フラグ編集
                       PERFORM   2101-DAISAN-GENBUTU-HENSYU-SEC
                       MOVE    4        TO   IDY2
                   END-IF
               END-PERFORM
      *
      *        組合せ公費保険番号情報のセット
               PERFORM   KOHNUMHC-SET-SEC
           END-PERFORM
      *
      *
      *    特記事項のセット
      *    老保の編集
           MOVE    ZERO       TO     FLG-ROUJINHOKEN
           IF      RECE02-HKNNUM        =   SPACE
               IF  (   RECE01-SRYYM         >=  "200210"            )
               AND ((  RECE02-AGE           >=  75             ) OR 
                    (( RECE02-BIRTHDAY      <=  19320930 ) AND
                     ( RECE02-AGE           >=  70       )     )    )
                   ADD      1           TO  TOKKI-IDX
                   MOVE    "04"         TO  RECE02-TOKKI (TOKKI-IDX)
               ELSE
                   INITIALIZE                       PTKOHINF-REC
grpsys             MOVE    RECE01-HOSPNUM       TO  PTKOH-HOSPNUM
                   MOVE    RECE01-PTID          TO  PTKOH-PTID
                   IF      RECE01-SRYYM     <   WRK-CONS-SRYYM-200804
                       MOVE    "027"                TO  PTKOH-KOHNUM
                   ELSE
                       MOVE    "968"                TO  PTKOH-KOHNUM
                   END-IF 
                   MOVE    PTKOHINF-REC         TO  MCPDATA-REC
                   MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                   MOVE    "key4"               TO  MCP-PATHNAME
grpsys             PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC               =   ZERO
                       MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                       MOVE    "key4"               TO  MCP-PATHNAME
                       PERFORM  PTKOHINF-READ-SEC
                       PERFORM  UNTIL   FLG-PTKOHINF    =   1
                           IF      PTKOH-TEKSTYMD(1:6)  <=
                                                    RECE01-SRYYM
                           AND     RECE01-SRYYM         <=
                                                    PTKOH-TEKEDYMD(1:6)
                           AND     PTKOH-SAKJOKBN   =  SPACE
                               ADD      1           TO  TOKKI-IDX
                               MOVE    "04"         TO  RECE02-TOKKI
                                                            (TOKKI-IDX)
                               MOVE    1            TO  FLG-ROUJINHOKEN
                               MOVE    1            TO  FLG-PTKOHINF     
                           ELSE
                               MOVE    "tbl_ptkohinf"   TO  MCP-TABLE
                               MOVE    "key4"           TO  MCP-PATHNAME
                               PERFORM  PTKOHINF-READ-SEC
                           END-IF   
                       END-PERFORM
                   END-IF
                   MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                   MOVE    "key4"               TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               END-IF
           END-IF
      *    老併、老健、医併、医療の編集
           EVALUATE    RECE01-RECEIPT-KBN (1:1)
               WHEN    "1"
                   ADD      1               TO  TOKKI-IDX
                   MOVE    "07"             TO  RECE02-TOKKI (TOKKI-IDX)
               WHEN    "2"
                   ADD      1               TO  TOKKI-IDX
                   MOVE    "08"             TO  RECE02-TOKKI (TOKKI-IDX)
               WHEN    "3"
                   ADD      1               TO  TOKKI-IDX
                   MOVE    "38"             TO  RECE02-TOKKI (TOKKI-IDX)
               WHEN    "4"
                   ADD      1               TO  TOKKI-IDX
                   MOVE    "39"             TO  RECE02-TOKKI (TOKKI-IDX)
           END-EVALUATE
      *
           IF   RECE01-SRYYM    >=  "200210"
      *      低所得情報編集
             IF   RECE02-RECESYUBETU  =  "8888"  OR  "6666"
               CONTINUE
             ELSE
               PERFORM   2101-TEISYOTOKU-HENSYU-SEC
             END-IF
           END-IF
      *
      *    在総診フラグセット
           PERFORM    ZAITAKU-SET-SEC
      *
      *    レセプト種別不明エラー情報
           IF      RECE01-TEISEIKBN      =   "1"
               MOVE    RECE01-ERR-ERRKBN     TO  RECE02-ERR-ERRKBN
               MOVE    RECE01-ERR-DAY        TO  RECE02-ERR-DAY
               MOVE    RECE01-ERR-HKNCOMBINUM
                                             TO  RECE02-ERR-HKNCOMBINUM
               MOVE    RECE01-ERR-TEKSTYMD   TO  RECE02-ERR-TEKSTYMD
               MOVE    RECE01-ERR-TEKEDYMD   TO  RECE02-ERR-TEKEDYMD
           END-IF 
      *
           IF      RECE01-TEISEIKBN      =   "9"
               MOVE    88                    TO  RECE02-ERR-ERRKBN
           END-IF
      *
      *    印刷順
           IF      IDZ                 =   1
               MOVE    RECE01-PRTSEQ       TO  RECE02-PRTSEQ
           END-IF
      *
      *    レセプト請求しない地方公費情報
           IF      IDZ                 =   1
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       4
                   MOVE    RECE01-KOHSKY-KOHID-O     (IDX)
                                       TO  RECE02-KOHSKY-KOHID  (IDX)
                   MOVE    RECE01-KOHSKY-KOHHKNNUM-O (IDX)
                                       TO  RECE02-KOHSKY-KOHNUM (IDX)
      *
                   INITIALIZE                          PTKOHINF-REC
grpsys             MOVE    RECE02-HOSPNUM          TO  PTKOH-HOSPNUM
                   MOVE    RECE02-PTID             TO  PTKOH-PTID
                   MOVE    RECE02-KOHSKY-KOHID  (IDX)
                                                   TO  PTKOH-KOHID
                   MOVE    PTKOHINF-REC            TO  MCPDATA-REC
                   MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM PTKOHINF-READ-SEC
                       IF      FLG-PTKOHINF           =   ZERO
                           MOVE    PTKOH-FTNJANUM       TO
                                       RECE02-KOHSKY-KOHFTNJANUM (IDX)
                       END-IF
                   END-IF    
      *
                   MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               END-PERFORM
           END-IF
      *
      *    主科設定
           IF    ( IDZ                 =   1      )
           AND   ( SYS-2009-SYUKA-KBN  =   "1"    )
           AND   ( RECE02-RECESYUBETU  
                                   NOT =   "8888" )
             IF  (     ( RECE02-TEISYUTUSAKI   =   "1" )
                   AND ( SYS-2009-SYUKA-SYAHO
                                           NOT =   "0" ) )
             OR  (     ( RECE02-TEISYUTUSAKI   =   "2" )
                   AND ( SYS-2009-SYUKA-KOKUHO
                                           NOT =   "0" ) )
             OR  (     ( RECE02-TEISYUTUSAKI   =   "6" )
                   AND ( SYS-2009-SYUKA-KOUIKI
                                           NOT =   "0" ) )
               PERFORM 2101-SYUKA-HENSYU-SEC
      *        主科未設定のとき主科取得を行なう
               IF      RECE02-SYUKA            =   "XX"
                   INITIALIZE                      ORCSSYUACCAREA
                   MOVE    RECE02-PREFNUM      TO  SPA-PREFNUM
                   MOVE    RECE02-SRYYM        TO  LNK-SYUACC-I-SRYYM
                   MOVE    RECE02-BIRTHDAY     TO  SPA-BIRTHDAY
                   MOVE    RECE02-PTID          TO  SPA-PTID
                   MOVE    RECE02-NYUGAIKBN    TO  SPA-NYUGAIKBN
                   MOVE   "05"                 TO  LNK-SYUACC-I-KBN
                   CALL   "ORCSSYUKAMAIN"      USING   SPA-AREA
                                                       ORCSSYUACCAREA
      *
                   PERFORM 2101-SYUKA-HENSYU-SEC
               END-IF
             END-IF
           END-IF    
      *
      *    旧総合病院の場合の診療科名称
           IF    ( IDZ                 =   1     )
           AND   ( RECE02-RECEKA   NOT =   SPACE )
      *        レセ電診療科コード検索
               SET     TBL-RECEDENSRYKACD-IDX  TO  1
               SEARCH  TBL-RECEDENSRYKACD-OCC  
                                       VARYING TBL-RECEDENSRYKACD-IDX
                   AT  END
                       MOVE    SPACE           TO  RECE02-SYUKANAME
                   WHEN    RECE02-RECEKA   =   TBL-RECEDEN-SRYKACD
                                               (TBL-RECEDENSRYKACD-IDX)
                       MOVE    TBL-RECEDEN-SRYKANAME
                                       (TBL-RECEDENSRYKACD-IDX)
                                              TO  RECE02-SYUKANAME
               END-SEARCH
           END-IF
      *
      *    入退院履歴出力処理
           IF    ( IDZ             =   1   )
           AND   ( SYS-2005-NYUTAIINREKI-KSIKBN
                               NOT =   "0" )               
               PERFORM 240-PTNYUINRRK-SYORI-SEC
           END-IF
      *
      *    未請求フラグ
           IF    ( IDZ             =   1   )
               MOVE    RECE01-MISEIKYU-FLG     TO  RECE02-MISEIKYU-FLG
           END-IF
      *
      *    医師別出力設定（一括作成時リアルタイムプレビュー時のみ）
           IF    ( IDZ             =   1                     )
           AND   ( SYS-25-DRCD-PRTKBN
                                   =   "1"                   )
           AND   ( RECE02-SRYYM    >=  WRK-CONS-SRYYM-200804 )
             IF    ( WRK-PARA-SRYYM  NOT =   "000000"  )
             OR    ( WRK-PARA-SHELLID    =   "RECEPTX" )
               INITIALIZE                      ORCSDRCDGETAREA
               MOVE    RECE02-HOSPNUM  TO  SPA-HOSPNUM
               MOVE    "2"             TO  LNK-DRCDGET-I-NYUGAIKBN
               MOVE    RECE02-SRYYM    TO  LNK-DRCDGET-I-SRYYM
               MOVE    RECE02-PTID     TO  LNK-DRCDGET-I-PTID
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       10
                   MOVE    RECE02-HKNCOMBI (IDX)
                                       TO  LNK-DRCDGET-I-HKNCOMBI (IDX)
               END-PERFORM
               CALL    "ORCSDRCDGET"   USING   SPA-AREA
                                               ORCSDRCDGETAREA
               IF      LNK-DRCDGET-O-DRCD-SRYKA
                                       NOT =   SPACE
                   MOVE    LNK-DRCDGET-O-DRCD  TO  RECE02-DRCD
                   MOVE    LNK-DRCDGET-O-DRCD-SRYKA
                                               TO  RECE02-DRCD-SRYKA
               END-IF
             END-IF 
           END-IF
      *
      *    患者特記事項
           IF      IDZ             =   1
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       5
                       OR      IDX >       TBL-PTTOKKI-MAX
                   MOVE    TBL-PTTOKKI-CD (IDX)    TO  
                                               RECE02-PTTOKKI-CD   (IDX)
                   MOVE    TBL-PTTOKKI-NAIYO
                                          (IDX)    TO  
                                               RECE02-PTTOKKI-NAIYO(IDX)
               END-PERFORM
           END-IF
      *
      *    生保まとめフラグ（公費単独）
           IF      WRK-SEIHO-FLG           =   1
               IF    ( RECE02-KOHNUM (1)       =   "012"   OR  "025"
                                               OR  "015"   OR  "016"
                                               OR  "021"   OR  "053" )
               AND   ( RECE02-KOHNUM (2)       =   SPACE   OR  "972"
                                               OR  "974"             )
                   MOVE    "1"                 TO  RECE02-SEIHOFLG
               END-IF
           END-IF
      *
      *    点検レセ０点公費フラグ
           IF      IDZ             =   1
               MOVE    RECE01-TENKEN-FLG   TO  RECE02-TENKEN-FLG
           END-IF 
      *
           PERFORM     220-DATA-WRITE-SEC
           .
       2101-RECE02-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主科情報編集処理
      *****************************************************************
       2101-SYUKA-HENSYU-SEC                   SECTION.
      *
           INITIALIZE                      ORCSSYUACCAREA
           MOVE    RECE02-HOSPNUM  TO  SPA-HOSPNUM
           MOVE    RECE02-SRYYM    TO  LNK-SYUACC-I-SRYYM
           MOVE    RECE02-PTID     TO  SPA-PTID
           MOVE    RECE02-NYUGAIKBN
                                   TO  SPA-NYUGAIKBN
           MOVE    SYS-2009-SYUKA-KBN
                                   TO  SPA-SYUKA-KBN
           MOVE    SYS-2009-SYUKA-NYUIN
                                   TO  SPA-SYUKA-NYUIN
           MOVE    SYS-2009-SYUKA-GAIRAI
                                   TO  SPA-SYUKA-GAIRAI
           MOVE    SYS-2009-SYUKA-MODE
                                   TO  SPA-SYUKA-MODE
           MOVE    SYS-2009-SYUKA-SELECT
                                   TO  SPA-SYUKA-SELECT
           MOVE    RECE02-RECESYUBETU
                                   TO  LNK-SYUACC-IO-RECESYUBETU (1)
           MOVE    RECE02-HKNJANUM-KEY
                                   TO  LNK-SYUACC-IO-HKNJANUM    (1)
           MOVE    RECE02-HOJOKBN-KEY
                                   TO  LNK-SYUACC-IO-HOJOKBN     (1)
           MOVE    RECE02-TEISYUTUSAKI
                                   TO  LNK-SYUACC-IO-TEISYUTUSAKI(1)
           MOVE    RECE02-KOHID-KEY
                                   TO  LNK-SYUACC-IO-KOHID       (1)
           MOVE    RECE02-RECEIPT-KBN
                                   TO  LNK-SYUACC-IO-RECEIPT-KBN (1)
      *    精神入院か生保単独複数のとき
           IF    ( RECE02-HKNNUM   =   SPACE      )
           AND   ( RECE02-TEKSTYMD
                               NOT =   "00000000" )
               MOVE    RECE02-TEKSTYMD
                                   TO  LNK-SYUACC-IO-HKNJANUM    (1)
           END-IF
      *
           MOVE    "06"            TO  LNK-SYUACC-I-KBN
      *
      *    点検レセ０点公費記載ありのとき
           IF      RECE01-TENKEN-FLG   =   "1"
               EVALUATE    RECE02-RECESYUBETU (3:1)
                   WHEN    "5"
                       MOVE    "4"         TO
                                   LNK-SYUACC-IO-RECESYUBETU (1) (3:1)
                   WHEN    "4"
                       MOVE    "3"         TO
                                   LNK-SYUACC-IO-RECESYUBETU (1) (3:1)
                   WHEN    "3"
                       MOVE    "2"         TO
                                   LNK-SYUACC-IO-RECESYUBETU (1) (3:1)
                   WHEN    "2"
                       MOVE    "1"         TO
                                   LNK-SYUACC-IO-RECESYUBETU (1) (3:1)
                       IF      RECE02-RECESYUBETU (2:1)    =   "2"
                           MOVE    "08"            TO  LNK-SYUACC-I-KBN
                       END-IF
               END-EVALUATE
           END-IF
      *
           CALL    "ORCSSYUKAMAIN" USING   SPA-AREA
                                           ORCSSYUACCAREA
           IF      LNK-SYUACC-IO-H-SRYKA
                                   NOT =   SPACE
               MOVE    LNK-SYUACC-IO-H-SRYKA   TO  RECE02-SYUKA
           ELSE
               IF      LNK-SYUACC-IO-SYUKA-INPUT (1)
                                       NOT =   SPACE
                   MOVE    LNK-SYUACC-IO-SYUKA-INPUT (1)
                                                   TO  RECE02-SYUKA
               ELSE
                   IF      LNK-SYUACC-IO-SYUKA-AUTO (1)
                                           NOT =   SPACE
                       MOVE    LNK-SYUACC-IO-SYUKA-AUTO (1) 
                                                   TO  RECE02-SYUKA
                   ELSE
                       MOVE    "XX"                TO  RECE02-SYUKA
                   END-IF
               END-IF
           END-IF
      * 
      *    主科設定時のみ診療科名称を設定    
           IF  (     ( RECE02-TEISYUTUSAKI   =   "1" )
                 AND ( SYS-2009-SYUKA-SYAHO  =   "1" ) )
           OR  (     ( RECE02-TEISYUTUSAKI   =   "2" )
                 AND ( SYS-2009-SYUKA-KOKUHO =   "1" ) )
           OR  (     ( RECE02-TEISYUTUSAKI   =   "6" )
                 AND ( SYS-2009-SYUKA-KOUIKI =   "1" ) )
               MOVE    SPACE                TO  SYSKANRI-REC
               INITIALIZE                       SYSKANRI-REC
               MOVE    "1005"               TO  SYS-KANRICD
               MOVE    RECE02-SYUKA         TO  SYS-KBNCD
               MOVE    RECE02-SRYYM         TO  SYS-STYUKYMD (1:6)
               MOVE    "01"                 TO  SYS-STYUKYMD (7:2)
               MOVE    SYS-STYUKYMD         TO  SYS-EDYUKYMD
               MOVE    "tbl_syskanri"       TO  WRK-MCP-TABLE
               MOVE    "key10"              TO  WRK-MCP-PATHNAME
                PERFORM 960-KANRIMST-READ-SEC
               IF      FLG-SYSKANRI         =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1005-REC
               ELSE
                   INITIALIZE                      SYS-1005-REC
               END-IF
               MOVE    SYS-1005-RECESRYKA  TO  RECE02-RECESYUKA
               MOVE    SYS-1005-SRYKANAME  TO  RECE02-SYUKANAME
           END-IF
      *
      *    診療科２、診療科３のセット
      *    主科未設定または平成２２年３月以前の場合はセットしない
           IF    ( RECE02-SYUKA    =   "XX"   )
           OR    ( RECE02-SRYYM    <=  201003 )
               GO  TO  2101-SYUKA-HENSYU-EXT
           END-IF     
      * 
      *    主科設定時以外セットしない
           IF  (     ( RECE02-TEISYUTUSAKI   =   "1" )
                 AND ( SYS-2009-SYUKA-SYAHO  =   "1" ) )
           OR  (     ( RECE02-TEISYUTUSAKI   =   "2" )
                 AND ( SYS-2009-SYUKA-KOKUHO =   "1" ) )
           OR  (     ( RECE02-TEISYUTUSAKI   =   "6" )
                 AND ( SYS-2009-SYUKA-KOUIKI =   "1" ) )
               CONTINUE
           ELSE
               GO  TO  2101-SYUKA-HENSYU-EXT
           END-IF     
      *
           INITIALIZE                  WRK-SYUACC-IO-SYUKA-AREA
           MOVE    ZERO            TO  IDZ1
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       50
                   OR      LNK-SYUACC-IO-SRYKA (1 IDY)
                                   =   SPACE
      *        主科情報のレセ電診療科情報取得
               MOVE    SPACE                TO  SYSKANRI-REC
               INITIALIZE                       SYSKANRI-REC
               MOVE    "1005"               TO  SYS-KANRICD
               MOVE    LNK-SYUACC-IO-SRYKA (1 IDY)
                                            TO  SYS-KBNCD
               MOVE    RECE02-SRYYM         TO  SYS-STYUKYMD (1:6)
               MOVE    "01"                 TO  SYS-STYUKYMD (7:2)
               MOVE    SYS-STYUKYMD         TO  SYS-EDYUKYMD
               MOVE    "tbl_syskanri"       TO  WRK-MCP-TABLE
               MOVE    "key10"              TO  WRK-MCP-PATHNAME
                PERFORM 960-KANRIMST-READ-SEC
               IF      FLG-SYSKANRI         =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1005-REC
               ELSE
                   INITIALIZE                      SYS-1005-REC
               END-IF
    *    
               IF    ( RECE02-SYUKA    =   LNK-SYUACC-IO-SRYKA (1 IDY) )
               OR    ( RECE02-RECESYUKA
                                       =   SYS-1005-RECESRYKA          )
                   CONTINUE
               ELSE
                   ADD     1           TO  IDZ1
                   MOVE    LNK-SYUACC-IO-SRYKA (1 IDY)
                                       TO  WRK-SYUACC-IO-SRYKA (IDZ1)
                   MOVE    LNK-SYUACC-IO-TENSU (1 IDY)
                                       TO  WRK-SYUACC-IO-TENSU (IDZ1)
                   MOVE    LNK-SYUACC-IO-KAISU (1 IDY)
                                       TO  WRK-SYUACC-IO-KAISU (IDZ1)
                   MOVE    SYS-1005-RECESRYKA
                                       TO  WRK-SYUACC-IO-RECESYUKA(IDZ1)
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDZ1    FROM    1   BY  1
                   UNTIL   IDZ1    >       49
                   OR      WRK-SYUACC-IO-SRYKA (IDZ1)
                                   =   SPACE
             COMPUTE IDY     =   IDZ1    +   1
             PERFORM VARYING IDZ2    FROM    IDY BY  1
                     UNTIL   IDZ2    >   50
                     OR      WRK-SYUACC-IO-SRYKA (IDZ2)
                                     =   SPACE
      *
               EVALUATE    SYS-2009-SYUKA-GAIRAI
                   WHEN    "1"
                       IF      WRK-SYUACC-IO-KAISU (IDZ1)
                                       =   WRK-SYUACC-IO-KAISU (IDZ2)
                           EVALUATE    SYS-2009-SYUKA-SELECT
                               WHEN    "0"
                               WHEN    "1"
                                   IF      WRK-SYUACC-IO-SRYKA (IDZ1)
                                                   >
                                           WRK-SYUACC-IO-SRYKA (IDZ2)
                                       MOVE
                                           WRK-SYUACC-IO-SRYKATBL (IDZ1)
                                                      TO  WRK-SYUACC-REC
                                       MOVE
                                           WRK-SYUACC-IO-SRYKATBL (IDZ2)
                                                      TO
                                           WRK-SYUACC-IO-SRYKATBL (IDZ1)
                                       MOVE
                                           WRK-SYUACC-REC
                                                      TO
                                           WRK-SYUACC-IO-SRYKATBL (IDZ2)
                                   END-IF
                               WHEN    "2"
                                   IF      WRK-SYUACC-IO-SRYKA (IDZ1)
                                                   <
                                           WRK-SYUACC-IO-SRYKA (IDZ2)
                                       MOVE
                                           WRK-SYUACC-IO-SRYKATBL (IDZ1)
                                                      TO  WRK-SYUACC-REC
                                       MOVE
                                           WRK-SYUACC-IO-SRYKATBL (IDZ2)
                                                      TO
                                           WRK-SYUACC-IO-SRYKATBL (IDZ1)
                                       MOVE
                                           WRK-SYUACC-REC
                                                      TO
                                           WRK-SYUACC-IO-SRYKATBL (IDZ2)
                                   END-IF
                           END-EVALUATE
                       ELSE
                           IF      WRK-SYUACC-IO-KAISU (IDZ1)
                                       <   WRK-SYUACC-IO-KAISU (IDZ2)
                               MOVE    WRK-SYUACC-IO-SRYKATBL (IDZ1)
                                                      TO  WRK-SYUACC-REC
                               MOVE    WRK-SYUACC-IO-SRYKATBL (IDZ2)
                                                      TO
                                           WRK-SYUACC-IO-SRYKATBL (IDZ1)
                               MOVE    WRK-SYUACC-REC
                                                      TO
                                           WRK-SYUACC-IO-SRYKATBL (IDZ2)
                           END-IF
                       END-IF
                   WHEN    "2"
                       IF      WRK-SYUACC-IO-TENSU (IDZ1)
                                       =   WRK-SYUACC-IO-TENSU (IDZ2)
                           EVALUATE    SYS-2009-SYUKA-SELECT
                               WHEN    "0"
                               WHEN    "1"
                                   IF      WRK-SYUACC-IO-SRYKA (IDZ1)
                                                   >
                                           WRK-SYUACC-IO-SRYKA (IDZ2)
                                       MOVE
                                           WRK-SYUACC-IO-SRYKATBL (IDZ1)
                                                      TO  WRK-SYUACC-REC
                                       MOVE
                                           WRK-SYUACC-IO-SRYKATBL (IDZ2)
                                                      TO
                                           WRK-SYUACC-IO-SRYKATBL (IDZ1)
                                       MOVE
                                           WRK-SYUACC-REC
                                                      TO
                                           WRK-SYUACC-IO-SRYKATBL (IDZ2)
                                   END-IF
                               WHEN    "2"
                                   IF      WRK-SYUACC-IO-SRYKA (IDZ1)
                                                   <
                                           WRK-SYUACC-IO-SRYKA (IDZ2)
                                       MOVE
                                           WRK-SYUACC-IO-SRYKATBL (IDZ1)
                                                      TO  WRK-SYUACC-REC
                                       MOVE
                                           WRK-SYUACC-IO-SRYKATBL (IDZ2)
                                                      TO
                                           WRK-SYUACC-IO-SRYKATBL (IDZ1)
                                       MOVE
                                           WRK-SYUACC-REC
                                                      TO
                                           WRK-SYUACC-IO-SRYKATBL (IDZ2)
                                   END-IF
                           END-EVALUATE
                       ELSE
                           IF      WRK-SYUACC-IO-TENSU (IDZ1)
                                       <   WRK-SYUACC-IO-TENSU (IDZ2)
                               MOVE    WRK-SYUACC-IO-SRYKATBL (IDZ1)
                                                      TO  WRK-SYUACC-REC
                               MOVE    WRK-SYUACC-IO-SRYKATBL (IDZ2)
                                                      TO
                                           WRK-SYUACC-IO-SRYKATBL (IDZ1)
                               MOVE    WRK-SYUACC-REC TO
                                           WRK-SYUACC-IO-SRYKATBL (IDZ2)
                           END-IF
                       END-IF
               END-EVALUATE
             END-PERFORM
           END-PERFORM
      *
           IF      WRK-SYUACC-IO-SRYKA (1)
                                   NOT =   SPACE
               MOVE    WRK-SYUACC-IO-SRYKA (1)
                                           TO  RECE02-SYUKA2
               MOVE    WRK-SYUACC-IO-RECESYUKA (1)
                                           TO  RECE02-RECESYUKA2
           END-IF
           IF      WRK-SYUACC-IO-SRYKA (2)
                                   NOT =   SPACE
               IF      WRK-SYUACC-IO-RECESYUKA (1)
                                   NOT =   WRK-SYUACC-IO-RECESYUKA (2)
                   MOVE    WRK-SYUACC-IO-SRYKA (2)
                                               TO  RECE02-SYUKA3
                   MOVE    WRK-SYUACC-IO-RECESYUKA (2)
                                               TO  RECE02-RECESYUKA3
               ELSE
                   IF      WRK-SYUACC-IO-SRYKA (3)
                                           NOT =   SPACE
                       IF      WRK-SYUACC-IO-RECESYUKA (1)
                                   NOT =   WRK-SYUACC-IO-RECESYUKA (3)
                           MOVE    WRK-SYUACC-IO-SRYKA (3)
                                                   TO  RECE02-SYUKA3
                           MOVE    WRK-SYUACC-IO-RECESYUKA (3)
                                               TO  RECE02-RECESYUKA3
                       END-IF
                   END-IF    
               END-IF
           END-IF
           .
      *
       2101-SYUKA-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    低所得情報編集
      *****************************************************************
       2101-TEISYOTOKU-HENSYU-SEC                   SECTION.
      *
           MOVE     99       TO    SYOTOKU-IDX
           MOVE     99       TO    SYOTOKU-IDX2(1)
           MOVE     99       TO    SYOTOKU-IDX2(2)
           MOVE     99       TO    SYOTOKU-IDX2(3)
           MOVE     99       TO    SYOTOKU-IDX2(4)
           MOVE     ZERO     TO    SYOTOKU-IDX9
      *
           IF   RECE02-SRYYM   >=  "200604"
              IF  RECE02-HKNNUM      NOT = SPACE
                IF   RECE02-TEKSTYMD(1:6)  >=  RECE02-SRYYM
                    MOVE   RECE02-TEKSTYMD(7:2)    TO   SYOTOKU-IDX
                ELSE
                    MOVE   1                       TO   SYOTOKU-IDX
                END-IF
              ELSE
                PERFORM  VARYING   KOHFTN-IDX   FROM   1   BY   1
                          UNTIL    KOHFTN-IDX     >    4
                  IF   (RECE02-KOHNUM(KOHFTN-IDX)  NOT = SPACE)
                  AND  (RECE02-KOHNUM(KOHFTN-IDX)(1:1) = "0"  )
                    IF  RECE02-KOHTEKSTYMD(KOHFTN-IDX)(1:6)
                                           >=  RECE02-SRYYM
                      MOVE   RECE02-KOHTEKSTYMD(KOHFTN-IDX)(7:2)
                                  TO   SYOTOKU-IDX9
                    ELSE
                      MOVE   1    TO   SYOTOKU-IDX9
                    END-IF
                    IF    SYOTOKU-IDX9  <  SYOTOKU-IDX
                        MOVE  SYOTOKU-IDX9  TO  SYOTOKU-IDX
                    END-IF
                  END-IF
                END-PERFORM
              END-IF
              MOVE     SYOTOKU-IDX    TO   SYOTOKU-IDX9
              PERFORM  VARYING   KOHFTN-IDX   FROM   1   BY   1
                        UNTIL    KOHFTN-IDX     >    4
                IF   (RECE02-KOHNUM(KOHFTN-IDX)  NOT = SPACE)
                AND  (RECE02-KOHNUM(KOHFTN-IDX)(1:1)
                                         >=  "1" AND <=  "8")
                  IF  RECE02-KOHTEKSTYMD(KOHFTN-IDX)(1:6)
                                         >=  RECE02-SRYYM
                     MOVE   RECE02-KOHTEKSTYMD(KOHFTN-IDX)(7:2)
                                    TO   SYOTOKU-IDX2(KOHFTN-IDX)
                  ELSE
                     MOVE   1       TO   SYOTOKU-IDX2(KOHFTN-IDX)
                  END-IF
                  IF   SYOTOKU-IDX9  =   99
                     IF    SYOTOKU-IDX2(KOHFTN-IDX) <  SYOTOKU-IDX
                        MOVE   SYOTOKU-IDX2(KOHFTN-IDX)
                                    TO   SYOTOKU-IDX
                     END-IF
                  END-IF
                END-IF
              END-PERFORM
              IF   SYOTOKU-IDX  =  99
                  MOVE      1       TO   SYOTOKU-IDX
              END-IF
           ELSE
              MOVE       1          TO   SYOTOKU-IDX
              PERFORM  VARYING   KOHFTN-IDX   FROM   1   BY   1
                        UNTIL    KOHFTN-IDX     >    4
                  IF   (RECE02-KOHNUM(KOHFTN-IDX)  NOT = SPACE)
                  AND  (RECE02-KOHNUM(KOHFTN-IDX)(1:1)
                                           >=  "1" AND <=  "8")
                     MOVE     1     TO   SYOTOKU-IDX2(KOHFTN-IDX)
                  END-IF
              END-PERFORM
           END-IF
      *
           INITIALIZE                ORCSSYOTOKUAREA
           MOVE  RECE02-HOSPNUM  TO  LNK-SYOTOKU-HOSPNUM
           MOVE  RECE02-PTID     TO  LNK-SYOTOKU-PTID
           MOVE  RECE02-SRYYM    TO  LNK-SYOTOKU-SRYYM
           MOVE  SPACE           TO  LNK-SYOTOKU-SKJCHKFLG
      *
           CALL    "ORCSSYOTOKU"   USING  ORCSSYOTOKUAREA
                                          SPA-AREA
      *
           IF    LNK-SYOTOKU-RC    =   ZERO
                MOVE  LNK-SYOTOKU-SYUKBN(SYOTOKU-IDX)
                                 TO  RECE02-TEISYOTOKU
                PERFORM  VARYING   KOHFTN-IDX   FROM   1   BY   1
                          UNTIL    KOHFTN-IDX     >    4
                   IF  SYOTOKU-IDX2(KOHFTN-IDX) =  99
                     MOVE  SPACE TO  RECE02-TEISYOTOKU2(KOHFTN-IDX)
                   ELSE
                     MOVE  SYOTOKU-IDX2(KOHFTN-IDX)  TO  SYOTOKU-IDX9
                     MOVE  LNK-SYOTOKU-CHIKBN(SYOTOKU-IDX9)
                                 TO  RECE02-TEISYOTOKU2(KOHFTN-IDX)
                   END-IF
                END-PERFORM
           END-IF
      *
           .
       2101-TEISYOTOKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    高齢非該当編集
      *****************************************************************
       2101-KOREI-HIGAITO-HENSYU-SEC                   SECTION.
      *
           IF    RECE02-AGE         >=    70
             IF  (RECE02-HKNNUM       =  SPACE OR "040" OR "039"
                                               OR "068" OR "069")
             OR  (RECE02-HKNNUM(1:2)  =   "98" OR  "90" OR  "91")
                CONTINUE
             ELSE
              IF ( RECE02-HKNNUM      = "060"           )
              OR ((RECE02-HKNNUM  NOT = "060"          ) AND
                  (RECE02-HOJOKBN     = SPACE          ))
                MOVE      RECE01-SRYYM       TO   WRK-SRYYYMM
                IF   RECE02-TEKSTYMD(1:6)  >=  RECE01-SRYYM
                  MOVE  RECE02-TEKSTYMD(7:2) TO   WRK-SRYDD
                ELSE
                  MOVE  01                   TO   WRK-SRYDD
                END-IF
      *
                INITIALIZE                       PTKOHINF-REC
                MOVE    RECE01-HOSPNUM       TO  PTKOH-HOSPNUM
                MOVE    RECE01-PTID          TO  PTKOH-PTID
                MOVE    "976"                TO  PTKOH-KOHNUM
                MOVE    PTKOHINF-REC         TO  MCPDATA-REC
                MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                MOVE    "key4"               TO  MCP-PATHNAME
                PERFORM 900-DBSELECT-SEC
                IF      MCP-RC               =   ZERO
                  MOVE    "tbl_ptkohinf"     TO  MCP-TABLE
                  MOVE    "key4"             TO  MCP-PATHNAME
                  PERFORM  PTKOHINF-READ-SEC
                  PERFORM  UNTIL   FLG-PTKOHINF    =   1
                    IF    (PTKOH-TEKSTYMD  <=  WRK-SRYYMD    )
                    AND   (WRK-SRYYMD      <=  PTKOH-TEKEDYMD)
                    AND   (PTKOH-SAKJOKBN   =  SPACE         )
                       MOVE    1    TO   RECE02-KOREIHIGAITO
                       MOVE    1    TO   FLG-PTKOHINF
                    ELSE
                       MOVE    "tbl_ptkohinf"    TO  MCP-TABLE
                       MOVE    "key4"            TO  MCP-PATHNAME
                       PERFORM  PTKOHINF-READ-SEC
                    END-IF
                  END-PERFORM
                END-IF
                MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                MOVE    "key4"               TO  MCP-PATHNAME
                PERFORM  900-CLOSE-SEC
              END-IF
             END-IF
           END-IF
      *
           .
       2101-KOREI-HIGAITO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    第三者行為現物給付フラグ編集
      *****************************************************************
       2101-DAISAN-GENBUTU-HENSYU-SEC                   SECTION.
      *
           INITIALIZE                     PTRSIINF-REC
           MOVE    RECE01-HOSPNUM    TO   PTRSI-HOSPNUM
           MOVE    RECE01-PTID       TO   PTRSI-PTID
           MOVE    RECE01-KOHIDHC(IDX IDY2)
                                     TO   PTRSI-KOHID
           MOVE    PTRSIINF-REC      TO   MCPDATA-REC
           MOVE    "tbl_ptrsiinf"    TO   MCP-TABLE
           MOVE    "key"             TO   MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC  =  ZERO
             MOVE    "tbl_ptrsiinf"  TO   MCP-TABLE
             MOVE    "key"           TO   MCP-PATHNAME
             PERFORM  PTRSIINF-READ-SEC
             IF    FLG-PTRSIINF     =  ZERO
      *        現物給付対象とする
               IF     RECE01-SRYYM   >=  "201204"
                 IF   PTRSI-DAISAN-GENBTKBN  =  "2"
                   MOVE   1   TO   RECE02-DAISAN-GENBUTUFLG(IDX)
                   MOVE   1   TO   RECE02-DAISAN-GENBUTU-GAITO
                 END-IF
               END-IF
      *        特記事項「１０第三」記載しない
               IF     PTRSI-DAISAN-TOKKIKBN  =  "2"
                   MOVE   1   TO   RECE02-DAISAN-TOKKI
               END-IF
             END-IF
           END-IF
           MOVE    "tbl_ptrsiinf"    TO   MCP-TABLE
           MOVE    "key"             TO   MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       2101-DAISAN-GENBUTU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費情報のセット
      *****************************************************************
       KOHI-SET-SEC                SECTION.
      *
           INITIALIZE                           PTKOHINF-REC
grpsys     MOVE    RECE01-HOSPNUM       TO      PTKOH-HOSPNUM
           MOVE    RECE01-PTID          TO      PTKOH-PTID
           MOVE    RECE01-KOHID-O(IDX)  TO      PTKOH-KOHID
           MOVE    PTKOHINF-REC         TO      MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM  PTKOHINF-READ-SEC
               IF       FLG-PTKOHINF       =   ZERO
                   ADD     1               TO  IDX2
      *
                   MOVE   PTKOH-KOHID      TO  RECE02-KOHID       (IDX2)
                   MOVE   PTKOH-KOHNUM     TO  RECE02-KOHNUM      (IDX2)
                   MOVE   PTKOH-TEKSTYMD   TO  RECE02-KOHTEKSTYMD (IDX2)
                   MOVE   PTKOH-TEKEDYMD   TO  RECE02-KOHTEKEDYMD (IDX2)
                   MOVE   WRK-RECESKYKBN   TO  RECE02-RECESKYKBN  (IDX2)
                   MOVE   PTKOH-FTNJANUM   TO  RECE02-KOHFTNJANUM (IDX2)
                   MOVE   SPACE            TO
                                           RECE02-KOHNINIKYUKBN (IDX2)
      *            法別番号・本人−外来−内容のセット
                   INITIALIZE                  HKNNUM-REC
grpsys             MOVE   RECE01-HOSPNUM   TO  HKN-HOSPNUM
                   MOVE   PTKOH-KOHNUM     TO  HKN-HKNNUM
                   PERFORM 900-HKNNUM-CHK-SEC
                   IF       FLG-HKNNUM     =   ZERO
                       MOVE   HKN-HBTNUM          TO
                                       RECE02-KOHHBTNUM        (IDX2)
                       MOVE   HKN-HON-GAIFTNKBN   TO
                                       RECE02-HON-GAIFTNKBN    (IDX2)
                       MOVE   HKN-HON-GAIKFTNRATE TO
                                       RECE02-HON-GAIKFTNRATE  (IDX2)
                       MOVE   HKN-HON-GAIKJGNGAK  TO
                                       RECE02-HON-GAIKJGNGAK   (IDX2)
                       MOVE   HKN-HON-GAIDJGNGAK  TO
                                       RECE02-HON-GAIDJGNGAK   (IDX2)
                       MOVE   HKN-HON-GAIDJGNCNT  TO
                                       RECE02-HON-GAIDJGNCNT   (IDX2)
                       MOVE   HKN-HON-GAIMNAIJGNGAK
                                                  TO
                                       RECE02-HON-GAIMNAIJGNGAK(IDX2)
                       MOVE   HKN-HON-GAIMGAIJGNGAK
                                                  TO
                                       RECE02-HON-GAIMGAIJGNGAK(IDX2)
                       MOVE   HKN-HON-GAIMJGNCNT  TO
                                       RECE02-HON-GAIMJGNCNT   (IDX2)
                       MOVE   HKN-TSY-GAIFTNKBN   TO
                                       RECE02-TSY-GAIFTNKBN    (IDX2)
                       MOVE   HKN-TSY-GAIKFTNRATE TO
                                       RECE02-TSY-GAIKFTNRATE  (IDX2)
                       MOVE   HKN-TSY-GAIKJGNGAK  TO
                                       RECE02-TSY-GAIKJGNGAK   (IDX2)
                       MOVE   HKN-TSY-GAIDJGNGAK  TO
                                       RECE02-TSY-GAIDJGNGAK   (IDX2)
                       MOVE   HKN-TSY-GAIDJGNCNT  TO
                                       RECE02-TSY-GAIDJGNCNT   (IDX2)
                       MOVE   HKN-TSY-GAIMNAIJGNGAK
                                                  TO
                                       RECE02-TSY-GAIMNAIJGNGAK(IDX2)
                       MOVE   HKN-TSY-GAIMGAIJGNGAK
                                                  TO
                                       RECE02-TSY-GAIMGAIJGNGAK(IDX2)
                       MOVE   HKN-TSY-GAIMJGNCNT  TO
                                       RECE02-TSY-GAIMJGNCNT   (IDX2)
                       MOVE   HKN-TNK-GAIFTNKBN   TO
                                       RECE02-TNK-GAIFTNKBN    (IDX2)
                       MOVE   HKN-TNK-GAIKFTNRATE TO
                                       RECE02-TNK-GAIKFTNRATE  (IDX2)
                       MOVE   HKN-TNK-GAIKJGNGAK  TO
                                       RECE02-TNK-GAIKJGNGAK   (IDX2)
                       MOVE   HKN-TNK-GAIDJGNGAK  TO
                                       RECE02-TNK-GAIDJGNGAK   (IDX2)
                       MOVE   HKN-TNK-GAIDJGNCNT  TO
                                       RECE02-TNK-GAIDJGNCNT   (IDX2)
                       MOVE   HKN-TNK-GAIMNAIJGNGAK
                                                  TO
                                       RECE02-TNK-GAIMNAIJGNGAK(IDX2)
                       MOVE   HKN-TNK-GAIMGAIJGNGAK
                                                  TO
                                       RECE02-TNK-GAIMGAIJGNGAK(IDX2)
                       MOVE   HKN-TNK-GAIMJGNCNT  TO
                                       RECE02-TNK-GAIMJGNCNT   (IDX2)
                   END-IF
      *             
      *            生保又は中国残留邦人等のとき月代わり受給者番号マスタ
      *            より受給者番号をセット 
                   IF      PTKOH-KOHNUM    =   WRK-CONS-SEIHO-HKNNUM
grpsys                 MOVE    RECE02-HOSPNUM  TO  MONTHLYNUM-HOSPNUM
                       MOVE    RECE02-PTID     TO  MONTHLYNUM-PTID
                       MOVE    PTKOH-KOHID     TO  MONTHLYNUM-KOHID
                       MOVE    RECE02-SRYYM    TO  MONTHLYNUM-SRYYM
                       MOVE    RECE02-NYUGAIKBN
                                               TO  MONTHLYNUM-NYUGAIKBN
                       MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
                       MOVE    "tbl_monthlynum"
                                               TO  WRK-MCP-TABLE
                       MOVE    "key4"          TO  WRK-MCP-PATHNAME
                       PERFORM 910-MONTHLYNUM-READ-SEC
                       IF    ( FLG-MONTHLYNUM          =   ZERO  )
                           MOVE    MONTHLYNUM-JKYSNUM 
                                           TO  RECE02-KOHJKYSNUM (IDX2)
                       ELSE                       
grpsys                     MOVE    RECE02-HOSPNUM  TO
                                                   MONTHLYNUM-HOSPNUM
                           MOVE    RECE02-PTID     TO  MONTHLYNUM-PTID
                           MOVE    ZERO            TO  MONTHLYNUM-KOHID
                           MOVE    RECE02-SRYYM    TO  MONTHLYNUM-SRYYM
                           MOVE    RECE02-NYUGAIKBN
                                                   TO
                                                   MONTHLYNUM-NYUGAIKBN
                           MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
                           MOVE    "tbl_monthlynum"
                                                   TO  WRK-MCP-TABLE
                           MOVE    "key4"          TO  WRK-MCP-PATHNAME
                           PERFORM 910-MONTHLYNUM-READ-SEC
                           IF    ( FLG-MONTHLYNUM          =   ZERO  )
                               MOVE    MONTHLYNUM-JKYSNUM 
                                           TO  RECE02-KOHJKYSNUM (IDX2)
                           ELSE                       
                               MOVE   PTKOH-JKYSNUM
                                           TO  RECE02-KOHJKYSNUM (IDX2)
                           END-IF
                       END-IF    
                   ELSE                       
                       IF      PTKOH-KOHNUM    =
                                           WRK-CONS-ZANRYUU-HKNNUM
                           MOVE    RECE02-HOSPNUM
                                               TO  MONTHLYNUM-HOSPNUM
                           MOVE    RECE02-PTID TO  MONTHLYNUM-PTID
                           MOVE    PTKOH-KOHID TO  MONTHLYNUM-KOHID
                           MOVE    RECE02-SRYYM
                                               TO  MONTHLYNUM-SRYYM
                           MOVE    RECE02-NYUGAIKBN
                                               TO  MONTHLYNUM-NYUGAIKBN
                           MOVE    MONTHLYNUM-REC
                                               TO  MCPDATA-REC
                           MOVE    "tbl_monthlynum"
                                               TO  WRK-MCP-TABLE
                           MOVE    "key4"      TO  WRK-MCP-PATHNAME
                           PERFORM 910-MONTHLYNUM-READ-SEC
                           IF    ( FLG-MONTHLYNUM  =   ZERO  )
                               MOVE    MONTHLYNUM-JKYSNUM 
                                           TO  RECE02-KOHJKYSNUM (IDX2)
                           ELSE                       
                               MOVE   PTKOH-JKYSNUM
                                           TO  RECE02-KOHJKYSNUM (IDX2)
                           END-IF
                       ELSE    
                           MOVE   PTKOH-JKYSNUM
                                           TO  RECE02-KOHJKYSNUM (IDX2)
                       END-IF
                   END-IF    
               ELSE
                   DISPLAY  "PTKOHINF FETCH ERR"  
               END-IF
           ELSE
               DISPLAY   "PTKOHINF SELECT ERR" 
           END-IF
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    神奈川地方公費対応
           IF    ( RECE02-PREFNUM  =   "14"           )
           AND   ( RECE02-TEISYUTUSAKI
                                   =   "2"            )
      *****AND   ( RECE02-KOHNUM      (IDX2)
      *****                        =   "180" OR "280" )
           AND   ( RECE02-KOHHBTNUM   (IDX2)
                                   =   "80"  )
           AND   ( RECE02-KOHFTNJANUM (IDX2)
                                   =   SPACE          )
               MOVE   "4"          TO  RECE02-RECESKYKBN(IDX2)
           END-IF
      *
           .
       KOHI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト種別セット処理
      *****************************************************************
       RECESYUBETU-SET-SEC              SECTION.
      *
      *    保険変更後、訂正していないとき
      *    保険組合せがないとき
           IF      RECE01-TEISEIKBN     =   "1"
               GO  TO  RECESYUBETU-SET-EXT
           END-IF
      *    公費が４つを超えたとき
           IF      RECE01-TEISEIKBN     =   "9"
               GO  TO  RECESYUBETU-SET-EXT
           END-IF
      *
      *    地方公費データのとき主保険が無かったら不明分とする
           IF      IDZ                  =   2
           AND     RECE02-HKNNUM        =   SPACE
      *
      *        沖縄県で精神障害者特別措置(121)の場合は不明分としない
               IF    ( RECE02-PREFNUM       =   "47"  )
               AND   ( RECE02-KOHNUM (1)    =   "121" )
                   CONTINUE
               ELSE
                   MOVE    6                    TO  RECE02-ERR-ERRKBN
                   GO  TO  RECESYUBETU-SET-EXT
               END-IF
           END-IF
      *
      *    平成２０年４月改正以降の種別設定
           IF      RECE02-SRYYM        >=  WRK-CONS-SRYYM-200804
      *
               INITIALIZE                  ORCSSYUBETUGETAREA
               MOVE    1               TO  LNK-SYUBETUGET-SYOFLG
               MOVE    RECE01-NYUGAIKBN
                                       TO  LNK-SYUBETUGET-NYUGAIKBN
               MOVE    RECE02-SRYYM    TO  LNK-SYUBETUGET-SRYYM
               MOVE    RECE02-HKNNUM   TO  LNK-SYUBETUGET-HKNNUM
               MOVE    RECE02-HONKZKKBN
                                       TO  LNK-SYUBETUGET-HKN-HONKZKKBN
               MOVE    RECE02-HOJOKBN  TO  LNK-SYUBETUGET-HKN-HOJOKBN
               IF      WRK-JIHI-FLG    =   "1"
                   MOVE    "8"             TO
                                       LNK-SYUBETUGET-HKN-HKNKOHSBTKBN
               END-IF
               IF      WRK-JIHI-FLG    =   "2"
                   MOVE    "9"             TO
                                       LNK-SYUBETUGET-HKN-HKNKOHSBTKBN
               END-IF
               MOVE    WRK-NENREI      TO  LNK-SYUBETUGET-AGE
               MOVE    RECE01-SYUGAKUKBN
                                       TO  LNK-SYUBETUGET-SYUGAKUKBN
               MOVE    RECE01-PREFNUM  TO  LNK-SYUBETUGET-PREFNUM
               MOVE    RECE02-RJNHKNNUM
                                       TO  LNK-SYUBETUGET-RJNHKNNUM
               MOVE    RECE02-RJNPAYKBN
                                       TO  LNK-SYUBETUGET-RJNPAYKBN
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       4
                   MOVE    RECE02-KOHNUM (IDX)     TO
                                       LNK-SYUBETUGET-KOHNUM     (IDX)
                   MOVE    RECE02-KOHHBTNUM (IDX)  TO
                                       LNK-SYUBETUGET-KOHHBTNUM  (IDX)
                   IF      RECE02-RECESKYKBN (IDX) =   SPACE
                       MOVE    "1"                 TO
                                       LNK-SYUBETUGET-RECESKYKBN (IDX)
                   END-IF 
               END-PERFORM
               IF      RECE02-KOREIHIGAITO  =   1
                   MOVE    SPACE       TO  LNK-SYUBETUGET-HKN-HOJOKBN
               END-IF
               MOVE    RECE02-BIRTHDAY TO  LNK-SYUBETUGET-BIRTHDAY
      *
      *        国保のとき９７７の公費の有無
               IF    ( LNK-SYUBETUGET-BIRTHDAY
                                           <   19440402 )
               AND   ( LNK-SYUBETUGET-HKNNUM
                                           =   "060"    )
               AND   ( LNK-SYUBETUGET-SRYYM
                                           >   "201404" )
                   MOVE    "99"                TO
                                               LNK-SYUBETUGET-RJNPAYKBN
      *
                   MOVE    LNK-SYUBETUGET-SRYYM
                                               TO  WRK-SRYYYMM
                   IF      RECE02-TEKSTYMD (1:6)
                                               >=  RECE01-SRYYM
                       MOVE  RECE02-TEKSTYMD (7:2) TO  WRK-SRYDD
                   ELSE
                       MOVE  01                    TO  WRK-SRYDD
                   END-IF
      *
                   INITIALIZE                      PTKOHINF-REC
                   MOVE    RECE01-HOSPNUM      TO  PTKOH-HOSPNUM
                   MOVE    RECE01-PTID         TO  PTKOH-PTID
                   MOVE    "977"               TO  PTKOH-KOHNUM
                   MOVE    PTKOHINF-REC        TO  MCPDATA-REC
                   MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                       MOVE    "key4"              TO  MCP-PATHNAME
                       PERFORM PTKOHINF-READ-SEC
                       PERFORM UNTIL   FLG-PTKOHINF    =   1
                           IF    ( PTKOH-TEKSTYMD  <=  WRK-SRYYMD     )
                           AND   ( WRK-SRYYMD      <=  PTKOH-TEKEDYMD )
                           AND   ( PTKOH-SAKJOKBN  =   SPACE           )
                               MOVE    SPACE           TO
                                               LNK-SYUBETUGET-RJNPAYKBN
                               MOVE    1               TO  FLG-PTKOHINF
                           ELSE
                               MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                               MOVE    "key4"          TO  MCP-PATHNAME
                               PERFORM  PTKOHINF-READ-SEC
                           END-IF
                       END-PERFORM
                   END-IF
                   MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                   MOVE    "key4"               TO  MCP-PATHNAME
                   PERFORM  900-CLOSE-SEC
               END-IF
      *
               CALL    "ORCSSYUBETUGET"    USING   ORCSSYUBETUGETAREA
      *
               MOVE    LNK-SYUBETUGET-RECESYUBETU
                                           TO  RECE02-RECESYUBETU
               MOVE    LNK-SYUBETUGET-RECESYUBETU-X
                                           TO  RECE02-RECESYUBETU-X
               MOVE    LNK-SYUBETUGET-RECESYUBETU-Y
                                           TO  RECE02-RECESYUBETU-Y
      *
      *        公費分で種別が設定できなかったとき不明分にしない
               IF    ( IDZ                 =   2    )
               AND   ( RECE02-RECESYUBETU  =   ZERO ) 
                   MOVE    5555                TO  RECE02-RECESYUBETU
               END-IF                 
      *
      *        DISPLAY   "RECE02-RECESYUBETU: " RECE02-RECESYUBETU
               GO  TO  RECESYUBETU-SET-EXT
           END-IF    
      *    
      *    自費のときのレセプト種別を編集
           IF     (RECE02-HKNNUM        NOT =   SPACE)   AND
                  (WRK-JIHI-FLG         =   "1"      ) 
               MOVE    8888             TO  RECE02-RECESYUBETU    
               GO  TO  RECESYUBETU-SET-EXT
           END-IF
      *         
           IF     (RECE02-HKNNUM    NOT =   SPACE)   AND
                  (WRK-JIHI-FLG         =   SPACE  OR  "2" )
               EVALUATE    RECE02-HKNNUM
                   WHEN    "060"
                   WHEN    "067"
                   WHEN    "068"
                   WHEN    "069"
                       IF      RECE02-HKNNUM   =   "060"   OR   "068"
                           MOVE   "1"      TO  WK-KOKUHO
                       ELSE
                           MOVE   "2"      TO  WK-KOKUHO
                       END-IF
                   WHEN    "039"
                   WHEN    "040"
                       MOVE   "3"          TO  WK-KOKUHO
                   WHEN    OTHER
                       MOVE    "1"         TO  WK-IHO
               END-EVALUATE
           END-IF
      *
           IF      RECE02-SRYYM     <      "200210" 
             IF      RECE02-RJNHKNNUM     =   "027"
                 MOVE        "1"          TO  WK-RJN
             END-IF
           END-IF
      *
           MOVE    ZERO            TO    WK-KOHI-CNT
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       4
               IF    ( RECE02-KOHNUM     (IDX) NOT =   SPACE )
               AND   ( RECE02-RECESKYKBN (IDX)     =   SPACE )
                   ADD     1               TO    WK-KOHI-CNT
               END-IF
           END-PERFORM    
      *
      *    山形地方公費対応
           IF    ( RECE02-PREFNUM      =   "06"     )
           AND   ( RECE02-SRYYM        <=  "200512" )
               IF  WK-KOKUHO               =     "1"  OR  "2"
                   IF   (RECE02-KOHNUM(1) (1:1)  =   "1")   OR
                        (RECE02-KOHNUM(2) (1:1)  =   "1")   OR
                        (RECE02-KOHNUM(3) (1:1)  =   "1")   OR
                        (RECE02-KOHNUM(4) (1:1)  =   "1")   OR
                        (RECE02-KOHNUM(1) (1:1)  =   "2")   OR
                        (RECE02-KOHNUM(2) (1:1)  =   "2")   OR
                        (RECE02-KOHNUM(3) (1:1)  =   "2")   OR
                        (RECE02-KOHNUM(4) (1:1)  =   "2")
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
               END-IF
           END-IF
      *
      *    福岡地方公費対応
           IF    ( RECE02-PREFNUM      =   "40"                  )
           AND   ( RECE02-SRYYM        <=  WRK-CONS-SRYYM-200609 )
               IF  WK-KOKUHO               =   "1" OR  "2"
                   IF   (RECE02-KOHNUM   (1)(1:1)  =   "1" OR  "2")
                   AND  (RECE02-KOHHBTNUM(1)   NOT =   "41"       )     
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
                   IF   (RECE02-KOHNUM   (2)(1:1)  =   "1" OR  "2")
                   AND  (RECE02-KOHHBTNUM(2)   NOT =   "41"       )     
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
                   IF   (RECE02-KOHNUM   (3)(1:1)  =   "1" OR  "2")
                   AND  (RECE02-KOHHBTNUM(3)   NOT =   "41"       )     
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
                   IF   (RECE02-KOHNUM   (4)(1:1)  =   "1" OR  "2")
                   AND  (RECE02-KOHHBTNUM(4)   NOT =   "41"       )     
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
               END-IF
           END-IF
      *   平成１４年１０月対応
      *    ３歳未満の場合、ＷＫ−ＨＯＮＫＺＫＫＢＮに"３"をセット
           IF  RECE01-HKNID             NOT =   ZERO
               IF    ( RECE02-SRYYM         >=  200210                )
               AND   ( RECE02-SRYYM         <   WRK-CONS-SRYYM-200804 )
                   IF      WRK-NENREI         <     3
                               MOVE   "3"     TO   WK-HONKZKKBN
                   END-IF
               END-IF
      *    平成２０年４月改正対応
               IF      RECE02-SRYYM        >=  WRK-CONS-SRYYM-200804
                   IF      RECE01-SYUGAKUKBN   =   1
                       MOVE   "3"          TO  WK-HONKZKKBN
                   END-IF
               END-IF
           END-IF
      *
      *    １割、２割負担の高齢者の場合ＷＫ−ＨＯＮＫＺＫＫＢＮに
      *    "９"または"８"をセット
      *    老人保険の場合はＷＫ−ＲＪＮにセットする
           IF  RECE02-HKNID             NOT =   ZERO
               EVALUATE    TRUE
                   WHEN    RECE02-SRYYM     >=  WRK-CONS-SRYYM-200804
                     EVALUATE    RECE02-HKNNUM
                       WHEN    "002" 
                           EVALUATE   RECE02-HOJOKBN
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   RECE02-HOJOKBN
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
                           IF     (WRK-NENREI  >=   70)  AND
                                  (WRK-NENREI  <=   74)
                               EVALUATE   RECE02-HOJOKBN
                                   WHEN   "1"
                                   WHEN   "4"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "2"
                                   WHEN   "5"
                                       MOVE   "8"   TO   WK-HONKZKKBN
                                   WHEN   "3"
                                   WHEN   "6"
                                       MOVE   "7"   TO   WK-HONKZKKBN
                               END-EVALUATE
                           END-IF     
                       WHEN    "039" 
                       WHEN    "040" 
                           EVALUATE   RECE02-HOJOKBN
                               WHEN   "1"
                                   MOVE   "9"   TO   WK-RJN
                               WHEN   "3"
                                   MOVE   "7"   TO   WK-RJN
                           END-EVALUATE
                       WHEN    "067" 
                       WHEN    "069" 
                           IF      WRK-NENREI  >=   65
                               MOVE   "?"   TO   WK-HONKZKKBN
                           ELSE
                               EVALUATE   RECE02-HOJOKBN
                                   WHEN   "9"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "8"
                                       MOVE   "?"   TO   WK-HONKZKKBN
                                   WHEN   "7"
                                       MOVE   "7"   TO   WK-HONKZKKBN
                               END-EVALUATE
                           END-IF
                       WHEN    OTHER 
                           EVALUATE   RECE02-HOJOKBN
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                     END-EVALUATE
                   WHEN    RECE02-SRYYM     >   WRK-CONS-SRYYM-200609
                     EVALUATE    RECE02-HKNNUM
                       WHEN    "002" 
                           EVALUATE   RECE02-HOJOKBN
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   RECE02-HOJOKBN
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
                           IF     (WRK-NENREI  >=   70)  AND
                                  (WRK-NENREI  <=   74)
                               EVALUATE   RECE02-HOJOKBN
                                   WHEN   "1"
                                   WHEN   "4"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "2"
                                   WHEN   "5"
                                       MOVE   "8"   TO   WK-HONKZKKBN
                                   WHEN   "3"
                                   WHEN   "6"
                                       MOVE   "7"   TO   WK-HONKZKKBN
                               END-EVALUATE
                           END-IF     
                       WHEN    OTHER 
                           EVALUATE   RECE02-HOJOKBN
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                     END-EVALUATE
                     IF      RECE02-RJNHKNID >   ZERO
                       EVALUATE   RECE02-RJNPAYKBN
                           WHEN   "01"
                               MOVE   "9"   TO   WK-RJN
                           WHEN   "02"
                               MOVE   "?"   TO   WK-RJN
                           WHEN   "03"
                               MOVE   "7"   TO   WK-RJN
                       END-EVALUATE
                     END-IF
                   WHEN    RECE02-SRYYM     >=      "200210"
                     EVALUATE    RECE02-HKNNUM
                       WHEN    "002" 
                           EVALUATE   RECE02-HOJOKBN
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   RECE02-HOJOKBN
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
                           IF     (WRK-NENREI  >=   70)  AND
                                  (WRK-NENREI  <=   74)
                               EVALUATE   RECE02-HOJOKBN
                                   WHEN   "1"
                                   WHEN   "4"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "2"
                                   WHEN   "5"
                                       MOVE   "8"   TO   WK-HONKZKKBN
                               END-EVALUATE
                           END-IF     
                       WHEN    OTHER 
                           EVALUATE   RECE02-HOJOKBN
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                     END-EVALUATE
                     IF      RECE02-RJNHKNID >   ZERO
                       EVALUATE   RECE02-RJNPAYKBN
                           WHEN   "01"
                               MOVE   "9"   TO   WK-RJN
                           WHEN   "02"
                               MOVE   "8"   TO   WK-RJN
                       END-EVALUATE
                     END-IF
               END-EVALUATE
           END-IF
      *
           IF    ( WK-RJN          NOT =   SPACE )
           AND   ( RECE02-HONKZKKBN    =   "1"   )     
               IF      RECE02-HKNNUM   =   "063"   OR  "072"   OR
                                           "073"   OR  "074"   OR
                                           "075"
                   MOVE    "2"          TO  WK-IHO
               END-IF
           END-IF
      *
      *          
      *----(01.00.13) LINE ADD END   --------------------------
      *
      *      DISPLAY "WK-HONKZKKBN: " WK-HONKZKKBN 
      *      DISPLAY "RECE01-NYUGAIKBN: " RECE01-NYUGAIKBN 
      *      DISPLAY "WK-IHO: " WK-IHO 
      *      DISPLAY "WK-KOKUHO: " WK-KOKUHO 
      *      DISPLAY "WK-RJN: " WK-RJN 
      *      DISPLAY "WK-KOHI-CNT: " WK-KOHI-CNT 
      *
           INITIALIZE                  ORCSSYUBETUGETAREA  
           MOVE    WK-KOKUHO           TO  LNK-SYUBETUGET-KOKUHO
           MOVE    WK-IHO              TO  LNK-SYUBETUGET-IHO
           MOVE    WK-RJN              TO  LNK-SYUBETUGET-RJN
           MOVE    WK-KOHI-CNT         TO  LNK-SYUBETUGET-KOHI-CNT
           MOVE    WK-HONKZKKBN        TO  LNK-SYUBETUGET-HONKZKKBN
           MOVE    RECE01-NYUGAIKBN    TO  LNK-SYUBETUGET-NYUGAIKBN
           MOVE    RECE02-SRYYM        TO  LNK-SYUBETUGET-SRYYM
           MOVE    RECE02-HKNNUM       TO  LNK-SYUBETUGET-HKNNUM
           CALL    "ORCSSYUBETUGET"    USING   ORCSSYUBETUGETAREA
      *
           MOVE    LNK-SYUBETUGET-RECESYUBETU
                                       TO  RECE02-RECESYUBETU
           MOVE    LNK-SYUBETUGET-RECESYUBETU-X
                                       TO  RECE02-RECESYUBETU-X
      *
      *    公費分で種別が設定できなかったとき不明分にしない
           IF    ( IDZ                 =   2    )
           AND   ( RECE02-RECESYUBETU  =   ZERO ) 
               MOVE    5555                TO  RECE02-RECESYUBETU
           END-IF                 
      *
      *     DISPLAY   "RECE02-RECESYUBETU: " RECE02-RECESYUBETU
           .
       RECESYUBETU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主保険−給付割合のセット
      *****************************************************************
       KYURATE-SET-SEC                SECTION.
      *
      *----(01.00.14) LINE MOD START --------------------------
      *平成１４年１０月改正対応
      *平成１４年９月まで有効
      *    補助区分から患者の負担割合を再セット
      *   （国保のみ）
           IF      RECE02-SRYYM     <=      "200209"
             IF    RECE02-HKNNUM           =    "060"
               EVALUATE   RECE02-HOJOKBN
                   WHEN   "0"
                          MOVE   ZERO      TO   RECE02-KYURATE
                   WHEN   "1"
                          MOVE   10        TO   RECE02-KYURATE
                   WHEN   "2"
                          MOVE   20        TO   RECE02-KYURATE
                   WHEN   "3"
                          MOVE   30        TO   RECE02-KYURATE
                   WHEN   "4"
                          MOVE   10        TO   RECE02-KYURATE
                   WHEN   "5"
                          MOVE   20        TO   RECE02-KYURATE
                   WHEN   "6"
                          MOVE   30        TO   RECE02-KYURATE
               END-EVALUATE
             ELSE
               IF    RECE02-HONKZKKBN        =    "1"
                     MOVE   WRK-HKN-HON-GAIKFTNRATE  TO  RECE02-KYURATE
               ELSE
                     MOVE   WRK-HKN-KZK-GAIKFTNRATE  TO  RECE02-KYURATE
               END-IF
             END-IF
           END-IF
      *----(01.00.14) LINE MOD START --------------------------
      *
      *----(01.00.14) LINE ADD START --------------------------
      *平成１４年１０月改正対応
      *平成１４年１０月以降有効
      *    主保険情報の補助区分から患者の負担割合を再セット
           IF    ( RECE02-SRYYM     >=      "200210" )
           AND   ( RECE02-SRYYM     <=      WRK-CONS-SRYYM-200609 ) 
               EVALUATE    RECE02-HKNNUM
                   WHEN    "060"
                   WHEN    "068"
                       IF       WRK-NENREI           <     3
                           IF    RECE02-HOJOKBN =    "0"  OR   "1" OR
                                                     "4"
                               EVALUATE   RECE02-HOJOKBN
                                  WHEN   "0"
                                      MOVE   ZERO    TO   RECE02-KYURATE
                                  WHEN   "1"
                                      MOVE   10      TO   RECE02-KYURATE
                                  WHEN   "4"
                                      MOVE   10      TO   RECE02-KYURATE
                               END-EVALUATE
                           ELSE
                               MOVE  20      TO    RECE02-KYURATE
                           END-IF
                       ELSE
                           EVALUATE   RECE02-HOJOKBN
                              WHEN   "0"
                                  MOVE   ZERO      TO   RECE02-KYURATE
                              WHEN   "1"
                                  MOVE   10        TO   RECE02-KYURATE
                              WHEN   "2"
                                  MOVE   20        TO   RECE02-KYURATE
                              WHEN   "3"
                                  MOVE   30        TO   RECE02-KYURATE
                              WHEN   "4"
                                  MOVE   10        TO   RECE02-KYURATE
                              WHEN   "5"
                                  MOVE   20        TO   RECE02-KYURATE
                              WHEN   "6"
                                  MOVE   30        TO   RECE02-KYURATE
                              WHEN   "8"
                                  MOVE   20        TO   RECE02-KYURATE
                              WHEN   "9"
                                  MOVE   10        TO   RECE02-KYURATE
                           END-EVALUATE
                       END-IF
                   WHEN    "067"
                   WHEN    "069"
                       IF       WRK-NENREI           <     3
                           MOVE  20      TO    RECE02-KYURATE
                       ELSE
                           IF      RECE02-HOJOKBN      NOT =  SPACE
                               EVALUATE   RECE02-HOJOKBN
                                   WHEN   "8"
                                       MOVE    20  TO  RECE02-KYURATE
                                   WHEN   "9"
                                       MOVE    10  TO  RECE02-KYURATE
                               END-EVALUATE
                           ELSE
                               IF   RECE02-HKNJANUM = "67330753"
                                 MOVE   20         TO  RECE02-KYURATE
                               ELSE
                                 IF RECE02-HONKZKKBN      =    "1"
                                   MOVE   WRK-HKN-HON-GAIKFTNRATE
                                                   TO  RECE02-KYURATE
                                 ELSE
                                   MOVE   WRK-HKN-KZK-GAIKFTNRATE
                                                   TO  RECE02-KYURATE
                                 END-IF
                               END-IF
                           END-IF
                       END-IF
                   WHEN    SPACE
                       MOVE    ZERO            TO  RECE02-KYURATE
                   WHEN    OTHER
                       IF      WRK-NENREI           <     3
                           MOVE    20          TO  RECE02-KYURATE
                       ELSE
                           IF      RECE02-HOJOKBN      =     SPACE
                               IF      RECE02-HONKZKKBN    =     "1"
                                   MOVE   WRK-HKN-HON-GAIKFTNRATE
                                                   TO  RECE02-KYURATE
                               ELSE
                                   MOVE   WRK-HKN-KZK-GAIKFTNRATE
                                                   TO  RECE02-KYURATE
                               END-IF
                           ELSE
                               EVALUATE   RECE02-HOJOKBN
                                   WHEN   "8"
                                       MOVE    20  TO  RECE02-KYURATE
                                   WHEN   "9"
                                       MOVE    10  TO  RECE02-KYURATE
                              END-EVALUATE
                          END-IF
                      END-IF
               END-EVALUATE
      *
               IF      RJN-RATE-KBN      NOT =    SPACE
                   EVALUATE   RJN-RATE-KBN
                       WHEN   "1"
                           MOVE   10        TO  RECE02-KYURATE
                       WHEN   "2"
                           MOVE   20        TO  RECE02-KYURATE
                   END-EVALUATE
               END-IF
           END-IF
      *
      *平成１８年１０月改正対応
      *    主保険情報の補助区分から患者の負担割合を再セット
           IF    ( RECE02-SRYYM    >   WRK-CONS-SRYYM-200609 )
           AND   ( RECE02-SRYYM    <   WRK-CONS-SRYYM-200804 ) 
               EVALUATE    RECE02-HKNNUM
                   WHEN    "060"
                   WHEN    "068"
                       IF       WRK-NENREI           <     3
                           IF    RECE02-HOJOKBN =    "0"  OR   "1" OR
                                                     "4"
                               EVALUATE   RECE02-HOJOKBN
                                  WHEN   "0"
                                      MOVE   ZERO    TO   RECE02-KYURATE
                                  WHEN   "1"
                                      MOVE   10      TO   RECE02-KYURATE
                                  WHEN   "4"
                                      MOVE   10      TO   RECE02-KYURATE
                               END-EVALUATE
                           ELSE
                               MOVE  20      TO    RECE02-KYURATE
                           END-IF
                       ELSE
                           EVALUATE   RECE02-HOJOKBN
                              WHEN   "0"
                                  MOVE   ZERO      TO   RECE02-KYURATE
                              WHEN   "1"
                                  MOVE   10        TO   RECE02-KYURATE
                              WHEN   "2"
                                  MOVE   20        TO   RECE02-KYURATE
                              WHEN   "3"
                                  MOVE   30        TO   RECE02-KYURATE
                              WHEN   "4"
                                  MOVE   10        TO   RECE02-KYURATE
                              WHEN   "5"
                                  MOVE   20        TO   RECE02-KYURATE
                              WHEN   "6"
                                  MOVE   30        TO   RECE02-KYURATE
                              WHEN   "8"
                                  MOVE   20        TO   RECE02-KYURATE
                              WHEN   "9"
                                  MOVE   10        TO   RECE02-KYURATE
                           END-EVALUATE
                       END-IF
                   WHEN    "067"
                   WHEN    "069"
                       IF       WRK-NENREI           <     3
                           MOVE  20      TO    RECE02-KYURATE
                       ELSE
                           IF      RECE02-HOJOKBN      NOT =  SPACE
                               EVALUATE   RECE02-HOJOKBN
                                   WHEN   "7"
                                       MOVE    30  TO  RECE02-KYURATE
                                   WHEN   "8"
                                       MOVE    20  TO  RECE02-KYURATE
                                   WHEN   "9"
                                       MOVE    10  TO  RECE02-KYURATE
                               END-EVALUATE
                           ELSE
                               IF   RECE02-HKNJANUM = "67330753"
                                 MOVE   20         TO  RECE02-KYURATE
                               ELSE
                                 IF RECE02-HONKZKKBN      =    "1"
                                   MOVE   WRK-HKN-HON-GAIKFTNRATE
                                                   TO  RECE02-KYURATE
                                 ELSE
                                   MOVE   WRK-HKN-KZK-GAIKFTNRATE
                                                   TO  RECE02-KYURATE
                                 END-IF
                               END-IF
                           END-IF
                       END-IF
                   WHEN    SPACE
                       MOVE    ZERO            TO  RECE02-KYURATE
                   WHEN    OTHER
                       IF      WRK-NENREI           <     3
                           MOVE    20          TO  RECE02-KYURATE
                       ELSE
                           IF      RECE02-HOJOKBN      =     SPACE
                               IF      RECE02-HONKZKKBN    =     "1"
                                   MOVE   WRK-HKN-HON-GAIKFTNRATE
                                                   TO  RECE02-KYURATE
                               ELSE
                                   MOVE   WRK-HKN-KZK-GAIKFTNRATE
                                                   TO  RECE02-KYURATE
                               END-IF
                           ELSE
                               EVALUATE   RECE02-HOJOKBN
                                   WHEN   "7"
                                       MOVE    30  TO  RECE02-KYURATE
                                   WHEN   "9"
                                       MOVE    10  TO  RECE02-KYURATE
                              END-EVALUATE
                          END-IF
                      END-IF
               END-EVALUATE
      *
               IF      RJN-RATE-KBN      NOT =    SPACE
                   EVALUATE   RJN-RATE-KBN
                       WHEN   "1"
                           MOVE   10        TO  RECE02-KYURATE
                       WHEN   "3"
                           MOVE   30        TO  RECE02-KYURATE
                   END-EVALUATE
               END-IF
           END-IF
      *
      *平成２０年４月改正対応
      *    主保険情報の補助区分から患者の負担割合を再セット
           IF    ( RECE02-SRYYM    >=  WRK-CONS-SRYYM-200804 ) 
               EVALUATE    RECE02-HKNNUM
                   WHEN    "060"
                   WHEN    "068"
                       IF       RECE02-SYUGAKUKBN    =   1
                           EVALUATE   RECE02-HOJOKBN
                              WHEN   "0"
                                  MOVE   ZERO    TO   RECE02-KYURATE
                              WHEN   "1"
                                  MOVE   10      TO   RECE02-KYURATE
                              WHEN   "4"
                                  MOVE   10      TO   RECE02-KYURATE
                              WHEN   OTHER
                                  MOVE   20      TO   RECE02-KYURATE
                           END-EVALUATE
                       ELSE
                           EVALUATE   RECE02-HOJOKBN
                              WHEN   "0"
                                  MOVE   ZERO      TO   RECE02-KYURATE
                              WHEN   "1"
                                  MOVE   10        TO   RECE02-KYURATE
                              WHEN   "2"
                                  MOVE   20        TO   RECE02-KYURATE
                              WHEN   "3"
                                  MOVE   30        TO   RECE02-KYURATE
                              WHEN   "4"
                                  MOVE   10        TO   RECE02-KYURATE
                              WHEN   "5"
                                  MOVE   20        TO   RECE02-KYURATE
                              WHEN   "6"
                                  MOVE   30        TO   RECE02-KYURATE
                              WHEN   "8"
                                  MOVE   20        TO   RECE02-KYURATE
                              WHEN   "9"
                                  MOVE   10        TO   RECE02-KYURATE
                           END-EVALUATE
                       END-IF
                   WHEN    "067"
                   WHEN    "069"
                       IF       RECE01-SYUGAKUKBN    =   1
                           MOVE  20      TO    RECE02-KYURATE
                       ELSE
                           IF      RECE02-HOJOKBN      NOT =  SPACE
                               EVALUATE   RECE02-HOJOKBN
                                   WHEN   "7"
                                       MOVE    30  TO  RECE02-KYURATE
                                   WHEN   "8"
                                       MOVE    20  TO  RECE02-KYURATE
                                   WHEN   "9"
                                       MOVE    10  TO  RECE02-KYURATE
                               END-EVALUATE
                           ELSE
                               IF  (RECE02-HKNJANUM  = "67330753")
                               AND (RECE02-SRYYM    <= "201109"  )
                                 MOVE   20         TO  RECE02-KYURATE
                               ELSE
                                 IF RECE02-HONKZKKBN      =    "1"
                                   MOVE   WRK-HKN-HON-GAIKFTNRATE
                                                   TO  RECE02-KYURATE
                                 ELSE
                                   MOVE   WRK-HKN-KZK-GAIKFTNRATE
                                                   TO  RECE02-KYURATE
                                 END-IF
                               END-IF
                           END-IF
                       END-IF
                   WHEN    SPACE
                       MOVE    ZERO            TO  RECE02-KYURATE
                   WHEN    "039"
                   WHEN    "040"
                       EVALUATE   RECE02-HOJOKBN
                           WHEN   "1"
                               MOVE    10  TO  RECE02-KYURATE
                           WHEN   "3"
                               MOVE    30  TO  RECE02-KYURATE
                       END-EVALUATE
                   WHEN    OTHER
                       IF       RECE01-SYUGAKUKBN    =   1
                           MOVE    20          TO  RECE02-KYURATE
                       ELSE
                           IF      RECE02-HOJOKBN      =     SPACE
                               IF      RECE02-HONKZKKBN    =     "1"
                                   MOVE   WRK-HKN-HON-GAIKFTNRATE
                                                   TO  RECE02-KYURATE
                               ELSE
                                   MOVE   WRK-HKN-KZK-GAIKFTNRATE
                                                   TO  RECE02-KYURATE
                               END-IF
                           ELSE
                               EVALUATE   RECE02-HOJOKBN
                                   WHEN   "7"
                                       MOVE    30  TO  RECE02-KYURATE
                                   WHEN   "9"
                                       MOVE    10  TO  RECE02-KYURATE
                                   WHEN   "8"
                                       MOVE    20  TO  RECE02-KYURATE
                              END-EVALUATE
                          END-IF
                      END-IF
               END-EVALUATE
           END-IF
      *
      *    船員保険（被保険者）＋通勤災害フラグセット
           IF    RECE02-HKNNUM           =    "002"
               EVALUATE   RECE02-HOJOKBN
                    WHEN   "3"
                    WHEN   "C"
                    WHEN   "F"
                    WHEN   "I"
                        MOVE     "1"     TO     RECE02-SENIN-TUUSAI
               END-EVALUATE
           END-IF
      *
           .
       KYURATE-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    組合せ公費保険番号情報のセット
      *****************************************************************
       KOHNUMHC-SET-SEC                SECTION.
      *
           IF      RECE01-KOHNUMHC (IDX 1) =   "027"
               MOVE    2                   TO  IDY1
           ELSE
               MOVE    1                   TO  IDY1
           END-IF
           MOVE    ZERO            TO  IDX2
           PERFORM VARYING IDY2    FROM    IDY1    BY  1
                   UNTIL   IDY2    >       4
               ADD     1                   TO  IDX2  
               MOVE    RECE01-KOHNUMHC (IDX IDY2)
                                           TO  WRK-HKNNUM
               PERFORM KOHNUM-CHECK-SEC
               IF      FLG-CHK         =   1
                   MOVE RECE01-KOHNUMHC (IDX IDY2) TO
                                           RECE02-KOHNUMHC (IDX IDX2)
                   MOVE RECE01-KOHIDHC  (IDX IDY2) TO
                                           RECE02-KOHIDHC  (IDX IDX2)
               END-IF
           END-PERFORM
           .
       KOHNUMHC-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       KOHNUM-CHECK-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *
           IF      WRK-HKNNUM          NOT =   SPACE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       10
                       OR      RECE01-KOHHKNNUM-O (IDY)  =   SPACE
                       OR      FLG-CHK     =  1
                   IF      WRK-HKNNUM      =   RECE01-KOHHKNNUM-O (IDY)
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-PERFORM
           END-IF             
      *
           .
       KOHNUM-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    在総診フラグセット
      *****************************************************************
       ZAITAKU-SET-SEC                SECTION.
      *
      *    高齢者判定フラグ
           IF     (( RECE02-AGE          >=  70      )   OR
                   ( RECE02-AGE          >=  65  AND
                     RECE01-KOHID    NOT  =   ZERO   )   OR
                   ( FLG-ROUJINHOKEN      =   1      )   OR
                   ( RECE02-HKNNUM        =  "039"   ))
           AND    (  RECE02-KOREIHIGAITO  =  ZERO     )
               MOVE    "1"             TO  RECE02-KOUREI-HANTEIFLG
           END-IF
      *
      *    自費または特別療養費、公費単独の場合は
      *    在宅総合・在宅末期フラグをクリアする
           IF      RECE02-RECESYUBETU  =   "8888"  OR  "6666"
               MOVE    SPACE           TO  RECE02-ZAITAKU
               GO  TO  ZAITAKU-SET-EXT
           END-IF
           IF      RECE02-HKNNUM        =   SPACE 
               MOVE    SPACE           TO  RECE02-ZAITAKU
               GO  TO  ZAITAKU-SET-EXT
           END-IF
      *
      *    対象でない患者のとき在宅総合・在宅末期フラグをクリアする
           MOVE    ZERO    TO   FLG-GENBUTU70
           MOVE    SPACE   TO   WRK-ZAITAKU
           IF     (( RECE02-AGE          >=  70      )   OR
                   ( RECE02-AGE          >=  65  AND
                     RECE01-KOHID    NOT  =   ZERO   )   OR
      ***          ( RECE02-AGE          >=  65  AND
      ***            FLG-ROUJINHOKEN      =   1      )   OR
      ***          ( RECE02-AGE          >=  65  AND
      ***            RECE02-HKNNUM        =  "039"   )
                   ( FLG-ROUJINHOKEN      =   1      )   OR
                   ( RECE02-HKNNUM        =  "039"   ))
           AND    (  RECE02-KOREIHIGAITO  =  ZERO     )
               MOVE   1    TO   FLG-GENBUTU70
           ELSE
             IF  RECE01-SRYYM    >=  "200704"
               MOVE   2    TO   FLG-GENBUTU70
             ELSE
               MOVE    SPACE           TO  RECE02-ZAITAKU
               GO  TO  ZAITAKU-SET-EXT
             END-IF
           END-IF
      *
           IF   RECE01-SRYYM    >=  "200210"
            IF       RECE01-SRYYM    >=  "201604"
      ***           PERFORM   VARYING   IDX3   FROM    1   BY   1
      ***                      UNTIL  ( IDX3   >   TBL-ZAITAKU6-MAX )
      ***                        OR   ( WRK-ZAITAKU    =    "1"     )
      ***              MOVE  TBL-ZAITAKU6(IDX3)  TO  WRK-ZAITAKU-SRYCD
      ***              PERFORM  ZAITAKU-HENSYU-SEC
      ***           END-PERFORM
                    CONTINUE
            ELSE
             IF      RECE01-SRYYM    >=  "201404"
                    PERFORM   VARYING   IDX3   FROM    1   BY   1
                               UNTIL  ( IDX3   >   TBL-ZAITAKU5-MAX )
                                 OR   ( WRK-ZAITAKU    =    "1"     )
                       MOVE  TBL-ZAITAKU5(IDX3)  TO  WRK-ZAITAKU-SRYCD
                       PERFORM  ZAITAKU-HENSYU-SEC
                    END-PERFORM
             ELSE
              IF     RECE01-SRYYM    >=  "201204"
                    PERFORM   VARYING   IDX3   FROM    1   BY   1
                               UNTIL  ( IDX3   >   TBL-ZAITAKU4-MAX )
                                 OR   ( WRK-ZAITAKU    =    "1"     )
                       MOVE  TBL-ZAITAKU4(IDX3)  TO  WRK-ZAITAKU-SRYCD
                       PERFORM  ZAITAKU-HENSYU-SEC
                    END-PERFORM
              ELSE
               IF    RECE01-SRYYM    >=  "200804"
                    PERFORM   VARYING   IDX3   FROM    1   BY   1
                               UNTIL  ( IDX3   >   TBL-ZAITAKU3-MAX )
                                 OR   ( WRK-ZAITAKU    =    "1"     )
                       MOVE  TBL-ZAITAKU3(IDX3)  TO  WRK-ZAITAKU-SRYCD
                       PERFORM  ZAITAKU-HENSYU-SEC
                    END-PERFORM
               ELSE
                IF   RECE01-SRYYM    >=  "200604"
                    PERFORM   VARYING   IDX3   FROM    1   BY   1
                               UNTIL  ( IDX3   >   TBL-ZAITAKU2-MAX )
                                 OR   ( WRK-ZAITAKU    =    "1"     )
                       MOVE  TBL-ZAITAKU2(IDX3)  TO  WRK-ZAITAKU-SRYCD
                       PERFORM  ZAITAKU-HENSYU-SEC
                    END-PERFORM
                ELSE
                    PERFORM   VARYING   IDX3   FROM    1   BY   1
                               UNTIL  ( IDX3   >   TBL-ZAITAKU-MAX  )
                                 OR   ( WRK-ZAITAKU    =    "1"     )
                       MOVE  TBL-ZAITAKU(IDX3)   TO  WRK-ZAITAKU-SRYCD
                       PERFORM  ZAITAKU-HENSYU-SEC
                    END-PERFORM
                END-IF
               END-IF
              END-IF
             END-IF
            END-IF
      *
            IF   ((WRK-ZAITAKU     =   "1"     ) AND
                  (RECE01-SRYYM   <=   "201203"))
            OR   ( RECE01-SRYYM   >=   "201204" )
               EVALUATE   FLG-GENBUTU70
                 WHEN        1
                    MOVE   "1"   TO  RECE02-ZAITAKU
                 WHEN        2
                    MOVE   "2"   TO  RECE02-ZAITAKU
               END-EVALUATE
            END-IF
      *
            IF    (WRK-ZAITAKU     =   "1"     )
            AND   (RECE01-SRYYM   >=   "201204")
            AND   (RECE01-SRYYM   <=   "201603")
               EVALUATE   FLG-GENBUTU70
                 WHEN        1
                    MOVE   "1"   TO  RECE02-ZAITAKU-SANTEI-FLG
               END-EVALUATE
            END-IF
      *
           END-IF
      *
           .
       ZAITAKU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    在総診編集
      *****************************************************************
       ZAITAKU-HENSYU-SEC                SECTION.
      *
           INITIALIZE                           SRYACT-REC
grpsys     MOVE    RECE01-HOSPNUM       TO      SRY-HOSPNUM
           MOVE    RECE01-NYUGAIKBN     TO      SRY-NYUGAIKBN
           MOVE    RECE01-PTID          TO      SRY-PTID
           MOVE    RECE01-SRYYM         TO      SRY-SRYYM
           MOVE    WRK-ZAITAKU-SRYCD    TO      SRY-SRYCD(1)
           MOVE    WRK-ZAITAKU-SRYCD    TO      SRY-SRYCD(2)
           MOVE    WRK-ZAITAKU-SRYCD    TO      SRY-SRYCD(3)
           MOVE    WRK-ZAITAKU-SRYCD    TO      SRY-SRYCD(4)
           MOVE    WRK-ZAITAKU-SRYCD    TO      SRY-SRYCD(5)
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key7"               TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"         TO  MCP-TABLE
               MOVE    "key7"               TO  MCP-PATHNAME
               PERFORM   SRYACT-READ-SEC
               PERFORM   UNTIL   FLG-SRYACT   =    1
                   INITIALIZE                    SRYACCT-REC
grpsys             MOVE    SRY-HOSPNUM      TO   ACCT-HOSPNUM
                   MOVE    SRY-NYUGAIKBN    TO   ACCT-NYUGAIKBN
                   MOVE    SRY-SRYKA        TO   ACCT-SRYKA
                   MOVE    SRY-PTID         TO   ACCT-PTID
                   MOVE    SRY-SRYYM        TO   ACCT-SRYYM
                   MOVE    SRY-ZAINUM       TO   ACCT-ZAINUM
                   MOVE    SRYACCT-REC      TO   MCPDATA-REC
                   MOVE    "tbl_sryacct"    TO   MCP-TABLE
                   MOVE    "key31"          TO   MCP-PATHNAME
grpsys             PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC           =       ZERO
                       MOVE    "tbl_sryacct"    TO   MCP-TABLE
                       MOVE    "key31"          TO   MCP-PATHNAME
                       PERFORM   SRYACCT-READ-SEC
                       IF      FLG-SRYACCT  =       ZERO
                         IF  WRK-ZAITAKU-SRYCD  = "099140003"
                           IF  (ACCT-ZAIKAISU NOT =  ZERO)
                             PERFORM  ZAITAKU-HENSYU-HKNKENSAKU-SEC
                           END-IF
                         ELSE
                           IF  (ACCT-ZAITEN   NOT =  ZERO)  AND
                               (ACCT-ZAIKAISU NOT =  ZERO)
                             PERFORM  ZAITAKU-HENSYU-HKNKENSAKU-SEC
                           END-IF
                         END-IF
                       END-IF
                   END-IF
                   MOVE    "tbl_sryacct"    TO   MCP-TABLE
                   MOVE    "key31"          TO   MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
      *
                   MOVE    "tbl_sryact"     TO  MCP-TABLE
                   MOVE    "key7"           TO  MCP-PATHNAME
                   PERFORM   SRYACT-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "key7"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       ZAITAKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    在総診保険検索
      *****************************************************************
       ZAITAKU-HENSYU-HKNKENSAKU-SEC                SECTION.
      *
           INITIALIZE                HKNCOMBI-REC
           MOVE    ACCT-HOSPNUM  TO  COMB-HOSPNUM
           MOVE    ACCT-PTID     TO  COMB-PTID
           MOVE    ACCT-HKNCOMBI TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC  TO  MCPDATA-REC
           PERFORM 910-HKNCOMBI-INV-SEC
           IF      FLG-HKNCOMBI  =   ZERO
              IF (COMB-HKNNUM = "971" OR "973" OR "975"
                             OR "068" OR "069" OR "040")
              OR (COMB-SYU-HKNKOHSBTKBN  =  "8" OR "9" )
              OR (COMB-KOH1HKNNUM = "970"              )
                 CONTINUE
              ELSE
               IF  RECE01-SRYYM   >=  "201204"
                 PERFORM  VARYING  IDX6   FROM   1   BY   1
                           UNTIL  (IDX6                   >   10 )
                             OR   (RECE02-HKNCOMBI(IDX6)  =  ZERO)
                    IF  RECE02-HKNCOMBI(IDX6) =  ACCT-HKNCOMBI
                      MOVE    "1"     TO  WRK-ZAITAKU
                      MOVE    10      TO  IDX6
                    END-IF
                 END-PERFORM
               ELSE
                 MOVE    "1"     TO  WRK-ZAITAKU
               END-IF
              END-IF
           END-IF
      *
           .
       ZAITAKU-HENSYU-HKNKENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       220-DATA-WRITE-SEC                SECTION.
      *
           WRITE   RECE02-REC
      *
           IF      STS-RECE02          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE02PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE02          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-OUT (IDZ)
      *
           .
       220-DATA-WRITE-EXT.
           EXIT.
      * 
      *****************************************************************
      *    保険履歴情報編集処理
      *****************************************************************
       230-RECE09-HENSYU-SEC                   SECTION.
      *
           MOVE    ZERO                TO  FLG-RIREKI-HEN
      *
           IF      RECE01-TEISEIKBN             =   SPACE
             IF  (     ( RECE01-HKNID           NOT =   ZERO  )
                   AND ( RECE01-HKNNUM(1:2)     NOT =   "98" AND
                                                        "90" AND
                                                        "91"  ) )
             OR  (     ( RECE01-HKNID               =   ZERO  )
                   AND ( RECE01-KOHHKNNUM-O (1)     =   "012" 
                                                    OR  "025"
                                                    OR  "015"
                                                    OR  "016"
                                                    OR  "021"
                                                    OR  "053" )
                   AND ( RECE01-KOHHKNNUM-O (2)     =   SPACE ) )
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       10
                       OR      RECE01-TEKSTYMDX (IDX)  =   SPACE
                 MOVE    SPACE                TO  RECE09-REC
                 INITIALIZE                       RECE09-REC
                 MOVE    RECE01-HOSPNUM       TO  RECE09-HOSPNUM
                 MOVE    RECE01-SRYYM         TO  RECE09-SRYYM
                 MOVE    RECE01-NYUGAIKBN     TO  RECE09-NYUGAIKBN
                 MOVE    RECE01-PTID          TO  RECE09-PTID
                 MOVE    RECE01-RECEKA        TO  RECE09-RECEKA
                 MOVE    "000"                TO  RECE09-KOHNUM
                 IF    ( RECE01-HKNID             =   ZERO  )
                 AND   ( RECE01-KOHHKNNUM-O (1)   =   "012" ) 
                 AND   ( RECE01-KOHHKNNUM-O (2)   =   SPACE )
                     MOVE    RECE01-KOH-KOHID     TO  RECE09-KOHID
                 END-IF
                 MOVE    RECE01-TEKSTYMDX (IDX)
                                              TO  RECE09-TEKSTYMD
                 PERFORM VARYING IDYY    FROM    1   BY  1
                         UNTIL   IDYY    >       100
      *            公費単独と主保険の摘要年月日が重複するとき
grpsys             IF    ( TBL-RECE09-HOSPNUM (IDYY)
                                             >   ZERO  )
                   AND   ( RECE01-HKNNUM NOT =   SPACE )
                   AND   ( TBL-RECE09-HKNNUM  (IDYY)
                                             =   SPACE )
grpsys                 IF      RECE01-TEKSTYMDX (IDX)
                                         >   TBL-RECE09-TEKEDYMDX (IDYY)
                       OR      RECE01-TEKEDYMDX (IDX)
                                         <   TBL-RECE09-TEKSTYMDX (IDYY)
                           CONTINUE
                       ELSE
                           INITIALIZE            TBL-RECE09-OCC   (IDYY)
                       END-IF
                   END-IF
      *
grpsys             IF      TBL-RECE09-HOSPNUM (IDYY)  =   ZERO
                     MOVE    RECE01-TEKSTYMDX (IDX)
                                                  TO  RECE09-TEKSTYMDX
                     MOVE    RECE01-TEKEDYMDX (IDX)
                                                  TO  RECE09-TEKEDYMDX
      * 
                     IF      RECE01-HKNNUM       =   "060"   OR  "067"
                                                 OR  "068"   OR  "069"
                         MOVE    2               TO  RECE09-TEISYUTUSAKI
                     ELSE
                         IF      RECE01-HKNNUM   =   "040"   OR  "039"
                             MOVE    6           TO  RECE09-TEISYUTUSAKI
                         ELSE
                             MOVE    1           TO  RECE09-TEISYUTUSAKI
                         END-IF
                     END-IF
      *
                     IF      RECE01-HKNNUM   NOT =   SPACE                                               
                         INITIALIZE                       PTHKNINF-REC
grpsys                   MOVE    RECE01-HOSPNUM       TO  PTHKN-HOSPNUM
                         MOVE    RECE01-PTID          TO  PTHKN-PTID
                         MOVE    RECE01-HKNNUM        TO  PTHKN-HKNNUM
                         MOVE    RECE01-TEKSTYMDX(IDX)
                                                      TO  PTHKN-TEKSTYMD
                         MOVE    RECE01-TEKEDYMDX(IDX)
                                                      TO  PTHKN-TEKEDYMD
                         MOVE    PTHKNINF-REC         TO  MCPDATA-REC
                         MOVE    "tbl_pthkninf"       TO  MCP-TABLE
                         MOVE    "key4"               TO  MCP-PATHNAME
grpsys                   PERFORM 900-DBSELECT-SEC
                         IF      MCP-RC               =   ZERO
                             MOVE    "tbl_pthkninf"   TO  MCP-TABLE
                             MOVE    "key4"           TO  MCP-PATHNAME
                             PERFORM PTHKNINF-READ-SEC
                         ELSE
                             MOVE    1                TO  FLG-PTHKNINF  
                         END-IF
      *
                         MOVE    ZERO                TO  FLG-RIREKI-HEN
      *
                         PERFORM          UNTIL   FLG-PTHKNINF     =   1
                                          OR      FLG-RIREKI-HEN   =   1
                           IF      PTHKN-SAKJOKBN   =   SPACE
      *                      後期高齢者で船員保険の履歴は対象外とする
                             IF  (   PTHKN-HKNNUM     =   "002"        )
                             AND (   PTHKN-HOJOKBN    =   "A" OR "B" OR
                                                          "C" OR "G" OR 
                                                          "H" OR "I" OR
                                                          "D" OR "E" OR
                                                          "F"          )
                                 IF    ( RECE01-AGE  >=  75    )
                                 OR    ( RECE01-AGE  =   74          AND
                                         RECE01-BIRTHDAY (5:2)
                                                 =
                                                   RECE01-SRYYM(5:2) AND 
                                         RECE01-BIRTHDAY (7:2)
                                             NOT =   "01"  ) 
                                     MOVE    1   TO  FLG-RIREKI-HEN
                                 END-IF  
                             END-IF
      *
                             PERFORM VARYING IDZZ    FROM    1   BY  1
                                     UNTIL   IDZZ    >       100
                                     OR      FLG-RIREKI-HEN   =   1
      *                          主保険が同じ保険があるときは対象外とする
                                 IF    ( TBL-RECE09-HOSPNUM  (IDZZ)
                                                     >   ZERO          )
                                 AND   ( TBL-RECE09-HKNNUM   (IDZZ)
                                                     =   PTHKN-HKNNUM  )
                                 AND   ( TBL-RECE09-HKNJANUM (IDZZ)
                                                     =   PTHKN-HKNJANUM)
                                 AND   ( TBL-RECE09-HONKZKKBN(IDZZ)
                                                     =
                                                     PTHKN-HONKZKKBN   )
                                 AND   ( TBL-RECE09-HOJOKBN  (IDZZ)
                                                     =   PTHKN-HOJOKBN )
                                 AND   ( TBL-RECE09-KIGO     (IDZZ)
                                                     =   PTHKN-KIGO    )
                                 AND   ( TBL-RECE09-NUM      (IDZZ)
                                                     =   PTHKN-NUM     )
                                     MOVE    1   TO  FLG-RIREKI-HEN
                                 END-IF
                             END-PERFORM
      *
                             IF      FLG-RIREKI-HEN  =   ZERO
                                 MOVE   PTHKN-HKNID  TO  RECE09-HKNID
                                 MOVE   PTHKN-HKNNUM TO  RECE09-HKNNUM
                                 MOVE   PTHKN-HKNJANUM
                                                     TO  RECE09-HKNJANUM
                                 MOVE   PTHKN-HONKZKKBN
                                                     TO RECE09-HONKZKKBN
                                 MOVE   PTHKN-HOJOKBN
                                                     TO  RECE09-HOJOKBN
                                 MOVE   PTHKN-CONTKBN
                                                     TO  RECE09-CONTKBN
                                 IF     PTHKN-HKNNUM NOT =   "039"     
                                     MOVE   PTHKN-KIGO   TO  RECE09-KIGO
                                 END-IF
                                 MOVE   PTHKN-NUM    TO  RECE09-NUM
                                 MOVE   SPACE        TO  RECE09-CERTNUM
      *         
                                 INITIALIZE                  HKNNUM-REC
grpsys                           MOVE   RECE01-HOSPNUM
                                                     TO  HKN-HOSPNUM
                                 MOVE   PTHKN-HKNNUM TO  HKN-HKNNUM
                                 MOVE   RECE01-TEKSTYMDX (IDX)
                                                     TO  HKN-TEKSTYMD
                                 MOVE   RECE01-TEKEDYMDX (IDX)
                                                     TO  HKN-TEKEDYMD
                                 MOVE   HKNNUM-REC   TO  MCPDATA-REC
                                 MOVE   "tbl_hknnum" TO  MCP-TABLE
                                 MOVE   "key5"       TO  MCP-PATHNAME
grpsys                           PERFORM 900-DBSELECT-SEC
                                 IF     MCP-RC       =   ZERO
                                   MOVE   "tbl_hknnum"   TO MCP-TABLE
                                   MOVE   "key5"         TO MCP-PATHNAME
                                   PERFORM HKNNUM-READ-SEC
                                   IF      FLG-HKNNUM    =   ZERO
                                       MOVE   HKN-TANSEIDONAME
                                                 TO  RECE09-TANSEIDONAME
                                       MOVE   HKN-HBTNUM
                                                 TO RECE09-HKNHBTNUM
                                   END-IF
                                 END-IF
                                 MOVE   "tbl_hknnum" TO  MCP-TABLE
                                 MOVE   "key5"       TO  MCP-PATHNAME
                                 PERFORM 900-CLOSE-SEC
                                 MOVE    1           TO  FLG-PTHKNINF
                             END-IF
                           END-IF
      * 
                           MOVE    "tbl_pthkninf"    TO  MCP-TABLE
                           MOVE    "key4"            TO  MCP-PATHNAME
                           PERFORM PTHKNINF-READ-SEC
                         END-PERFORM
                         MOVE    "tbl_pthkninf"      TO  MCP-TABLE
                         MOVE    "key4"              TO  MCP-PATHNAME
                         PERFORM 900-CLOSE-SEC
      *
                         IF      FLG-RIREKI-HEN      =   ZERO
                           MOVE    RECE09-REC        TO
                                                 TBL-RECE09-OCC (IDYY)
                         END-IF
                         MOVE    100               TO  IDYY
                     ELSE                                            
                       INITIALIZE                      PTKOHINF-REC
                       MOVE    RECE01-HOSPNUM      TO  PTKOH-HOSPNUM
                       MOVE    RECE01-PTID         TO  PTKOH-PTID
                       MOVE    RECE01-KOHID-O (1)  TO  PTKOH-KOHID
                       MOVE    PTKOHINF-REC        TO  MCPDATA-REC
                       MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM 900-DBSELECT-SEC
                       IF      MCP-RC              =   ZERO
                           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                           MOVE    "key"               TO  MCP-PATHNAME
                           PERFORM  PTKOHINF-READ-SEC
                       ELSE
                           MOVE    1                TO  FLG-PTKOHINF  
                       END-IF
                       MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
      *
                       IF      FLG-PTKOHINF    =   ZERO
                           MOVE    PTKOH-FTNJANUM  TO  RECE09-KIGO
      *
                         IF      PTKOH-KOHNUM    =   "012"   OR  "025"
                           MOVE    RECE01-HOSPNUM  TO
                                                   MONTHLYNUM-HOSPNUM
                           MOVE    RECE01-PTID     TO  MONTHLYNUM-PTID
                           MOVE    PTKOH-KOHID     TO  MONTHLYNUM-KOHID
                           MOVE    RECE01-SRYYM    TO  MONTHLYNUM-SRYYM
                           MOVE    RECE01-NYUGAIKBN
                                                   TO
                                                   MONTHLYNUM-NYUGAIKBN
                           MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
                           MOVE    "tbl_monthlynum"
                                                   TO  WRK-MCP-TABLE
                           MOVE    "key4"          TO  WRK-MCP-PATHNAME
                           PERFORM 910-MONTHLYNUM-READ-SEC
                           IF      FLG-MONTHLYNUM  =   ZERO
                               MOVE    MONTHLYNUM-JKYSNUM 
                                                   TO  RECE09-NUM
                           ELSE                       
                               MOVE    RECE01-HOSPNUM
                                                   TO
                                                   MONTHLYNUM-HOSPNUM
                               MOVE    RECE01-PTID TO  MONTHLYNUM-PTID
                               MOVE    ZERO        TO  MONTHLYNUM-KOHID
                               MOVE    RECE01-SRYYM
                                                   TO  MONTHLYNUM-SRYYM
                               MOVE    RECE01-NYUGAIKBN
                                                   TO
                                                   MONTHLYNUM-NYUGAIKBN
                               MOVE    MONTHLYNUM-REC
                                                   TO  MCPDATA-REC
                               MOVE    "tbl_monthlynum"
                                                   TO  WRK-MCP-TABLE
                               MOVE    "key4"      TO  WRK-MCP-PATHNAME
                               PERFORM 910-MONTHLYNUM-READ-SEC
                               IF      FLG-MONTHLYNUM  =   ZERO
                                   MOVE    MONTHLYNUM-JKYSNUM 
                                                       TO  RECE09-NUM
                               ELSE                       
                                   MOVE   PTKOH-JKYSNUM
                                                       TO  RECE09-NUM
                               END-IF
                           END-IF    
                         ELSE                       
                             MOVE   PTKOH-JKYSNUM  TO  RECE09-NUM
                         END-IF
      *         
                         INITIALIZE                  HKNNUM-REC
                         MOVE   RECE01-HOSPNUM   TO  HKN-HOSPNUM
                         MOVE   PTKOH-KOHNUM     TO  HKN-HKNNUM
                         MOVE   RECE01-TEKSTYMDX (IDX)
                                                 TO  HKN-TEKSTYMD
                         MOVE   RECE01-TEKEDYMDX (IDX)
                                                 TO  HKN-TEKEDYMD
                         MOVE   HKNNUM-REC       TO  MCPDATA-REC
                         MOVE   "tbl_hknnum"     TO  MCP-TABLE
                         MOVE   "key5"           TO  MCP-PATHNAME
                         PERFORM 900-DBSELECT-SEC
                         IF     MCP-RC           =   ZERO
                             MOVE   "tbl_hknnum"     TO  MCP-TABLE
                             MOVE   "key5"           TO  MCP-PATHNAME
                             PERFORM HKNNUM-READ-SEC
                             IF      FLG-HKNNUM      =   ZERO
                                 MOVE   HKN-TANSEIDONAME
                                               TO  RECE09-TANSEIDONAME
                             END-IF
                         END-IF
                         MOVE   "tbl_hknnum"     TO  MCP-TABLE
                         MOVE   "key5"           TO  MCP-PATHNAME
                         PERFORM 900-CLOSE-SEC
      *
                         MOVE    RECE09-REC        TO
                                               TBL-RECE09-OCC (IDYY)
                         MOVE    100               TO  IDYY
                       END-IF
                     END-IF
                   END-IF  
                 END-PERFORM
               END-PERFORM
             END-IF           
           END-IF           
      *
           .
       230-RECE09-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    患者特記事項情報取得処理
      *****************************************************************
       230-PTTOKKI-HENSYU-SEC          SECTION.
      *
           INITIALIZE                      TABLE-PTTOKKI-AREA
           MOVE    ZERO                TO  IDXX
      *
           IF      RECE01-SRYYM        <   WRK-CONS-SRYYM-200804
               GO  TO   230-PTTOKKI-HENSYU-EXT
           END-IF
      *
           INITIALIZE                      PTTOKKI-REC
           MOVE    RECE01-HOSPNUM      TO  PTTOKKI-HOSPNUM
           MOVE    RECE01-PTID         TO  PTTOKKI-PTID
           MOVE    "1"                 TO  PTTOKKI-NYUGAIKBN
           MOVE    RECE01-SRYYM        TO  PTTOKKI-STYM
                                           PTTOKKI-EDYM
           MOVE    PTTOKKI-REC         TO  MCPDATA-REC
           MOVE    "tbl_pttokki"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pttokki"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-PTTOKKI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTTOKKI  
           END-IF
      *
           PERFORM             UNTIL   FLG-PTTOKKI     =   1
                                       OR  IDXX        >=  100
               ADD     1               TO  IDXX
               MOVE    PTTOKKI-CD      TO  TBL-PTTOKKI-CD    (IDXX)
               MOVE    PTTOKKI-NAIYO   TO  TBL-PTTOKKI-NAIYO (IDXX)
      * 
               MOVE    "tbl_pttokki"   TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               PERFORM 900-PTTOKKI-READ-SEC
           END-PERFORM
      * 
           MOVE    "tbl_pttokki"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    IDXX                TO  TBL-PTTOKKI-MAX
      *
           .
       230-PTTOKKI-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    入退院履歴より情報取得処理
      *****************************************************************
       240-PTNYUINRRK-SYORI-SEC                   SECTION.
      *
      *    保険組合せ取得
           INITIALIZE                      WRK-DAY-AREA
                                           WRK-HKNCOMBIINF-AREA
                                           WRK-HKNCOMBI-TABLE
           MOVE    LOW-VALUE           TO  KEY-AREA
      *
      *    入院会計読み込み
           INITIALIZE                      NYUINACCT-REC
grpsys     MOVE    RECE02-HOSPNUM      TO  NACCT-HOSPNUM
           MOVE    RECE02-PTID         TO  NACCT-PTID
           MOVE    RECE02-SRYYM        TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
      *
      *    入院会計がないとき記載なし 
           IF      FLG-NYUINACCT       =   1
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
               GO  TO  240-PTNYUINRRK-SYORI-EXT
           END-IF
      *   
      *    保険組合せ編集
           PERFORM UNTIL       FLG-NYUINACCT     =   1
               EVALUATE    NACCT-ZAISKBKBN
                   WHEN    "1"
                       MOVE    1               TO  IDX
                   WHEN    "5"
                       MOVE    2               TO  IDX
               END-EVALUATE
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1    >       31
                   IF      NACCT-DAY  (IDX1)       >   ZERO
                       MOVE    NACCT-DAY (IDX1)    TO
                                                   WRK-DAY (IDX IDX1)
                   END-IF
               END-PERFORM            
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    入院会計より保険組合せ読込     
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       31
               IF      WRK-DAY (1 IDX)     >   ZERO    AND
                       WRK-DAY (2 IDX)     >   ZERO
      *
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       10
      *
      *                保険組合せ追加
                       IF      WRK-COMB-HKNCOMBINUM (IDX1) =   ZERO
                           INITIALIZE                  HKNCOMBI-REC
grpsys                     MOVE    RECE02-HOSPNUM  TO  COMB-HOSPNUM
                           MOVE    RECE02-PTID     TO  COMB-PTID
                           MOVE    WRK-DAY (2 IDX) TO  COMB-HKNCOMBINUM 
                           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
                           PERFORM 910-HKNCOMBI-INV-SEC
      *                   （対象外の組合せは１をセット）
                           IF      FLG-HKNCOMBI    =   ZERO
                               MOVE    HKNCOMBI-REC    TO
                                           WRK-HKNCOMBI-REC  (IDX1)
                               IF      COMB-HKNNUM     =
                                           WRK-CONS-ROUSAI-HKNNUM OR
                                           WRK-CONS-JIBAI-HKNNUM  OR
                                           WRK-CONS-KOUGAI-HKNNUM 
                                   MOVE    "1"         TO
                                           WRK-HKNCOMBI-SYOKBN (IDX1)
                               ELSE
      *                            自費入院分の履歴は記載しない場合
                                   IF      SYS-2005-NYUTAIINREKI-KSIKBN
                                                           =   "2"
                                       INITIALIZE          HKNNUM-REC
                                       MOVE   RECE02-HOSPNUM
                                                       TO  HKN-HOSPNUM
                                       MOVE   COMB-HKNNUM
                                                       TO  HKN-HKNNUM
                                       PERFORM 900-HKNNUM-CHK-SEC
                                       IF      HKN-HKNKOHSBTKBN
                                                       =   "8"
                                           MOVE    "1"     TO
                                           WRK-HKNCOMBI-SYOKBN (IDX1)
                                       END-IF
                                   END-IF
                               END-IF
                           ELSE
                               MOVE    WRK-DAY (2 IDX) TO
                                           WRK-COMB-HKNCOMBINUM (IDX1)
                               MOVE    "1"             TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
                           END-IF
                       END-IF    
                       IF      WRK-DAY (2 IDX)     =
                                           WRK-COMB-HKNCOMBINUM (IDX1)
                           MOVE    10              TO  IDX1
                       END-IF            
                   END-PERFORM
               END-IF
           END-PERFORM    
      *
      *    入院会計の保険組合せを追加
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >       10
                   OR      WRK-COMB-HKNCOMBINUM (IDX2) =   ZERO
               IF      WRK-HKNCOMBI-SYOKBN (IDX2)  =   SPACE
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       10
                       IF      WRK-HKNCOMBI (IDX)  =   ZERO
                           MOVE    WRK-COMB-HKNCOMBINUM (IDX2)  TO
                                                   WRK-HKNCOMBI (IDX)
                           MOVE    10                   TO  IDX
                       ELSE
                           IF      WRK-COMB-HKNCOMBINUM (IDX2)   =
                                                   WRK-HKNCOMBI (IDX)
                               MOVE    10                   TO  IDX
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF 
           END-PERFORM
      *
      *    診療会計読み込み
           INITIALIZE                      SRYACCT-REC
grpsys     MOVE    RECE02-HOSPNUM      TO  ACCT-HOSPNUM
           MOVE    RECE02-PTID         TO  ACCT-PTID
           MOVE    RECE02-SRYYM        TO  ACCT-SRYYM
           MOVE    "1"                 TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key37"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO   MCP-TABLE
               MOVE    "key37"             TO   MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           PERFORM UNTIL       FLG-SRYACCT     =   1
      *        保険組合せ読込み
               INITIALIZE                  HKNCOMBI-REC
grpsys         MOVE    RECE02-HOSPNUM  TO  COMB-HOSPNUM
               MOVE    RECE02-PTID     TO  COMB-PTID
               MOVE    ACCT-HKNCOMBI   TO  COMB-HKNCOMBINUM 
               MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
               PERFORM  910-HKNCOMBI-INV-SEC
      *
               IF      FLG-HKNCOMBI    =   ZERO
                   IF      COMB-HKNNUM     =   WRK-CONS-ROUSAI-HKNNUM OR
                                               WRK-CONS-JIBAI-HKNNUM  OR
                                               WRK-CONS-KOUGAI-HKNNUM
                       CONTINUE
                   ELSE    
      *                自費入院分の履歴は記載しない場合
                       INITIALIZE                  HKNNUM-REC
                       MOVE    RECE02-HOSPNUM  TO  HKN-HOSPNUM
                       MOVE    COMB-HKNNUM     TO  HKN-HKNNUM
                       PERFORM 900-HKNNUM-CHK-SEC
                       IF    ( SYS-2005-NYUTAIINREKI-KSIKBN
                                               =   "2" )
                       AND   ( HKN-HKNKOHSBTKBN
                                               =   "8" )
                           CONTINUE
                       ELSE
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX >       10
                               IF      WRK-HKNCOMBI (IDX)      =   ZERO
                                   MOVE    COMB-HKNCOMBINUM    TO
                                                   WRK-HKNCOMBI (IDX)
                                   MOVE    10                  TO  IDX
                               ELSE
                                   IF      COMB-HKNCOMBINUM    =
                                                   WRK-HKNCOMBI (IDX)
                                       MOVE    10              TO  IDX
                                   END-IF
                               END-IF
                          END-PERFORM
                       END-IF
                   END-IF
               END-IF    
      *
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM     UNTIL   FLG-SRYACCT     =   1 
                           OR      KEY-NEW     NOT =   KEY-OLD
                   MOVE    "tbl_sryacct"    TO   MCP-TABLE
                   MOVE    "key37"          TO   MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               END-PERFORM
           END-PERFORM
      *
           MOVE    "tbl_sryacct"    TO   MCP-TABLE
           MOVE    "key37"          TO   MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
                   OR      WRK-COMB-HKNCOMBINUM (IDX) =   ZERO
      *       主保険情報読み出し
               IF      WRK-COMB-HKNID (IDX)    NOT =   ZERO
                   INITIALIZE                      PTHKNINF-REC
grpsys             MOVE    RECE02-HOSPNUM      TO  PTHKN-HOSPNUM
                   MOVE    RECE02-PTID         TO  PTHKN-PTID
                   MOVE    WRK-COMB-HKNID (IDX)
                                               TO  PTHKN-HKNID
                   MOVE    WRK-COMB-TEKSTYMD (IDX)
                                               TO  PTHKN-TEKSTYMD
                                                   PTHKN-TEKEDYMD
                   MOVE    PTHKNINF-REC        TO  MCPDATA-REC
                   PERFORM 910-PTHKNINF-INV-SEC
                   MOVE    PTHKN-TEKSTYMD      TO  WRK-TEKSTYMDX (IDX)
                   MOVE    PTHKN-TEKEDYMD      TO  WRK-TEKEDYMDX (IDX)
               END-IF
           END-PERFORM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
                   OR      WRK-COMB-HKNCOMBINUM (IDX)  =   ZERO
               IF      WRK-TEKSTYMD-X  =   SPACE 
                   MOVE    WRK-TEKSTYMDX (IDX) TO  WRK-TEKSTYMD-X
                   MOVE    WRK-TEKEDYMDX (IDX) TO  WRK-TEKEDYMD-X
               ELSE
                   IF      WRK-TEKSTYMD-X >   WRK-TEKSTYMDX (IDX)
                       MOVE    WRK-TEKSTYMDX (IDX)
                                               TO  WRK-TEKSTYMD-X
                   END-IF
                   IF      WRK-TEKEDYMD-X <   WRK-TEKEDYMDX (IDX)
                       MOVE    WRK-TEKEDYMDX (IDX)
                                               TO  WRK-TEKEDYMD-X
                   END-IF
               END-IF
           END-PERFORM
      *
           INITIALIZE                      WRK-SHONUM
      *
      *    診療月内の入退院情報
           INITIALIZE                      PTNYUINRRK-REC
grpsys     MOVE    RECE02-HOSPNUM      TO  PTNYUINRRK-HOSPNUM
           MOVE    RECE02-PTID         TO  PTNYUINRRK-PTID
           MOVE    RECE02-SRYYM        TO  PTNYUINRRK-NYUINYMD (1:6)
           MOVE    "31"                TO  PTNYUINRRK-NYUINYMD (7:2)
           MOVE    RECE02-SRYYM        TO  PTNYUINRRK-TAIINYMD (1:6)
           MOVE    "01"                TO  PTNYUINRRK-TAIINYMD (7:2)
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-PTNYUINRRK-KEY23-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTNYUINRRK23
           END-IF
           PERFORM                 UNTIL   FLG-PTNYUINRRK23    =   1
             IF    ( PTNYUINRRK-TENNYUYMD  >   WRK-TEKEDYMD-X )
             AND   ( WRK-TEKEDYMD-X    NOT =   SPACE          )
                 CONTINUE
             ELSE
               IF      PTNYUINRRK-NYUINYMD (1:6)
                                               <   RECE02-SRYYM
                   MOVE    1                   TO  IDX4
               ELSE
                   MOVE    PTNYUINRRK-NYUINYMD TO  WRK-SRYYMD
                   MOVE    WRK-SRYDD           TO  IDX4
               END-IF 
               IF      PTNYUINRRK-TAIINYMD (1:6)
                                               >   RECE02-SRYYM
                   MOVE    31                  TO  IDX5
               ELSE
                   MOVE    PTNYUINRRK-TAIINYMD TO  WRK-SRYYMD
                   MOVE    WRK-SRYDD           TO  IDX5
               END-IF
      *        保険組合せのチェック（会計に対象の組合せがないときは全て）
               IF      WRK-DAY-AREA        =   ALL "0"
                   PERFORM 2401-PTNYUINRRK-HENSYU-SEC
               ELSE
                   PERFORM VARYING IDX FROM    IDX4    BY  1
                           UNTIL   IDX >       IDX5
                       IF      IDX             =   IDX4
      *                    同日再入院のとき保険組合せ取得
                           INITIALIZE               ORCSNYUINDAY-AREA
                           MOVE    RECE02-HOSPNUM
                                               TO  LNK-NYUINDAY-HOSPNUM
                           MOVE    RECE02-PTID TO  LNK-NYUINDAY-PTID
                           MOVE    PTNYUINRRK-NYUINYMD (1:6)
                                               TO  LNK-NYUINDAY-SANTEIYM
                           CALL    "ORCSNYUINDAY"
                                               USING   ORCSNYUINDAY-AREA
                                                       SPA-AREA
                           IF    ( LNK-NYUINDAY-RC  =   ZERO )
                           AND   ( LNK-NYUINDAY-DOUJITSU-KBN (IDX)
                                                    =   "1"  )
                               PERFORM VARYING IDY FROM    1   BY  1
                                       UNTIL   IDY >       10
                                       OR  WRK-HKNCOMBI (IDY)   =   ZERO
                                   IF     LNK-NYUINDAY-DOUJITSU-HKNCOMBI
                                                               (IDX)
                                               =   WRK-HKNCOMBI (IDY)
                                       PERFORM
                                           2401-PTNYUINRRK-HENSYU-SEC
                                       MOVE    10              TO  IDY
                                       MOVE   IDX5             TO  IDX
                                   END-IF
                              END-PERFORM
                           ELSE    
                               PERFORM VARYING IDY FROM    1   BY  1
                                       UNTIL   IDY >       10
                                       OR  WRK-HKNCOMBI (IDY)  =   ZERO
                                   IF      WRK-DAY (2 IDX)
                                               =   WRK-HKNCOMBI (IDY)
                                       PERFORM
                                           2401-PTNYUINRRK-HENSYU-SEC
                                       MOVE    10              TO  IDY
                                       MOVE    IDX5            TO  IDX
                                   END-IF
                               END-PERFORM
                           END-IF
                       ELSE
                           PERFORM VARYING IDY FROM    1   BY  1
                                   UNTIL   IDY >       10
                                   OR  WRK-HKNCOMBI (IDY)   =   ZERO
                               IF      WRK-DAY (2 IDX)
                                               =   WRK-HKNCOMBI (IDY)
                                   PERFORM 2401-PTNYUINRRK-HENSYU-SEC
                                   MOVE    10                  TO  IDY
                                   MOVE   IDX5                 TO  IDX
                               END-IF
                           END-PERFORM
                       END-IF
                   END-PERFORM
               END-IF
             END-IF          
      *                 
             PERFORM 900-PTNYUINRRK-KEY23-READ-SEC
           END-PERFORM        
      *                 
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *                                                                
           .
       240-PTNYUINRRK-SYORI-EXT.
           EXIT.
      * 
      *****************************************************************
      *    入退院履歴情報編集処理
      *****************************************************************
       2401-PTNYUINRRK-HENSYU-SEC           SECTION.
      *
           IF      WRK-SHONUM          =   ZERO
               MOVE    PTNYUINRRK-SHONUM   TO  WRK-SHONUM
           END-IF
      *
      *    入退院履歴情報
           PERFORM 24012-RECE12-WRITE-SEC
      *
      *    診療月内の入退院情報の履歴
           IF      FLG-RECE12          =   1                            
               MOVE    PTNYUINRRK-REC  TO  WRK-PTNRRK-REC
               INITIALIZE                  PTNYUINRRK-REC
grpsys         MOVE    RECE01-HOSPNUM  TO  PTNYUINRRK-HOSPNUM
               MOVE    RECE01-PTID     TO  PTNYUINRRK-PTID
               MOVE    WRK-PTNRRK-SHONUM
                                       TO  PTNYUINRRK-SHONUM
               MOVE    RECE01-SRYYM    TO  PTNYUINRRK-NYUINYMD (1:6)
               MOVE    "31"            TO  PTNYUINRRK-NYUINYMD (7:2)
               MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
               MOVE    "tbl_ptnyuinrrk"
                                       TO  MCP-TABLE
               MOVE    "key51"         TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   PERFORM 900-PTNYUINRRK-KEY22-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-PTNYUINRRK22 
               END-IF
      *
               PERFORM             UNTIL   FLG-PTNYUINRRK22 =  1
                   PERFORM 24012-RECE12-WRITE-SEC
                   PERFORM 900-PTNYUINRRK-KEY22-READ-SEC
               END-PERFORM
      *                 
               MOVE    "tbl_ptnyuinrrk"
                                       TO  MCP-TABLE
               MOVE    "key51"         TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
           .
       2401-PTNYUINRRK-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入退院履歴出力処理
      *****************************************************************
       24012-RECE12-WRITE-SEC         SECTION.
      *
      *    他院歴の履歴は記載しない
           IF    ( PTNYUINRRK-JTIKBN        =   "5" )
           AND   ( PTNYUINRRK-TAINRELKBN    =   "1" )
               GO  TO  24012-RECE12-WRITE-EXT
           END-IF
      *
           INITIALIZE                      RECE12-REC
grpsys     MOVE    RECE02-KEY          TO  RECE12-KEY1
           MOVE    PTNYUINRRK-NYUINYMD TO  RECE12-NYUINYMD
           IF      PTNYUINRRK-TAIINYMD =   "99999999"
               MOVE    1                   TO  RECE12-NYUINYMD-SEQNO
           ELSE
               MOVE    ZERO                TO  RECE12-NYUINYMD-SEQNO
           END-IF
           MOVE    RECE12-REC          TO  WRK-RECE12-REC
           PERFORM 900-RECE12-READ-SEC
           IF      FLG-RECE12          =   1
               MOVE    WRK-RECE12-REC      TO  RECE12-REC
               MOVE    PTNYUINRRK-TAIINYMD TO  RECE12-TAIINYMD
               MOVE    PTNYUINRRK-NYUINCHUKBN
                                           TO  RECE12-NYUINCHUKBN
      *        入院年月日と同じ初回番号のときのみセット
               IF      PTNYUINRRK-SHONUM   =    WRK-SHONUM
                   MOVE    WRK-SHONUM           TO  RECE12-SHONUM
               END-IF
               WRITE   RECE12-REC
      *
               IF      STS-RECE12          =   "00"
                   CONTINUE
               ELSE
                   MOVE    SPACE               TO  WRK-ERR-AREA
                   INITIALIZE                      WRK-ERR-AREA
                   MOVE    3                   TO  WRK-ERR-FLG
                   MOVE    RECE12PARA          TO  WRK-ERR-FILE-NM
                   MOVE    STS-RECE12          TO  WRK-ERR-FILE-STS
                   PERFORM 500-FILE-ERR-ABORT-SEC
               END-IF
      *
               ADD     1                   TO  CNT-RECE12
               MOVE    "1"                 TO  RECE02-NYUINRRK-FLG
           END-IF    
           .
       24012-RECE12-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費単独レセの出力処理
      *****************************************************************
       250-SEIHO-RECE01-WRITE-SEC      SECTION.
      *
      *    主保険とのまとめがなく生保、中国残留邦人等、自立支援以外との
      *    公費単独の場合
           IF    ( WRK-SEIHO-FLG           =   ZERO )
           AND   (  WRK-KOH-01-HOSPNUM     >   ZERO )
               MOVE    WRK-KOH-RECE01-REC  TO  RECE01-REC
      *        ファイル編集処理
               DISPLAY "SEH"
               PERFORM 2101-RECE02-HENSYU-SEC
           ELSE
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       WRK-SEIHO-RECE01-MAX
                   IF      WRK-SEIHO-RECE01-PRTFLG (IDXX)
                                           =   1
                       CONTINUE
                   ELSE
                       MOVE    WRK-SEIHO-RECE01-REC (IDXX)
                                               TO  RECE01-REC
      *                ファイル編集処理
               DISPLAY "SEH"
                       PERFORM 2101-RECE02-HENSYU-SEC
                   END-IF
               END-PERFORM
           END-IF
           .
       250-SEIHO-RECE01-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
             IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
             END-IF
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルエラー処理
      *****************************************************************
       500-FILE-ERR-ABORT-SEC           SECTION.
      *
           EVALUATE    WRK-ERR-FLG
               WHEN    1
                   MOVE    "ファイルＯＰＥＮエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file open err" TO  WRK-ERR-FILE-MSG1
               WHEN    2
                   MOVE    "ファイル読み込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file read err" TO  WRK-ERR-FILE-MSG1
               WHEN    3
                   MOVE    "ファイル書き込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file write err"
                                           TO  WRK-ERR-FILE-MSG1
               WHEN    4
                   MOVE    "ファイル更新エラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file rewrite err"
                                           TO  WRK-ERR-FILE-MSG1
           END-EVALUATE
      *
           STRING  WRK-ERR-FILE-MSG        DELIMITED   BY  SPACE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   " STS="                 DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
           END-STRING
           PERFORM 500-ERR-HENSYU-SEC
      *
           STRING  "ORCR0020 "             DELIMITED   BY  SIZE
                   WRK-ERR-FILE-MSG1       DELIMITED   BY  SIZE
                   " FILE="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   "  STS="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                   LOW-VALUE               DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
           END-STRING
           PERFORM 500-COBABORT-SEC
      *
           .
       500-FILE-ERR-ABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中止処理
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     =   1    )
                   MOVE    "処理が中止されました"
                                           TO  WRK-RECEERR
      *
                   OPEN    INPUT   RECEERR-FILE
                   IF      STS-RECEERR         =   ZERO
                       CLOSE   RECEERR-FILE
                   ELSE
                       OPEN    OUTPUT              RECEERR-FILE
      *    
                       MOVE    WRK-RECEERR         TO  RECEERR-REC
                       WRITE   RECEERR-REC
                       CLOSE   RECEERR-FILE
      *         
      *                ジョブ管理終了処理
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
grpsys                 PERFORM 900-CALL-ORCSJOB-SEC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           IF      CNT-OUT (1)          =   ZERO
           AND     CNT-OUT (2)          =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *    ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE01  CNT-IN  :" CNT-IN  (1) 
           DISPLAY "RECE02  CNT-OUT :" CNT-OUT (1) 
           DISPLAY "RECE09  CNT-OUT :" CNT-RECE09  
           DISPLAY "RECE12  CNT-OUT :" CNT-RECE12  
           DISPLAY "RECE01X CNT-IN  :" CNT-IN  (2) 
           DISPLAY "RECE02X CNT-OUT :" CNT-OUT (2) 
           DISPLAY "*** ORCR0020 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル名取得処理
      *****************************************************************
       800-FILENM-SET-SEC        SECTION.
      *
           MOVE    WRK-PARA-TERMID     TO  RECEDAT1PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEDAT1PARA-HOSPNUM
           .
       800-FILENM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報読込
      *****************************************************************
       PTHKNINF-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           .
       PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       PTKOHINF-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       PTKOHINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者労災保険情報読込
      *****************************************************************
       PTRSIINF-READ-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           .
       PTRSIINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスタ検索処理
      *****************************************************************
       900-HKNNUM-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM
           MOVE    HKN-HKNNUM          TO  WRK-CHK-HKNNUM
      *
           INITIALIZE                      HKNNUM-REC
      *
      *    保険番号マスタ退避ワークチェック
      *
           PERFORM VARYING WRK-HKNNUM-IDX    FROM    1   BY  1
                   UNTIL   WRK-HKNNUM-IDX    >   WRK-HKNNUM-MAX
                   OR      HKN-HKNNUM        NOT =   SPACE
               IF    ( WRK-CHK-HKNNUM  =
                                       TBL-HKN-HKNNUM (WRK-HKNNUM-IDX) )
               AND   ( TBL-HKN-TEKSTYMD (WRK-HKNNUM-IDX) (1:6)
                                       <=  RECE02-SRYYM                )
               AND   ( RECE02-SRYYM    <=
                               TBL-HKN-TEKEDYMD(WRK-HKNNUM-IDX)(1:6)   )
                   MOVE    TBL-HKNNUM-REC (WRK-HKNNUM-IDX)
                                       TO  HKNNUM-REC
               END-IF
           END-PERFORM
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF      HKN-HKNNUM          =   SPACE
               INITIALIZE                      HKNNUM-REC
grpsys         MOVE   RECE02-HOSPNUM       TO  HKN-HOSPNUM
               MOVE   WRK-CHK-HKNNUM       TO  HKN-HKNNUM
               MOVE   HKNNUM-REC           TO  MCPDATA-REC
               MOVE   "tbl_hknnum"         TO  MCP-TABLE
               MOVE   "key2"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF     MCP-RC               =   ZERO
                   MOVE   "tbl_hknnum"         TO  MCP-TABLE
                   MOVE   "key2"               TO  MCP-PATHNAME
                   PERFORM HKNNUM-READ-SEC
                   PERFORM UNTIL ( FLG-HKNNUM  =    1                 )
                           OR    ((HKN-TEKSTYMD (1:6)
                                               <=  RECE02-SRYYM     )
                                    AND
                                  (RECE02-SRYYM
                                               <=  HKN-TEKEDYMD(1:6)) )
                       MOVE   "tbl_hknnum"         TO  MCP-TABLE
                       MOVE   "key2"               TO  MCP-PATHNAME
                       PERFORM HKNNUM-READ-SEC
                   END-PERFORM
      *
                   IF      FLG-HKNNUM          =   ZERO
      *                退避ワークに編集
                       IF      TBL-HKNNUM-CONST-MAX
                                                   >   WRK-HKNNUM-MAX
                           COMPUTE WRK-HKNNUM-MAX
                                           =   WRK-HKNNUM-MAX  +   1
                           MOVE    HKNNUM-REC      TO
                                       TBL-HKNNUM-REC (WRK-HKNNUM-MAX)
                       END-IF
                   ELSE
                       INITIALIZE                  HKNNUM-REC
                       MOVE    1               TO  FLG-HKNNUM
                   END-IF
               ELSE
                   INITIALIZE                  HKNNUM-REC
                   MOVE    1               TO  FLG-HKNNUM
               END-IF
      *
               MOVE   "tbl_hknnum"         TO  MCP-TABLE
               MOVE   "key2"               TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
      *
       900-HKNNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険番号読込
      *****************************************************************
       HKNNUM-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       SRYACCT-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       SRYACT-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       SRYACT-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      *
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       960-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-NEXT-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-KANRIMST-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    月代わり受給者番号マスタ読込
      *****************************************************************
       910-MONTHLYNUM-READ-SEC         SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC               =   ZERO
                   MOVE    ZERO                 TO  FLG-MONTHLYNUM
                   MOVE    MCPDATA-REC          TO  MONTHLYNUM-REC
               ELSE
                   MOVE    1                   TO  FLG-MONTHLYNUM
               END-IF
           ELSE
               MOVE    1                   TO  FLG-MONTHLYNUM
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-MONTHLYNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ　ＲＥＡＤ
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYACCT
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           IF      FLG-SRYACCT         =   ZERO
               MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
           END-IF
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスタ　ＲＥＡＤ
      *****************************************************************
       900-NYUINACCT-READ-SEC          SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-NYUINACCT
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読込
      *****************************************************************
       910-HKNCOMBI-INV-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   IF      COMB-DELKBN         =   "1"
                       MOVE    1                   TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                      HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                      HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    入院履歴読込（ＫＥＹ２２）
      *****************************************************************
       900-PTNYUINRRK-KEY22-READ-SEC    SECTION.
      *    
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key51"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC               =   ZERO
               MOVE    ZERO                 TO  FLG-PTNYUINRRK22
               MOVE    MCPDATA-REC          TO  PTNYUINRRK-REC
               IF      PTNYUINRRK-JTIKBN    =   "5"
                   IF      PTNYUINRRK-TAINRELKBN    =   "1"
                       CONTINUE
                   ELSE
                       GO  TO  900-PTNYUINRRK-KEY22-READ-SEC
                   END-IF
               END-IF
           ELSE
               MOVE    1                    TO  FLG-PTNYUINRRK22
           END-IF
           .
       900-PTNYUINRRK-KEY22-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴読込（ＫＥＹ１３）
      *****************************************************************
       900-PTNYUINRRK-KEY23-READ-SEC    SECTION.
      *    
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC               =   ZERO
               MOVE    ZERO                 TO  FLG-PTNYUINRRK23
               MOVE    MCPDATA-REC          TO  PTNYUINRRK-REC
           ELSE
               MOVE    1                    TO  FLG-PTNYUINRRK23
           END-IF
           .
       900-PTNYUINRRK-KEY23-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスタ読込
      *****************************************************************
       910-PTHKNINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"     TO  MCP-TABLE
           MOVE    "key3"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"     TO  MCP-TABLE
               MOVE    "key3"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTHKNINF
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
                   IF      PTHKN-SAKJOKBN      =   "1"
                       MOVE    1                   TO  FLG-PTHKNINF
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"     TO  MCP-TABLE
           MOVE    "key3"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-PTHKNINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    旧姓履歴マスタ読み込み
      *****************************************************************
       900-KYUSEIRRK-READ-SEC          SECTION.
      *
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  KYUSEIRRK-REC
                   MOVE    ZERO                TO  FLG-KYUSEIRRK
               ELSE
                   MOVE    1                   TO  FLG-KYUSEIRRK
               END-IF
           ELSE
               MOVE    1               TO  FLG-KYUSEIRRK
           END-IF
      *
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-KYUSEIRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者特記事項読込
      *****************************************************************
       900-PTTOKKI-READ-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTTOKKI-REC
               MOVE    ZERO            TO  FLG-PTTOKKI
           ELSE
               INITIALIZE                  PTTOKKI-REC
               MOVE    1               TO  FLG-PTTOKKI
           END-IF
      *
           .
       900-PTTOKKI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理
      *****************************************************************
       RECE01-READ-SEC            SECTION.
      *
           READ    RECE01-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-RECE01
               NOT  AT  END
                   IF      STS-RECE01          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE01PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE01          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   ADD     1     TO    CNT-IN (IDZ)
                                       CNT-IN-CANCEL
                   MOVE    RECE01-PTID
                                 TO    KEY-N-PTID
                   MOVE    RECE01-SRYYM
                                 TO    KEY-N-SRYYM
                   MOVE    RECE01-RECEIPT-KBN
                                 TO    KEY-N-RECEIPT-KBN
      *
                   DISPLAY "RECE01 " IDZ " PTID=" RECE01-PTID 
                                         " PTNUM=" RECE01-PTNUM 
                           " RECEIPT-KBN=" RECE01-RECEIPT-KBN
           END-READ
           .
       RECE01-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入退院履歴読込
      *****************************************************************
       900-RECE12-READ-SEC            SECTION.
      *
           READ    RECE12-FILE
               INVALID
                   MOVE    1           TO  FLG-RECE12
               NOT INVALID
                   MOVE    ZERO        TO  FLG-RECE12
           END-READ
           .
       900-RECE12-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "ＤＢ読み込みエラー　TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCR0020 select err TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " PATHNAME="        DELIMITED   BY  SIZE
                       MCP-PATHNAME        DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
