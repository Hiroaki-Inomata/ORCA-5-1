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
      *
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCR0105.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : レセプト印刷用ファイル作成（医保）
      *                      ２００８年４月改正以降
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/03/06    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.02.01    NACL-門脇    08/03/17  宮崎県地方公費（１９０）の対応
      *  04.02.02    NACL-藤原    08/03/25  特記事項の変更
      *                                     診療区分６４：病理診断対応
      *  04.02.03    NACL-門脇    08/04/08  北海道道老廃止
      *  04.02.04    NACL-藤原    08/04/18  「レセ電通信医２０００６号」
      *                                     症状詳記区分コード５２の追加
      *  04.02.05    NACL-藤原    08/04/24  続紙の変更
      *                                     ７０歳代前半の特例措置対象者
      *                                     対応 
      *                                     国保のみ２割の記載
      *  04.02.06    NACL-門脇    08/06/12  愛媛県地方公費対応
      *                                     （一部負担金再掲欄編集）
      *  04.02.07    NACL-門脇    08/06/25  全国公費２種の一部負担金等記載対応
      *  04.02.08    NACL-門脇    08/10/02  北海道特定疾患対応
      *
      *  04.03.01    NACL-藤原    08/05/07  公費４種までの併用の記録対応
      *  04.03.02    NACL-藤原    08/06/02  リアルタイムプレビュー時の表示
      *  04.03.03    NACL-藤原    08/06/19  医師別出力対応
      *  04.03.04    NACL-門脇    08/07/07  複数保険組み合わせ使用時の
      *                                     請求点・実日数記載対応
      *  04.03.05    NACL-藤原    08/07/09  仮レセ作成（院外処方含む）対応
      *  04.03.06    NACL-藤原    08/08/05  患者登録特記事項入力対応
      *  04.03.07    NACL-藤原    08/07/23  宮城県、岩手県地震対応
      *                                     ２００８年６月〜対象 
      *  04.03.08    NACL-門脇    08/09/02  請求点判定追加
      *  04.03.09    NACL-藤原    08/09/24  平成２０年１０月改正対応
      *  04.03.10    NACL-門脇    08/12/16  平成２１年１月改正対応
      *  04.03.11    NACL-門脇    09/01/15  平成２１年１月改正対応
      *  04.03.12    NACL-藤原    09/01/16  症状詳記区分の表題の変更
      *  04.03.13    NACL-門脇    09/02/09  公費分点数編集修正
      *
      *  04.04.01    NACL-門脇    09/02/16  地方公費のレセ記載対応
      *  04.04.02    NACL-藤原    09/01/14  月途中の生保変更時の保険履歴
      *                                     記載対応
      *                                     生保まとめ分点欄記載対応
      *  04.04.03    NACL-門脇    09/05/11  平成２１年５月改正対応
      *  04.04.04    NACL-藤原    09/08/17  「平成２１年７月中国・九州
      *                                     北部豪雨」対応
      *                                     ２００９年７月〜対象 
      *  04.05.05    NACL-藤原    09/09/18  「平成２１年台風第９号」対応
      *                                     兵庫県・岡山県
      *                                     ２００９年８月〜２０１０年１月対象 
      *  04.04.06    NACL-藤原    09/10/21  月途中の負担者番号変更対応
      *                                     （自立支援の公費単独） 
      *  04.04.07    NACL-藤原    09/12/14  摘要欄記載の改頁処理の修正
      *  04.04.08    NACL-門脇    10/02/16  レセ記載しない地方公費がある場合
      *                                     の保険欄一部負担金編集対応
      *  04.04.09    NACL-門脇    10/04/30  再掲欄の記載条件修正
      *  04.04.10    NACL-門脇    10/05/21  生保まとめ分点欄記載対応
      *  04.04.11    NACL-藤原    10/08/23  後期高齢非該当対応
      *                                     （高齢受給者票がない場合） 
      *
      *  04.05.01    NACL-門脇    09/06/10  摘要欄下部へのコメント記載対応
      *  04.05.02    NACL-藤原    09/06/18  社保レセプト遍綴順対応
      *  04.05.03    NACL-藤原    09/11/13  特記事項照会対応
      *                                     特記事項略称追加
      *  04.05.04    NACL-門脇    10/02/08  地方公費の全国公費併用分請求点
      *                                     集計対応
      *  04.05.05    NACL-藤原    10/03/03  平成２２年４月改正対応
      *                                     平成２２年４月分以降の処理を
      *                                     行わない
      *  04.05.06    NACL-藤原    10/03/12  平成２２年４月改正対応
      *                                     平成２２年４月分以降の政府管
      *                                     掌をエラーとする
      *  04.05.07    NACL-藤原    10/03/29  平成２２年４月改正対応
      *                                     平成２２年４月分以降の処理を
      *                                     行うようにする
      *  04.05.08    NACL-藤原    10/04/13  愛知県対応
      *                                     生保と特定疾患９１の記載順 
      *  04.05.09    NACL-藤原    10/08/11  「平成２２年７月中国地方豪雨」対応
      *                                     広島県・山口県
      *                                     ２０１０年７月〜２０１０年１２月対象 
      *  04.05.10    NACL-藤原    10/11/16  「鹿児島県奄美市等における局地的大雨」対応
      *                                     鹿児島県
      *                                     ２０１０年１０月〜２０１１年３月対象 
      *  04.05.11    NACL-藤原    10/09/14  医療観察法対応
      *                                     公費単独のレセプトとする
      *  04.05.12    NACL-門脇    11/02/17  災害対応修正（平成２３年１月〜）
      *  04.05.14    NACL-藤原    11/02/22  レセ電データの記号番号区切り
      *                                     文字設定対応
      *  04.05.15    NACL-藤原    11/03/24  災害対応
      *  04.05.16    NACL-門脇    11/03/25  災害対応（災２対応）
      *  04.05.17    NACL-藤原    11/03/25  災害対応
      *                                     「不詳」のとき連絡先記載
      *  04.05.18    NACL-藤原    11/03/29  災害対応
      *                                     「不詳・災１」の追加
      *  04.05.19    NACL-藤原    11/03/30  災害対応
      *                                     特記事項９６、９７の追加
      *  04.05.20    NACL-藤原    11/04/01  災害対応
      *                                     「〒」「ＴＥＬ：」の記載追加
      *  04.05.21    NACL-藤原    11/04/04  災害対応
      *                                     震災以前一部負担金の記載追加
      *  04.05.22    NACL-藤原    11/04/18  災害対応
      *                                     災２の保険履歴記載の判定削除
      *  04.05.23    NACL-藤原    11/04/20  災害対応
      *                                     公費負担医療の記載対応
      *  04.05.24    NACL-門脇    11/06/20  公費分離区分区切り編集修正
      *  04.05.25    NACL-藤原    11/08/02  点検レセ印字位置変更対応
      *  04.05.26    NACL-門脇    11/08/08  ７０歳未満現物給付の摘要欄
      *                                     低所得記載修正
      *  04.05.27    NACL-藤原    11/08/25  公費単独対応
      *                                     保険履歴を最大１００に変更
      *  04.05.28    NACL-藤原    11/09/05  「請求しない」設定の請求管理
      *                                     の保険の記号番号を更新する
      *  04.05.29    NACL-藤原    11/10/24  「請求しない」設定の請求管理
      *                                     の削除処理の有無対応
      *  04.05.30    NACL-門脇    11/12/13  災害に関するコメント修正
      *  04.05.31    NACL-藤原    11/12/16  東大阪市国保の減免対応
      *  04.05.32    NACL-門脇    12/04/10  平成２４年４月改正
      *                                     現物給付化対応
      *                                     平成２４年４月以降の社保分に
      *                                     ついて在総診の設定を行わない
      *  04.05.33    NACL-門脇    12/04/24  保険欄−一部負担金欄記載に関
      *                                     するフラグ編集
      *  04.05.34    NACL-門脇    12/06/28  生保まとめ分点欄摘要欄記載対応
      *  04.05.35    NACL-門脇    12/09/19  保険欄−一部負担金欄金額置換対応
      *  04.05.36    NACL-門脇    12/10/22  生保まとめ分点欄点数集計修正
      *  04.05.37    NACL-藤原    12/11/05  摘要欄の公費情報の記載方法修正
      *  04.05.38    NACL-門脇    12/11/08  公費欄−一部負担金欄金額置換対応
      *
      *  04.06.01    NACL-藤原    10/06/23  レセ電請求時の受付エラー分の
      *                                     種別不明レセ対応
      *                                     病名なし
      *                                     単位コードなし
      *                                     生保の受給者番号なし
      *  04.06.02    NACL-門脇    10/09/13  出産一時金対応（外来）
      *  04.06.03    NACL-藤原    10/10/04  レセプトコメント文字数増対応
      *  04.06.04    NACL-門脇    10/12/13  保険別請求チェック表の本体取込み
      *  04.06.05    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *                                     調査用にcrehms,updhms 追加
      *  04.06.06    NACL-藤原    11/10/07  傷病名を３行まで記載可能とする
      *  04.06.07    NACL-藤原    11/12/12  点検レセ０点公費印字対応
      *  04.06.08    NACL-藤原    12/07/04  表示期間によるレセプト記載対応
      *  04.06.09    NACL-藤原    12/10/04  同一レセプトの公費が４つを超
      *                                     えた場合の種別不明対応
      *  04.06.10    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *  04.06.11    NACL-藤原    13/04/03  表示期間によるレセプトへの転
      *                                     帰日有無による病名文字の変更
      *  04.06.12    NACL-藤原    13/06/12  国保で本人・家族の変更があるが
      *                                     給付割合の変更がないときは１枚
      *                                     にまとめるときの公費データの
      *                                     更新方法の修正
      *
      *  04.07.01    NACL-藤原    11/06/02  入退院履歴記載対応
      *                                     「2自費分は含まない」を追加
      *  04.07.02    NACL-門脇    11/09/01  システム管理２０１０機能追加対応
      *  04.07.03    NACL-藤原    12/01/24  monpe-0.6.20対応
      *  04.07.04    NACL-藤原    12/02/13  健保組合への直接請求と審査
      *                                     支払機関請求対応
      *  04.07.05    NACL-藤原    12/09/24  点検用レセプトの医師名記載対応
      *  04.07.06    NACL-藤原    12/11/20  リアルタイムプレビュー時の
      *                                     一画面対応
      *  04.07.07    NACL-藤原    12/12/03  ターミナルデジットによる出力対応
      *  04.07.08    NACL-藤原    13/02/07  労災レセ電対応
      *  04.07.09    NACL-門脇    13/09/11  自賠責（第三者行為）対応
      *  04.07.10    NACL-門脇    13/11/11  減額（円）の対応
      *  04.07.11    NACL-藤原    13/12/09  医療情報連携基盤対応
      *  04.07.12    NACL-門脇    14/02/03  第三者行為対応（院外処方のみの
      *                                     保険組み合わせの病名記載対応）
      *  04.07.13    NACL-藤原    13/12/09  点検用レセプトの医師名記載修正
      *  04.07.14    NACL-小豆沢  14/04/25  平成２６年４月改正対応
      *  04.07.15    NACL-藤原    14/04/22  ＥＦファイル作成対応
      *  04.07.16    NACL-藤原    14/06/02  医保分で受診日指定時、医保＋
      *                                     第三者行為も対象とする
      *  04.07.17    NACL-藤原    14/06/06  未請求解除時の削除処理追加
      *  04.07.18    NACL-門脇    14/07/10  （感染症＋生保＋生保）レセプト
      *                                     の点数欄記載修正
      *  04.07.19    NACL-藤原    14/12/03  平成２６年１１月改正対応
      *  04.07.20    NACL-門脇    14/12/05  平成２７年１月改正対応
      *  04.07.21    NACL-門脇    15/01/07  平成２７年１月改正対応
      *                                     （公費請求テーブル編集）
      *  04.07.22    NACL-門脇    15/02/09  第三者行為のコメント編集条件修正
      *  04.07.23    NACL-門脇    15/03/17  請求点数０点公費レセ記載対応
      *  04.07.24    NACL-藤原    15/06/04  ＥＦファイルに種別不明分があ
      *                                     れば処理終了
      *  04.07.25    NACL-藤原    15/07/30  「下船後３ヶ月以内」の記録の修正
      *
      *  04.08.01    NACL-藤原    12/01/08  包括診療分の点検用レセプト記
      *                                     載対応
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.03    NACL-藤原    15/02/04  請求管理への公費情報の更新
      *  04.08.04    NACL-藤原    15/09/01  保険履歴の記載対応
      *  04.08.05    NACL-門脇    16/02/08  RECE02の予備項目使用分について、
      *                                     個々に項目を設ける事に伴う修正。
      *                                     第３第４公費の括弧再掲記載対応
      *  04.08.06    NACL-門脇    16/03/15  生保まとめ分点欄記載修正
      *  04.08.07    NACL-藤原    16/04/01  平成２８年４月改正対応
      *  04.08.08    NACL-藤原    16/09/06  レセ電請求時の受付エラー分の
      *                                     種別不明レセ対応
      *                                     残留法人等の負担者番号、受給者番号なし
      *  04.08.09    NACL-門脇    16/09/27  保険欄−一部負担金の減額コメント
      *                                     記載修正
      *  04.08.10    NACL-門脇    16/09/28  自賠責（第三者行為）
      *                                     保険別請求チェック表の機能追加
      *  04.08.11    NACL-藤原    17/03/02  未コード化特定器材コード廃止対応
      *  04.08.12    NACL-門脇    17/03/17  保険別請求チェック表の機能追加
      *                                     （公費請求書に関わる対応）
      *  04.08.13    NACL-門脇    17/03/27  請求金額０円レセプト作成対応
      *  04.08.14    NACL-藤原    17/04/12  請求点数不一致時の種別不明対応
      *  04.08.15    NACL-藤原    17/05/26  直接請求する保険者に地方職員
      *                                     共済組合を追加
      *  04.08.16    NACL-門脇    17/07/03  （災１）等の記載を括弧なしで
      *                                     記載するよう変更
      *  04.08.17    NACL-藤原    18/03/28  平成３０年４月改正対応
      *  04.08.18    NACL-門脇    18/06/04  診療区分別点数欄の集計修正
      *
      *  05.00.01    NACL-藤原    17/07/04  ＨＡＯＲＩ対応
      *  05.00.02    NACL-伊藤    17/12/20  診療情報システム対応
      *  05.00.03    NACL-藤原    18/06/11  ファイルのエラー処理修正
      *  05.00.04    NACL-藤原    19/03/25  元号改正対応
      *                                     Warning 対応
      *  05.00.05    NACL-門脇    19/06/04  生保に関わる保険組み合わせ作成
      *                                     対応（主保険なしの場合）
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE02-FILE     ASSIGN  RECE01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE02-KEY
                                   FILE    STATUS  IS  STS-RECE02.
      *
           SELECT  RECE02X-FILE    ASSIGN  RECE02XPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE02X-KEY
                                   FILE    STATUS  IS  STS-RECE02X.
      *
           SELECT  RECE031-FILE     ASSIGN  RECE031PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE031-KEY
                                   FILE    STATUS  IS  STS-RECE03.
      *
           SELECT  RECE061-FILE     ASSIGN  RECE061PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE061-KEY
                                   FILE    STATUS  IS  STS-RECE06.
      *                             
           SELECT  RECE09-FILE     ASSIGN  RECE09PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE09-KEY
                                   FILE    STATUS  IS  STS-RECE09.
      *
           SELECT  RECE11-FILE     ASSIGN  RECE11PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE11-KEY
                                   FILE    STATUS  IS  STS-RECE11.
      *
           SELECT  RECE12-FILE     ASSIGN  RECE12PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE12-KEY
                                   FILE    STATUS  IS  STS-RECE12.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       FD  RECE02-FILE.
       01  RECE02-REC.
           COPY    "CPRCF002.INC".
      *
       FD  RECE02X-FILE.
       01  RECE02X-REC. 
           COPY    "CPRCF002.INC"  REPLACING  //RECE02//
                                   BY         //RECE02X//.
      *
       FD  RECE031-FILE.
       01  RECE031-REC.
           COPY    "CPRCF0031.INC".
      *
       FD  RECE061-FILE.
       01  RECE061-REC. 
           COPY    "CPRCF0061.INC".
      *
       FD  RECE09-FILE.
       01  RECE09-REC. 
           COPY    "CPRCF009.INC".
      *
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
           COPY    "COMMON-SPA".
      *
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE01PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE02XPARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE031PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE061PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE09PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE11PARA//.
      *
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
      *****COPY    "HCM11V01.INC"  REPLACING  //HCM11V01//
      *****COPY    "HCM11V02.INC"  REPLACING  //HCM11V02//
      *****COPY    "HCM11V03.INC"  REPLACING  //HCM11V03//
      *****COPY    "HCM11V04.INC"  REPLACING  //HCM11V04//
      *****COPY    "HCM11V05.INC"  REPLACING  //HCM11V05//
      *****COPY    "HCM11V06.INC"  REPLACING  //HCM11V06//
           COPY    "HCM11V07.INC"  REPLACING  //HCM11V07//
                                   BY         //HC06//.
      *****COPY    "HCM12V01.INC"  REPLACING  //HCM12V01//
      *****COPY    "HCM12V02.INC"  REPLACING  //HCM12V02//
           COPY    "HCM12V03.INC"  REPLACING  //HCM12V03//
                                   BY         //HC061//.
      *****COPY    "HCM13V01.INC"  REPLACING  //HCM13V01//
      *****COPY    "HCM13V02.INC"  REPLACING  //HCM13V02//
           COPY    "HCM13V03.INC"  REPLACING  //HCM13V03//
                                   BY         //HC062//.
      *****COPY    "HCM14V01.INC"  REPLACING  //HCM14V01//
      *****COPY    "HCM14V02.INC"  REPLACING  //HCM14V02//
           COPY    "HCM14V03.INC"  REPLACING  //HCM14V03//
                                   BY         //HC063//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE02          PIC X(02).
           03  STS-RECE03          PIC X(02).
           03  STS-RECE06          PIC X(02).
           03  STS-RECE09          PIC X(02).
           03  STS-RECE11          PIC X(02).
           03  STS-RECE12          PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECE02X         PIC X(02).  
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(03).
           03  FLG-RECE02          PIC 9(01).
           03  FLG-RECE031         PIC 9(01).
           03  FLG-RECE061         PIC 9(01).
           03  FLG-RECE08          PIC 9(01).
           03  FLG-RECE09          PIC 9(01).
           03  FLG-RECE11          PIC 9(01).
           03  FLG-RECE12          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-KUGIRI          PIC 9(01).
           03  FLG-RECE07          PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-RECECOM         PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-OPECOM          PIC 9(01).
           03  FLG-BTPARA          PIC 9(01).
           03  FLG-KOHSKY          PIC 9(01).
           03  FLG-RECE02X         PIC 9(01).
           03  FLG-RECE-PUTH       PIC 9(01).
           03  FLG-RECE-PUTB       PIC 9(01).
      *
           03  FLG-TENKI           PIC 9(01).     
           03  FLG-SRYKBNPRT       PIC 9(01).
           03  FLG-SET             PIC 9(01).
           03  FLG-OPECOM-CHK      PIC 9(01).
           03  FLG-SYS-1900        PIC 9(01).
           03  FLG-SYS-1901        PIC 9(01).
           03  FLG-SYS-1902        PIC 9(01).
           03  FLG-SYS-2008        PIC 9(01).
           03  FLG-RECE-PUT-UM     PIC 9(01).
           03  FLG-HENSYU          PIC 9(01).
           03  FLG-LINE            PIC 9(01).
           03  FLG-RECECOM-SJKBN   PIC 9(01).
           03  FLG-STS             PIC 9(04).
           03  FLG-MEISYO-HENSYU   PIC 9(01).
           03  FLG-JIRITU-HENSYU   PIC 9(01).
           03  FLG-JNISSU          PIC 9(01).
           03  FLG-TOTALTEN        PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
           03  CNT-RECE02          PIC 9(06).
           03  CNT-RECE02X         PIC 9(06).
           03  CNT-RECE03          PIC 9(06).
           03  CNT-RECE061         PIC 9(06).
           03  CNT-RECE09          PIC 9(06).
           03  CNT-OUT             PIC 9(06).
           03  CNT-ZAIMEISAISU     PIC 9(03).
           03  CNT-PAGE            PIC 9(03).
           03  CNT-KENSU           PIC 9(03). 
           03  CNT-COMMIT          PIC 9(03). 
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDW1                PIC 9(04).
           03  IDW2                PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDY1                PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDY3                PIC 9(03).
           03  IDXX                PIC 9(03).
           03  IDWW                PIC 9(03).
           03  IDXY                PIC 9(03).
           03  IDXZ                PIC 9(03).
           03  IDZZ                PIC 9(03).
      *
           03  IDA                 PIC 9(04).
           03  IDB                 PIC 9(04).
           03  IDC                 PIC 9(04).
           03  IDD                 PIC 9(04).
           03  IDX1                PIC 9(02).
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
           03  IDX4                PIC 9(02).
           03  IDX5                PIC 9(02).
           03  IDX6                PIC 9(02).
           03  IDX7                PIC 9(02).
      *     
           03  SEIHIDX             PIC 9(02).
           03  KAKKIDX             PIC 9(02).
           03  SESNIDX             PIC 9(02).
      *     
           03  SYSKAN-IDX          PIC 9(03).
      *
           03  IDX-KUGIRI1         PIC 9(02).
           03  IDX-KUGIRI2         PIC 9(02).
           03  IDX-KUGIRI3         PIC 9(02).
           03  IDX-KUGIRI4         PIC 9(02).
      *    
       01  KEY-AREA                          VALUE   LOW-VALUE. 
           03  KEY-NEW.
               05  KEY-NEW1.
                   07  KEY-N-HOSPNUM   PIC  9(02).
                   07  KEY-N-SRYYM     PIC  9(06).
                   07  KEY-N-NYUGAIKBN PIC  X(01).
                   07  KEY-N-PTID      PIC  9(10).
                   07  KEY-N-RECEKA    PIC  X(02).
               05  KEY-N-TEISYUTUSAKI  PIC  9(01).
               05  KEY-N-RECESYUBETU   PIC  9(04).
               05  KEY-N-HKNJANUM-KEY  PIC  X(08).
           03  KEY-OLD.
               05  KEY-OLD1.
                   07  KEY-O-HOSPNUM   PIC  9(02).
                   07  KEY-O-SRYYM     PIC  9(06).
                   07  KEY-O-NYUGAIKBN PIC  X(01).
                   07  KEY-O-PTID      PIC  9(10).
                   07  KEY-O-RECEKA    PIC  X(02).
               05  KEY-O-TEISYUTUSAKI  PIC  9(01).
               05  KEY-O-RECESYUBETU   PIC  9(04).
               05  KEY-O-HKNJANUM-KEY  PIC  X(08).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC         OCCURS  100.
               05  TBL-SYS1001         PIC  X(610).
      *    医療機関（住所等情報）退避用領域
       01  TBL-SYS1002-T.
           03  TBL-SYS1002-OCC         OCCURS  100.
               05  TBL-SYS1002         PIC  X(610).
      *    帳票別医療機関編集情報指定退避用領域
       01  TBL-SYS1900-T.
           03  TBL-SYS1900-OCC         OCCURS  100.
               05  TBL-SYS1900         PIC  X(610).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-CREYMD  PIC  X(08).
               05  WRK-PARA-CREHMS  PIC  X(08).
               05  WRK-PARA-SRYYM   PIC  X(06).
               05  WRK-PARA-TERMID  PIC  X(16).
               05  WRK-PARA-SYSYMD  PIC  X(08).
               05  WRK-PARA-SYOKBN  PIC  X(01).
               05  WRK-PARA-RECEKBN PIC  X(01).
               05  WRK-PARA-RECEYM  PIC  X(06). 
               05  WRK-PARA-JOBID   PIC  9(07).
               05  WRK-PARA-SHELLID PIC  X(08).
           03  WRK-PARA-PRTKBN      PIC  X(01).
           03  WRK-PARA-DLTKBN      PIC  X(01).
           03  WRK-PARA-GYOUMUKBN   PIC  X(01).
           03  WRK-PARA-HOSPNUM     PIC  9(02).
      *
           03  WRK-SYS-SRYYMD       PIC  X(08).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-OPECOM-MCP-TABLE
                                   PIC X(64).
           03  WRK-OPECOM-MCP-PATHNAME
                                   PIC X(64).
      *
           03  WRK-RECENUM         PIC 9(06). 
      *
           03  WRK-HKNJANUM        PIC X(08).
           03  WRK-KYURATE-X.
               05  WRK-KYURATE-9   PIC 9(03).
           03  WRK-KYURATE         PIC Z9.
           03  WRK-GENMEN-FTNRATE.
               05  WRK-GENMEN-FTNRATE1  PIC 9(02).
               05  WRK-GENMEN-FTNRATE2  PIC 9(01).
           03  WRK-BEDSU           PIC ZZZZ.
           03  WRK-HBTNUM-X.
               05  WRK-HBTNUM-9    PIC 9(03).
           03  WRK-RECE02-HBTNUM-X. 
               05  WRK-RECE02-HBTNUM-9
                                   PIC 9(02).
      *
           03  WRK-SYUBETU-SRT1    PIC X(04).
           03  WRK-RECE-PUTB-SYUBETU-SRT1 PIC 9(9).
           03  WRK-RECE-PUTB-SYUBETU-SRT1-R
                               REDEFINES   WRK-RECE-PUTB-SYUBETU-SRT1.
               05  FILLER          PIC X(01).
               05  WRK-RECE-PUTB-SYUBETU-SRT1-X
                                   PIC X(08).
      *
           03  WRK-UPDCNT          PIC 9(08).
      *
           03  WRK-HSEQNUM         PIC 9(08).
      *
           03  WRK-PRVLAYOUT       PIC X(01).
      *
           03  WRK-HC06-HEAD4-G    PIC X(128).
      *
           03  WRK-SUM-TENSU       PIC 9(08).
      *
           03  WRK-TAIHI-FTNMONEY  PIC 9(09).
      *
           03  WRK-DRNAME          PIC X(20).
      *
           03  WRK-TOKKI           PIC X(02).
           03  WRK-TOKKI-X         PIC X(12).
           03  WRK-TOKKINM         PIC X(12).
           03  WRK-TOKKI-AREA.
               05  WRK-TOKKI-OCC   OCCURS  10. 
                   07  WRK-TOKKI-O PIC X(02).
                   07  WRK-TOKKINM-O
                                   PIC X(12).
      *
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY      PIC X(04).
               05  WRK-HEN-H1      PIC X(01).
               05  WRK-HEN-MM      PIC X(02).
               05  WRK-HEN-H2      PIC X(01).
               05  WRK-HEN-DD      PIC X(02).
      *
           03  WRK-CNT             PIC 9(10).
           03  WRK-CNT-X           REDEFINES   WRK-CNT
                                   PIC X(10).
      *
           03  WRK-1033-TABLE.
               05  WRK-1033-TBL    OCCURS  5.
                   07  WRK-1033-HKNJANUM-G.
                       09  WRK-1033-HKNJANUM
                                   PIC X(08) OCCURS  20.
                   07  WRK-1033-STYUKYMD
                                   PIC X(08).
                   07  WRK-1033-EDYUKYMD 
                                   PIC X(08). 
      *
           03  WRK-1009-KETA       PIC 9(02).                                      
      *
           03  WRK-BYO-OLD.
               05  WRK-BO-SYUBYOFLG            PIC X(01).
               05  WRK-BO-SRYYMD               PIC X(08).
               05  WRK-BO-TENKIKBN             PIC X(01).
               05  WRK-BO-TENKIYMD             PIC X(08).
      *
      *    1桁目:生保 2桁目:結核 3桁目:精神
           03  WRK-3SYU-KBN.
               05  WRK-3SYU        PIC X(03).
      *
           03  WRK-EDIT-AREA.
      *        傷病名連番     
               05  WRK-LINE                    PIC 9(02).
      *        編集傷病名文字数 
               05  WRK-CLM                     PIC 9(03).
      *        ワーク編集傷病名文字数 
               05  WRK-WCLM                    PIC 9(03).
      *        病名文字数         
               05  WRK-MOJISU                  PIC 9(03).
      *        編集開始位置         
               05  WRK-STCLM                   PIC 9(03).
      *        傷病名欄編集行         
               05  WRK-LINEBYO                 PIC 9(02).
      *        適用欄編集行         
               05  WRK-LINETEK                 PIC 9(03).
      *        前行の続きのとき（傷病名）         
               05  WRK-CONTINUE                PIC 9(01).
      *    適用欄最大行数
           03  WRK-TEKIYOU-LINE                PIC 9(03).
      *    適用欄行数
           03  WRK-TEKIYOU-LINE-GYO            PIC 9(03).
      *    傷病名編集
           03  WRK-BYOMEI                      PIC X(150).
           03  WRK-BYOMEIW                     PIC X(150).
      *    傷病名編集フラグ（疑い）
           03  WRK-HENFLG1                     PIC X(01).
      *    傷病名編集フラグ（急性）
           03  WRK-HENFLG2                     PIC X(01). 
      *    傷病名編集（傷病名欄）
           03  WRK-BYOMEI1                     PIC X(54).
           03  WRK-BYOMEI1-W   REDEFINES       WRK-BYOMEI1.
               05  WRK-BYOMEI1-NO              PIC X(06).
               05  WRK-BYOMEI1-NAME            PIC X(48).
               
      *    傷病名編集（適用欄）
           03  WRK-BYOMEI2                     PIC X(52).
           03  WRK-BYOMEI2-W   REDEFINES       WRK-BYOMEI2.
               05  WRK-BYOMEI2-NO              PIC X(08).
               05  WRK-BYOMEI2-NAME            PIC X(44).
               05  WRK-BYOMEI2-NAMEW   REDEFINES   WRK-BYOMEI2-NAME.
                   07  FILLER                  PIC X(18).
                   07  WRK-BYOMEI2-ELSE.
                       09  FILLER              PIC X(02).
                       09  WRK-BYOMEI2-YMD-G   PIC X(02).
                       09  WRK-BYOMEI2-YMD     PIC X(18).
                       09  WRK-BYOMEI2-TENKI   PIC X(04).
               05  WRK-BYOMEI2R-NAMEW  REDEFINES   WRK-BYOMEI2-NAME.
                   07  FILLER                  PIC X(06).
                   07  WRK-BYOMEI2R-ELSE.
                       09  FILLER              PIC X(02).
                       09  WRK-BYOMEI2R-YMD-G  PIC X(02).
                       09  WRK-BYOMEI2R-YMD    PIC X(18).
                       09  WRK-BYOMEI2R-TENKI  PIC X(04).
                       09  WRK-BYOMEI2R-TENKIYMD
                                               PIC X(12).
               05  WRK-BYOMEI2R1-NAMEW REDEFINES   WRK-BYOMEI2-NAME.
                   07  FILLER                  PIC X(12).
                   07  WRK-BYOMEI2R1-ELSE.
                       09  FILLER              PIC X(02).
                       09  WRK-BYOMEI2R1-YMD-G PIC X(02).
                       09  WRK-BYOMEI2R1-YMD   PIC X(18).
                       09  WRK-BYOMEI2R1-TENKI PIC X(04).
                       09  WRK-BYOMEI2R1-TENKIYMD
                                               PIC X(06).
      *
      *    傷病名が３行になるときの編集
           03  WRK-BYOMEI3                     PIC X(48).
           03  WRK-MOJISU3                     PIC 9(03).
      *                 
      *    入退院履歴編集（適用欄）
           03  WRK-NYUTAI-RIREKI-AREA.
               05  WRK-NYUTAI-RIREKI           PIC X(52).
               05  WRK-NYUTAI-RIREKI-W         REDEFINES
                                               WRK-NYUTAI-RIREKI.
                   07  WRK-NYUTAI-MSG          PIC X(12).
                   07  FILLER                  PIC X(40).
               05  WRK-NYUTAI-RIREKI-W1        REDEFINES
                                               WRK-NYUTAI-RIREKI.
                   07  FILLER                  PIC X(08).
                   07  WRK-NYUTAI-NYUINYMD     PIC X(20).
                   07  WRK-NYUTAI-FL1          PIC X(02).
                   07  WRK-NYUTAI-TAIINYMD     PIC X(20).
                   07  WRK-NYUTAI-FL2          PIC X(02).
      *
      *    入退院履歴編集（自院歴−入院中区分）
           03  WRK-NYUINCHUKBN                 PIC X(01).
      *                 
      *    保険履歴編集（適用欄）
           03  WRK-HKN-RIREKI-AREA.
               05  WRK-HKN-RIREKI-TBL          OCCURS  100.  
                   07  WRK-HKN-RIREKI          PIC X(52).
                   07  WRK-HKN-RIREKI-W   REDEFINES    WRK-HKN-RIREKI.
                       09  WRK-HKN-MEISYO      PIC X(04).
                       09  WRK-HKN-TEKSTYMD.
                           11  WRK-HKN-TEKSTYY PIC X(08).
                           11  WRK-HKN-TEKSTMD PIC X(12).
                       09  WRK-HKN-FL.
                           11  WRK-HKN-FL1     PIC X(04).
                           11  WRK-HKN-TEKEDYMD.
                               13  WRK-HKN-TEKEDYY
                                               PIC X(08).
                               13  WRK-HKN-TEKEDMD
                                               PIC X(12).
                           11  WRK-HKN-FL2     PIC X(04).
           03  WRK-RIREKI-MAX                  PIC 9(03).                
           03  WRK-HKN-RIREKI-X                PIC X(60).
           03  WRK-HKN-HKNJANUM                PIC X(16).
           03  WRK-HKN-NUM                     PIC X(50).
           03  WRK-HKN-KIGO                    PIC X(50).
           03  WRK-HKN-TEISYUTUSAKI            PIC X(04).    
           03  WRK-HKN-FIL                     PIC X(02).
           03  WRK-SRYYMST                     PIC X(12).
           03  WRK-SRYYMED                     PIC X(12).
      *
           03  WRK-KOH-HENSYU-AREA.
               05  WRK-KOH-HENSYU-MAX          PIC 9(02).
               05  WRK-KOH-HENSYU              PIC X(60).
               05  WRK-KOH-HENSYU-X            PIC X(52)   OCCURS  10.
               05  WRK-KOH-JKYSNUM             PIC X(16).
               05  WRK-KOH-FTNJANUM            PIC X(20).
               05  WRK-KOH-JNISSU              PIC X(04).
               05  WRK-KOH-FL                  PIC X(06).
               05  WRK-KOH-SAIKEI              PIC X(14).
      *
           03  WRK-WLINETEK                    PIC 9(03).
           03  WRK-TNSIDX                      PIC 9(02).
           03  WRK-R061-ZAINUM-O               PIC 9(08).
           03  WRK-R061-SRYKBN-O               PIC X(02).
           03  WRK-R061-KOHBUNRI-O             PIC 9(01).
      *
           03  WRK-SRYYMD                      PIC X(08).
           03  WRK-KGKZ                        PIC ZZZ,ZZZ,ZZZ.
           03  WRK-KGK9                        PIC ZZZ,ZZZ,ZZ9.
           03  WRK-KGK7                        PIC ZZZZ,ZZ9.
           03  WRK-KGK8                        PIC ZZZZZ,ZZ9.
           03  WRK-KGK6                        PIC ZZZ,ZZ9.
           03  WRK-KGK2                        PIC Z9.
      *
           03  WRK-FTNMONEY-HENSYU-AREA.
               05  WRK-FTNMONEY-HENSYU-G       OCCURS  4.
                   07  WRK-KOH-FTNMONEY        PIC  X(08).
                   07  WRK-KOH-TOTALTEN        PIC  X(07).
      *
           03  WRK-HENSYU-KOHNUM               PIC  X(03).
           03  WRK-HENSYU-KOHNAME.
               05  WRK-HENSYU-KOHNAME1         PIC  X(10).
               05  WRK-HENSYU-KOHNAME2-PLUS    PIC  X(02).
               05  WRK-HENSYU-KOHNAME2         PIC  X(10).
               05  WRK-HENSYU-KOHNAME3-PLUS    PIC  X(02).
               05  WRK-HENSYU-KOHNAME3         PIC  X(10).
      *
      *    レセプトコメント編集
           03  WRK-COMMENT-G.
               05  WRK-COMMENT     PIC X(50)
                                       OCCURS  48.
           03  WRK-COMM            PIC X(2400).
           03  WRK-COMM1           PIC X(2400).
           03  WRK-COL             PIC 9(03). 
           03  WRK-RECECOM         PIC 9(01).
           03  WRK-RECEHEN         PIC 9(01).
           03  WRK-RECECOM-NAME    PIC X(52).
           03  WRK-SJKBN-X.
               05  WRK-SJKBN       PIC 9(02).
           03  WRK-RECECOM-SJKBN   PIC X(02).
      *
      *    手術歴編集
           03  WRK-OPECOM          PIC 9(01).
           03  WRK-OPECOM-HEN      PIC X(50).
      *
      *    複数枚レセ用連番
           03  WRK-HEN-PAGE-AREA.
               05  WRK-PAGE1       PIC Z9.
               05  WRK-PAGE-HEN    PIC X(01).
               05  WRK-PAGE2       PIC Z9.
      *
           03  WRK-DAISAN-DATA.
               05  WRK-DAISAN-FTNMONEY     PIC 9(09).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
      ***  03  WRK-FUTAN           PIC ZZZ,ZZZ,ZZ9. 
           03  WRK-FUTAN           PIC ZZZZZZZZ9.
           03  WRK-FUTAN2          PIC ZZZZZZZ9.
           03  WRK-FUTAN3          PIC ZZ,ZZZ,ZZ9.
           03  WRK-FUTAN4          PIC ZZZ,ZZZ,ZZ9.
           03  WRK-TITLE           PIC X(50).
           03  WRK-TEKIYOU         PIC X(50).
           03  WRK-ADRS.
               05  WRK-ADRS-R      PIC X(50)  OCCURS  5.
      *
           03  WRK-TOTALTEN-OCC.
               05  WRK-TOTALTEN    PIC 9(8)    OCCURS  5.
      *
      *    県単用項目ファイル用ワーク     
           COPY    "HC06.INC"      REPLACING  //HC06//
                                   BY         //RECE11-HC06//.
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-ERRMSG                          PIC X(300).
           03  WRK-MCP-RC                          PIC 9(9).
           03  WRK-ERR-FILE-STS                    PIC X(02).
           03  WRK-ERR-FILE-MSG                    PIC X(100).
           03  WRK-ERR-FILE-MSG1                   PIC X(17).
           03  WRK-ERR-FLG                         PIC 9(01).
      *    エラーファイル名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *    FULLNAME内のBASENAME開始位置
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *    レセプト格納用
       01  TABLE-AREA.
           03  TBL-RECE-MAX        PIC 9(02).
           03  TBL-RECE-OCC            OCCURS  99.
               05  TBL-RECE-PRTDATA
                                   PIC X(10000).
               05  TBL-RECE-FORMID PIC X(20). 
               05  TBL-RECE-RENNUM PIC 9(02).
      *
      *    編綴詳細設定用テーブル
       01  TABLE-RECE-PUTB-AREA.
      *    種別用 
           03  TABLE-SYUBETU-AREA  OCCURS  2.
               05  TBL-SYUBETU-RECE-PUTB-TBL    OCCURS  50.
                   07  TBL-SYUBETU-RECE-PUTB-SYUBETU
                                   PIC X(04).
                   07  TBL-SYUBETU-RECE-PUTB-REC
                                   PIC X(1000).
      *    公費用
           03  TABLE-KOH-AREA      OCCURS  2.
               05  TBL-KOH-RECE-PUTB-TBL       OCCURS  2000.
                   07  TBL-KOH-RECE-PUTB-HKNNUM
                                   PIC X(03).
                   07  TBL-KOH-RECE-PUTB-REC
                                   PIC X(1000).
      *
       01  WRK-BYOMEI-AREA.     
      *    傷病名最大文字数（傷病名欄）
           03  WRK-BYOMEI1-MAX     PIC 9(02)   VALUE   48.
      *    傷病名最大文字数（適用欄）
           03  WRK-BYOMEI2-MAX     PIC 9(02)   VALUE   44.
      *    傷病名欄最大行数
           03  WRK-BYOMEI-LINE     PIC 9(02)   VALUE   5.
      *    適用欄最大行数
           03  WRK-TEKIYOU1-LINE   PIC 9(02)   VALUE   35.
      *    適用欄（続紙）最大行数（横タイプ）
           03  WRK-TEKIYOU2-LINE   PIC 9(03)   VALUE   140.
      *    適用欄（続紙）最大行数（縦タイプ）
           03  WRK-TEKIYOU3-LINE   PIC 9(03)   VALUE   110.
      *    適用欄（続紙）最大行数（縦タイプ・長）
           03  WRK-TEKIYOU4-LINE   PIC 9(03)   VALUE   138.
      *    適用欄（続紙）の行別行数（横タイプ）
           03  WRK-TEKIYOU2-LINE-GYO
                                   PIC 9(03)   VALUE   35.
      *    適用欄（続紙）行別行数（縦タイプ）
           03  WRK-TEKIYOU3-LINE-GYO
                                   PIC 9(03)   VALUE   55.
      *
      *   特記事項
       01  TBL-TOKKI-AREA.
           03  FILLER          PIC X(02)   VALUE   "01".
           03  FILLER          PIC X(10)   VALUE   "０１公".
           03  FILLER          PIC X(02)   VALUE   "02".
           03  FILLER          PIC X(10)   VALUE   "０２長".
           03  FILLER          PIC X(02)   VALUE   "03".
           03  FILLER          PIC X(10)   VALUE   "０３長処".
           03  FILLER          PIC X(02)   VALUE   "04".
           03  FILLER          PIC X(10)   VALUE   "０４後保".
           03  FILLER          PIC X(02)   VALUE   "07".
           03  FILLER          PIC X(10)   VALUE   "０７老併".
           03  FILLER          PIC X(02)   VALUE   "08".
           03  FILLER          PIC X(10)   VALUE   "０８老健".
           03  FILLER          PIC X(02)   VALUE   "09".
           03  FILLER          PIC X(10)   VALUE   "０９施".
           03  FILLER          PIC X(02)   VALUE   "10".
           03  FILLER          PIC X(10)   VALUE   "１０第三".
           03  FILLER          PIC X(02)   VALUE   "11".
           03  FILLER          PIC X(10)   VALUE   "１１薬治".
           03  FILLER          PIC X(02)   VALUE   "12".
           03  FILLER          PIC X(10)   VALUE   "１２器治".
           03  FILLER          PIC X(02)   VALUE   "13".
           03  FILLER          PIC X(10)   VALUE   "１３先進".
           03  FILLER          PIC X(02)   VALUE   "14".
           03  FILLER          PIC X(10)   VALUE   "１４制超".
           03  FILLER          PIC X(02)   VALUE   "15".
           03  FILLER          PIC X(10)   VALUE   "１５経過".
           03  FILLER          PIC X(02)   VALUE   "16".
           03  FILLER          PIC X(10)   VALUE   "１６長２".
           03  FILLER          PIC X(02)   VALUE   "17".
           03  FILLER          PIC X(10)   VALUE   "１７上位".
           03  FILLER          PIC X(02)   VALUE   "18".
           03  FILLER          PIC X(10)   VALUE   "１８一般".
           03  FILLER          PIC X(02)   VALUE   "19".
           03  FILLER          PIC X(10)   VALUE   "１９低所".
           03  FILLER          PIC X(02)   VALUE   "20".
           03  FILLER          PIC X(10)   VALUE   "２０二割".
           03  FILLER          PIC X(02)   VALUE   "21".
           03  FILLER          PIC X(10)   VALUE   "２１高半".
           03  FILLER          PIC X(02)   VALUE   "25".
           03  FILLER          PIC X(10)   VALUE   "２５出産".
           03  FILLER          PIC X(02)   VALUE   "26".
           03  FILLER          PIC X(10)   VALUE   "２６区ア".
           03  FILLER          PIC X(02)   VALUE   "27".
           03  FILLER          PIC X(10)   VALUE   "２７区イ".
           03  FILLER          PIC X(02)   VALUE   "28".
           03  FILLER          PIC X(10)   VALUE   "２８区ウ".
           03  FILLER          PIC X(02)   VALUE   "29".
           03  FILLER          PIC X(10)   VALUE   "２９区エ".
           03  FILLER          PIC X(02)   VALUE   "30".
           03  FILLER          PIC X(10)   VALUE   "３０区オ".
           03  FILLER          PIC X(02)   VALUE   "36".
           03  FILLER          PIC X(10)   VALUE   "３６加治".
           03  FILLER          PIC X(02)   VALUE   "96".
           03  FILLER          PIC X(10)   VALUE   "９６".
           03  FILLER          PIC X(02)   VALUE   "97".
           03  FILLER          PIC X(10)   VALUE   "９７".
           03  FILLER          PIC X(02)   VALUE   "37".
           03  FILLER          PIC X(10)   VALUE   "３７申出".
           03  FILLER          PIC X(02)   VALUE   "38".
           03  FILLER          PIC X(10)   VALUE   "３８医併".
           03  FILLER          PIC X(02)   VALUE   "39".
           03  FILLER          PIC X(10)   VALUE   "３９医療".
       01  TBL-TOKKI-R         REDEFINES   TBL-TOKKI-AREA.
           03  TBL-TOKKI       OCCURS   31.
               05  TBL-TOKKICD             PIC X(02).
               05  TBL-TOKKINAME           PIC X(10).
       01  TBL-TOKKI-MAX                   PIC 9(02)   VALUE   31.
      *
      *   症状詳記区分の表題
       01  TBL-RECECOM-AREA.
           03  FILLER              PIC X(52)   VALUE
               "【主たる疾患の診断根拠となる臨床症状】".
           03  FILLER              PIC X(52)   VALUE
               "【主たる疾患の診断根拠となる臨床症状の診察検査所見】".
           03  FILLER              PIC X(52)   VALUE
               "【主な治療行為（手術、処置、薬物治療等）の必要性】".
           03  FILLER              PIC X(52)   VALUE
               "【主な治療行為（手術、処置、薬物治療等）の経過】".
           03  FILLER              PIC X(52)   VALUE
               "【合計点数が１００万点以上の薬剤に係る症状等】".
           03  FILLER              PIC X(52)   VALUE
               "【合計点数が１００万点以上の処置に係る症状等】".
           03  FILLER              PIC X(52)   VALUE
               "【その他】".
           03  FILLER              PIC X(52)   VALUE
               "【薬事法に規定する治験に係る治験概要】".
           03  FILLER              PIC X(52)   VALUE
               "【疾患別リハビリテーションに係る治療継続の理由等】".
           03  FILLER              PIC X(52)   VALUE
               "【廃用症候群に係る評価表】".
           03  FILLER              PIC X(52)   VALUE
               "【上記以外の診療報酬明細書】".
           03  FILLER              PIC X(52)   VALUE
               "【医薬品医療機器等法に規定する治験に係る治験概要】".
       01  TBL-RECECOM-AREA-R      REDEFINES   TBL-RECECOM-AREA.
           03  TBL-RECECOM-NAME    PIC X(52)   OCCURS   12.
      *
      *   退職者保険番号
       01  WRK-RETIRE-AREA.
      *        組合
           03  WRK-RETIRE1         PIC X(03)   VALUE  "063".
      *        国公
           03  WRK-RETIRE2         PIC X(03)   VALUE  "072".
      *        地公
           03  WRK-RETIRE3         PIC X(03)   VALUE  "073".
      *        警察
           03  WRK-RETIRE4         PIC X(03)   VALUE  "074".
      *        公学・私学
           03  WRK-RETIRE5         PIC X(03)   VALUE  "075".
      *        国保
           03  WRK-RETIRE6         PIC X(03)   VALUE  "067".
      *
      *    疑い編集
       01  WRK-UTAGAI-AREA.
      *    疑い
           03  WRK-CONS-UTAGAI     PIC X(06)   VALUE   "の疑い".
           03  WRK-CONS-UTAGAI-MOJISU
                                   PIC 9(02)   VALUE   6.       
      *    急性
           03  WRK-CONS-KYUSEI     PIC X(04)   VALUE   "急性".      
           03  WRK-CONS-KYUSEI-MOJISU
                                   PIC 9(02)   VALUE   4.       
      *
      *    区切り線
       01  WRK-LINE-AREA.
           03  WRK-KUGIRI1         PIC X(52)   VALUE
               ALL "―".
           03  WRK-KUGIRI2.
               05  FILLER          PIC X(22)   VALUE
                                   "＝＝＝＝＝＝＝　以下　".
               05  WRK-KUGIRI2-KOHNAME         PIC X(10).
               05  FILLER          PIC X(20)   VALUE
                                   "　適用分　＝＝＝＝＝".
           03  WRK-KUGIRI3         PIC X(52)   VALUE
               ALL "―".
           03  WRK-KUGIRI4.
               05  FILLER          PIC X(16)   VALUE
                                   "＝＝＝＝　以下　".
               05  WRK-KUGIRI4-KOHNAME         PIC X(22).
               05  FILLER          PIC X(14)   VALUE
                                   "　適用分　＝＝".
           03  WRK-KUGIRI5.
               05  FILLER          PIC X(10)   VALUE
                                   "＝＝以下　".
               05  WRK-KUGIRI5-KOHNAME         PIC X(34).
               05  FILLER          PIC X(08)   VALUE
                                   "　適用分".
           03  WRK-KUGIRI6.
               05  FILLER          PIC X(52).
      *
      *
       01  WRK-TENSU-AREA.
           03  WRK-KHNTEN.
               05  WRK-KHNTENZ     PIC  ZZZZZZZ.
           03  WRK-KAISU.
               05  WRK-KAISUZ      PIC  ZZZ.
           03  WRK-KOUTEN.
               05  WRK-KOUTENZ     PIC  ZZZZZZZ.
           03  WRK-TENSU.
               05  WRK-TENSUZ      PIC  ZZZZZZZ.
           03  WRK-JINISSU.
               05  WRK-JINISSUZ    PIC  ZZ.
           03  WRK-JINISSU2.
               05  WRK-JINISSU2Z   PIC  Z9.
           03  WRK-SAIKEI.
               05  WRK-SAIKEIZ     PIC  ZZZZZZ9.
      *
      *    ＯＣＲ編集領域
       01  OCR1-AREA.
           03  OCR1-HKNJANUM                           PIC 9(08).
           03  OCR1-HOSPCD                             PIC 9(07).
           03  OCR1-SKYTENSU                           PIC 9(07).
           03  OCR1-CHKDGT1                            PIC 9(01).
           03  OCR1-YKZFTN                             PIC 9(05).
           03  OCR1-CHKDGT2                            PIC 9(01).
           03  OCR1-FTNMONEY                           PIC 9(05).
           03  OCR1-CHKDGT3                            PIC 9(01).
           03  OCR1-CHKDGT4                            PIC 9(01).
           03  OCR1-JNISSU                             PIC 9(02).
           03  OCR1-SRYYM                              PIC 9(04).
           03  OCR1-CHKDGT5                            PIC 9(01).
           03  OCR1-FTNJANUM                           PIC 9(08).
           03  OCR1-JKYSNUM                            PIC 9(07).
           03  OCR1-CHKDGT6                            PIC 9(01).
           03  OCR1-PREFNUM                            PIC 9(02).
           03  OCR1-TENSUHYO                           PIC 9(01).
           03  OCR1-HKNSYUBETU1                        PIC 9(01).
           03  OCR1-HKNSYUBETU2                        PIC 9(01).
           03  OCR1-HONKZKKBN                          PIC 9(01).
           03  OCR1-SEIRINO                            PIC 9(01).
           03  OCR1-CHKDGT7                            PIC 9(01).
      *
       01  OCR2-AREA.
           03  OCR2-SEX                                PIC 9(01).
           03  OCR2-GENGO                              PIC 9(01).
           03  OCR2-BIRTHYY                            PIC 9(02).
           03  OCR2-CHKDGT1                            PIC 9(01).
           03  OCR2-KIGO                               PIC 9(10).
           03  OCR2-NUM                                PIC 9(10).
           03  OCR2-CHKDGT2                            PIC 9(01).
           03  OCR2-AREA-X.
               05  OCR2-FTNJANUM                       PIC 9(08).
               05  OCR2-JKYSNUM                        PIC 9(07).
               05  OCR2-CHKDGT3                        PIC 9(01).
               05  OCR2-JNISSU                         PIC 9(02).
               05  OCR2-TENSU                          PIC 9(07).
               05  OCR2-CHKDGT4                        PIC 9(01).
               05  OCR2-YKZFTN                         PIC 9(05).
               05  OCR2-CHKDGT5                        PIC 9(01).
               05  OCR2-FTNMONEY                       PIC 9(05).
               05  OCR2-CHKDGT6                        PIC 9(01).
               05  OCR2-CHKDGT7                        PIC 9(01).
           03  OCR2-YOBI                               PIC X(02).
      *
       01  OCR3-AREA.
           03  OCR3-AREA-X.
               05  OCR3-FTNJANUM                       PIC 9(08).
               05  OCR3-JKYSNUM                        PIC 9(07).
               05  OCR3-CHKDGT1                        PIC 9(01).
               05  OCR3-JNISSU                         PIC 9(02).
               05  OCR3-TENSU                          PIC 9(07).
               05  OCR3-CHKDGT2                        PIC 9(01).
               05  OCR3-YKZFTN                         PIC 9(05).
               05  OCR3-CHKDGT3                        PIC 9(01).
               05  OCR3-FTNMONEY                       PIC 9(05).
               05  OCR3-CHKDGT4                        PIC 9(01).
               05  OCR3-CHKDGT5                        PIC 9(01).
           03  OCR3-YOBI                               PIC X(28).
      *
      *    全角変換領域
       01  TBL-SUUJI-AREA.
           03  FILLER              PIC X(02)   VALUE   "１".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(02)   VALUE   "２".
           03  FILLER              PIC X(01)   VALUE   "2".
           03  FILLER              PIC X(02)   VALUE   "３".
           03  FILLER              PIC X(01)   VALUE   "3".
           03  FILLER              PIC X(02)   VALUE   "４".
           03  FILLER              PIC X(01)   VALUE   "4".
           03  FILLER              PIC X(02)   VALUE   "５".
           03  FILLER              PIC X(01)   VALUE   "5".
           03  FILLER              PIC X(02)   VALUE   "６".
           03  FILLER              PIC X(01)   VALUE   "6".
           03  FILLER              PIC X(02)   VALUE   "７".
           03  FILLER              PIC X(01)   VALUE   "7".
           03  FILLER              PIC X(02)   VALUE   "８".
           03  FILLER              PIC X(01)   VALUE   "8".
           03  FILLER              PIC X(02)   VALUE   "９".
           03  FILLER              PIC X(01)   VALUE   "9".
           03  FILLER              PIC X(02)   VALUE   "０".
           03  FILLER              PIC X(01)   VALUE   "0".
       01  TBL-SUUJI-AREA-R        REDEFINES   TBL-SUUJI-AREA.
           03  TBL-SUUJI-G         OCCURS  10.   
               05  TBL-SUUJI-X     PIC X(02).
               05  TBL-SUUJI-9     PIC X(01).
      *
       01  WRK-ERR-MSG-AREA.
           03  WRK-ERR-MSG-1.
               05  WRK-ERR-HKNCOMBINUM-1
                                   PIC X(08).
               05  FILLER          PIC X(32)   VALUE
                   "の保険組合せが存在しません". 
      *
           03  WRK-ERR-MSG-2.
               05  WRK-ERR-DAY-2   PIC X(04).
               05  FILLER          PIC X(04)   VALUE
                   "日の". 
               05  WRK-ERR-MSG-21.
                   07  FILLER          PIC X(10)   VALUE
                   "保険組合せ". 
                   07  WRK-ERR-HKNCOMBINUM-2
                                       PIC X(08).
                   07  FILLER          PIC X(14)   VALUE
                   "は適用開始日が". 
                   07  WRK-ERR-TEKSTYMD-2
                                       PIC X(20).
                   07  FILLER          PIC X(14)   VALUE
                   "となっています". 
      *
           03  WRK-ERR-MSG-3.
               05  WRK-ERR-DAY-3       PIC X(04).
               05  FILLER              PIC X(04)   VALUE
                   "日の". 
               05  WRK-ERR-MSG-31.
                   07  FILLER          PIC X(10)   VALUE
                   "保険組合せ". 
                   07  WRK-ERR-HKNCOMBINUM-3
                                       PIC X(08).
                   07  FILLER          PIC X(14)   VALUE
                   "は適用終了日が". 
                   07  WRK-ERR-TEKEDYMD-3
                                       PIC X(20).
                   07  FILLER          PIC X(14)   VALUE
                   "となっています". 
      *
           03  WRK-ERR-MSG-4.
               05  FILLER          PIC X(48)  VALUE
               "地方公費のみ選択されています。主保険がありません".
      *
           03  WRK-ERR-MSG-5.
               05  FILLER          PIC X(14)  VALUE
                   "【主科未設定】".
      *
           03  WRK-ERR-MSG-6.
               05  FILLER              PIC X(30)  VALUE
               "【未請求設定の患者です】".
      *
           03  WRK-ERR-MSG-7.
               05  FILLER          PIC X(28)   VALUE
                   "老人保険が使用されています". 
      *
           03  WRK-ERR-MSG-8.
               05  FILLER          PIC X(38)   VALUE
                   "６５歳以上で退職国保が使用されています". 
      *
           03  WRK-ERR-MSG-9.
               05  FILLER          PIC X(48)   VALUE
                   "【リアルタイムプレビューより作成のため提出不可】". 
      *
           03  WRK-ERR-MSG-10.
               05  FILLER          PIC X(28)   VALUE
                   "政府管掌が使用されています". 
      *
           03  WRK-ERR-MSG-11.
               05  FILLER          PIC X(24)   VALUE
                   "対象の傷病名がありません". 
      *
           03  WRK-ERR-MSG-12.
               05  FILLER          PIC X(26)   VALUE
                   "コメントの入力がありません". 
      *
           03  WRK-ERR-MSG-13.
               05  FILLER          PIC X(28)   VALUE
                   "単位コードの入力がありません". 
      *
           03  WRK-ERR-MSG-14.
               05  FILLER          PIC X(46)   VALUE
                   "生保の負担者番号、受給者番号の入力がありません". 
      *
           03  WRK-ERR-MSG-15.
               05  FILLER          PIC X(20)  VALUE
                   "【０点公費記載あり】".
      *
           03  WRK-ERR-MSG-16.
               05  FILLER          PIC X(42)   VALUE
                   "残留法人等の負担者番号、受給者番号の入力が". 
               05  FILLER          PIC X(10)   VALUE
                   "ありません". 
           03  WRK-ERR-MSG-17.
               05  FILLER          PIC X(52)   VALUE
               "０５９で始まる「その他材料コード」が使用されています". 
      *
           03  WRK-ERR-MSG-18.
               05  FILLER          PIC X(52)   VALUE
               "請求点数と点数欄の合計が不一致です". 
      *
           03  WRK-ERR-MSG-88.
               05  FILLER          PIC X(24)  VALUE
                   "公費が４つを超えています".
      *
       01  WRK-CONS-AREA.    
      *    固定端末ＩＤ
           03  WRK-CONS-TERMID         PIC X(32)   VALUE
                                   "0100007FZZZZZZZZZZZZZZZZZZZZZZZZ". 
      *    改行コード
           03  WRK-CONS-RETURN         PIC X(01)   VALUE   X"0A".
      *
           03  WRK-CONS-SRYYM-200803   PIC 9(06)   VALUE   200803. 
           03  WRK-CONS-SRYYMD-20080331
                                       PIC 9(08)   VALUE   20080331. 
      *
           03  WRK-CONS-BODYLIST-MAX
                                   PIC 9(02)   VALUE   50.
           03  WRK-CONS-KOHLIST-MAX
                                   PIC 9(04)   VALUE   2000.
           03  WRK-CONS-RECECOM-MAX
                                   PIC 9(02)   VALUE   48.
           03  WRK-CONS-RECECOM-LENGTH-MAX
                                   PIC 9(04)   VALUE   2400.
      *
       01  WRK-RECE07-REC. 
           COPY    "CPRCF007.INC"  REPLACING  //RECE07//
                                   BY         //WRK-RECE07//.
      *     
           COPY    "ORCA-DBMETA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK2005.INC".
           COPY    "CPSK200501.INC"
                                   REPLACING  //SYS-200501//
                                   BY         //SYS-25//.
           COPY    "CPSK200505.INC".
      *
           COPY    "CPSK2008.INC".
      *
           COPY    "CPSK2009.INC".
      *     
      *    県内扱い保険者情報
           COPY    "CPSK1033.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
           COPY    "CPSK1002.INC".
      *
      *    レセプト作成指示（個別指示）コード
           COPY    "CPSK2002.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
           COPY    "CPSK1901.INC".
           COPY    "CPSK1902.INC".
      *
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSK1038.INC".
      *
           COPY    "CPSK1043.INC".
      *
           COPY    "CPRCF008.INC".
      *
           COPY    "CPRCF010.INC".
      *
      *    レセプト電算
           COPY    "CPRECEDEN.INC".
      *
      *    保険マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    レセプトコメントマスタ
       01  RECECOM-REC.
           COPY    "CPRECECOM.INC".
      *
      *    レセプト管理     
       01  RECE07-REC. 
           COPY    "CPRCF007.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    手術歴マスタ
       01  OPECOM-REC.
           COPY    "CPOPECOM.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *    主科
       01  SYUKA-REC.
           COPY    "CPSYUKA.INC".
      *
      *    編綴管理レコード（管理部）
       01  RECE-PUTH-REC.
           COPY    "CPRECE-PUTH.INC".
      *
      *    編綴管理レコード（明細部）
       01  RECE-PUTB-REC.
           COPY    "CPRECE-PUTB.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *    数値変換サブ
           COPY    "CPORCSNUM.INC".
      *    チェックディジット（モジュラス１１用）算出サブ
           COPY    "CPORCCHKDGT11.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    PUSH
           COPY    "CPPUSHBATCH01.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(400).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-STS     =   2
      *
           PERFORM 300-END-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  CNT-AREA
                                       TABLE-AREA
           INITIALIZE                  SYS-1001-REC
           INITIALIZE                  SYS-1002-REC
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-DLTKBN
                                               WRK-PARA-GYOUMUKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
      *
           DISPLAY "WRK-PARA-HOSPNUM   =" WRK-PARA-HOSPNUM
           DISPLAY "WRK-PARA-SYOKBN    =" WRK-PARA-SYOKBN
           DISPLAY "WRK-PARA-SRYYM     =" WRK-PARA-SRYYM
           DISPLAY "WRK-PARA-PRTKBN    =" WRK-PARA-PRTKBN
           DISPLAY "WRK-PARA-DLTKBN    =" WRK-PARA-DLTKBN 
           DISPLAY "WRK-PARA-GYOUMUKBN =" WRK-PARA-GYOUMUKBN
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0105"      TO  JOB-PGID
               MOVE    "印刷データ編集"
                                       TO  JOB-SHELLMSG
               PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        処理中止設定処理
               IF      FLG-STS             =   ZERO
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
      *         
      *        ジョブ管理更新（中止フラグ非活性）処理
               IF      FLG-STS             =   ZERO
                   MOVE    "STP"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    3               TO  JOB-STOPFLG
                   PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *
      *    ファイルＯＰＥＮ処理
           PERFORM 110-FILE-OPEN-SEC
      *
      *    システム管理マスタ（医療機関情報）
           IF      FLG-STS             =   ZERO
               PERFORM 120-HOSPINF-GET-SEC
           END-IF
      *
           IF    ( WRK-PARA-SHELLID
                                   NOT =   "RECEPTX" )
           AND   ( FLG-STS             =   ZERO      )
               PERFORM 5001-DB-DELETE-SEC
           END-IF
      *
      *    ＥＦファイル作成・連携基盤用・診療情報システム用のとき処理終了
           IF      WRK-PARA-PRTKBN      =   "9" OR  "8" OR "B"
               IF      WRK-PARA-PRTKBN      =   "8"
                   PERFORM 1001-EFOUT-CHECK-SEC
               END-IF
               MOVE    2                    TO  FLG-STS
           ELSE
               IF      FLG-STS             =   ZERO
                  PERFORM 990-RECE02-READ-SEC
               END-IF
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルＯＰＥＮ処理
      *****************************************************************
       110-FILE-OPEN-SEC          SECTION.
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    "RECE020"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (2)
           MOVE    "RECE02X"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               MOVE    WRK-CONS-TERMID     TO  RECEDAT1PARA-TERMID
           END-IF
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (3)
           MOVE    "RECE031"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (4)
           MOVE    "RECE061"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (5)
           MOVE    "RECE090"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (6)
           MOVE    "RECE110"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (7)
           MOVE    "RECE120"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (8)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE01PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE02XPARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE031PARA
           MOVE    SGETTEMP-FULLNAMES (5)
                                       TO  RECE061PARA
           MOVE    SGETTEMP-FULLNAMES (6)
                                       TO  RECE09PARA
           MOVE    SGETTEMP-FULLNAMES (7)
                                       TO  RECE11PARA
           MOVE    SGETTEMP-FULLNAMES (8)
                                       TO  RECE12PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           OPEN    I-O                 RECE02-FILE
           IF      STS-RECE02          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE01PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE02          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               OPEN    INPUT               RECE02X-FILE
               IF      STS-RECE02X         =   "00"
                   CONTINUE
               ELSE
                   MOVE    SPACE               TO  WRK-ERR-AREA
                   INITIALIZE                      WRK-ERR-AREA
                   MOVE    1                   TO  WRK-ERR-FLG
                   MOVE    RECE02XPARA         TO  WRK-ERR-FILE-NM
                   MOVE    STS-RECE02X         TO  WRK-ERR-FILE-STS
                   PERFORM 500-FILE-ERR-ABORT-SEC
               END-IF
           END-IF    
      *
           OPEN    INPUT               RECE031-FILE
           IF      STS-RECE03          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE031PARA         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE03          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE061-FILE
           IF      STS-RECE06          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE061PARA         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE06          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE09-FILE
           IF      STS-RECE09          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE09PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE09          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE11-FILE
           IF      STS-RECE11          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE11PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE11          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE12-FILE
           IF      STS-RECE12          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE12PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE12          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           .
       110-FILE-OPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報検索
      *****************************************************************
       120-HOSPINF-GET-SEC            SECTION.
      *
           IF      WRK-PARA-SRYYM    NOT =  ZERO
      *        医療機関情報
               INITIALIZE                   SYSKANRI-REC
               MOVE    "1001"           TO  SYS-KANRICD
               MOVE    "*"              TO  SYS-KBNCD
               MOVE    WRK-PARA-SRYYM   TO  SYS-STYUKYMD (1:6)
               MOVE    "01"             TO  SYS-STYUKYMD (7:2)
               MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
               MOVE    "tbl_syskanri"   TO  WRK-MCP-TABLE
               MOVE    "key10"          TO  WRK-MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI     =   ZERO
                   MOVE    MCPDATA-REC   TO  SYS-1001-REC
               ELSE
                   MOVE    "医療機関情報が取得できませんでした。"
                                         TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    2               TO  FLG-STS
                   GO  TO  120-HOSPINF-GET-EXT
               END-IF
           ELSE
               MOVE     ZERO               TO  SYSKAN-IDX
               MOVE     SPACE              TO  TBL-SYS1001-T
               INITIALIZE                      TBL-SYS1001-T
      *
               INITIALIZE                      SYSKANRI-REC
               MOVE    "1001"              TO  SYS-KANRICD
               MOVE    "*"                 TO  SYS-KBNCD
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-N-SEC
                   PERFORM UNTIL ( FLG-SYSKANRI    =   1   )
                           OR    ( SYSKAN-IDX      >=  100 )
                       MOVE    MCPDATA-REC         TO  SYS-1001-REC
                       IF      SYS-1001-EDYUKYMD   <=
                                               WRK-CONS-SRYYMD-20080331
                           CONTINUE
                       ELSE
                           ADD     1             TO  SYSKAN-IDX
                            MOVE    SYS-1001-REC  TO  TBL-SYS1001
                                                          (SYSKAN-IDX)
                       END-IF
      *
                       MOVE    "tbl_syskanri"    TO  MCP-TABLE
                       MOVE    "key8"            TO  MCP-PATHNAME
                       PERFORM 900-SYSKANRI-N-SEC
                   END-PERFORM
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               IF      SYSKAN-IDX          =   ZERO
                   MOVE    "医療機関情報が取得できませんでした。"
                                               TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    2            TO  FLG-STS
                   GO  TO  120-HOSPINF-GET-EXT
               END-IF
           END-IF
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *        医療機関情報（所在地ほか）
               INITIALIZE                   SYSKANRI-REC
               MOVE    "1002"           TO  SYS-KANRICD
               MOVE    "*"              TO  SYS-KBNCD
               MOVE    WRK-PARA-SRYYM   TO  SYS-STYUKYMD (1:6)
               MOVE    "01"             TO  SYS-STYUKYMD (7:2)
               MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
               MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
               MOVE    "key10"             TO  WRK-MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI     =   ZERO
                   MOVE    MCPDATA-REC      TO  SYS-1002-REC
               ELSE    
                   MOVE "医療機関情報（所在地）が取得できませんでした。"
                                            TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    2               TO  FLG-STS
                   GO  TO  120-HOSPINF-GET-EXT
               END-IF
           ELSE
               MOVE     ZERO               TO  SYSKAN-IDX
               MOVE     SPACE              TO  TBL-SYS1002-T
               INITIALIZE                      TBL-SYS1002-T
      *
               INITIALIZE                      SYSKANRI-REC
               MOVE    "1002"              TO  SYS-KANRICD
               MOVE    "*"                 TO  SYS-KBNCD
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM  900-SYSKANRI-N-SEC
                   PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                             OR    (SYSKAN-IDX   >=  100)
      *
                       MOVE    MCPDATA-REC   TO  SYS-1002-REC
                       IF      SYS-1002-EDYUKYMD   <=
                                               WRK-CONS-SRYYMD-20080331
                           CONTINUE
                       ELSE
                           ADD     1             TO  SYSKAN-IDX
                           MOVE    SYS-1002-REC  TO  TBL-SYS1002
                                                         (SYSKAN-IDX)
                       END-IF
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key8"              TO  MCP-PATHNAME
                       PERFORM 900-SYSKANRI-N-SEC
                   END-PERFORM
              END-IF
      *
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
      *
              IF      SYSKAN-IDX          =   ZERO
                   MOVE
                       "医療機関情報（所在地）が取得できませんでした。"
                                               TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    2            TO  FLG-STS
                   GO  TO  120-HOSPINF-GET-EXT
               END-IF
           END-IF
      *
      *    医療機関編集情報−帳票別指定情報
           MOVE    ZERO                TO  FLG-SYS-1900
                                           FLG-SYS-1901
                                           FLG-SYS-1902
      *
           IF   WRK-PARA-SRYYM     NOT =  ZERO
              INITIALIZE                   SYSKANRI-REC
              MOVE    "1900"           TO  SYS-KANRICD
              MOVE    "*"              TO  SYS-KBNCD
              MOVE    WRK-PARA-SRYYM   TO  SYS-STYUKYMD (1:6)
              MOVE    "01"             TO  SYS-STYUKYMD (7:2)
              MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
              MOVE    "tbl_syskanri"   TO  WRK-MCP-TABLE
              MOVE    "key10"          TO  WRK-MCP-PATHNAME
              PERFORM 900-SYSKANRI-READ-SEC
              IF      FLG-SYSKANRI     =   ZERO
                  MOVE    MCPDATA-REC      TO  SYS-1900-REC
              ELSE    
                  INITIALIZE                   SYS-1900-REC
              END-IF
              IF      SYS-1900-PRTKBN (1)  =   SPACE
                  CONTINUE
              ELSE
                  INITIALIZE                       SYSKANRI-REC
                  MOVE    "1901"               TO  SYS-KANRICD
                  MOVE    SYS-1900-PRTKBN (1)  TO  SYS-KBNCD
                  MOVE    WRK-PARA-SRYYM       TO  SYS-STYUKYMD (1:6)
                  MOVE    "01"                 TO  SYS-STYUKYMD (7:2)
                  MOVE    SYS-STYUKYMD         TO  SYS-EDYUKYMD
                  MOVE    "tbl_syskanri"       TO  WRK-MCP-TABLE
                  MOVE    "key10"              TO  WRK-MCP-PATHNAME
                  PERFORM 900-SYSKANRI-READ-SEC
                  IF      FLG-SYSKANRI     =   ZERO
                      MOVE    MCPDATA-REC      TO  SYS-1901-REC
                      MOVE    1                TO  FLG-SYS-1901
                  END-IF
                  INITIALIZE                       SYSKANRI-REC
                  MOVE    "1902"               TO  SYS-KANRICD
                  MOVE    SYS-1900-PRTKBN (1)  TO  SYS-KBNCD
                  MOVE    WRK-PARA-SRYYM       TO  SYS-STYUKYMD (1:6)
                  MOVE    "01"                 TO  SYS-STYUKYMD (7:2)
                  MOVE    SYS-STYUKYMD         TO  SYS-EDYUKYMD
                  MOVE    "tbl_syskanri"       TO  WRK-MCP-TABLE
                  MOVE    "key10"              TO  WRK-MCP-PATHNAME
                  PERFORM 900-SYSKANRI-READ-SEC
                  IF      FLG-SYSKANRI     =   ZERO
                      MOVE    MCPDATA-REC      TO  SYS-1902-REC
                      MOVE    1                TO  FLG-SYS-1902
                  END-IF
              END-IF    
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1900-T
              INITIALIZE                      TBL-SYS1900-T
      *
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1900"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
              MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"      TO  MCP-TABLE
                  MOVE    "key8"              TO  MCP-PATHNAME
                  PERFORM  900-SYSKANRI-N-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-1900-REC
                    IF      SYS-1900-EDYUKYMD   <=
                                               WRK-CONS-SRYYMD-20080331
                           CONTINUE
                    ELSE
                        IF      SYS-1900-PRTKBN (1)  =   SPACE
                            CONTINUE
                        ELSE
                            ADD     1             TO  SYSKAN-IDX
                            MOVE    SYS-1900-REC  TO
                                              TBL-SYS1900 (SYSKAN-IDX)
                            MOVE    1            TO  FLG-SYS-1900
                        END-IF
                    END-IF 
      *
                    MOVE    "tbl_syskanri"      TO  MCP-TABLE
                    MOVE    "key8"              TO  MCP-PATHNAME
                    PERFORM 900-SYSKANRI-N-SEC
      *
                  END-PERFORM
              END-IF
      *
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF 
      *         
      *    レセプト編集区分情報
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
           MOVE    "2005"           TO  SYS-KANRICD
           MOVE    "*"              TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM   TO  SYS-STYUKYMD (1:6)
           MOVE    "01"             TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-2005-REC
           ELSE    
               INITIALIZE                   SYS-2005-REC
           END-IF
           IF      SYS-2005-BYOMEIHENKBN    =   SPACE
               MOVE    ZERO             TO  SYS-2005-BYOMEIHENKBN
           END-IF
           IF      SYS-2005-TENKIHENKBN     =   SPACE
               MOVE    ZERO             TO  SYS-2005-TENKIHENKBN
           END-IF
           IF      SYS-2005-SYUBYOHENFLG    =   SPACE
               MOVE    ZERO             TO  SYS-2005-SYUBYOHENFLG
           END-IF
           IF      SYS-2005-NEXTPRTKBN-G      =   SPACE
               MOVE    ZERO             TO  SYS-2005-NEXTPRTKBN-G
           END-IF
           IF      SYS-2005-SYAOCRKBN   =   SPACE
               MOVE    ZERO             TO  SYS-2005-SYAOCRKBN
           END-IF
           IF      SYS-2005-KKHOCRKBN   =   SPACE
               MOVE    ZERO             TO  SYS-2005-KKHOCRKBN
           END-IF
           IF      SYS-2005-MOJI-KISAIKBN
                                        =   SPACE
               MOVE    ZERO             TO  SYS-2005-MOJI-KISAIKBN
           END-IF
           IF      SYS-2005-KYURATE-KISAIKBN
                                        =   SPACE
               MOVE    ZERO             TO  SYS-2005-KYURATE-KISAIKBN
           END-IF
           IF      SYS-2005-RECESYUBETU-KISAIKBN
                                        =   SPACE
               MOVE    ZERO             TO
                                        SYS-2005-RECESYUBETU-KISAIKBN
           END-IF
           IF      SYS-2005-NYUTAIINREKI-KSIKBN
                                   =   SPACE
               MOVE    "0"             TO  SYS-2005-NYUTAIINREKI-KSIKBN
           END-IF
           DISPLAY "SYS-2005-TENKIHENKBN            ="
                                    SYS-2005-TENKIHENKBN
      *         
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
           MOVE    "2005"           TO  SYS-KANRICD
           MOVE    "01"             TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM   TO  SYS-STYUKYMD (1:6)
           MOVE    "01"             TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-25-REC
           ELSE    
               INITIALIZE                   SYS-25-REC
           END-IF
           IF      SYS-25-KOUIKIOCRKBN
                                    =   SPACE
               MOVE    ZERO             TO  SYS-25-KOUIKIOCRKBN
           END-IF
           IF      SYS-25-INSPECT-CHARACTER-KBN
                                    =   SPACE
               MOVE    ZERO             TO
                                    SYS-25-INSPECT-CHARACTER-KBN
           END-IF
           IF      SYS-25-DRNAME-KSIKBN
                                        =   SPACE
               MOVE    "0"              TO  SYS-25-DRNAME-KSIKBN
           END-IF
           IF      SYS-25-TERMINAL-DIGIT-PRTKBN
                                   =   SPACE
               MOVE    "0"             TO
                                       SYS-25-TERMINAL-DIGIT-PRTKBN
           END-IF
           IF      SYS-25-TENKIBYOMEI-KBN
                                    =   SPACE
               MOVE    ZERO             TO  SYS-25-TENKIBYOMEI-KBN
           END-IF
           IF      SYS-25-HKN-RIREKI-KBN
                                        =   SPACE
               MOVE    "0"              TO  SYS-25-HKN-RIREKI-KBN
           END-IF
      *
           DISPLAY "SYS-200501-INSPECT-CHARACTER-KBN="
                                    SYS-25-INSPECT-CHARACTER-KBN
                    " " WRK-PARA-PRTKBN
           DISPLAY "SYS-200501-DRNAME-KSIKBN        ="
                                    SYS-25-DRNAME-KSIKBN
           DISPLAY "SYS-200501-TERMINAL-DIGIT-PRTKBN="
                                    SYS-25-TERMINAL-DIGIT-PRTKBN
           DISPLAY "SYS-200501-TENKIBYOMEI-KBN      ="
                                    SYS-25-TENKIBYOMEI-KBN
           DISPLAY "SYS-200501-HKN-RIREKI-KBN       ="
                                    SYS-25-HKN-RIREKI-KBN
      *
      *    レセプト・総括編集情報（区分コード０５のデータ読込）
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
           MOVE    "2005"           TO  SYS-KANRICD
           MOVE    "05"             TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM   TO  SYS-STYUKYMD (1:6)
           MOVE    "01"             TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-200505-REC
           ELSE
               INITIALIZE                      SYS-200505-REC
           END-IF
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       5
                   OR      SYS-200505-HKNJANUM (IDX)
                               =   SPACE
               DISPLAY "SYS-200505-HKNJANUM="  IDX " "
                                        SYS-200505-HKNJANUM (IDX) " "
                                        SYS-200505-HKNJANUM-63 (IDX)
           END-PERFORM
      *         
      *    レセプト編綴順情報
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                     SYSKANRI-REC
           MOVE    "2008"              TO  SYS-KANRICD
           MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key13"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-2008-REC
           ELSE    
               INITIALIZE                   SYS-2008-REC
           END-IF
           IF      SYS-2008-PREFKBN-PRTJYUN
                                NOT =   SPACE
               MOVE    1                TO  FLG-SYS-2008
           END-IF
      *
      *    レセプト編綴順詳細設定情報（一括作成時のみ）
           IF   WRK-PARA-SRYYM    NOT =  ZERO
               PERFORM 1201-HENTETU-HENSYU-SEC
           END-IF
      *         
      *    レセプト編集区分情報
           MOVE    SPACE           TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
           MOVE    "2009"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM  TO  SYS-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    "tbl_syskanri"  TO  WRK-MCP-TABLE
           MOVE    "key10"         TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI     =   ZERO
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
      *    県内扱い保険者情報
           MOVE    SPACE            TO  WRK-1033-TABLE
      *     
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
           MOVE    "1033"           TO  SYS-KANRICD
           MOVE    "*"              TO  SYS-KBNCD
           MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-N-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-SYSKANRI   =   1
                               OR      IDX            >=  5
               MOVE    MCPDATA-REC         TO  SYS-1033-REC
               IF      SYS-1033-EDYUKYMD   <=  WRK-CONS-SRYYMD-20080331
                   CONTINUE
               ELSE    
                   ADD     1              TO  IDX
                   MOVE    SYS-1033-STYUKYMD
                                          TO  WRK-1033-STYUKYMD   (IDX)
                   MOVE    SYS-1033-EDYUKYMD
                                          TO  WRK-1033-EDYUKYMD   (IDX)
                   MOVE    SYS-1033-TBL   TO  WRK-1033-HKNJANUM-G (IDX)
               END-IF    
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-N-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    患者番号構成管理情報編集
           MOVE    ZERO                TO  WRK-1009-KETA 
           IF   WRK-PARA-SRYYM    NOT =  ZERO
               MOVE    SPACE           TO  SYSKANRI-REC
               INITIALIZE                  SYSKANRI-REC
               MOVE    "1009"          TO  SYS-KANRICD
               MOVE    "*"             TO  SYS-KBNCD
               MOVE    WRK-PARA-SRYYM  TO  SYS-STYUKYMD (1:6)
               MOVE    "01"            TO  SYS-STYUKYMD (7:2)
               MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
               MOVE    "tbl_syskanri"  TO  WRK-MCP-TABLE
               MOVE    "key10"         TO  WRK-MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI    =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1009-REC
      *
                   DISPLAY "SYS-1009-PTNUMKSIKBN=" SYS-1009-PTNUMKSIKBN
                          " JIYKSIKETA=" SYS-1009-JIYKSIKETA
                          " HJNKSIRENNUMKETA=" SYS-1009-HJNKSIRENNUMKETA
                          " KKCKSIMAEKETA=" SYS-1009-KKCKSIMAEKETA
                          " KKCKSIRENNUMKETA=" SYS-1009-KKCKSIRENNUMKETA
                          " KKCKSIATOKETA=" SYS-1009-KKCKSIATOKETA
      *
                   EVALUATE    SYS-1009-PTNUMKSIKBN
                       WHEN    "1"
                           MOVE    SYS-1009-JIYKSIKETA TO  WRK-1009-KETA
                       WHEN    "2"
                           MOVE    SYS-1009-HJNKSIRENNUMKETA
                                                       TO  WRK-1009-KETA
                       WHEN    "3"
                           COMPUTE WRK-1009-KETA   =
                                           SYS-1009-KKCKSIMAEKETA      +
                                           SYS-1009-KKCKSIRENNUMKETA   +
                                           SYS-1009-KKCKSIATOKETA 
                   END-EVALUATE
                   IF      WRK-1009-KETA       <   3
                       MOVE    ZERO                TO  WRK-1009-KETA 
                   ELSE
                       COMPUTE WRK-1009-KETA   =   WRK-1009-KETA   -
                                                   1
                   END-IF
                   DISPLAY "WRK-1009-KETA       =" WRK-1009-KETA
               ELSE
                   MOVE    "患者番号構成管理情報が取得できませんでした"
                                           TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    2               TO  FLG-STS
                   GO  TO  120-HOSPINF-GET-EXT
               END-IF
            ELSE
               INITIALIZE                  SYS-1009-REC
            END-IF   
      *
      *    リアルタイムプレビュー時の画面設定取得
           MOVE    "0"                 TO  WRK-PRVLAYOUT
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               EVALUATE    WRK-PARA-GYOUMUKBN
                   WHEN    "1"
                   WHEN    "2"
                       MOVE    SPACE           TO  SYSKANRI-REC
                       INITIALIZE                  SYSKANRI-REC
                       MOVE    "1043"          TO  SYS-KANRICD
                       MOVE    "*"             TO  SYS-KBNCD
                       MOVE    WRK-PARA-SRYYM  TO  SYS-STYUKYMD (1:6)
                       MOVE    "01"            TO  SYS-STYUKYMD (7:2)
                       MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
                       MOVE    "tbl_syskanri"  TO  WRK-MCP-TABLE
                       MOVE    "key10"         TO  WRK-MCP-PATHNAME
                       PERFORM 900-SYSKANRI-READ-SEC
                       IF      FLG-SYSKANRI    =   ZERO
                           MOVE    MCPDATA-REC     TO  SYS-1043-REC
                       ELSE    
                           INITIALIZE                  SYS-1043-REC
                       END-IF
                       IF      SYS-1043-PRVLAYOUT
                                               =   SPACE
                           MOVE    "0"         TO  SYS-1043-PRVLAYOUT
                       END-IF
                       MOVE    SYS-1043-PRVLAYOUT
                                               TO  WRK-PRVLAYOUT
      *
                       DISPLAY "SYS-1043-PRVLAYOUT=" SYS-1043-PRVLAYOUT
                   WHEN    "3"
                       MOVE    SPACE           TO  SYSKANRI-REC
                       INITIALIZE                  SYSKANRI-REC
                       MOVE    "1038"          TO  SYS-KANRICD
                       MOVE    "*"             TO  SYS-KBNCD
                       MOVE    WRK-PARA-SRYYM  TO  SYS-STYUKYMD (1:6)
                       MOVE    "01"            TO  SYS-STYUKYMD (7:2)
                       MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
                       MOVE    "tbl_syskanri"  TO  WRK-MCP-TABLE
                       MOVE    "key10"         TO  WRK-MCP-PATHNAME
                       PERFORM 900-SYSKANRI-READ-SEC
                       IF      FLG-SYSKANRI    =   ZERO
                           MOVE    MCPDATA-REC     TO  SYS-1038-REC
                       ELSE    
                           INITIALIZE                  SYS-1038-REC
                       END-IF
                       IF      SYS-1038-PRVLAYOUT
                                               =   SPACE
                           MOVE    "0"         TO  SYS-1038-PRVLAYOUT
                       END-IF
                       MOVE    SYS-1038-PRVLAYOUT
                                               TO  WRK-PRVLAYOUT
      *
                       DISPLAY "SYS-1038-PRVLAYOUT=" SYS-1038-PRVLAYOUT
               END-EVALUATE
           END-IF
      *
      *    パラメタＤＢ読み込み
           INITIALIZE                      BTPARA-REC
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
               MOVE    WRK-PARA-JOBID      TO  BTPARA-JOBID
               MOVE    WRK-PARA-HOSPNUM    TO  BTPARA-HOSPNUM
               PERFORM 900-BTPARA-READ-SEC
               IF      FLG-BTPARA          =   1
                   MOVE
                   "パラメタ情報が取得できませんでした。"
                                           TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    2               TO  FLG-STS
               END-IF
           END-IF
           .
       120-HOSPINF-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト編綴順詳細設定情報（一括作成時のみ）
      *****************************************************************
       1201-HENTETU-HENSYU-SEC     SECTION.
      *
           INITIALIZE                  TABLE-RECE-PUTB-AREA
      *
      *    レセプト編綴順詳細設定情報（ＨＥＡＤ部）
           INITIALIZE                      RECE-PUTH-REC
           MOVE    SPA-HOSPNUM         TO  RECE-PUTH-HOSPNUM
           MOVE    WRK-PARA-SRYYM      TO  RECE-PUTH-STYUKYMD (1:6)
           MOVE    "01"                TO  RECE-PUTH-STYUKYMD (7:2)
           MOVE    RECE-PUTH-STYUKYMD  TO  RECE-PUTH-EDYUKYMD
           MOVE    RECE-PUTH-REC       TO  MCPDATA-REC
           PERFORM 900-RECE-PUTH-READ-SEC
      *
      *    「使用しない」以外のときＨＥＡＤ部の情報を２００８に設定
           IF      RECE-PUTH-PREFKBN-PRTJYUN =   "3"
             OR    FLG-RECE-PUTH             =    1
               CONTINUE
           ELSE
               MOVE    RECE-PUTH-PREFKBN-PRTJYUN
                                           TO  SYS-2008-PREFKBN-PRTJYUN
               MOVE    RECE-PUTH-HKNJANUM-PRTJYUN
                                           TO  SYS-2008-HKNJANUM-PRTJYUN
               MOVE    RECE-PUTH-HKNJANUM-PRTKBN
                                           TO  SYS-2008-HKNJANUM-PRTKBN
               MOVE    RECE-PUTH-HKNJANUM
                                           TO  SYS-2008-HKNJANUM
      *
               MOVE    1                   TO  FLG-RECE-PUT-UM
      *
      *        レセプト編綴順詳細設定情報（ＢＯＤＹ部）
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       2
                   INITIALIZE                      RECE-PUTB-REC
                   MOVE    SPA-HOSPNUM         TO  RECE-PUTB-HOSPNUM
                   EVALUATE    IDXX
                       WHEN    1
                           MOVE    "1"         TO  RECE-PUTB-PREFKBN
                       WHEN    2
                           MOVE    "2"         TO  RECE-PUTB-PREFKBN
                   END-EVALUATE
                   MOVE    WRK-PARA-SRYYM  TO  RECE-PUTB-STYUKYMD (1:6)
                   MOVE    "01"            TO  RECE-PUTB-STYUKYMD (7:2)
                   MOVE    RECE-PUTB-STYUKYMD
                                           TO  RECE-PUTB-EDYUKYMD
                   MOVE    RECE-PUTB-REC   TO  MCPDATA-REC
                   MOVE    "tbl_rece_putb" TO  MCP-TABLE
                   MOVE    "key4"          TO  MCP-PATHNAME
                   PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC          =   ZERO
                       MOVE    "tbl_rece_putb" TO  MCP-TABLE
                       MOVE    "key4"          TO  MCP-PATHNAME
                       PERFORM 900-RECE-PUTB-READ-N-SEC
                   ELSE
                       INITIALIZE                  RECE-PUTB-REC
                       MOVE    1               TO  FLG-RECE-PUTB
                   END-IF
      *
                   MOVE    ZERO            TO  IDX
                                               IDY
                   PERFORM     UNTIL   FLG-RECE-PUTB
                                           =   1
      ***                         OR      IDX >=  WRK-CONS-BODYLIST-MAX
      ***                         OR      IDY >=  WRK-CONS-KOHLIST-MAX
      *
                       EVALUATE    RECE-PUTB-KOUHI-NO
                           WHEN    ZERO
                               ADD     1               TO  IDX
                               MOVE    RECE-PUTB-REC   TO
                                       TBL-SYUBETU-RECE-PUTB-REC
                                                             (IDXX IDX)
                               MOVE    RECE-PUTB-SYUBETU-GAI
                                                       TO
                                       TBL-SYUBETU-RECE-PUTB-SYUBETU
                                                             (IDXX IDX)
                           WHEN    OTHER
                               ADD     1               TO  IDY
                               MOVE    RECE-PUTB-REC   TO
                                       TBL-KOH-RECE-PUTB-REC (IDXX IDY)
                               MOVE    RECE-PUTB-KOUHI-NUM
                                                       TO
                                       TBL-KOH-RECE-PUTB-HKNNUM
                                                             (IDXX IDY)
                       END-EVALUATE
      *
                       MOVE    "tbl_rece_putb" TO  MCP-TABLE
                       MOVE    "key4"          TO  MCP-PATHNAME
                       PERFORM 900-RECE-PUTB-READ-N-SEC
                   END-PERFORM
      *
                   MOVE    "tbl_rece_putb" TO  MCP-TABLE
                   MOVE    "key4"          TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               END-PERFORM
           END-IF    
      *
           .
       1201-HENTETU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＥＦファイル種別不明分チェック処理
      *****************************************************************
       1001-EFOUT-CHECK-SEC        SECTION.
      *
           PERFORM 900-RECE02-READ-SEC
      *
           PERFORM         UNTIL   FLG-RECE02     =   1
      *        不明分があれば処理終了
               IF    ( RECE02-RECESYUBETU  =   ZERO )
               OR    ( RECE02-ERR-ERRKBN   =   15   )
                   IF      RECE02-RECESYUBETU  =   ZERO
                       STRING   "レセプトが種別不明です　患者番号＝"
                                                   DELIMITED  BY  SIZE
                                RECE02-PTNUM       DELIMITED  BY  SIZE
                                                   INTO  WRK-RECEERR
                       END-STRING
                   ELSE
                       STRING  "０５９で始まる「その他材料コード」"
                                                   DELIMITED  BY  SIZE
                               "が使用されています　患者番号＝"
                                                   DELIMITED  BY  SIZE
                                RECE02-PTNUM       DELIMITED  BY  SIZE
                                                   INTO  WRK-RECEERR
                       END-STRING
                   END-IF
                   PERFORM 500-ERR-HENSYU-SEC
                   STRING  "ORCR0105 "         DELIMITED   BY  SIZE
                           WRK-RECEERR         DELIMITED   BY  SPACE
                           LOW-VALUE           DELIMITED   BY  SIZE
                                               INTO    WRK-ERRMSG
                   END-STRING
                   PERFORM 500-COBABORT-SEC
               END-IF
      *
               PERFORM 900-RECE02-READ-SEC
           END-PERFORM
      *
           .
       1001-EFOUT-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM         UNTIL   FLG-RECE02     =   1
                           OR      FLG-STS        =   2 
      *
      *        患者番号別保険履歴情報編集処理      
               PERFORM 2001-RIREKI-HEN-SEC 
               MOVE    KEY-NEW         TO  KEY-OLD
               PERFORM         UNTIL   FLG-RECE02     =   1
                               OR      FLG-STS        =   2
                               OR      KEY-NEW1   NOT =   KEY-OLD1
      *            レセプト種別別件数カウント
                   MOVE    ZERO            TO  CNT-KENSU
                   MOVE    KEY-NEW         TO  KEY-OLD
                   PERFORM         UNTIL   FLG-RECE02     =   1
                                   OR      FLG-STS        =   2
                                   OR      KEY-NEW    NOT =   KEY-OLD
      *
                       ADD     1                      TO  CNT-KENSU
      *                                      
                       PERFORM 2001-SYORI-SEC
      *
                       ADD     1               TO  CNT-COMMIT
                       IF      CNT-COMMIT      =   300
                           MOVE    ZERO            TO  CNT-COMMIT
                           PERFORM 900-DBCOMMIT-SEC
                           PERFORM 900-DBSTART-SEC
                       END-IF
      *
                       REWRITE RECE02-REC
                       PERFORM 990-RECE02-READ-SEC
                   END-PERFORM
               END-PERFORM    
           END-PERFORM
      *
      *    公費請求書出力
           IF      FLG-STS         =   2
               CONTINUE
           ELSE
               PERFORM 2002-KOHSKY-WRITE-SEC
           END-IF 
      *
           MOVE    2               TO  FLG-STS
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号別保険履歴情報編集処理
      *****************************************************************
       2001-RIREKI-HEN-SEC      SECTION.
      *
      *    診療年月編集
           MOVE    RECE02-SRYYM        TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3 (11:12)
                                       TO  WRK-SRYYMST
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    RECE02-SRYYM        TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD 
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3 (11:12)
                                       TO  WRK-SRYYMED
      *
           MOVE    SPACE               TO  WRK-HKN-RIREKI-AREA
           MOVE    ZERO                TO  WRK-RIREKI-MAX
                                           CNT-RECE09
      *     
           INITIALIZE                      RECE09-REC
           MOVE    RECE02-HOSPNUM      TO  RECE09-HOSPNUM
           MOVE    RECE02-SRYYM        TO  RECE09-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE09-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE09-PTID
           MOVE    RECE02-RECEKA       TO  RECE09-RECEKA
      *
           START   RECE09-FILE
               KEY IS    >=   RECE09-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE09
               NOT INVALID
                   PERFORM 990-RECE09-NEXT-SEC
           END-START
           PERFORM UNTIL ( FLG-RECE09           =   1                )
                   OR    ( RECE02-HOSPNUM   NOT =   RECE09-HOSPNUM   )
                   OR    ( RECE02-SRYYM     NOT =   RECE09-SRYYM     )
                   OR    ( RECE02-NYUGAIKBN NOT =   RECE09-NYUGAIKBN )
                   OR    ( RECE02-PTID      NOT =   RECE09-PTID      )
                   OR    ( RECE02-RECEKA    NOT =   RECE09-RECEKA    )
      *
               ADD     1                  TO  CNT-RECE09
      *
               ADD     1                  TO  WRK-RIREKI-MAX
               IF      WRK-RIREKI-MAX     >   100
                   MOVE    "保険履歴情報がテーブルオーバーしました"
                                              TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    2               TO  FLG-STS
                   GO  TO  2001-RIREKI-HEN-EXT
               END-IF    
      *
               EVALUATE    RECE09-TEISYUTUSAKI
                   WHEN    "1"
                       IF      RECE09-HKNID   >   ZERO
                           MOVE    "社保"     TO  WRK-HKN-TEISYUTUSAKI
                       ELSE
                           MOVE    SPACE      TO  WRK-HKN-TEISYUTUSAKI   
                       END-IF
                   WHEN    OTHER
                       MOVE    SPACE      TO  WRK-HKN-TEISYUTUSAKI   
               END-EVALUATE          
      *
               EVALUATE    RECE09-TEISYUTUSAKI
                   WHEN    "6"
                       MOVE    SPACE      TO  WRK-HKN-FIL
                   WHEN    OTHER
                       MOVE    "―"       TO  WRK-HKN-FIL
               END-EVALUATE
      *
               IF  ( ( RECE09-NUM         =   SPACE )
               AND   ( RECE09-KIGO        =   SPACE )
               AND   ( RECE09-HKNJANUM    =   SPACE ) )
               OR  (   SYS-25-HKN-RIREKI-KBN
                                           =   "1"    )
                   STRING  WRK-HKN-TEISYUTUSAKI   DELIMITED  BY  SPACE
                           RECE09-TANSEIDONAME    DELIMITED  BY  SPACE
                              INTO    WRK-HKN-RIREKI (WRK-RIREKI-MAX)
                   END-STRING
               ELSE
                   MOVE    SPACE           TO  WRK-HKN-HKNJANUM
                   IF      RECE09-HKNJANUM     NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI
                       MOVE    RECE09-HKNJANUM TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-HKN-HKNJANUM
                       END-IF
                   END-IF
                   MOVE    SPACE           TO  WRK-HKN-KIGO
                   IF      RECE09-KIGO     NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI
                       MOVE    RECE09-KIGO     TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-HKN-KIGO
                       END-IF
                   END-IF
                   MOVE    SPACE           TO  WRK-HKN-NUM
                   IF      RECE09-NUM      NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI
                       MOVE    RECE09-NUM      TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                                   TO  WRK-HKN-NUM
                       END-IF
                   END-IF
                   MOVE    SPACE                  TO  WRK-HKN-RIREKI-X
                   IF      RECE09-HKNID   >   ZERO
                       STRING
                           WRK-HKN-TEISYUTUSAKI   DELIMITED  BY  SPACE
                           RECE09-TANSEIDONAME    DELIMITED  BY  SPACE
                           "　"                   DELIMITED  BY  SIZE
                           WRK-HKN-HKNJANUM       DELIMITED  BY  SPACE
                           "　"                   DELIMITED  BY  SIZE
                           WRK-HKN-KIGO           DELIMITED  BY  SPACE
                           WRK-HKN-FIL            DELIMITED  BY  SPACE
                           WRK-HKN-NUM            DELIMITED  BY  SPACE
                                              INTO    WRK-HKN-RIREKI-X
                       END-STRING
                   ELSE
                       STRING
                           RECE09-TANSEIDONAME    DELIMITED  BY  SPACE
                           "　"                   DELIMITED  BY  SIZE
                           WRK-HKN-KIGO           DELIMITED  BY  SPACE
                           WRK-HKN-FIL            DELIMITED  BY  SPACE
                           WRK-HKN-NUM            DELIMITED  BY  SPACE
                                              INTO    WRK-HKN-RIREKI-X
                       END-STRING
                   END-IF
                   IF      WRK-HKN-RIREKI-X (53:) NOT =   SPACE           
                       STRING  WRK-HKN-TEISYUTUSAKI
                                                  DELIMITED  BY  SPACE
                               RECE09-TANSEIDONAME
                                                  DELIMITED  BY  SPACE
                               "　"               DELIMITED  BY  SIZE
                               WRK-HKN-HKNJANUM   DELIMITED  BY  SPACE
                              INTO    WRK-HKN-RIREKI (WRK-RIREKI-MAX)
                       END-STRING
                       ADD     1                   TO  WRK-RIREKI-MAX
                       IF      WRK-RIREKI-MAX     >   100
                           MOVE
                               "保険履歴情報がテーブルオーバーしました"
                                                   TO  WRK-RECEERR
                           PERFORM 500-ERR-HENSYU-SEC
                           MOVE    2               TO  FLG-STS
                           GO  TO  2001-RIREKI-HEN-EXT
                       END-IF    
                       STRING  WRK-HKN-KIGO       DELIMITED  BY  SPACE
                               WRK-HKN-FIL        DELIMITED  BY  SPACE
                               WRK-HKN-NUM        DELIMITED  BY  SPACE
                               INTO
                                   WRK-HKN-RIREKI (WRK-RIREKI-MAX)(13:)
                       END-STRING
                   ELSE
                       MOVE    WRK-HKN-RIREKI-X   TO
                                       WRK-HKN-RIREKI (WRK-RIREKI-MAX)  
                   END-IF   
               END-IF                   
      *********IF      WRK-HKN-RIREKI (WRK-RIREKI-MAX)(13:)
               IF      WRK-HKN-RIREKI (WRK-RIREKI-MAX)
                                          NOT =   SPACE
                   ADD     1                   TO  WRK-RIREKI-MAX
                   IF      WRK-RIREKI-MAX     >   100
                       MOVE    "保険履歴情報がテーブルオーバーしました"
                                                   TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    2               TO  FLG-STS
                       GO  TO  2001-RIREKI-HEN-EXT
                   END-IF    
               END-IF                             
      *
               MOVE    RECE09-TEKSTYMDX   TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3-G(1:2)    TO  
                                  WRK-HKN-TEKSTYMD(WRK-RIREKI-MAX)(1:2)
               MOVE    LNK-DAY2-EDTYMD3(5:18)     TO  
                                  WRK-HKN-TEKSTYMD(WRK-RIREKI-MAX)(3:)
      *  
               IF      RECE09-TEKEDYMDX(1:6)  =   RECE02-SRYYM                      
                   MOVE    RECE09-TEKEDYMDX   TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3-G(1:2)    TO  
                                  WRK-HKN-TEKEDYMD(WRK-RIREKI-MAX)(1:2)
                   MOVE    LNK-DAY2-EDTYMD3(5:18)     TO  
                                  WRK-HKN-TEKEDYMD(WRK-RIREKI-MAX)(3:)
                   MOVE    "から"             TO
                                      WRK-HKN-FL1     (WRK-RIREKI-MAX)
                   MOVE    "まで"             TO
                                      WRK-HKN-FL2     (WRK-RIREKI-MAX)
               ELSE
                   MOVE    "から有効"         TO
                                      WRK-HKN-FL      (WRK-RIREKI-MAX) 
               END-IF                                    
      *
               PERFORM 990-RECE09-NEXT-SEC
           END-PERFORM
      *
           IF    ( CNT-RECE09             =   1    )
               MOVE    SPACE                  TO  WRK-HKN-RIREKI-AREA
               MOVE    ZERO                   TO  WRK-RIREKI-MAX
           END-IF          
      *
           .
       2001-RIREKI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       2001-SYORI-SEC                SECTION.
      *
           MOVE    1                  TO  CNT-PAGE
           MOVE    WRK-TEKIYOU1-LINE  TO  WRK-TEKIYOU-LINE
           INITIALIZE                     TABLE-AREA
           MOVE    SPACE              TO  WRK-DRNAME
      *
      *    医療保険・公費のフラグ
           MOVE    SPACE              TO  WRK-3SYU-KBN
           MOVE    ZERO               TO  SEIHIDX
           MOVE    ZERO               TO  KAKKIDX
           MOVE    ZERO               TO  SESNIDX
           PERFORM   VARYING   IDX  FROM    1   BY  1
                     UNTIL   ( IDX                >    4    )
                     OR      ( RECE02-KOHID(IDX)  =   ZERO  )
      *        生保
               IF      RECE02-KOHNUM (IDX)  =   "012"
                   MOVE    "1"          TO  WRK-3SYU (1:1)
                   MOVE     IDX         TO  SEIHIDX
               END-IF
      *        結核
               IF      RECE02-KOHNUM (IDX)  =   "010"  OR  "011"
                   MOVE    "1"          TO  WRK-3SYU (2:1)
                   MOVE     IDX         TO  KAKKIDX
               END-IF
      *        自立支援法
               IF      RECE02-KOHNUM (IDX)  =   "021"  OR
                                                "015"  OR  "016"  OR
                                                "024"
                   MOVE    "1"          TO  WRK-3SYU (3:1)
                   MOVE     IDX         TO  SESNIDX
               END-IF
           END-PERFORM
      *
           MOVE     ZERO       TO    FLG-JIRITU-HENSYU
           IF ((RECE02-HKNNUM     =  SPACE                  ) AND
               (RECE02-KOHNUM(1)  =  "021" OR "015" OR "016") AND
               (RECE02-KOHNUM(2)  =  "012"                  ) AND
               (RECE02-KOHNUM(3)  =  SPACE                  ) AND
               (RECE02-KOHNUM(4)  =  SPACE                  ))
                    MOVE       1       TO    FLG-JIRITU-HENSYU
           END-IF
           IF ((RECE02-HKNNUM     =  SPACE                  ) AND
               (RECE02-KOHNUM(1)  =  "010"                  ) AND
               (RECE02-KOHNUM(2)  =  "012"                  ) AND
               (RECE02-KOHNUM(3)  =  "012"                  ) AND
               (RECE02-KOHNUM(4)  =  SPACE                  ))
                    MOVE       2       TO    FLG-JIRITU-HENSYU
           END-IF
           IF ((RECE02-HKNNUM     =  SPACE                  ) AND
               (RECE02-KOHNUM(1)  =  "021" OR "015" OR "016") AND
               (RECE02-KOHNUM(2)  =  "012"                  ) AND
               (RECE02-KOHNUM(3)  =  "012"                  ) AND
               (RECE02-KOHNUM(4)  =  SPACE                  ))
                    MOVE       3       TO    FLG-JIRITU-HENSYU
           END-IF
      *
      *    県単用項目ファイル読込処理
           PERFORM 301-RECE11-HENSYU-SEC
      *
      *    帳票編集<ヘッダー部>処理
           PERFORM 310-HC101-HEAD-HEN-SEC
      *
      *    帳票編集<点数>処理
           PERFORM 330-HC101-TENSU-HEN-SEC
      *
      *    帳票編集<適用欄下段コメント>処理
           PERFORM 350-HC101-HOSOKU-HEN-SEC
      *
      *    帳票編集<傷病名>処理
           PERFORM 320-HC101-BYOMEI-HEN-SEC
      *
      *    帳票編集<公費３、公費４、適用欄上段コメント>処理
           PERFORM 380-HC101-TEKIYOU-COM-HEN-SEC
      *
      *    帳票編集<入退院履歴>処理
           IF    ( SYS-2005-NYUTAIINREKI-KSIKBN
                               NOT =   "0" )               
           AND   ( RECE02-NYUINRRK-FLG
                                   =   "1" )               
               PERFORM 380-HC101-NYUTAI-RIREKI-HEN-SEC
           END-IF
      *
      *    帳票編集<手術歴>処理
           PERFORM 380-HC101-OPECOM-HEN-SEC 
      *
      *    帳票編集<剤>処理
           PERFORM 340-HC101-ZAI-HEN-SEC
      *
      *    帳票編集<コメント>処理
           PERFORM 360-HC101-COMMENT-HEN-SEC 
      *
      *    帳票編集<保険履歴情報>処理
           PERFORM 370-HC101-HKN-RIREKI-HEN-SEC 
      *
      *    帳票印刷処理
           PERFORM 390-PRINT-OUT-SEC
      *
      *    帳票更新処理
           PERFORM 391-DB-WRITE-SEC
      *
           .
       2001-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    公費請求書出力処理
      *****************************************************************
       2002-KOHSKY-WRITE-SEC           SECTION.
      *
           IF      WRK-PARA-SHELLID     =   "RECEPTX"
               GO  TO  2002-KOHSKY-WRITE-EXT
           END-IF
      *
           PERFORM 990-RECE02X-READ-SEC
      *
           PERFORM             UNTIL   FLG-RECE02X =   1
                               OR      FLG-ERR     =   1
                               OR      FLG-STS     =   2
      *
               INITIALIZE                     TABLE-AREA
      *
               IF      RECE02X-RECESYUBETU     =   ZERO
                   IF      RECE02X-HKNNUM      NOT =   SPACE 
                       CONTINUE
                   ELSE
                       PERFORM 20021-KOHSKY-FUMEI-HENSYU-SEC
                   END-IF
               ELSE
                   PERFORM 20022-KOHSKY-WRITE-SEC
               END-IF 
      *
               PERFORM 990-RECE02X-READ-SEC
           END-PERFORM
      *
           .
       2002-KOHSKY-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書不明分レセプト出力処理
      *****************************************************************
       20021-KOHSKY-FUMEI-HENSYU-SEC           SECTION.
      *
           IF      WRK-PARA-SRYYM      =   ZERO
      *        医療機関編集
               MOVE  RECE02X-SRYYM         TO  WRK-SYS-SRYYMD(1:6)
               MOVE  "01"                  TO  WRK-SYS-SRYYMD(7:2)
               PERFORM 900-SYSKANRI-HENSYU-SEC
           END-IF
      *
           MOVE    RECE02X-REC         TO  RECE02-REC 
      *
           MOVE    1                   TO  CNT-PAGE
           MOVE    1                   TO  FLG-RECE11
      *
           MOVE    SPACE               TO  WRK-3SYU-KBN
           MOVE    ZERO                TO  SEIHIDX
           MOVE    ZERO                TO  KAKKIDX
           MOVE    ZERO                TO  SESNIDX
      *
      *    帳票編集<ヘッダー部>処理
           PERFORM 310-HC101-HEAD-HEN-SEC
      *
      *    帳票印刷処理
           PERFORM 390-PRINT-OUT-SEC
      *
      *    帳票更新処理
           PERFORM 391-DB-WRITE-SEC
      *
           .
       20021-KOHSKY-FUMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書編集処理
      *****************************************************************
       20022-KOHSKY-WRITE-SEC          SECTION.
      *
           MOVE    SPACE               TO  KOHSKY-REC
           INITIALIZE                      KOHSKY-REC
           MOVE    RECE02X-HOSPNUM     TO  KOHSKY-HOSPNUM
           MOVE    RECE02X-SRYYM       TO  KOHSKY-SRYYM
           MOVE    RECE02X-NYUGAIKBN   TO  KOHSKY-NYUGAIKBN
           MOVE    RECE02X-PTID        TO  KOHSKY-PTID
           IF      RECE02X-RECEKA      =   SPACE
               MOVE    ZERO                TO  KOHSKY-RECEKA
           ELSE
               MOVE    RECE02X-RECEKA      TO  KOHSKY-RECEKA
           END-IF
           MOVE    RECE02X-TEISYUTUSAKI    TO  KOHSKY-TEISYUTUSAKI
           IF      RECE02X-HKNJANUM-KEY    =   SPACE
               MOVE    ZERO                    TO  KOHSKY-HKNJANUM-KEY
           ELSE
               MOVE    RECE02X-HKNJANUM-KEY    TO  KOHSKY-HKNJANUM-KEY
           END-IF
           IF      RECE02X-HOJOKBN-KEY     =   SPACE
               MOVE    ZERO                    TO  KOHSKY-HOJOKBN-KEY
           ELSE
               MOVE    RECE02X-HOJOKBN-KEY     TO  KOHSKY-HOJOKBN-KEY
           END-IF
           MOVE    RECE02X-KOHID-KEY   TO  KOHSKY-KOHID-KEY
           IF      RECE02X-TEKSTYMD    =   SPACE
               MOVE    ZERO                TO  KOHSKY-TEKSTYMD
           ELSE
               MOVE    RECE02X-TEKSTYMD    TO  KOHSKY-TEKSTYMD
           END-IF
           MOVE    RECE02X-RECEIPT-KBN TO  KOHSKY-RECEIPT-KBN
      *
           MOVE     999999999          TO  WRK-TAIHI-FTNMONEY
      *    レセプト種別を請求管理より編集
           INITIALIZE                      RECE10-REC
           MOVE    KOHSKY-KEY          TO  RECE10-KEY
           MOVE    "tbl_seikyu"        TO  WRK-MCP-TABLE
           MOVE    "key43"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SEIKYU-INV-SEC
           IF      FLG-SEIKYU          =   ZERO
               MOVE    MCPDATA-REC         TO  RECE10-REC
      *        点検用レセ０点公費のとき作成しない
               IF      RECE10-SKYKBN-RECEDEN   =   "1"
                   DISPLAY "RECE10-SKYKBN-RECEDEN="
                                               RECE10-SKYKBN-RECEDEN
                   GO  TO  20022-KOHSKY-WRITE-EXT
               END-IF
      *    
               MOVE    RECE10-RECESYUBETU  TO  KOHSKY-RECESYUBETU
               MOVE    RECE10-RECEKA       TO  KOHSKY-RECEKA
               IF   RECE10-FLG (5)  NOT =  ZERO
                 MOVE  RECE10-FTNMONEY(1)  TO  WRK-TAIHI-FTNMONEY
               END-IF
           ELSE
               INITIALIZE                      RECE10-REC
               MOVE    KOHSKY-KEY          TO  RECE10-KEY
               MOVE    "tbl_seikyu"        TO  WRK-MCP-TABLE
               MOVE    "key84"             TO  WRK-MCP-PATHNAME
               PERFORM 900-SEIKYU-INV-SEC
               IF      FLG-SEIKYU          =   ZERO
                   MOVE    MCPDATA-REC         TO  RECE10-REC
      *            点検用レセ０点公費のとき作成しない
                   IF      RECE10-SKYKBN-RECEDEN   =   "1"
                       DISPLAY "RECE10-SKYKBN-RECEDEN="
                                               RECE10-SKYKBN-RECEDEN
                       GO  TO  20022-KOHSKY-WRITE-EXT
                   END-IF
      *    
                   MOVE    RECE10-RECESYUBETU  TO  KOHSKY-RECESYUBETU
                   MOVE    RECE10-RECEKA       TO  KOHSKY-RECEKA
                   MOVE    RECE10-TEKSTYMD     TO  KOHSKY-TEKSTYMD
                   IF      RECE10-FLG (5)  NOT =   ZERO
                       MOVE    RECE10-FTNMONEY(1)  TO WRK-TAIHI-FTNMONEY
                   END-IF
               ELSE
                   GO  TO  20022-KOHSKY-WRITE-EXT
               END-IF
           END-IF
      *
           PERFORM 900-KOHSKY-INV-SEC
           IF      FLG-KOHSKY          =   ZERO
               MOVE    MCPDATA-REC         TO  KOHSKY-REC
               MOVE    WRK-PARA-CREYMD     TO  KOHSKY-UPDYMD
               IF      KOHSKY-SKYKBN       =   "2"
                   MOVE    "1"                 TO  KOHSKY-SKYKBN
               END-IF 
               MOVE    "2"                 TO  KOHSKY-TEISEIKBN
           ELSE
               MOVE    "1"                 TO  KOHSKY-SKYKBN
               MOVE    WRK-PARA-CREYMD     TO  KOHSKY-CREYMD
               MOVE    ZERO                TO  KOHSKY-UPDYMD
               MOVE    ZERO                TO  KOHSKY-SKYYM
                                               KOHSKY-HENREIYM
                                               KOHSKY-TEISEIKBN
           END-IF
           MOVE    RECE02X-PTNUM       TO  KOHSKY-PTNUM
           MOVE    RECE02X-KANANAME    TO  KOHSKY-KANANAME
           MOVE    RECE02X-NAME        TO  KOHSKY-NAME
           MOVE    RECE02X-SEX         TO  KOHSKY-SEX
           MOVE    RECE02X-BIRTHDAY    TO  KOHSKY-BIRTHDAY
           MOVE    RECE02X-AGE         TO  KOHSKY-AGE  
           MOVE    RECE02X-DEATHKBN    TO  KOHSKY-DEATHKBN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       20
               MOVE    RECE02X-SRYKA   (IDX)
                                       TO  KOHSKY-SRYKA (IDX)
           END-PERFORM
           MOVE    RECE02X-HKN         TO  KOHSKY-HKN
      *****IF      RECE02X-KYURATE  >  ZERO
      *         COMPUTE WRK-KYURATE-9   =
      *                        (100     -   RECE02X-KYURATE)
      *                                         / 10 
      *         MOVE    WRK-KYURATE-9   TO  KOHSKY-KYURATE
      *     ELSE
      *         MOVE    ZERO            TO  KOHSKY-KYURATE
      *****END-IF             
           MOVE    RECE02X-KYURATE     TO  KOHSKY-KYURATE
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
               MOVE    RECE02X-KOHID       (IDX) 
                                       TO  KOHSKY-KOHID      (IDX)
               MOVE    RECE02X-KOHNUM      (IDX)
                                       TO  KOHSKY-KOHNUM     (IDX)
               MOVE    RECE02X-KOHHBTNUM   (IDX)
                                       TO  KOHSKY-KOHHBTNUM  (IDX)
               MOVE    RECE02X-KOHFTNJANUM (IDX)
                                       TO  KOHSKY-KOHFTNJANUM(IDX)
               MOVE    RECE02X-KOHJKYSNUM  (IDX)
                                       TO  KOHSKY-KOHJKYSNUM (IDX)
           END-PERFORM
      ***  PERFORM VARYING IDX FROM    1   BY  1
      ***          UNTIL   IDX >       10
      ***      MOVE   RECE02X-HKNCOMBIINF (IDX)
      ***                                  TO KOHSKY-HKNCOMBIINF (IDX)
      ***  END-PERFORM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               MOVE   RECE02X-HKNCOMBI(IDX)   TO  KOHSKY-HKNCOMBI(IDX)
               MOVE   RECE02X-HKNNUMHC(IDX)   TO  KOHSKY-HKNNUMHC(IDX)
               MOVE   RECE02X-KOHNUMHC(IDX 1) TO  KOHSKY-KOHNUMHC(IDX 1)
               MOVE   RECE02X-KOHNUMHC(IDX 2) TO  KOHSKY-KOHNUMHC(IDX 2)
               MOVE   RECE02X-KOHNUMHC(IDX 3) TO  KOHSKY-KOHNUMHC(IDX 3)
               MOVE   RECE02X-KOHNUMHC(IDX 4) TO  KOHSKY-KOHNUMHC(IDX 4)
           END-PERFORM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       50
               MOVE    RECE02X-SRYKBNINF (IDX)
                                       TO  KOHSKY-SRYKBNINF (IDX)
           END-PERFORM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       5
               MOVE    RECE02X-JNISSUINF (IDX)
                                       TO  KOHSKY-JNISSUINF (IDX)
               MOVE    RECE02X-TOTALTENINF (IDX)
                                       TO  KOHSKY-TOTALTENINF (IDX)
      *********MOVE    RECE02X-FTNMONEYINF(IDX)
      *********                        TO  KOHSKY-FTNMONEYINF (IDX)
               MOVE    RECE02X-FTNMONEY   (IDX)
                                       TO  KOHSKY-FTNMONEY    (IDX)
               MOVE    RECE02X-SHOKUJIINF (IDX)
                                       TO  KOHSKY-SHOKUJIINF (IDX)
           END-PERFORM
           IF  WRK-TAIHI-FTNMONEY  NOT = 999999999
               MOVE    WRK-TAIHI-FTNMONEY
                                       TO  KOHSKY-FTNMONEY(1)
           END-IF
           MOVE    RECE02X-SHOSHINTIME TO  KOHSKY-SHOSHINTIME
           IF    ((RECE02X-ZAITAKU  =  "1"          ) AND
                  (RECE02X-SRYYM   <=  "201203"     ))
           OR    ( RECE02X-ZAITAKU-SANTEI-FLG  = "1" )
             MOVE       "1"     TO   KOHSKY-ZAITAKU
           END-IF
           IF      RECE02X-ZAITAKU  =  "2" OR "3"
             IF  RECE02X-SRYYM   >=  "201501"
               EVALUATE  RECE02X-DISCOUNTTENTNK
                 WHEN       "26"
                   MOVE   "2"     TO   KOHSKY-ZAITAKU
                 WHEN       "27"
                   MOVE   "3"     TO   KOHSKY-ZAITAKU
                 WHEN       "28"
                   MOVE   "4"     TO   KOHSKY-ZAITAKU
                 WHEN       "29"
                   MOVE   "5"     TO   KOHSKY-ZAITAKU
                 WHEN       "30"
                   MOVE   "6"     TO   KOHSKY-ZAITAKU
               END-EVALUATE
             ELSE
               EVALUATE  RECE02X-DISCOUNTTENTNK
                 WHEN       "17"
                   MOVE   "2"     TO   KOHSKY-ZAITAKU
                 WHEN       "18"
                   MOVE   "3"     TO   KOHSKY-ZAITAKU
                 WHEN       "19"
                   MOVE   "4"     TO   KOHSKY-ZAITAKU
               END-EVALUATE
             END-IF
           END-IF
           IF      RECE02X-ZAITAKU-SAIKEI  NOT =  999999999
             MOVE  RECE02X-ZAITAKU-SAIKEI  TO  KOHSKY-ZAITAKU-SAIKEI
           END-IF
      *    一部負担金・食事療養費標準負担額区分
           MOVE    RECE02X-SHOKUJIFTNKBN
                                       TO  KOHSKY-SHOKUJIFTNKBN
      *
           MOVE    RECE02X-SYUGAKUKBN  TO  KOHSKY-FLG (1)
      *    ７０歳代前半の特例措置対象者のみ２割の記載
           IF      RECE02X-RJNGAIKOHFTN (2)
                                       =   1
               MOVE    1                   TO  KOHSKY-FLG (2)
           END-IF
      *    ７５歳到達月の特例対象者
           IF      RECE02X-RJNNYUKOHFTN (1)  NOT =  ZERO
               MOVE   RECE02X-RJNNYUKOHFTN (1)  TO  KOHSKY-FLG (3)
           END-IF
      *    特定疾患等対象者
           IF      RECE02X-RJNNYUKOHFTN (2)  NOT =  ZERO
               MOVE   RECE02X-RJNNYUKOHFTN (2)  TO  KOHSKY-FLG (4)
           END-IF
      *
           MOVE    KOHSKY-REC          TO  MCPDATA-REC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-KOHSKY          =   1    
               MOVE    "DBINSERT"          TO  MCP-FUNC
           ELSE
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           END-IF 
           PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "ORCR0105 KOHSKY MCP-RC= "  MCP-RC
               IF      FLG-KOHSKY          =   1
                   MOVE    "公費請求ＤＢに追加できませんでした"
                                               TO  WRK-RECEERR
               ELSE
                   MOVE    "公費請求ＤＢに更新できませんでした"
                                               TO  WRK-RECEERR
               END-IF
      *********PERFORM 500-ERR-HENSYU-SEC
               PERFORM 900-DB-ERR-ABORT-SEC
           END-IF
      *
           .
       20022-KOHSKY-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    県単用項目ファイル読込処理
      *****************************************************************
       301-RECE11-HENSYU-SEC           SECTION.
      *
           MOVE    SPACE               TO  RECE11-HC06
      *       
           INITIALIZE                      RECE11-REC
           MOVE    RECE02-KEY          TO  RECE11-KEY
           PERFORM 990-RECE11-READ-SEC
           IF      FLG-RECE11          =   ZERO
               MOVE    RECE11-PRTDATA      TO  RECE11-HC06
      *        カスタマイズ項目があるときＯＣＲに反映させる 
               IF ( ( RECE02-TEISYUTUSAKI   =   1               )  AND
                    ( SYS-2005-SYAOCRKBN    =   ZERO    OR  "2" )      )
               OR ( ( RECE02-TEISYUTUSAKI   =   2               )  AND
                    ( SYS-2005-KKHOCRKBN    =   "1"     OR  "3" )      )
               OR ( ( RECE02-TEISYUTUSAKI   =   6               )  AND
                    ( SYS-25-KOUIKIOCRKBN
                                            =   "1"     OR  "3" )      )
                   MOVE    RECE02-OCR1            TO  OCR1-AREA
                   MOVE    RECE02-OCR2            TO  OCR2-AREA
                   MOVE    RECE02-OCR3            TO  OCR3-AREA
               END-IF  
           END-IF    
           .
       301-RECE11-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HC101-HEAD-HEN-SEC          SECTION.
      *
           INITIALIZE                      HC06
           MOVE    SPACE               TO  HC06
      *
      *    特別療養費の場合「特別療養費」文字記載をシス管の設定により行う
           IF      RECE02-RECESYUBETU  =   6666
               EVALUATE    SYS-2005-MOJI-KISAIKBN
                   WHEN    ZERO
                       MOVE    "特別療養費"    TO  HC06-MOJI1
                   WHEN    "1"
                       MOVE    "特別療養費"    TO  HC06-MOJI2
               END-EVALUATE
           END-IF
      *
      *    患者番号
           MOVE    RECE02-PTNUM        TO  HC06-PTNUM
      *
      *    提出先
           IF    ( RECE02-RECESYUBETU  =    ZERO    OR  8888 )
           OR    ( RECE02-ERR-ERRKBN   >    ZERO             )
               MOVE    SPACE           TO  HC06-TEISYUTUSAKI
           ELSE    
               EVALUATE    RECE02-TEISYUTUSAKI
                   WHEN    1
                       MOVE    "１社"          TO  HC06-TEISYUTUSAKI
                   WHEN    2
                   WHEN    6
                       MOVE    "２国"          TO  HC06-TEISYUTUSAKI
                   WHEN    OTHER
                       MOVE    SPACE           TO  HC06-TEISYUTUSAKI
               END-EVALUATE
           END-IF    
      *
      *    診療年月（診療年月日として退避）
           MOVE    RECE02-SRYYM        TO  WRK-SYMD
           MOVE    1                   TO  WRK-SDD
           MOVE    WRK-SYMD            TO  WRK-SRYYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD2-G  TO  HC06-SRYGG
           MOVE    LNK-DAY2-EDTYMD1-Y  TO  HC06-SRYY
           MOVE    LNK-DAY2-EDTYMD1-M  TO  HC06-SRYM
      *
      *    県番
           MOVE    SYS-1001-PREFNUM    TO  HC06-PREFNUM
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN    TO  HC06-HOSPCDN
      *
      *    保険種別１
           PERFORM 3110-HKNSBT1-SET-SEC
      *
      *    保険種別２
           PERFORM 3110-HKNSBT2-SET-SEC
      *
      *    本人・家族
           PERFORM 3110-HONKZK-SET-SEC
      *
      *    特別療養費の場合シス管の設定により
           IF      RECE02-RECESYUBETU  =   6666
               EVALUATE    SYS-2005-RECESYUBETU-KISAIKBN
                   WHEN    ZERO
                       CONTINUE
                   WHEN    "1"
                       MOVE    SPACE       TO  HC06-HKNSBT1
                                               HC06-HKNSBT2
                                               HC06-HONKZK
               END-EVALUATE
           END-IF
      *
      *    保険者番号・公費−負担者番号・受給者番号・診療実日数・記号番号
           PERFORM 3110-KANREN-NUM-SET-SEC
      *
      *    給付割合
           PERFORM 3110-KYURATE-SET-SEC
      *
      *    カナ氏名
           IF     RECE02-KANANAME (41:)    NOT =   SPACE
                MOVE    RECE02-KANANAME    TO  HC06-KANANAME2
           ELSE
                MOVE    RECE02-KANANAME    TO  HC06-KANANAME
           END-IF
      *
      *    氏名
           MOVE    RECE02-NAME         TO  HC06-NAME
      *
      *    性別
           EVALUATE    RECE02-SEX
           WHEN    "1"
               MOVE    "１"            TO  HC06-SEXNUM
               MOVE    "男"            TO  HC06-SEXN
           WHEN    "2"
               MOVE    "２"            TO  HC06-SEXNUM
               MOVE    "女"            TO  HC06-SEXN
           END-EVALUATE
      *
      *    生年月日
           MOVE    RECE02-BIRTHDAY     TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           INITIALIZE              ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI 
           MOVE    LNK-DAY1-GENGO    TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      KANACHK-RC      =   ZERO
           AND     KANACHK-MAX     >   ZERO
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                       TO  HC06-BIRTHDAY(1:)
           END-IF    
           MOVE    RECE02-BIRTHDAY     TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD2 (1:2)
                                       TO  HC06-BIRTHDAY(3:2)
           MOVE    LNK-DAY2-EDTYMD2 (5:)
                                       TO  HC06-BIRTHDAY(5:)
      *
      *    特記事項
           PERFORM 3110-TOKKI-SET-SEC
      *
      *    職務上の事由
           EVALUATE    RECE02-HKNNUM
               WHEN    "002"
               WHEN    "031"  THRU  "034"
                   EVALUATE    RECE02-HOJOKBN
                       WHEN    "1"
                       WHEN    "A"
                       WHEN    "D"
                       WHEN    "G"
                           MOVE    "職務上"              TO  HC06-JIYU
                       WHEN    "2"
                       WHEN    "B"
                       WHEN    "E"
                       WHEN    "H"
                           MOVE    "下船後３ヶ月以内"    TO  HC06-JIYU
                       WHEN    "3"
                       WHEN    "C"
                       WHEN    "F"
                       WHEN    "I"
                           MOVE    "通勤災害"            TO  HC06-JIYU
                   END-EVALUATE
           END-EVALUATE        
      *
      *    医療機関所在地
           IF      FLG-SYS-1902        =   1
               MOVE    SYS-1902-ADRS1      TO  HC06-HOSPADRS (1:)
               MOVE    SYS-1902-ADRS2      TO  HC06-HOSPADRS (51:)
           ELSE
               MOVE    SYS-1002-ADRS       TO  HC06-HOSPADRS
           END-IF
           INSPECT HC06-HOSPADRS   REPLACING   ALL "  "    BY  "　"
      *
      *    医療機関名称
           IF      FLG-SYS-1901        =   1
               MOVE    SYS-1901-HOSPNAME1  TO  HC06-HOSPNAME (1:)
               MOVE    SYS-1901-HOSPNAME2  TO  HC06-HOSPNAME (41:)
           ELSE 
               MOVE    SYS-1001-HOSPNAME   TO  HC06-HOSPNAME
           END-IF
           INSPECT HC06-HOSPNAME   REPLACING   ALL "  "    BY  "　"
      *
      *    医療機関ＴＥＬ
           IF      FLG-SYS-1902        =   1
               MOVE    SYS-1902-TEL        TO  HC06-HOSPTEL
           ELSE
               MOVE    SYS-1002-TEL        TO  HC06-HOSPTEL
           END-IF
      *
      **    診療科
      *    旧総合病院のときの診療科の記載
           IF      RECE02-RECEKA   NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI 
               MOVE    RECE02-RECEKA       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
                   STRING  "（"            DELIMITED   BY  SIZE
                           KANACHK-OUT-INPUT(1:KANACHK-MAX)    
                                           DELIMITED   BY  SIZE
                           RECE02-SYUKANAME
                                           DELIMITED   BY  SPACE 
                           "）"            DELIMITED   BY  SIZE
                                           INTO  HC06-SRYKA
                   END-STRING
               END-IF
           ELSE
               MOVE    SPACE           TO  HC06-SRYKA
           END-IF
      *    主科設定ありのときの記載
           IF      RECE02-SYUKA     NOT =   SPACE  AND "XX"
               IF  (     ( RECE02-TEISYUTUSAKI   =   "1" )
                     AND ( SYS-2009-SYUKA-SYAHO  =   "1" ) )
               OR  (     ( RECE02-TEISYUTUSAKI   =   "2" )
                     AND ( SYS-2009-SYUKA-KOKUHO =   "1" ) )
               OR  (     ( RECE02-TEISYUTUSAKI   =   "6" )
                     AND ( SYS-2009-SYUKA-KOUIKI =   "1" ) )
                   IF      RECE02-RECESYUKA NOT =   SPACE
                       INITIALIZE                  ORCSKANACHKAREA
                       MOVE    "2"                 TO  KANACHK-SYORI 
                       MOVE    RECE02-RECESYUKA    TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"       USING
                                                       ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           STRING  "（"            DELIMITED   BY  SIZE
                                   KANACHK-OUT-INPUT(1:KANACHK-MAX)    
                                                   DELIMITED   BY  SIZE
                                   RECE02-SYUKANAME
                                                   DELIMITED   BY  SPACE
                                   "）"            DELIMITED   BY  SIZE
                                                   INTO  HC06-SRYKA
                           END-STRING
                       END-IF
                   ELSE
                       STRING  "（"            DELIMITED   BY  SIZE
                               RECE02-SYUKANAME
                                               DELIMITED   BY  SPACE 
                               "）"            DELIMITED   BY  SIZE
                                               INTO  HC06-SRYKA
                       END-STRING
                   END-IF 
               END-IF
           END-IF    
      *     県単用項目
            IF      FLG-RECE11         =   ZERO
                IF      RECE11-SRYKA   NOT =   SPACE
                    MOVE    RECE11-SRYKA   TO  HC06-SRYKA
                END-IF
            END-IF    
      *
      *    病床数
           IF    ( SYS-1001-BEDSU      >   ZERO )
           AND   ( SYS-1001-HOSPSBT    =   "1"  ) 
               IF      RECE02-BEDSUFLG     =   "1" 
                   MOVE    SYS-1001-BEDSU      TO  WRK-BEDSU
                   MOVE    WRK-BEDSU           TO  HC06-BEDSU
               ELSE
                   IF      SYS-1001-BEDSUIPN   >=  200
                       MOVE    SYS-1001-BEDSUIPN   TO  WRK-BEDSU
                       MOVE    WRK-BEDSU           TO  HC06-BEDSU
                   END-IF
               END-IF
           END-IF
      *
      *    医師名（一括作成の点検用レセプトまたはリアルタイムプレビュー時のみ）
           IF    ( SYS-25-DRNAME-KSIKBN
                                       =   "1"         )
           AND   ( RECE02-DRCD     NOT =   SPACE       )
           AND   ( WRK-PARA-PRTKBN     =   "2" OR  "3" 
                                       OR  "4" OR  "5" )
               IF  ( ( WRK-PARA-SHELLID
                                       NOT =   "RECEPTX" ) AND
                     ( WRK-PARA-SYOKBN     =   "1"       )     )
               OR  (   WRK-PARA-SHELLID    =   "RECEPTX"       )
                   IF      CNT-PAGE                =   1
                       MOVE    SPACE               TO  WRK-DRNAME
                       MOVE    SPACE               TO  SYSKANRI-REC
                       INITIALIZE                      SYSKANRI-REC
                       MOVE    "1010"              TO  SYS-KANRICD
                       MOVE    RECE02-DRCD         TO  SYS-KBNCD
                       MOVE    RECE02-SRYYM        TO  SYS-STYUKYMD(1:6)
                       MOVE    "01"                TO  SYS-STYUKYMD(7:2)
                       MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
                       MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
                       MOVE    "key10"             TO  WRK-MCP-PATHNAME
                       PERFORM 900-SYSKANRI-READ-SEC
                       IF      FLG-SYSKANRI    =   ZERO
                           MOVE    MCPDATA-REC         TO  SYS-1010-REC
                           MOVE  SYS-1010-NAME (1:20)  TO  WRK-DRNAME
                       END-IF
                   END-IF  
                   MOVE    WRK-DRNAME          TO  HC06-DRNAME
               END-IF
           END-IF
      *
      *    ＯＣＲ
           IF      CNT-PAGE                =   1
               IF ( ( RECE02-TEISYUTUSAKI  =   1            )  AND
                    ( SYS-2005-SYAOCRKBN   =   ZERO OR  "2" )     )
               OR ( ( RECE02-TEISYUTUSAKI  =   2            )  AND
                    ( SYS-2005-KKHOCRKBN   =   "1"  OR  "3" )     )
               OR ( ( RECE02-TEISYUTUSAKI  =   6            )  AND
                    ( SYS-25-KOUIKIOCRKBN  =   "1"  OR  "3" )     )
      *            レセプト特記事項編集情報に該当項目があるとき
      *            ＯＣＲに反映させる 
                   IF    ( RECE02-2006-FTNJANUMKBN (1)
                                           NOT =   SPACE )
                   OR    ( RECE02-2006-FTNJANUMKBN (2)
                                           NOT =   SPACE )
                   OR    ( RECE02-2006-FTNJANUMKBN (3)
                                           NOT =   SPACE )
                   OR    ( RECE02-2006-FTNJANUMKBN (4)
                                           NOT =   SPACE )
                   OR    ( RECE02-2006-JKYSNUMKBN  (1)
                                           NOT =   SPACE )
                   OR    ( RECE02-2006-JKYSNUMKBN  (2)
                                           NOT =   SPACE )
                   OR    ( RECE02-2006-JKYSNUMKBN  (3)
                                           NOT =   SPACE )
                   OR    ( RECE02-2006-JKYSNUMKBN  (4)
                                           NOT =   SPACE )
                       MOVE    RECE02-OCR1            TO  OCR1-AREA
                       MOVE    RECE02-OCR2            TO  OCR2-AREA
                       MOVE    RECE02-OCR3            TO  OCR3-AREA
                       PERFORM 3302-OCR-HENSYU-SEC
                   END-IF
      *
      *            カスタマイズ項目があるときＯＣＲに反映させる 
                   IF      FLG-RECE11          =   ZERO
                       MOVE    RECE02-OCR1            TO  OCR1-AREA
                       MOVE    RECE02-OCR2            TO  OCR2-AREA
                       MOVE    RECE02-OCR3            TO  OCR3-AREA
                       PERFORM 3301-OCR-HENSYU-SEC
                   END-IF
               END-IF    
      *
               MOVE    RECE02-OCR3     TO  HC06-OCR1
               MOVE    RECE02-OCR2     TO  HC06-OCR2
               MOVE    RECE02-OCR1     TO  HC06-OCR3
           END-IF
      *
      *    県単用項目
      *    但し、レセプト種別不明のときは原因を編集する
      *    主科未設定の場合は記載する
           IF      CNT-PAGE            =   1
               IF    ( RECE02-RECESYUBETU  =    ZERO )
               OR    ( RECE02-ERR-ERRKBN   >    ZERO )
                   PERFORM 3101-FUMEI-HENSYU-SEC
               ELSE 
                   IF      RECE02-MISEIKYU-FLG     =   "01"
                       MOVE    WRK-ERR-MSG-6   TO   HC06-HEAD4-G (5:)
                   ELSE 
                    IF      RECE02-SYUKA         =   "XX"
                        MOVE    WRK-ERR-MSG-5   TO   HC06-HEAD4-G (5:)
                    ELSE 
                     IF      FLG-RECE11          =   ZERO
                       INSPECT RECE11-HC06-HEAD1-G  REPLACING   ALL "  "
                                                        BY  "　" 
                       INSPECT RECE11-HC06-HEAD2-G  REPLACING   ALL "  "
                                                        BY  "　" 
                       INSPECT RECE11-HC06-HEAD3-G  REPLACING   ALL "  "
                                                        BY  "　" 
                       INSPECT RECE11-HEAD4-G       REPLACING   ALL "  "
                                                        BY  "　" 
                       MOVE    RECE11-HC06-HEAD1-G TO  HC06-HEAD1-G
                       MOVE    RECE11-HC06-HEAD2-G TO  HC06-HEAD2-G
                       MOVE    RECE11-HC06-HEAD3-G TO  HC06-HEAD3-G
                       MOVE    RECE11-HEAD4-G      TO  HC06-HEAD4-G
                     END-IF
                    END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    点検用レセプト作成時
           IF      WRK-PARA-PRTKBN     =   "2" OR  "3" OR  "4" OR  "5"
               IF      SYS-25-INSPECT-CHARACTER-KBN
                                           =   "0"
                   MOVE    "点検用レセプト"    TO  HC06-LABEL
                   MOVE    "MonpeLayerIn -L SEN MonpeLayerOut"
                                               TO  HC06-LOPTION
               END-IF
           ELSE
               MOVE    "MonpeLayerIn -L SEN MonpeLayerOut"
                                           TO  HC06-LOPTION
           END-IF     
      *
      *    リアルタイムプレビュー時の表示
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               MOVE    SPACE                TO  WRK-HC06-HEAD4-G
               MOVE    HC06-HEAD4-G (5:)    TO  WRK-HC06-HEAD4-G
               MOVE    WRK-ERR-MSG-9        TO  HC06-HEAD4-G (5:)
               MOVE    WRK-HC06-HEAD4-G     TO  HC06-HEAD4-G (53:)
           END-IF
      *
      *    点検用レセプト作成時０点公費を記載したとき
           IF      RECE02-TENKEN-FLG   =   "1"
               MOVE    SPACE                TO  WRK-HC06-HEAD4-G
               MOVE    HC06-HEAD4-G (5:)    TO  WRK-HC06-HEAD4-G
               MOVE    WRK-ERR-MSG-15       TO  HC06-HEAD4-G (5:)
               MOVE    WRK-HC06-HEAD4-G     TO  HC06-HEAD4-G (25:)
           END-IF
      *
           IF      CNT-PAGE            =   1
               EVALUATE    RECE02-RJNGAIKOHFTN (1)
                   WHEN    1
                   WHEN    8
                   WHEN    10
                   WHEN    11
                   WHEN    2
                   WHEN    7
                   WHEN    4
                   WHEN    5
                       MOVE    SPACE            TO  WRK-HC06-HEAD4-G
                       MOVE    HC06-HEAD4-G (5:)
                                                TO  WRK-HC06-HEAD4-G
                       MOVE    SPACE            TO  HC06-HEAD4-G    
                       MOVE    WRK-HC06-HEAD4-G (1:48)
                                                TO  HC06-HEAD4 (1) (15:)
                       MOVE    WRK-HC06-HEAD4-G (49:48)
                                                TO  HC06-HEAD4 (2) (15:)
                       EVALUATE    RECE02-RJNGAIKOHFTN (1)
                           WHEN    1
                           WHEN    8
                           WHEN    10
                           WHEN    11
                               MOVE    "不詳"  TO  HC06-HEAD5
                           WHEN    2
                           WHEN    7
                               MOVE    "災１"  TO  HC06-HEAD5
                           WHEN    4
                               MOVE    "災２"  TO  HC06-HEAD5
                           WHEN    5
                               MOVE    "不詳・災１"
                                               TO  HC06-HEAD5
                       END-EVALUATE
               END-EVALUATE
           END-IF
      *
           .
       310-HC101-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト種別不明のとき不明の原因を編集処理
      *****************************************************************
       3101-FUMEI-HENSYU-SEC              SECTION.
      *
           EVALUATE    RECE02-ERR-ERRKBN
               WHEN    1
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE02-ERR-HKNCOMBINUM
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-ERR-HKNCOMBINUM-1
                   END-IF
                   MOVE    WRK-ERR-MSG-1   TO   HC06-HEAD4-G (5:)
               WHEN    2
               WHEN    3
                   IF      RECE02-ERR-DAY  >   ZERO
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE02-ERR-DAY  TO  WRK-KGK2
                       MOVE    WRK-KGK2        TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-ERR-DAY-2
                       END-IF
                   END-IF
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE02-ERR-HKNCOMBINUM
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-ERR-HKNCOMBINUM-2
                   END-IF
                   MOVE    RECE02-ERR-TEKSTYMD
                                           TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD2
                                           TO   WRK-ERR-TEKSTYMD-2
                   IF      RECE02-ERR-ERRKBN    =   3
                       MOVE    WRK-ERR-MSG-21  TO   HC06-HEAD4-G (5:)
                   ELSE
                       MOVE    WRK-ERR-MSG-2   TO   HC06-HEAD4-G (5:)
                   END-IF
               WHEN    4
               WHEN    5
                   IF      RECE02-ERR-DAY  >   ZERO
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE02-ERR-DAY  TO  WRK-KGK2
                       MOVE    WRK-KGK2        TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-ERR-DAY-3
                       END-IF
                   END-IF
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE02-ERR-HKNCOMBINUM
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-ERR-HKNCOMBINUM-3
                   END-IF
                   MOVE    RECE02-ERR-TEKEDYMD
                                           TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD2
                                           TO   WRK-ERR-TEKEDYMD-3
                   IF      RECE02-ERR-ERRKBN    =   5
                       MOVE    WRK-ERR-MSG-31  TO   HC06-HEAD4-G (5:)
                   ELSE
                       MOVE    WRK-ERR-MSG-3   TO   HC06-HEAD4-G (5:)
                   END-IF
               WHEN    6
                   MOVE    WRK-ERR-MSG-4   TO   HC06-HEAD4-G (5:)
               WHEN    7
                   MOVE    WRK-ERR-MSG-7   TO   HC06-HEAD4-G (5:)
               WHEN    8
                   MOVE    WRK-ERR-MSG-8   TO   HC06-HEAD4-G (5:)
               WHEN    9
                   MOVE    WRK-ERR-MSG-10  TO   HC06-HEAD4-G (5:)
               WHEN    10
                   MOVE    WRK-ERR-MSG-11  TO   HC06-HEAD4-G (5:)
               WHEN    11
                   MOVE    WRK-ERR-MSG-12  TO   HC06-HEAD4-G (5:)
               WHEN    12
                   MOVE    WRK-ERR-MSG-13  TO   HC06-HEAD4-G (5:)
               WHEN    13
                   MOVE    WRK-ERR-MSG-14  TO   HC06-HEAD4-G (5:)
               WHEN    14
                   MOVE    WRK-ERR-MSG-16  TO   HC06-HEAD4-G (5:)
               WHEN    15
                   MOVE    WRK-ERR-MSG-17  TO   HC06-HEAD4-G (5:)
               WHEN    16
                   MOVE    WRK-ERR-MSG-18  TO   HC06-HEAD4-G (5:)
               WHEN    88
                   MOVE    WRK-ERR-MSG-88  TO   HC06-HEAD4-G (5:)
           END-EVALUATE        
           INSPECT HC06-HEAD4-G  REPLACING ALL "  "    BY  "　"
      *
      *    主科未設定のとき     
           IF      RECE02-SYUKA         =   "XX"
               MOVE    HC06-HEAD4-G (5:)    TO  WRK-HC06-HEAD4-G
               MOVE    WRK-ERR-MSG-5        TO  HC06-HEAD4-G (5:)
               MOVE    WRK-HC06-HEAD4-G     TO  HC06-HEAD4-G (19:)
           END-IF
      *
      *    特別療養費の場合「特別療養費」文字記載を行う
           IF      RECE02-HKNNUM       =   "068"   OR  "069"
               MOVE    "特別療養費"    TO  HC06-MOJI1
           END-IF
      *
           .
       3101-FUMEI-HENSYU-EXT.
           EXIT.
      *     
      *****************************************************************
      *    帳票編集<続紙ヘッダー部>処理
      *****************************************************************
       310-HCN061-HEAD-HEN-SEC          SECTION.
      *
           EVALUATE    SYS-2005-NEXTPRTKBN-G
               WHEN    "1"
                   INITIALIZE                      HC061
                   MOVE    SPACE               TO  HC061
      *
                   MOVE    "（医科入院外）"    TO  HC061-TYTLE
      *
      *            患者番号
                   MOVE    RECE02-PTNUM        TO  HC061-PTNUM
      *
      *            提出先
                   MOVE    HC06-TEISYUTUSAKI   TO  HC061-TEISYUTUSAKI
      *
      *            診療年月
                   MOVE    HC06-SRYGG          TO  HC061-SRYGG
                   MOVE    HC06-SRYY           TO  HC061-SRYY 
                   MOVE    HC06-SRYM           TO  HC061-SRYM 
      *
      *            県番
                   MOVE    SYS-1001-PREFNUM    TO  HC061-PREFNUM
      *
      *            医療機関コード
                   MOVE    SYS-1001-HOSPCDN    TO  HC061-HOSPCDN
      *
      *            保険種別１
                   MOVE    HC06-HKNSBT1        TO  HC061-HKNSBT1  
      *
      *            保険種別２
                   MOVE    HC06-HKNSBT2        TO  HC061-HKNSBT2  
      *
      *            本人・家族
                   MOVE    HC06-HONKZK         TO  HC061-HONKZK
      * 
      *            保険者番号・記号番号
                   MOVE    HC06-HKNJANUM       TO  HC061-HKNJANUM
                   MOVE    HC06-KIGO           TO  HC061-KIGO
                   MOVE    HC06-NUM            TO  HC061-NUM
      *     
      *            公費−負担者番号・受給者番号
                   IF      HC06-KOHJKYSNUM1N   =   SPACE
                       MOVE    HC06-KOHJKYSNUM1
                                               TO  HC061-KOHJKYSNUM1
                   ELSE
                       MOVE    HC06-KOHJKYSNUM1N
                                               TO  HC061-KOHJKYSNUM1N
                   END-IF
                   IF      HC06-KOHJKYSNUM2N   =   SPACE
                       MOVE    HC06-KOHJKYSNUM2
                                               TO  HC061-KOHJKYSNUM2
                   ELSE
                       MOVE    HC06-KOHJKYSNUM2N
                                               TO  HC061-KOHJKYSNUM2N
                   END-IF
                   IF      HC06-KOHFTNJANUM1N  =   SPACE
                       MOVE    HC06-KOHFTNJANUM1
                                               TO  HC061-KOHFTNJANUM1
                   ELSE
                       MOVE    HC06-KOHFTNJANUM1N
                                               TO  HC061-KOHFTNJANUM1N
                   END-IF
                   IF      HC06-KOHFTNJANUM2N  =   SPACE
                       MOVE    HC06-KOHFTNJANUM2
                                               TO  HC061-KOHFTNJANUM2
                   ELSE
                       MOVE    HC06-KOHFTNJANUM2N
                                               TO  HC061-KOHFTNJANUM2N
                   END-IF
      *
      *            給付割合
                   MOVE    HC06-KYURATE        TO  HC061-KYURATE
      *
      *           氏名
                   MOVE    HC06-NAME           TO  HC061-NAME
      *
      *            特別療養費の場合の「特別療養費」文字記載
                   MOVE    HC06-MOJI1          TO  HC061-MOJI1
                   MOVE    HC06-MOJI2          TO  HC061-MOJI2
      *
      *            点検用レセプト作成時
                   MOVE    HC06-LABEL          TO  HC061-LABEL
                   MOVE    HC06-LOPTION        TO  HC061-LOPTION
               WHEN    "2"
                   INITIALIZE                      HC062
                   MOVE    SPACE               TO  HC062
      *
                   MOVE    "（医科入院外）"    TO  HC062-TYTLE
      *
      *            患者番号
                   MOVE    RECE02-PTNUM        TO  HC062-PTNUM
      *
      *            提出先
                   MOVE    HC06-TEISYUTUSAKI   TO  HC062-TEISYUTUSAKI
      *
      *            診療年月
                   MOVE    HC06-SRYGG          TO  HC062-SRYGG
                   MOVE    HC06-SRYY           TO  HC062-SRYY 
                   MOVE    HC06-SRYM           TO  HC062-SRYM 
      *
      *            県番
                   MOVE    SYS-1001-PREFNUM    TO  HC062-PREFNUM
      *
      *            医療機関コード
                   MOVE    SYS-1001-HOSPCDN    TO  HC062-HOSPCDN
      *
      *            保険種別１
                   MOVE    HC06-HKNSBT1        TO  HC062-HKNSBT1  
      *
      *            保険種別２
                   MOVE    HC06-HKNSBT2        TO  HC062-HKNSBT2  
      *
      *            本人・家族
                   MOVE    HC06-HONKZK         TO  HC062-HONKZK
      * 
      *            保険者番号・記号番号
                   MOVE    HC06-HKNJANUM       TO  HC062-HKNJANUM
                   MOVE    HC06-KIGO           TO  HC062-KIGO
                   MOVE    HC06-NUM            TO  HC062-NUM
      *     
      *            公費−負担者番号・受給者番号
                   IF      HC06-KOHJKYSNUM1N   =   SPACE
                       MOVE    HC06-KOHJKYSNUM1
                                               TO  HC062-KOHJKYSNUM1
                   ELSE
                       MOVE    HC06-KOHJKYSNUM1N
                                               TO  HC062-KOHJKYSNUM1N
                   END-IF
                   IF      HC06-KOHJKYSNUM2N   =   SPACE
                       MOVE    HC06-KOHJKYSNUM2
                                               TO  HC062-KOHJKYSNUM2
                   ELSE
                       MOVE    HC06-KOHJKYSNUM2N
                                               TO  HC062-KOHJKYSNUM2N
                   END-IF
                   IF      HC06-KOHFTNJANUM1N  =   SPACE
                       MOVE    HC06-KOHFTNJANUM1
                                               TO  HC062-KOHFTNJANUM1
                   ELSE
                       MOVE    HC06-KOHFTNJANUM1N
                                               TO  HC062-KOHFTNJANUM1N
                   END-IF
                   IF      HC06-KOHFTNJANUM2N  =   SPACE
                       MOVE    HC06-KOHFTNJANUM2
                                               TO  HC062-KOHFTNJANUM2
                   ELSE
                       MOVE    HC06-KOHFTNJANUM2N
                                               TO  HC062-KOHFTNJANUM2N
                   END-IF
      *
      *            給付割合
                   MOVE    HC06-KYURATE        TO  HC062-KYURATE
      *
      *           氏名
                   MOVE    HC06-NAME           TO  HC062-NAME
      *
      *            特別療養費の場合の「特別療養費」文字記載
                   MOVE    HC06-MOJI1          TO  HC062-MOJI1
                   MOVE    HC06-MOJI2          TO  HC062-MOJI2
      *
      *            点検用レセプト作成時
                   MOVE    HC06-LABEL          TO  HC062-LABEL
                   MOVE    HC06-LOPTION        TO  HC062-LOPTION
               WHEN    "3"
                   INITIALIZE                      HC063
                   MOVE    SPACE               TO  HC063
      *
                   MOVE    "（医科入院外）"    TO  HC063-TYTLE
      *
      *            患者番号
                   MOVE    RECE02-PTNUM        TO  HC063-PTNUM
      *
      *            提出先
                   MOVE    HC06-TEISYUTUSAKI   TO  HC063-TEISYUTUSAKI
      *
      *            診療年月
                   MOVE    HC06-SRYGG          TO  HC063-SRYGG
                   MOVE    HC06-SRYY           TO  HC063-SRYY
                   MOVE    HC06-SRYM           TO  HC063-SRYM
      *
      *            県番
                   MOVE    SYS-1001-PREFNUM    TO  HC063-PREFNUM
      *
      *            医療機関コード
                   MOVE    SYS-1001-HOSPCDN    TO  HC063-HOSPCDN
      *
      *            保険種別１
                   MOVE    HC06-HKNSBT1        TO  HC063-HKNSBT1  
      *
      *            保険種別２
                   MOVE    HC06-HKNSBT2        TO  HC063-HKNSBT2  
      *
      *            本人・家族
                   MOVE    HC06-HONKZK         TO  HC063-HONKZK
      * 
      *            保険者番号・記号番号
                   MOVE    HC06-HKNJANUM       TO  HC063-HKNJANUM
                   MOVE    HC06-KIGO           TO  HC063-KIGO
                   MOVE    HC06-NUM            TO  HC063-NUM
      *     
      *            公費−負担者番号・受給者番号
                   IF      HC06-KOHJKYSNUM1N   =   SPACE
                       MOVE    HC06-KOHJKYSNUM1
                                               TO  HC063-KOHJKYSNUM1
                   ELSE
                       MOVE    HC06-KOHJKYSNUM1N
                                               TO  HC063-KOHJKYSNUM1N
                   END-IF
                   IF      HC06-KOHJKYSNUM2N   =   SPACE
                       MOVE    HC06-KOHJKYSNUM2
                                               TO  HC063-KOHJKYSNUM2
                   ELSE
                       MOVE    HC06-KOHJKYSNUM2N
                                               TO  HC063-KOHJKYSNUM2N
                   END-IF
                   IF      HC06-KOHFTNJANUM1N  =   SPACE
                       MOVE    HC06-KOHFTNJANUM1
                                               TO  HC063-KOHFTNJANUM1
                   ELSE
                       MOVE    HC06-KOHFTNJANUM1N
                                               TO  HC063-KOHFTNJANUM1N
                   END-IF
                   IF      HC06-KOHFTNJANUM2N  =   SPACE
                       MOVE    HC06-KOHFTNJANUM2
                                               TO  HC063-KOHFTNJANUM2
                   ELSE
                       MOVE    HC06-KOHFTNJANUM2N
                                               TO  HC063-KOHFTNJANUM2N
                   END-IF
      *
      *            給付割合
                   MOVE    HC06-KYURATE        TO  HC063-KYURATE
      *
      *           氏名
                   MOVE    HC06-NAME           TO  HC063-NAME
      *
      *            特別療養費の場合の「特別療養費」文字記載
                   MOVE    HC06-MOJI1          TO  HC063-MOJI1
                   MOVE    HC06-MOJI2          TO  HC063-MOJI2
      *
      *            点検用レセプト作成時
                   MOVE    HC06-LABEL          TO  HC063-LABEL
                   MOVE    HC06-LOPTION        TO  HC063-LOPTION
           END-EVALUATE
           .
       310-HCN061-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  <保険種別１>編集処理
      *****************************************************************
       3110-HKNSBT1-SET-SEC              SECTION.
      *
      *    不明分・自費は編集しない
           IF    ( RECE02-RECESYUBETU  =    ZERO    OR  8888 )
           OR    ( RECE02-ERR-ERRKBN   >    ZERO             )
               CONTINUE
           ELSE                                        
               IF      RECE02-RECESYUBETU  =   6666
                   EVALUATE    RECE02-RECESYUBETU-X (2:1)
                       WHEN    "1"
                           MOVE    "１国"          TO  HC06-HKNSBT1
                       WHEN    "2"
                           MOVE    "２公費"        TO  HC06-HKNSBT1
                       WHEN    "3"
                           MOVE    "３後期"        TO  HC06-HKNSBT1
                       WHEN    "4"
                           MOVE    "４退職"        TO  HC06-HKNSBT1
                   END-EVALUATE
               ELSE
                   EVALUATE    RECE02-RECESYUBETU-Y (2:1)
                       WHEN    "1"
                           EVALUATE    RECE02-TEISYUTUSAKI
                               WHEN    1
                                   MOVE    "１社"      TO  HC06-HKNSBT1
                               WHEN    2
                                   MOVE    "１国"      TO  HC06-HKNSBT1
                           END-EVALUATE
                       WHEN    "2"
                           MOVE    "２公費"        TO  HC06-HKNSBT1
                       WHEN    "3"
                           MOVE    "３後期"        TO  HC06-HKNSBT1
                       WHEN    "4"
                           MOVE    "４退職"        TO  HC06-HKNSBT1
                   END-EVALUATE
               END-IF
           END-IF    
      *
           .
       310-HKNSBT1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  <保険種別２>編集処理
      *****************************************************************
       3110-HKNSBT2-SET-SEC              SECTION.
      *
      *    不明分・自費は編集しない
           IF    ( RECE02-RECESYUBETU  =    ZERO    OR  8888 )
           OR    ( RECE02-ERR-ERRKBN   >    ZERO             )
               CONTINUE
           ELSE                                        
               IF      RECE02-RECESYUBETU  =   6666
                   EVALUATE    RECE02-RECESYUBETU-X (3:1)
                       WHEN    "1"
                           MOVE    "１単独"        TO  HC06-HKNSBT2
                       WHEN    "2"
                           MOVE    "２２併"        TO  HC06-HKNSBT2
                       WHEN    "3"
                       WHEN    "4"
                       WHEN    "5"
                           MOVE    "３３併"        TO  HC06-HKNSBT2
                   END-EVALUATE
               ELSE
                   EVALUATE    RECE02-RECESYUBETU-Y (3:1)
                       WHEN    "1"
                           MOVE    "１単独"        TO  HC06-HKNSBT2
                       WHEN    "2"
                           MOVE    "２２併"        TO  HC06-HKNSBT2
                       WHEN    "3"
                           MOVE    "３３併"        TO  HC06-HKNSBT2
                   END-EVALUATE
               END-IF
           END-IF    
      *
           .
       310-HKNSBT2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  <本人・家族>編集処理
      *****************************************************************
       3110-HONKZK-SET-SEC             SECTION.
      *
      *    不明分・自費は編集しない
           IF    ( RECE02-RECESYUBETU  =    ZERO    OR  8888 )
           OR    ( RECE02-ERR-ERRKBN   >    ZERO             )
               CONTINUE
           ELSE                                        
               IF      RECE02-RECESYUBETU  =   6666
                   EVALUATE    RECE02-RECESYUBETU-X (4:1)
                       WHEN    "0"
                           MOVE    "０高外７"     TO  HC06-HONKZK
                       WHEN    "2"
                           MOVE    "２本外"       TO  HC06-HONKZK
                       WHEN    "4"
                           MOVE    "４六外"       TO  HC06-HONKZK
                       WHEN    "6"
                           MOVE    "６家外"       TO  HC06-HONKZK
                       WHEN    "8"
                           MOVE    "８高外一"     TO  HC06-HONKZK
                   END-EVALUATE
               ELSE
                   EVALUATE    RECE02-RECESYUBETU-Y (4:1)
                       WHEN    "0"
                           MOVE    "０高外７"     TO  HC06-HONKZK
                       WHEN    "2"
                           MOVE    "２本外"       TO  HC06-HONKZK
                       WHEN    "4"
                           MOVE    "４六外"       TO  HC06-HONKZK
                       WHEN    "6"
                           MOVE    "６家外"       TO  HC06-HONKZK
                       WHEN    "8"
                           MOVE    "８高外一"     TO  HC06-HONKZK
                   END-EVALUATE
               END-IF
           END-IF    
      *
           .
       310-HONKZK-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  主保険−保険者番号８桁
      *          公費−負担者番号  ８桁
      *          記号・番号　診療実日数     編集処理
      *****************************************************************
       3110-KANREN-NUM-SET-SEC        SECTION.
      *
      *    <保険者番号>
           MOVE    SPACE              TO  WRK-HKNJANUM
      *     
           IF      WRK-3SYU-KBN           =   ALL "1" AND 
                   RECE02-KOHNUM (4)      =   SPACE   AND  
                   RECE02-HKNNUM          =   SPACE            
      *        ３種の場合（８桁）
               IF    ( RECE02-RJNGAIKOHFTN (1)      =   6   OR  7 )
               AND   ( RECE02-KOHFTNJANUM(SEIHIDX)  =   SPACE     )
                   MOVE    RECE02-KOHHBTNUM(SEIHIDX)    TO  WRK-HKNJANUM
               ELSE
                   MOVE    RECE02-KOHFTNJANUM(SEIHIDX)  TO  WRK-HKNJANUM
               END-IF
           ELSE
               IF  RECE02-TEISYUTUSAKI    =   "2"
                   IF      RECE02-HKNJANUM (1:2)  =   "67"
                       MOVE    RECE02-HKNJANUM  TO  WRK-HKNJANUM
                   ELSE
      *            国保の場合（６桁右詰に編集）
                       MOVE    SPACE              TO  WRK-HKNJANUM(1:2)
                       MOVE    RECE02-HKNJANUM    TO  WRK-HKNJANUM(3:)
                   END-IF
               ELSE
                   IF  RECE02-HKNNUM      =   "001"
      *                政府管掌の場合（４桁右詰に編集）
                       MOVE    SPACE            TO  WRK-HKNJANUM(1:4)
                       MOVE    RECE02-HKNJANUM  TO  WRK-HKNJANUM(5:)
                   ELSE
      *                他の保険の場合（８桁）
                       MOVE    RECE02-HKNJANUM  TO  WRK-HKNJANUM
                   END-IF
               END-IF
           END-IF
           MOVE   WRK-HKNJANUM    TO    HC06-HKNJANUM
      *
      *    <記号・番号>
           IF      WRK-3SYU-KBN           =   ALL "1" AND 
                   RECE02-KOHNUM (4)      =   SPACE   AND  
                   RECE02-HKNNUM          =   SPACE            
      *        ３種の場合（８桁）
               IF      RECE02-KOHJKYSNUM(SEIHIDX)
                                       NOT =   SPACE
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE02-KOHJKYSNUM(SEIHIDX)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  HC06-KIGO
                   END-IF
               END-IF    
           ELSE
               IF      RECE02-KIGO     NOT =   SPACE 
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE02-KIGO       TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  HC06-KIGO
                   END-IF
               END-IF
               IF      RECE02-NUM      NOT =   SPACE 
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE02-NUM      TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  HC06-NUM
                   END-IF
               END-IF    
           END-IF
      *
      *    <公費負担者番号・公費受給者番号>
      *    
           MOVE    ZERO            TO  IDC
           MOVE    ZERO            TO  IDD
                                       IDX4
                                       IDX5
           IF      WRK-3SYU-KBN           =   ALL "1" AND
                   RECE02-KOHNUM (4)      =   SPACE   AND  
                   RECE02-HKNNUM          =   SPACE            
      *        ３種の場合
               IF  KAKKIDX         >   SESNIDX
                   MOVE    SESNIDX     TO  IDA
                   MOVE    KAKKIDX     TO  IDB
               ELSE
                   MOVE    KAKKIDX     TO  IDA
                   MOVE    SESNIDX     TO  IDB
               END-IF
               COMPUTE IDX1     =   SEIHIDX    +   1
               COMPUTE IDX2     =   IDA        +   1
               COMPUTE IDX3     =   IDB        +   1
           ELSE    
      *        ３種以外の場合
               MOVE    1               TO  IDA
               MOVE    2               TO  IDB
               MOVE    1               TO  IDX1
               MOVE    2               TO  IDX2
               MOVE    3               TO  IDX3
               IF      RECE02-KOHNUM (3)   NOT =   SPACE
                   MOVE    3               TO  IDC
                   MOVE    4               TO  IDX4
               END-IF
               IF      RECE02-KOHNUM (4)   NOT =   SPACE
                   MOVE    4               TO  IDD
                   MOVE    5               TO  IDX5
               END-IF
           END-IF
      *    公負１
           IF      RECE02-KOHFTNJANUM(IDA)
                                   NOT =   SPACE 
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI 
               MOVE    RECE02-KOHFTNJANUM(IDA)
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   IF      KANACHK-SYUBETU =   1
                       MOVE    RECE02-KOHFTNJANUM(IDA)
                                       TO  HC06-KOHFTNJANUM1
                   ELSE
                       MOVE    RECE02-KOHFTNJANUM(IDA)
                                       TO  HC06-KOHFTNJANUM1N
                   END-IF
               END-IF                         
           END-IF    
      *    公負２
           IF      RECE02-KOHFTNJANUM(IDB)
                                   NOT =   SPACE
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI 
               MOVE    RECE02-KOHFTNJANUM(IDB)
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   IF      KANACHK-SYUBETU =   1
                       MOVE    RECE02-KOHFTNJANUM(IDB)
                                       TO  HC06-KOHFTNJANUM2
                   ELSE
                       MOVE    RECE02-KOHFTNJANUM(IDB)
                                       TO  HC06-KOHFTNJANUM2N
                   END-IF
               END-IF                         
           END-IF    
      *    公受１
           IF      RECE02-KOHJKYSNUM(IDA)
                                   NOT =   SPACE
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI 
               MOVE    RECE02-KOHJKYSNUM(IDA)
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   IF      KANACHK-SYUBETU =   1
                       MOVE    RECE02-KOHJKYSNUM(IDA)
                                       TO  HC06-KOHJKYSNUM1
                   ELSE
                       MOVE    RECE02-KOHJKYSNUM(IDA)
                                       TO  HC06-KOHJKYSNUM1N
                   END-IF
               END-IF                        
           END-IF    
      *    公受２
           IF      RECE02-KOHJKYSNUM(IDB)
                                   NOT =   SPACE
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI 
               MOVE    RECE02-KOHJKYSNUM(IDB)
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   IF      KANACHK-SYUBETU =   1
                       MOVE    RECE02-KOHJKYSNUM(IDB)
                                       TO  HC06-KOHJKYSNUM2
                   ELSE
                       MOVE    RECE02-KOHJKYSNUM(IDB)
                                       TO  HC06-KOHJKYSNUM2N
                   END-IF
               END-IF                    
           END-IF    
      *
      *    レセプト特記事項編集情報
      *    公負１
           EVALUATE    RECE02-2006-FTNJANUMKBN  (IDA)
               WHEN    SPACE
                   CONTINUE
               WHEN    "01"
                   MOVE    SPACE           TO  HC06-KOHFTNJANUM1
                                               HC06-KOHFTNJANUM1N
               WHEN    "02"
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    RECE02-2006-FTNJANUM (IDA)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-SYUBETU =   "1"
                       MOVE    RECE02-2006-FTNJANUM (IDA)
                                               TO  HC06-KOHFTNJANUM1 
                       MOVE    SPACE           TO  HC06-KOHFTNJANUM1N
                   END-IF
               WHEN    "03"
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    RECE02-KOHFTNJANUM (IDA)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-SYUBETU =   "1"
                       INITIALIZE                   ORCSNUMAREA
                       MOVE    RECE02-KOHFTNJANUM (IDA) (1:KANACHK-MAX)
                                                TO  SNUM-INX
                       CALL    "ORCSNUM"        USING   ORCSNUMAREA
                       IF      SNUM-RC          =   ZERO
                           MOVE   SNUM-OUTEDT (8:8)
                                               TO  HC06-KOHFTNJANUM1 
                           MOVE    SPACE       TO  HC06-KOHFTNJANUM1N
                       END-IF
                   END-IF
           END-EVALUATE
      *    公受１
           EVALUATE    RECE02-2006-JKYSNUMKBN  (IDA)
               WHEN    SPACE
                   CONTINUE
               WHEN    "01"
                   MOVE    SPACE           TO  HC06-KOHJKYSNUM1
                                               HC06-KOHJKYSNUM1N
               WHEN    OTHER
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    RECE02-2006-JKYSNUM (IDA)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-SYUBETU =   "1"
                       MOVE    RECE02-2006-JKYSNUM (IDA)
                                               TO  HC06-KOHJKYSNUM1
                       MOVE    SPACE           TO  HC06-KOHJKYSNUM1N
                   END-IF
           END-EVALUATE
      *    公負２
           EVALUATE    RECE02-2006-FTNJANUMKBN  (IDB)
               WHEN    SPACE
                   CONTINUE
               WHEN    "01"
                   MOVE    SPACE           TO  HC06-KOHFTNJANUM2
                                               HC06-KOHFTNJANUM2N
               WHEN    "02"
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    RECE02-2006-FTNJANUM (IDB)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-SYUBETU =   "1"
                       MOVE    RECE02-2006-FTNJANUM (IDB)
                                               TO  HC06-KOHFTNJANUM2
                       MOVE    SPACE           TO  HC06-KOHFTNJANUM2N
                   END-IF
               WHEN    "03"
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    RECE02-KOHFTNJANUM (IDB)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-SYUBETU =   "1"
                       INITIALIZE                   ORCSNUMAREA
                       MOVE    RECE02-KOHFTNJANUM (IDB) (1:KANACHK-MAX)
                                                TO  SNUM-INX
                       CALL    "ORCSNUM"        USING   ORCSNUMAREA
                       IF      SNUM-RC          =   ZERO
                           MOVE   SNUM-OUTEDT (8:8)
                                               TO  HC06-KOHFTNJANUM2
                           MOVE    SPACE       TO  HC06-KOHFTNJANUM2N
                       END-IF
                   END-IF
           END-EVALUATE
      *    公受２
           EVALUATE    RECE02-2006-JKYSNUMKBN  (IDB)
               WHEN    SPACE
                   CONTINUE
               WHEN    "01"
                   MOVE    SPACE           TO  HC06-KOHJKYSNUM2
                                               HC06-KOHJKYSNUM2N
               WHEN    OTHER
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    RECE02-2006-JKYSNUM (IDB)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-SYUBETU =   "1"
                       MOVE    RECE02-2006-JKYSNUM (IDB)
                                               TO  HC06-KOHJKYSNUM2
                       MOVE    SPACE           TO  HC06-KOHJKYSNUM2N
                   END-IF
           END-EVALUATE
      *        県単用項目
           IF      FLG-RECE11              =   ZERO
               IF      RECE11-KOHINF (IDA) NOT =   SPACE
                   MOVE    SPACE               TO   HC06-KOH-TBL1
      *            公負１
                   IF      RECE11-KOHFTNJANUM(IDA)
                                       NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "1"             TO  KANACHK-SYORI 
                       MOVE    RECE11-KOHFTNJANUM(IDA)
                                               TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           IF      KANACHK-SYUBETU =   1
                               MOVE    RECE11-KOHFTNJANUM(IDA)
                                               TO  HC06-KOHFTNJANUM1
                           ELSE
                               MOVE    RECE11-KOHFTNJANUM(IDA)
                                               TO  HC06-KOHFTNJANUM1N
                           END-IF
                       END-IF
                   END-IF
      *            公受１
                   IF      RECE11-KOHJKYSNUM(IDA)
                                       NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "1"             TO  KANACHK-SYORI 
                       MOVE    RECE11-KOHJKYSNUM(IDA)
                                               TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           IF      KANACHK-SYUBETU =   1
                              MOVE    RECE11-KOHJKYSNUM(IDA)
                                               TO  HC06-KOHJKYSNUM1
                           ELSE
                               MOVE    RECE11-KOHJKYSNUM(IDA)
                                               TO  HC06-KOHJKYSNUM1N
                           END-IF
                       END-IF                        
                   END-IF
               END-IF 
      *
               IF      RECE11-KOHINF (IDB) NOT =   SPACE
                   MOVE    SPACE               TO   HC06-KOH-TBL2
      *            公負２
                   IF      RECE11-KOHFTNJANUM(IDB)
                                       NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "1"             TO  KANACHK-SYORI 
                       MOVE    RECE11-KOHFTNJANUM(IDB)
                                               TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           IF      KANACHK-SYUBETU =   1
                               MOVE    RECE11-KOHFTNJANUM(IDB)
                                               TO  HC06-KOHFTNJANUM2
                           ELSE
                               MOVE    RECE11-KOHFTNJANUM(IDB)
                                               TO  HC06-KOHFTNJANUM2N
                           END-IF
                       END-IF                        
                   END-IF
      *            公受２
                   IF      RECE11-KOHJKYSNUM(IDB)
                                           NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "1"             TO  KANACHK-SYORI 
                       MOVE    RECE11-KOHJKYSNUM(IDB)
                                               TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           IF      KANACHK-SYUBETU =   1
                              MOVE    RECE11-KOHJKYSNUM(IDB)
                                               TO  HC06-KOHJKYSNUM2
                           ELSE
                               MOVE    RECE11-KOHJKYSNUM(IDB)
                                               TO  HC06-KOHJKYSNUM2N
                           END-IF
                       END-IF                        
                   END-IF
               END-IF                        
           END-IF    
      *    
      *    災害対応
      *
           IF      RECE02-RJNGAIKOHFTN (1) =   6   OR  7   OR  9   OR
                                               11
      *        公負１
               IF    ( RECE02-KOHNUM     (IDA) NOT =   SPACE )
               AND   ( RECE02-KOHFTNJANUM(IDA)     =   SPACE )
                   MOVE    RECE02-KOHHBTNUM(IDA)   TO HC06-KOHFTNJANUM1 
                   MOVE    SPACE                   TO HC06-KOHFTNJANUM1N
               END-IF
      *        公負２
               IF    ( RECE02-KOHNUM     (IDB) NOT =   SPACE )
               AND   ( RECE02-KOHFTNJANUM(IDB)     =   SPACE )
                   MOVE    RECE02-KOHHBTNUM(IDB)   TO HC06-KOHFTNJANUM2
                   MOVE    SPACE                   TO HC06-KOHFTNJANUM2N
               END-IF
           END-IF
      *
      *    <診療実日数>
           IF      CNT-PAGE        =   1
               MOVE      ZERO             TO  FLG-JNISSU
               IF    ((IDX1                         =   1   ) AND
                      (RECE02-HKNNUM            NOT =  SPACE))
               OR    ((IDX1                     NOT =   1   ) AND
                      (RECE02-KOHNUM(IDX1 - 1)  NOT =  SPACE))
                  MOVE    RECE02-JNISSU (IDX1)
                                          TO  WRK-JINISSU2Z
                  MOVE    WRK-JINISSU2    TO  HC06-JINISSU
                  MOVE    1               TO  FLG-JNISSU
               END-IF
               IF      RECE02-KOHNUM(IDX2 - 1)  NOT =  SPACE
                  IF (FLG-JNISSU                 = ZERO               )
                  OR (RECE02-JNISSU(IDX1)    NOT = RECE02-JNISSU(IDX2))
                  OR (RECE02-JSKISAIFLG          = "1"                )
                  OR (RECE02-EKF-JNISSUFLG(IDX2) = "1"                )
                      MOVE    RECE02-JNISSU (IDX2)
                                              TO  WRK-JINISSU2Z
                      MOVE    WRK-JINISSU2    TO  HC06-JINISSU1
                  END-IF
               END-IF
               IF      RECE02-KOHNUM(IDX3 - 1)  NOT =  SPACE
                  IF (RECE02-JNISSU(IDX2)    NOT = RECE02-JNISSU(IDX3))
                  OR (RECE02-JSKISAIFLG          = "1"                )
                  OR (RECE02-EKF-JNISSUFLG(IDX3) = "1"                )
                      MOVE    RECE02-JNISSU (IDX3)
                                              TO  WRK-JINISSU2Z
                      MOVE    WRK-JINISSU2    TO  HC06-JINISSU2
                  END-IF
               END-IF
      *        県単用項目
               IF      FLG-RECE11           =   ZERO
                   IF      RECE11-JNISSU (IDX1) >   ZERO
                       IF      RECE11-JNISSU (IDX1) =   99
                           MOVE    ZERO            TO  WRK-KGK2
                       ELSE  
                           MOVE    RECE11-JNISSU (IDX1)
                                                   TO  WRK-KGK2
                       END-IF
                       MOVE    WRK-KGK2        TO  HC06-JINISSU
                   END-IF
                   IF      RECE11-JNISSU (IDX2) >   ZERO
                       IF      RECE11-JNISSU (IDX2) =   99
                           MOVE    ZERO            TO  WRK-KGK2
                       ELSE  
                           MOVE    RECE11-JNISSU (IDX2)
                                                   TO  WRK-KGK2
                       END-IF
                       MOVE    WRK-KGK2        TO  HC06-JINISSU1
                   END-IF
                   IF      RECE11-JNISSU (IDX3) >   ZERO
                       IF      RECE11-JNISSU (IDX3) =   99
                           MOVE    ZERO            TO  WRK-KGK2
                       ELSE  
                           MOVE    RECE11-JNISSU (IDX3)
                                                   TO  WRK-KGK2
                       END-IF
                       MOVE    WRK-KGK2        TO  HC06-JINISSU2
                   END-IF
               END-IF
           END-IF
      *
           .
       3110-KANREN-NUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  <給付割合>　編集処理
      *****************************************************************
       3110-KYURATE-SET-SEC        SECTION.
      *
           MOVE   SPACE              TO  HC06-KYURATE
      *          
      *    国保のみ印字
           IF    ( RECE02-TEISYUTUSAKI =   "2"   )
      *        ７０歳代前半の特例措置対象者のみ２割の記載
               IF      RECE02-RJNGAIKOHFTN (2)     =   1
                   MOVE    8               TO  WRK-KYURATE-9
               ELSE
                   COMPUTE WRK-KYURATE-9   =   (100 -   RECE02-KYURATE)
                                               / 10 
               END-IF
               IF      WRK-KYURATE-9       >   ZERO
                   MOVE   "給割"               TO  HC06-KYURATE
                   MOVE    WRK-KYURATE-9       TO  WRK-KYURATE
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    WRK-KYURATE     TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF    ( KANACHK-RC      =   ZERO )
                   AND   ( KANACHK-MAX     >   ZERO )
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  HC06-KYURATE(5:)
                   END-IF    
               END-IF
           END-IF
      *
      *    ★
      *    神奈川県国保老健は給付割合を記載（暫定的な対応）
           IF    ( RECE02-TEISYUTUSAKI        =   "2"    )
           AND   ( RECE02-PREFNUM             =   "14"   )
           AND   ( RECE02-RJNHKNNUM       NOT =   SPACE  )
           AND  (( RECE02-HKNJANUM-KEY (1:2)  =   "14"   )
           OR    ( RECE02-HKNJANUM-KEY (1:4)  =   "6714" ))
               COMPUTE WRK-KYURATE-9   =   (100 -   RECE02-KYURATE)
                                           / 10 
               IF      WRK-KYURATE-9       >   ZERO
                   MOVE   "給割"               TO  HC06-KYURATE
                   MOVE    WRK-KYURATE-9       TO  WRK-KYURATE
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    WRK-KYURATE     TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF    ( KANACHK-RC      =   ZERO )
                   AND   ( KANACHK-MAX     >   ZERO )
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  HC06-KYURATE(5:)
                   END-IF    
               END-IF
           END-IF
      *
      *    特別療養費のときシス管の設定より編集
           IF      RECE02-RECESYUBETU  =   6666
               EVALUATE    SYS-2005-KYURATE-KISAIKBN
                   WHEN    ZERO
                       MOVE   "給割１０"           TO  HC06-KYURATE
                   WHEN    "1" 
                       MOVE   "給割０"             TO  HC06-KYURATE
                   WHEN    "2" 
                       MOVE   SPACE                TO  HC06-KYURATE
               END-EVALUATE
           END-IF
      *    
      *    レセプト特記事項編集情報
           IF      RECE02-2006-KYURATE >   ZERO
               COMPUTE WRK-KYURATE-9   =
                                       RECE02-2006-KYURATE /   10 
               MOVE   "給割"           TO  HC06-KYURATE
               MOVE    WRK-KYURATE-9   TO  WRK-KYURATE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI
               MOVE    WRK-KYURATE     TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING
                                           ORCSKANACHKAREA
               IF    ( KANACHK-RC      =   ZERO )
               AND   ( KANACHK-MAX     >   ZERO )
                   MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  HC06-KYURATE(5:)
               END-IF
           END-IF
      *
      *    県単用項目
           IF      FLG-RECE11          =   ZERO
           AND     RECE11-KYURATE      >   ZERO
               IF      RECE11-KYURATE      =   999
                   MOVE    10                  TO  WRK-KYURATE-9
               ELSE
                   COMPUTE WRK-KYURATE-9   =   (100 -   RECE11-KYURATE)
                                               / 10
               END-IF
               IF      WRK-KYURATE-9       >   ZERO
                   MOVE   "給割"           TO  HC06-KYURATE
                   MOVE    WRK-KYURATE-9   TO  WRK-KYURATE
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    WRK-KYURATE     TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF    ( KANACHK-RC      =   ZERO )
                   AND   ( KANACHK-MAX     >   ZERO )
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  HC06-KYURATE(5:)
                   END-IF    
               END-IF
           END-IF    
           .
       3110-KYURATE-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  <特記事項>編集処理
      *****************************************************************
       3110-TOKKI-SET-SEC          SECTION.
      *
           MOVE    ZERO                TO  IDZ
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( IDX                 >   5     )
                   OR    ( RECE02-TOKKI(IDX)   =   SPACE )
                   OR      HC06-TOKKI (5)  NOT =   SPACE
      *
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL ( IDY             >   TBL-TOKKI-MAX  )
                   IF      RECE02-TOKKI(IDX)   =   TBL-TOKKICD(IDY)
                       ADD     1                TO  IDZ
                       MOVE    TBL-TOKKINAME(IDY)
                                                TO  HC06-TOKKI (IDZ)
                       MOVE    TBL-TOKKI-MAX    TO  IDY
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    患者特記事項編集情報
           IF      RECE02-PTTOKKI-AREA     NOT =   SPACE
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX                 >   5
                       OR      HC06-TOKKI (5)  NOT =   SPACE
                       OR      RECE02-PTTOKKI-NAIYO(IDX)   =   SPACE
                   PERFORM VARYING IDWW    FROM    1   BY  1
                           UNTIL   IDWW    >       5
                       IF      HC06-TOKKI (IDWW)   =   SPACE
                           MOVE    RECE02-PTTOKKI-NAIYO(IDX)        
                                                TO  HC06-TOKKI (IDWW)
                       END-IF
                       IF      RECE02-PTTOKKI-NAIYO(IDX)
                                                =   HC06-TOKKI (IDWW)
                           MOVE    5            TO  IDWW
                       END-IF    
                   END-PERFORM
               END-PERFORM
           END-IF
      *
      *    レセプト特記事項編集情報
           IF      RECE02-2006-TOKKIINF    NOT =   SPACE
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX                 >   4
                       OR      HC06-TOKKI (5)  NOT =   SPACE
                       OR      RECE02-2006-TOKKI (IDX) =   SPACE
                   PERFORM VARYING IDWW    FROM    1   BY  1
                           UNTIL   IDWW    >       5
                       IF      HC06-TOKKI (IDWW)   =   SPACE
                           MOVE    RECE02-2006-TOKKI (IDX)           
                                                TO  HC06-TOKKI (IDWW)
                       END-IF
                       IF      RECE02-2006-TOKKI (IDX)
                                                =   HC06-TOKKI (IDWW)
                           MOVE    5            TO  IDWW
                       END-IF    
                   END-PERFORM
               END-PERFORM
           END-IF
      *
      *    県単用項目
           IF      FLG-RECE11           =   ZERO
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX                 >   5
                       OR      HC06-TOKKI (5)  NOT =   SPACE
                       OR      RECE11-HC06-TOKKI(IDX)  =   SPACE
                   PERFORM VARYING IDWW    FROM    1   BY  1
                           UNTIL   IDWW    >       5
                       IF      HC06-TOKKI (IDWW)   =   SPACE
                           MOVE    RECE11-HC06-TOKKI (IDX)
                                                TO  HC06-TOKKI (IDWW)
                       END-IF
                       IF      RECE11-HC06-TOKKI (IDX)
                                                =   HC06-TOKKI (IDWW)
                           MOVE    5            TO  IDWW
                       END-IF    
                   END-PERFORM
               END-PERFORM
           END-IF
      *
           .
       3110-TOKKI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<傷病名>処理
      *****************************************************************
       320-HC101-BYOMEI-HEN-SEC      SECTION.
      *
           MOVE    LOW-VALUE           TO  WRK-BYO-OLD
      *     
           INITIALIZE                  WRK-EDIT-AREA
           MOVE    SPACE               TO  WRK-BYOMEI1
                                           WRK-BYOMEI2
                                           WRK-BYOMEI3
           MOVE    ZERO                TO  FLG-TENKI
                                           WRK-MOJISU3                                
      *     
           INITIALIZE                      RECE031-REC
           MOVE    RECE02-KEY          TO  RECE031-KEY1
      *
           START   RECE031-FILE
               KEY IS    >=   RECE031-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE031
               NOT INVALID
                   PERFORM 990-RECE031-NEXT-SEC
           END-START
      *
           PERFORM
               UNTIL  ( FLG-RECE031          =   1                    )
               OR     ( RECE02-KEY       NOT =   RECE031-KEY1         )
      *    
      *        病名編集処理
               PERFORM 3210-BYOMEI-SET-SEC
      *
           END-PERFORM
      *
           MOVE    1               TO  FLG-RECE031     
      *
           IF  WRK-LINEBYO      <  WRK-BYOMEI-LINE
               IF  WRK-BYOMEI1-NAME  NOT =   SPACE
                   PERFORM 3221-BYOMEI1-OUT-SEC
               END-IF
           ELSE
               IF  WRK-BYOMEI2-NAME  NOT =   SPACE
                   MOVE    1             TO  FLG-TENKI
                   PERFORM 3222-BYOMEI2-OUT-SEC
                   MOVE    ZERO          TO  FLG-TENKI
               END-IF
           END-IF
      *
      *    主病名との区切り線編集処理     
           IF    ( SYS-2005-SYUBYOHENFLG   =   "3" )
           AND   ( WRK-BO-SYUBYOFLG        =   "1" )
               IF      WRK-LINEBYO     <   WRK-BYOMEI-LINE
                   ADD     1           TO  WRK-LINEBYO
                   MOVE    WRK-KUGIRI3 TO HC06-BYOMEI(WRK-LINEBYO)(2:52)
               ELSE
                   IF      WRK-LINETEK >=   WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   END-IF 
                   MOVE    1           TO  FLG-KUGIRI
                   PERFORM 34110-KUGIRI-LINE-SEC
                   MOVE    ZERO        TO  FLG-KUGIRI   
               END-IF
           END-IF
           .
       320-HC101-BYOMEI-HEN-EXT.
           EXIT.
      ****************************************************************
      *     病名編集処理
      ****************************************************************
       3210-BYOMEI-SET-SEC           SECTION.
      * 
      *    連結表示のとき
           IF      SYS-2005-BYOMEIHENKBN    =   "0"
      *        前のデータと同一主病名フラグ・診療開始日・転帰区分
               IF  ( RECE031-SYUBYOFLG    =   WRK-BO-SYUBYOFLG )  AND
                   ( RECE031-SRYYMD       =   WRK-BO-SRYYMD    )  AND
                   ( RECE031-TENKIKBN     =   WRK-BO-TENKIKBN  )  AND 
                   ( RECE031-TENKIYMD     =   WRK-BO-TENKIYMD  )   
      *
      *            病名をつなげると何桁になるかを計算（，＋病名）
                   IF  WRK-CLM           =   ZERO
                       MOVE    WRK-CLM   TO  WRK-WCLM
                   ELSE
                       COMPUTE WRK-WCLM  =   WRK-CLM   +   2
                   END-IF
                   COMPUTE     WRK-WCLM  =
                              ( RECE031-BYOMEIMOJI  *   2 )
                                                    +   WRK-WCLM
      *
                   EVALUATE    TRUE
      *                傷病名欄に編集
                       WHEN    WRK-LINEBYO     <   WRK-BYOMEI-LINE
                           IF  WRK-WCLM          >   WRK-BYOMEI1-MAX
                               PERFORM 3221-BYOMEI1-OUT-SEC
                           ELSE
                               IF  WRK-CLM       >   ZERO
      *                            前の病名の直後に編集
                                   COMPUTE WRK-STCLM =   WRK-CLM   +
                                                         1
                                   MOVE    "，"  TO  WRK-BYOMEI1-NAME
                                                           (WRK-STCLM:)
                                   COMPUTE WRK-CLM   =   WRK-CLM   +
                                                         2
                               END-IF
                           END-IF
      *                適用欄に編集
                       WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
                           IF  WRK-WCLM          >   WRK-BYOMEI2-MAX
                               MOVE    1             TO  FLG-TENKI
                               PERFORM 3222-BYOMEI2-OUT-SEC
                               MOVE    ZERO          TO  FLG-TENKI
                           ELSE
                               IF  WRK-CLM       >   ZERO
      *                        前の病名の直後に編集
                                   COMPUTE WRK-STCLM =   WRK-CLM   +
                                                         1
                                   MOVE    "，"  TO  WRK-BYOMEI2-NAME
                                                       (WRK-STCLM:)
                                   COMPUTE WRK-CLM   =   WRK-CLM   +
                                                         2
                               END-IF
                           END-IF
                   END-EVALUATE
               ELSE
      *            主病名フラグ・診療開始日・転帰区分 が異なる場合
                   IF      WRK-BYO-OLD        =   LOW-VALUE
                       CONTINUE
                   ELSE
                       IF  WRK-LINEBYO       <  WRK-BYOMEI-LINE
                           IF  WRK-BYOMEI1-NAME  NOT =   SPACE
                               PERFORM 3221-BYOMEI1-OUT-SEC
                           END-IF
                       ELSE
                           IF  WRK-BYOMEI2   NOT =   SPACE
                               MOVE    1             TO  FLG-TENKI
                               PERFORM 3222-BYOMEI2-OUT-SEC
                               MOVE    ZERO          TO  FLG-TENKI 
                           END-IF
                       END-IF
                   END-IF    
               END-IF
           ELSE
                IF  WRK-LINEBYO       <   WRK-BYOMEI-LINE
                    IF  WRK-BYOMEI1-NAME  NOT =   SPACE
                        PERFORM 3221-BYOMEI1-OUT-SEC
                    END-IF
                ELSE
                    IF  WRK-BYOMEI2   NOT =   SPACE
                        MOVE    1             TO  FLG-TENKI
                        PERFORM 3222-BYOMEI2-OUT-SEC
                        MOVE    ZERO          TO  FLG-TENKI
                    END-IF
                END-IF
           END-IF
      *
      *    主病名との区切り線編集処理     
           PERFORM 3211-SYUBYOFLG-HENSYU-SEC
      *
      *    対象病名を編集
           COMPUTE     WRK-STCLM         =   WRK-CLM   +   1
      *    単病名文字数を計算
           COMPUTE WRK-MOJISU          =   RECE031-BYOMEIMOJI   *   2
           MOVE    RECE031-BYOMEI      TO  WRK-BYOMEI
           MOVE    SPACE               TO  WRK-BYOMEI3
           MOVE    ZERO                TO  WRK-MOJISU3
      *
           EVALUATE    TRUE
      *        傷病名欄に編集
               WHEN    WRK-LINEBYO     <   WRK-BYOMEI-LINE
                   MOVE    WRK-BYOMEI      TO  WRK-BYOMEI1-NAME
                                                          (WRK-STCLM:)
                   IF      WRK-MOJISU       >   WRK-BYOMEI1-MAX
                       MOVE    RECE031-SRYYMD  TO  WRK-BO-SRYYMD
                       MOVE    RECE031-TENKIKBN
                                               TO  WRK-BO-TENKIKBN
                       MOVE    RECE031-TENKIYMD
                                               TO  WRK-BO-TENKIYMD
                       PERFORM 3221-BYOMEI1-OUT-SEC
                       IF      WRK-LINEBYO     <   WRK-BYOMEI-LINE 
                           MOVE    WRK-BYOMEI(49:) TO  WRK-BYOMEI1-NAME
                                                          (WRK-STCLM:)
                           MOVE    1               TO  WRK-CONTINUE
      *
                           MOVE    WRK-BYOMEI(97:) TO  WRK-BYOMEI3
                           COMPUTE WRK-MOJISU3     =   WRK-MOJISU   -
                                                       96                                   
                       ELSE
                           MOVE    WRK-BYOMEI(49:) TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
                           MOVE    1               TO  WRK-CONTINUE    
      *
                           MOVE    WRK-BYOMEI(93:) TO  WRK-BYOMEI3
                           COMPUTE WRK-MOJISU3     =   WRK-MOJISU   -
                                                       92
                       END-IF                                   
                   END-IF    
      *        適用欄に編集
               WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
                   MOVE    WRK-BYOMEI      TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
                   IF      WRK-MOJISU       >   WRK-BYOMEI2-MAX 
                       MOVE    RECE031-SRYYMD  TO  WRK-BO-SRYYMD
                       MOVE    RECE031-TENKIKBN
                                               TO  WRK-BO-TENKIKBN
                       MOVE    RECE031-TENKIYMD
                                               TO  WRK-BO-TENKIYMD
                       PERFORM  3222-BYOMEI2-OUT-SEC
                       MOVE    1               TO  WRK-CONTINUE
                       MOVE    WRK-BYOMEI(45:) TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
      *
                       MOVE    WRK-BYOMEI(89:) TO  WRK-BYOMEI3
                       COMPUTE WRK-MOJISU3     =   WRK-MOJISU   -   88
                   END-IF                                       
           END-EVALUATE
      *
           COMPUTE     WRK-CLM           =   WRK-MOJISU
                                         +   WRK-CLM
      *
           IF      WRK-BYOMEI3       NOT =   SPACE
               EVALUATE    TRUE
      *            傷病名欄に編集
                   WHEN    WRK-LINEBYO     <   WRK-BYOMEI-LINE
                       PERFORM 3221-BYOMEI1-OUT-SEC
                       IF      WRK-LINEBYO     <   WRK-BYOMEI-LINE 
                           MOVE    WRK-BYOMEI3     TO  WRK-BYOMEI1-NAME
                                                          (WRK-STCLM:)
                           MOVE    1               TO  WRK-CONTINUE
                       ELSE
                           MOVE    WRK-BYOMEI3     TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
                            MOVE    1               TO  WRK-CONTINUE    
                       END-IF                                   
      *            適用欄に編集
                   WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
                       PERFORM  3222-BYOMEI2-OUT-SEC
                       MOVE    1               TO  WRK-CONTINUE
                       MOVE    WRK-BYOMEI3     TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
               END-EVALUATE
      *
               COMPUTE     WRK-CLM           =   WRK-MOJISU3
                                             +   WRK-CLM
           END-IF
      *     
           MOVE    RECE031-SYUBYOFLG     TO  WRK-BO-SYUBYOFLG
           MOVE    RECE031-SRYYMD        TO  WRK-BO-SRYYMD
           MOVE    RECE031-TENKIKBN      TO  WRK-BO-TENKIKBN
           MOVE    RECE031-TENKIYMD      TO  WRK-BO-TENKIYMD
      *
           PERFORM 990-RECE031-NEXT-SEC
      *
           .
       3210-BYOMEI-SET-EXT.
           EXIT.
      ****************************************************************
      *      主病名と副病名の区切り線の編集
      ****************************************************************
       3211-SYUBYOFLG-HENSYU-SEC         SECTION.
      *
      *    主病名編集フラグが区切り有のときのみ処理を行う
           IF      SYS-2005-SYUBYOHENFLG   =   "1" OR  "3"
      *        主病名との区切り線（副傷病名があるときのみ）     
               IF      WRK-BYO-OLD             =   LOW-VALUE
                   CONTINUE
               ELSE
                   IF    ( FLG-RECE031             =   ZERO            )
                   AND   ( RECE031-SYUBYOFLG   NOT =   WRK-BO-SYUBYOFLG)
                   AND   ( WRK-BO-SYUBYOFLG        =   "1"             )
                       IF      WRK-LINEBYO     <   WRK-BYOMEI-LINE
                           ADD     1           TO  WRK-LINEBYO
                           MOVE    WRK-KUGIRI3 TO
                                          HC06-BYOMEI(WRK-LINEBYO)(2:52)
                       ELSE
                           IF      WRK-LINETEK >=   WRK-TEKIYOU-LINE
                               PERFORM 34120-PAGE-SEC
                           END-IF    
                           MOVE    1           TO  FLG-KUGIRI
                           PERFORM 34110-KUGIRI-LINE-SEC
                           MOVE    ZERO        TO  FLG-KUGIRI   
                       END-IF
                   END-IF
               END-IF
           END-IF        
      *
           .
       3211-SYUBYOFLG-HENSYU-EXT.
           EXIT.
      ****************************************************************
      *     傷病名１出力処理
      *      WRK-CONTINUE = 1 の場合、２行編集の２行目
      ****************************************************************
       3221-BYOMEI1-OUT-SEC        SECTION.
      *
      *    傷病名欄の行カウント
           ADD     1               TO  WRK-LINEBYO
      *
           IF  WRK-CONTINUE        =   ZERO
      *        病名の項番カウント
               ADD     1                   TO  WRK-LINE
      *        項番
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI 
               MOVE    "("             TO  KANACHK-MAE-INPUT (1:1)
               MOVE    WRK-LINE (2:1)  TO  KANACHK-MAE-INPUT (2:1)
               MOVE    ")"             TO  KANACHK-MAE-INPUT (3:1)
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-BYOMEI1-NO 
               END-IF    
      *        診療開始日
               MOVE    WRK-BYOMEI1-NO      TO  HC06-SRYYMD
                                                    (WRK-LINEBYO)
               MOVE    WRK-BO-SRYYMD       TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3-G (1:2)
                                           TO  HC06-SRYYMD
                                                    (WRK-LINEBYO)(7:2)
               MOVE    LNK-DAY2-EDTYMD3 (5:)
                                           TO  HC06-SRYYMD
                                                    (WRK-LINEBYO)(9:)
      *        転帰
               MOVE    WRK-BYOMEI1-NO    TO  HC06-TENKI(WRK-LINEBYO)
               EVALUATE    WRK-BO-TENKIKBN
               WHEN    "1"
                   MOVE    "治ゆ"    TO  HC06-TENKI(WRK-LINEBYO)(7:)
               WHEN    "2"
                   MOVE    "死亡"    TO  HC06-TENKI(WRK-LINEBYO)(7:)
               WHEN    "3"
                   MOVE    "中止"    TO  HC06-TENKI(WRK-LINEBYO)(7:)
               WHEN    OTHER
                   MOVE    SPACE     TO  HC06-TENKI(WRK-LINEBYO)
               END-EVALUATE
      *        転帰日
               IF      WRK-BO-TENKIYMD     =   SPACE
                   CONTINUE 
               ELSE
                   MOVE    WRK-BO-TENKIYMD TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3 (11:)
                                           TO  HC06-TENKIYMD
                                                    (WRK-LINEBYO)
               END-IF                                     
           END-IF
      *    病名
           INSPECT WRK-BYOMEI1
                           REPLACING    ALL   "  "   BY   "　"
           MOVE    WRK-BYOMEI1     TO  HC06-BYOMEI(WRK-LINEBYO)
      *
      *    クリア
           MOVE    SPACE           TO  WRK-BYOMEI1
           MOVE    ZERO            TO  WRK-CLM
           MOVE    ZERO            TO  WRK-CONTINUE
      *
           .
       3221-BYOMEI1-OUT-EXT.
           EXIT.
      ****************************************************************
      *     傷病名２出力処理
      *      WRK-CONTINUE = 0 の場合、２行編集の１行目
      ****************************************************************
       3222-BYOMEI2-OUT-SEC        SECTION.
      *
      *    適用欄の行カウント
           IF      WRK-LINETEK     >=   WRK-TEKIYOU-LINE
               PERFORM 34120-PAGE-SEC
           END-IF    
           ADD     1               TO  WRK-LINETEK
      
      *
           IF  WRK-CONTINUE        =   ZERO
      *        病名の項番カウント
               ADD     1                   TO  WRK-LINE
      *        項番
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI 
               IF  WRK-LINE            <   10
                   MOVE    "("             TO  KANACHK-MAE-INPUT (1:1)
                   MOVE    WRK-LINE (2:1)  TO  KANACHK-MAE-INPUT (2:1)
                   MOVE    ")"             TO  KANACHK-MAE-INPUT (3:1)
               ELSE    
                   MOVE    "("             TO  KANACHK-MAE-INPUT (1:1)
                   MOVE    WRK-LINE (1:2)  TO  KANACHK-MAE-INPUT (2:2)
                   MOVE    ")"             TO  KANACHK-MAE-INPUT (4:1)
               END-IF
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-BYOMEI2-NO 
               END-IF    
           END-IF    
      *
      *    診療開始日・転帰を印字するスペースが有る場合
           IF      SYS-2005-TENKIHENKBN    =   ZERO
               IF  WRK-BYOMEI2-ELSE    =   SPACE
      *            診療年月日・転帰　編集処理
                   PERFORM 32221-SRYYMD-TENKI-HEN-SEC
                   MOVE    ZERO            TO  FLG-TENKI
               END-IF
           ELSE
               IF      SYS-25-TENKIBYOMEI-KBN
                                       =   "0"
                   IF  WRK-BYOMEI2R1-ELSE  =   SPACE
      *                診療年月日・転帰　編集処理
                       PERFORM 32221-SRYYMD-TENKI-HEN-SEC
                       MOVE    ZERO            TO  FLG-TENKI
                   END-IF
               ELSE
                   IF  WRK-BYOMEI2R-ELSE   =   SPACE
      *                診療年月日・転帰　編集処理
                       PERFORM 32221-SRYYMD-TENKI-HEN-SEC
                       MOVE    ZERO            TO  FLG-TENKI
                   END-IF
               END-IF
           END-IF    
      *
           INSPECT WRK-BYOMEI2
                           REPLACING    ALL   "  "   BY   "　"
           IF      CNT-PAGE        =   1
               MOVE    WRK-BYOMEI2 TO  HC06-TEKIYO  (WRK-LINETEK)
           ELSE
               EVALUATE    SYS-2005-NEXTPRTKBN-G
                   WHEN    "0"    
                       MOVE    WRK-BYOMEI2 TO  HC06-TEKIYO (WRK-LINETEK)
                   WHEN    "1"    
                       MOVE    WRK-BYOMEI2 TO  HC061-TEKIYO(WRK-LINETEK)
                   WHEN    "2"    
                       MOVE    WRK-BYOMEI2 TO  HC062-TEKIYO(WRK-LINETEK)
                   WHEN    "3"    
                       MOVE    WRK-BYOMEI2 TO  HC063-TEKIYO(WRK-LINETEK)
               END-EVALUATE
           END-IF    
      *
      *    最後の病名に印字するスペースがなかったとき 
           IF       FLG-TENKI              =   1     
      *        診療年月日・転帰　編集処理
               MOVE    SPACE           TO  WRK-BYOMEI2
               PERFORM 32221-SRYYMD-TENKI-HEN-SEC
      *        適用欄の行カウント
               IF      WRK-LINETEK     >=   WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF    
               ADD     1               TO  WRK-LINETEK
               INSPECT WRK-BYOMEI2
                           REPLACING    ALL   "  "   BY   "　"
               IF      CNT-PAGE        =   1
                   MOVE    WRK-BYOMEI2 TO  HC06-TEKIYO  (WRK-LINETEK)
               ELSE
                   EVALUATE    SYS-2005-NEXTPRTKBN-G
                       WHEN    "0"    
                           MOVE    WRK-BYOMEI2 
                                           TO  HC06-TEKIYO (WRK-LINETEK)
                       WHEN    "1"    
                           MOVE    WRK-BYOMEI2
                                           TO  HC061-TEKIYO(WRK-LINETEK)
                       WHEN    "2"    
                           MOVE    WRK-BYOMEI2
                                           TO  HC062-TEKIYO(WRK-LINETEK)
                       WHEN    "3"    
                           MOVE    WRK-BYOMEI2
                                           TO  HC063-TEKIYO(WRK-LINETEK)
                   END-EVALUATE
               END-IF    
           END-IF
      *
      *
      *    クリア
           MOVE    SPACE           TO  WRK-BYOMEI2
           MOVE    ZERO            TO  WRK-CLM
           MOVE    ZERO            TO  WRK-CONTINUE
      *
           .
       3222-BYOMEI2-OUT-EXT.
           EXIT.
      *****************************************************************
      *    診療年月日・転帰　編集処理
      *****************************************************************
       32221-SRYYMD-TENKI-HEN-SEC    SECTION.
      *
      *    転帰日を編集しないとき
           IF      SYS-2005-TENKIHENKBN    =   ZERO
      *        診療開始日
               MOVE    WRK-BO-SRYYMD       TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3-G (1:2)
                                           TO  WRK-BYOMEI2-YMD-G
               MOVE    LNK-DAY2-EDTYMD3 (5:)
                                           TO  WRK-BYOMEI2-YMD
      *        転帰
               EVALUATE    WRK-BO-TENKIKBN
               WHEN    "1"
                   MOVE    "治ゆ"          TO  WRK-BYOMEI2-TENKI
               WHEN    "2"
                   MOVE    "死亡"          TO  WRK-BYOMEI2-TENKI
               WHEN    "3"
                   MOVE    "中止"          TO  WRK-BYOMEI2-TENKI
               WHEN    OTHER
                   MOVE    SPACE           TO  WRK-BYOMEI2-TENKI
               END-EVALUATE
           ELSE
               IF      SYS-25-TENKIBYOMEI-KBN
                                           =   "0"
      *            診療開始日
                   MOVE    WRK-BO-SRYYMD       TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3-G (1:2)
                                               TO  WRK-BYOMEI2R1-YMD-G
                   MOVE    LNK-DAY2-EDTYMD3 (5:)
                                               TO  WRK-BYOMEI2R1-YMD
      *            転帰
                   EVALUATE    WRK-BO-TENKIKBN
                   WHEN    "1"
                       MOVE    "治ゆ"          TO  WRK-BYOMEI2R1-TENKI
                   WHEN    "2"
                       MOVE    "死亡"          TO  WRK-BYOMEI2R1-TENKI
                   WHEN    "3"
                       MOVE    "中止"          TO  WRK-BYOMEI2R1-TENKI
                   WHEN    OTHER
                       MOVE    SPACE           TO  WRK-BYOMEI2R1-TENKI
                   END-EVALUATE
      *            転帰日
                   IF      WRK-BO-TENKIYMD     =   SPACE
                       CONTINUE
                   ELSE
                       MOVE    WRK-BO-TENKIYMD     TO  WRK-SYMD
                       PERFORM 31012-SEIWA-HEN-SEC
                       MOVE    LNK-DAY2-EDTYMD3 (17:)
                                                   TO
                                               WRK-BYOMEI2R1-TENKIYMD
                   END-IF
               ELSE
      *            診療開始日
                   MOVE    WRK-BO-SRYYMD       TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3-G (1:2)
                                               TO  WRK-BYOMEI2R-YMD-G
                   MOVE    LNK-DAY2-EDTYMD3 (5:)
                                               TO  WRK-BYOMEI2R-YMD
      *            転帰
                   EVALUATE    WRK-BO-TENKIKBN
                   WHEN    "1"
                       MOVE    "治ゆ"          TO  WRK-BYOMEI2R-TENKI
                   WHEN    "2"
                       MOVE    "死亡"          TO  WRK-BYOMEI2R-TENKI
                   WHEN    "3"
                       MOVE    "中止"          TO  WRK-BYOMEI2R-TENKI
                   WHEN    OTHER
                       MOVE    SPACE           TO  WRK-BYOMEI2R-TENKI
                   END-EVALUATE
      *            転帰日
                   IF      WRK-BO-TENKIYMD     =   SPACE
                       CONTINUE 
                   ELSE
                       MOVE    WRK-BO-TENKIYMD     TO  WRK-SYMD
                       PERFORM 31012-SEIWA-HEN-SEC
                       MOVE    LNK-DAY2-EDTYMD3 (11:)
                                               TO  WRK-BYOMEI2R-TENKIYMD
                   END-IF
               END-IF                                
           END-IF                                     
      *
           .
       32221-SRYYMD-TENKI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<点数>処理
      *****************************************************************
       330-HC101-TENSU-HEN-SEC      SECTION.
      *
           EVALUATE  RECE02-SHOSHINTIME
               WHEN  "1"
                     MOVE  "（時間外）"   TO  HC06-SHOSHINTIME
               WHEN  "2"
                     MOVE  "（深夜）"     TO  HC06-SHOSHINTIME
               WHEN  "3"
                     MOVE  "（休日）"     TO  HC06-SHOSHINTIME
               WHEN  "4"
                     MOVE  "時外・深夜"   TO  HC06-SHOSHINTIME
               WHEN  "5"
                     MOVE  "時外・休日"   TO  HC06-SHOSHINTIME
               WHEN  "6"
                     MOVE  "休日・深夜"   TO  HC06-SHOSHINTIME
               WHEN  "7"
                     MOVE  "外・休・深"   TO  HC06-SHOSHINTIME
           END-EVALUATE
      *     
           IF      RECE02-HKNNUM      =   SPACE
               IF      SEIHIDX            >   ZERO 
      *            長崎の生保と被爆（０１２＋１８６）のときはそのまま記載
                   IF    ( SYS-1001-PREFNUM    =   "42"  )
                   AND   ( RECE02-KOHNUM (1)   =   "186" )
                   AND   ( RECE02-KOHNUM (2)   =   "012" )
                       MOVE    2                  TO  IDX1
                       MOVE    3                  TO  IDX2
                   ELSE
      *                愛知の生保と特定疾患（０１２＋１９１）のときはそのまま記載
                       IF    ( SYS-1001-PREFNUM    =   "23"  )
                       AND   ( RECE02-KOHNUM (1)   =   "191" )
                       AND   ( RECE02-KOHNUM (2)   =   "012" )
                           MOVE    2                  TO  IDX1
                           MOVE    3                  TO  IDX2
                       ELSE
                           COMPUTE IDX1    =   SEIHIDX   +   1
                           MOVE    2                  TO  IDX2
                           IF      IDX1               =   IDX2
                               COMPUTE  IDX2  =  IDX1  +  1
                           END-IF
                       END-IF
                   END-IF
               ELSE
                   MOVE    2                  TO  IDX1
                   MOVE    3                  TO  IDX2
               END-IF 
           ELSE 
               MOVE    1                  TO  IDX1
               MOVE    2                  TO  IDX2
           END-IF      
      *            
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   WRK-TEKIYOU-LINE
      *        <単価>
               EVALUATE    IDX
      *        再診
               WHEN    2
               WHEN    3
               WHEN    4
               WHEN    5
               WHEN    6
                   COMPUTE   WRK-TNSIDX          =   IDX   -   1
                   MOVE    RECE02-KHNTEN (IDX)   TO  WRK-KHNTENZ
                   MOVE    WRK-KHNTEN            TO  HC06-KHNTEN
                                                         (WRK-TNSIDX)
      *        投薬
               WHEN    15
                   MOVE    RECE02-KHNTEN (IDX)   TO  WRK-KHNTENZ
                   MOVE    WRK-KHNTEN            TO  HC06-KHNTEN1
               WHEN    18
               WHEN    19
                   COMPUTE   WRK-TNSIDX          =   IDX   -   17
                   MOVE    RECE02-KHNTEN (IDX)   TO  WRK-KHNTENZ
                   MOVE    WRK-KHNTEN            TO  HC06-KHNTEN2
                                                          (WRK-TNSIDX)
               WHEN    OTHER
                   CONTINUE
               END-EVALUATE
      *        <回数>
               MOVE    RECE02-KAISU(IDX)         TO  WRK-KAISUZ
               MOVE    WRK-KAISU                 TO  HC06-KAISU   (IDX)
      *        <点数・公費点数>
      *        自立支援医療と生活保護の併用の時、点数欄に総医療費分を記載
               IF    FLG-JIRITU-HENSYU  NOT =  ZERO
                   MOVE    RECE02-ALLTENSU(IDX)
                                           TO  WRK-TENSUZ
               ELSE
                   MOVE    RECE02-TENSU(IDX IDX1)
                                           TO  WRK-TENSUZ
               END-IF
               MOVE    WRK-TENSU                 TO  HC06-TENSU   (IDX)
               IF      IDX2                      >   ZERO
                 IF   ( RECE02-HKNNUM                =  SPACE )
                 OR   ((RECE02-HKNNUM            NOT =  SPACE) AND
                       (RECE02-HENTOTALTEN(IDX2) NOT =  ZERO ) AND
                       (RECE02-HENTOTALTEN(IDX1) NOT =
                                     RECE02-HENTOTALTEN(IDX2)))
                  IF    RECE02-SEIHOFLG    =  "1"
                    IF   RECE02-SYUTANARIFLG  =  ZERO
                      MOVE  RECE02-TENSU(IDX IDX2) TO  WRK-TENSUZ
                      MOVE  WRK-TENSU              TO  HC06-TENSU(IDX)
      *
                      COMPUTE WRK-SUM-TENSU = RECE02-TENSU(IDX IDX2)
                                            - RECE02-TENSU(IDX IDX1)
                      MOVE  WRK-SUM-TENSU          TO  WRK-KOUTENZ
                      MOVE  WRK-KOUTEN             TO  HC06-KOUTEN(IDX)
                    ELSE
                      COMPUTE WRK-SUM-TENSU = RECE02-TENSU(IDX IDX2)
                                            + RECE02-SYUTANTEN(IDX)
                      MOVE  WRK-SUM-TENSU          TO  WRK-TENSUZ
                      MOVE  WRK-TENSU              TO  HC06-TENSU(IDX)
      *
                      MOVE  RECE02-TENSU(IDX IDX2) TO  WRK-KOUTENZ
                      MOVE  WRK-KOUTEN             TO  HC06-KOUTEN(IDX)
                    END-IF
                  ELSE
                      MOVE  RECE02-TENSU(IDX IDX2) TO  WRK-KOUTENZ
                      MOVE  WRK-KOUTEN             TO  HC06-KOUTEN(IDX)
                  END-IF
                 END-IF
               END-IF
           END-PERFORM
      *
      *        <請求点・一部負担金>
      *
      *    
           MOVE    ZERO            TO  IDC
           MOVE    ZERO            TO  IDD
                                       IDX4
                                       IDX5
           IF      WRK-3SYU-KBN           =   ALL "1" AND
                   RECE02-KOHNUM (4)      =   SPACE   AND  
                   RECE02-HKNNUM          =   SPACE            
      *        ３種の場合
               IF  KAKKIDX         >   SESNIDX
                   MOVE    SESNIDX     TO  IDA
                   MOVE    KAKKIDX     TO  IDB
               ELSE
                   MOVE    KAKKIDX     TO  IDA
                   MOVE    SESNIDX     TO  IDB
               END-IF
               COMPUTE IDX1     =   SEIHIDX    +   1
               COMPUTE IDX2     =   IDA        +   1
               COMPUTE IDX3     =   IDB        +   1
           ELSE    
      *        ３種以外の場合
               MOVE    1               TO  IDA
               MOVE    2               TO  IDB
               MOVE    1               TO  IDX1
               MOVE    2               TO  IDX2
               MOVE    3               TO  IDX3
               IF      RECE02-KOHNUM (3)   NOT =   SPACE
                   MOVE    3               TO  IDC
                   MOVE    4               TO  IDX4
               END-IF
               IF      RECE02-KOHNUM (4)   NOT =   SPACE
                   MOVE    4               TO  IDD
                   MOVE    5               TO  IDX5
               END-IF
           END-IF
      *    保険（減免）
           EVALUATE    RECE02-GENMENKBN
               WHEN    1
                   IF    RECE02-GENGAKURATE  >   ZERO
                       COMPUTE WRK-KYURATE-9   =
                                       RECE02-GENGAKURATE  /   10 
                       MOVE    WRK-KYURATE-9   TO  WRK-KYURATE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    WRK-KYURATE     TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF    ( KANACHK-RC      =   ZERO )
                       AND   ( KANACHK-MAX     >   ZERO )
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  HC06-GENMEN3(5:)
                       END-IF    
                       MOVE   "減額"           TO  HC06-GENMEN3(1:4)
                       MOVE   "割"             TO  HC06-GENMEN3(9:2)
      *
                       MOVE    RECE02-GENGAKURATE
                                               TO  WRK-GENMEN-FTNRATE
                       IF   WRK-GENMEN-FTNRATE2  NOT =  ZERO
                         INITIALIZE            ORCSKANACHKAREA
                         MOVE  "2"             TO  KANACHK-SYORI
                         MOVE  WRK-GENMEN-FTNRATE2
                                               TO  KANACHK-MAE-INPUT
                         CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                         IF  ( KANACHK-RC      =   ZERO )
                         AND ( KANACHK-MAX     >   ZERO )
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  HC06-GENMEN3(11:)
                         END-IF
                         MOVE "分"             TO  HC06-GENMEN3(13:2)
                       END-IF
                   ELSE
                     IF  RECE02-GENGAKU      >   ZERO
                       MOVE    RECE02-GENGAKU      TO  WRK-KGK6
                       MOVE    WRK-KGK6            TO  HC06-GENMENKIN
                       MOVE    "減額金額"          TO  HC06-GENMEN
                       MOVE    "円"                TO  HC06-GENMEN2
                     ELSE
                       MOVE    "減額後金額"        TO  HC06-GENMEN
                       MOVE    "円"                TO  HC06-GENMEN2
                     END-IF
                   END-IF
               WHEN    2
                   MOVE    "免除"             TO  HC06-GENMEN
               WHEN    3
                   MOVE    "支払猶予"         TO  HC06-GENMEN
           END-EVALUATE 
      *    保険
           MOVE      ZERO                     TO  FLG-TOTALTEN
           IF    ((IDX1                         =   1   ) AND
                  (RECE02-HKNNUM            NOT =  SPACE))
           OR    ((IDX1                     NOT =   1   ) AND
                  (RECE02-KOHNUM(IDX1 - 1)  NOT =  SPACE))
               MOVE    RECE02-TOTALTEN (IDX1) TO  WRK-KGK6
               MOVE    WRK-KGK6               TO  HC06-TOTALTEN(1)
               MOVE    1                      TO  FLG-TOTALTEN
               IF    ( RECE02-FTNMONEY (IDX1) >    ZERO   )
               OR    ( RECE02-ZAITAKU       =  "1" OR "2" )
               OR    ( RECE02-RJNNYUFTN       =    1      )
                   MOVE    RECE02-FTNMONEY (IDX1) TO  WRK-KGK7
                   IF      RECE02-GENMENKBN   =   ZERO
                       MOVE    WRK-KGK7           TO  HC06-FTNMONEY (1)
                   ELSE
                       MOVE    WRK-KGK7           TO  HC06-FTNMONEY-GEN
                   END-IF
               END-IF
           END-IF
           IF  ((RECE02-SAIKEIZEROKSI1       =   1                ) AND
                (RECE02-ZAITAKU-SAIKEI   NOT =  999999999         ))
           OR  ((RECE02-SAIKEIZEROKSI1       =  ZERO              ) AND
                (RECE02-ZAITAKU-SAIKEI   NOT =  999999999 AND ZERO))
             MOVE    "("                    TO  HC06-FTNMONEYSAI(1:1)
             MOVE   RECE02-ZAITAKU-SAIKEI   TO  WRK-KGK7
             MOVE   WRK-KGK7                TO  HC06-FTNMONEYSAI(3:8)
             MOVE    ")"                    TO  HC06-FTNMONEYSAI(11:1)
           END-IF
           IF  ((RECE02-SAIKEIZEROKSI2       =   1                ) AND
                (RECE02-ZAITAKU-SAIKEI2  NOT =  999999999         ))
           OR  ((RECE02-SAIKEIZEROKSI2       =  ZERO              ) AND
                (RECE02-ZAITAKU-SAIKEI2  NOT =  999999999 AND ZERO))
             MOVE    "("                    TO  HC06-FTNMONEYSAI2(1:1)
             MOVE   RECE02-ZAITAKU-SAIKEI2  TO  WRK-KGK7
             MOVE   WRK-KGK7                TO  HC06-FTNMONEYSAI2(3:8)
             MOVE    ")"                    TO  HC06-FTNMONEYSAI2(11:1)
           END-IF
      *
           INITIALIZE                     WRK-FTNMONEY-HENSYU-AREA
      *    公費１
           IF    ( RECE02-KOHNUM(IDX2 - 1)    NOT =   SPACE            )
           AND   ((RECE02-TOTALTEN (IDX2)
                                      NOT =   RECE02-TOTALTEN (IDX1)) OR
                  (RECE02-FTNMONEY (IDX2)
                                      NOT =   RECE02-FTNMONEY (IDX1))  )
             IF    RECE02-EKF-FTNKEIFLG(IDX2)  =  "2"
               CONTINUE
             ELSE
               IF    RECE02-FTNMONEY (IDX2)   >   ZERO
                   MOVE    RECE02-FTNMONEY (IDX2) TO  WRK-KGK7
                   MOVE    WRK-KGK7               TO
                                                  WRK-KOH-FTNMONEY(1)
               END-IF
             END-IF
           END-IF
           IF    ( RECE02-KOHNUM(IDX2 - 1)    NOT =   SPACE         )
           AND   ( RECE02-FTNMONEY (IDX2) =   RECE02-FTNMONEY (IDX1))
           AND   ( RECE02-KISAIFLG (IDX2) =   "1"                   )
               IF    RECE02-FTNMONEY (IDX2)   >   ZERO
                   MOVE    RECE02-FTNMONEY (IDX2) TO  WRK-KGK7
                   MOVE    WRK-KGK7               TO
                                                  WRK-KOH-FTNMONEY(1)
               END-IF
           END-IF
           IF    ( RECE02-KOHNUM(IDX2 - 1)    NOT =   SPACE )
           AND   ( RECE02-FTNMONEY (IDX2)         =   ZERO  )
           AND   ( RECE02-ZEROKISAIFLG (IDX2)     =   "1"   )
                   MOVE    RECE02-FTNMONEY (IDX2) TO  WRK-KGK7
                   MOVE    WRK-KGK7               TO
                                                  WRK-KOH-FTNMONEY(1)
           END-IF
           IF      RECE02-KOHNUM(IDX2 - 1)    NOT =   SPACE
             IF  (FLG-TOTALTEN                 = ZERO                 )
             OR  (RECE02-TOTALTEN(IDX1)    NOT = RECE02-TOTALTEN(IDX2))
             OR  (RECE02-JSKISAIFLG            = "1"                  )
             OR  (RECE02-EKF-TOTALTENFLG(IDX2) = "1"                  )
               MOVE    RECE02-TOTALTEN (IDX2) TO  WRK-KGK6
               MOVE    WRK-KGK6               TO  WRK-KOH-TOTALTEN(1)
             END-IF
           END-IF
      *    公費２
           IF  ( RECE02-KOHNUM(IDX3 - 1)    NOT =   SPACE            )
           AND ((RECE02-TOTALTEN (IDX3)
                                    NOT =   RECE02-TOTALTEN (IDX1)) OR
                (RECE02-TOTALTEN (IDX3)
                                    NOT =   RECE02-TOTALTEN (IDX2)) OR
                (RECE02-FTNMONEY (IDX3)
                                    NOT =   RECE02-FTNMONEY (IDX1)) OR
                (RECE02-FTNMONEY (IDX3)
                                    NOT =   RECE02-FTNMONEY (IDX2))  )
               IF    RECE02-EKF-FTNKEIFLG(IDX3)  =  "2"
                 CONTINUE
               ELSE
                 IF      RECE02-FTNMONEY (IDX3)   >   ZERO
                     MOVE    RECE02-FTNMONEY (IDX3)
                                              TO  WRK-KGK7
                   MOVE    WRK-KGK7           TO  WRK-KOH-FTNMONEY(2)
                 END-IF
               END-IF
           END-IF
           IF    ( RECE02-KOHNUM(IDX3 - 1)    NOT =   SPACE         )
           AND   ( RECE02-FTNMONEY (IDX3) =   RECE02-FTNMONEY (IDX2))
           AND   ( RECE02-KISAIFLG (IDX3) =   "1"                   )
                 IF    RECE02-FTNMONEY (IDX3)   >   ZERO
                   MOVE    RECE02-FTNMONEY (IDX3)
                                              TO  WRK-KGK7
                   MOVE    WRK-KGK7           TO  WRK-KOH-FTNMONEY(2)
                 END-IF
           END-IF
           IF    ( RECE02-KOHNUM(IDX3 - 1)    NOT =   SPACE )
           AND   ( RECE02-FTNMONEY (IDX3)         =   ZERO  )
           AND   ( RECE02-ZEROKISAIFLG (IDX3)     =   "1"   )
                   MOVE    RECE02-FTNMONEY (IDX3)
                                              TO  WRK-KGK7
                   MOVE    WRK-KGK7           TO  WRK-KOH-FTNMONEY(2)
           END-IF
           IF      RECE02-KOHNUM(IDX3 - 1)    NOT =   SPACE
             IF  (RECE02-TOTALTEN(IDX2)    NOT = RECE02-TOTALTEN(IDX3))
             OR  (RECE02-JSKISAIFLG            = "1"                  )
             OR  (RECE02-EKF-TOTALTENFLG(IDX3) = "1"                  )
               MOVE    RECE02-TOTALTEN (IDX3) TO  WRK-KGK6
               MOVE    WRK-KGK6               TO  WRK-KOH-TOTALTEN(2)
             END-IF
           END-IF
      *
      *    公費３
           IF      IDX4                    >   ZERO
               IF    ( RECE02-KOHNUM(IDX4 - 1)    NOT =   SPACE       )
               AND   ((RECE02-TOTALTEN (IDX4)
                                   NOT =   RECE02-TOTALTEN (IDX1)) OR
                      (RECE02-TOTALTEN (IDX4)
                                   NOT =   RECE02-TOTALTEN (IDX2)) OR
                      (RECE02-TOTALTEN (IDX4)
                                   NOT =   RECE02-TOTALTEN (IDX3)) OR
                      (RECE02-FTNMONEY (IDX4)
                                   NOT =   RECE02-FTNMONEY (IDX1)) OR
                      (RECE02-FTNMONEY (IDX4)
                                   NOT =   RECE02-FTNMONEY (IDX2)) OR
                      (RECE02-FTNMONEY (IDX4)
                                   NOT =   RECE02-FTNMONEY (IDX3))    )
                   IF    RECE02-EKF-FTNKEIFLG(IDX4)  =  "2"
                       CONTINUE
                   ELSE
                       IF      RECE02-FTNMONEY (IDX4)   >   ZERO
                           MOVE    RECE02-FTNMONEY (IDX4)
                                               TO  WRK-KGK7
                           MOVE    WRK-KGK7    TO  WRK-KOH-FTNMONEY(3)
                       END-IF
                   END-IF
               END-IF
               IF    ( RECE02-KOHNUM(IDX4 - 1)    NOT =   SPACE        )
               AND   ( RECE02-FTNMONEY (IDX4)  = RECE02-FTNMONEY (IDX3))
               AND   ( RECE02-KISAIFLG (IDX4)  = "1"                   )
                   IF      RECE02-FTNMONEY (IDX4)   >   ZERO
                       MOVE    RECE02-FTNMONEY (IDX4)
                                               TO  WRK-KGK7
                       MOVE    WRK-KGK7        TO  WRK-KOH-FTNMONEY(3)
                   END-IF
               END-IF
               IF    ( RECE02-KOHNUM(IDX4 - 1)    NOT =   SPACE )
               AND   ( RECE02-FTNMONEY (IDX4)         =   ZERO  )
               AND   ( RECE02-ZEROKISAIFLG (IDX4)     =   "1"   )
                   MOVE    RECE02-FTNMONEY (IDX4)
                                               TO  WRK-KGK7
                   MOVE    WRK-KGK7            TO  WRK-KOH-FTNMONEY(3)
               END-IF
               IF      RECE02-KOHNUM(IDX4 - 1)    NOT =   SPACE
                 IF  (RECE02-TOTALTEN(IDX3)
                                        NOT = RECE02-TOTALTEN(IDX4))
                 OR  (RECE02-JSKISAIFLG            = "1"           )
                 OR  (RECE02-EKF-TOTALTENFLG(IDX4) = "1"           )
                   MOVE    RECE02-TOTALTEN (IDX4)
                                              TO  WRK-KGK6
                   MOVE    WRK-KGK6           TO  WRK-KOH-TOTALTEN(3)
                 END-IF
               END-IF
           END-IF
      *    公費４
           IF      IDX5                   >   ZERO
               IF  ( RECE02-KOHNUM(IDX5 - 1)    NOT =   SPACE         )
               AND ((RECE02-TOTALTEN (IDX5)
                                    NOT =   RECE02-TOTALTEN (IDX1)) OR
                    (RECE02-TOTALTEN (IDX5)
                                    NOT =   RECE02-TOTALTEN (IDX2)) OR
                    (RECE02-TOTALTEN (IDX5)
                                    NOT =   RECE02-TOTALTEN (IDX3)) OR
                    (RECE02-TOTALTEN (IDX5)
                                    NOT =   RECE02-TOTALTEN (IDX4)) OR
                    (RECE02-FTNMONEY (IDX5)
                                    NOT =   RECE02-FTNMONEY (IDX1)) OR
                    (RECE02-FTNMONEY (IDX5)
                                    NOT =   RECE02-FTNMONEY (IDX2)) OR
                    (RECE02-FTNMONEY (IDX5)
                                    NOT =   RECE02-FTNMONEY (IDX3)) OR
                    (RECE02-FTNMONEY (IDX5)
                                    NOT =   RECE02-FTNMONEY (IDX4))   )
                   IF    RECE02-EKF-FTNKEIFLG(IDX5)  =  "2"
                     CONTINUE
                   ELSE
                       IF      RECE02-FTNMONEY (IDX5)   >   ZERO
                           MOVE    RECE02-FTNMONEY (IDX5)
                                                TO  WRK-KGK7
                           MOVE    WRK-KGK7     TO  WRK-KOH-FTNMONEY(4)
                       END-IF
                   END-IF
               END-IF
               IF    ( RECE02-KOHNUM(IDX5 - 1)    NOT =   SPACE       )
               AND   ( RECE02-FTNMONEY (IDX5) = RECE02-FTNMONEY (IDX4))
               AND   ( RECE02-KISAIFLG (IDX5) = "1"                   )
                     IF    RECE02-FTNMONEY (IDX5)   >   ZERO
                       MOVE    RECE02-FTNMONEY (IDX5)
                                               TO  WRK-KGK7
                       MOVE    WRK-KGK7        TO  WRK-KOH-FTNMONEY(4)
                     END-IF
               END-IF
               IF    ( RECE02-KOHNUM(IDX5 - 1)    NOT =   SPACE )
               AND   ( RECE02-FTNMONEY (IDX5)         =   ZERO  )
               AND   ( RECE02-ZEROKISAIFLG (IDX5)     =   "1"   )
                   MOVE    RECE02-FTNMONEY (IDX5)
                                               TO  WRK-KGK7
                   MOVE    WRK-KGK7            TO  WRK-KOH-FTNMONEY(4)
               END-IF
               IF      RECE02-KOHNUM(IDX5 - 1)    NOT =   SPACE
                 IF  (RECE02-TOTALTEN(IDX4)
                                        NOT = RECE02-TOTALTEN(IDX5))
                 OR  (RECE02-JSKISAIFLG            = "1"           )
                 OR  (RECE02-EKF-TOTALTENFLG(IDX5) = "1"           )
                   MOVE    RECE02-TOTALTEN (IDX5)
                                          TO  WRK-KGK6
                   MOVE    WRK-KGK6       TO  WRK-KOH-TOTALTEN(4)
                 END-IF
               END-IF
           END-IF
      *
      *    県単用項目
           IF      FLG-RECE11          =   ZERO
      *        保険
               IF      RECE11-TOTALTEN (IDX1)  >   ZERO
                   IF      RECE11-TOTALTEN (IDX1)  =   99999999
                       MOVE    ZERO                    TO  WRK-KGK6
                   ELSE
                       MOVE    RECE11-TOTALTEN (IDX1)  TO  WRK-KGK6
                   END-IF
                   MOVE    WRK-KGK6                TO  HC06-TOTALTEN(1)
               END-IF
               IF      RECE11-FTNMONEY (IDX1)  >   ZERO
                   IF      RECE11-FTNMONEY (IDX1)  =   999999999
                       MOVE    ZERO                    TO  WRK-KGK7
                   ELSE
                       MOVE    RECE11-FTNMONEY (IDX1)  TO  WRK-KGK7
                   END-IF
                   IF      RECE02-GENMENKBN    =   ZERO
                       MOVE    WRK-KGK7            TO  HC06-FTNMONEY(1)
                   ELSE
                       MOVE    WRK-KGK7           TO  HC06-FTNMONEY-GEN
                   END-IF
               END-IF
      *        公費１
               IF      RECE11-TOTALTEN (IDX2)  >   ZERO                     
                   IF      RECE11-TOTALTEN (IDX2)  =   99999999
                       MOVE    ZERO            TO  WRK-KGK6
                       MOVE    WRK-KGK6        TO  WRK-KOH-TOTALTEN(1)
                   ELSE
                       MOVE    RECE11-TOTALTEN (IDX2)
                                               TO  WRK-KGK6
                       MOVE    WRK-KGK6        TO  WRK-KOH-TOTALTEN(1)
                   END-IF
               END-IF
               IF    RECE11-FTNMONEY (IDX2)    >   ZERO
                   IF      RECE11-FTNMONEY (IDX2)  =   999999999
                       MOVE    ZERO            TO  WRK-KGK7
                   MOVE    WRK-KGK7            TO  WRK-KOH-FTNMONEY(1)
                   ELSE
                       MOVE    RECE11-FTNMONEY (IDX2)
                                               TO  WRK-KGK7
                   MOVE    WRK-KGK7            TO  WRK-KOH-FTNMONEY(1)
                   END-IF
               END-IF
      *        公費２
               IF      RECE11-TOTALTEN (IDX3)  >   ZERO                      
                   IF      RECE11-TOTALTEN (IDX3)  =   99999999
                       MOVE    ZERO            TO  WRK-KGK6
                       MOVE    WRK-KGK6        TO  WRK-KOH-TOTALTEN(2)
                   ELSE
                       MOVE    RECE11-TOTALTEN (IDX3)
                                               TO  WRK-KGK6
                       MOVE    WRK-KGK6        TO  WRK-KOH-TOTALTEN(2)
                   END-IF
               END-IF
               IF      RECE11-FTNMONEY (IDX3)  >   ZERO
                   IF      RECE11-FTNMONEY (IDX3)  =   999999999
                       MOVE    ZERO            TO  WRK-KGK7
                   MOVE    WRK-KGK7            TO  WRK-KOH-FTNMONEY(2)
                   ELSE
                       MOVE    RECE11-FTNMONEY (IDX3)
                                               TO  WRK-KGK7
                   MOVE    WRK-KGK7            TO  WRK-KOH-FTNMONEY(2)
                   END-IF
               END-IF
      *        公費３
               IF      IDX4                   >   ZERO
                   IF      RECE11-TOTALTEN (IDX4)  >   ZERO                      
                       IF      RECE11-TOTALTEN (IDX4)  =   99999999
                           MOVE    ZERO        TO  WRK-KGK6
                           MOVE    WRK-KGK6    TO  WRK-KOH-TOTALTEN(3)
                       ELSE
                           MOVE    RECE11-TOTALTEN (IDX4)
                                               TO  WRK-KGK6
                           MOVE    WRK-KGK6    TO  WRK-KOH-TOTALTEN(3)
                       END-IF
                   END-IF
                   IF      RECE11-FTNMONEY (IDX4)  >   ZERO
                       IF      RECE11-FTNMONEY (IDX4)  =   999999999
                           MOVE    ZERO        TO  WRK-KGK7
                           MOVE    WRK-KGK7    TO  WRK-KOH-FTNMONEY(3)
                       ELSE
                           MOVE    RECE11-FTNMONEY (IDX4)
                                               TO  WRK-KGK7
                           MOVE    WRK-KGK7    TO  WRK-KOH-FTNMONEY(3)
                       END-IF
                   END-IF
               END-IF
      *        公費４
               IF      IDX5                   >   ZERO
                   IF      RECE11-TOTALTEN (IDX5)  >   ZERO                      
                       IF      RECE11-TOTALTEN (IDX5)  =   99999999
                           MOVE    ZERO        TO  WRK-KGK6
                           MOVE    WRK-KGK6    TO  WRK-KOH-TOTALTEN(4)
                       ELSE
                           MOVE    RECE11-TOTALTEN (IDX5)
                                               TO  WRK-KGK6
                           MOVE    WRK-KGK6    TO  WRK-KOH-TOTALTEN(4)
                       END-IF
                   END-IF
                   IF      RECE11-FTNMONEY (IDX5)  >   ZERO
                       IF      RECE11-FTNMONEY (IDX5)  =   999999999
                           MOVE    ZERO        TO  WRK-KGK7
                           MOVE    WRK-KGK7    TO  WRK-KOH-FTNMONEY(4)
                       ELSE
                           MOVE    RECE11-FTNMONEY (IDX5)
                                               TO  WRK-KGK7
                           MOVE    WRK-KGK7    TO  WRK-KOH-FTNMONEY(4)
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( IDX4                   =   ZERO )
           AND   ( IDX5                   =   ZERO )
               MOVE    WRK-KOH-FTNMONEY (1)   TO  HC06-FTNMONEY (2)
               MOVE    WRK-KOH-TOTALTEN (1)   TO  HC06-TOTALTEN (2)
               MOVE    WRK-KOH-FTNMONEY (2)   TO  HC06-FTNMONEY (3)
               MOVE    WRK-KOH-TOTALTEN (2)   TO  HC06-TOTALTEN (3)
           ELSE
      *        公費３併のとき
               IF    ( IDX4                   >   ZERO )
               AND   ( IDX5                   =   ZERO )
                   MOVE    WRK-KOH-FTNMONEY (1)   TO  HC06-FTNMONEY (2)
                   MOVE    WRK-KOH-TOTALTEN (1)   TO  HC06-TOTALTEN (2)
                   MOVE    WRK-KOH-FTNMONEY (2)   TO  HC06-FTNMONEY1(3)
                   MOVE    WRK-KOH-TOTALTEN (2)   TO  HC06-TOTALTEN1(3)
                   MOVE    WRK-KOH-FTNMONEY (3)   TO  HC06-FTNMONEY1(4)
                   MOVE    WRK-KOH-TOTALTEN (3)   TO  HC06-TOTALTEN1(4)
               END-IF
               IF    ( IDX4                   >   ZERO )
               AND   ( IDX5                   >   ZERO )
      *            公費４併のとき
                   MOVE    WRK-KOH-FTNMONEY (1)   TO  HC06-FTNMONEY1(1)
                   MOVE    WRK-KOH-TOTALTEN (1)   TO  HC06-TOTALTEN1(1)
                   MOVE    WRK-KOH-FTNMONEY (2)   TO  HC06-FTNMONEY1(2)
                   MOVE    WRK-KOH-TOTALTEN (2)   TO  HC06-TOTALTEN1(2)
                   MOVE    WRK-KOH-FTNMONEY (3)   TO  HC06-FTNMONEY1(3)
                   MOVE    WRK-KOH-TOTALTEN (3)   TO  HC06-TOTALTEN1(3)
                   MOVE    WRK-KOH-FTNMONEY (4)   TO  HC06-FTNMONEY1(4)
                   MOVE    WRK-KOH-TOTALTEN (4)   TO  HC06-TOTALTEN1(4)
               END-IF
           END-IF
      *
           .
       330-HC101-TENSU-HEN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    編集処理（レセプトカスタマイズ項目から編集）
      *****************************************************************
       3301-OCR-HENSYU-SEC              SECTION.
      *
           IF      RECE11-KOHINF (IDA) NOT =   SPACE
      *        公費１負担者番号
               IF      RECE11-KOHFTNJANUM(IDA) =      SPACE
                   MOVE  ZERO               TO      OCR2-FTNJANUM
               ELSE
                   INITIALIZE                       ORCSNUMAREA
                   MOVE    RECE11-KOHFTNJANUM(IDA)
                                            TO      SNUM-INX
                   CALL    "ORCSNUM"        USING   ORCSNUMAREA
                   IF      SNUM-RC          =       ZERO
                       MOVE   SNUM-OUTNUM
                                        TO      OCR2-FTNJANUM
                   ELSE
                       MOVE   ZERO      TO      OCR2-FTNJANUM
                   END-IF
               END-IF
      *        公費１受給者番号
               IF      RECE11-KOHJKYSNUM(IDA) =       SPACE
                   MOVE  ZERO               TO      OCR2-JKYSNUM
               ELSE
                   INITIALIZE                       ORCSNUMAREA
                   MOVE    RECE11-KOHJKYSNUM(IDA)
                                            TO      SNUM-INX
                   CALL    "ORCSNUM"        USING   ORCSNUMAREA
                   IF      SNUM-RC          =       ZERO
                       MOVE   SNUM-OUTNUM
                                        TO      OCR2-JKYSNUM
                   ELSE
                       MOVE   ZERO      TO      OCR2-JKYSNUM
                   END-IF
               END-IF
           END-IF 
      *
           IF      RECE11-KOHINF (IDB) NOT =   SPACE
      *        公費２負担者番号
               IF      RECE11-KOHFTNJANUM(IDB) =      SPACE
                   MOVE  ZERO               TO      OCR3-FTNJANUM
               ELSE
                   INITIALIZE                       ORCSNUMAREA
                   MOVE    RECE11-KOHFTNJANUM(IDB)
                                            TO      SNUM-INX
                   CALL    "ORCSNUM"        USING   ORCSNUMAREA
                   IF      SNUM-RC          =       ZERO
                       MOVE   SNUM-OUTNUM
                                        TO      OCR3-FTNJANUM
                   ELSE
                       MOVE   ZERO      TO      OCR3-FTNJANUM
                   END-IF
               END-IF
      *        公費２受給者番号
               IF      RECE11-KOHJKYSNUM(IDB) =       SPACE
                   MOVE  ZERO               TO      OCR3-JKYSNUM
               ELSE
                   INITIALIZE                       ORCSNUMAREA
                   MOVE    RECE11-KOHJKYSNUM(IDB)
                                            TO      SNUM-INX
                   CALL    "ORCSNUM"        USING   ORCSNUMAREA
                   IF      SNUM-RC          =       ZERO
                       MOVE   SNUM-OUTNUM
                                        TO      OCR3-JKYSNUM
                   ELSE
                       MOVE   ZERO      TO      OCR3-JKYSNUM
                   END-IF
               END-IF
           END-IF    
      *  
      *    診療実日数
           IF      RECE11-JNISSU (IDX1) >   ZERO
               IF      RECE11-JNISSU   (IDX1)  =   99
                   MOVE    ZERO                    TO  OCR1-JNISSU
               ELSE
                   MOVE    RECE11-JNISSU(IDX1)     TO  OCR1-JNISSU
               END-IF
           END-IF
      *    公費１診療実日数
           IF      RECE11-JNISSU (IDX2) >   ZERO
               IF      RECE11-JNISSU   (IDX2)  =   99
                   MOVE    ZERO                    TO  OCR2-JNISSU
               ELSE
                   MOVE    RECE11-JNISSU  (IDX2)   TO  OCR2-JNISSU
               END-IF
           END-IF
      *    公費２診療実日数
           IF      RECE11-JNISSU (IDX3) >   ZERO
               IF      RECE11-JNISSU   (IDX3)  =   99
                   MOVE    ZERO                    TO  OCR3-JNISSU
               ELSE
                   MOVE    RECE11-JNISSU  (IDX3)   TO  OCR3-JNISSU
               END-IF
           END-IF
      *    請求点数
           IF      RECE11-TOTALTEN (IDX1)  >   ZERO
               IF      RECE11-TOTALTEN (IDX1)  =   99999999
                   MOVE    ZERO                    TO  OCR1-SKYTENSU
               ELSE
                   MOVE    RECE11-TOTALTEN(IDX1)   TO  OCR1-SKYTENSU
               END-IF
           END-IF
           IF      RECE11-FTNMONEY (IDX1)  >   ZERO
               IF      RECE11-FTNMONEY (IDX1)  =   999999999
                   MOVE    ZERO                    TO  OCR1-FTNMONEY
               ELSE
                   MOVE    RECE11-FTNMONEY(IDX1)   TO  OCR1-FTNMONEY
               END-IF
           END-IF
      *    公費・請求点数１
           IF      RECE11-TOTALTEN (IDX2)  >   ZERO                     
               IF      RECE11-TOTALTEN (IDX2)  =   99999999
                   MOVE    ZERO                    TO  OCR2-TENSU
               ELSE
                   MOVE    RECE11-TOTALTEN(IDX2)   TO  OCR2-TENSU
               END-IF 
           END-IF
           IF    RECE11-FTNMONEY (IDX2)    >   ZERO
               IF      RECE11-FTNMONEY (IDX2)  =   999999999
                   MOVE    ZERO                    TO  OCR2-FTNMONEY
               ELSE
                   MOVE    RECE11-FTNMONEY(IDX2)   TO  OCR2-FTNMONEY
               END-IF
           END-IF
      *    公費・請求点数２
           IF      RECE11-TOTALTEN (IDX3)  >   ZERO                      
               IF      RECE11-TOTALTEN (IDX3)  =   99999999
                   MOVE    ZERO                    TO  OCR3-TENSU
               ELSE
                   MOVE    RECE11-TOTALTEN(IDX3)   TO  OCR3-TENSU
               END-IF
           END-IF
           IF      RECE11-FTNMONEY (IDX3)  >   ZERO
               IF      RECE11-FTNMONEY (IDX3)  =   999999999
                   MOVE    ZERO                    TO  OCR3-FTNMONEY
               ELSE
                   MOVE    RECE11-FTNMONEY(IDX3)   TO  OCR3-FTNMONEY
               END-IF
           END-IF
      *
           PERFORM 33011-OCR1-HENSYU-SEC
           PERFORM 33012-OCR2-HENSYU-SEC
           PERFORM 33013-OCR3-HENSYU-SEC
           .
       3301-OCR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトＯＣＲ領域のセット１（Ｆ００２）
      *****************************************************************
       33011-OCR1-HENSYU-SEC                   SECTION.
      *
      *チェックディジット１
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR1-SKYTENSU        TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR1-CHKDGT1
           ELSE
                   MOVE  ZERO           TO      OCR1-CHKDGT1
           END-IF
      *チェックディジット３
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR1-FTNMONEY        TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR1-CHKDGT3
           ELSE
                   MOVE  ZERO           TO      OCR1-CHKDGT3
           END-IF
      *チェックディジット４
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR1-AREA(1:35)      TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR1-CHKDGT4
           ELSE
                   MOVE  ZERO           TO      OCR1-CHKDGT4
           END-IF
      *チェックディジット５
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR1-AREA(37:6)      TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR1-CHKDGT5
           ELSE
                   MOVE  ZERO           TO      OCR1-CHKDGT5
           END-IF
      *
           MOVE    OCR1-AREA            TO      RECE02-OCR1           
      *
           .
       33011-OCR1-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトＯＣＲ領域のセット２（Ｆ００２）
      *****************************************************************
       33012-OCR2-HENSYU-SEC                   SECTION.
      *
      *チェックディジット２
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR2-AREA(6:20)      TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR2-CHKDGT2
           ELSE
                   MOVE  ZERO           TO      OCR2-CHKDGT2
           END-IF
      *
           IF      OCR2-AREA-X          =   SPACE
               CONTINUE
           ELSE
      *
               INSPECT OCR2-AREA-X
                           REPLACING    ALL   " "   BY   "0"
      *
      *        チェックディジット３
               INITIALIZE                           LNK-CHKDGT-AREA
               MOVE    1                    TO      LNK-I-SYORIKBN
               MOVE    OCR2-AREA(27:15)     TO      LNK-I-CODE
               CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
               IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR2-CHKDGT3
               ELSE
                   MOVE  ZERO           TO      OCR2-CHKDGT3
               END-IF
      *        チェックディジット４
               INITIALIZE                           LNK-CHKDGT-AREA
               MOVE    1                    TO      LNK-I-SYORIKBN
               MOVE    OCR2-AREA(45:7)      TO      LNK-I-CODE
               CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
               IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR2-CHKDGT4
               ELSE
                   MOVE  ZERO           TO      OCR2-CHKDGT4
               END-IF
      *        チェックディジット６
               INITIALIZE                           LNK-CHKDGT-AREA
               MOVE    1                    TO      LNK-I-SYORIKBN
               MOVE    OCR2-AREA(59:5)      TO      LNK-I-CODE
               CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
               IF      LNK-O-ERRCD          =       ZERO
                  MOVE  LNK-O-CHKDGT   TO      OCR2-CHKDGT6
               ELSE
                   MOVE  ZERO           TO      OCR2-CHKDGT6
               END-IF
      *        チェックディジット７
               INITIALIZE                           LNK-CHKDGT-AREA
               MOVE    1                    TO      LNK-I-SYORIKBN
               MOVE    OCR2-AREA(43:22)     TO      LNK-I-CODE
               CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
               IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR2-CHKDGT7
               ELSE
                   MOVE  ZERO           TO      OCR2-CHKDGT7
               END-IF
      *
               IF      OCR2-AREA-X          =   ALL "0"
                   MOVE    SPACE                    TO  OCR2-AREA-X
               END-IF
           END-IF
      *  
           MOVE    OCR2-AREA            TO      RECE02-OCR2
      *
           .
       33012-OCR2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトＯＣＲ領域のセット３（Ｆ００２）
      *****************************************************************
       33013-OCR3-HENSYU-SEC                   SECTION.
      *
           IF      OCR3-AREA-X          =   SPACE
               GO  TO  33013-OCR3-HENSYU-EXT
           END-IF
      *
           INSPECT OCR3-AREA-X
                           REPLACING    ALL   " "   BY   "0"
      *
      *チェックディジット１
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR3-AREA(1:15)      TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR3-CHKDGT1
           ELSE
                   MOVE  ZERO           TO      OCR3-CHKDGT1
           END-IF
      *チェックディジット２
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR3-AREA(19:7)      TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR3-CHKDGT2
           ELSE
                   MOVE  ZERO           TO      OCR3-CHKDGT2
           END-IF
      *チェックディジット４
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR3-AREA(33:5)      TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR3-CHKDGT4
           ELSE
                   MOVE  ZERO           TO      OCR3-CHKDGT4
           END-IF
      *チェックディジット５
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR3-AREA(17:22)     TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR3-CHKDGT5
           ELSE
                   MOVE  ZERO           TO      OCR3-CHKDGT5
           END-IF
      *
           IF      OCR3-AREA-X          =   ALL "0"
               MOVE    SPACE                TO  RECE02-OCR3
           ELSE 
               MOVE    OCR3-AREA            TO  RECE02-OCR3
           END-IF
      *
           .
       33013-OCR3-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    編集処理（レセプト特記事項編集情報から編集）
      *****************************************************************
       3302-OCR-HENSYU-SEC              SECTION.
      *
      *    公費１負担者番号
           EVALUATE    RECE02-2006-FTNJANUMKBN  (IDA)
               WHEN    SPACE
                   CONTINUE
               WHEN    "01"
                   MOVE  ZERO               TO  OCR2-FTNJANUM
               WHEN    "02"
                   INITIALIZE                   ORCSNUMAREA
                   MOVE    RECE02-2006-FTNJANUM (IDA)
                                            TO  SNUM-INX
                   CALL    "ORCSNUM"        USING   ORCSNUMAREA
                   IF      SNUM-RC          =   ZERO
                       MOVE   SNUM-OUTNUM
                                            TO  OCR2-FTNJANUM
                   ELSE
                       MOVE   ZERO          TO  OCR2-FTNJANUM
                   END-IF
               WHEN    "03"
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    RECE02-KOHFTNJANUM (IDA)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-SYUBETU =   "1"
                       INITIALIZE                   ORCSNUMAREA
                       MOVE    RECE02-KOHFTNJANUM (IDA) (1:KANACHK-MAX)
                                                TO  SNUM-INX
                       CALL    "ORCSNUM"        USING   ORCSNUMAREA
                       IF      SNUM-RC          =   ZERO
                           MOVE   SNUM-OUTEDT (8:8)
                                               TO  OCR2-FTNJANUM
                       ELSE
                           MOVE   ZERO         TO  OCR2-FTNJANUM
                       END-IF
                   ELSE
                       MOVE   ZERO         TO  OCR2-FTNJANUM
                   END-IF
           END-EVALUATE
      *
      *    公費１受給者番号
           EVALUATE    RECE02-2006-JKYSNUMKBN  (IDA)
               WHEN    SPACE
                   CONTINUE
               WHEN    "01"
                   MOVE  ZERO               TO      OCR2-JKYSNUM
               WHEN    OTHER
                   INITIALIZE                       ORCSNUMAREA
                   MOVE    RECE02-2006-JKYSNUM (IDA)
                                            TO      SNUM-INX
                   CALL    "ORCSNUM"        USING   ORCSNUMAREA
                   IF      SNUM-RC          =       ZERO
                       MOVE   SNUM-OUTNUM
                                        TO      OCR2-JKYSNUM
                   ELSE
                       MOVE   ZERO      TO      OCR2-JKYSNUM
                   END-IF
           END-EVALUATE
      *
      *    公費２負担者番号
           EVALUATE    RECE02-2006-FTNJANUMKBN  (IDB)
               WHEN    SPACE
                   CONTINUE
               WHEN    "01"
                   MOVE  ZERO               TO      OCR3-FTNJANUM
               WHEN    "02"
                   INITIALIZE                       ORCSNUMAREA
                   MOVE    RECE02-2006-FTNJANUM (IDB)
                                            TO      SNUM-INX
                   CALL    "ORCSNUM"        USING   ORCSNUMAREA
                   IF      SNUM-RC          =       ZERO
                       MOVE   SNUM-OUTNUM
                                        TO      OCR3-FTNJANUM
                   ELSE
                       MOVE   ZERO      TO      OCR3-FTNJANUM
                   END-IF
               WHEN    "03"
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    RECE02-KOHFTNJANUM (IDB)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-SYUBETU =   "1"
                       INITIALIZE                   ORCSNUMAREA
                       MOVE    RECE02-KOHFTNJANUM (IDB) (1:KANACHK-MAX)
                                                TO  SNUM-INX
                       CALL    "ORCSNUM"        USING   ORCSNUMAREA
                       IF      SNUM-RC          =   ZERO
                           MOVE   SNUM-OUTEDT (8:8)
                                               TO  OCR3-FTNJANUM
                       ELSE
                           MOVE   ZERO         TO  OCR3-FTNJANUM
                       END-IF
                   ELSE
                       MOVE   ZERO         TO  OCR3-FTNJANUM
                   END-IF
           END-EVALUATE
      *    公費２受給者番号
           EVALUATE    RECE02-2006-JKYSNUMKBN  (IDB)
               WHEN    SPACE
                   CONTINUE
               WHEN    "01"
                   MOVE  ZERO               TO      OCR3-JKYSNUM
               WHEN    OTHER
                   INITIALIZE                       ORCSNUMAREA
                   MOVE    RECE02-2006-JKYSNUM (IDB)
                                            TO      SNUM-INX
                   CALL    "ORCSNUM"        USING   ORCSNUMAREA
                   IF      SNUM-RC          =       ZERO
                       MOVE   SNUM-OUTNUM
                                        TO      OCR3-JKYSNUM
                   ELSE
                       MOVE   ZERO      TO      OCR3-JKYSNUM
                   END-IF
           END-EVALUATE
      *
           PERFORM 33011-OCR1-HENSYU-SEC
           PERFORM 33012-OCR2-HENSYU-SEC
           PERFORM 33013-OCR3-HENSYU-SEC
           .
       3302-OCR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<剤>処理
      *****************************************************************
       340-HC101-ZAI-HEN-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-LINE
      *
           IF    ( RECE02-TEISYOTOKU  NOT =  SPACE )
           AND   ( RECE02-ZAITAKU         =   "1"  )
      *        摘要欄に病名が表示されている場合、１行あける
               IF  WRK-LINETEK             >   ZERO
      *
                   IF  WRK-LINETEK          >=   WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   END-IF    
                   MOVE    1           TO  FLG-KUGIRI
                   PERFORM 34110-KUGIRI-LINE-SEC
                   MOVE    ZERO        TO  FLG-KUGIRI
                   MOVE    1           TO  FLG-LINE   
               END-IF
      *
               IF  WRK-LINETEK           >=  WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF
               ADD     1               TO  WRK-LINETEK
               IF      CNT-PAGE        =   1   
                   EVALUATE    RECE02-TEISYOTOKU
                       WHEN    "1"
                       WHEN    "4"
                           MOVE    "低所得１"
                                       TO  HC06-TEKIYO(WRK-LINETEK)
                       WHEN    "2"
                           MOVE    "低所得２"
                                       TO  HC06-TEKIYO(WRK-LINETEK)
                   END-EVALUATE        
               ELSE
                   EVALUATE    SYS-2005-NEXTPRTKBN-G
                       WHEN    "0"   
                           EVALUATE    RECE02-TEISYOTOKU
                               WHEN    "1"
                               WHEN    "4"
                                   MOVE    "低所得１"
                                       TO  HC06-TEKIYO(WRK-LINETEK)
                               WHEN    "2"
                                   MOVE    "低所得２"
                                       TO  HC06-TEKIYO(WRK-LINETEK)
                           END-EVALUATE        
                       WHEN    "1"   
                           EVALUATE    RECE02-TEISYOTOKU
                               WHEN    "1"
                               WHEN    "4"
                                   MOVE    "低所得１"
                                       TO  HC061-TEKIYO(WRK-LINETEK)
                               WHEN    "2"
                                   MOVE    "低所得２"
                                       TO  HC061-TEKIYO(WRK-LINETEK)
                           END-EVALUATE        
                       WHEN    "2"   
                           EVALUATE    RECE02-TEISYOTOKU
                               WHEN    "1"
                               WHEN    "4"
                                   MOVE    "低所得１"
                                       TO  HC062-TEKIYO(WRK-LINETEK)
                               WHEN    "2"
                                   MOVE    "低所得２"
                                       TO  HC062-TEKIYO(WRK-LINETEK)
                           END-EVALUATE        
                       WHEN    "3"   
                           EVALUATE    RECE02-TEISYOTOKU
                               WHEN    "1"
                               WHEN    "4"
                                   MOVE    "低所得１"
                                       TO  HC063-TEKIYO(WRK-LINETEK)
                               WHEN    "2"
                                   MOVE    "低所得２"
                                       TO  HC063-TEKIYO(WRK-LINETEK)
                           END-EVALUATE        
                   END-EVALUATE
               END-IF    
      *
               MOVE    ZERO            TO  FLG-LINE
           END-IF
      *         
           MOVE    ZERO                TO  WRK-R061-ZAINUM-O
           MOVE    SPACE               TO  WRK-R061-SRYKBN-O
           MOVE    ZERO                TO  WRK-R061-KOHBUNRI-O
           MOVE    ZERO                TO  CNT-RECE061
           MOVE    ZERO                TO  CNT-ZAIMEISAISU
      *
      *    パラメータ０６ファイルより編集
           INITIALIZE                      RECE061-REC
           MOVE    RECE02-KEY          TO  RECE061-KEY1
      *
           START   RECE061-FILE
               KEY IS    >=   RECE061-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE061
               NOT INVALID
                   PERFORM 990-RECE061-NEXT-SEC
           END-START
      *
           PERFORM
               UNTIL  ( FLG-RECE061          =   1            )
               OR     ( RECE02-KEY       NOT =   RECE061-KEY1 )
      *
      *        適用欄に剤の記載が必要なとき区切り線がまだ記載されていないとき
               IF      FLG-LINE            =   ZERO
      *            摘要欄に記載がある場合、１行あける
                   IF  WRK-LINETEK             >   ZERO
      *
                       IF  WRK-LINETEK          >=   WRK-TEKIYOU-LINE
                           PERFORM 34120-PAGE-SEC
                       END-IF    
                       MOVE    1           TO  FLG-KUGIRI
                       PERFORM 34110-KUGIRI-LINE-SEC
                       MOVE    ZERO        TO  FLG-KUGIRI
                   END-IF
                   MOVE    1           TO  FLG-LINE   
               END-IF
      *
               MOVE    ZERO                  TO  FLG-KUGIRI
               IF  RECE061-ZAINUM        NOT =   WRK-R061-ZAINUM-O
                   MOVE    ZERO              TO  CNT-ZAIMEISAISU
               END-IF
               ADD     1                     TO  CNT-ZAIMEISAISU
      *
      *        レセプト０６ファイル編集処理
               PERFORM 3410-RECE061-HENSYU-SEC
      *
               MOVE    RECE061-ZAINUM        TO  WRK-R061-ZAINUM-O
               MOVE    RECE061-SRYKBN        TO  WRK-R061-SRYKBN-O
               MOVE    RECE061-KOHBUNRI      TO  WRK-R061-KOHBUNRI-O
      *
               PERFORM 990-RECE061-NEXT-SEC
           END-PERFORM
      *
           .
       340-HC101-ZAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    レセプト０６ファイル編集処理
      *****************************************************************
       3410-RECE061-HENSYU-SEC           SECTION.
      *
           MOVE    ZERO                  TO   FLG-KUGIRI
     *
           IF  CNT-ZAIMEISAISU           =   1
               IF  CNT-RECE061           >   1
      *            公費分離区分が変更している場合、公費区切り線分を追加
                   IF  RECE061-KOHBUNRI  NOT =   WRK-R061-KOHBUNRI-O
                       MOVE    2             TO  FLG-KUGIRI
                   ELSE
      *                診療区分が変更している場合、診療区切り線分を追加
                       IF  RECE061-SRYKBN    NOT =   WRK-R061-SRYKBN-O
                           MOVE    1             TO  FLG-KUGIRI
                       END-IF
                   END-IF
               END-IF
           END-IF    
      *
     
           IF      WRK-LINETEK          >=   WRK-TEKIYOU-LINE
               PERFORM 34120-PAGE-SEC
      *
      *        区切り線（改頁後には診療区切り線は表示しない）
               IF      FLG-KUGIRI        =   2
                   PERFORM 34110-KUGIRI-LINE-SEC
               END-IF
           ELSE
      *        区切り線
               IF      FLG-KUGIRI    NOT =   ZERO
                   PERFORM 34110-KUGIRI-LINE-SEC
               END-IF
           END-IF    
      *
      *    診療区分
           IF  WRK-LINETEK          >=   WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
           END-IF
      *              
           ADD     1                 TO  WRK-LINETEK
      *
           MOVE    ZERO                  TO  FLG-SRYKBNPRT
           IF      CNT-PAGE          >   1 
               EVALUATE    SYS-2005-NEXTPRTKBN-G
                   WHEN    ZERO
                       CONTINUE
                   WHEN    "1"
                       IF      WRK-LINETEK     =   36  OR  71  OR  106
                           MOVE    1               TO  FLG-SRYKBNPRT
                       END-IF
                   WHEN    "2"
                       IF      WRK-LINETEK     =   56
                           MOVE    1               TO  FLG-SRYKBNPRT
                       END-IF
                   WHEN    "3"
                       IF      WRK-LINETEK     =   70
                           MOVE    1               TO  FLG-SRYKBNPRT
                       END-IF
               END-EVALUATE
           END-IF
      *
           IF    ( FLG-KUGIRI        =   1   OR  2 )
           OR    ( WRK-LINETEK       =   1 )
           OR    ( CNT-RECE061       =   1 )
           OR    ( FLG-SRYKBNPRT     =   1 ) 
               IF      CNT-PAGE        =   1   
                   MOVE    RECE061-SRYKBN  TO  HC06-SRYKBN
                                                       (WRK-LINETEK)
               ELSE                                    
                   EVALUATE    SYS-2005-NEXTPRTKBN-G
                       WHEN    ZERO
                           MOVE    RECE061-SRYKBN  TO  HC06-SRYKBN
                                                       (WRK-LINETEK)
                       WHEN    "1"
                           MOVE    RECE061-SRYKBN  TO  HC061-SRYKBN
                                                       (WRK-LINETEK)
                       WHEN    "2"
                           MOVE    RECE061-SRYKBN  TO  HC062-SRYKBN
                                                       (WRK-LINETEK)
                       WHEN    "3"
                           MOVE    RECE061-SRYKBN  TO  HC063-SRYKBN
                                                       (WRK-LINETEK)
                   END-EVALUATE
               END-IF
           END-IF                                    
      *
      *    明細
           INSPECT RECE061-TEKIYO-INF
                           REPLACING    ALL   "  "   BY   "　"
           IF      CNT-PAGE        =   1   
               MOVE    RECE061-TEKIYO-INF  TO  HC06-TEKIYO (WRK-LINETEK)
           ELSE    
               EVALUATE    SYS-2005-NEXTPRTKBN-G
                   WHEN    ZERO
                       MOVE    RECE061-TEKIYO-INF
                                           TO  HC06-TEKIYO (WRK-LINETEK)
                   WHEN    "1"
                       MOVE    RECE061-TEKIYO-INF
                                           TO  HC061-TEKIYO(WRK-LINETEK)
                   WHEN    "2"
                       MOVE    RECE061-TEKIYO-INF
                                           TO  HC062-TEKIYO(WRK-LINETEK)
                   WHEN    "3"
                       MOVE    RECE061-TEKIYO-INF
                                           TO  HC063-TEKIYO(WRK-LINETEK)
               END-EVALUATE
           END-IF  
      *
           .
       3410-RECE061-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    区切り線　編集処理
      *****************************************************************
       34110-KUGIRI-LINE-SEC       SECTION.
      *
           ADD     1               TO  WRK-LINETEK
      *
           EVALUATE    FLG-KUGIRI
           WHEN    2
               PERFORM 34110-KOHNAME-SET-SEC
               IF      CNT-PAGE        =   1   
                   MOVE    WRK-KUGIRI6 TO  HC06-TEKIYO  (WRK-LINETEK)
               ELSE    
                   EVALUATE    SYS-2005-NEXTPRTKBN-G
                       WHEN    ZERO
                           MOVE    WRK-KUGIRI6
                                       TO  HC06-TEKIYO  (WRK-LINETEK)
                       WHEN    "1"
                           MOVE    WRK-KUGIRI6 
                                       TO  HC061-TEKIYO (WRK-LINETEK)
                       WHEN    "2"
                           MOVE    WRK-KUGIRI6 
                                       TO  HC062-TEKIYO (WRK-LINETEK)
                       WHEN    "3"
                           MOVE    WRK-KUGIRI6 
                                       TO  HC063-TEKIYO (WRK-LINETEK)
                   END-EVALUATE
               END-IF    
           WHEN    1
               IF      CNT-PAGE        =   1   
                   MOVE    WRK-KUGIRI1 TO  HC06-TEKIYO  (WRK-LINETEK)
               ELSE    
                   EVALUATE    SYS-2005-NEXTPRTKBN-G
                       WHEN    ZERO
                           MOVE    WRK-KUGIRI1
                                       TO  HC06-TEKIYO  (WRK-LINETEK)
                       WHEN    "1"
                           MOVE    WRK-KUGIRI1 
                                       TO  HC061-TEKIYO (WRK-LINETEK)
                       WHEN    "2"
                           MOVE    WRK-KUGIRI1 
                                       TO  HC062-TEKIYO (WRK-LINETEK)
                       WHEN    "3"
                           MOVE    WRK-KUGIRI1 
                                       TO  HC063-TEKIYO (WRK-LINETEK)
                   END-EVALUATE
               END-IF    
           END-EVALUATE
      *
           .
       34110-KUGIRI-LINE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    改ページ処理
      *****************************************************************
       34120-PAGE-SEC       SECTION.
      *
      *    帳票印刷処理
           PERFORM 390-PRINT-OUT-SEC
      *     
      *    帳票編集<ヘッダー部>処理
           ADD     1             TO  CNT-PAGE
           EVALUATE    SYS-2005-NEXTPRTKBN-G
               WHEN    "0"
                   PERFORM 310-HC101-HEAD-HEN-SEC
               WHEN    OTHER
                   PERFORM 310-HCN061-HEAD-HEN-SEC
           END-EVALUATE
           MOVE    ZERO          TO  WRK-LINETEK
      *
      *    続紙の最大行をセット     
           IF      CNT-PAGE        =   1   
               MOVE    WRK-TEKIYOU1-LINE TO  WRK-TEKIYOU-LINE
           ELSE    
               EVALUATE    SYS-2005-NEXTPRTKBN-G
                   WHEN    "0"
                       MOVE    WRK-TEKIYOU1-LINE TO  WRK-TEKIYOU-LINE
                   WHEN    "1"   
                       MOVE    WRK-TEKIYOU2-LINE TO  WRK-TEKIYOU-LINE
                   WHEN    "2"   
                       MOVE    WRK-TEKIYOU3-LINE TO  WRK-TEKIYOU-LINE
                   WHEN    "3"   
                       MOVE    WRK-TEKIYOU4-LINE TO  WRK-TEKIYOU-LINE
               END-EVALUATE
           END-IF    
      *
           .
       34120-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    区切り線　公費名セット処理
      *****************************************************************
       34110-KOHNAME-SET-SEC          SECTION.
      *
           IF      RECE02-KOHBUNRICHIKAN  =   ZERO
              MOVE     SPACE             TO  WRK-KUGIRI6
              MOVE     SPACE             TO  WRK-KUGIRI2-KOHNAME
              MOVE     RECE02-KOHNUM (RECE061-KOHBUNRI)
                                         TO  WRK-HENSYU-KOHNUM
              PERFORM  341101-HKNNUM-KENSAKU-SEC
              MOVE     HKN-TANSEIDONAME  TO  WRK-KUGIRI2-KOHNAME
              INSPECT  WRK-KUGIRI2-KOHNAME
                       REPLACING    ALL   "  "   BY   "　"
              MOVE     WRK-KUGIRI2       TO  WRK-KUGIRI6
           ELSE
              MOVE     SPACE             TO  WRK-KUGIRI6
              MOVE     SPACE             TO  WRK-KUGIRI4-KOHNAME
              MOVE     SPACE             TO  WRK-KUGIRI5-KOHNAME
              MOVE     SPACE             TO  WRK-HENSYU-KOHNAME
              MOVE     ZERO              TO  IDX-KUGIRI2
              MOVE     ZERO              TO  IDX-KUGIRI4
              PERFORM  VARYING  IDX-KUGIRI1  FROM   1   BY   1
                        UNTIL   IDX-KUGIRI1   >    10
                         OR     RECE02-HKNCOMBI(IDX-KUGIRI1)  =  ZERO
                 IF  RECE061-HKNCOMBI  =  RECE02-HKNCOMBI(IDX-KUGIRI1)
                    MOVE    IDX-KUGIRI1   TO   IDX-KUGIRI2
                    MOVE    10            TO   IDX-KUGIRI1
                 END-IF
              END-PERFORM
              IF     IDX-KUGIRI2    NOT =    ZERO
                MOVE     ZERO     TO   FLG-MEISYO-HENSYU
                PERFORM  VARYING  IDX-KUGIRI1  FROM   1   BY   1
                          UNTIL   IDX-KUGIRI1   >     4
                 IF  RECE02-KOHNUMHC(IDX-KUGIRI2 IDX-KUGIRI1)
                                                    NOT =  SPACE
                    PERFORM  VARYING  IDX-KUGIRI3  FROM   1   BY   1
                              UNTIL   IDX-KUGIRI3   >     4
                               OR     RECE02-KOHNUM(IDX-KUGIRI3)
                                                        =  SPACE
                      IF  (RECE02-KOHNUMHC(IDX-KUGIRI2 IDX-KUGIRI1)
                                  =   RECE02-KOHNUM(IDX-KUGIRI3))
                      AND (RECE02-KOHIDHC (IDX-KUGIRI2 IDX-KUGIRI1)
                                  =   RECE02-KOHID (IDX-KUGIRI3))
                         MOVE     RECE02-KOHNUM(IDX-KUGIRI3)
                                         TO  WRK-HENSYU-KOHNUM
                         PERFORM  341101-HKNNUM-KENSAKU-SEC
                         IF   RECE02-SEIHOFLG  =  "1"
                           IF  (RECE02-HKNNUMHC(IDX-KUGIRI2)
                                                 NOT =  SPACE)
                           AND (RECE02-KOHNUMHC(IDX-KUGIRI2 1)
                                                 NOT =  SPACE)
                           AND (RECE02-KOHNUMHC(IDX-KUGIRI2 2)
                                                     =  SPACE)
                           AND (RECE02-KOHNUMHC(IDX-KUGIRI2 3)
                                                     =  SPACE)
                           AND (RECE02-KOHNUMHC(IDX-KUGIRI2 4)
                                                     =  SPACE)
                             MOVE   1      TO   FLG-MEISYO-HENSYU
                           END-IF
                           IF  (RECE02-HKNNUMHC(IDX-KUGIRI2)
                                                     =  SPACE)
                           AND (RECE02-KOHNUMHC(IDX-KUGIRI2 1)
                                                 NOT =  SPACE)
                           AND (RECE02-KOHNUMHC(IDX-KUGIRI2 2)
                                                     =  SPACE)
                           AND (RECE02-KOHNUMHC(IDX-KUGIRI2 3)
                                                     =  SPACE)
                           AND (RECE02-KOHNUMHC(IDX-KUGIRI2 4)
                                                     =  SPACE)
                             MOVE   2      TO   FLG-MEISYO-HENSYU
                           END-IF
                         END-IF
                         ADD        1      TO   IDX-KUGIRI4
                         EVALUATE   IDX-KUGIRI4
                           WHEN         1
                              MOVE  HKN-TANSEIDONAME
                                         TO WRK-HENSYU-KOHNAME1
                              EVALUATE  FLG-MEISYO-HENSYU
                                WHEN       1
                                  MOVE  "（併用分）"
                                         TO WRK-HENSYU-KOHNAME2
      ***                       WHEN       2
      ***                         MOVE  "（単独分）"
      ***                                TO WRK-HENSYU-KOHNAME2
                              END-EVALUATE
                           WHEN         2
                              MOVE  "＋" TO WRK-HENSYU-KOHNAME2-PLUS
                              MOVE  HKN-TANSEIDONAME
                                         TO WRK-HENSYU-KOHNAME2
                           WHEN         3
                              MOVE  "＋" TO WRK-HENSYU-KOHNAME3-PLUS
                              MOVE  HKN-TANSEIDONAME
                                         TO WRK-HENSYU-KOHNAME3
                           WHEN         4
                              CONTINUE
                         END-EVALUATE
                         MOVE    4       TO IDX-KUGIRI3
                      END-IF
                    END-PERFORM
                 END-IF
                END-PERFORM
              END-IF
              EVALUATE   IDX-KUGIRI4
                WHEN         0
                WHEN         1
                WHEN         2
                  STRING  WRK-HENSYU-KOHNAME1      DELIMITED  BY  SPACE
                          WRK-HENSYU-KOHNAME2-PLUS DELIMITED  BY  SPACE
                          WRK-HENSYU-KOHNAME2      DELIMITED  BY  SPACE
                          INTO         WRK-KUGIRI4-KOHNAME
                  END-STRING
                  INSPECT  WRK-KUGIRI4-KOHNAME
                       REPLACING    ALL   "  "   BY   "　"
                  MOVE     WRK-KUGIRI4     TO   WRK-KUGIRI6
                WHEN         3
                WHEN         4
                  STRING  WRK-HENSYU-KOHNAME1      DELIMITED  BY  SPACE
                          WRK-HENSYU-KOHNAME2-PLUS DELIMITED  BY  SPACE
                          WRK-HENSYU-KOHNAME2      DELIMITED  BY  SPACE
                          WRK-HENSYU-KOHNAME3-PLUS DELIMITED  BY  SPACE
                          WRK-HENSYU-KOHNAME3      DELIMITED  BY  SPACE
                          INTO         WRK-KUGIRI5-KOHNAME
                  END-STRING
                  INSPECT  WRK-KUGIRI5-KOHNAME
                       REPLACING    ALL   "  "   BY   "　"
                  MOVE     WRK-KUGIRI5     TO   WRK-KUGIRI6
              END-EVALUATE
           END-IF
      *
           .
       34110-KOHNAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスタ検索
      *****************************************************************
       341101-HKNNUM-KENSAKU-SEC          SECTION.
      *
           MOVE    SPACE              TO  HKNNUM-REC
           MOVE    SPA-HOSPNUM        TO  HKN-HOSPNUM
           MOVE    WRK-HENSYU-KOHNUM  TO  HKN-HKNNUM
           MOVE    WRK-SRYYMD         TO  HKN-TEKSTYMD
                                          HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC         TO  MCPDATA-REC
           MOVE   "tbl_hknnum"     TO  MCP-TABLE
           MOVE   "key5"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE   "tbl_hknnum"     TO  MCP-TABLE
               MOVE   "key5"           TO  MCP-PATHNAME
               PERFORM 910-HKNMST-NEXT-SEC
           ELSE
               MOVE    1              TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"    TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       341101-HKNNUM-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<適用欄下段コメント>処理
      *****************************************************************
       350-HC101-HOSOKU-HEN-SEC      SECTION.
      *
      *    適用欄下段コメント（レセプト特記事項編集情報）
           IF    ( RECE02-2006-TEKIYOU-COMMENT2 (1)    =   SPACE )
           AND   ( RECE02-2006-TEKIYOU-COMMENT2 (2)    =   SPACE )
           AND   ( RECE02-2006-TEKIYOU-COMMENT2 (3)    =   SPACE )
           AND   ( RECE02-2006-TEKIYOU-COMMENT2 (4)    =   SPACE )
           AND   ( RECE02-2006-TEKIYOU-COMMENT2 (5)    =   SPACE )
               CONTINUE
           ELSE
               STRING  RECE02-2006-TEKIYOU-COMMENT2 (1)
                                           DELIMITED   BY  SPACE
                       RECE02-2006-TEKIYOU-COMMENT2 (2)
                                           DELIMITED   BY  SPACE
                       RECE02-2006-TEKIYOU-COMMENT2 (3)
                                           DELIMITED   BY  SPACE
                       RECE02-2006-TEKIYOU-COMMENT2 (4)
                                           DELIMITED   BY  SPACE
                       RECE02-2006-TEKIYOU-COMMENT2 (5)
                                           DELIMITED   BY  SPACE
                                           INTO    HC06-BODY
               END-STRING        
           END-IF
      *
      *    県単用項目
           IF    ( FLG-RECE11           =   ZERO  )
           AND   ( RECE11-HC06-BODY NOT =   SPACE )    
               INSPECT RECE11-HC06-BODY     REPLACING   ALL "  "
                                                        BY  "　" 
               MOVE    RECE11-HC06-BODY     TO  HC06-BODY
           END-IF
      *
      *    県単用項目（適用欄下段欄外コメント）
           IF      FLG-RECE11           =   ZERO
               INSPECT RECE11-HC06-FOOT-G   REPLACING   ALL "  "
                                                        BY  "　" 
               MOVE    RECE11-HC06-FOOT-G   TO  HC06-FOOT-G
           END-IF
      *
           .
       350-HC101-HOSOKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<コメント>処理
      *****************************************************************
       360-HC101-COMMENT-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-RECECOM
           MOVE    ZERO                TO  FLG-RECECOM-SJKBN
           MOVE    SPACE               TO  WRK-RECECOM-NAME 
                                           WRK-RECECOM-SJKBN
      *     
      *    適用欄コメント     
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key7"              TO  WRK-MCP-PATHNAME
           PERFORM 3601-RECECOM-HENSYU-SEC
      *     
      *    症状詳記コメント（０１〜０６）
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key8"              TO  WRK-MCP-PATHNAME
           PERFORM 3601-RECECOM-HENSYU-SEC
      *     
      *    症状詳記コメント（その他）    
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key9"              TO  WRK-MCP-PATHNAME
           PERFORM 3601-RECECOM-HENSYU-SEC
      *     
      *    症状詳記コメント（５０、５１、５２、９０）     
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key23"             TO  WRK-MCP-PATHNAME
           PERFORM 3601-RECECOM-HENSYU-SEC
           .
       360-HC101-COMMENT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセコメント読み込み処理
      *****************************************************************
       3601-RECECOM-HENSYU-SEC    SECTION.
      *
           INITIALIZE                      RECECOM-REC
           MOVE    RECE02-HOSPNUM      TO  RECECOM-HOSPNUM
           MOVE    RECE02-SRYYM        TO  RECECOM-SRYYM
           MOVE    RECE02-PTID         TO  RECECOM-PTID
           MOVE    RECE02-NYUGAIKBN    TO  RECECOM-NYUGAIKBN
      * 
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-RECECOM-READ-SEC
           ELSE
               INITIALIZE                  RECECOM-REC
               MOVE    1               TO  FLG-RECECOM
           END-IF
      *
           PERFORM             UNTIL   FLG-RECECOM   =   1
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       10
                       OR      RECE02-HKNCOMBI (IDY) =   ZERO
                   IF      RECECOM-HKNCOMBI    =   RECE02-HKNCOMBI(IDY)
                       PERFORM 3601-COMMENT-HENSYU-SEC                 
                       MOVE    10                  TO  IDY
                   END-IF
               END-PERFORM
      *                 
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-RECECOM-READ-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       3601-RECECOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントの改行コード削除処理
      *****************************************************************
       3601-COMMENT-HENSYU-SEC    SECTION.
      *
           MOVE    RECECOM-COMMENT TO  WRK-COMM
           MOVE    ZERO            TO  WRK-RECEHEN                      
           MOVE    ZERO            TO  FLG-RECECOM-SJKBN
      *
      *    症状詳記区分の見出しを記載
           IF      RECECOM-SJKBN   =   "07"    OR  "08"    OR  "09"
               MOVE    "07"            TO  RECECOM-SJKBN
           END-IF 
           IF      RECECOM-SJKBN   NOT =   WRK-RECECOM-SJKBN
               MOVE   RECECOM-SJKBN        TO  WRK-RECECOM-SJKBN
      *
               MOVE    SPACE           TO  WRK-RECECOM-NAME
               EVALUATE    WRK-RECECOM-SJKBN
                   WHEN    "01"    THRU    "07"
                       MOVE    1               TO  FLG-RECECOM-SJKBN
                       MOVE    RECECOM-SJKBN   TO  WRK-SJKBN-X
                       MOVE    TBL-RECECOM-NAME (WRK-SJKBN)
                                               TO  WRK-RECECOM-NAME
                   WHEN    "50"
                       MOVE    1               TO  FLG-RECECOM-SJKBN
                       IF      RECE02-SRYYM    <   201412
                           MOVE    TBL-RECECOM-NAME (8)
                                                   TO  WRK-RECECOM-NAME
                       ELSE
                           MOVE    TBL-RECECOM-NAME (12)
                                                   TO  WRK-RECECOM-NAME
                       END-IF
                   WHEN    "51"
                       MOVE    1               TO  FLG-RECECOM-SJKBN
                       MOVE    TBL-RECECOM-NAME (9)
                                               TO  WRK-RECECOM-NAME
                   WHEN    "52"
                       MOVE    1               TO  FLG-RECECOM-SJKBN
                       MOVE    TBL-RECECOM-NAME (10)
                                               TO  WRK-RECECOM-NAME
                   WHEN    "90"
                       MOVE    1               TO  FLG-RECECOM-SJKBN
                       MOVE    TBL-RECECOM-NAME (11)
                                               TO  WRK-RECECOM-NAME
               END-EVALUATE
           END-IF
      *
           PERFORM     UNTIL       WRK-RECEHEN  =   1      
      *
               MOVE    SPACE           TO  WRK-COMMENT-G
                                           WRK-COMM1
               MOVE    ZERO            TO  WRK-COL
      *
               IF      WRK-COMM (1:1)  =   WRK-CONS-RETURN
                   MOVE    WRK-COMM (2:)   TO  WRK-COMM1
               ELSE             
                   INSPECT WRK-COMM    TALLYING    WRK-COL
                                               FOR ALL WRK-CONS-RETURN
                   IF      WRK-COL         >   ZERO
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >
                                           WRK-CONS-RECECOM-LENGTH-MAX
                           IF      WRK-COMM(IDX:1) =   WRK-CONS-RETURN
                               COMPUTE IDY1    =   IDX     -   1
                               COMPUTE IDY2    =   IDX     +   1   
                               MOVE    WRK-COMM (1:IDY1)
                                               TO  WRK-COMMENT-G
                               MOVE    WRK-COMM (IDY2:)
                                               TO  WRK-COMM1
                               MOVE    WRK-CONS-RECECOM-LENGTH-MAX
                                               TO  IDX
                           END-IF
                       END-PERFORM                                                                                  
                   ELSE
                       MOVE    WRK-COMM        TO  WRK-COMMENT-G
                       MOVE    1               TO  WRK-RECEHEN
                   END-IF
      *                                       
                   IF      WRK-COMMENT-G   >   SPACE
      *                摘要欄に１行あける
                     IF      WRK-RECECOM         =   ZERO
                       MOVE    1               TO  WRK-RECECOM
                       IF  WRK-LINETEK         >   ZERO
                           IF      WRK-LINETEK >=  WRK-TEKIYOU-LINE
                               PERFORM 34120-PAGE-SEC
                           ELSE
                               MOVE    1           TO  FLG-KUGIRI
                               PERFORM 34110-KUGIRI-LINE-SEC
                               MOVE    ZERO        TO  FLG-KUGIRI   
                           END-IF
                       END-IF
                     END-IF
      *              症状詳記区分の見出しを記載
                     IF      FLG-RECECOM-SJKBN   =   1
                       IF      WRK-LINETEK      >=
                                                WRK-TEKIYOU-LINE
                           PERFORM 34120-PAGE-SEC
                       END-IF
                       ADD     1               TO  WRK-LINETEK
                       IF      CNT-PAGE        =   1   
                           MOVE    WRK-RECECOM-NAME
                                       TO  HC06-TEKIYO (WRK-LINETEK)
                       ELSE                
                           EVALUATE    SYS-2005-NEXTPRTKBN-G
                               WHEN    "0"
                                   MOVE    WRK-RECECOM-NAME
                                       TO  HC06-TEKIYO (WRK-LINETEK)
                               WHEN    "1"
                                   MOVE    WRK-RECECOM-NAME
                                       TO  HC061-TEKIYO(WRK-LINETEK)
                               WHEN    "2"
                                   MOVE    WRK-RECECOM-NAME
                                       TO  HC062-TEKIYO(WRK-LINETEK)
                               WHEN    "3"
                                   MOVE    WRK-RECECOM-NAME
                                       TO  HC063-TEKIYO(WRK-LINETEK)
                           END-EVALUATE
                       END-IF
                       MOVE    ZERO            TO  FLG-RECECOM-SJKBN
                     END-IF            
      *                 
                     PERFORM VARYING IDW FROM    1   BY  1
                             UNTIL   IDW >       WRK-CONS-RECECOM-MAX
                           OR      WRK-COMMENT (IDW)   =   SPACE
                       IF      WRK-LINETEK      >=
                                                WRK-TEKIYOU-LINE
                           PERFORM 34120-PAGE-SEC
                       END-IF
                       ADD     1               TO  WRK-LINETEK
                       IF      CNT-PAGE        =   1   
                           MOVE    WRK-COMMENT (IDW)
                                       TO  HC06-TEKIYO (WRK-LINETEK)
                       ELSE                
                           EVALUATE    SYS-2005-NEXTPRTKBN-G
                               WHEN    "0"
                                   MOVE    WRK-COMMENT (IDW)
                                       TO  HC06-TEKIYO (WRK-LINETEK)
                               WHEN    "1"
                                   MOVE    WRK-COMMENT (IDW)
                                       TO  HC061-TEKIYO(WRK-LINETEK)
                               WHEN    "2"
                                   MOVE    WRK-COMMENT (IDW)
                                       TO  HC062-TEKIYO(WRK-LINETEK)
                               WHEN    "3"
                                   MOVE    WRK-COMMENT (IDW)
                                       TO  HC063-TEKIYO(WRK-LINETEK)
                           END-EVALUATE
                       END-IF                
                     END-PERFORM
                 END-IF
               END-IF  
               MOVE    WRK-COMM1       TO  WRK-COMM
           END-PERFORM   
      *
           .
       3601-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<保険履歴情報>処理
      *****************************************************************
       370-HC101-HKN-RIREKI-HEN-SEC      SECTION.
      *
      *    公費単独以外、心身喪失単独は保険履歴の記載をしない
           IF    ( RECE02-HKNNUM       =   SPACE )
           AND   ( RECE02-KOHNUM (1)
                                   NOT =   SPACE )
           AND   ( RECE02-KOHNUM (2)
                                   NOT =   SPACE )
               GO  TO  370-HC101-HKN-RIREKI-HEN-EXT
           END-IF
           IF    ( RECE02-HKNNUM       =   SPACE )
           AND   ( RECE02-KOHNUM (1)
                                       =   "030" )
           AND   ( RECE02-KOHNUM (2)
                                       =   SPACE )
               GO  TO  370-HC101-HKN-RIREKI-HEN-EXT
           END-IF
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       WRK-RIREKI-MAX  
      *        摘要欄に１行あける
               IF      IDXX                =   1
                   IF  WRK-LINETEK             >   ZERO
                       IF      WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                           PERFORM 34120-PAGE-SEC
                       ELSE
                           MOVE    1               TO  FLG-KUGIRI
                           PERFORM 34110-KUGIRI-LINE-SEC
                           MOVE    ZERO            TO  FLG-KUGIRI
                       END-IF
                   END-IF
               END-IF
      *
      *
               IF      WRK-LINETEK        >=  WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF
               ADD     1               TO  WRK-LINETEK
               INSPECT WRK-HKN-RIREKI-TBL (IDXX)   REPLACING   ALL "  "
                                                               BY  "　"
               IF      CNT-PAGE        =   1   
                   MOVE    WRK-HKN-RIREKI-TBL (IDXX)
                                       TO  HC06-TEKIYO (WRK-LINETEK)
               ELSE                        
                   EVALUATE    SYS-2005-NEXTPRTKBN-G
                       WHEN    "0"   
                           MOVE    WRK-HKN-RIREKI-TBL (IDXX)
                                       TO  HC06-TEKIYO (WRK-LINETEK)
                       WHEN    "1"   
                           MOVE    WRK-HKN-RIREKI-TBL (IDXX)
                                       TO  HC061-TEKIYO(WRK-LINETEK)
                       WHEN    "2"   
                           MOVE    WRK-HKN-RIREKI-TBL (IDXX)
                                       TO  HC062-TEKIYO(WRK-LINETEK)
                       WHEN    "3"   
                           MOVE    WRK-HKN-RIREKI-TBL (IDXX)
                                       TO  HC063-TEKIYO(WRK-LINETEK)
                   END-EVALUATE
               END-IF                        
           END-PERFORM
      *
           .
       370-HC101-HKN-RIREKI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<公費３、公費４、適用欄上段コメント>処理
      *****************************************************************
       380-HC101-TEKIYOU-COM-HEN-SEC      SECTION.
      *
      *    公費３、公費４
           IF    ( RECE02-KOHNUM (3)   =   SPACE )
           AND   ( RECE02-KOHNUM (4)   =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE              WRK-KOH-HENSYU-AREA
      * 
               PERFORM VARYING IDZZ    FROM    1   BY  1
                       UNTIL   IDZZ    >       2
      *
                 MOVE    ZERO                  TO  IDY
                 EVALUATE    IDZZ
                     WHEN    1
                         IF      IDC             >   ZERO
                             MOVE    IDC             TO  IDY
                         END-IF
                     WHEN    2
                         IF      IDD             >   ZERO
                             MOVE    IDD             TO  IDY
                         END-IF
                 END-EVALUATE
      *
                 IF    ( IDY                       >   ZERO  )
                 AND   ( RECE02-KOHNUM (IDY)   NOT =   SPACE )
                   IF      RECE02-KOHFTNJANUM (IDY)
                                           NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE02-KOHFTNJANUM (IDY)
                                               TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF    ( KANACHK-RC      =   ZERO )
                       AND   ( KANACHK-MAX     >   ZERO )
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-KOH-FTNJANUM
                       END-IF
                   ELSE
                       MOVE    SPACE           TO  WRK-KOH-FTNJANUM     
      *                災害対応     
                       IF      RECE02-RJNGAIKOHFTN (1) =   6   OR  7
                                                       OR  9   OR  11
                           MOVE    RECE02-KOHHBTNUM (IDY)
                                               TO  KANACHK-MAE-INPUT
                           CALL    "ORCSKANACHK"
                                               USING   ORCSKANACHKAREA
                           IF    ( KANACHK-RC      =   ZERO )
                           AND   ( KANACHK-MAX     >   ZERO )
                               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                                   TO  WRK-KOH-FTNJANUM
                           END-IF
                       END-IF 
                   END-IF
                   IF      RECE02-KOHJKYSNUM (IDY)
                                           NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE02-KOHJKYSNUM (IDY)
                                               TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF    ( KANACHK-RC      =   ZERO )
                       AND   ( KANACHK-MAX     >   ZERO )
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-KOH-JKYSNUM
                       END-IF    
                   ELSE
                       MOVE    SPACE           TO  WRK-KOH-JKYSNUM    
                   END-IF
                   COMPUTE IDXY    =   IDY     +   1
                   MOVE    RECE02-JNISSU (IDXY)
                                           TO  WRK-JINISSU2Z
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    WRK-JINISSU2    TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF    ( KANACHK-RC      =   ZERO )
                   AND   ( KANACHK-MAX     >   ZERO )
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-KOH-JNISSU
                   ELSE
                       MOVE    SPACE       TO  WRK-KOH-JNISSU  
                   END-IF
      *
                   MOVE        SPACE       TO  WRK-KOH-SAIKEI
                   IF  ((IDY                         =   3       ) AND
                        (RECE02-SAIKEIZEROKSI3       =   1       ) AND
                        (RECE02-ZAITAKU-SAIKEI3  NOT =  999999999))
                   OR  ((IDY                         =   4       ) AND
                        (RECE02-SAIKEIZEROKSI4       =   1       ) AND
                        (RECE02-ZAITAKU-SAIKEI4  NOT =  999999999))
                     EVALUATE  IDY
                       WHEN     3
                         MOVE  RECE02-ZAITAKU-SAIKEI3  TO  WRK-SAIKEIZ
                       WHEN     4
                         MOVE  RECE02-ZAITAKU-SAIKEI4  TO  WRK-SAIKEIZ
                     END-EVALUATE
                     MOVE    ZERO          TO  IDX7
                     INITIALIZE            ORCSKANACHKAREA
                     MOVE    "2"           TO  KANACHK-SYORI
                     MOVE    WRK-SAIKEI    TO  KANACHK-MAE-INPUT
                     CALL    "ORCSKANACHK" USING   ORCSKANACHKAREA
                     IF    ( KANACHK-RC    =   ZERO )
                     AND   ( KANACHK-MAX   >   ZERO )
                       PERFORM  VARYING   IDX6  FROM   1   BY   2
                                 UNTIL    IDX6    >    KANACHK-MAX
                         IF  KANACHK-OUT-INPUT(IDX6:2)  NOT =  "　"
                             MOVE   IDX6         TO  IDX7
                             MOVE   KANACHK-MAX  TO  IDX6
                         END-IF
                       END-PERFORM
                       IF    IDX7   NOT = ZERO
                         COMPUTE  IDX6  =  KANACHK-MAX - IDX7 + 1
                         MOVE   KANACHK-OUT-INPUT(IDX7:IDX6)
                                           TO  WRK-KOH-SAIKEI
                       END-IF
                     END-IF
                   END-IF
      *
                   ADD     1               TO  WRK-KOH-HENSYU-MAX
                   EVALUATE    IDZZ
                       WHEN    1
                           MOVE    "第３公費"  TO  WRK-KOH-HENSYU-X
                                                   (WRK-KOH-HENSYU-MAX) 
                       WHEN    2
                           MOVE    "第４公費"  TO  WRK-KOH-HENSYU-X
                                                   (WRK-KOH-HENSYU-MAX)
                   END-EVALUATE
      *
                   ADD     1               TO  WRK-KOH-HENSYU-MAX
                   EVALUATE    IDZZ
                       WHEN    1
                           MOVE    "公３（"    TO  WRK-KOH-FL
                       WHEN    2
                           MOVE    "公４（"    TO  WRK-KOH-FL
                   END-EVALUATE
                   STRING  WRK-KOH-FL          DELIMITED  BY  SIZE
                           WRK-KOH-FTNJANUM    DELIMITED  BY  SPACE
                           "）"                DELIMITED  BY  SIZE
                                               INTO    WRK-KOH-HENSYU-X
                                                   (WRK-KOH-HENSYU-MAX)
                   END-STRING
      *
                   ADD     1               TO  WRK-KOH-HENSYU-MAX
                   STRING  "受　（"            DELIMITED  BY  SIZE
                           WRK-KOH-JKYSNUM     DELIMITED  BY  SPACE
                           "）"                DELIMITED  BY  SIZE
                                               INTO    WRK-KOH-HENSYU-X
                                                   (WRK-KOH-HENSYU-MAX)
                   END-STRING
      *
                   ADD     1               TO  WRK-KOH-HENSYU-MAX
                   STRING  "実　（"            DELIMITED  BY  SIZE
                           WRK-KOH-JNISSU      DELIMITED  BY  SPACE
                           "日）"              DELIMITED  BY  SIZE
                                               INTO    WRK-KOH-HENSYU-X
                                                   (WRK-KOH-HENSYU-MAX)
                   END-STRING
      *
                   IF   WRK-KOH-SAIKEI  NOT =  SPACE
                     ADD     1             TO  WRK-KOH-HENSYU-MAX
                     STRING  "公費給付対象額（"
                                               DELIMITED  BY  SIZE
                             WRK-KOH-SAIKEI    DELIMITED  BY  SPACE
                             "円）"            DELIMITED  BY  SIZE
                                               INTO    WRK-KOH-HENSYU-X
                                                   (WRK-KOH-HENSYU-MAX)
                     END-STRING
                   END-IF
                 END-IF
               END-PERFORM
      *    
      *        摘要欄に病名が記載されている場合１行あける
               IF  WRK-LINETEK             >   ZERO
                   IF      WRK-LINETEK         >=   WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   ELSE
                       ADD     1               TO  WRK-LINETEK
                   END-IF
               END-IF
      *
               PERFORM VARYING IDW     FROM    1   BY  1
                       UNTIL   IDW     >       WRK-KOH-HENSYU-MAX
                   INSPECT WRK-KOH-HENSYU-X (IDW)  REPLACING   ALL "  "
                                                               BY  "　"
      * 
                   IF  WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   END-IF    
                   ADD     1                   TO  WRK-LINETEK
      *
                   IF      CNT-PAGE        =   1   
                       MOVE    WRK-KOH-HENSYU-X (IDW)
                                       TO  HC06-TEKIYO (WRK-LINETEK)
                   ELSE                    
                       EVALUATE    SYS-2005-NEXTPRTKBN-G
                           WHEN    "0"   
                               MOVE    WRK-KOH-HENSYU-X (IDW)
                                           TO  HC06-TEKIYO (WRK-LINETEK)
                           WHEN    "1"   
                               MOVE    WRK-KOH-HENSYU-X (IDW)
                                           TO  HC061-TEKIYO(WRK-LINETEK)
                           WHEN    "2"   
                               MOVE    WRK-KOH-HENSYU-X (IDW)
                                           TO  HC062-TEKIYO(WRK-LINETEK)
                           WHEN    "3"   
                                MOVE    WRK-KOH-HENSYU-X (IDW)
                                           TO  HC063-TEKIYO(WRK-LINETEK)
                       END-EVALUATE
                   END-IF                    
               END-PERFORM
           END-IF
      *
      *    災害対応
           IF      RECE02-RJNGAIKOHFTN (1) =   3   OR  ZERO
               CONTINUE
           ELSE
      *        摘要欄に記載されている場合１行あける
               IF      WRK-LINETEK     >   ZERO
                   IF      WRK-LINETEK     >=   WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   ELSE
                       ADD     1               TO  WRK-LINETEK
                   END-IF
               END-IF
      *
               EVALUATE    RECE02-RJNGAIKOHFTN (1)
                WHEN    1
                WHEN    8
                WHEN    10
                WHEN    11
                WHEN    2
                WHEN    7
                WHEN    4
                WHEN    5
                 EVALUATE    RECE02-RJNGAIKOHFTN (1)
                         WHEN    1
                         WHEN    8
                         WHEN    10
                         WHEN    11
                             MOVE    "不詳"  TO  WRK-TITLE
                         WHEN    2
                         WHEN    7
                             MOVE    "災１"  TO  WRK-TITLE
                         WHEN    4
                             MOVE    "災２"  TO  WRK-TITLE
                         WHEN    5
                             MOVE    "不詳・災１"
                                             TO  WRK-TITLE
                 END-EVALUATE
                 INSPECT WRK-TITLE   REPLACING   ALL "  "    BY  "　"
      * 
                 IF      WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
                 END-IF    
                 ADD     1                   TO  WRK-LINETEK
      *
                 IF      CNT-PAGE        =   1   
                   MOVE    WRK-TITLE       TO  HC06-TEKIYO (WRK-LINETEK)
                 ELSE                    
                   EVALUATE    SYS-2005-NEXTPRTKBN-G
                       WHEN    "0"   
                           MOVE  WRK-TITLE TO  HC06-TEKIYO (WRK-LINETEK)
                       WHEN    "1"   
                           MOVE  WRK-TITLE TO  HC061-TEKIYO(WRK-LINETEK)
                       WHEN    "2"   
                           MOVE  WRK-TITLE TO  HC062-TEKIYO(WRK-LINETEK)
                       WHEN    "3"   
                           MOVE  WRK-TITLE TO  HC063-TEKIYO(WRK-LINETEK)
                    END-EVALUATE
                 END-IF
               END-EVALUATE
      *
      *        震災以前一部負担金の記載（災２）
               IF      RECE02-RJNGAIKOHFTN (1) =   4
                   MOVE    SPACE               TO  WRK-TITLE
                   MOVE    RECE02-GENMEN-NASI-KIN
                                           TO  WRK-FUTAN
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    WRK-FUTAN       TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF    ( KANACHK-RC      =   ZERO )
                   AND   ( KANACHK-MAX     >   ZERO )
                     IF   RECE02-SRYYM   =  "201103"
                       STRING  "【震災以前一部負担金】　　　　"
                                               DELIMITED  BY  SIZE
                               KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               DELIMITED  BY  SIZE
                               "円"            DELIMITED  BY  SIZE
                                               INTO    WRK-TITLE
                       END-STRING
                     ELSE
                       STRING  "【非減免対象分扱い一部負担金】"
                                               DELIMITED  BY  SIZE
                               KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               DELIMITED  BY  SIZE
                               "円"            DELIMITED  BY  SIZE
                                               INTO    WRK-TITLE
                       END-STRING
                     END-IF
                   END-IF
                   INSPECT WRK-TITLE   REPLACING   ALL "  "    BY  "　"
      * 
                   IF      WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   END-IF    
                   ADD     1                   TO  WRK-LINETEK
      *
                   IF      CNT-PAGE        =   1   
                       MOVE    WRK-TITLE   TO  HC06-TEKIYO (WRK-LINETEK)
                   ELSE                    
                       EVALUATE    SYS-2005-NEXTPRTKBN-G
                           WHEN    "0"   
                               MOVE    WRK-TITLE   TO
                                               HC06-TEKIYO (WRK-LINETEK)
                           WHEN    "1"   
                               MOVE    WRK-TITLE   TO
                                               HC061-TEKIYO(WRK-LINETEK)
                           WHEN    "2"   
                               MOVE    WRK-TITLE   TO
                                               HC062-TEKIYO(WRK-LINETEK)
                           WHEN    "3"   
                               MOVE    WRK-TITLE   TO
                                               HC063-TEKIYO(WRK-LINETEK)
                        END-EVALUATE
                   END-IF
               END-IF
      *                   
      *        災害対応（連絡先記載）
               EVALUATE    RECE02-RJNGAIKOHFTN (1)
                 WHEN    6
                 WHEN    7
                 WHEN    8
                 WHEN    9
                 WHEN    11
                   IF    ( RECE02-HOME-POST1
                                           =   SPACE )
                   AND   ( RECE02-HOME-ADRS1
                                           =   SPACE )
                   AND   ( RECE02-RENRAKU-POST1
                                           =   SPACE )
                   AND   ( RECE02-RENRAKU-NAME
                                           =   SPACE )
                   AND   ( RECE02-RENRAKU-ADRS1
                                           =   SPACE )
                   AND   ( RECE02-TEL      =   SPACE )
                       CONTINUE
                   ELSE
      *                自宅住所記載
                       IF   ( RECE02-HOME-ADRS1      =   SPACE )
                       AND  ( RECE02-HOME-POST1      =   SPACE )
                           CONTINUE
                       ELSE
                           MOVE     "【住所】"      TO  WRK-TITLE
                           INSPECT WRK-TITLE
                                       REPLACING   ALL "  "    BY  "　"
      * 
                           IF      WRK-LINETEK     >=  WRK-TEKIYOU-LINE
                               PERFORM 34120-PAGE-SEC
                           END-IF    
      * 
                           ADD     1                   TO  WRK-LINETEK
                           IF      CNT-PAGE        =   1   
                               MOVE    WRK-TITLE       TO
                                               HC06-TEKIYO (WRK-LINETEK)
                           ELSE                    
                               EVALUATE    SYS-2005-NEXTPRTKBN-G
                                   WHEN    "0"   
                                       MOVE  WRK-TITLE TO
                                               HC06-TEKIYO (WRK-LINETEK)
                                   WHEN    "1"   
                                       MOVE  WRK-TITLE TO
                                               HC061-TEKIYO(WRK-LINETEK)
                                   WHEN    "2"   
                                       MOVE  WRK-TITLE TO
                                               HC062-TEKIYO(WRK-LINETEK)
                                   WHEN    "3"   
                                       MOVE  WRK-TITLE TO
                                               HC063-TEKIYO(WRK-LINETEK)
                               END-EVALUATE
                           END-IF
      *                    自宅住所記載
                           MOVE    SPACE               TO  WRK-ADRS
                           IF      RECE02-HOME-POST1   =   SPACE
                               MOVE    RECE02-HOME-ADRS1   TO  WRK-ADRS
                           ELSE
                               STRING   "〒"       DELIMITED  BY  SIZE
                                    RECE02-HOME-POST1
                                                   DELIMITED  BY  SPACE
                                    RECE02-HOME-ADRS1
                                                   DELIMITED  BY  SPACE
                                                   INTO    WRK-ADRS
                               END-STRING
                           END-IF
                           PERFORM VARYING IDW     FROM    1   BY  1
                                   UNTIL   IDW     >       5
                                   OR      WRK-ADRS-R (IDW)
                                                   =       SPACE
                               INSPECT WRK-ADRS-R (IDW)
                                       REPLACING   ALL "  "    BY  "　"
      * 
                               IF      WRK-LINETEK >=  WRK-TEKIYOU-LINE
                                   PERFORM 34120-PAGE-SEC
                               END-IF    
                               ADD     1               TO  WRK-LINETEK
      *
                               IF      CNT-PAGE        =   1   
                                   MOVE    WRK-ADRS-R (IDW)
                                           TO  HC06-TEKIYO (WRK-LINETEK)
                               ELSE                    
                                 EVALUATE    SYS-2005-NEXTPRTKBN-G
                                   WHEN    "0"   
                                       MOVE    WRK-ADRS-R (IDW)
                                           TO  HC06-TEKIYO (WRK-LINETEK)
                                   WHEN    "1"   
                                       MOVE    WRK-ADRS-R (IDW)
                                           TO  HC061-TEKIYO(WRK-LINETEK)
                                   WHEN    "2"   
                                       MOVE    WRK-ADRS-R (IDW)
                                           TO  HC062-TEKIYO(WRK-LINETEK)
                                   WHEN    "3"   
                                       MOVE    WRK-ADRS-R (IDW)
                                           TO  HC063-TEKIYO(WRK-LINETEK)
                                 END-EVALUATE
                               END-IF                    
                           END-PERFORM
                       END-IF
      *                連絡先記載
                       IF    ( RECE02-RENRAKU-POST1
                                               =   SPACE )
                       AND   ( RECE02-RENRAKU-NAME
                                               =   SPACE )
                       AND   ( RECE02-RENRAKU-ADRS1
                                               =   SPACE )
                       AND   ( RECE02-TEL      =   SPACE )
                           CONTINUE
                       ELSE
                           MOVE     "【連絡先】"  TO  WRK-TITLE
                           INSPECT WRK-TITLE
                                       REPLACING   ALL "  "    BY  "　"
      * 
                           IF      WRK-LINETEK >=  WRK-TEKIYOU-LINE
                               PERFORM 34120-PAGE-SEC
                           END-IF    
                           ADD     1               TO  WRK-LINETEK
      * 
                           IF      CNT-PAGE        =   1   
                               MOVE    WRK-TITLE       TO
                                               HC06-TEKIYO (WRK-LINETEK)
                           ELSE                    
                               EVALUATE    SYS-2005-NEXTPRTKBN-G
                                   WHEN    "0"   
                                       MOVE  WRK-TITLE TO
                                               HC06-TEKIYO (WRK-LINETEK)
                                   WHEN    "1"   
                                       MOVE  WRK-TITLE TO
                                               HC061-TEKIYO(WRK-LINETEK)
                                   WHEN    "2"   
                                       MOVE  WRK-TITLE TO
                                               HC062-TEKIYO(WRK-LINETEK)
                                   WHEN    "3"   
                                       MOVE  WRK-TITLE TO
                                               HC063-TEKIYO(WRK-LINETEK)
                               END-EVALUATE
                           END-IF
      *                    事業所名記載
                           PERFORM VARYING IDW     FROM    1   BY  1
                                   UNTIL   IDW     >       2
                                   OR      RECE02-RENRAKU-NAME-R (IDW)
                                                   =       SPACE
                               MOVE    RECE02-RENRAKU-NAME-R (IDW)
                                                       TO  WRK-TITLE
                               INSPECT WRK-TITLE
                                       REPLACING   ALL "  "    BY  "　"
                               
      * 
                               IF      WRK-LINETEK >=  WRK-TEKIYOU-LINE
                                   PERFORM 34120-PAGE-SEC
                               END-IF    
                               ADD     1               TO  WRK-LINETEK
      *
                               IF      CNT-PAGE        =   1   
                                   MOVE    WRK-TITLE
                                           TO  HC06-TEKIYO (WRK-LINETEK)
                               ELSE                    
                                 EVALUATE    SYS-2005-NEXTPRTKBN-G
                                   WHEN    "0"   
                                       MOVE  WRK-TITLE
                                           TO  HC06-TEKIYO (WRK-LINETEK)
                                   WHEN    "1"   
                                       MOVE  WRK-TITLE
                                           TO  HC061-TEKIYO(WRK-LINETEK)
                                   WHEN    "2"   
                                       MOVE  WRK-TITLE
                                           TO  HC062-TEKIYO(WRK-LINETEK)
                                   WHEN    "3"   
                                       MOVE  WRK-TITLE
                                           TO  HC063-TEKIYO(WRK-LINETEK)
                                 END-EVALUATE
                               END-IF                    
                           END-PERFORM
      *                    連絡先住所記載
                           MOVE    SPACE               TO  WRK-ADRS
                           IF      RECE02-RENRAKU-POST1
                                                       =   SPACE
                               MOVE    RECE02-RENRAKU-ADRS1
                                                           TO  WRK-ADRS
                           ELSE
                               STRING   "〒"       DELIMITED  BY  SIZE
                                    RECE02-RENRAKU-POST1
                                                   DELIMITED  BY  SPACE
                                    RECE02-RENRAKU-ADRS1
                                                   DELIMITED  BY  SPACE
                                                   INTO    WRK-ADRS
                               END-STRING
                           END-IF
                           PERFORM VARYING IDW     FROM    1   BY  1
                                   UNTIL   IDW     >       5
                                   OR      WRK-ADRS-R (IDW)
                                                   =       SPACE
                               INSPECT WRK-ADRS-R (IDW)
                                       REPLACING   ALL "  "    BY  "　"
      * 
                               IF      WRK-LINETEK >=  WRK-TEKIYOU-LINE
                                   PERFORM 34120-PAGE-SEC
                               END-IF    
                               ADD     1               TO  WRK-LINETEK
      *
                               IF      CNT-PAGE        =   1   
                                   MOVE    WRK-ADRS-R (IDW)
                                           TO  HC06-TEKIYO (WRK-LINETEK)
                               ELSE                    
                                 EVALUATE    SYS-2005-NEXTPRTKBN-G
                                   WHEN    "0"   
                                       MOVE  WRK-ADRS-R (IDW)
                                           TO  HC06-TEKIYO (WRK-LINETEK)
                                   WHEN    "1"   
                                       MOVE  WRK-ADRS-R (IDW)
                                           TO  HC061-TEKIYO(WRK-LINETEK)
                                   WHEN    "2"   
                                       MOVE  WRK-ADRS-R (IDW)
                                           TO  HC062-TEKIYO(WRK-LINETEK)
                                   WHEN    "3"   
                                       MOVE  WRK-ADRS-R (IDW)
                                           TO  HC063-TEKIYO(WRK-LINETEK)
                                 END-EVALUATE
                               END-IF                    
                           END-PERFORM
      *                    連絡先記載
                           IF     RECE02-TEL      =   SPACE
                               CONTINUE
                           ELSE
                               MOVE    SPACE          TO  WRK-TEKIYOU
                               STRING   "ＴＥＬ：" DELIMITED  BY  SIZE
                                        RECE02-TEL DELIMITED  BY  SIZE
                                                   INTO    WRK-TEKIYOU
                               END-STRING
                               INSPECT WRK-TEKIYOU
                                       REPLACING   ALL "  "    BY  "　"
      * 
                               IF      WRK-LINETEK >=  WRK-TEKIYOU-LINE
                                   PERFORM 34120-PAGE-SEC
                               END-IF    
                               ADD     1               TO  WRK-LINETEK
      *
                               IF      CNT-PAGE        =   1   
                                   MOVE    WRK-TEKIYOU
                                           TO  HC06-TEKIYO (WRK-LINETEK)
                               ELSE                    
                                 EVALUATE    SYS-2005-NEXTPRTKBN-G
                                   WHEN    "0"   
                                       MOVE    WRK-TEKIYOU
                                           TO  HC06-TEKIYO (WRK-LINETEK)
                                   WHEN    "1"   
                                       MOVE    WRK-TEKIYOU
                                           TO  HC061-TEKIYO(WRK-LINETEK)
                                   WHEN    "2"   
                                       MOVE    WRK-TEKIYOU
                                           TO  HC062-TEKIYO(WRK-LINETEK)
                                   WHEN    "3"   
                                       MOVE    WRK-TEKIYOU
                                           TO  HC063-TEKIYO(WRK-LINETEK)
                                 END-EVALUATE
                               END-IF                    
                           END-IF
                       END-IF
                   END-IF
               END-EVALUATE
           END-IF
      *
      *    東大阪市減免コメント
           IF      RECE02-GENMEN-COM   =   1   OR   2
      *        摘要欄に記載されている場合１行あける
               IF      WRK-LINETEK     >   ZERO
                   IF      WRK-LINETEK     >=   WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   ELSE
                       ADD     1               TO  WRK-LINETEK
                   END-IF
               END-IF
      *
               IF      WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF    
               ADD     1                   TO  WRK-LINETEK
      *
               IF      CNT-PAGE        =   1
                   EVALUATE    RECE02-GENMEN-COM
                       WHEN    1
                           MOVE    "決定区分１"
                                           TO  HC06-TEKIYO (WRK-LINETEK)
                       WHEN    2
                           MOVE    "決定区分２"
                                           TO  HC06-TEKIYO (WRK-LINETEK)
                   END-EVALUATE
               ELSE                    
                   EVALUATE    SYS-2005-NEXTPRTKBN-G
                       WHEN    "0"   
                           EVALUATE    RECE02-GENMEN-COM
                               WHEN    1
                                   MOVE    "決定区分１"
                                           TO  HC06-TEKIYO (WRK-LINETEK)
                               WHEN    2
                                   MOVE    "決定区分２"
                                           TO  HC06-TEKIYO (WRK-LINETEK)
                           END-EVALUATE
                       WHEN    "1"   
                           EVALUATE    RECE02-GENMEN-COM
                               WHEN    1
                                   MOVE    "決定区分１"
                                           TO  HC061-TEKIYO(WRK-LINETEK)
                               WHEN    2
                                   MOVE    "決定区分２"
                                           TO  HC061-TEKIYO(WRK-LINETEK)
                           END-EVALUATE
                       WHEN    "2"   
                           EVALUATE    RECE02-GENMEN-COM
                               WHEN    1
                                   MOVE    "決定区分１"
                                           TO  HC062-TEKIYO(WRK-LINETEK)
                               WHEN    2
                                   MOVE    "決定区分２"
                                           TO  HC062-TEKIYO(WRK-LINETEK)
                           END-EVALUATE
                       WHEN    "3"   
                           EVALUATE    RECE02-GENMEN-COM
                               WHEN    1
                                   MOVE    "決定区分１"
                                           TO  HC063-TEKIYO(WRK-LINETEK)
                               WHEN    2
                                   MOVE    "決定区分２"
                                           TO  HC063-TEKIYO(WRK-LINETEK)
                           END-EVALUATE
                   END-EVALUATE
               END-IF
           END-IF
      *
      *    第三者行為対応
           IF  (RECE02-DAISAN-GAITO   =   1  OR  2  OR  4)
           AND (RECE02-DAISAN-TOKKI   =   ZERO           )
      *        摘要欄に記載されている場合１行あける
               IF      WRK-LINETEK     >   ZERO
                   IF      WRK-LINETEK     >=   WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   ELSE
                       ADD     1               TO  WRK-LINETEK
                   END-IF
               END-IF
      *
      *        請求点数
               MOVE    SPACE                   TO  WRK-TITLE
               MOVE    RECE02-DAISAN-TOTALTEN  TO  WRK-FUTAN3
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI
               MOVE    WRK-FUTAN3      TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC      =   ZERO )
               AND   ( KANACHK-MAX     >   ZERO )
                 STRING  "（第三者行為）請求点　　　"
                                         DELIMITED  BY  SIZE
                          KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                         DELIMITED  BY  SIZE
                          "点"           DELIMITED  BY  SIZE
                                         INTO    WRK-TITLE
                 END-STRING
               END-IF
               INSPECT WRK-TITLE   REPLACING   ALL "  "    BY  "　"
      *
               IF      WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF
               ADD     1                   TO  WRK-LINETEK
      *
               IF      CNT-PAGE        =   1
                     MOVE    WRK-TITLE  TO  HC06-TEKIYO (WRK-LINETEK)
               ELSE
                   EVALUATE    SYS-2005-NEXTPRTKBN-G
                     WHEN    "0"
                       MOVE  WRK-TITLE  TO  HC06-TEKIYO (WRK-LINETEK)
                     WHEN    "1"
                       MOVE  WRK-TITLE  TO  HC061-TEKIYO(WRK-LINETEK)
                     WHEN    "2"
                       MOVE  WRK-TITLE  TO  HC062-TEKIYO(WRK-LINETEK)
                     WHEN    "3"
                       MOVE  WRK-TITLE  TO  HC063-TEKIYO(WRK-LINETEK)
                    END-EVALUATE
               END-IF
      *
      *        一部負担金
               MOVE    SPACE                   TO  WRK-TITLE
               MOVE    RECE02-DAISAN-FTNMONEY  TO  WRK-FUTAN4
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI
               MOVE    WRK-FUTAN4      TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC      =   ZERO )
               AND   ( KANACHK-MAX     >   ZERO )
                 STRING  "（第三者行為）一部負担額"
                                         DELIMITED  BY  SIZE
                          KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                         DELIMITED  BY  SIZE
                          "円"           DELIMITED  BY  SIZE
                                         INTO    WRK-TITLE
                 END-STRING
               END-IF
               INSPECT WRK-TITLE   REPLACING   ALL "  "    BY  "　"
      *
               IF      WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF
               ADD     1                   TO  WRK-LINETEK
      *
               IF      CNT-PAGE        =   1
                     MOVE    WRK-TITLE  TO  HC06-TEKIYO (WRK-LINETEK)
               ELSE
                   EVALUATE    SYS-2005-NEXTPRTKBN-G
                     WHEN    "0"
                       MOVE  WRK-TITLE  TO  HC06-TEKIYO (WRK-LINETEK)
                     WHEN    "1"
                       MOVE  WRK-TITLE  TO  HC061-TEKIYO(WRK-LINETEK)
                     WHEN    "2"
                       MOVE  WRK-TITLE  TO  HC062-TEKIYO(WRK-LINETEK)
                     WHEN    "3"
                       MOVE  WRK-TITLE  TO  HC063-TEKIYO(WRK-LINETEK)
                    END-EVALUATE
               END-IF
           END-IF
      *
      *    適用欄上段コメント（レセプト特記事項編集情報）
           IF    ( RECE02-2006-TEKIYOU-COMMENT1 (1)    =   SPACE )
           AND   ( RECE02-2006-TEKIYOU-COMMENT1 (2)    =   SPACE )
           AND   ( RECE02-2006-TEKIYOU-COMMENT1 (3)    =   SPACE )
           AND   ( RECE02-2006-TEKIYOU-COMMENT1 (4)    =   SPACE )
           AND   ( RECE02-2006-TEKIYOU-COMMENT1 (5)    =   SPACE )
               CONTINUE
           ELSE
      *        摘要欄に記載されている場合１行あける
               IF  WRK-LINETEK             >   ZERO
                   IF      WRK-LINETEK         >=   WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   ELSE
                       ADD     1               TO  WRK-LINETEK
                   END-IF
               END-IF
      *
               PERFORM VARYING IDW     FROM    1   BY  1
                       UNTIL   IDW     >       5
                   IF      RECE02-2006-TEKIYOU-COMMENT1 (IDW)
                                           NOT =   SPACE
                       IF  WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                           PERFORM 34120-PAGE-SEC
                       END-IF    
                       ADD     1                   TO  WRK-LINETEK
      *
                       IF      CNT-PAGE        =   1   
                           MOVE    RECE02-2006-TEKIYOU-COMMENT1 (IDW)
                                           TO  HC06-TEKIYO (WRK-LINETEK)
                       ELSE                    
                           EVALUATE    SYS-2005-NEXTPRTKBN-G
                               WHEN    "0"   
                                   MOVE    RECE02-2006-TEKIYOU-COMMENT1
                                                                   (IDW)
                                       TO  HC06-TEKIYO (WRK-LINETEK)
                               WHEN    "1"   
                                   MOVE    RECE02-2006-TEKIYOU-COMMENT1
                                                                   (IDW)
                                           TO  HC061-TEKIYO(WRK-LINETEK)
                               WHEN    "2"   
                                   MOVE    RECE02-2006-TEKIYOU-COMMENT1
                                                                   (IDW)
                                           TO  HC062-TEKIYO(WRK-LINETEK)
                               WHEN    "3"   
                                   MOVE    RECE02-2006-TEKIYOU-COMMENT1
                                                                   (IDW)
                                           TO  HC063-TEKIYO(WRK-LINETEK)
                           END-EVALUATE
                       END-IF                    
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       380-HC101-TEKIYOU-COM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<入退院履歴情報>処理
      *****************************************************************
       380-HC101-NYUTAI-RIREKI-HEN-SEC      SECTION.
      *
      *    最初の行のとき２行あける
           COMPUTE WRK-WLINETEK    =   WRK-LINETEK +   2
           IF  WRK-WLINETEK        >   WRK-TEKIYOU-LINE
               PERFORM 34120-PAGE-SEC
           END-IF    
           ADD     2               TO  WRK-LINETEK
      *
           MOVE    ZERO                TO  WRK-NYUINCHUKBN
      *
      *    最初の行のコメントのセット
           MOVE    SPACE               TO  WRK-NYUTAI-RIREKI-AREA
           MOVE    "入退院履歴"        TO  WRK-NYUTAI-MSG
      *
           IF      WRK-LINETEK         >=  WRK-TEKIYOU-LINE
               PERFORM 34120-PAGE-SEC
           END-IF
           ADD     1                   TO  WRK-LINETEK
      *
           INSPECT WRK-NYUTAI-RIREKI-AREA  REPLACING   ALL "  "
                                                       BY  "　"
           IF      CNT-PAGE            =   1
               MOVE    WRK-NYUTAI-RIREKI-AREA
                                       TO  HC06-TEKIYO (WRK-LINETEK)
           ELSE
               EVALUATE    SYS-2005-NEXTPRTKBN-G
                   WHEN    "0"
                       MOVE    WRK-NYUTAI-RIREKI-AREA
                                           TO  HC06-TEKIYO (WRK-LINETEK)
                   WHEN    "1"
                       MOVE    WRK-NYUTAI-RIREKI-AREA
                                           TO  HC061-TEKIYO(WRK-LINETEK)
                   WHEN    "2"
                       MOVE    WRK-NYUTAI-RIREKI-AREA
                                           TO  HC062-TEKIYO(WRK-LINETEK)
                   WHEN    "3"
                       MOVE    WRK-NYUTAI-RIREKI-AREA
                                           TO  HC063-TEKIYO(WRK-LINETEK)
               END-EVALUATE
           END-IF
           MOVE    SPACE           TO  WRK-NYUTAI-RIREKI-AREA
      *
           INITIALIZE                      RECE12-REC
           MOVE    RECE02-KEY          TO  RECE12-KEY1
      *
           START   RECE12-FILE
               KEY IS    >=   RECE12-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE12
               NOT INVALID
                   PERFORM 900-RECE12-NEXT-SEC
           END-START
      *
           PERFORM UNTIL ( FLG-RECE12           =  1                   )
                   OR    ( RECE02-KEY       NOT =  RECE12-KEY1         )
      *
               IF      WRK-NYUINCHUKBN     =   ZERO
                   MOVE    RECE12-NYUINYMD     TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3-G(1:2)   TO
                                               WRK-NYUTAI-NYUINYMD (1:2)
                   MOVE    LNK-DAY2-EDTYMD3(5:18)    TO  
                                               WRK-NYUTAI-NYUINYMD (3:)
                   MOVE    "〜"                TO  WRK-NYUTAI-FL1
               END-IF
      *                          
               IF      RECE12-TAIINYMD (1:6)   
                                           >   RECE02-SRYYM
                   MOVE    SPACE               TO  WRK-NYUTAI-TAIINYMD
                   CONTINUE
               ELSE
                   MOVE    RECE12-TAIINYMD     TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3-G(1:2)   TO
                                               WRK-NYUTAI-TAIINYMD (1:2)
                   MOVE    LNK-DAY2-EDTYMD3(5:18)    TO  
                                               WRK-NYUTAI-TAIINYMD (3:)
               END-IF
      *
               IF      RECE12-NYUINCHUKBN  =   ZERO
                   IF  WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   END-IF    
                   ADD     1                   TO  WRK-LINETEK
      *
                   INSPECT WRK-NYUTAI-RIREKI-AREA  REPLACING   ALL "  "
                                                               BY  "　"  
                   IF      CNT-PAGE        =   1   
                       MOVE    WRK-NYUTAI-RIREKI-AREA
                                           TO  HC06-TEKIYO (WRK-LINETEK)
                   ELSE                    
                       EVALUATE    SYS-2005-NEXTPRTKBN-G
                           WHEN    "0"   
                               MOVE    WRK-NYUTAI-RIREKI-AREA
                                           TO  HC06-TEKIYO (WRK-LINETEK)
                           WHEN    "1"   
                               MOVE    WRK-NYUTAI-RIREKI-AREA
                                           TO  HC061-TEKIYO(WRK-LINETEK)
                           WHEN    "2"   
                               MOVE    WRK-NYUTAI-RIREKI-AREA
                                           TO  HC062-TEKIYO(WRK-LINETEK)
                           WHEN    "3"   
                               MOVE    WRK-NYUTAI-RIREKI-AREA
                                           TO  HC063-TEKIYO(WRK-LINETEK)
                       END-EVALUATE
                   END-IF                    
                   MOVE    SPACE           TO  WRK-NYUTAI-RIREKI-AREA
               END-IF
      *
               MOVE    RECE12-NYUINCHUKBN  TO  WRK-NYUINCHUKBN    
      *
               PERFORM 900-RECE12-NEXT-SEC
           END-PERFORM
      *
      *    最後の履歴が入院中のときに編集する
           IF      WRK-NYUTAI-RIREKI-AREA
                                       NOT =   SPACE                   
               IF  WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF    
               ADD     1                   TO  WRK-LINETEK
      *
               INSPECT WRK-NYUTAI-RIREKI-AREA  REPLACING   ALL "  "
                                                           BY  "　"  
               IF      CNT-PAGE        =   1   
                   MOVE    WRK-NYUTAI-RIREKI-AREA
                                       TO  HC06-TEKIYO (WRK-LINETEK)
               ELSE                    
                   EVALUATE    SYS-2005-NEXTPRTKBN-G
                       WHEN    "0"   
                           MOVE    WRK-NYUTAI-RIREKI-AREA
                                       TO  HC06-TEKIYO (WRK-LINETEK)
                       WHEN    "1"   
                           MOVE    WRK-NYUTAI-RIREKI-AREA
                                       TO  HC061-TEKIYO(WRK-LINETEK)
                       WHEN    "2"   
                           MOVE    WRK-NYUTAI-RIREKI-AREA
                                       TO  HC062-TEKIYO(WRK-LINETEK)
                       WHEN    "3"   
                           MOVE    WRK-NYUTAI-RIREKI-AREA
                                       TO  HC063-TEKIYO(WRK-LINETEK)
                   END-EVALUATE
               END-IF                    
               MOVE    SPACE           TO  WRK-NYUTAI-RIREKI-AREA
           END-IF
      *
      *    最後の行の後に１行あける
           IF      WRK-NYUTAI-RIREKI-AREA
                                       =   SPACE                   
               IF  WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF    
               ADD     1               TO  WRK-LINETEK     
           END-IF
      *
           .
       380-HC101-NYUTAI-RIREKI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<手術歴>処理
      *****************************************************************
       380-HC101-OPECOM-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-OPECOM
           MOVE    SPACE               TO  WRK-OPECOM-HEN
      *
      *    手術歴編集
           INITIALIZE                      OPECOM-REC
           MOVE    RECE02-HOSPNUM      TO  OPECOM-HOSPNUM
           MOVE    RECE02-PTID         TO  OPECOM-PTID
           MOVE    RECE02-SRYYM        TO  OPECOM-TEKSTYM
                                           OPECOM-TEKEDYM
           MOVE    RECE02-NYUGAIKBN    TO  OPECOM-NYUGAIKBN
           MOVE    "tbl_opecom"        TO  WRK-OPECOM-MCP-TABLE
           MOVE    "key4"              TO  WRK-OPECOM-MCP-PATHNAME
           PERFORM 900-OPECOM-READ-SEC
      *
           PERFORM             UNTIL   FLG-OPECOM   =   1
               MOVE    ZERO                TO  FLG-OPECOM-CHK
               IF      OPECOM-HKNNUM   NOT =   SPACE
                   IF    ( OPECOM-HKNNUM       =   RECE02-HKNNUM )
                   AND   ( OPECOM-HKNID        =   RECE02-HKNID  )
                       MOVE    1                   TO  FLG-OPECOM-CHK
                   ELSE
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >       4
                               OR      RECE02-KOHNUM (IDY) =   SPACE
                           IF    ( OPECOM-HKNNUM   
                                               =   RECE02-KOHNUM (IDY) )
                           AND   ( OPECOM-HKNID   
                                               =   RECE02-KOHID  (IDY) )
                               MOVE    1       TO  FLG-OPECOM-CHK
                               MOVE    4       TO  IDY
                           END-IF
                       END-PERFORM
                   END-IF    
               ELSE
                   MOVE    1                   TO  FLG-OPECOM-CHK
               END-IF
               IF      FLG-OPECOM-CHK          =   1
                   MOVE    OPECOM-SRYYMD       TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-OPECOM-HEN
                   PERFORM 3801-OPECOM-HENSYU-SEC                 
                   MOVE    OPECOM-OPENAME      TO  WRK-OPECOM-HEN
                   PERFORM 3801-OPECOM-HENSYU-SEC
                   IF      OPECOM-OPECOMMENT   NOT =   SPACE                
                       MOVE    OPECOM-OPECOMMENT   TO  WRK-OPECOM-HEN
                       PERFORM 3801-OPECOM-HENSYU-SEC
                   END-IF                 
               END-IF               
      *                 
               MOVE    WRK-OPECOM-MCP-TABLE
                                           TO  MCP-TABLE
               MOVE    WRK-OPECOM-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-OPECOM-READ-N-SEC
           END-PERFORM
      *
           MOVE    WRK-OPECOM-MCP-TABLE
                                       TO  MCP-TABLE
           MOVE    WRK-OPECOM-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    コメント編集
           INITIALIZE                      OPECOM-REC
           MOVE    RECE02-HOSPNUM      TO  OPECOM-HOSPNUM
           MOVE    RECE02-PTID         TO  OPECOM-PTID
           MOVE    RECE02-SRYYM        TO  OPECOM-TEKSTYM
                                           OPECOM-TEKEDYM
           MOVE    RECE02-NYUGAIKBN    TO  OPECOM-NYUGAIKBN
           MOVE    "tbl_opecom"        TO  WRK-OPECOM-MCP-TABLE
           MOVE    "key5"              TO  WRK-OPECOM-MCP-PATHNAME
           PERFORM 900-OPECOM-READ-SEC
      *
           PERFORM             UNTIL   FLG-OPECOM   =   1
               MOVE    ZERO                TO  FLG-OPECOM-CHK
               IF      OPECOM-HKNNUM   NOT =   SPACE
                   IF    ( OPECOM-HKNNUM       =   RECE02-HKNNUM )
                   AND   ( OPECOM-HKNID        =   RECE02-HKNID  )
                       MOVE    1                   TO  FLG-OPECOM-CHK
                   ELSE
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >       4
                               OR      RECE02-KOHNUM (IDY) =   SPACE
                           IF    ( OPECOM-HKNNUM   
                                               =   RECE02-KOHNUM (IDY) )
                           AND   ( OPECOM-HKNID   
                                               =   RECE02-KOHID  (IDY) )
                               MOVE    1       TO  FLG-OPECOM-CHK
                               MOVE    4       TO  IDY
                           END-IF
                       END-PERFORM
                   END-IF    
               ELSE
                   MOVE    1                   TO  FLG-OPECOM-CHK
               END-IF
               IF      FLG-OPECOM-CHK          =   1
                   MOVE    OPECOM-OPENAME      TO  WRK-OPECOM-HEN
                   PERFORM 3801-OPECOM-HENSYU-SEC
                   IF      OPECOM-OPECOMMENT   NOT =   SPACE                
                       MOVE    OPECOM-OPECOMMENT   TO  WRK-OPECOM-HEN
                       PERFORM 3801-OPECOM-HENSYU-SEC
                   END-IF                 
               END-IF               
      *                 
               MOVE    WRK-OPECOM-MCP-TABLE
                                           TO  MCP-TABLE
               MOVE    WRK-OPECOM-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-OPECOM-READ-N-SEC
           END-PERFORM
      *
           MOVE    WRK-OPECOM-MCP-TABLE
                                       TO  MCP-TABLE
           MOVE    WRK-OPECOM-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       380-HC101-OPECOM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントの改行コード削除処理
      *****************************************************************
       3801-OPECOM-HENSYU-SEC    SECTION.
      *
           IF      WRK-OPECOM          =   ZERO
      *        傷病名との間に区切り線を出力
               IF      WRK-LINETEK         >   ZERO
                   IF      WRK-LINETEK >=  WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   ELSE
                       MOVE    1               TO  FLG-KUGIRI
                       PERFORM 34110-KUGIRI-LINE-SEC
                       MOVE    ZERO            TO  FLG-KUGIRI
                   END-IF
               END-IF
           ELSE
      *        手術歴とコメントの間に改行
               IF    ( WRK-OPECOM-MCP-PATHNAME
                                           =   "key5" )
               AND   ( WRK-OPECOM          =   1      )
                   IF      WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   ELSE
                       ADD     1                   TO  WRK-LINETEK
                   END-IF
               END-IF
           END-IF
      *
           IF      WRK-OPECOM-MCP-PATHNAME =   "key4"
               MOVE    1                   TO  WRK-OPECOM
           ELSE
               MOVE    2                   TO  WRK-OPECOM
           END-IF                 
      *
           IF      WRK-LINETEK         >=  WRK-TEKIYOU-LINE
               PERFORM 34120-PAGE-SEC
           END-IF
           ADD     1                   TO  WRK-LINETEK
           IF      CNT-PAGE            =   1   
               MOVE    WRK-OPECOM-HEN  TO  HC06-TEKIYO (WRK-LINETEK)
           ELSE                
               EVALUATE    SYS-2005-NEXTPRTKBN-G
                   WHEN    "0"
                       MOVE    WRK-OPECOM-HEN
                                           TO  HC06-TEKIYO (WRK-LINETEK)
                   WHEN    "1"
                       MOVE    WRK-OPECOM-HEN
                                           TO  HC061-TEKIYO(WRK-LINETEK)
                   WHEN    "2"
                       MOVE    WRK-OPECOM-HEN
                                           TO  HC062-TEKIYO(WRK-LINETEK)
                   WHEN    "3"
                       MOVE    WRK-OPECOM-HEN
                                           TO  HC063-TEKIYO(WRK-LINETEK)
               END-EVALUATE
           END-IF                
      *
           MOVE    SPACE               TO  WRK-OPECOM-HEN
           .
       3801-OPECOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票格納処理
      *****************************************************************
       390-PRINT-OUT-SEC               SECTION.
      *
           IF      TBL-RECE-MAX        =   99
               STRING   "レセプトが９９頁を超えるため処理を中止します"
                                               DELIMITED  BY  SIZE
                        "　患者番号＝"         DELIMITED  BY  SIZE
                        RECE02-PTNUM           DELIMITED  BY  SIZE  
                                               INTO  WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               STRING  "ORCR0105 "             DELIMITED   BY  SIZE
                       WRK-RECEERR             DELIMITED   BY  SPACE
                       LOW-VALUE               DELIMITED   BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           ADD     1                   TO  TBL-RECE-MAX
      * 
           MOVE    CNT-PAGE            TO  TBL-RECE-RENNUM(TBL-RECE-MAX)
           IF      CNT-PAGE            =   1
      *********MOVE    "HCM11V01.red"      TO
      *********MOVE    "HCM11V02.red"      TO
      *********MOVE    "HCM11V03.red"      TO
               MOVE    "HCM11V07.red"      TO
                                       TBL-RECE-FORMID  (TBL-RECE-MAX)
               MOVE    HC06                TO
                                       TBL-RECE-PRTDATA (TBL-RECE-MAX)
           ELSE
               EVALUATE    SYS-2005-NEXTPRTKBN-G
                   WHEN    ZERO
      *****************MOVE    "HCM11V01.red"      TO
      *********        MOVE    "HCM11V02.red"      TO
      *********        MOVE    "HCM11V03.red"      TO
                       MOVE    "HCM11V07.red"      TO
                                       TBL-RECE-FORMID  (TBL-RECE-MAX)
                       MOVE    HC06                TO
                                       TBL-RECE-PRTDATA (TBL-RECE-MAX)
                   WHEN    "1"   
      *****************MOVE    "HCM12V01.red"      TO
                       MOVE    "HCM12V03.red"      TO
                                       TBL-RECE-FORMID  (TBL-RECE-MAX)
                       MOVE    HC061               TO
                                       TBL-RECE-PRTDATA (TBL-RECE-MAX)
                   WHEN    "2"   
      *****************MOVE    "HCM13V01.red"      TO
                       MOVE    "HCM13V03.red"      TO
                                       TBL-RECE-FORMID  (TBL-RECE-MAX)
                       MOVE    HC062               TO
                                       TBL-RECE-PRTDATA (TBL-RECE-MAX)
                   WHEN    "3"   
      *****************MOVE    "HCM14V01.red"      TO
                       MOVE    "HCM14V03.red"      TO
                                       TBL-RECE-FORMID  (TBL-RECE-MAX)
                       MOVE    HC063               TO
                                       TBL-RECE-PRTDATA (TBL-RECE-MAX)
               END-EVALUATE
           END-IF
      *
      *   リアルタイムプレビュー時の画面設定
           IF    ( WRK-PARA-SHELLID    =   "RECEPTX" )
           AND   ( WRK-PRVLAYOUT       =   "1"       ) 
               EVALUATE    TBL-RECE-FORMID  (TBL-RECE-MAX)
                   WHEN    "HCM11V07.red"
                       MOVE    "HCM11V07P.red"     TO
                                       TBL-RECE-FORMID  (TBL-RECE-MAX)
                   WHEN    "HCM12V03.red"
                       MOVE    "HCM12V03P.red"     TO
                                       TBL-RECE-FORMID  (TBL-RECE-MAX)
                   WHEN    "HCM13V03.red"
                       MOVE    "HCM13V03P.red"     TO
                                       TBL-RECE-FORMID  (TBL-RECE-MAX)
                   WHEN    "HCM14V03.red"
                       MOVE    "HCM14V03P.red"     TO
                                       TBL-RECE-FORMID  (TBL-RECE-MAX)
               END-EVALUATE
           END-IF
      *
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票更新処理
      *****************************************************************
       391-DB-WRITE-SEC                SECTION.
      *
           MOVE    ZERO                 TO  FLG-ERR
      *
           PERFORM VARYING IDXY    FROM    1   BY  1
                   UNTIL   IDXY    >       TBL-RECE-MAX
                   OR      FLG-ERR >       ZERO
      *
               ADD     1                    TO  WRK-RECENUM
      * 
      *        レセプト明細管理ＤＢ
               PERFORM 3901-RECE08-INSERT-SEC
      *
      *        レセプト帳票管理ファイル出力
               IF    ( FLG-ERR              =   ZERO     )
               AND   ( WRK-PARA-SHELLID NOT =  "RECEPTX" )
                   PERFORM 3903-RECE07-WRITE-SEC
               END-IF
           END-PERFORM
      *
      *    請求管理ＤＢ更新
           IF      FLG-ERR              =   ZERO
               PERFORM 3904-RECE10-HENSYU-SEC
           END-IF
      *
           IF      FLG-ERR              =   1
               MOVE    2                    TO  FLG-STS
           END-IF
      *
           .
       391-DB-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細ＤＢ
      *****************************************************************
       3901-RECE08-INSERT-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-SYUBETU-SRT1 
      *
           INITIALIZE                  RECE08-REC
           MOVE    WRK-PARA-HOSPNUM    TO  RECE08-HOSPNUM
           MOVE    "HC06"              TO  RECE08-PRTID
      *->  MOVE    WRK-PARA-SRYYM      TO  RECE08-RECEYM
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
                   MOVE    ALL "9"         TO  RECE08-RECEYM
           ELSE
                   MOVE    WRK-PARA-SRYYM  TO  RECE08-RECEYM
           END-IF
           MOVE    WRK-PARA-CREYMD     TO  RECE08-CREYMD
           MOVE    WRK-PARA-CREHMS (1:6)
                                       TO  RECE08-CREHMS
           IF      RECE02-TEISYUTUSAKI =   "2"
               IF      RECE02-HKNJANUM-KEY (1:2)   =   "67"
                   MOVE    RECE02-HKNJANUM-KEY (3:2)
                                           TO  RECE08-PREFNUM
               ELSE
                   MOVE    RECE02-HKNJANUM-KEY (1:2)
                                           TO  RECE08-PREFNUM
               END-IF
               IF      RECE08-PREFNUM  =   SYS-1001-PREFNUM
                   MOVE    "1"             TO  RECE08-PREFKBN
               ELSE
                   MOVE    "2"             TO  RECE08-PREFKBN
                   MOVE    RECE02-SRYYM        TO  WRK-SYMD (1:6)
                   MOVE    "01"                TO  WRK-SYMD (7:2)
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       5
                           OR      WRK-1033-STYUKYMD (IDX) =   SPACE
                       IF      WRK-1033-STYUKYMD (IDX) <=  WRK-SYMD
                       AND     WRK-1033-EDYUKYMD (IDX) >=  WRK-SYMD      
                           PERFORM VARYING IDY FROM    1   BY  1
                                   UNTIL   IDY >       20
                                   OR      WRK-1033-HKNJANUM (IDX IDY)
                                                           =   SPACE     
                               IF      RECE02-HKNJANUM-KEY = 
                                           WRK-1033-HKNJANUM  (IDX IDY)
                                   MOVE    "1" TO  RECE08-PREFKBN
                                   MOVE    20  TO  IDY
                                   MOVE    5   TO  IDX
                               END-IF
                           END-PERFORM
                       END-IF            
                   END-PERFORM
               END-IF        
           ELSE
               IF      RECE02-TEISYUTUSAKI =   "6"
                   MOVE    RECE02-HKNJANUM-KEY (3:2)
                                           TO  RECE08-PREFNUM
                   IF      RECE08-PREFNUM  =   SYS-1001-PREFNUM
                       MOVE    "1"             TO  RECE08-PREFKBN
                   ELSE
                       MOVE    "2"             TO  RECE08-PREFKBN
                   END-IF
               ELSE
                   MOVE    "0"             TO  RECE08-PREFKBN
                   MOVE    "00"            TO  RECE08-PREFNUM
               END-IF
           END-IF
      *
           MOVE    RECE08-PREFKBN      TO  RECE02-PREFKBN
           MOVE    RECE08-PREFNUM      TO  RECE02-HKNJANUM-PREFNUM
      *
           MOVE    RECE02-HKNJANUM-KEY TO  RECE08-HKNJANUM
           MOVE    RECE02-TEKSTYMD     TO  RECE08-TEKSTYMD
           MOVE    RECE02-TEISYUTUSAKI TO  RECE08-TEISYUTUSAKI
           IF    ( RECE02-PTNUM        =   SPACE )
           OR    ( RECE02-ERR-ERRKBN   >    ZERO )
               MOVE    ZERO                TO  RECE08-RECESYUBETU
           ELSE      
               MOVE    RECE02-RECESYUBETU  TO  RECE08-RECESYUBETU
      *        平成２０年４月以降公費３併・４併のとき
               EVALUATE    RECE02-RECESYUBETU (3:1)
                   WHEN    "4"
                   WHEN    "5"
                       MOVE    "3"         TO  RECE08-RECESYUBETU (3:1)
               END-EVALUATE
           END-IF    
           MOVE    RECE02-PTNUM        TO  RECE08-PTNUM
           PERFORM VARYING IDX FROM    20  BY  -1
                   UNTIL   IDX <       1
               IF      RECE08-PTNUM (IDX:1)    NOT =   SPACE
                   MOVE    1               TO  IDX 
               ELSE  
                   MOVE    "Z"             TO  RECE08-PTNUM (IDX:1)
               END-IF
           END-PERFORM 
           MOVE    RECE02-SRYYM        TO  RECE08-SRYYM
           MOVE    TBL-RECE-RENNUM (IDXY)
                                       TO  RECE08-RENNUM
           IF      WRK-PARA-SYOKBN     =   "1"
               MOVE    WRK-RECENUM         TO  RECE08-RECENUM
           ELSE
               IF      BTPARA-INFO-KBN     =   "01"    OR  "04"
                                                       OR  "05"
                   COMPUTE RECE08-RECENUM  =
                           RECE02-PRTSEQ   *   100 +   
                                               TBL-RECE-RENNUM (IDXY)
               ELSE
                   MOVE    WRK-RECENUM         TO  RECE08-RECENUM
               END-IF
           END-IF
      *
           MOVE    "00"                TO  RECE08-SRYKA
           MOVE    "99999999"          TO  RECE08-HKNJANUM-S
           MOVE    "999"               TO  RECE08-RECESYUBETU-SRT
           MOVE    "99"                TO  RECE08-PREFKBN-SRT
           MOVE    "9"                 TO  RECE08-HKNJANUM-SRT
                                           RECE08-ZAITAKU
                                           RECE08-CHOUKI
                                           RECE08-KOUGAKU
      *     主科設定
           IF    ( SYS-2009-SYUKA-KBN  =   "1"    )
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
                   MOVE    RECE02-SYUKA        TO  RECE08-SRYKA
               END-IF
           END-IF
      *
      *    編綴順項目の設定
           IF    ( RECE02-TEISYUTUSAKI =   "6"    )
           OR    ( RECE02-RECESYUBETU  =   "6666"  OR  "8888"  OR
                                           "0000" )
           OR    ( RECE02-ERR-ERRKBN   >   ZERO   )
               CONTINUE
           ELSE
               IF    ( RECE02-TEISYUTUSAKI =   "1"    )
                   PERFORM 39012-SYAHO-HENTETU-HENSYU-SEC
               ELSE
                   IF    ( FLG-SYS-2008        =   ZERO   )
                   AND   ( FLG-RECE-PUT-UM     =   ZERO   )
                       CONTINUE
                   ELSE
                       PERFORM 39011-HENTETU-HENSYU-SEC
                   END-IF
               END-IF
           END-IF
      *
           MOVE    RECE08-REC          TO  MCPDATA-REC
           PERFORM 900-RECE08-READ-SEC
      
           IF      FLG-RECE08          =   ZERO
               DISPLAY "ORCR0105 MCP-RC=" MCP-RC " " RECE08-KEY 
               STRING   "レセプト明細に更新できません　患者番号＝"
                                               DELIMITED  BY  SIZE
                        RECE02-PTNUM           DELIMITED  BY  SIZE  
                                               INTO  WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                  TO  FLG-ERR
           ELSE    
               IF      RECE08-TEISYUTUSAKI =   "1"
                   EVALUATE    RECE02-HKNNUM                   
                       WHEN    "001"
                       WHEN    "009"
                           MOVE    "1"             TO  RECE08-HKNKBN
                       WHEN    "002"
                           MOVE    "2"             TO  RECE08-HKNKBN
                       WHEN    "003"
                           MOVE    "3"             TO  RECE08-HKNKBN
                       WHEN    "004"
                           MOVE    "4"             TO  RECE08-HKNKBN
                       WHEN    "031"  THRU  "034"
                           MOVE    "5"             TO  RECE08-HKNKBN
                       WHEN    "006"
                           MOVE    "6"             TO  RECE08-HKNKBN
                       WHEN    "007"               
                           MOVE    "7"             TO  RECE08-HKNKBN
                       WHEN    "063"
                       WHEN    "072"  THRU  "075"
                           MOVE    "8"             TO  RECE08-HKNKBN
                   END-EVALUATE
               END-IF                      
               MOVE    RECE02-KANANAME     TO  RECE08-KANANAME
               MOVE    "2"                 TO  RECE08-NYUGAIKBN
               MOVE    WRK-PARA-TERMID     TO  RECE08-TERMID
               MOVE    ZERO                TO  RECE08-PRTJYUN
                                               RECE08-PRTRENNUM
               MOVE    TBL-RECE-FORMID  (IDXY)
                                           TO  RECE08-FORMID
               MOVE    TBL-RECE-PRTDATA (IDXY)
                                           TO  RECE08-PRTDATA
      *        複数枚レセの場合連番を記載
               IF      TBL-RECE-MAX        >   1
                   MOVE    SPACE               TO  WRK-HEN-PAGE-AREA
                   MOVE    TBL-RECE-RENNUM (IDXY) 
                                               TO  WRK-PAGE1
                   MOVE    "/"                 TO  WRK-PAGE-HEN
                   MOVE    TBL-RECE-MAX        TO  WRK-PAGE2
                   EVALUATE    RECE08-FORMID
      *****************WHEN    "HCM11V01.red"
      *****************WHEN    "HCM11V02.red"
      *****************WHEN    "HCM11V03.red"
                       WHEN    "HCM11V07.red"
                           MOVE    RECE08-PRTDATA  TO HC06
                           MOVE    WRK-HEN-PAGE-AREA
                                                   TO HC06-PAGE
                           MOVE    HC06            TO RECE08-PRTDATA
      *****************WHEN    "HCM12V01.red" 
                       WHEN    "HCM12V03.red"                  
                           MOVE    RECE08-PRTDATA  TO HC061
                           MOVE    WRK-HEN-PAGE-AREA
                                                   TO HC061-PAGE
                           MOVE    HC061           TO RECE08-PRTDATA
      *****************WHEN    "HCM13V01.red" 
                       WHEN    "HCM13V03.red"                  
                           MOVE    RECE08-PRTDATA  TO HC062
                           MOVE    WRK-HEN-PAGE-AREA
                                                   TO HC062-PAGE
                           MOVE    HC062           TO RECE08-PRTDATA
      *****************WHEN    "HCM14V01.red" 
                       WHEN    "HCM14V03.red"                  
                           MOVE    RECE08-PRTDATA  TO HC063
                           MOVE    WRK-HEN-PAGE-AREA
                                                   TO HC063-PAGE
                           MOVE    HC063           TO RECE08-PRTDATA
                   END-EVALUATE
               END-IF
      *
      *        医師別出力設定
               MOVE    RECE02-DRCD         TO  RECE08-DRCD
               MOVE    RECE02-DRCD-SRYKA   TO  RECE08-DRCD-SRYKA
      *        ターミナルデジット出力設定
               IF    ( SYS-25-TERMINAL-DIGIT-PRTKBN
                                           =   "1"  )
               AND   ( WRK-PARA-SRYYM  NOT =   ZERO )
               AND   ( WRK-1009-KETA       >   ZERO )
                   MOVE    RECE02-PTNUM (WRK-1009-KETA:2)
                                           TO   RECE08-SORT-2
               END-IF
     *
               MOVE    "tbl_receprt"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    RECE08-REC          TO  MCPDATA-REC
               PERFORM 900-ORCDBMAIN-SEC
      *
               IF      MCP-RC              =   ZERO
                   ADD     1                   TO  CNT-OUT
               ELSE
                   DISPLAY "ORCR0105 RECE08=" MCP-RC " " RECE08-KEY
                   STRING
                       "レセプト明細に更新できませんでした　患者番号＝"
                                               DELIMITED  BY  SIZE
                        RECE02-PTNUM           DELIMITED  BY  SIZE
                                               INTO  WRK-RECEERR
                   END-STRING
      *************PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 900-DB-ERR-ABORT-SEC
               END-IF
           END-IF
           .
       3901-RECE08-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保編綴設定処理
      *****************************************************************
       39011-HENTETU-HENSYU-SEC     SECTION.
      *
      *    検索用保険者番号
           IF      RECE08-HKNJANUM(1:2)    =   "67"
               MOVE    RECE08-HKNJANUM(3:) TO  RECE08-HKNJANUM-S
           ELSE
               MOVE    RECE08-HKNJANUM     TO  RECE08-HKNJANUM-S
           END-IF
      *
      *    県内県外印刷順
           EVALUATE    SYS-2008-PREFKBN-PRTJYUN
                                   ALSO    RECE08-PREFKBN
               WHEN    "1"         ALSO    "1"
                   MOVE    "10"            TO  RECE08-PREFKBN-SRT
               WHEN    "1"         ALSO    "2"
                   MOVE    "20"            TO  RECE08-PREFKBN-SRT
               WHEN    "2"         ALSO    "1"
                   MOVE    "20"            TO  RECE08-PREFKBN-SRT
               WHEN    "2"         ALSO    "2"
                   MOVE    "10"            TO  RECE08-PREFKBN-SRT
           END-EVALUATE
      *
      *    保険者印刷順（県内分のみ）
           IF      RECE08-PREFKBN      =   "1"
               EVALUATE    SYS-2008-HKNJANUM-PRTJYUN
                   WHEN    "1"
                       IF      RECE08-PREFNUM  =   SYS-1001-PREFNUM
                           IF      RECE08-HKNJANUM-S (3:1)  =   "3"
                               MOVE    "2" TO RECE08-PREFKBN-SRT(2:)
                           ELSE
                               MOVE    "1" TO RECE08-PREFKBN-SRT(2:)
                           END-IF
                       ELSE
                           MOVE    "3" TO  RECE08-PREFKBN-SRT (2:)
                       END-IF
                   WHEN    "2"
                       IF      RECE08-PREFNUM  =   SYS-1001-PREFNUM
                           IF      RECE08-HKNJANUM-S (3:1)  =   "3"
                               MOVE    "3" TO RECE08-PREFKBN-SRT(2:)
                           ELSE
                               MOVE    "1" TO RECE08-PREFKBN-SRT(2:)
                           END-IF
                       ELSE
                           MOVE    "2" TO  RECE08-PREFKBN-SRT (2:)
                       END-IF
                   WHEN    "3"
                       IF      RECE08-PREFNUM  =   SYS-1001-PREFNUM
                           IF      RECE08-HKNJANUM-S (3:1)  =   "3"
                               MOVE    "1" TO RECE08-PREFKBN-SRT(2:)
                           ELSE
                               MOVE    "2" TO RECE08-PREFKBN-SRT(2:)
                           END-IF
                       ELSE
                           MOVE    "3" TO  RECE08-PREFKBN-SRT (2:)
                       END-IF                               
                   WHEN    "4"
                       IF      RECE08-PREFNUM  =   SYS-1001-PREFNUM
                           IF      RECE08-HKNJANUM-S (3:1)  =   "3"
                               MOVE    "1" TO RECE08-PREFKBN-SRT(2:)
                           ELSE
                               MOVE    "3" TO RECE08-PREFKBN-SRT(2:)
                           END-IF
                       ELSE
                           MOVE    "2" TO  RECE08-PREFKBN-SRT (2:)
                       END-IF
                   WHEN    "5"
                       IF      RECE08-PREFNUM  =   SYS-1001-PREFNUM
                           IF      RECE08-HKNJANUM-S (3:1)  =   "3"
                               MOVE    "3" TO RECE08-PREFKBN-SRT(2:)
                           ELSE
                               MOVE    "2" TO RECE08-PREFKBN-SRT(2:)
                           END-IF
                       ELSE
                           MOVE    "1" TO  RECE08-PREFKBN-SRT (2:)
                       END-IF
                   WHEN    "6"
                       IF      RECE08-PREFNUM  =   SYS-1001-PREFNUM
                           IF      RECE08-HKNJANUM-S (3:1)  =   "3"
                               MOVE    "2" TO RECE08-PREFKBN-SRT(2:)
                           ELSE
                               MOVE    "3" TO RECE08-PREFKBN-SRT(2:)
                           END-IF
                       ELSE
                           MOVE    "1" TO  RECE08-PREFKBN-SRT (2:)
                       END-IF
               END-EVALUATE
           END-IF
      *
      *    所属保険者による保険者印刷区分の設定
           MOVE    "1"             TO  RECE08-HKNJANUM-SRT
           IF    ( SYS-2008-HKNJANUM   =   RECE08-HKNJANUM-S )
           AND   ( RECE08-PREFKBN      =   "1"               )
               EVALUATE    SYS-2008-HKNJANUM-PRTKBN
                   WHEN    "1"
                       MOVE    "0"     TO  RECE08-HKNJANUM-SRT
                   WHEN    "2"
                       MOVE    "9"     TO  RECE08-HKNJANUM-SRT
               END-EVALUATE
           END-IF
      *
      *    レセプト種別印刷区分（詳細設定なしのとき）
           IF      FLG-RECE-PUT-UM     =   ZERO
               EVALUATE    SYS-2008-RECESYUBETU-PRTJYUN
                                   ALSO    RECE02-RECESYUBETU-Y (2:1)
                   WHEN    "1"     ALSO    "1"
                   WHEN    "2"     ALSO    "1"
                   WHEN    "3"     ALSO    "3"
                   WHEN    "4"     ALSO    "3"
                   WHEN    "5"     ALSO    "4"
                   WHEN    "6"     ALSO    "4"
                       MOVE    "1"     TO  RECE08-RECESYUBETU-SRT (1:1)
                   WHEN    "3"     ALSO    "1"
                   WHEN    "5"     ALSO    "1"
                   WHEN    "1"     ALSO    "3"
                   WHEN    "6"     ALSO    "3"
                   WHEN    "2"     ALSO    "4"
                   WHEN    "4"     ALSO    "4"
                       MOVE    "2"     TO  RECE08-RECESYUBETU-SRT (1:1)
                   WHEN    "4"     ALSO    "1"
                   WHEN    "6"     ALSO    "1"
                   WHEN    "2"     ALSO    "3"
                   WHEN    "5"     ALSO    "3"
                   WHEN    "1"     ALSO    "4"
                   WHEN    "3"     ALSO    "4"
                       MOVE    "3"     TO  RECE08-RECESYUBETU-SRT (1:1)
               END-EVALUATE    
               MOVE    RECE02-RECESYUBETU-Y (3:2)
                                       TO  RECE08-RECESYUBETU-SRT (2:2)
               IF      RECE02-RECESYUBETU-Y (4:1)  =   "0"
                   MOVE    "A"         TO  RECE08-RECESYUBETU-SRT (3:1)
               END-IF
      *
               IF    ((SYS-2008-ZAITAKU NOT = SPACE    ) AND
                      (RECE02-ZAITAKU   =  "1"         ) AND
                      (RECE02-SRYYM    <=  "201203"    ))
               OR    ((SYS-2008-ZAITAKU NOT = SPACE    ) AND
                      (RECE02-ZAITAKU-SANTEI-FLG  = "1"))
                   MOVE    "1"              TO  RECE08-ZAITAKU
               END-IF
      *
               IF      SYS-2008-CHOUKI
                                    NOT =   SPACE
                   PERFORM VARYING IDX     FROM    1   BY  1
                           UNTIL   IDX     >       4
                           OR      RECE02-ETC-KOHNUM (IDX) =   SPACE
                       IF      RECE02-ETC-KOHNUM (IDX) =  "972" OR "974"
                           MOVE    "1"             TO  RECE08-CHOUKI
                           MOVE    4               TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
           ELSE
      *        編綴詳細設定より設定
               IF      RECE08-PREFKBN      =   "1"
                   MOVE    1                   TO  IDXX
               ELSE
                   MOVE    2                   TO  IDXX
               END-IF
      *
      *        ＨＥＡＤ情報の設定
               IF    ( RECE-PUTH-KOUGAKU-TEN (IDXX)
                                        >   ZERO                     )
               AND   ( RECE02-TOTALTEN (1)
                                        >=
                                        RECE-PUTH-KOUGAKU-TEN (IDXX) )
                   MOVE    "1"              TO  RECE08-KOUGAKU
               END-IF
      *
               IF    ((RECE-PUTH-ZAITAKU(IDXX) NOT = SPACE) AND
                      (RECE02-ZAITAKU        =    "1"     ) AND
                      (RECE02-SRYYM         <=    "201203"))
               OR    ((RECE-PUTH-ZAITAKU(IDXX) NOT = SPACE) AND
                      (RECE02-ZAITAKU-SANTEI-FLG  = "1"   ))
                   MOVE    "1"              TO  RECE08-ZAITAKU
               END-IF
      *
               IF    ( RECE02-ETC-KOHNUM (1)
                                   NOT =   SPACE )
               AND   ( RECE-PUTH-CHOUKI  (IDXX) 
                                   NOT =   SPACE )
                   PERFORM VARYING IDX     FROM    1   BY  1
                           UNTIL   IDX     >       4
                           OR      RECE02-ETC-KOHNUM (IDX) =   SPACE
                       IF      RECE02-ETC-KOHNUM (IDX) =  "972" OR "974"
                           MOVE    "1"             TO  RECE08-CHOUKI
                           MOVE    4               TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
      *
               EVALUATE    RECE-PUTH-KKHKBN (IDXX)
                   WHEN    "01"
                       IF      RECE08-HKNJANUM-S (3:1)
                                               NOT =   "3"
                           MOVE    RECE02-HONKZKKBN
                                               TO  RECE08-HONKZKKBN
                       END-IF 
                   WHEN    "02"
                       IF      RECE08-HKNJANUM-S (3:1)
                                                   =   "3"
                           MOVE    RECE02-HONKZKKBN
                                               TO  RECE08-HONKZKKBN
                       END-IF 
               END-EVALUATE
      *
      *        ＢＯＤＹ情報の設定
               MOVE    ZERO                TO  FLG-HENSYU
      *        公費からの設定
               IF      TBL-KOH-RECE-PUTB-HKNNUM (IDXX 1)
                                       NOT =   SPACE    
      *            長期
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       4
                           OR      RECE02-ETC-KOHNUM (IDX)
                                               =   SPACE
                           OR      FLG-HENSYU  =   1
                       IF      RECE02-ETC-KOHNUM (IDX) 
                                               =  "972" OR "974"
                           PERFORM VARYING IDY FROM    1   BY  1
                                   UNTIL   IDY >   WRK-CONS-KOHLIST-MAX
                                   OR      TBL-KOH-RECE-PUTB-HKNNUM
                                                           (IDXX IDY)
                                                       =   SPACE
                                   OR      FLG-HENSYU  =   1
                               MOVE    TBL-KOH-RECE-PUTB-REC (IDXX IDY)
                                                   TO  RECE-PUTB-REC
      *
                               IF    ( RECE-PUTB-KOUHI-NUM   =   "XXX" )
                               AND   ( RECE02-RECESYUBETU-Y  =
                                               RECE-PUTB-SYUBETU-GAI )
                                   PERFORM 390111-HENTETU-HENSYU-SEC
                               END-IF
                           END-PERFORM
                       END-IF
                   END-PERFORM
      *
      *            在総診
                   IF    ((FLG-HENSYU          =   ZERO    ) AND
                          (RECE02-ZAITAKU      =   "1"     ) AND
                          (RECE02-SRYYM       <=   "201203"))
                   OR    ((FLG-HENSYU          =   ZERO    ) AND
                          (RECE02-ZAITAKU-SANTEI-FLG  = "1"))
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >   WRK-CONS-KOHLIST-MAX
                               OR      TBL-KOH-RECE-PUTB-HKNNUM
                                                           (IDXX IDY)
                                                   =   SPACE
                               OR      FLG-HENSYU  =   1
                           MOVE    TBL-KOH-RECE-PUTB-REC (IDXX IDY)
                                                   TO  RECE-PUTB-REC
      *
                           IF    ( RECE-PUTB-KOUHI-NUM   =   "YYY" )
                           AND   ( RECE02-RECESYUBETU-Y  =
                                           RECE-PUTB-SYUBETU-GAI   )
                               PERFORM 390111-HENTETU-HENSYU-SEC
                           END-IF
                       END-PERFORM
                   END-IF
      *
      *            その他の公費
                   IF    ( FLG-HENSYU          =   ZERO  )
                   AND   ( RECE02-KOHNUM (1)
                                           NOT =   SPACE )
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       4
                               OR      RECE02-KOHNUM (IDX)
                                                   =   SPACE
                               OR      FLG-HENSYU  =   1
                           IF      RECE02-KOHNUM (IDX) 
                                               >=  "100"   AND <=  "899"
                               PERFORM VARYING IDY FROM    1   BY  1
                                       UNTIL   IDY >
                                               WRK-CONS-KOHLIST-MAX
                                       OR      TBL-KOH-RECE-PUTB-HKNNUM
                                                           (IDXX IDY)
                                                           =   SPACE
                                       OR      FLG-HENSYU  =   1
                                   MOVE    TBL-KOH-RECE-PUTB-REC
                                                           (IDXX IDY)
                                                   TO  RECE-PUTB-REC
                                   IF    ( RECE02-KOHNUM (IDX)   =
                                               RECE-PUTB-KOUHI-NUM   )
                                   AND   ( RECE02-RECESYUBETU-Y  =
                                               RECE-PUTB-SYUBETU-GAI )
                                       PERFORM 390111-HENTETU-HENSYU-SEC
                                   END-IF
                               END-PERFORM
      *
      *                        三歳未満のみ
                               IF      RECE02-RECESYUBETU-Y (4:1)
                                                               =   "4"
                                   PERFORM VARYING IDY FROM    1   BY  1
                                           UNTIL   IDY >
                                               WRK-CONS-KOHLIST-MAX
                                           OR  TBL-KOH-RECE-PUTB-HKNNUM
                                                           (IDXX IDY)
                                                               =   SPACE
                                           OR      FLG-HENSYU  =   1
                                       MOVE    TBL-KOH-RECE-PUTB-REC
                                                           (IDXX IDY)
                                                   TO  RECE-PUTB-REC
                                       IF    ( RECE02-KOHNUM (IDX) =
                                               RECE-PUTB-KOUHI-NUM   )
                                       AND   ( RECE02-RECESYUBETU-Y(2:1)
                                                                  =
                                           RECE-PUTB-SYUBETU-GAI (2:1) )
                                           PERFORM
                                               390111-HENTETU-HENSYU-SEC
                                       END-IF
                                   END-PERFORM
                               END-IF
                           END-IF    
                       END-PERFORM
                   END-IF
               END-IF
      *
      *        公費からの設定がなかったとき
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       WRK-CONS-BODYLIST-MAX
                       OR      TBL-SYUBETU-RECE-PUTB-SYUBETU
                                                       (IDXX IDY)
                                           =   SPACE
                       OR      FLG-HENSYU  =   1
                   MOVE    TBL-SYUBETU-RECE-PUTB-REC (IDXX IDY)
                                           TO  RECE-PUTB-REC
                   IF      RECE02-RECESYUBETU-Y    =
                                               RECE-PUTB-SYUBETU-GAI
                       PERFORM 390111-HENTETU-HENSYU-SEC
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       39011-HENTETU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    詳細設定情報出力処理
      *****************************************************************
       390111-HENTETU-HENSYU-SEC   SECTION.
      *
           MOVE    RECE-PUTB-SYUBETU-SRT1
                                       TO  RECE08-SYUBETU-SRT1
           MOVE    RECE-PUTB-SYUBETU-SRT2
                                       TO  RECE08-SYUBETU-SRT2
           IF       RECE-PUTB-KOUHI-NO >   ZERO
               MOVE    RECE-PUTB-KOUHI-NO  TO  RECE08-KOUHI-NO
           ELSE
               MOVE    500                 TO  RECE08-KOUHI-NO
           END-IF
           MOVE    RECE-PUTB-GRP-NO1   TO  RECE08-GRP-NO1
           MOVE    RECE-PUTB-GRP-NO2   TO  RECE08-GRP-NO2
           EVALUATE    RECE-PUTB-SYUBETU-UNDER-NO1 
               WHEN    "01"
                   COMPUTE WRK-KYURATE-9   =   100 -   RECE02-KYURATE
                   MOVE    WRK-KYURATE-X   TO  RECE08-SYUBETU-UNDER-1
               WHEN    "02"
                   MOVE    RECE02-KYURATE  TO  RECE08-SYUBETU-UNDER-1
               WHEN    "03"
                   MOVE    RECE02-KOHHBTNUM(1)
                                           TO  RECE08-SYUBETU-UNDER-1
               WHEN    "04"
                   IF      RECE02-KOHHBTNUM(1) NUMERIC
                       MOVE    RECE02-KOHHBTNUM(1) TO
                                                   WRK-RECE02-HBTNUM-X
                   ELSE
                       MOVE    "00"                TO
                                                   WRK-RECE02-HBTNUM-X
                   END-IF
                   COMPUTE WRK-HBTNUM-9    =   100 -
                                               WRK-RECE02-HBTNUM-9
                   MOVE    WRK-HBTNUM-X    TO  RECE08-SYUBETU-UNDER-1
               WHEN    "05"
                   MOVE    RECE02-KIGO     TO   RECE08-SYUBETU-UNDER-1-1
                   MOVE    RECE02-NUM      TO   RECE08-SYUBETU-UNDER-1-2
               WHEN    "06"
                   MOVE    RECE02-KOHJKYSNUM (1)
                                           TO  RECE08-SYUBETU-UNDER-1
               WHEN    "07"
                   MOVE    RECE08-HKNJANUM-S
                                           TO  RECE08-SYUBETU-UNDER-1
          END-EVALUATE
           EVALUATE    RECE-PUTB-SYUBETU-UNDER-NO2
               WHEN    "01"
                   COMPUTE WRK-KYURATE-9   =   100 -   RECE02-KYURATE
                   MOVE    WRK-KYURATE-X   TO  RECE08-SYUBETU-UNDER-2
               WHEN    "02"
                   MOVE    RECE02-KYURATE  TO  RECE08-SYUBETU-UNDER-2
               WHEN    "03"
                   MOVE    RECE02-KOHHBTNUM(1)
                                           TO  RECE08-SYUBETU-UNDER-2
               WHEN    "04"
                   IF      RECE02-KOHHBTNUM(1) NUMERIC
                       MOVE    RECE02-KOHHBTNUM(1) TO
                                                   WRK-RECE02-HBTNUM-X
                   ELSE
                       MOVE    "00"                TO
                                                   WRK-RECE02-HBTNUM-X
                   END-IF
                   COMPUTE WRK-HBTNUM-9    =   100 -
                                               WRK-RECE02-HBTNUM-9
                   MOVE    WRK-HBTNUM-X    TO  RECE08-SYUBETU-UNDER-2
          END-EVALUATE
      *
          IF      RECE-PUTH-SRT-01 (IDXX) =   "02"
              MOVE    RECE-PUTB-SYUBETU-SRT1
                                          TO  WRK-RECE-PUTB-SYUBETU-SRT1
              MOVE    WRK-RECE-PUTB-SYUBETU-SRT1-X
                                          TO  RECE08-HKNJANUM-S
              MOVE    RECE-PUTB-SYUBETU-GAI-SRT1
                                          TO  WRK-SYUBETU-SRT1
          ELSE
              MOVE    SPACE               TO  WRK-SYUBETU-SRT1  
          END-IF
      *
          MOVE    1                    TO  FLG-HENSYU
      *
           .
       390111-HENTETU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    社保編綴設定処理
      *****************************************************************
       39012-SYAHO-HENTETU-HENSYU-SEC     SECTION.
      *
      *    ２００８年３月以前は設定しない
           IF      RECE02-SRYYM        <=  "200803"
               GO  TO  39012-SYAHO-HENTETU-HENSYU-EXT
           END-IF
      *
           EVALUATE    RECE08-RECESYUBETU
               WHEN    "1128"
               WHEN    "1138"
                   MOVE    "010"   TO  RECE08-RECESYUBETU-SRT
               WHEN    "1118"
                   MOVE    "020"   TO  RECE08-RECESYUBETU-SRT
               WHEN    "1120"
               WHEN    "1130"
                   MOVE    "030"   TO  RECE08-RECESYUBETU-SRT
               WHEN    "1110"
                   MOVE    "040"   TO  RECE08-RECESYUBETU-SRT
               WHEN    "1122"
               WHEN    "1132"
                   MOVE    "050"   TO  RECE08-RECESYUBETU-SRT
               WHEN    "1112"
                   MOVE    "060"   TO  RECE08-RECESYUBETU-SRT
               WHEN    "1126"
               WHEN    "1136"
                   MOVE    "070"   TO  RECE08-RECESYUBETU-SRT
               WHEN    "1116"
                   MOVE    "080"   TO  RECE08-RECESYUBETU-SRT
               WHEN    "1124"
               WHEN    "1134"
                   MOVE    "090"   TO  RECE08-RECESYUBETU-SRT
               WHEN    "1114"
                   MOVE    "100"   TO  RECE08-RECESYUBETU-SRT
               WHEN    "1222"
               WHEN    "1232"
                   MOVE    "110"   TO  RECE08-RECESYUBETU-SRT
               WHEN    "1212"
                   MOVE    "120"   TO  RECE08-RECESYUBETU-SRT
           END-EVALUATE    
      *
      *****EVALUATE    RECE08-RECESYUBETU
      *****    WHEN    "1127"
      *****        MOVE    1       TO  RECE08-SYUBETU-SRT1
      *****END-EVALUATE    
      *
      *    医保単独分の設定   
           IF    (  RECE02-HKNNUM  NOT =   SPACE )
           AND   (  RECE02-KOHNUM (1)  =   SPACE )
      *        保険による設定   
               EVALUATE    RECE02-HKNNUM
                   WHEN    "001"
                       MOVE    1           TO  RECE08-GRP-NO1
                       MOVE    "001"       TO  RECE08-GRP-NO2
                   WHEN    "009"
                       MOVE    1           TO  RECE08-GRP-NO1
                       MOVE    "002"       TO  RECE08-GRP-NO2
                   WHEN    "002"
                       MOVE    2           TO  RECE08-GRP-NO1
                       EVALUATE    RECE02-HOJOKBN
                           WHEN    "A"
                           WHEN    "B"
                           WHEN    "C"
                           WHEN    "G"
                           WHEN    "H"
                           WHEN    "I"
                           WHEN    "1"
                           WHEN    "2"
                           WHEN    "3"
                               MOVE    "001"   TO  RECE08-GRP-NO2
                           WHEN    OTHER
                               MOVE    "002"   TO  RECE08-GRP-NO2
                       END-EVALUATE        
                   WHEN    "003"
                       MOVE    3           TO  RECE08-GRP-NO1
                       MOVE    "001"       TO  RECE08-GRP-NO2
                   WHEN    "004"
                       MOVE    3           TO  RECE08-GRP-NO1
                       MOVE    "002"       TO  RECE08-GRP-NO2
                   WHEN    "031"  THRU  "034"
                       MOVE    4           TO  RECE08-GRP-NO1
                       EVALUATE    RECE02-HOJOKBN
                           WHEN    "B"
                           WHEN    "H"
                           WHEN    "2"
                               MOVE    "001"   TO  RECE08-GRP-NO2
                           WHEN    OTHER
                               MOVE    "002"   TO  RECE08-GRP-NO2
                       END-EVALUATE        
                   WHEN    "006"
                       MOVE    5           TO  RECE08-GRP-NO1
                   WHEN    "007"
                       MOVE    6           TO  RECE08-GRP-NO1
                   WHEN    "063"
                   WHEN    "072"  THRU  "075"
                       MOVE    7           TO  RECE08-GRP-NO1
               END-EVALUATE
      *
      *        長期の設定   
               IF    ( RECE02-ETC-KOHNUM (1)
                                       NOT =   SPACE )
                   PERFORM VARYING IDX     FROM    1   BY  1
                           UNTIL   IDX     >       4
                           OR      RECE02-ETC-KOHNUM (IDX) =   SPACE
                       IF      RECE02-ETC-KOHNUM (IDX) =  "972" OR "974"
                           MOVE    "1"             TO  RECE08-CHOUKI
                           MOVE    4               TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
      *    医保と公費併用分の第一公費の設定   
           IF    (  RECE02-HKNNUM  NOT =   SPACE )
           AND   (  RECE02-KOHNUM (1)
                                   NOT =   SPACE )
               MOVE    RECE02-KOHNUM (1)   TO  RECE08-KOUHI-NO
      *********MOVE    RECE02-FTNJANUM(1)  TO  RECE08-SYUBETU-SRT2
           END-IF
      *
      *    在総診等
           IF    ((RECE02-ZAITAKU   =   "1"        ) AND
                  (RECE02-SRYYM    <=  "201203"    ))
      *****OR    ( RECE02-ZAITAKU-SANTEI-FLG  = "1" )
               MOVE    "1"              TO  RECE08-ZAITAKU
           END-IF
      *
      *    検索用保険者番号（直接請求する健保組合）
           IF      SYS-200505-HKNJANUM (1)
                               NOT =   SPACE
               MOVE    "8"             TO  RECE08-HKNJANUM-SRT
      *
               IF       RECE08-HKNJANUM (1:2)
                                        =   "06"    OR  "63"    OR
                                            "32"
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       5
                           OR      SYS-200505-HKNJANUM (IDX)
                                       =   SPACE
                       IF      RECE08-HKNJANUM =
                                               SYS-200505-HKNJANUM (IDX)
                                           OR  SYS-200505-HKNJANUM-63
                                                                   (IDX)
                           MOVE    SYS-200505-HKNJANUM (IDX)
                                                   TO  RECE08-HKNJANUM-S
                           MOVE    5               TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
     *
           .
       39012-SYAHO-HENTETU-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    出力処理
      *****************************************************************
       3903-RECE07-WRITE-SEC        SECTION.
      *
           INITIALIZE                      RECE07-REC
           MOVE    RECE08-HOSPNUM      TO  RECE07-HOSPNUM
           MOVE    RECE08-PRTID        TO  RECE07-PRTID
           IF      WRK-PARA-SRYYM      =   ZERO
               MOVE    "2"                 TO  RECE07-SYOKBN
           ELSE
               MOVE    "1"                 TO  RECE07-SYOKBN
           END-IF         
           MOVE    WRK-PARA-RECEYM     TO  RECE07-SRYYM
           MOVE    RECE08-CREYMD       TO  RECE07-CREYMD
           MOVE    RECE08-CREHMS       TO  RECE07-CREHMS
           MOVE    RECE08-TEISYUTUSAKI TO  RECE07-TEISYUTUSAKI
           MOVE    RECE08-RECESYUBETU  TO  RECE07-RECESYUBETU
           MOVE    RECE08-NYUGAIKBN    TO  RECE07-NYUGAIKBN
           MOVE    RECE08-SRYKA        TO  RECE07-SRYKA
           IF      RECE08-TEISYUTUSAKI =   "1"
      ******** MOVE    "99999999"          TO  RECE08-HKNJANUM-S
               MOVE    "999"               TO  RECE08-RECESYUBETU-SRT
               MOVE    "99"                TO  RECE08-PREFKBN-SRT
      ******** MOVE    "9"                 TO  RECE08-HKNJANUM-SRT
               MOVE    "9"                 TO  RECE08-ZAITAKU
                                               RECE08-CHOUKI
               MOVE    RECE08-HKNJANUM-S   TO  RECE07-HKNJANUM
               MOVE    RECE08-PREFKBN-SRT  TO  RECE07-PREFKBN-SRT
               MOVE    RECE08-ZAITAKU      TO  RECE07-ZAITAKU
               MOVE    RECE08-CHOUKI       TO  RECE07-CHOUKI
               MOVE    RECE08-HKNJANUM-SRT TO  RECE07-HKNJANUM-SRT
               MOVE    RECE08-RECESYUBETU-SRT
                                           TO  RECE07-RECESYUBETU-SRT
           ELSE   
               MOVE    RECE08-HKNJANUM-S   TO  RECE07-HKNJANUM
               MOVE    RECE08-PREFKBN-SRT  TO  RECE07-PREFKBN-SRT
               MOVE    RECE08-ZAITAKU      TO  RECE07-ZAITAKU
               MOVE    RECE08-CHOUKI       TO  RECE07-CHOUKI
               MOVE    RECE08-HKNJANUM-SRT TO  RECE07-HKNJANUM-SRT
               MOVE    RECE08-RECESYUBETU-SRT
                                           TO  RECE07-RECESYUBETU-SRT
           END-IF
           MOVE    RECE07-REC          TO  MCPDATA-REC
           MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
           MOVE    "key4"              TO  WRK-MCP-PATHNAME
           PERFORM 900-RECE07-READ-SEC
           IF      FLG-RECE07          =   1
               INITIALIZE                      RECE07-REC
               MOVE    RECE08-HOSPNUM      TO  RECE07-HOSPNUM
               MOVE    RECE08-PRTID        TO  RECE07-PRTID
               IF      WRK-PARA-SRYYM      =   ZERO
                   MOVE    "2"                 TO  RECE07-SYOKBN
               ELSE
                   MOVE    "1"                 TO  RECE07-SYOKBN
               END-IF         
               MOVE    WRK-PARA-RECEYM     TO  RECE07-SRYYM
               MOVE    RECE08-CREYMD       TO  RECE07-CREYMD
               MOVE    RECE08-CREHMS       TO  RECE07-CREHMS
               MOVE    RECE08-TEISYUTUSAKI TO  RECE07-TEISYUTUSAKI
               MOVE    RECE08-RECESYUBETU  TO  RECE07-RECESYUBETU
               MOVE    WRK-PARA-RECEKBN    TO  RECE07-RECEKBN
               MOVE    RECE08-NYUGAIKBN    TO  RECE07-NYUGAIKBN
               MOVE    RECE08-HKNJANUM-S   TO  RECE07-HKNJANUM
               MOVE    RECE08-PREFKBN-SRT  TO  RECE07-PREFKBN-SRT
               MOVE    RECE08-ZAITAKU      TO  RECE07-ZAITAKU
               MOVE    RECE08-CHOUKI       TO  RECE07-CHOUKI
               MOVE    RECE08-KOUGAKU      TO  RECE07-KOUGAKU
               MOVE    RECE08-SRYKA        TO  RECE07-SRYKA
               MOVE    RECE08-HKNJANUM-SRT TO  RECE07-HKNJANUM-SRT
               MOVE    RECE08-RECESYUBETU-SRT
                                           TO  RECE07-RECESYUBETU-SRT
               MOVE    RECE08-TERMID       TO  RECE07-TERMID
               MOVE    RECE08-OPID         TO  RECE07-OPID
               IF      WRK-PARA-SYOKBN     =   "2"
                   EVALUATE    BTPARA-INFO-KBN
                       WHEN    "01"
                           MOVE    ZERO            TO  RECE07-SYORISBT
                       WHEN    "02"
                           MOVE    "1"             TO  RECE07-SYORISBT
                           MOVE    BTPARA-INFO-PARA (17:6)
                                                   TO  RECE07-STYMD
                       WHEN    "03"
                           MOVE    "2"             TO  RECE07-SYORISBT
                           MOVE    BTPARA-INFO-PARA (1:8)
                                                   TO  RECE07-STYMD
                           MOVE    BTPARA-INFO-PARA (9:8)
                                                   TO  RECE07-EDYMD
                       WHEN    "04"
                           MOVE    "3"             TO  RECE07-SYORISBT
                           MOVE    BTPARA-INFO-PARA (1:6)
                                                   TO  RECE07-STYMD
                       WHEN    "05"
                           MOVE    "4"             TO  RECE07-SYORISBT
                   END-EVALUATE
               END-IF 
               EVALUATE    WRK-PARA-PRTKBN
                   WHEN    "2"
                       MOVE    "1"                 TO  RECE07-PRTKBN
                   WHEN    "3"
                       MOVE    "2"                 TO  RECE07-PRTKBN
                   WHEN    "4"
                       MOVE    "3"                 TO  RECE07-PRTKBN
                   WHEN    "5"
                       MOVE    "4"                 TO  RECE07-PRTKBN
               END-EVALUATE
               MOVE    1                   TO  RECE07-PAGE
               MOVE    1                   TO  RECE07-KENSU
               IF      RECE08-ZAITAKU      =   "1"
                   ADD     1               TO  RECE07-PAGE-ZAITAKU
               END-IF
               IF      RECE08-CHOUKI       =   "1"
                   ADD     1               TO  RECE07-PAGE-CHOUKI
               END-IF
               IF      RECE08-KOUGAKU      =   "1"  
                   ADD     1               TO  RECE07-PAGE-KOUGAKU
               END-IF
               IF    ( RECE08-CHOUKI       =   "1" )  
               AND   ( RECE08-ZAITAKU      =   "1" )
                   ADD     1               TO  RECE07-PAGE-DUP
               END-IF
               MOVE    SYS-2008-KBNCD      TO  WRK-HSEQNUM
               MOVE    WRK-HSEQNUM         TO  RECE07-HSEQNUM
      *        編綴順項目の設定（国保特別療養費以外）
               IF    ( RECE02-TEISYUTUSAKI =   "1"    )
               OR    ( RECE02-RECESYUBETU  =   "6666"  OR  "8888"  OR
                                               "0000" )
               OR    ( RECE02-ERR-ERRKBN   >   ZERO   )
                   CONTINUE
               ELSE
                   IF      FLG-RECE-PUT-UM     =   1
                       MOVE    99999999            TO  RECE07-HSEQNUM
      *                編綴詳細設定より設定
                       IF      RECE08-PREFKBN      =   "1"
                           MOVE    1                   TO  IDXX
                       ELSE
                           MOVE    2                   TO  IDXX
                       END-IF
                       MOVE    RECE08-PREFKBN  TO  RECE07-PREFKBN
                       MOVE    RECE-PUTH-ZAITAKU     (IDXX) 
                                               TO  RECE07-ZAITAKU-KBN
                       MOVE    RECE-PUTH-CHOUKI      (IDXX)
                                               TO  RECE07-CHOUKI-KBN
                       MOVE    RECE-PUTH-SRT-LAST    (IDXX)
                                               TO  RECE07-SRT-LAST
                       MOVE    RECE-PUTH-KOUGAKU-TEN    (IDXX)
                                               TO  RECE07-KOUGAKU-TEN
                   END-IF
               END-IF    
           ELSE
               ADD     1                   TO  RECE07-PAGE
               IF    ( CNT-KENSU           =   1 )
               AND   ( TBL-RECE-RENNUM (IDXY)
                                           =   1 )
                   ADD     1                   TO  RECE07-KENSU
                   IF      RECE08-ZAITAKU      =   "1"
                       ADD     1               TO  RECE07-PAGE-ZAITAKU
                   END-IF
                   IF      RECE08-CHOUKI       =   "1"  
                       ADD     1               TO  RECE07-PAGE-CHOUKI
                   END-IF
                   IF      RECE08-KOUGAKU      =   "1"  
                       ADD     1               TO  RECE07-PAGE-KOUGAKU
                   END-IF
                   IF    ( RECE08-CHOUKI       =   "1" )  
                   AND   ( RECE08-ZAITAKU      =   "1" )
                       ADD     1               TO  RECE07-PAGE-DUP
                   END-IF
               END-IF
           END-IF
           IF    WRK-SYUBETU-SRT1  NOT =   SPACE
              MOVE    WRK-SYUBETU-SRT1     TO  RECE07-SYUBETU-SRT1
           END-IF   
      *
           MOVE    RECE07-REC          TO  MCPDATA-REC
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-RECE07          =   1    
               MOVE    "DBINSERT"          TO  MCP-FUNC 
           ELSE
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           END-IF 
           PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "ORCR0105 RECEKANRI MCP-RC= "  MCP-RC
               IF      FLG-RECE07          =   1
               MOVE    "レセプト管理ＤＢに追加できませんでした"
                                               TO  WRK-RECEERR
               ELSE
               MOVE    "レセプト管理ＤＢに更新できませんでした"
                                               TO  WRK-RECEERR
               END-IF
      *********PERFORM 500-ERR-HENSYU-SEC
               PERFORM 900-DB-ERR-ABORT-SEC
           END-IF
           .
       3903-RECE07-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理ＤＢ更新
      *****************************************************************
       3904-RECE10-HENSYU-SEC        SECTION.
      * 
      *    テスト患者のとき更新しない
           IF      RECE02-TSTPTNUMKBN   =   "1"
               GO  TO  3904-RECE10-HENSYU-EXT
           END-IF
      * 
      *    レセプト種別不明または自費のとき更新しない
           IF    ( RECE02-RECESYUBETU      =   ZERO    OR  8888 )
           OR    ( RECE02-ERR-ERRKBN       >   ZERO             )
               GO  TO  3904-RECE10-HENSYU-EXT
           END-IF
      *
      *     主科設定のとき未設定なら更新しない
           IF    ( SYS-2009-SYUKA-KBN  =   "1"  )
           AND   ( RECE02-SYUKA        =   "XX" )
               IF  (     ( RECE02-TEISYUTUSAKI   =   "1" )
                     AND ( SYS-2009-SYUKA-SYAHO  =   "1" ) )
               OR  (     ( RECE02-TEISYUTUSAKI   =   "2" )
                     AND ( SYS-2009-SYUKA-KOKUHO =   "1" ) )
               OR  (     ( RECE02-TEISYUTUSAKI   =   "6" )
                     AND ( SYS-2009-SYUKA-KOUIKI =   "1" ) )
                   GO  TO  3904-RECE10-HENSYU-EXT
               END-IF
           END-IF
      *          
           INITIALIZE                  RECE10-REC
           MOVE    RECE02-HOSPNUM      TO  RECE10-HOSPNUM
           MOVE    RECE02-SRYYM        TO  RECE10-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE10-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE10-PTID
           IF    ( SYS-2009-SYUKA-KBN  =   "1"    )
               IF  (     ( RECE02-TEISYUTUSAKI   =   "1" )
                     AND ( SYS-2009-SYUKA-SYAHO  =   "1" ) )
               OR  (     ( RECE02-TEISYUTUSAKI   =   "2" )
                     AND ( SYS-2009-SYUKA-KOKUHO =   "1" ) )
               OR  (     ( RECE02-TEISYUTUSAKI   =   "6" )
                     AND ( SYS-2009-SYUKA-KOUIKI =   "1" ) )
                   MOVE    RECE02-SYUKA        TO  RECE10-RECEKA
               ELSE
                   IF      RECE02-RECEKA       =   SPACE
                       MOVE    ZERO            TO  RECE10-RECEKA
                   ELSE
                       MOVE    RECE02-RECEKA   TO  RECE10-RECEKA
                   END-IF
               END-IF
           ELSE
               IF      RECE02-RECEKA       =   SPACE
                   MOVE    ZERO            TO  RECE10-RECEKA
               ELSE
                   MOVE    RECE02-RECEKA   TO  RECE10-RECEKA
               END-IF
           END-IF
           MOVE    RECE02-TEISYUTUSAKI TO  RECE10-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE10-RECESYUBETU
           IF      RECE02-HKNJANUM-KEY =   SPACE
               MOVE    ZERO                TO  RECE10-HKNJANUM
           ELSE
               MOVE    RECE02-HKNJANUM-KEY TO  RECE10-HKNJANUM
           END-IF
           IF      RECE02-HOJOKBN-KEY      =   SPACE
               MOVE    ZERO                TO  RECE10-HOJOKBN-KEY
           ELSE
               MOVE    RECE02-HOJOKBN-KEY  TO  RECE10-HOJOKBN-KEY
           END-IF   
           MOVE    RECE02-KOHID-KEY    TO  RECE10-KOHID-KEY
           IF      RECE02-TEKSTYMD     =   SPACE
               MOVE    ZERO            TO  RECE10-TEKSTYMD
           ELSE
               MOVE    RECE02-TEKSTYMD TO  RECE10-TEKSTYMD
           END-IF
           MOVE    RECE02-RECEIPT-KBN  TO  RECE10-RECEIPT-KBN
      *
           MOVE    "tbl_seikyu"        TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM 900-SEIKYU-INV-SEC
           IF      FLG-SEIKYU          =   ZERO
               MOVE    MCPDATA-REC         TO  RECE10-REC
               MOVE    WRK-PARA-CREYMD     TO  RECE10-UPDYMD
               MOVE    WRK-PARA-CREHMS (1:6)
                                           TO  RECE10-UPDHMS
               IF      RECE10-SKYKBN       =   "2"
                   MOVE    "1"                 TO  RECE10-SKYKBN
               END-IF 
               MOVE    "2"                 TO  RECE10-TEISEIKBN
           ELSE
               MOVE    "1"                 TO  RECE10-SKYKBN
               MOVE    RECE02-PTNUM        TO  RECE10-PTNUM
               MOVE    RECE02-HKNNUM       TO  RECE10-HKNNUM
                                               RECE10-HKNNUM-S
               MOVE    RECE02-HKNID        TO  RECE10-HKNID
               MOVE    RECE02-HONKZKKBN    TO  RECE10-HONKZKKBN
               MOVE    RECE02-HOJOKBN      TO  RECE10-HOJOKBN
               MOVE    RECE02-CONTKBN      TO  RECE10-CONTKBN
               MOVE    ZERO                TO  RECE10-SKYYM
                                               RECE10-HENREIYM
                                               RECE10-TEISEIKBN
               MOVE    RECE02-HKNNUM   TO  RECE10-HKNNUM-S
               IF      RECE02-HKNJANUM-KEY =   SPACE
                   MOVE    ZERO            TO  RECE10-HKNJANUM-S
               ELSE
                   IF      RECE02-HKNJANUM-KEY(1:2)    =   "67"
                       MOVE    RECE02-HKNJANUM-KEY(3:)
                                           TO  RECE10-HKNJANUM-S
                       IF      RECE02-RECESYUBETU  =    "6666"
                           MOVE    "069"       TO  RECE10-HKNNUM-S
                       ELSE
                           MOVE    "067"       TO  RECE10-HKNNUM-S
                       END-IF
                   ELSE
                       MOVE    RECE02-HKNJANUM-KEY
                                           TO  RECE10-HKNJANUM-S
                   END-IF
               END-IF
               MOVE    RECE08-PREFKBN      TO  RECE10-PREFKBN
               MOVE    RECE08-PREFNUM      TO  RECE10-PREFNUM
               MOVE    WRK-PARA-CREYMD     TO  RECE10-CREYMD
               MOVE    WRK-PARA-CREHMS (1:6)
                                           TO  RECE10-CREHMS
               MOVE    ZERO                TO  RECE10-UPDYMD
                                               RECE10-UPDHMS
           END-IF
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
               MOVE    RECE02-KOHNUM (IDX) TO
                                       RECE10-KOHNUM (IDX)
               MOVE    RECE02-KOHID  (IDX) TO
                                       RECE10-KOHID  (IDX)
           END-PERFORM
      *
           MOVE    RECE02-KIGO         TO  RECE10-KIGO
           MOVE    RECE02-NUM          TO  RECE10-NUM
      *
      *    治験は請求区分を０：請求しないとする
           IF      RECE10-HKNNUM       >=  "900"
           AND     RECE10-HKNNUM       <=  "969"
                   MOVE    ZERO            TO  RECE10-SKYKBN
           END-IF
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       5
               MOVE    RECE02-JNISSUINF (IDX)  TO
                                       RECE10-JNISSUINF (IDX)
               MOVE    RECE02-TOTALTENINF (IDX)    TO 
                                       RECE10-TOTALTENINF (IDX)
               MOVE    RECE02-FTNMONEYINF(IDX) TO
                                       RECE10-FTNMONEYINF (IDX)
           END-PERFORM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       45
               MOVE    RECE02-SRYKBNINF (IDX)  TO
                                       RECE10-SRYKBNINF (IDX)
           END-PERFORM
      *    給付割合
           IF      RECE02-KYURATE  >   ZERO
               COMPUTE WRK-KYURATE-9   =
                              (100     -   RECE02-KYURATE)
                                               / 10 
               MOVE    WRK-KYURATE-9   TO  RECE10-KYURATE
           ELSE
               IF      RECE02-TEISYUTUSAKI =   "1"
                   MOVE    ZERO            TO  RECE10-KYURATE
               ELSE
                   MOVE    10              TO  RECE10-KYURATE
               END-IF        
           END-IF
      *     
           MOVE    RECE02-AGE          TO  RECE10-AGE 
           MOVE    RECE02-SYUGAKUKBN   TO  RECE10-FLG (1)
      *    ７０歳代前半の特例措置対象者のみ２割の記載
           IF      RECE02-RJNGAIKOHFTN (2) =   1
               MOVE    1                   TO  RECE10-FLG (2)
           END-IF
      *    ７５歳到達月の特例対象者
           IF      RECE02-RJNNYUKOHFTN (1)  NOT =  ZERO
               MOVE   RECE02-RJNNYUKOHFTN (1)  TO  RECE10-FLG (3)
           END-IF
      *    特定疾患等対象者
           IF      RECE02-RJNNYUKOHFTN (2)  NOT =  ZERO
               MOVE   RECE02-RJNNYUKOHFTN (2)  TO  RECE10-FLG (4)
           END-IF
      *    公費請求一部負担金置き換え
           IF      RECE02-RJNNYUKOHFTN (3)  NOT =  ZERO
               MOVE   RECE02-RJNNYUKOHFTN (3)  TO  RECE10-FLG (5)
           END-IF
      *
           IF    ((RECE02-ZAITAKU  =  "1"          ) AND
                  (RECE02-SRYYM   <=  "201203"     ))
           OR    ((RECE02-ZAITAKU-SANTEI-FLG  = "1") AND
                  (RECE02-TEISYUTUSAKI   NOT  = "1"))
               MOVE    "1"    TO   RECE10-ZAITAKU
           END-IF
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
                   OR      RECE02-ETC-KOHNUM (IDX) =   SPACE
               MOVE    RECE02-ETC-KOHNUM   (IDX)   TO
                                       RECE10-ETC-KOHNUM   (IDX)
               MOVE    RECE02-ETC-JNISSU   (IDX)   TO
                                       RECE10-ETC-JNISSU   (IDX)
               MOVE    RECE02-ETC-TOTALTEN (IDX)   TO
                                       RECE10-ETC-TOTALTEN (IDX)
               MOVE    RECE02-ETC-FTNMONEY (IDX)   TO
                                       RECE10-ETC-FTNMONEY (IDX)
               MOVE    RECE02-ETC-FTNMONEY-FTN (IDX)   TO
                                       RECE10-ETC-FTNMONEY-FTN (IDX)
               MOVE    RECE02-ETC-FTNMONEY-KOJ (IDX)   TO
                                       RECE10-ETC-FTNMONEY-KOJ (IDX)
           END-PERFORM
      *    一部負担金・食事療養費標準負担額区分
           MOVE    RECE02-SHOKUJIFTNKBN
                                       TO  RECE10-SHOKUJIFTNKBN
      *
      *    特記事項
           INITIALIZE                  WRK-TOKKI-AREA
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( IDX                 >   5     )
                   OR    ( RECE02-TOKKI (IDX)  =   SPACE )
               MOVE    SPACE               TO  WRK-TOKKI-X
               MOVE    RECE02-TOKKI (IDX)  TO  WRK-TOKKI
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL ( IDY             >   TBL-TOKKI-MAX  )
                   IF      RECE02-TOKKI(IDX)   =   TBL-TOKKICD(IDY)
                       MOVE    TBL-TOKKINAME(IDY)
                                                TO  WRK-TOKKINM
                       MOVE    TBL-TOKKI-MAX    TO  IDY
                   END-IF
               END-PERFORM
               PERFORM 39041-RECE10-TOKKI-HENSYU-SEC
           END-PERFORM
      *
      *    特記事項（患者特記事項情報）
           IF      RECE02-PTTOKKI-AREA     NOT =   SPACE
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       5
                       OR      RECE02-PTTOKKI-NAIYO (IDX)
                                               =   SPACE
                   MOVE    RECE02-PTTOKKI-NAIYO (IDX) 
                                               TO  WRK-TOKKI-X
                   MOVE    SPACE               TO  WRK-TOKKI
                                                   WRK-TOKKINM
                   PERFORM 39041-RECE10-TOKKI-HENSYU-SEC
               END-PERFORM
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       5
                       OR      RECE02-PTTOKKI-CD (IDX)
                                               =   SPACE
                   MOVE    SPACE               TO  WRK-TOKKI-X
                                                   WRK-TOKKINM
                   MOVE    RECE02-PTTOKKI-CD (IDX) 
                                               TO  WRK-TOKKI
                   PERFORM 39041-RECE10-TOKKI-HENSYU-SEC
               END-PERFORM
           END-IF
      *
      *    特記事項（レセプト特記事項編集情報）
           IF      RECE02-2006-TOKKIINF    NOT =   SPACE
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       4
                   IF      RECE02-2006-TOKKI (IDX)
                                           NOT =   SPACE
                       MOVE    RECE02-2006-TOKKI (IDX) 
                                                   TO  WRK-TOKKI-X
                       MOVE    SPACE               TO  WRK-TOKKI
                                                       WRK-TOKKINM
                       PERFORM 39041-RECE10-TOKKI-HENSYU-SEC
                   END-IF
               END-PERFORM
           END-IF
           IF      RECE02-2006-RECEDEN-TOKKIINF
                                           NOT =   SPACE
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       5
                   IF      RECE02-2006-RECEDEN-TOKKI (IDX)
                                           NOT =   SPACE
                       MOVE    SPACE               TO  WRK-TOKKI-X
                                                       WRK-TOKKINM
                       MOVE    RECE02-2006-RECEDEN-TOKKI (IDX) 
                                                   TO  WRK-TOKKI
                       PERFORM 39041-RECE10-TOKKI-HENSYU-SEC
                   END-IF
               END-PERFORM
           END-IF
      *
      *    特記事項（県単用項目）
           IF      FLG-RECE11           =   ZERO
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       4
                   IF      RECE11-HC06-TOKKI (IDX)
                                           NOT =   SPACE
                       MOVE    RECE11-HC06-TOKKI (IDX)
                                                   TO  WRK-TOKKI-X
                       MOVE    SPACE               TO  WRK-TOKKI
                                                       WRK-TOKKINM
                       PERFORM 39041-RECE10-TOKKI-HENSYU-SEC
                   END-IF
               END-PERFORM
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       5
                   IF      RECE11-TOKKI (IDX)
                                           NOT =   SPACE
                       MOVE    SPACE               TO  WRK-TOKKI-X
                                                       WRK-TOKKINM
                       MOVE    RECE11-TOKKI (IDX)  TO  WRK-TOKKI
                       PERFORM 39041-RECE10-TOKKI-HENSYU-SEC
                   END-IF
               END-PERFORM
           END-IF
      *
      *    特記事項コードのある特記事項から編集する
           MOVE    ZERO            TO  IDY
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       10
                   OR      WRK-TOKKI-OCC (IDX)
                                   =   SPACE
                   OR      IDY     =   5
               IF      WRK-TOKKI-O (IDX)
                                   NOT =   SPACE
                   ADD     1                   TO  IDY
                   MOVE    WRK-TOKKI-O   (IDX) TO  RECE10-TOKKI   (IDY)
                   MOVE    WRK-TOKKINM-O (IDX) TO  RECE10-TOKKINM (IDY)
               END-IF
           END-PERFORM
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       10
                   OR      WRK-TOKKI-OCC (IDX)
                                   =   SPACE
                   OR      IDY     =   5
               IF      WRK-TOKKI-O (IDX)
                                       =   SPACE
                   ADD     1                   TO  IDY
                   MOVE    WRK-TOKKI-O   (IDX) TO  RECE10-TOKKI   (IDY)
                   MOVE    WRK-TOKKINM-O (IDX) TO  RECE10-TOKKINM (IDY)
               END-IF
           END-PERFORM
      *
      *    災害対応 
           MOVE    RECE02-RJNGAIKOHFTN (1)
                                       TO  RECE10-ETC-YKZFTN (1)
      *
      *    後期高齢者非該当
           IF      RECE02-KOREIHIGAITO  =   1
               MOVE    1                   TO  RECE10-ETC-YKZFTN (2)
           END-IF 
      *
           MOVE    "00000000000000000000000000000000000000000000000000"
                                       TO  RECE10-RESERVE-AREA
      *
           IF     (RECE02-SRYYM    >=  "201204")
           AND    (RECE02-ZAITAKU  NOT =  SPACE)
               MOVE   RECE02-ZAITAKU   TO  RECE10-RESERVE-AREA(1:1)
           END-IF
      *
           IF     (RECE02-ICBKINKSI-ARI    =  1  OR  2  OR  3)
           OR     (RECE02-ICBKINKSI-NASI   =  1  OR  2  OR  3)
             EVALUATE  RECE02-ICBKINKSI-ARI
               WHEN       1
                 MOVE   "1"        TO  RECE10-RESERVE-AREA(2:1)
               WHEN       2
                 MOVE   "2"        TO  RECE10-RESERVE-AREA(2:1)
               WHEN       3
                 MOVE   "3"        TO  RECE10-RESERVE-AREA(2:1)
             END-EVALUATE
             EVALUATE  RECE02-ICBKINKSI-NASI
               WHEN       1
                 MOVE   "1"        TO  RECE10-RESERVE-AREA(3:1)
               WHEN       2
                 MOVE   "2"        TO  RECE10-RESERVE-AREA(3:1)
               WHEN       3
                 MOVE   "3"        TO  RECE10-RESERVE-AREA(3:1)
             END-EVALUATE
             IF    RECE02-SAIKEI-CHIKAN-KIN1  NOT =  123456789
               IF  RECE02-SAIKEI-CHIKAN-KIN1      =  009999999
                 MOVE   "2"        TO  RECE10-RESERVE-AREA(4:1)
               ELSE
                 MOVE   "1"        TO  RECE10-RESERVE-AREA(4:1)
               END-IF
             END-IF
             IF    RECE02-SAIKEI-CHIKAN-KIN2  NOT =  123456789
               IF  RECE02-SAIKEI-CHIKAN-KIN2      =  009999999
                 MOVE   "2"        TO  RECE10-RESERVE-AREA(5:1)
               ELSE
                 MOVE   "1"        TO  RECE10-RESERVE-AREA(5:1)
               END-IF
             END-IF
             IF    RECE02-SAIKEI-CHIKAN-KIN3  NOT =  123456789
               IF  RECE02-SAIKEI-CHIKAN-KIN3      =  009999999
                 MOVE   "2"        TO  RECE10-RESERVE-AREA(11:1)
               ELSE
                 MOVE   "1"        TO  RECE10-RESERVE-AREA(11:1)
               END-IF
             END-IF
             IF    RECE02-SAIKEI-CHIKAN-KIN4  NOT =  123456789
               IF  RECE02-SAIKEI-CHIKAN-KIN4      =  009999999
                 MOVE   "2"        TO  RECE10-RESERVE-AREA(12:1)
               ELSE
                 MOVE   "1"        TO  RECE10-RESERVE-AREA(12:1)
               END-IF
             END-IF
             IF    RECE02-HKN-CHIKAN-KIN      NOT =  123456789
                 MOVE   "1"        TO  RECE10-RESERVE-AREA(6:1)
             END-IF
           END-IF
      *
           IF      RECE02-KOHCHIKAN  =  1
             IF    RECE02-KOH-CHIKAN-KIN1     NOT =  123456789
                 MOVE   "1"        TO  RECE10-RESERVE-AREA(7:1)
             END-IF
             IF    RECE02-KOH-CHIKAN-KIN2     NOT =  123456789
                 MOVE   "1"        TO  RECE10-RESERVE-AREA(8:1)
             END-IF
             IF    RECE02-KOH-CHIKAN-KIN3     NOT =  123456789
                 MOVE   "1"        TO  RECE10-RESERVE-AREA(9:1)
             END-IF
             IF    RECE02-KOH-CHIKAN-KIN4     NOT =  123456789
                 MOVE   "1"        TO  RECE10-RESERVE-AREA(10:1)
             END-IF
           END-IF
      *
      *    「医保＋第三者行為」の医療費一部負担額を記録
           IF    RECE02-DAISAN-FTNMONEY  NOT = ZERO
               MOVE  RECE02-DAISAN-FTNMONEY
                                      TO  WRK-DAISAN-FTNMONEY
               MOVE  WRK-DAISAN-DATA  TO  RECE10-RESERVE-AREA(13:9)
           END-IF
      *
      *    請求額情報（公費負担のうちレセプト請求しない地方公費分）を記録
           IF    RECE02-SKYIRYO-KOHFTN-KOHSKY  NOT = ZERO
               MOVE  RECE02-SKYIRYO-KOHFTN-KOHSKY
                                      TO  WRK-DAISAN-FTNMONEY
               MOVE  WRK-DAISAN-DATA  TO  RECE10-RESERVE-AREA(22:9)
           END-IF
      *
           IF      RECE02-SRYYM    >=  "201004"
               MOVE    RECE02-SKYIRYO-INF  TO  RECE10-SKYIRYO-INF
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       10
                   MOVE    RECE02-HKNCOMBI (IDX)
                                           TO  RECE10-HKNCOMBI (IDX)
               END-PERFORM
           END-IF
      *
      *    点検レセ０点公費フラグ
           IF      RECE02-TENKEN-FLG   =   "1"
               MOVE    "1"                 TO  RECE10-SKYKBN-RECEDEN
           ELSE
               MOVE    SPACE               TO  RECE10-SKYKBN-RECEDEN
           END-IF
      *
      *    直接請求フラグ
           IF    ( RECE02-TEISYUTUSAKI =   "1"        )
           AND   ( RECE08-HKNJANUM-SRT =   "8"        )
           AND   ( RECE08-HKNJANUM-S
                                   NOT =   "99999999" )
               MOVE    "1"                 TO  RECE10-DIRECT-FLG
               MOVE    RECE08-HKNJANUM-S   TO  RECE10-HKNJANUM-S
           ELSE
               MOVE    "0"                 TO  RECE10-DIRECT-FLG
           END-IF
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-SEIKYU          =   1    
               MOVE    "DBINSERT"          TO  MCP-FUNC
           ELSE
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           END-IF 
           PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "ORCR0105 SEIKYU MCP-RC= "  MCP-RC
               IF      FLG-SEIKYU          =   1
               MOVE    "請求管理ＤＢに追加できませんでした"
                                               TO  WRK-RECEERR
               ELSE
               MOVE    "請求管理ＤＢに更新できませんでした"
                                               TO  WRK-RECEERR
               END-IF
      *********PERFORM 500-ERR-HENSYU-SEC
               PERFORM 900-DB-ERR-ABORT-SEC
           END-IF
           .
       3904-RECE10-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    特記事項（請求管理）編集処理
      *****************************************************************
       39041-RECE10-TOKKI-HENSYU-SEC        SECTION.
      *
      *    全角数字のとき半角数字に変換、残りは名称にセット
           IF      WRK-TOKKI-X     NOT =   SPACE    
               MOVE    ZERO                TO  IDXY
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    WRK-TOKKI-X         TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-SYUBETU     =   "2"  )
                   MOVE    WRK-TOKKI-X         TO  WRK-TOKKINM
                   PERFORM VARYING IDY     FROM    1   BY  2
                           UNTIL   IDY     >       4
                           OR      WRK-TOKKI-X (IDY:2)
                                           =   SPACE
                       PERFORM VARYING IDXX    FROM    1   BY  1
                               UNTIL   IDXX    >       10
                           IF      WRK-TOKKI-X (IDY:2)
                                               =   TBL-SUUJI-X(IDXX)
                               ADD     1           TO  IDXY
                               MOVE    TBL-SUUJI-9(IDXX)
                                                   TO  WRK-TOKKI(IDXY:1)
                               MOVE    SPACE       TO  WRK-TOKKINM
                               COMPUTE IDXZ    =   IDY     +   2
                               MOVE    WRK-TOKKI-X (IDXZ:)
                                                   TO  WRK-TOKKINM
                           END-IF
                       END-PERFORM
                   END-PERFORM
               END-IF
           END-IF
      *    
           IF    ( WRK-TOKKI       NOT =   SPACE )
           OR    ( WRK-TOKKINM     NOT =   SPACE )
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL   IDY     >       10
                   IF    ( WRK-TOKKI-O (IDY)
                                           =   SPACE )
                   AND   ( WRK-TOKKINM-O (IDY)
                                           =   SPACE )
                       MOVE    WRK-TOKKI       TO  WRK-TOKKI-O   (IDY)
                       MOVE    WRK-TOKKINM     TO  WRK-TOKKINM-O (IDY)
                   END-IF
                   IF    ( WRK-TOKKI       NOT =   SPACE )
                   AND   ( WRK-TOKKINM     NOT =   SPACE )
                       IF    ( WRK-TOKKI       =   WRK-TOKKI-O   (IDY) )
                           MOVE    10              TO  IDY
                       END-IF
                   ELSE
                       IF      WRK-TOKKI   NOT =   SPACE
                           IF      WRK-TOKKI   =   WRK-TOKKI-O (IDY)
                               MOVE    10              TO  IDY
                           END-IF
                       ELSE
                           IF      WRK-TOKKINM =   WRK-TOKKINM-O (IDY)
                               MOVE    10              TO  IDY
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM      
           END-IF    
           .
       39041-RECE10-TOKKI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
      *     MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦→和暦日付編集
      *****************************************************************
       31012-SE-WA-YMDEDIT-SEC        SECTION.
      *
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY1-AREA
           MOVE    "11"                TO  LNK-DAY1-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"   USING   STS-AREA-DAY  LNK-DAY1-AREA
           IF  STS-DAY-RC1             =   ZERO
               MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
           ELSE
               MOVE    SPACE               TO  WRK-SYMD
           END-IF
           .
       31012-SE-WA-YMDEDIT-EXT.
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
      *            ジョブ管理終了処理
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
                   PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
           MOVE    2                   TO  FLG-STS
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
           STRING  "ORCR0105 "             DELIMITED   BY  SIZE
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
               PERFORM 900-CALL-ORCSJOB-SEC
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
                      PERFORM 900-CALL-ORCSJOB-SEC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
                   MOVE    2                   TO  FLG-STS
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    データ削除処理
      *****************************************************************
       5001-DB-DELETE-SEC               SECTION.
      *
      *    ＥＦファイル作成、連携基盤用、診療情報システム用のとき
           IF      WRK-PARA-PRTKBN =   "9" OR  "8" OR  "B"
               IF      WRK-PARA-SYOKBN     =   "1"
                   INITIALIZE                      RECEDEN-REC
                   MOVE    WRK-PARA-HOSPNUM    TO  RECEDEN-HOSPNUM
      *************MOVE    WRK-PARA-SRYYM      TO  RECEDEN-SRYYM
                   MOVE    "2"                 TO  RECEDEN-NYUGAIKBN
                   MOVE    RECEDEN-REC         TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   IF      WRK-PARA-PRTKBN =   "9"
                       MOVE    "tbl_renkei_tmp"    TO  MCP-TABLE
                   ELSE
                       IF      WRK-PARA-PRTKBN =   "8"
                           MOVE    "tbl_efout"         TO  MCP-TABLE
                       ELSE
                           MOVE    "tbl_receden_tmp"   TO  MCP-TABLE
                       END-IF
                   END-IF
                   MOVE    "del1"              TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       IF      WRK-PARA-PRTKBN =   "8"
                           MOVE
                          "ＥＦファイル作成削除処理でエラーとなりました"
                                                       TO  WRK-RECEERR
                       ELSE
                           IF      WRK-PARA-PRTKBN =   "9"
                               MOVE
                               "連携基盤削除処理でエラーとなりました"
                                                       TO  WRK-RECEERR
                           ELSE
                               MOVE
                       "診療情報システム用削除処理でエラーとなりました"
                                                       TO  WRK-RECEERR
                           END-IF
                       END-IF
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               ELSE
                   EVALUATE    BTPARA-INFO-KBN
                     WHEN    "01"
      *                レセプト指示（個別指示）コード読み込み
                       INITIALIZE                      SYS-2002-REC
                       MOVE    "2002"              TO  SYS-2002-KANRICD
                       MOVE    WRK-PARA-HOSPNUM    TO  SYS-2002-HOSPNUM
                       MOVE    ZERO                TO  SYS-2002-STYUKYMD
                       MOVE    ZERO                TO  SYS-2002-EDYUKYMD
                       MOVE    SYS-2002-REC        TO  MCPDATA-REC
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key2"              TO  MCP-PATHNAME
                       PERFORM 900-DBSELECT-SEC
                       IF      MCP-RC              =   ZERO
                           MOVE    "tbl_syskanri"  TO  MCP-TABLE
                           MOVE    "key2"          TO  MCP-PATHNAME
                           PERFORM 900-SYSKANRI-N-SEC
                       ELSE
                           MOVE    1                   TO  FLG-SYSKANRI
                       END-IF
      *
                       PERFORM         UNTIL       FLG-SYSKANRI    =   1
                           MOVE    MCPDATA-REC     TO  SYS-2002-REC
      *
                           DISPLAY "DELETE PTNUM=" SYS-2002-PTNUM
                                           " SRYYM=" SYS-2002-SRYYM
      *                    診療年月のレセ電削除処理
                           INITIALIZE                  RECEDEN-REC
                           MOVE    WRK-PARA-HOSPNUM    TO
                                                       RECEDEN-HOSPNUM
                           MOVE    SYS-2002-PTID       TO  RECEDEN-PTID
                           MOVE    SYS-2002-SRYYM      TO  RECEDEN-SRYYM
                           MOVE    SYS-2002-NYUGAIKBN  TO
                                                       RECEDEN-NYUGAIKBN
                           MOVE    RECEDEN-REC         TO  MCPDATA-REC
                           MOVE    "DBDELETE"          TO  MCP-FUNC
                           IF      WRK-PARA-PRTKBN =   "9"
                               MOVE    "tbl_renkei_tmp"    TO  MCP-TABLE
                           ELSE
                               IF      WRK-PARA-PRTKBN =   "8"
                                   MOVE    "tbl_efout"         TO
                                                               MCP-TABLE
                               ELSE
                                   MOVE    "tbl_receden_tmp"   TO
                                                               MCP-TABLE
                               END-IF
                           END-IF
                           MOVE    "del2"              TO  MCP-PATHNAME
                           PERFORM 900-ORCDBMAIN-SEC
                           IF      MCP-RC           NOT =   ZERO
                               DISPLAY "DBPATH=" MCP-TABLE
                                       " " MCP-PATHNAME
                                       " MCP-RC=" MCP-RC
                               IF      WRK-PARA-PRTKBN =   "8"
                                   MOVE
                          "ＥＦファイル作成削除処理でエラーとなりました"
                                                       TO  WRK-RECEERR
                               ELSE
                                   IF      WRK-PARA-PRTKBN =   "9"
                                       MOVE
                               "連携基盤削除処理でエラーとなりました"
                                                       TO  WRK-RECEERR
                                   ELSE
                                       MOVE
                       "診療情報システム用削除処理でエラーとなりました"
                                                       TO  WRK-RECEERR
                                   END-IF
                               END-IF
                               PERFORM 500-ERR-HENSYU-SEC
                               GO  TO  5001-DB-DELETE-EXT
                           END-IF
      *
                           MOVE    "tbl_syskanri"  TO  MCP-TABLE
                           MOVE    "key2"          TO  MCP-PATHNAME
                           PERFORM 900-SYSKANRI-N-SEC
                       END-PERFORM
      *
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key2"              TO  MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
                 END-EVALUATE
               END-IF      
               GO  TO  5001-DB-DELETE-EXT
           END-IF
      *
           IF      WRK-PARA-SYOKBN     =   "1"
      *        診療年月の請求管理削除処理（社保分）
               IF      WRK-PARA-RECEKBN    =   "0" OR  "1"          
                   INITIALIZE                  RECE10-REC
                   MOVE    WRK-PARA-HOSPNUM    TO  RECE10-HOSPNUM
                   MOVE    WRK-PARA-SRYYM      TO  RECE10-SRYYM
                   MOVE    "2"                 TO  RECE10-NYUGAIKBN
                   MOVE    1                   TO  RECE10-TEISYUTUSAKI
                   MOVE    "2"                 TO  RECE10-NYUGAIKBN
                   MOVE    RECE10-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_seikyu"        TO  MCP-TABLE
                   MOVE    "del6"              TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                       "請求管理削除処理（社保分）でエラーとなりました"
                                           TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF      
      *
      *        診療年月の請求管理削除処理（国保分）
               IF      WRK-PARA-RECEKBN    =   "0" OR  "2"          
                   INITIALIZE                      RECE10-REC
                   MOVE    WRK-PARA-HOSPNUM    TO  RECE10-HOSPNUM
                   MOVE    WRK-PARA-SRYYM      TO  RECE10-SRYYM
                   MOVE    "2"                 TO  RECE10-NYUGAIKBN
                   MOVE    2                   TO  RECE10-TEISYUTUSAKI
                   MOVE    "2"                 TO  RECE10-NYUGAIKBN
                   MOVE    RECE10-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_seikyu"        TO  MCP-TABLE
                   MOVE    "del6"              TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                       "請求管理削除処理（国保分）でエラーとなりました"
                                           TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF      
      *
      *        診療年月の請求管理削除処理（広域連合分）
               IF      WRK-PARA-RECEKBN    =   "0" OR  "7"          
                   INITIALIZE                      RECE10-REC
                   MOVE    WRK-PARA-HOSPNUM    TO  RECE10-HOSPNUM
                   MOVE    WRK-PARA-SRYYM      TO  RECE10-SRYYM
                   MOVE    "2"                 TO  RECE10-NYUGAIKBN
                   MOVE    6                   TO  RECE10-TEISYUTUSAKI
                   MOVE    "2"                 TO  RECE10-NYUGAIKBN
                   MOVE    RECE10-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_seikyu"        TO  MCP-TABLE
                   MOVE    "del6"              TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                   "請求管理削除処理（広域連合分）でエラーとなりました"
                                           TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF      
      *
      *        診療年月の公費請求書削除処理（社保分）
               IF      WRK-PARA-RECEKBN    =   "0" OR  "1"          
                   INITIALIZE                      KOHSKY-REC
                   MOVE    WRK-PARA-HOSPNUM    TO  KOHSKY-HOSPNUM
                   MOVE    WRK-PARA-SRYYM      TO  KOHSKY-SRYYM
                   MOVE    1                   TO  KOHSKY-TEISYUTUSAKI
                   MOVE    "2"                 TO  KOHSKY-NYUGAIKBN
                   MOVE    KOHSKY-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_kohsky"        TO  MCP-TABLE
                   MOVE    "del1"              TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                      "公費請求書削除処理（社保分）でエラーとなりました"
                                               TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF      
      *
      *        診療年月公費請求書削除処理（国保分）
               IF      WRK-PARA-RECEKBN    =   "0" OR  "2"          
                   INITIALIZE                      KOHSKY-REC
                   MOVE    WRK-PARA-HOSPNUM    TO  KOHSKY-HOSPNUM
                   MOVE    WRK-PARA-SRYYM      TO  KOHSKY-SRYYM
                   MOVE    2                   TO  KOHSKY-TEISYUTUSAKI
                   MOVE    "2"                 TO  KOHSKY-NYUGAIKBN
                   MOVE    KOHSKY-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_kohsky"        TO  MCP-TABLE
                   MOVE    "del1"              TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                      "公費請求書削除処理（国保分）でエラーとなりました"
                                               TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF      
      *
      *        診療年月公費請求書削除処理（広域連合分）
               IF      WRK-PARA-RECEKBN    =   "0" OR  "7"          
                   INITIALIZE                      KOHSKY-REC
                   MOVE    WRK-PARA-HOSPNUM    TO  KOHSKY-HOSPNUM
                   MOVE    WRK-PARA-SRYYM      TO  KOHSKY-SRYYM
                   MOVE    6                   TO  KOHSKY-TEISYUTUSAKI
                   MOVE    "2"                 TO  KOHSKY-NYUGAIKBN
                   MOVE    KOHSKY-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_kohsky"        TO  MCP-TABLE
                   MOVE    "del1"              TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                  "公費請求書削除処理（広域連合分）でエラーとなりました"
                                               TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF      
      *
      *        診療年月のレセ電削除処理（社保分）
               IF      WRK-PARA-RECEKBN    =   "0" OR  "1"          
                   INITIALIZE                  RECEDEN-REC
                   MOVE    WRK-PARA-HOSPNUM    TO  RECEDEN-HOSPNUM
                   MOVE    WRK-PARA-SRYYM      TO  RECEDEN-SRYYM
                   MOVE    1                   TO  RECEDEN-TEISYUTUSAKI
                   MOVE    "2"                 TO  RECEDEN-NYUGAIKBN
                   MOVE    RECEDEN-REC         TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_receden"       TO  MCP-TABLE
                   MOVE    "del1"              TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                       "レセ電削除処理（社保分）でエラーとなりました"
                                               TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF      
      *
      *        診療年月のレセ電削除処理（国保分）
               IF      WRK-PARA-RECEKBN    =   "0" OR  "2"          
                   INITIALIZE                  RECEDEN-REC
                   MOVE    WRK-PARA-HOSPNUM    TO  RECEDEN-HOSPNUM
                   MOVE    WRK-PARA-SRYYM      TO  RECEDEN-SRYYM
                   MOVE    2                   TO  RECEDEN-TEISYUTUSAKI
                   MOVE    "2"                 TO  RECEDEN-NYUGAIKBN
                   MOVE    RECEDEN-REC         TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_receden"       TO  MCP-TABLE
                   MOVE    "del1"              TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                       "レセ電削除処理（国保分）でエラーとなりました"
                                               TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF      
      *
      *        診療年月のレセ電削除処理（広域連合分）
               IF      WRK-PARA-RECEKBN    =   "0" OR  "7"          
                   INITIALIZE                  RECEDEN-REC
                   MOVE    WRK-PARA-HOSPNUM    TO  RECEDEN-HOSPNUM
                   MOVE    WRK-PARA-SRYYM      TO  RECEDEN-SRYYM
                   MOVE    6                   TO  RECEDEN-TEISYUTUSAKI
                   MOVE    "2"                 TO  RECEDEN-NYUGAIKBN
                   MOVE    RECEDEN-REC         TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_receden"       TO  MCP-TABLE
                   MOVE    "del1"              TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                   "レセ電削除処理（広域連合分）でエラーとなりました"
                                               TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF      
      *
      *        診療年月レセプト管理削除処理（一括分）
               INITIALIZE                  RECE07-REC
               MOVE    WRK-PARA-HOSPNUM    TO  RECE07-HOSPNUM
               MOVE    "HC06"              TO  RECE07-PRTID
               MOVE    WRK-PARA-SRYYM      TO  RECE07-SRYYM
               MOVE    "1"                 TO  RECE07-SYOKBN
               MOVE    "2"                 TO  RECE07-NYUGAIKBN
               MOVE    WRK-PARA-RECEKBN    TO  RECE07-RECEKBN
               MOVE    RECE07-REC          TO  MCPDATA-REC
               MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
               PERFORM 900-RECE07-READ-SEC
               IF      FLG-RECE07          =   ZERO
                   MOVE    RECE07-REC          TO  WRK-RECE07-REC
                   MOVE    RECE07-REC          TO  MCPDATA-REC 
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_recekanri"     TO  MCP-TABLE
                   MOVE    "del1"              TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                           "レセプト管理削除処理でエラーとなりました"
                                                TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
      *
      *            診療年月レセプト削除処理（一括分）
                   INITIALIZE                  RECE08-REC
                   MOVE    WRK-RECE07-HOSPNUM  TO  RECE08-HOSPNUM
                   MOVE    WRK-RECE07-PRTID    TO  RECE08-PRTID
                   MOVE    WRK-RECE07-SRYYM    TO  RECE08-RECEYM
                   MOVE    WRK-RECE07-CREYMD   TO  RECE08-CREYMD
                   MOVE    WRK-RECE07-CREHMS   TO  RECE08-CREHMS
                   MOVE    WRK-RECE07-NYUGAIKBN
                                               TO  RECE08-NYUGAIKBN
                   MOVE    RECE08-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_receprt"       TO  MCP-TABLE
                   MOVE    "del1"              TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                           "レセプト削除処理でエラーとなりました"
                                               TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF
           ELSE
               MOVE    TBL-SYS1001 (1)     TO  SYS-1001-REC
      *
      *        レセプト管理削除処理（個別分）
               INITIALIZE                  RECE07-REC
               MOVE    WRK-PARA-HOSPNUM    TO  RECE07-HOSPNUM
               MOVE    "HC06"              TO  RECE07-PRTID
               MOVE    "2"                 TO  RECE07-NYUGAIKBN
               MOVE    RECE07-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "del2"              TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC           NOT =   ZERO
                   DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                           " MCP-RC=" MCP-RC
                   MOVE
                   "レセプト管理削除処理（個別分）でエラーとなりました"
                                           TO    WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  5001-DB-DELETE-EXT
               END-IF
      *
      *        レセプト削除処理（個別分）
               INITIALIZE                  RECE08-REC
               MOVE    WRK-PARA-HOSPNUM    TO  RECE08-HOSPNUM
               MOVE    "HC06"              TO  RECE08-PRTID
               MOVE    "2"                 TO  RECE08-NYUGAIKBN
               MOVE    RECE08-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_receprt"       TO  MCP-TABLE
               MOVE    "del2"              TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC           NOT =   ZERO
                   DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                           " MCP-RC=" MCP-RC
                   MOVE
                       "レセプト削除処理（個別分）でエラーとなりました"
                                           TO    WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  5001-DB-DELETE-EXT
               END-IF
      *
               EVALUATE    BTPARA-INFO-KBN
                   WHEN    "01"
                   WHEN    "04"
                   WHEN    "05"
      *                レセプト指示（個別指示）コード読み込み
                       INITIALIZE                      SYS-2002-REC
                       MOVE    "2002"              TO  SYS-2002-KANRICD
                       MOVE    WRK-PARA-HOSPNUM    TO  SYS-2002-HOSPNUM
                       MOVE    ZERO                TO  SYS-2002-STYUKYMD
                       MOVE    ZERO                TO  SYS-2002-EDYUKYMD
                       MOVE    SYS-2002-REC        TO  MCPDATA-REC
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key2"              TO  MCP-PATHNAME
                       PERFORM 900-DBSELECT-SEC
                       IF      MCP-RC              =   ZERO
                           MOVE    "tbl_syskanri"  TO  MCP-TABLE
                           MOVE    "key2"          TO  MCP-PATHNAME
                           PERFORM 900-SYSKANRI-N-SEC
                       ELSE
                           MOVE    1                   TO  FLG-SYSKANRI
                       END-IF
      *
                       PERFORM         UNTIL       FLG-SYSKANRI    =   1
                           MOVE    MCPDATA-REC     TO  SYS-2002-REC
      *
                           DISPLAY "DELETE PTNUM=" SYS-2002-PTNUM
                                           " SRYYM=" SYS-2002-SRYYM
      *
      *                    診療年月の請求管理削除処理
                           INITIALIZE                  RECE10-REC
                           MOVE    WRK-PARA-HOSPNUM    TO
                                                       RECE10-HOSPNUM
                           MOVE    SYS-2002-PTID       TO  RECE10-PTID
                           MOVE    SYS-2002-SRYYM      TO  RECE10-SRYYM
                           MOVE    SYS-2002-NYUGAIKBN  TO
                                                       RECE10-NYUGAIKBN
                           MOVE    1                   TO
                                                   RECE10-TEISYUTUSAKI
                           MOVE    2                   TO
                                                   RECE10-XXTEISYUTUSAKI
                           MOVE    RECE10-REC          TO  MCPDATA-REC
                           MOVE    "DBDELETE"          TO  MCP-FUNC
                           MOVE    "tbl_seikyu"        TO  MCP-TABLE
                           IF      WRK-PARA-DLTKBN     =   "0"
                               MOVE    "del4"          TO  MCP-PATHNAME
                           ELSE
                               MOVE    "del7"          TO  MCP-PATHNAME
                           END-IF
                           PERFORM 900-ORCDBMAIN-SEC
                           IF      MCP-RC           NOT =   ZERO
                               DISPLAY "DBPATH=" MCP-TABLE " "
                                                 MCP-PATHNAME
                                       " MCP-RC=" MCP-RC
                               MOVE
                        "請求管理削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                               PERFORM 500-ERR-HENSYU-SEC
                               GO  TO  5001-DB-DELETE-EXT
                           END-IF
      *
                           INITIALIZE                  RECE10-REC
                           MOVE    WRK-PARA-HOSPNUM    TO
                                                       RECE10-HOSPNUM
                           MOVE    SYS-2002-PTID       TO  RECE10-PTID
                           MOVE    SYS-2002-SRYYM      TO  RECE10-SRYYM
                           MOVE    SYS-2002-NYUGAIKBN  TO
                                                       RECE10-NYUGAIKBN
                           MOVE    6                   TO
                                                   RECE10-TEISYUTUSAKI
                                                   RECE10-XXTEISYUTUSAKI
                           MOVE    RECE10-REC          TO  MCPDATA-REC
                           MOVE    "DBDELETE"          TO  MCP-FUNC
                           MOVE    "tbl_seikyu"        TO  MCP-TABLE
                           IF      WRK-PARA-DLTKBN     =   "0"
                               MOVE    "del4"          TO  MCP-PATHNAME
                           ELSE
                               MOVE    "del7"          TO  MCP-PATHNAME
                           END-IF
                           PERFORM 900-ORCDBMAIN-SEC
                           IF      MCP-RC           NOT =   ZERO
                               DISPLAY "DBPATH=" MCP-TABLE " "
                                                 MCP-PATHNAME
                                       " MCP-RC=" MCP-RC
                               MOVE
                        "請求管理削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                               PERFORM 500-ERR-HENSYU-SEC
                               GO  TO  5001-DB-DELETE-EXT
                           END-IF
      *
      *                    診療年月の公費請求書削除処理
                           INITIALIZE                  KOHSKY-REC
                           MOVE    WRK-PARA-HOSPNUM    TO
                                                       KOHSKY-HOSPNUM
                           MOVE    SYS-2002-PTID       TO  KOHSKY-PTID
                           MOVE    SYS-2002-SRYYM      TO  KOHSKY-SRYYM
                           MOVE    SYS-2002-NYUGAIKBN  TO
                                                       KOHSKY-NYUGAIKBN
                           MOVE    KOHSKY-REC          TO  MCPDATA-REC
                           MOVE    "DBDELETE"          TO  MCP-FUNC
                           MOVE    "tbl_kohsky"        TO  MCP-TABLE
                           IF      WRK-PARA-DLTKBN     =   "0"
                               MOVE    "del2"          TO  MCP-PATHNAME
                           ELSE
                               MOVE    "del4"          TO  MCP-PATHNAME
                           END-IF
                           PERFORM 900-ORCDBMAIN-SEC
                           IF      MCP-RC           NOT =   ZERO
                               DISPLAY "DBPATH=" MCP-TABLE
                                       " " MCP-PATHNAME
                                       " MCP-RC=" MCP-RC
                               MOVE
                      "公費請求書削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                               PERFORM 500-ERR-HENSYU-SEC
                               GO  TO  5001-DB-DELETE-EXT
                           END-IF
      *
      *                    診療年月のレセ電削除処理
                           INITIALIZE                  RECEDEN-REC
                           MOVE    WRK-PARA-HOSPNUM    TO
                                                       RECEDEN-HOSPNUM
                           MOVE    SYS-2002-PTID       TO  RECEDEN-PTID
                           MOVE    SYS-2002-SRYYM      TO  RECEDEN-SRYYM
                           MOVE    SYS-2002-NYUGAIKBN  TO
                                                       RECEDEN-NYUGAIKBN
                           MOVE    1                   TO
                                                   RECEDEN-TEISYUTUSAKI
                           MOVE    RECEDEN-REC         TO  MCPDATA-REC
                           MOVE    "DBDELETE"          TO  MCP-FUNC
                           MOVE    "tbl_receden"       TO  MCP-TABLE
                           MOVE    "del4"              TO  MCP-PATHNAME
                           PERFORM 900-ORCDBMAIN-SEC
                           IF      MCP-RC           NOT =   ZERO
                               DISPLAY "DBPATH=" MCP-TABLE
                                       " " MCP-PATHNAME
                                       " MCP-RC=" MCP-RC
                               MOVE
                          "レセ電削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                               PERFORM 500-ERR-HENSYU-SEC
                               GO  TO  5001-DB-DELETE-EXT
                           END-IF
      *
                           INITIALIZE                  RECEDEN-REC
                           MOVE    WRK-PARA-HOSPNUM    TO
                                                       RECEDEN-HOSPNUM
                           MOVE    SYS-2002-PTID       TO  RECEDEN-PTID
                           MOVE    SYS-2002-SRYYM      TO  RECEDEN-SRYYM
                           MOVE    SYS-2002-NYUGAIKBN  TO
                                                       RECEDEN-NYUGAIKBN
                           MOVE    2                   TO
                                                   RECEDEN-TEISYUTUSAKI
                           MOVE    RECEDEN-REC         TO  MCPDATA-REC
                           MOVE    "DBDELETE"          TO  MCP-FUNC
                           MOVE    "tbl_receden"       TO  MCP-TABLE
                           MOVE    "del4"              TO  MCP-PATHNAME
                           PERFORM 900-ORCDBMAIN-SEC
                           IF      MCP-RC           NOT =   ZERO
                               DISPLAY "DBPATH=" MCP-TABLE
                                       " " MCP-PATHNAME
                                       " MCP-RC=" MCP-RC
                               MOVE
                          "レセ電削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                               PERFORM 500-ERR-HENSYU-SEC
                               GO  TO  5001-DB-DELETE-EXT
                           END-IF
      *
                           INITIALIZE                  RECEDEN-REC
                           MOVE    WRK-PARA-HOSPNUM    TO
                                                       RECEDEN-HOSPNUM
                           MOVE    SYS-2002-PTID       TO  RECEDEN-PTID
                           MOVE    SYS-2002-SRYYM      TO  RECEDEN-SRYYM
                           MOVE    SYS-2002-NYUGAIKBN  TO
                                                       RECEDEN-NYUGAIKBN
                           MOVE    6                   TO
                                                   RECEDEN-TEISYUTUSAKI
                           MOVE    RECEDEN-REC         TO  MCPDATA-REC
                           MOVE    "DBDELETE"          TO  MCP-FUNC
                           MOVE    "tbl_receden"       TO  MCP-TABLE
                           MOVE    "del4"              TO  MCP-PATHNAME
                           PERFORM 900-ORCDBMAIN-SEC
                           IF      MCP-RC           NOT =   ZERO
                               DISPLAY "DBPATH=" MCP-TABLE
                                       " " MCP-PATHNAME
                                       " MCP-RC=" MCP-RC
                               MOVE
                          "レセ電削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                               PERFORM 500-ERR-HENSYU-SEC
                               GO  TO  5001-DB-DELETE-EXT
                           END-IF
      *
                           MOVE    "tbl_syskanri"  TO  MCP-TABLE
                           MOVE    "key2"          TO  MCP-PATHNAME
                           PERFORM 900-SYSKANRI-N-SEC
                       END-PERFORM
      *
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key2"              TO  MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
                   WHEN    "02"
                       INITIALIZE                      PTINF-REC
                       MOVE    WRK-PARA-HOSPNUM    TO  PTINF-HOSPNUM
                       MOVE    BTPARA-INFO-PARA (1:8)
                                                   TO  PTINF-ST-SRYYMD
                       MOVE    BTPARA-INFO-PARA (9:8)
                                                   TO  PTINF-ED-SRYYMD
                       MOVE    "2"                 TO  PTINF-NYUGAIKBN
                       MOVE    ZERO                TO  PTINF-HKNKBN
                       MOVE    BTPARA-INFO-PARA(17:6) 
                                                   TO  PTINF-SRYYM
                       MOVE    BTPARA-INFO-PARA(17:6)
                                                   TO  PTINF-SRYYMD(1:6)
                       MOVE    "__"                TO  PTINF-SRYYMD(7:2)
                       MOVE    PTINF-REC           TO  MCPDATA-REC
                       MOVE    "tbl_ptinf"         TO  MCP-TABLE
                       MOVE    "key13"             TO  MCP-PATHNAME
                       PERFORM 900-DBSELECT-SEC
                       IF      MCP-RC              =   ZERO
                           MOVE    "tbl_ptinf"         TO  MCP-TABLE
                           MOVE    "key13"             TO  MCP-PATHNAME
                           PERFORM 900-PTINF-READ-N-SEC
                       ELSE
                           MOVE    1                  TO  FLG-PTINF
                       END-IF
      *
                       PERFORM     UNTIL       FLG-PTINF       =   1
      *
                           DISPLAY "DELETE PTID=" PTINF-PTID
                                       " SRYYM=" BTPARA-INFO-PARA (17:6)
      *
      *                    診療年月の請求管理削除処理
                           INITIALIZE                  RECE10-REC
                           MOVE    WRK-PARA-HOSPNUM
                                                   TO  RECE10-HOSPNUM
                           MOVE    PTINF-PTID      TO  RECE10-PTID
                           MOVE    BTPARA-INFO-PARA (17:6)
                                                   TO  RECE10-SRYYM
                           MOVE    "2"             TO  RECE10-NYUGAIKBN
                           MOVE    1               TO
                                                   RECE10-TEISYUTUSAKI
                           MOVE    2               TO
                                                   RECE10-XXTEISYUTUSAKI
                           MOVE    RECE10-REC      TO  MCPDATA-REC
                           MOVE    "DBDELETE"      TO  MCP-FUNC
                           MOVE    "tbl_seikyu"    TO  MCP-TABLE
                           IF      WRK-PARA-DLTKBN     =   "0"
                               MOVE    "del4"          TO  MCP-PATHNAME
                           ELSE
                               MOVE    "del7"          TO  MCP-PATHNAME
                           END-IF
                           PERFORM 900-ORCDBMAIN-SEC
                           IF      MCP-RC      NOT =   ZERO
                               DISPLAY "DBPATH=" MCP-TABLE
                                       " " MCP-PATHNAME
                                       " MCP-RC=" MCP-RC
                               MOVE
                        "請求管理削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                               PERFORM 500-ERR-HENSYU-SEC
                               GO  TO  5001-DB-DELETE-EXT
                           END-IF
      *
                           INITIALIZE                  RECE10-REC
                           MOVE    WRK-PARA-HOSPNUM
                                                   TO  RECE10-HOSPNUM
                           MOVE    PTINF-PTID      TO  RECE10-PTID
                           MOVE    BTPARA-INFO-PARA (17:6)
                                                   TO  RECE10-SRYYM
                           MOVE    "2"             TO  RECE10-NYUGAIKBN
                           MOVE    6               TO
                                                   RECE10-TEISYUTUSAKI
                                                   RECE10-XXTEISYUTUSAKI
                           MOVE    RECE10-REC      TO  MCPDATA-REC
                           MOVE    "DBDELETE"      TO  MCP-FUNC
                           MOVE    "tbl_seikyu"    TO  MCP-TABLE
                           IF      WRK-PARA-DLTKBN     =   "0"
                               MOVE    "del4"          TO  MCP-PATHNAME
                           ELSE
                               MOVE    "del7"          TO  MCP-PATHNAME
                           END-IF
                           PERFORM 900-ORCDBMAIN-SEC
                           IF      MCP-RC      NOT =   ZERO
                               DISPLAY "DBPATH=" MCP-TABLE
                                       " " MCP-PATHNAME
                                       " MCP-RC=" MCP-RC
                               MOVE
                        "請求管理削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                               PERFORM 500-ERR-HENSYU-SEC
                               GO  TO  5001-DB-DELETE-EXT
                           END-IF
      *
      *                    診療年月の公費請求書削除処理
                           INITIALIZE                  KOHSKY-REC
                           MOVE    WRK-PARA-HOSPNUM
                                                   TO  KOHSKY-HOSPNUM
                           MOVE    PTINF-PTID      TO  KOHSKY-PTID
                           MOVE    BTPARA-INFO-PARA (17:6)
                                                   TO  KOHSKY-SRYYM
                           MOVE    "2"             TO  KOHSKY-NYUGAIKBN
                           MOVE    KOHSKY-REC      TO  MCPDATA-REC
                           MOVE    "DBDELETE"      TO  MCP-FUNC
                           MOVE    "tbl_kohsky"    TO  MCP-TABLE
                           IF      WRK-PARA-DLTKBN     =   "0"
                               MOVE    "del2"          TO  MCP-PATHNAME
                           ELSE
                               MOVE    "del4"          TO  MCP-PATHNAME
                           END-IF
                           PERFORM 900-ORCDBMAIN-SEC
                           IF      MCP-RC       NOT =   ZERO
                               DISPLAY "DBPATH=" MCP-TABLE
                                       " " MCP-PATHNAME
                                       " MCP-RC=" MCP-RC
                               MOVE
                      "公費請求書削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                               PERFORM 500-ERR-HENSYU-SEC
                               GO  TO  5001-DB-DELETE-EXT
                           END-IF
      *
      *                    診療年月のレセ電削除処理
                           INITIALIZE                  RECEDEN-REC
                           MOVE    WRK-PARA-HOSPNUM
                                                   TO  RECEDEN-HOSPNUM
                           MOVE    PTINF-PTID      TO  RECEDEN-PTID
                           MOVE    BTPARA-INFO-PARA (17:6)
                                                   TO  RECEDEN-SRYYM
                           MOVE    "2"             TO  RECEDEN-NYUGAIKBN
                           MOVE    1               TO
                                                   RECEDEN-TEISYUTUSAKI
                           MOVE    RECEDEN-REC     TO  MCPDATA-REC
                           MOVE    "DBDELETE"      TO  MCP-FUNC
                           MOVE    "tbl_receden"   TO  MCP-TABLE
                           MOVE    "del4"          TO  MCP-PATHNAME
                           PERFORM 900-ORCDBMAIN-SEC
                           IF      MCP-RC           NOT =   ZERO
                               DISPLAY "DBPATH=" MCP-TABLE
                                       " " MCP-PATHNAME
                                       " MCP-RC=" MCP-RC
                               MOVE
                          "レセ電削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                               PERFORM 500-ERR-HENSYU-SEC
                               GO  TO  5001-DB-DELETE-EXT
                           END-IF
      *
                           INITIALIZE                  RECEDEN-REC
                           MOVE    WRK-PARA-HOSPNUM
                                                   TO  RECEDEN-HOSPNUM
                           MOVE    PTINF-PTID      TO  RECEDEN-PTID
                           MOVE    BTPARA-INFO-PARA (17:6)
                                                   TO  RECEDEN-SRYYM
                           MOVE    "2"             TO  RECEDEN-NYUGAIKBN
                           MOVE    2               TO
                                                   RECEDEN-TEISYUTUSAKI
                           MOVE    RECEDEN-REC     TO  MCPDATA-REC
                           MOVE    "DBDELETE"      TO  MCP-FUNC
                           MOVE    "tbl_receden"   TO  MCP-TABLE
                           MOVE    "del4"          TO  MCP-PATHNAME
                           PERFORM 900-ORCDBMAIN-SEC
                           IF      MCP-RC           NOT =   ZERO
                               DISPLAY "DBPATH=" MCP-TABLE
                                       " " MCP-PATHNAME
                                       " MCP-RC=" MCP-RC
                               MOVE
                          "レセ電削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                               PERFORM 500-ERR-HENSYU-SEC
                               GO  TO  5001-DB-DELETE-EXT
                           END-IF
      *
                           INITIALIZE                  RECEDEN-REC
                           MOVE    WRK-PARA-HOSPNUM
                                                   TO  RECEDEN-HOSPNUM
                           MOVE    PTINF-PTID      TO  RECEDEN-PTID
                           MOVE    BTPARA-INFO-PARA (17:6)
                                                   TO  RECEDEN-SRYYM
                           MOVE    "2"             TO  RECEDEN-NYUGAIKBN
                           MOVE    6               TO
                                                   RECEDEN-TEISYUTUSAKI
                           MOVE    RECEDEN-REC     TO  MCPDATA-REC
                           MOVE    "DBDELETE"      TO  MCP-FUNC
                           MOVE    "tbl_receden"   TO  MCP-TABLE
                           MOVE    "del4"          TO  MCP-PATHNAME
                           PERFORM 900-ORCDBMAIN-SEC
                           IF      MCP-RC           NOT =   ZERO
                               DISPLAY "DBPATH=" MCP-TABLE
                                       " " MCP-PATHNAME
                                       " MCP-RC=" MCP-RC
                               MOVE
                          "レセ電削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                               PERFORM 500-ERR-HENSYU-SEC
                               GO  TO  5001-DB-DELETE-EXT
                           END-IF
      *
                           MOVE    "tbl_ptinf"         TO  MCP-TABLE
                           MOVE    "key13"             TO  MCP-PATHNAME
                           PERFORM 900-PTINF-READ-N-SEC
                       END-PERFORM
      *
                       MOVE    "tbl_ptinf"         TO  MCP-TABLE
                       MOVE    "key13"             TO  MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
                   WHEN    "03"
                       INITIALIZE                      PTINF-REC
                       MOVE    WRK-PARA-HOSPNUM    TO  PTINF-HOSPNUM
                       MOVE    BTPARA-INFO-PARA (1:8)
                                                   TO  PTINF-ST-SRYYMD
                       MOVE    BTPARA-INFO-PARA (9:8)
                                                   TO  PTINF-ED-SRYYMD
                       MOVE    "2"                 TO  PTINF-NYUGAIKBN
                       MOVE    ZERO                TO  PTINF-HKNKBN
                       MOVE    "7"                 TO  PTINF-HKNKBN2
                       MOVE    PTINF-REC           TO  MCPDATA-REC
                       MOVE    "tbl_ptinf"         TO  MCP-TABLE
                       MOVE    "key16"             TO  MCP-PATHNAME
                       PERFORM 900-DBSELECT-SEC
                       IF      MCP-RC              =   ZERO
                           MOVE    "tbl_ptinf"         TO  MCP-TABLE
                           MOVE    "key16"             TO  MCP-PATHNAME
                           PERFORM 900-PTINF-READ-N-SEC
                       ELSE
                           MOVE    1                  TO  FLG-PTINF
                       END-IF
      *
                       PERFORM     UNTIL       FLG-PTINF       =   1
      *
                           DISPLAY "DELETE PTID=" PTINF-PTID
                                       " SRYYM=" BTPARA-INFO-PARA (1:6)
      *
      *                    診療年月の請求管理削除処理
                           INITIALIZE                  RECE10-REC
                           MOVE    WRK-PARA-HOSPNUM
                                                   TO  RECE10-HOSPNUM
                           MOVE    PTINF-PTID      TO  RECE10-PTID
                           MOVE    BTPARA-INFO-PARA (1:6)
                                                   TO  RECE10-SRYYM
                           MOVE    "2"             TO  RECE10-NYUGAIKBN
                           MOVE    1               TO
                                                   RECE10-TEISYUTUSAKI
                           MOVE    2               TO
                                                   RECE10-XXTEISYUTUSAKI
                           MOVE    RECE10-REC      TO  MCPDATA-REC
                           MOVE    "DBDELETE"      TO  MCP-FUNC
                           MOVE    "tbl_seikyu"    TO  MCP-TABLE
                           IF      WRK-PARA-DLTKBN     =   "0"
                               MOVE    "del4"          TO  MCP-PATHNAME
                           ELSE
                               MOVE    "del7"          TO  MCP-PATHNAME
                           END-IF
                           PERFORM 900-ORCDBMAIN-SEC
                           IF      MCP-RC      NOT =   ZERO
                               DISPLAY "DBPATH=" MCP-TABLE
                                       " " MCP-PATHNAME
                                       " MCP-RC=" MCP-RC
                               MOVE
                        "請求管理削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                               PERFORM 500-ERR-HENSYU-SEC
                               GO  TO  5001-DB-DELETE-EXT
                           END-IF
      *
                           INITIALIZE                  RECE10-REC
                           MOVE    WRK-PARA-HOSPNUM
                                                   TO  RECE10-HOSPNUM
                           MOVE    PTINF-PTID      TO  RECE10-PTID
                           MOVE    BTPARA-INFO-PARA (1:6)
                                                   TO  RECE10-SRYYM
                           MOVE    "2"             TO  RECE10-NYUGAIKBN
                           MOVE    6               TO
                                                   RECE10-TEISYUTUSAKI
                                                   RECE10-XXTEISYUTUSAKI
                           MOVE    RECE10-REC      TO  MCPDATA-REC
                           MOVE    "DBDELETE"      TO  MCP-FUNC
                           MOVE    "tbl_seikyu"    TO  MCP-TABLE
                           IF      WRK-PARA-DLTKBN     =   "0"
                               MOVE    "del4"          TO  MCP-PATHNAME
                           ELSE
                               MOVE    "del7"          TO  MCP-PATHNAME
                           END-IF
                           PERFORM 900-ORCDBMAIN-SEC
                           IF      MCP-RC      NOT =   ZERO
                               DISPLAY "DBPATH=" MCP-TABLE
                                       " " MCP-PATHNAME
                                       " MCP-RC=" MCP-RC
                               MOVE
                        "請求管理削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                               PERFORM 500-ERR-HENSYU-SEC
                               GO  TO  5001-DB-DELETE-EXT
                           END-IF
      *
      *                    診療年月の公費請求書削除処理
                           INITIALIZE                  KOHSKY-REC
                           MOVE    WRK-PARA-HOSPNUM
                                                   TO  KOHSKY-HOSPNUM
                           MOVE    PTINF-PTID      TO  KOHSKY-PTID
                           MOVE    BTPARA-INFO-PARA (1:6)
                                                   TO  KOHSKY-SRYYM
                           MOVE    "2"             TO  KOHSKY-NYUGAIKBN
                           MOVE    KOHSKY-REC      TO  MCPDATA-REC
                           MOVE    "DBDELETE"      TO  MCP-FUNC
                           MOVE    "tbl_kohsky"    TO  MCP-TABLE
                           IF      WRK-PARA-DLTKBN     =   "0"
                               MOVE    "del2"          TO  MCP-PATHNAME
                           ELSE
                               MOVE    "del4"          TO  MCP-PATHNAME
                           END-IF
                           PERFORM 900-ORCDBMAIN-SEC
                           IF      MCP-RC      NOT =   ZERO
                               DISPLAY "DBPATH=" MCP-TABLE
                                       " " MCP-PATHNAME
                                       " MCP-RC=" MCP-RC
                               MOVE
                      "公費請求書削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                               PERFORM 500-ERR-HENSYU-SEC
                               GO  TO  5001-DB-DELETE-EXT
                           END-IF
      *
      *                    診療年月のレセ電削除処理
                           INITIALIZE                  RECEDEN-REC
                           MOVE    WRK-PARA-HOSPNUM
                                                   TO  RECEDEN-HOSPNUM
                           MOVE    PTINF-PTID      TO  RECEDEN-PTID
                           MOVE    BTPARA-INFO-PARA (1:6)
                                                   TO  RECEDEN-SRYYM
                           MOVE    "2"             TO  RECEDEN-NYUGAIKBN
                           MOVE    1               TO
                                                   RECEDEN-TEISYUTUSAKI
                           MOVE    RECEDEN-REC     TO  MCPDATA-REC
                           MOVE    "DBDELETE"      TO  MCP-FUNC
                           MOVE    "tbl_receden"   TO  MCP-TABLE
                           MOVE    "del4"          TO  MCP-PATHNAME
                           PERFORM 900-ORCDBMAIN-SEC
                           IF      MCP-RC           NOT =   ZERO
                               DISPLAY "DBPATH=" MCP-TABLE
                                       " " MCP-PATHNAME
                                       " MCP-RC=" MCP-RC
                               MOVE
                          "レセ電削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                               PERFORM 500-ERR-HENSYU-SEC
                               GO  TO  5001-DB-DELETE-EXT
                           END-IF
      *
                           INITIALIZE                  RECEDEN-REC
                           MOVE    WRK-PARA-HOSPNUM
                                                   TO  RECEDEN-HOSPNUM
                           MOVE    PTINF-PTID      TO  RECEDEN-PTID
                           MOVE    BTPARA-INFO-PARA (1:6)
                                                   TO  RECEDEN-SRYYM
                           MOVE    "2"             TO  RECEDEN-NYUGAIKBN
                           MOVE    2               TO
                                                   RECEDEN-TEISYUTUSAKI
                           MOVE    RECEDEN-REC     TO  MCPDATA-REC
                           MOVE    "DBDELETE"      TO  MCP-FUNC
                           MOVE    "tbl_receden"   TO  MCP-TABLE
                           MOVE    "del4"          TO  MCP-PATHNAME
                           PERFORM 900-ORCDBMAIN-SEC
                           IF      MCP-RC           NOT =   ZERO
                               DISPLAY "DBPATH=" MCP-TABLE
                                       " " MCP-PATHNAME
                                       " MCP-RC=" MCP-RC
                               MOVE
                          "レセ電削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                               PERFORM 500-ERR-HENSYU-SEC
                               GO  TO  5001-DB-DELETE-EXT
                           END-IF
      *
                           INITIALIZE                  RECEDEN-REC
                           MOVE    WRK-PARA-HOSPNUM
                                                   TO  RECEDEN-HOSPNUM
                           MOVE    PTINF-PTID      TO  RECEDEN-PTID
                           MOVE    BTPARA-INFO-PARA (1:6)
                                                   TO  RECEDEN-SRYYM
                           MOVE    "2"             TO  RECEDEN-NYUGAIKBN
                           MOVE    6               TO
                                                   RECEDEN-TEISYUTUSAKI
                           MOVE    RECEDEN-REC     TO  MCPDATA-REC
                           MOVE    "DBDELETE"      TO  MCP-FUNC
                           MOVE    "tbl_receden"   TO  MCP-TABLE
                           MOVE    "del4"          TO  MCP-PATHNAME
                           PERFORM 900-ORCDBMAIN-SEC
                           IF      MCP-RC           NOT =   ZERO
                               DISPLAY "DBPATH=" MCP-TABLE
                                       " " MCP-PATHNAME
                                       " MCP-RC=" MCP-RC
                               MOVE
                          "レセ電削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                               PERFORM 500-ERR-HENSYU-SEC
                               GO  TO  5001-DB-DELETE-EXT
                           END-IF
      *
                           MOVE    "tbl_ptinf"         TO  MCP-TABLE
                           MOVE    "key16"             TO  MCP-PATHNAME
                           PERFORM 900-PTINF-READ-N-SEC
                       END-PERFORM
      *
                       MOVE    "tbl_ptinf"         TO  MCP-TABLE
                       MOVE    "key16"             TO  MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
               END-EVALUATE
           END-IF
           .
       5001-DB-DELETE-EXT.
           EXIT.    
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ファイルＣＬＯＳＥ処理
           IF      WRK-PARA-SHELLID
                                   NOT =   "RECEPTX"
               CLOSE                       RECE02X-FILE
           END-IF
           CLOSE                       RECE02-FILE
           CLOSE                       RECE031-FILE
           CLOSE                       RECE061-FILE
                                       RECE09-FILE
                                       RECE11-FILE
                                       RECE12-FILE
      *
      *    未請求解除データの更新
      *    提出用レセプト作成時のみ更新可能
           IF    ( WRK-PARA-SHELLID
                                   NOT =   "RECEPTX" )
           AND   ( FLG-END             =   ZERO      )
           AND   ( WRK-PARA-PRTKBN     =   "1"       )
               PERFORM 3001-MISEIKYU-FLG-KOUSIN-SEC
           END-IF 
      *
      *    ＰＵＳＨ通知処理
           IF    ( WRK-PARA-SHELLID
                                   NOT =   "RECEPTX" )
               PERFORM 3001-PUSH-SEC
           END-IF
      *
           DISPLAY "RECE02  CNT-IN  :"  CNT-RECE02
           DISPLAY "RECE02X CNT-IN  :"  CNT-RECE02X
           DISPLAY "        PAGE    :"  CNT-OUT
           DISPLAY "*** ORCR0105 END ***"
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    CNT-OUT         TO  JOB-UPDCNT
               MOVE    CNT-RECE02      TO  JOB-UPDCNT1
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求解除データの更新処理
      *****************************************************************
       3001-MISEIKYU-FLG-KOUSIN-SEC    SECTION.
      *
           OPEN    INPUT               RECE02-FILE
      *
           MOVE    ZERO                TO  FLG-RECE02
           PERFORM 900-RECE02-READ-SEC
      *
           PERFORM             UNTIL   FLG-RECE02  =   1
               IF      RECE02-MISEIKYU-FLG =   "02"
                   PERFORM 30011-MISEIKYU-FLG-03-SEC
               END-IF
      *
               MOVE    RECE02-HOSPNUM  TO  KEY-N-HOSPNUM
               MOVE    RECE02-SRYYM    TO  KEY-N-SRYYM
               MOVE    RECE02-NYUGAIKBN
                                       TO  KEY-N-NYUGAIKBN
               MOVE    RECE02-PTID     TO  KEY-N-PTID
               PERFORM UNTIL   FLG-RECE02     =   1
                       OR      RECE02-HOSPNUM NOT =   KEY-N-HOSPNUM
                       OR      RECE02-SRYYM   NOT =   KEY-N-SRYYM
                       OR      RECE02-NYUGAIKBN
                                              NOT =   KEY-N-NYUGAIKBN
                       OR      RECE02-PTID    NOT =   KEY-N-PTID
                   PERFORM 900-RECE02-READ-SEC
               END-PERFORM 
           END-PERFORM
      *
           CLOSE               RECE02-FILE
           .
       3001-MISEIKYU-FLG-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求設定解除処理
      *****************************************************************
       30011-MISEIKYU-FLG-03-SEC       SECTION.
      *
           DISPLAY "MISEIKYU PTNUM=" RECE02-PTNUM
                   " SRYYM=" RECE02-SRYYM     
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                  SYUKA-REC
           MOVE    RECE02-HOSPNUM  TO  SYUKA-HOSPNUM
           MOVE    RECE02-PTID     TO  SYUKA-PTID
           MOVE    RECE02-SRYYM    TO  SYUKA-SRYYM
           MOVE    RECE02-NYUGAIKBN
                                   TO  SYUKA-NYUGAIKBN
           MOVE    "YYYY"          TO  SYUKA-RECESYUBETU
           MOVE    "03"            TO  SYUKA-MISEIKYU-FLG
           MOVE    SMCNDATE-YMD    TO  SYUKA-UPYMD
           MOVE    SMCNDATE-HMS    TO  SYUKA-UPHMS
           MOVE    "recept1"       TO  SYUKA-TERMID
           MOVE    SPACE           TO  SYUKA-OPID
           MOVE    SYUKA-REC       TO  MCPDATA-REC
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           MOVE    "tbl_syuka"     TO  MCP-TABLE
           MOVE    "upd4"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC      NOT =   ZERO 
               MOVE    "主科ＤＢに更新できませんでした"
                                               TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                  TO  FLG-ERR
           END-IF
           .
      *
       30011-MISEIKYU-FLG-03-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＳＨ通知処理
      *****************************************************************
       3001-PUSH-SEC        SECTION.
      *
           EVALUATE    WRK-PARA-RECEKBN
               WHEN    "0"
                   IF    ( SYS-2005-SYARECEKBN =   "1" OR  "3" )
                   AND   ( SYS-2005-KKHRECEKBN =   "1" OR  "3" )
                   AND   ( SYS-25-KOUIKIRECEKBN
                                               =   "1" OR  "3" )
                       GO  TO  3001-PUSH-EXT
                   END-IF
               WHEN    "1"
                   IF      SYS-2005-SYARECEKBN =   "1" OR  "3"
                       GO  TO  3001-PUSH-EXT
                   END-IF
               WHEN    "2"
                   IF      SYS-2005-KKHRECEKBN =   "1" OR  "3"
                       GO  TO  3001-PUSH-EXT
                   END-IF
               WHEN    "7"
                   IF      SYS-25-KOUIKIRECEKBN
                                               =   "1" OR  "3"
                       GO  TO  3001-PUSH-EXT
                   END-IF
           END-EVALUATE
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           PERFORM 900-CALL-ORCSJOB-SEC
           IF    ( SJOBKANRI-RETURN
                                   =   ZERO  )
           AND   ( JOB-KARTE-UID
                               NOT =   SPACE )
               INITIALIZE                  PUSHBA01-REC
               MOVE    JOB-KARTE-UID   TO  PUSHBA01API-KARTE-UID
               MOVE    JOB-ORCA-UID    TO  PUSHBA01API-ORCA-UID
               MOVE    "000"           TO  PUSHBA01API-RESULT
               MOVE    "receipt_make"  TO  PUSHBA01-EVENT
               IF      WRK-PARA-SRYYM  =   ZERO
                   MOVE    "9999-12"       TO  PUSHBA01-PERFORM-MONTH
               ELSE
                   MOVE    WRK-PARA-SRYYM  TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE (1:7)
                                           TO  PUSHBA01-PERFORM-MONTH
               END-IF
               MOVE    CNT-RECE02      TO  WRK-CNT
               MOVE    WRK-CNT-X       TO  PUSHBA01-OUTPUT-COUNT
      *??
             display "push=" PUSHBA01-REC "#"
      *??
               MOVE    PUSHBA01-REC    TO  MCPDATA-REC
               MOVE    "PUSHEVENT"     TO  MCP-FUNC
               MOVE    "push_batch01"  TO  MCP-TABLE
               MOVE    "push_batch01"  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           END-IF
           .
       3001-PUSH-EXT.
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
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE               TO  WRK-HEN-DATE
           ELSE
               INITIALIZE                  WRK-HEN-DATE
               MOVE    WRK-SYMD (1:4)      TO  WRK-HEN-YY
               MOVE    WRK-SYMD (5:2)      TO  WRK-HEN-MM
               MOVE    WRK-SYMD (7:2)      TO  WRK-HEN-DD
               MOVE    "-"                 TO  WRK-HEN-H1
               MOVE    "-"                 TO  WRK-HEN-H2
               IF      WRK-SYMD            =   "99999999"
                   MOVE    "12"                TO  WRK-HEN-MM
                   MOVE    "31"                TO  WRK-HEN-DD
               END-IF
           END-IF
           .
       801-DAYHEN01-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本情報データ読み込み処理
      *****************************************************************
       990-RECE02-READ-SEC           SECTION.
      *
           READ    RECE02-FILE       NEXT
               AT  END
                   MOVE    1                   TO  FLG-RECE02
                   MOVE    HIGH-VALUE          TO  KEY-NEW
               NOT AT  END
                   IF      STS-RECE02          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE01PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE02      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO                TO  FLG-RECE02
      *
                   IF    ( RECE02-TOTALTEN (1) =   ZERO )
                   AND   ( RECE02-TOTALTEN (2) =   ZERO )
                   AND   ( RECE02-TOTALTEN (3) =   ZERO )
                   AND   ( RECE02-TOTALTEN (4) =   ZERO )
                   AND   ( RECE02-TOTALTEN (5) =   ZERO )
                   AND   ( RECE02-SKYZEROFLG   =   ZERO )
                       IF      RECE02-RECESYUBETU  NOT =   8888 AND ZERO
                           GO  TO  990-RECE02-READ-SEC
                       END-IF
                       IF    ( RECE02-RECESYUBETU  =   8888  )
                       AND   ( RECE02-JIHTEKFLG    =   SPACE )
                           GO  TO  990-RECE02-READ-SEC
                       END-IF
                   END-IF
      *
                   DISPLAY "RECE02  PTNUM=" RECE02-PTNUM
                           " SRYYM="        RECE02-SRYYM
                           " RECESYUBETU="  RECE02-RECESYUBETU
                           " ERR-ERRKBN="   RECE02-ERR-ERRKBN
                           " RJNGAIKOHFTN(1)="
                                            RECE02-RJNGAIKOHFTN (1)
      *
                   ADD     1               TO  CNT-RECE02
                   MOVE    RECE02-HOSPNUM  TO  KEY-N-HOSPNUM
                   MOVE    RECE02-SRYYM    TO  KEY-N-SRYYM
                   MOVE    RECE02-NYUGAIKBN
                                           TO  KEY-N-NYUGAIKBN
                   MOVE    RECE02-PTID     TO  KEY-N-PTID
                   MOVE    RECE02-RECEKA   TO  KEY-N-RECEKA
                   MOVE    RECE02-TEISYUTUSAKI
                                           TO  KEY-N-TEISYUTUSAKI
                   MOVE    RECE02-RECESYUBETU
                                           TO  KEY-N-RECESYUBETU
                   MOVE    RECE02-HKNJANUM-KEY
                                           TO  KEY-N-HKNJANUM-KEY
      *
      *            医療機関情報取得（個別作成時）
                   IF      WRK-PARA-SRYYM  =   ZERO
                       MOVE    RECE02-SRYYM    TO  WRK-SYS-SRYYMD(1:6)
                       MOVE    "01"            TO  WRK-SYS-SRYYMD(7:2)
                       PERFORM 900-SYSKANRI-HENSYU-SEC
                   END-IF    
      *
      *            請求点と点数欄の合計をチェック
                   IF      RECE02-ERR-ERRKBN   =   ZERO
                       INITIALIZE              WRK-TOTALTEN-OCC
      *
                       PERFORM VARYING IDY     FROM    1   BY  1
                               UNTIL   IDY     >       5
                           PERFORM VARYING IDX     FROM    1   BY  1
                                   UNTIL   IDX     >       45
                               ADD     RECE02-TENSU (IDX IDY)
                                               TO  WRK-TOTALTEN (IDY)
                           END-PERFORM
                       END-PERFORM
      *
                       IF    ( RECE02-TOTALTEN (1)
                                             NOT =   WRK-TOTALTEN (1) )
                       OR    ( RECE02-TOTALTEN (2)
                                             NOT =   WRK-TOTALTEN (2) )
                       OR    ( RECE02-TOTALTEN (3)
                                             NOT =   WRK-TOTALTEN (3) )
                       OR    ( RECE02-TOTALTEN (4)
                                             NOT =   WRK-TOTALTEN (4) )
                       OR    ( RECE02-TOTALTEN (5)
                                             NOT =   WRK-TOTALTEN (5) )
                           MOVE    16            TO  RECE02-ERR-ERRKBN
      *
                           DISPLAY "TOTALTEN (1)=" WRK-TOTALTEN    (1)
                                               "#" RECE02-TOTALTEN (1)
                           DISPLAY "TOTALTEN (2)=" WRK-TOTALTEN    (2)
                                               "#" RECE02-TOTALTEN (2)
                           DISPLAY "TOTALTEN (3)=" WRK-TOTALTEN    (3)
                                               "#" RECE02-TOTALTEN (3)
                           DISPLAY "TOTALTEN (4)=" WRK-TOTALTEN    (4)
                                               "#" RECE02-TOTALTEN (4)
                           DISPLAY "TOTALTEN (5)=" WRK-TOTALTEN    (4)
                                               "#" RECE02-TOTALTEN (5)
                       END-IF
                   END-IF
           END-READ
           .
       990-RECE02-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本情報データ読み込み処理（公費請求書）
      *****************************************************************
       990-RECE02X-READ-SEC           SECTION.
      *
           READ    RECE02X-FILE       NEXT
               AT  END
                   MOVE    1                   TO  FLG-RECE02X
               NOT AT  END
                   IF      STS-RECE02X         =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE02XPARA     TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE02X     TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
      *            テスト患者のとき読み飛ばす
                   IF      RECE02X-TSTPTNUMKBN     =   "1"
                       GO  TO  990-RECE02X-READ-SEC
                   END-IF
      *
                   IF    ( RECE02X-TOTALTEN (1) =   ZERO )
                   AND   ( RECE02X-TOTALTEN (2) =   ZERO )
                   AND   ( RECE02X-TOTALTEN (3) =   ZERO )
                   AND   ( RECE02X-TOTALTEN (4) =   ZERO )
                   AND   ( RECE02X-TOTALTEN (5) =   ZERO )
                       IF  RECE02X-RECESYUBETU   NOT =  ZERO
                           GO  TO  990-RECE02X-READ-SEC
                       END-IF
                   END-IF
      *
                   DISPLAY "RECE02X PTNUM=" RECE02X-PTNUM     
      *
                   MOVE    ZERO                TO  FLG-RECE02X
                   ADD     1                   TO  CNT-RECE02X
           END-READ
      *
           .
       990-RECE02X-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本情報データ読み込み処理（未請求解除）
      *****************************************************************
       900-RECE02-READ-SEC             SECTION.
      *
           READ    RECE02-FILE       NEXT
               AT  END
                   MOVE    1                   TO  FLG-RECE02
               NOT AT  END
                   IF      STS-RECE02          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE01PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE02      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO                TO  FLG-RECE02
           END-READ
      *
           .
       900-RECE02-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    医療機関情報取得処理（個別作成時）
      ****************************************************************
       900-SYSKANRI-HENSYU-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYS-1901
                                           FLG-SYS-1902
      *
      *    医療機関情報（基本情報）
           MOVE    ZERO                TO  FLG-SET
      *
           PERFORM VARYING SYSKAN-IDX  FROM    1   BY  1
                   UNTIL ( SYSKAN-IDX              >  100  )
                   OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                   OR    ( FLG-SET                 =   1   )
               MOVE    TBL-SYS1001(SYSKAN-IDX) TO  SYS-1001-REC
               IF    ( SYS-1001-STYUKYMD       <= WRK-SYS-SRYYMD    )
               AND   ( WRK-SYS-SRYYMD          <= SYS-1001-EDYUKYMD )
                   MOVE     1                  TO  FLG-SET
               END-IF
           END-PERFORM
           IF      FLG-SET             =   ZERO
               STRING  "医療機関情報が取得できませんでした。診療年月＝"
                                               DELIMITED  BY  SIZE 
                       WRK-SYS-SRYYMD (1:6)    DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    2           TO  FLG-STS
               GO  TO  900-SYSKANRI-HENSYU-EXT
           END-IF
      *
      *    医療機関情報（所在地ほか）
           MOVE    ZERO                TO  FLG-SET
      *
           PERFORM VARYING SYSKAN-IDX  FROM    1   BY  1
                   UNTIL ( SYSKAN-IDX              >  100  )
                   OR    ( TBL-SYS1002(SYSKAN-IDX) = SPACE )
                   OR    ( FLG-SET                 =   1   )
               MOVE    TBL-SYS1002(SYSKAN-IDX) TO  SYS-1002-REC
               IF    ( SYS-1002-STYUKYMD       <= WRK-SYS-SRYYMD    )
               AND   ( WRK-SYS-SRYYMD          <= SYS-1002-EDYUKYMD )
                   MOVE     1                  TO  FLG-SET
               END-IF
           END-PERFORM
           IF      FLG-SET             =   ZERO
               STRING
              "医療機関情報（所在地）が取得できませんでした。診療年月＝"
                                               DELIMITED  BY  SIZE 
                       WRK-SYS-SRYYMD (1:6)    DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    2                   TO  FLG-STS
           END-IF
      *
      *    帳票別指定医療機関編集情報
           IF      FLG-SYS-1900        =   1
               MOVE    ZERO                TO  FLG-SET
      *
               PERFORM VARYING SYSKAN-IDX  FROM    1   BY  1
                       UNTIL ( SYSKAN-IDX              >  100  )
                       OR    ( TBL-SYS1900(SYSKAN-IDX) = SPACE )
                       OR    ( FLG-SET                 =   1   )
                   MOVE    TBL-SYS1900(SYSKAN-IDX) TO  SYS-1900-REC
                   IF    ( SYS-1900-STYUKYMD   <= WRK-SYS-SRYYMD    )
                   AND   ( WRK-SYS-SRYYMD      <= SYS-1900-EDYUKYMD )
                           MOVE     1              TO  FLG-SET
                   END-IF
               END-PERFORM
               IF      FLG-SET             =   1
                  INITIALIZE                       SYSKANRI-REC
                  MOVE    "1901"               TO  SYS-KANRICD
                  MOVE    SYS-1900-PRTKBN (1)  TO  SYS-KBNCD
                  MOVE    WRK-SYS-SRYYMD (1:6) TO  SYS-STYUKYMD (1:6)
                  MOVE    "01"                 TO  SYS-STYUKYMD (7:2)
                  MOVE    SYS-STYUKYMD         TO   SYS-EDYUKYMD
                  MOVE    "tbl_syskanri"       TO  WRK-MCP-TABLE
                  MOVE    "key10"              TO  WRK-MCP-PATHNAME
                  PERFORM 900-SYSKANRI-READ-SEC
                  IF      FLG-SYSKANRI     =   ZERO
                      MOVE    MCPDATA-REC      TO  SYS-1901-REC
                      MOVE    1                TO  FLG-SYS-1901
                  END-IF
                  INITIALIZE                       SYSKANRI-REC
                  MOVE    "1902"               TO  SYS-KANRICD
                  MOVE    SYS-1900-PRTKBN (1)  TO  SYS-KBNCD
                  MOVE    WRK-SYS-SRYYMD (1:6) TO  SYS-STYUKYMD (1:6)
                  MOVE    "01"                 TO  SYS-STYUKYMD (7:2)
                  MOVE    SYS-STYUKYMD         TO   SYS-EDYUKYMD
                  MOVE    "tbl_syskanri"       TO  WRK-MCP-TABLE
                  MOVE    "key10"              TO  WRK-MCP-PATHNAME
                  PERFORM 900-SYSKANRI-READ-SEC
                  IF      FLG-SYSKANRI     =   ZERO
                      MOVE    MCPDATA-REC      TO  SYS-1902-REC
                      MOVE    1                TO  FLG-SYS-1902
                  END-IF
              END-IF    
           END-IF
      *
           .
       900-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      ****************************************************************
      *    病名情報データ読み込み処理
      ****************************************************************
       990-RECE031-NEXT-SEC           SECTION.
      *
           READ    RECE031-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE031
               NOT AT  END
                   IF      STS-RECE03          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE031PARA         TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE03          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE031
                   IF      RECE031-TENKIKBN     =   "8"
                       GO  TO  990-RECE031-NEXT-SEC
                   END-IF
           END-READ
      *
           .
       990-RECE031-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    適用欄明細情報データ読み込み処理
      ****************************************************************
       990-RECE061-NEXT-SEC           SECTION.
      *
           READ    RECE061-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE061
               NOT AT  END
                   IF      STS-RECE06          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE061PARA         TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE06          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE061
                   ADD     1         TO  CNT-RECE061
                   IF      RECE061-SRYKBN    =   "54"
                       MOVE    "50"          TO  RECE061-SRYKBN
                   END-IF
                   IF      RECE061-SRYKBN    =   "64"
                       MOVE    "60"          TO  RECE061-SRYKBN
                   END-IF
                   IF      RECE061-SRYKBN    =   "99"
                       MOVE    SPACE         TO  RECE061-SRYKBN
                   END-IF
           END-READ
      *
           .
       990-RECE061-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険履歴情報データ読み込み処理
      ****************************************************************
       990-RECE09-NEXT-SEC           SECTION.
      *
           READ    RECE09-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE09
               NOT AT  END
                   IF      STS-RECE09          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE09PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE09          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE09
           END-READ
      *
           .
       990-RECE09-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    カスタマイズデータ読み込み処理
      ****************************************************************
       990-RECE11-READ-SEC           SECTION.
      *
           READ    RECE11-FILE
               INVALID
                   MOVE    1           TO  FLG-RECE11
               NOT INVALID
                   IF      STS-RECE11          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE11PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE11          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO        TO  FLG-RECE11
           END-READ
      *
           .
       990-RECE11-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    入退院履歴情報データ読み込み処理
      ****************************************************************
       900-RECE12-NEXT-SEC           SECTION.
      *
           READ    RECE12-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE12
               NOT AT  END
                   IF      STS-RECE12          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE12PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE12          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE12
           END-READ
      *
           .
       900-RECE12-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト管理読込処理
      *****************************************************************
       900-RECE07-READ-SEC           SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE07
                   MOVE    MCPDATA-REC         TO  RECE07-REC
               ELSE
                   MOVE    1                   TO  FLG-RECE07
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-RECE07-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト印刷読込処理
      *****************************************************************
       900-RECE08-READ-SEC           SECTION.
      *
           MOVE    "tbl_receprt"       TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_receprt"       TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE08
               ELSE
                   MOVE    1                   TO  FLG-RECE08
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE08
           END-IF
      *
           MOVE    "tbl_receprt"       TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-RECE08-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理読込処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-N-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-N-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-N-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスタ読込処理
      *****************************************************************
       910-HKNMST-NEXT-SEC              SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
       910-HKNMST-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理読込処理
      *****************************************************************
       900-SEIKYU-INV-SEC         SECTION.
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SEIKYU-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理読込処理
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SEIKYU
           ELSE
               MOVE    1               TO  FLG-SEIKYU
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求読込処理
      *****************************************************************
       900-KOHSKY-INV-SEC         SECTION.
      *
           MOVE    KOHSKY-REC          TO  MCPDATA-REC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_kohsky"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-KOHSKY
               ELSE
                   MOVE    1               TO  FLG-KOHSKY
               END-IF
           ELSE
               MOVE    1                   TO  FLG-KOHSKY
           END-IF
      *
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-KOHSKY-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトコメント読込処理
      *****************************************************************
       900-RECECOM-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-RECECOM
               MOVE    MCPDATA-REC     TO  RECECOM-REC
           ELSE
               MOVE    1               TO  FLG-RECECOM
           END-IF
           .
       900-RECECOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術歴読込処理
      *****************************************************************
       900-OPECOM-READ-SEC           SECTION.
      *
           MOVE    OPECOM-REC          TO  MCPDATA-REC
           MOVE    WRK-OPECOM-MCP-TABLE
                                       TO  MCP-TABLE
           MOVE    WRK-OPECOM-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-OPECOM-MCP-TABLE
                                           TO  MCP-TABLE
               MOVE    WRK-OPECOM-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-OPECOM-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-OPECOM
               INITIALIZE                      OPECOM-REC
           END-IF
      *
           .
       900-OPECOM-READ-EXT.
           EXIT.
      *
     *****************************************************************
      *    手術歴読込処理
      *****************************************************************
       900-OPECOM-READ-N-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  OPECOM-REC
               MOVE    ZERO                TO  FLG-OPECOM
           ELSE
               INITIALIZE                      OPECOM-REC
               MOVE    1                   TO  FLG-OPECOM
           END-IF
      *
           .
       900-OPECOM-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込処理
      *****************************************************************
       900-BTPARA-READ-SEC         SECTION.
      *
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-BTPARA-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BTPARA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込処理
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  BTPARA-REC
               MOVE    ZERO                TO  FLG-BTPARA
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           .
       900-BTPARA-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    編綴管理レコード（管理部）読込処理
      *****************************************************************
       900-RECE-PUTH-READ-SEC        SECTION.
      *
           MOVE    "tbl_rece_puth" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_rece_puth" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-RECE-PUTH-READ-N-SEC
           ELSE
               INITIALIZE                  RECE-PUTH-REC
               MOVE    1               TO  FLG-RECE-PUTH
           END-IF
      *
           MOVE    "tbl_rece_puth" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-RECE-PUTH-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    編綴管理レコード（管理部）読込処理
      *****************************************************************
       900-RECE-PUTH-READ-N-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-RECE-PUTH
               MOVE    MCPDATA-REC         TO  RECE-PUTH-REC
           ELSE
               MOVE    1                   TO  FLG-RECE-PUTH
           END-IF
      *
           .
       900-RECE-PUTH-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    編綴管理レコード（明細部）読込処理
      *****************************************************************
       900-RECE-PUTB-READ-N-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-RECE-PUTB
               MOVE    MCPDATA-REC         TO  RECE-PUTB-REC
           ELSE
               MOVE    1                   TO  FLG-RECE-PUTB
           END-IF
      *
           .
       900-RECE-PUTB-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込処理
      *****************************************************************
       900-PTINF-READ-N-SEC       SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTINF-REC
               MOVE    ZERO                TO  FLG-PTINF
           ELSE
               MOVE    1                   TO  FLG-PTINF
                INITIALIZE                  PTINF-REC
           END-IF
      *
           .
       900-PTINF-READ-N-EXT.
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
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ更新エラー処理
      *****************************************************************
       900-DB-ERR-ABORT-SEC                SECTION.
      *
           PERFORM 500-ERR-HENSYU-SEC
      *
           MOVE    SPACE               TO  WRK-ERR-AREA
           INITIALIZE                      WRK-ERR-AREA
           MOVE    MCP-RC              TO  WRK-MCP-RC
      *
           STRING  "ORCR0105 db err PTNUM="
                                       DELIMITED   BY  SIZE
                   RECE02-PTNUM        DELIMITED   BY  SPACE
                   " TABLE="           DELIMITED   BY  SIZE
                   MCP-TABLE           DELIMITED   BY  SPACE
                   " MCP-FUNC="        DELIMITED   BY  SIZE
                   MCP-FUNC            DELIMITED   BY  SPACE
                   " PATHNAME="        DELIMITED   BY  SIZE
                   MCP-PATHNAME        DELIMITED   BY  SPACE
                   " RC="              DELIMITED   BY  SIZE
                   WRK-MCP-RC          DELIMITED   BY  SIZE
                   LOW-VALUE           DELIMITED   BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           PERFORM 500-COBABORT-SEC
           .
       900-DB-ERR-ABORT-EXT.
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
               STRING  "ORCR0105 select err TABLE="
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
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　ＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.
