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
       PROGRAM-ID.             ORCR0620.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（入院）
      *  コンポーネント名  : レセプト基本情報
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/12/10    NACL−藤原　  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    03.03.03  診療科数１０から２０へ変更
      *  01.00.02    NACL-藤原    03/03/07  レセプト記載外公費情報追加
      *  01.00.03    NACL-藤原    03/03/11  種別不明の情報を編集
      *  01.00.04    NACL-藤原    03/03/15  入院履歴から診療科の編集を行う
      *  01.00.05    NACL-藤原    03/04/14  不明分の保険履歴情報は作成しない
      *  01.00.06    NACL-門脇    03/04/16  船員保険（被保険者）＋通勤災害
      *                                     フラグセット
      *  01.00.07    NACL-藤原    03/04/21  地方公費データのとき主保険が
      *                                     無い場合は不明分とする
      *  01.00.08    NACL-藤原    03/05/09  入退院履歴情報に入院中区分の追加
      *  01.00.09    NACL-門脇    03/05/28  病棟番号・種別編集
      *  01.00.10    NACL-藤原    03/06/16  レセプト種別の修正
      *  01.00.11    NACL-藤原    03/07/25  入院歴に労災・自賠責を対象
      *                                     としないようにする
      *                                     入院会計の医保の組合せがないとき
      *                                     入院歴を全て表示する
      *  01.00.12    NACL-門脇    03/11/27  ”老保”編集条件修正
      *  01.00.13    NACL-門脇    04/02/12  年齢判定文修正
      *  01.00.14    NACL-藤原    04/03/08  シス管による生保受給者番号の編集
      *  01.00.15    NACL-門脇    04/04/06  公費単独７０歳未満で老人保険入力
      *                                     のある方の高齢者フラグ編集修正
      *  01.00.16    NACL-門脇    04/06/02  月途中に老保の変更があった場合
      *                                     のＫＥＹの編集の修正
      *  01.00.17    NACL-藤原    04/06/08  同一開始日の複数自費レセプトが
      *                                     別々に作成されるように修正
      *                                     診療科情報が未編集のとき
      *                                     最終入院科を編集する
      *  01.00.18    NACL-藤原    04/06/08  適用終了日の追加
      *  01.00.19    NACL-藤原    04/06/15  印刷順の追加（個別作成時のみ）
      *  01.00.20    NACL-藤原    04/07/12  生保の受給者番号を月代わり受給者DBに
      *                                     無い場合は公費DBより編集する
      *                                     保険組合せの公費ＩＤを追加
      *  01.00.21    NACL-藤原    04/07/20  特例退職者＋老人のとき不明分
      *                                     とする
      *  01.00.22    NACL-藤原    04/07/23  補助区分の追加に伴うレセプト
      *                                     種別・給付割合の編集を変更
      *  01.00.23    NACL-藤原    04/10/04  特例退職者＋老人のとき本人の
      *                                     み不明分とする
      *  中止処理追加
      *  01.01.01    NACL-藤原    04/10/07  中止処理の追加
      *  01.01.02    NACL-藤原    04/10/30  福岡県の地方公費は主保険が
      *                                     国保の場合は公費カウントしない
      *                                     （法別番号４１以外のとき）
      *                                     病棟区分の編集方法を変更
      *  01.01.03    NACL-藤原    04/11/01  記載なしの公費の処理
      *  01.01.04    NACL-藤原    05/01/08  同月に継続なしで２回入院した
      *                                     場合の入院年月日の記載方法
      *  01.01.05    NACL-門脇    05/02/14  高齢者一部負担額記載修正
      *  01.01.06    NACL-門脇    05/02/25  特記事項”老保”編集修正
      *  01.01.07    NACL-門脇    05/03/16  高齢者フラグ編集修正
      *
      *  02.06.01    NACL-藤原    05/05/17  神奈川県の障害(180､280)は
      *                                     国保で負担者番号が空白の場合
      *                                     公費カウントしない
      *  02.06.02    NACL-門脇    05/06/09  低所得フラグ編集修正
      *  02.06.03    NACL-竹田    05/05/30  realtime preview 処理追加
      *  02.06.04    NACL-小豆沢  05/06/20  転室時の入院レセ病棟区分の表示
      *                                     修正
      *  02.06.05    NACL-藤原    05/07/05  補助区分からの給付割合の編集
      *                                     を修正
      *                                     本人−入院−内容の追加
      *  02.06.06    NACL-藤原    05/07/28  レセプト特記事項編集情報のため
      *                                     のレセプト請求しない地方公費の
      *                                     を追加
      *
      *  02.07.01    NACL-藤原    05/08/20  特別療養費対応
      *  02.07.02    NACL-藤原    05/10/06  公費単独の三歳未満の割合設定
      *                                     をしないようにする
      *  02.07.03    NACL-門脇    06/01/11  病棟区分編集修正
      *  02.07.04    NACL-門脇    06/02/17  高齢者フラグ編集修正
      *
      *  02.08.01    NACL-藤原    06/02/20  主科対応
      *  02.09.01    NACL-門脇    06/05/18  岡山県西粟倉村退職者国保２割対応
      *
      *  03.02.01    NACL-藤原    06/07/31  国保主科対応
      *  03.02.02    NACL-藤原    06/09/07  ２００６年１０月改正対応
      *                                     福岡地方公費対応
      *
      *  03.03.01    NACL-門脇    06/09/27  ２００６年１０月改正対応
      *                                     特記事項１６長２対応
      *                                     レセプト種別
      *                                     老齢福祉年金
      *                                     病棟区分０１、０２、０７
      *  03.03.02    NACL-藤原    06/09/19  月途中での氏名変更対応
      *  03.03.03    NACL-藤原    06/12/13  公費分で種別が設定できなかっ
      *                                     たときの対応
      *
      *  03.04.01    NACL-門脇    06/11/29  病棟区分編集修正
      *  03.04.02    NACL-藤原    06/12/05  未請求レセ対応
      *  03.04.03    NACL-藤原    07/02/06  月途中の受給者番号変更に対応
      *  03.04.04    NACL-門脇    07/02/19  公費−本人−入院−回−負担割合
      *                                     のセット
      *  03.04.05    NACL-門脇    07/04/01  現物給付対応
      *
      *  03.05.01    NACL-小豆沢  07/05/30  グループ診療対応
      *  03.05.02    NACL-門脇    07/07/17  個別時のシステム管理編集修正
      *  03.05.03    NACL-藤原    07/06/19  病棟番号記載対応
      *  03.05.04    NACL-門脇    07/07/31  ７０歳未満現物給付保険者毎で計算
      *  03.05.05    NACL-門脇    07/08/01  所得情報取扱い機能強化対応
      *  03.05.06    NACL-門脇    07/08/01  標準負担額減額認定証設定対応
      *  03.05.07    NACL-門脇    07/08/01  月途中の所得情報変更対応
      *  03.05.08    NACL-藤原    07/09/04  宮崎県「公費９０」対応
      *                                     ２００７年９月以降のみ対象 
      *
      *  04.01.01    NACL-藤原    07/01/21  月途中の負担者番号変更対応
      *                                     （生保の公費単独のみ） 
      *  04.01.02    NACL-藤原    08/02/12  宮崎県「公費９０」対応
      *                                     通常の記載方法に戻す
      * 
      *  04.02.01    NACL-藤原    08/03/04  平成２０年４月改正対応
      *  04.02.02    NACL-藤原    08/03/25  広域連合対応
      *                                     中国残留法人等
      *                                     レセプト種別
      *  04.02.03    NACL-藤原    08/04/14  月途中の負担者番号変更対応
      *                                     （中国残留法人等の公費単独） 
      *  04.02.04    NACL-藤原    08/04/18  神奈川県の障害は
      *                                     国保で負担者番号が空白の場合
      *                                     法別番号８０で判定して
      *                                     公費カウントしない
      *  04.02.06    NACL-藤原    08/04/25  保険履歴の記載変更
      *  04.02.07    NACL-門脇    08/06/24  後期高齢者の現物給付対象編集修正
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
      *  04.04.02    NACL-藤原    09/03/10  特別な関係にある他院歴を初回
      *                                     入院日として起算しレセプト記
      *                                     載を行なう対応
      *  04.04.03    NACL-門脇    09/08/24  出産一時金対応
      *  04.04.04    NACL-藤原    09/10/21  月途中の負担者番号変更対応
      *                                     （自立支援の公費単独） 
      *  04.04.05    NACL-藤原    09/12/17  月途中に医保と自立支援の併用
      *                                     から自立支援単独になったとき
      *                                     のまとめ対応
      *  04.04.06    NACL-藤原    09/12/10  同月に初回による入院歴が複数
      *                                     ある場合の記録の修正 
      *  04.04.07    NACL-門脇    10/01/04  公費単独の保険組み合わせ
      *                                     フラグ編集
      *  04.04.08    NACL-門脇    10/08/23  高齢非該当対応
      *
      *  04.05.01    NACL-藤原    09/07/23  主科未設定患者の主科再取得対応
      *  04.05.02    NACL-藤原    10/01/05  治験対応
      *                                     平成２０年４月分より自費レセ
      *                                     とする
      *  04.05.03    NACL-藤原    10/09/14  医療観察法対応
      *                                     公費単独のレセプトとする
      *  04.05.04    NACL-門脇    11/02/18  出産一時金対応期間修正
      *  04.05.05    NACL-藤原    11/03/25  災害対応
      *                                     連絡先情報追加
      *  04.05.08    NACL-藤原    11/04/18  災害対応
      *                                     主保険情報が同じで適用期間が
      *                                     区切られている場合の保険履歴 
      *  04.05.09    NACL-藤原    11/08/25  公費単独対応
      *                                     公費を最大５０に変更
      *  04.05.10    NACL-門脇    11/10/05  岡山県西粟倉村退職者国保２割対応
      *                                     修正（平成２３年９月まで）
      *  04.05.11    NACL-藤原    11/11/18  他保険での同日再入院時の保険
      *                                     履歴対応
      *  04.05.12    NACL-藤原    11/11/21  診療年月＜転入年月の入院歴を
      *                                     処理対象外とする
      *  04.05.13    NACL-藤原    11/11/22  同日再入院対応
      *  04.05.14    NACL-門脇    12/11/19  ９７２と９７４の混在時対応
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.02    NACL-藤原    11/02/01  主保険がない場合、児童保護(53)
      *                                     は公費単独のレセプトとする
      *  04.06.03    NACL-藤原    11/02/24  月途中に医保と児童保護(53)の
      *                                     併用から単独になったときのまとめ
      *  04.06.04    NACL-藤原    11/12/12  点検レセ０点公費印字対応
      *  04.06.05    NACL-藤原    12/10/04  同一レセプトの公費が４つを超
      *                                     えた場合の種別不明対応
      *  04.06.06    NACL-藤原    12/09/27  生活保護期間重複対応
      *  04.06.07    NACL-藤原    12/10/25  保険履歴ファイルのエラー処理
      *                                     中止 
      *  04.06.07    NACL-藤原    12/12/21  入院料の算定がない入退院履歴
      *                                     の記載対応
      *
      *  04.07.01    NACL-門脇    12/10/02  高齢者判定フラグ編集
      *  04.07.02    NACL-藤原    12/12/17  複数枚レセプト分割対応
      *  04.07.03    NACL-門脇    13/07/10  自賠責（第三者行為）対応
      *  04.07.04    NACL-藤原    13/12/09  医療情報連携基盤対応
      *  04.07.05    NACL-門脇    14/04/08  長期の特記事項重複修正
      *  04.07.06    NACL-藤原    14/03/30  平成２６年４月改正対応
      *  04.07.07    NACL-藤原    14/04/30  治験が複数あるときの対応
      *  04.07.08    NACL-藤原    14/06/05  月途中のまとめのとき第二公費
      *                                     マル長のときも対象とする
      *  04.07.09    NACL-門脇    14/07/31  第三者行為の現物給付対応
      *  04.07.10    NACL-藤原    14/08/18  ７５歳以上の海外居住者２割対応
      *  04.07.11    NACL-藤原    14/09/10  月途中のまとめのとき第二公費
      *                                     マル長のときの修正
      *  04.07.12    NACL-門脇    14/10/21  低所得フラグ編集修正
      *  04.07.13    NACL-藤原    15/01/24  同日再入院時の入退院履歴の記録
      *                                     を修正
      *                                    （入院日と退院日が同じとき）
      *  04.07.14    NACL-藤原    15/07/07  入退院履歴のからの診療科設定
      *                                     に退院日、転出日を条件に追加
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.02    NACL-門脇    15/10/21  現物給付対象外の低所得（食事）対応
      *  04.08.03    NACL-藤原    15/10/08  複数枚レセプト分割対応
      *                                     公費単独の２回以上の分割対応
      *  04.08.04    NACL-門脇    16/02/08  RECE62の予備項目使用分について、
      *                                     個々に項目を設ける事に伴う修正
      *  04.08.05    NACL-藤原    16/07/20  ＥＦファイル作成対応
      *  04.08.06    NACL-藤原    17/03/31  同日再入院の入退院履歴のチェック
      *  04.08.07    NACL-門脇    17/09/26  平成２９年１０月改正
      *                                     （入院時生活療養の見直し）
      *  04.08.08    NACL-門脇    17/10/18  入院料と異なる保険組み合わせ
      *                                     での食事算定対応
      *
      *  05.00.01    NACL-藤原    18/06/11  Warning 対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE61-FILE     ASSIGN  RECE61PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE61-KEY
                                   FILE    STATUS  IS  STS-RECE61.
           SELECT  RECE62-FILE     ASSIGN  RECE62PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE62-KEY
                                   FILE    STATUS  IS  STS-RECE62.
           SELECT  RECE68-FILE     ASSIGN  RECE68PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE68-KEY
                                   FILE    STATUS  IS  STS-RECE68.
           SELECT  RECE69-FILE     ASSIGN  RECE69PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE69-KEY
                                   FILE    STATUS  IS  STS-RECE69.
      *    レセプト（県単用項目）データ
           SELECT  RECE67-FILE     ASSIGN  RECE67PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE67-KEY
                                   FILE    STATUS  IS  STS-RECE67.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE61-FILE.
       01  RECE61-REC. 
           COPY    "CPRCF061.INC".
      *
       FD  RECE62-FILE.
       01  RECE62-REC. 
           COPY    "CPRCF062.INC".
      *
       FD  RECE68-FILE.
       01  RECE68-REC. 
           COPY    "CPRCF068.INC".
      *
       FD  RECE69-FILE.
       01  RECE69-REC. 
           COPY    "CPRCF069.INC".
      *
      *    レセプト（県単用項目）データ
       FD  RECE67-FILE.
       01  RECE67-REC. 
           COPY    "CPRCF067.INC".
      *     
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE61PARA//.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE62PARA//.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE69PARA//.
      *
      *     入退院履歴情報ファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE68PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE67PARA//.
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
           03  STS-RECE61                              PIC X(02).
           03  STS-RECE62                              PIC X(02).
           03  STS-RECE68                              PIC X(02).
           03  STS-RECE69                              PIC X(02).
           03  STS-RECE67                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-RECE61                              PIC 9(01). 
           03  FLG-RECE68                              PIC 9(01). 
           03  FLG-PTHKNINF                            PIC 9(01).
           03  FLG-PTKOHINF                            PIC 9(01).
           03  FLG-HKNNUM                              PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-CHK                                 PIC 9(01).
           03  FLG-MONTHLYNUM                          PIC 9(01).
           03  FLG-PTNYUINRRK                          PIC 9(01).
           03  FLG-PTNYUINRRK12                        PIC 9(01).
           03  FLG-PTNYUINRRK22                        PIC 9(01).
           03  FLG-PTNYUINRRK23                        PIC 9(01).
           03  FLG-KYUSEIRRK                           PIC 9(01).
           03  FLG-PTTOKKI                             PIC 9(01).
           03  FLG-HKNCOMBI                            PIC 9(01). 
           03  FLG-PTRSIINF                            PIC 9(01).
      *
           03  FLG-ARI                                 PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
           03  FLG-ROUJINHOKEN                         PIC 9(01).
           03  FLG-TAISYO                              PIC 9(01).
           03  FLG-SEIHO                               PIC 9(01).
           03  FLG-RIREKI-HEN                          PIC 9(01).
           03  FLG-BUNBEN-SET                          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE68                              PIC 9(06). 
           03  CNT-RECE69                              PIC 9(06). 
           03  CNT-TBL-OCC                             OCCURS  2. 
               05  CNT-IN                              PIC 9(06).
               05  CNT-OUT                             PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
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
           03  IDW                                     PIC 9(03).
           03  IDX2                                    PIC 9(03).
           03  IDX3                                    PIC 9(02).
           03  IDX4                                    PIC 9(02).
           03  IDX5                                    PIC 9(02).
           03  IDWW                                    PIC 9(02).
           03  IDXX                                    PIC 9(02).
           03  IDYY                                    PIC 9(03).
           03  IDZZ                                    PIC 9(03).
           03  IDY1                                    PIC 9(03).
           03  IDY2                                    PIC 9(03).
           03  IDZ                                     PIC 9(02).
           03  TOKKI-IDX                               PIC 9(02).
           03  TOKKI-IDX2                              PIC 9(02).
           03  SYSKAN-IDX                              PIC 9(03).
           03  SYSKAN-1017-IDX                         PIC 9(03).
           03  SYOTOKU-IDX                             PIC 9(02).
           03  SYOTOKU-IDX2                            PIC 9(02)
                                                       OCCURS 4.
           03  SYOTOKU-IDX8                            PIC 9(02).
           03  SYOTOKU-IDX9                            PIC 9(02).
           03  SYOTOKU-IDX-ED                          PIC 9(02).
           03  KOHFTN-IDX                              PIC 9(02).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC                         OCCURS  100.
               05  TBL-SYS1001                         PIC X(610).
      *
      *    患者登録機能制御情報退避用領域
      *01  TBL-SYS1017-T.
      *    03  TBL-SYS1017-OCC                         OCCURS  10.
      *        05  TBL-SYS1017                         PIC X(610).
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
           03  WRK-PARA-PRTKBN                         PIC X(01).
           03  WRK-PARA-HOSPNUM                        PIC  9(02).
      *
           03  WRK-SYS-SRYYMD                          PIC  X(08).
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYYMM                         PIC  9(06).
               05  WRK-SRYDD                           PIC  9(02).
           03  WK-PTNYUINRRK-TENSTUYMD                 PIC  X(08).
           03  WK-PTNYUINRRK-TENSTUYMD2                PIC  X(08).
           03  WK-PTNYUINRRK-TENNYUYMD                 PIC  X(08).
      *
           03  WRK-TAIINYMD        PIC X(08).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-SEIHO-FLG             PIC 9(01).
      *
           03  WRK-BUNBENST-YMD.
               05  WRK-BUNBENST-YM       PIC 9(06).
               05  WRK-BUNBENST-DD       PIC 9(02).
           03  WRK-BUNBENED-YMD.
               05  WRK-BUNBENED-YM       PIC 9(06).
               05  WRK-BUNBENED-DD       PIC 9(02).
      *
           03  WRK-HKNNUM                PIC X(03).
      *
           03  WRK-JIHI-FLG              PIC X(01).
      *
           03  WRK-CHOKI                 PIC X(03).
           03  WRK-CHOKI2                PIC X(03).
      *
           03  RJN-RATE-KBN              PIC X(01).
      *
           03  WRK-RECESKYKBN            PIC X(01).
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
      *    病棟区分テーブル                                             
       01  WRK-BYOTOKBN-TABLE.                                          
           03  WRK-BYOTOKBN-T            OCCURS  4.                     
               05  WRK-BYOTOKBN          PIC X(02).                     
      *                                                                 
       01  WRK-BYOTOKBN-DATA             PIC X(02).                     
      *    初回番号(最新の入院歴分)                                                                 
       01  WRK-SHONUM                    PIC 9(03).                     
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *    ワーク入退院履歴
       01  WRK-RECE68-REC. 
           COPY    "CPRCF068.INC"  REPLACING  //RECE68//
                                   BY         //WRK-RECE68//.
      *
      *    ワーク入院履歴
       01  WRK-PTNRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //WRK-PTNRRK-//.
      *
      *    ワーク入力ファイル退避
       01  WRK-RECE61-REC.
           COPY    "CPRCF061.INC"      REPLACING  //RECE61-//
                                       BY         //WRK-RECE61-//.
      *
      *    ワーク生保まとめ
       01  WRK-SEIHO-RECE61-AREA.
           02  WRK-SEIHO-RECE61-REC    OCCURS  50. 
           COPY    "CPRCF061.INC"      REPLACING  //RECE61-//
                                       BY         //WRK-SEH-61-//.
           02  WRK-SEIHO-RECE61-PRTFLG PIC 9(01)  OCCURS  50.
           02  WRK-SEIHO-RECE61-MAX    PIC 9(03).
      *
           02  WRK-KOH-RECE61-REC. 
           COPY    "CPRCF061.INC"      REPLACING  //RECE61-//
                                       BY         //WRK-KOH-61-//.
      *
      *    ワーク保険履歴
       01  TABLE-RECE69-AREA.
           02  TBL-RECE69-OCC          OCCURS  100.
           COPY    "CPRCF069.INC"      REPLACING  //RECE69//
                                       BY         //TBL-RECE69//.
      * 
      *    保険番号マスタ退避ワーク
       01  WRK-HKNNUM-AREA.
           03  WRK-HKNNUM-IDX                  PIC 9(05).
           03  WRK-HKNNUM-MAX                  PIC 9(05).
           03  WRK-CHK-HKNNUM                  PIC X(03).
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
      *    固定値
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
      *    固定端末ＩＤ
           03  WRK-CONS-TERMID     PIC X(32)   VALUE
                                   "0100007FZZZZZZZZZZZZZZZZZZZZZZZZ". 
      *
           03  WRK-CONS-SRYYM-200609
                                   PIC 9(06)   VALUE   200609.      
           03  WRK-CONS-SRYYM-200804
                                   PIC 9(06)   VALUE   200804.      
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
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
      *    病棟情報                                                     
           COPY    "CPSK5001.INC".                                      
      *
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *
           COPY    "CPSK2009.INC".
      *
           COPY    "CPSK200501.INC"
                                   REPLACING  //SYS-200501//
                                   BY         //SYS-25//.
      *
      *    月代わり受給者番号
       01  MONTHLYNUM-REC.
           COPY    "CPMONTHLYNUM.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    旧姓履歴情報
       01  KYUSEIRRK-REC.
           COPY    "CPKYUSEIRRK.INC".
      *
      *    患者特記事項情報
       01  PTTOKKI-REC.
           COPY    "CPPTTOKKI.INC".
      *
      *    分娩管理テーブル
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
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
      *   保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
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
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       SPA-AREA
                                       WRK-AREA
                                       FLG-AREA 
                                       WRK-HKNNUM-AREA
                                       TABLE-HKNNUM-AREA  
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           DISPLAY "WRK-PARA-HOSPNUM=" WRK-PARA-HOSPNUM
           DISPLAY "WRK-PARA-PRTKBN =" WRK-PARA-PRTKBN
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    "RECE680"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (2)
           MOVE    "RECE690"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (3)
           MOVE    "RECE670"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (4)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE68PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE69PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE67PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ＤＢオープン処理
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0620"          TO  JOB-PGID
               MOVE    "保険情報編集"      TO  JOB-SHELLMSG
               PERFORM   900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
               IF      WRK-PARA-SRYYM  NOT =  ZERO
      *            医療機関情報
                   INITIALIZE                      SYSKANRI-REC
                   MOVE    "1001"              TO  SYS-KANRICD
                   MOVE    "*"                 TO  SYS-KBNCD
                   MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)              
                   MOVE    "01"                TO  SYS-STYUKYMD (7:2)
                   MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
                   PERFORM 960-SYSKANRI-READ-SEC
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC         TO  SYS-1001-REC
                   ELSE    
                       MOVE    "医療機関情報が取得できませんでした。"
                                                   TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
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
                   PERFORM   900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key8"              TO  MCP-PATHNAME
                       PERFORM 900-SYSKANRI-READ-SEC
                       PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                                 OR    (SYSKAN-IDX   >=  100)
      *
                           MOVE    MCPDATA-REC   TO  SYS-1001-REC
                           ADD     1             TO  SYSKAN-IDX
                           MOVE    SYS-1001-REC  TO
                                                 TBL-SYS1001(SYSKAN-IDX)
      *
                           MOVE    "tbl_syskanri"  TO  MCP-TABLE
                           MOVE    "key8"          TO  MCP-PATHNAME
                           PERFORM 900-SYSKANRI-READ-SEC
      *
                       END-PERFORM
                   ELSE
                       MOVE    "医療機関情報が取得できませんでした。"
                                                 TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1                 TO  FLG-END
                   END-IF
      *
      *            カーソルクロース
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               END-IF
           END-IF  
      *
           IF      FLG-END             =   ZERO
      *        患者番号構成管理情報編集
               MOVE    SPACE               TO  SYSKANRI-REC
               INITIALIZE                      SYSKANRI-REC
               MOVE    "1009"              TO  SYS-KANRICD
               MOVE    "*"                 TO  SYS-KBNCD
               MOVE    WRK-PARA-SRYYM      TO  SYS-STYUKYMD (1:6)              
               MOVE    "01"                TO  SYS-STYUKYMD (7:2)
               MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
               PERFORM 960-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1009-REC
                   MOVE    SYS-1009-PTNUMKSIKBN
                                           TO  SPA-1009-PTNUMKSIKBN
                   MOVE    SYS-1009-HJNKSIKBN
                                           TO  SPA-1009-HJNKSIKBN
                   MOVE    SYS-1009-HJNKSINENKBN 
                                           TO  SPA-1009-HJNKSINENKBN
                   MOVE    SYS-1009-HJNKSIRENNUMKBN
                                           TO  SPA-1009-HJNKSIRENNUMKBN
                   MOVE    SYS-1009-HJNKSIRENNUMKETA
                                           TO  SPA-1009-HJNKSIRENNUMKETA
               ELSE
                   MOVE    "患者番号構成管理情報が取得できませんでした"
                                           TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
      *        レセプト編集区分情報
               MOVE    SPACE           TO  SYSKANRI-REC
               INITIALIZE                  SYSKANRI-REC
               MOVE    "2009"          TO  SYS-KANRICD
               MOVE    "*"             TO  SYS-KBNCD
               MOVE    WRK-PARA-SRYYM  TO  SYS-STYUKYMD (1:6)              
               MOVE    "01"            TO  SYS-STYUKYMD (7:2)
               MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
               PERFORM 960-SYSKANRI-READ-SEC
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
               MOVE    SPACE           TO  SYSKANRI-REC
               INITIALIZE                  SYSKANRI-REC
               MOVE    "2005"          TO  SYS-KANRICD
               MOVE    "01"            TO  SYS-KBNCD
               MOVE    WRK-PARA-SRYYM  TO  SYS-STYUKYMD (1:6)
               MOVE    "01"            TO  SYS-STYUKYMD (7:2)
               MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
               PERFORM 960-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI    =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-25-REC
               ELSE    
                   INITIALIZE                  SYS-25-REC
               END-IF
               IF      SYS-25-DRCD-PRTKBN
                                       =   SPACE
                   MOVE    "0"         TO  SYS-25-DRCD-PRTKBN
               END-IF
               IF      SYS-25-NYUTAIINREKI-NYU-KSIKBN
                                       =   SPACE
                   MOVE    "0"         TO
                                   SYS-25-NYUTAIINREKI-NYU-KSIKBN
               END-IF
      *
               DISPLAY "SYS-200501-DRCD-PRTKBN            ="
                                    SYS-25-DRCD-PRTKBN
               DISPLAY "SYS-200501-NYUTAIINREKI-NYU-KSIKBN="
                                    SYS-25-NYUTAIINREKI-NYU-KSIKBN
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
      *
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       2
                   OR      FLG-END     =   1
      *
               IF      IDZ             =   1               
      *    レセプト処理
                   INITIALIZE                      SGETTEMP-AREA
                   MOVE    "RECE610"           TO  RECEDAT1PARA-FILE-ID
                   PERFORM 800-FILENM-SET-SEC
                   MOVE    RECEDAT1PARA-BASENAME
                                               TO  SGETTEMP-BASENAMES(1)
                   MOVE    "RECE620"           TO  RECEDAT1PARA-FILE-ID
                   PERFORM 800-FILENM-SET-SEC
                   MOVE    RECEDAT1PARA-BASENAME
                                               TO  SGETTEMP-BASENAMES(2)
               ELSE
      *    請求書処理
                   INITIALIZE                      SGETTEMP-AREA
                   MOVE    "RECE61X"           TO  RECEDAT1PARA-FILE-ID
                   PERFORM 800-FILENM-SET-SEC
                   MOVE    RECEDAT1PARA-BASENAME
                                               TO  SGETTEMP-BASENAMES(1)
                   MOVE    "RECE62X"           TO  RECEDAT1PARA-FILE-ID
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
                                           TO  RECE61PARA
               MOVE    SGETTEMP-FULLNAMES (2)
                                           TO  RECE62PARA
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
           MOVE    ZERO                TO  FLG-RECE61
           MOVE    LOW-VALUE           TO  KEY-AREA1
      *
           OPEN    OUTPUT              RECE62-FILE
           CLOSE                       RECE62-FILE
      *
           IF      IDZ             =   1               
               OPEN    OUTPUT              RECE68-FILE
               CLOSE                       RECE68-FILE
               OPEN    I-O                 RECE68-FILE
      *
               OPEN    OUTPUT              RECE69-FILE
               CLOSE                       RECE69-FILE
               OPEN    OUTPUT              RECE69-FILE
      *
               OPEN    OUTPUT              RECE67-FILE
               CLOSE                       RECE67-FILE
           END-IF    
      *
           OPEN    INPUT               RECE61-FILE
           IF      STS-RECE61          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE61PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE61          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    I-O                 RECE62-FILE
      *
           PERFORM     RECE61-READ-SEC
      *
           PERFORM                     UNTIL   FLG-RECE61  =   1
                                       OR      FLG-END     =   1
      *
               MOVE    ZERO            TO  FLG-SEIHO 
               INITIALIZE                  WRK-SEIHO-RECE61-AREA
                                           TABLE-RECE69-AREA
      *
               MOVE    KEY-NEW11       TO  KEY-OLD11
               PERFORM         UNTIL   FLG-RECE61      =   1
                               OR      FLG-END         =   1 
                               OR      KEY-NEW11   NOT =   KEY-OLD11
                   MOVE    ZERO            TO  FLG-SEIHO
                   INITIALIZE                  WRK-SEIHO-RECE61-AREA
      *
                   MOVE    KEY-NEW1        TO  KEY-OLD1
                   PERFORM         UNTIL   FLG-RECE61      =   1
                                   OR      FLG-END         =   1 
                                   OR      KEY-NEW1    NOT =   KEY-OLD1
                       IF      IDZ             =   1               
      *                    保険履歴情報編集処理
                           PERFORM 230-RECE69-HENSYU-SEC
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
                               IF      RECE61-HKNNUM   NOT =
                                                       "060" AND "067"
                                                   AND "068" AND "069" 
                                                   AND "039" AND "040"    
                                   PERFORM 210-HENSYU-SEC
                               END-IF
                           WHEN    "2"
                               IF      RECE61-HKNNUM       =
                                                       "060" OR  "067"
                                                   OR  "068" OR  "069"
                                   PERFORM 210-HENSYU-SEC
                               END-IF
                           WHEN    "7"
                               IF      RECE61-HKNNUM       =
                                                       "039" OR  "040"
                                   PERFORM 210-HENSYU-SEC
                               END-IF
                       END-EVALUATE
      *
                       PERFORM     RECE61-READ-SEC
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
                       IF    ( WRK-SEIHO-RECE61-MAX    >   ZERO )
                       AND   ( FLG-END                 =   ZERO )
                           IF      FLG-RECE61          =   ZERO
                               MOVE    RECE61-REC  TO  WRK-RECE61-REC
                           END-IF
                           PERFORM 250-SEIHO-RECE61-WRITE-SEC
                           IF      FLG-RECE61          =   ZERO
                               MOVE    WRK-RECE61-REC  TO  RECE61-REC
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
      *
               IF      IDZ             =   1               
      *            保険履歴情報編集処理
                   PERFORM VARYING IDWW    FROM    1   BY  1
                           UNTIL   IDWW    >       100
                           OR      TBL-RECE69-HOSPNUM (IDWW)  =   ZERO
                       MOVE    TBL-RECE69-OCC (IDWW)   TO  RECE69-REC
                       WRITE   RECE69-REC
      *
                       IF      STS-RECE69      =   "00"
                           ADD     1               TO  CNT-RECE69
                       ELSE
                           MOVE    SPACE           TO  WRK-ERR-AREA
                           INITIALIZE                  WRK-ERR-AREA
                           MOVE    3               TO  WRK-ERR-FLG
                           MOVE    RECE69PARA      TO  WRK-ERR-FILE-NM
                           MOVE    STS-RECE69      TO  WRK-ERR-FILE-STS
      ****                 PERFORM 500-FILE-ERR-ABORT-SEC
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           CLOSE   RECE61-FILE
                   RECE62-FILE
      *
           IF      IDZ             =   1               
               CLOSE   RECE69-FILE
                       RECE68-FILE
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
           IF      RECE61-HKNNUM        =   SPACE
      *********IF      RECE61-KOHHKNNUM-O (1)  = "020" OR  "030"   OR
      *********                                  "053"
               IF      RECE61-KOHHKNNUM-O (1)  = "020" OR  "030"
                   CONTINUE
               ELSE 
                   IF      RECE61-TEKSTYMD     =   ZERO
                       IF    ( RECE61-KOHHKNNUM-O (1)
                                                   =   "012" OR  "025"
                                                   OR  "015" OR  "016"
                                                   OR  "021" OR  SPACE )
      *                    同じ公費の公費単独の場合
                           IF    ( RECE61-KOHHKNNUM-O  (2)
                                           =   RECE61-KOHHKNNUM-O (1) 
                                           OR  SPACE                  )
                           AND   ( RECE61-KOHHKNNUM-O  (3)
                                           =   RECE61-KOHHKNNUM-O (1) 
                                           OR  SPACE                  )
                           AND   ( RECE61-KOHHKNNUM-O  (4)
                                           =   RECE61-KOHHKNNUM-O (1) 
                                           OR  SPACE                  )
                           AND   ( RECE61-KOHHKNNUM-O  (5)
                                           =   RECE61-KOHHKNNUM-O (1) 
                                           OR  SPACE                  )
                           AND   ( RECE61-KOHHKNNUM-O  (6)
                                           =   RECE61-KOHHKNNUM-O (1) 
                                           OR  SPACE                  )
                           AND   ( RECE61-KOHHKNNUM-O  (7)
                                           =   RECE61-KOHHKNNUM-O (1) 
                                           OR  SPACE                  )
                           AND   ( RECE61-KOHHKNNUM-O  (8)
                                           =   RECE61-KOHHKNNUM-O (1) 
                                           OR  SPACE                  )
                           AND   ( RECE61-KOHHKNNUM-O  (9)
                                           =   RECE61-KOHHKNNUM-O (1) 
                                           OR  SPACE                  )
                           AND   ( RECE61-KOHHKNNUM-O  (10)
                                           =   RECE61-KOHHKNNUM-O (1) 
                                           OR  SPACE                  )
                               GO  TO  210-HENSYU-EXT
                           ELSE
      *********************    IF    ( RECE61-KOHHKNNUM-O  (2)
      *********************                    =   "012"   OR  "025"
      *********************                    OR  "015"   OR  "016"
      *********************                    OR  "021"   OR  SPACE )
      *********************    AND   ( RECE61-KOHHKNNUM-O  (3)
      *********************                    =   "012"   OR  "025"
      *********************                    OR  "015"   OR  "016"
      *********************                    OR  "021"   OR  SPACE )
      *********************    AND   ( RECE61-KOHHKNNUM-O  (4)
      *********************                    =   "012"   OR  "025"
      *********************                    OR  "015"   OR  "016"
      *********************                    OR  "021"   OR  SPACE )
      *********************        生保、中国残留邦人等、自立支援のみの
      *********************        公費単独の場合退避
      *********************        MOVE    RECE61-REC      TO
      *********************                        WRK-KOH-RECE61-REC
      *********************        GO  TO  210-HENSYU-EXT
      *********************    ELSE
                                   MOVE    1               TO  FLG-SEIHO
      *********************    END-IF
                           END-IF
                       ELSE
                           MOVE    1               TO  FLG-SEIHO
                       END-IF
                   ELSE
                       IF    ( FLG-SEIHO            =   1     )
                       AND   ( RECE61-KOHHKNNUM-O (1)
                                                NOT =   "053" )
                           GO  TO  210-HENSYU-EXT
                       ELSE
      *                    公費単独の場合退避
                           ADD     1            TO  WRK-SEIHO-RECE61-MAX
                           MOVE    RECE61-REC   TO  WRK-SEIHO-RECE61-REC
                                                  (WRK-SEIHO-RECE61-MAX)
                           GO  TO  210-HENSYU-EXT
                       END-IF
                   END-IF
              END-IF
           END-IF
      *
      *    社保で公費併用の場合（生保、中国残留邦人等、自立支援、児童保護）
           IF    ( RECE61-HKNNUM   NOT =   SPACE )
           AND   ( RECE61-KOHHKNNUM-O (1)
                                       =   "012"   OR  "025"   OR 
                                           "015"   OR  "016"   OR
                                           "021"   OR  "053"      )
           AND   ( RECE61-KOHHKNNUM-O (2)
                                       =   SPACE   OR  "972"   OR
                                           "974"                  )
               IF      RECE61-HKNNUM       =   "060"   OR  "067"   OR
                                               "068"   OR  "069"   OR
                                               "039"   OR  "040"
                   CONTINUE
               ELSE
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       WRK-SEIHO-RECE61-MAX
      *              分割区分毎にまとめる
                     IF      RECE61-RECEIPT-KBN    =
                                   WRK-SEH-61-RECEIPT-KBN (IDXX)
                       PERFORM VARYING IDYY    FROM    1   BY  1
                               UNTIL   IDYY    >       10
                               OR      RECE61-KOHID-O (IDYY)
                                               =       ZERO
                           IF      WRK-SEH-61-KOHID-O (IDXX 1)
                                       =   RECE61-KOHID-O (IDYY)
                               MOVE    1           TO
                                           WRK-SEIHO-RECE61-PRTFLG(IDXX)
                               MOVE    1           TO  WRK-SEIHO-FLG
                               PERFORM VARYING IDZZ    FROM    1   BY  1
                                       UNTIL   IDZZ    >       10
                                   IF      RECE61-HKNCOMBI-O (IDZZ)
                                               =   ZERO
                                       MOVE
                                           WRK-SEH-61-HKNCOMBI-TBL
                                                              (IDXX 1)
                                                           TO
                                               RECE61-HKNCOMBI-TBL(IDZZ)
                                   END-IF
                                   IF      RECE61-HKNCOMBI-O (IDZZ)
                                               =
                                             WRK-SEH-61-HKNCOMBI-O
                                                              (IDXX 1)
                                       MOVE    10          TO  IDZZ
                                   END-IF
                               END-PERFORM    
                               MOVE    10          TO  IDYY
                               MOVE    WRK-SEIHO-RECE61-MAX
                                                   TO  IDXX
                           END-IF
                       END-PERFORM
                     END-IF
                   END-PERFORM
               END-IF                     
           END-IF
      *
      *    ファイル編集処理
           PERFORM 2101-RECE62-HENSYU-SEC
           .
       210-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ファイル編集処理
      *****************************************************************
       2101-RECE62-HENSYU-SEC           SECTION.
      *
           MOVE    SPACE                TO  WK-RECESYUBETU-AREA
           INITIALIZE                       WK-RECESYUBETU-AREA
      *     
           MOVE    ZERO                 TO  TOKKI-IDX
      *
           MOVE    SPACE                TO  RECE62-REC
           INITIALIZE                       RECE62-REC
      *
           MOVE    SPACE                TO  RJN-RATE-KBN
      *
           MOVE    RECE61-HOSPNUM       TO  RECE62-HOSPNUM
           MOVE    RECE61-SRYYM         TO  RECE62-SRYYM
           MOVE    RECE61-NYUGAIKBN     TO  RECE62-NYUGAIKBN
           MOVE    RECE61-PTID          TO  RECE62-PTID
           MOVE    RECE61-RECEKA        TO  RECE62-RECEKA
           IF      RECE61-HKNNUM (1:2)  =   "98"    OR  "90"    OR
                                            "91"
               MOVE    RECE61-HKNNUM        TO  RECE62-HKNJANUM-KEY
           ELSE  
               MOVE    RECE61-HKNJANUM      TO  RECE62-HKNJANUM-KEY
           END-IF
           MOVE    RECE61-HOJOKBN       TO  RECE62-HOJOKBN-KEY
           IF    ( RECE61-HKNNUM        =   SPACE                    )
           AND   ( RECE61-KOHHKNNUM-O (1)
                                        = "020" OR  "030"  OR  "053" )
               MOVE    RECE61-KOH-KOHID TO  RECE62-KOHID-KEY
           END-IF
           IF    ( RECE61-HKNNUM        =   SPACE            )
           AND   ( RECE61-KOHHKNNUM-O (1)
                                        =   "012"            )
               MOVE    RECE61-KOH-KOHID TO  RECE62-KOHID-KEY
           END-IF
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
                   OR      RECE61-TEKSTYMDX (IDX)  =   SPACE
               IF      RECE62-TEKSTYMD     =   SPACE 
                   MOVE    RECE61-TEKSTYMDX (IDX)  TO  RECE62-TEKSTYMD
               ELSE
                   IF      RECE62-TEKSTYMD <   RECE61-TEKSTYMDX (IDX)
                       MOVE    RECE61-TEKSTYMDX (IDX)  
                                               TO  RECE62-TEKSTYMD
                   END-IF
               END-IF
               IF      RECE62-TEKSTYMD-X   =   SPACE 
                   MOVE    RECE61-TEKSTYMDX (IDX)  TO  RECE62-TEKSTYMD-X
                   MOVE    RECE61-TEKEDYMDX (IDX)  TO  RECE62-TEKEDYMD-X
               ELSE
                   IF      RECE62-TEKSTYMD-X >   RECE61-TEKSTYMDX (IDX)
                       MOVE    RECE61-TEKSTYMDX (IDX)  
                                               TO  RECE62-TEKSTYMD-X
                   END-IF
                   IF      RECE62-TEKEDYMD-X <   RECE61-TEKEDYMDX (IDX)
                       MOVE    RECE61-TEKEDYMDX (IDX)  
                                               TO  RECE62-TEKEDYMD-X
                   END-IF
               END-IF
           END-PERFORM
           MOVE    RECE61-RECEIPT-KBN  TO  RECE62-RECEIPT-KBN
      *    患者番号
           MOVE    RECE61-PTNUM        TO  RECE62-PTNUM
      *    診療科情報のセット
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       20
                   OR      RECE61-SRYKA (IDX)  =   SPACE
               MOVE    RECE61-SRYKA (IDX)  TO  RECE62-SRYKA (IDX)
           END-PERFORM
      *    審査支払機関
           IF      RECE61-HKNNUM        =   "060"   OR    "067"   OR
                                            "068"   OR    "069"
               MOVE    2                TO  RECE62-TEISYUTUSAKI
           ELSE
               IF      RECE61-HKNNUM        =   "039"   OR    "040"
                   MOVE    6                TO  RECE62-TEISYUTUSAKI
               ELSE
                   MOVE    1                TO  RECE62-TEISYUTUSAKI
               END-IF
           END-IF
      *    患者情報の取得
           MOVE    RECE61-KANANAME     TO  RECE62-KANANAME
           MOVE    RECE61-NAME         TO  RECE62-NAME
           MOVE    RECE61-SEX          TO  RECE62-SEX
           MOVE    RECE61-BIRTHDAY     TO  RECE62-BIRTHDAY
           MOVE    RECE61-DEATHKBN     TO  RECE62-DEATHKBN
           MOVE    RECE61-TSTPTNUMKBN  TO  RECE62-TSTPTNUMKBN
           MOVE    RECE61-AGE          TO  WRK-NENREI
                                           RECE62-AGE
           MOVE    RECE61-SYUGAKUKBN   TO  RECE62-SYUGAKUKBN
      *
           MOVE    RECE61-HOME-POST    TO  RECE62-HOME-POST
           MOVE    RECE61-HOME-ADRS    TO  RECE62-HOME-ADRS
           MOVE    RECE61-HOME-BANTI   TO  RECE62-HOME-BANTI
           MOVE    RECE61-RENRAKU-POST TO  RECE62-RENRAKU-POST
           MOVE    RECE61-RENRAKU-NAME TO  RECE62-RENRAKU-NAME
           MOVE    RECE61-RENRAKU-ADRS TO  RECE62-RENRAKU-ADRS
           MOVE    RECE61-RENRAKU-BANTI
                                       TO  RECE62-RENRAKU-BANTI
           MOVE    RECE61-RENRAKU-TEL1 TO  RECE62-RENRAKU-TEL1
           MOVE    RECE61-RENRAKU-TEL2 TO  RECE62-RENRAKU-TEL2
      *    旧姓履歴の取得（月途中で保険変更があるとき）
           IF    ( RECE61-TEISEIKBN    NOT =   "1"   )
           AND   ( RECE62-TEKSTYMD-X   NOT =   SPACE )
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE    RECE62-SRYYM        TO  LNK-DAY7-YM
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
               IF    ( RECE62-TEKEDYMD-X   <   LNK-DAY7-KOYOMI )
                   INITIALIZE                      KYUSEIRRK-REC
                   MOVE    RECE62-HOSPNUM      TO  KYUSEI-HOSPNUM
                   MOVE    RECE62-PTID         TO  KYUSEI-PTID
                   MOVE    RECE62-TEKEDYMD-X   TO  KYUSEI-CHGYMD
                   MOVE    KYUSEIRRK-REC       TO  MCPDATA-REC
                   PERFORM 900-KYUSEIRRK-READ-SEC
                   IF       FLG-KYUSEIRRK      =   ZERO
                       MOVE    KYUSEI-NAME         TO  RECE62-NAME
                       MOVE    KYUSEI-KANANAME     TO  RECE62-KANANAME
                   END-IF
               END-IF
           END-IF
      *    県番号
           MOVE    RECE61-PREFNUM      TO  RECE62-PREFNUM
      *    患者保険情報テーブルから主保険情報の取得
           INITIALIZE                      WRK-HKNNUM-REC
      *
           IF  RECE61-HKNID             NOT =   ZERO
               MOVE    RECE61-HKNNUM   TO  RECE62-HKNNUM
               MOVE    RECE61-HKNID    TO  RECE62-HKNID
      *        法別番号のセット
               INITIALIZE                  HKNNUM-REC
               MOVE   RECE62-HOSPNUM   TO  HKN-HOSPNUM
               IF     RECE62-HKNNUM    =   "069"
                   MOVE    "067"           TO  HKN-HKNNUM
               ELSE
                   MOVE   RECE62-HKNNUM    TO  HKN-HKNNUM
               END-IF
               PERFORM 900-HKNNUM-CHK-SEC
               IF      FLG-HKNNUM      =   ZERO
                   MOVE    HKNNUM-REC      TO  WRK-HKNNUM-REC
                   MOVE    HKN-HBTNUM      TO  RECE62-HKNHBTNUM
                   MOVE    SPACE           TO  WRK-JIHI-FLG
                   IF      HKN-HKNKOHSBTKBN    =   "8"
                       MOVE   "1"              TO  WRK-JIHI-FLG
                   END-IF
                   IF      HKN-HKNKOHSBTKBN    =   "9"
                       MOVE   "2"              TO  WRK-JIHI-FLG
                   END-IF
               END-IF
      *                 
               MOVE    RECE61-HKN-HKNJANUM  TO  RECE62-HKNJANUM
               MOVE    RECE61-HKN-HONKZKKBN TO  RECE62-HONKZKKBN
                                                WK-HONKZKKBN
               MOVE    RECE61-HKN-HOJOKBN   TO  RECE62-HOJOKBN
               MOVE    RECE61-CONTKBN       TO  RECE62-CONTKBN
               IF      RECE62-HKNNUM    NOT  =   "039"
                   MOVE    RECE61-KIGO          TO  RECE62-KIGO
               END-IF
               MOVE    RECE61-NUM           TO  RECE62-NUM
               MOVE    RECE61-CERTNUM       TO  RECE62-CERTNUM
           END-IF
      *    患者公費情報テーブルから老人保険情報の取得
           IF      RECE61-SRYYM            <   WRK-CONS-SRYYM-200804
             IF  RECE61-KOHHKNNUM-O(1)        =       "027"
             OR  RECE61-KOHID                 >       ZERO
               INITIALIZE                           PTKOHINF-REC
               MOVE    RECE61-HOSPNUM       TO      PTKOH-HOSPNUM
               MOVE    RECE61-PTID          TO      PTKOH-PTID
               MOVE    RECE61-KOHID         TO      PTKOH-KOHID
               MOVE    PTKOHINF-REC         TO      MCPDATA-REC
               MOVE    "tbl_ptkohinf"       TO      MCP-TABLE
               MOVE    "key"                TO      MCP-PATHNAME
               PERFORM   900-DBSELECT-SEC
               IF      MCP-RC               =       ZERO
                   MOVE    "tbl_ptkohinf"   TO      MCP-TABLE
                   MOVE    "key"            TO      MCP-PATHNAME
                   PERFORM  PTKOHINF-READ-SEC
                   IF       FLG-PTKOHINF        =   ZERO
                        MOVE   PTKOH-KOHID      TO  RECE62-RJNHKNID
                        MOVE   PTKOH-KOHNUM     TO  RECE62-RJNHKNNUM
                        IF  ( PTKOH-TEKSTYMD (1:6)
                                                  =   RECE61-SRYYM    )
                        AND ( PTKOH-TEKSTYMD      >   RECE62-TEKSTYMD )
                           MOVE    PTKOH-TEKSTYMD  TO  RECE62-TEKSTYMD
                        END-IF
      *                 法別番号のセット
                        INITIALIZE                  HKNNUM-REC
                        MOVE   RECE61-HOSPNUM   TO  HKN-HOSPNUM
                        MOVE   PTKOH-KOHNUM     TO  HKN-HKNNUM
                        PERFORM 900-HKNNUM-CHK-SEC
                        IF       FLG-HKNNUM     =   ZERO
                            MOVE   HKN-HBTNUM TO RECE62-RJNHBTNUM
                        END-IF
      *                  
                        MOVE   PTKOH-FTNJANUM   TO  RECE62-RJNFTNJANUM
                        MOVE   PTKOH-JKYSNUM    TO  RECE62-RJNJKYSNUM
                        MOVE    PTKOH-PAYKBN    TO  RECE62-RJNPAYKBN
      *                 老人保険の患者について負担割合を退避
                        IF   RECE62-SRYYM   >=   "200210"
                            EVALUATE    PTKOH-PAYKBN
                                WHEN    "01"
                                    MOVE      "1"   TO  RJN-RATE-KBN
                                WHEN    "02"
                                    MOVE      "2"   TO  RJN-RATE-KBN
                                WHEN    "03"
                                    MOVE      "3"   TO  RJN-RATE-KBN
                            END-EVALUATE
                        END-IF
                   END-IF
               END-IF
               MOVE    "tbl_ptkohinf"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
             END-IF
           END-IF
      *
      *    長期高額療養費の編集
           MOVE    SPACE              TO   WRK-CHOKI
           MOVE    SPACE              TO   WRK-CHOKI2
           PERFORM   VARYING   IDX   FROM    1   BY   1
                     UNTIL    (IDX     >   10)    
                     OR       (RECE61-KOHHKNNUM-O(IDX)  =   SPACE)
               IF    RECE61-KOHHKNNUM-O(IDX)     =    "027"
                   CONTINUE
               ELSE
                 INITIALIZE                        HKNNUM-REC
                 MOVE    RECE61-HOSPNUM        TO  HKN-HOSPNUM
                 MOVE    RECE61-KOHHKNNUM-O(IDX) TO  HKN-HKNNUM
                 MOVE    "00"                  TO  HKN-PAYKBN
                 MOVE    RECE61-SRYYM          TO  HKN-TEKSTYMD(1:6)
                 MOVE    "01"                  TO  HKN-TEKSTYMD(7:2)
                 MOVE    RECE61-SRYYM          TO  HKN-TEKEDYMD(1:6)
                 MOVE    "01"                  TO  HKN-TEKEDYMD(7:2)
                 MOVE    HKNNUM-REC            TO  MCPDATA-REC
                 MOVE    "tbl_hknnum"          TO  MCP-TABLE
                 MOVE    "key"                 TO  MCP-PATHNAME
                 PERFORM   900-DBSELECT-SEC
                 IF      MCP-RC                =   ZERO
                   MOVE    "tbl_hknnum"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM  HKNNUM-READ-SEC
                   IF       FLG-HKNNUM    =   0
                       IF   HKN-HKNKOHSBTKBN   =   "2"
                           EVALUATE  HKN-HKNNUM
                            WHEN     "972"
                              PERFORM VARYING TOKKI-IDX2 FROM 1 BY 1
                                       UNTIL  TOKKI-IDX2   >  5
                                IF  RECE62-TOKKI(TOKKI-IDX2) = SPACE
                                  MOVE "02" TO RECE62-TOKKI(TOKKI-IDX2)
                                  MOVE   5  TO TOKKI-IDX2
                                ELSE
                                  IF  RECE62-TOKKI(TOKKI-IDX2) = "02"
                                    MOVE 5  TO TOKKI-IDX2
                                  END-IF
                                END-IF
                              END-PERFORM
                              MOVE    RECE61-KOHHKNNUM-O(IDX)
                                                   TO  WRK-CHOKI
                            WHEN     "974"
                              PERFORM VARYING TOKKI-IDX2 FROM 1 BY 1
                                       UNTIL  TOKKI-IDX2   >  5
                                IF  RECE62-TOKKI(TOKKI-IDX2) = SPACE
                                  MOVE "16" TO RECE62-TOKKI(TOKKI-IDX2)
                                  MOVE   5  TO TOKKI-IDX2
                                ELSE
                                  IF  RECE62-TOKKI(TOKKI-IDX2) = "16"
                                    MOVE 5  TO TOKKI-IDX2
                                  END-IF
                                END-IF
                              END-PERFORM
                              MOVE    RECE61-KOHHKNNUM-O(IDX)
                                                   TO  WRK-CHOKI2
                           END-EVALUATE
                       END-IF     
                   END-IF
                 END-IF
                 MOVE    "tbl_hknnum"       TO  MCP-TABLE
                 MOVE    "key"              TO  MCP-PATHNAME
                 PERFORM 900-CLOSE-SEC
               END-IF
           END-PERFORM
      *
           PERFORM VARYING TOKKI-IDX2 FROM 1 BY 1
                    UNTIL (TOKKI-IDX2               >  5   )
                     OR   (RECE62-TOKKI(TOKKI-IDX2) = SPACE)
               MOVE   TOKKI-IDX2  TO  TOKKI-IDX
           END-PERFORM
      *
      *    公費情報のセット
           MOVE    ZERO        TO  IDX2
           PERFORM   VARYING   IDX    FROM   1   BY   1
                     UNTIL    (IDX          >     10)
                     OR       (RECE61-KOHID-O(IDX)  =   ZERO)  
               IF  (RECE61-KOHHKNNUM-O (IDX)  =       "027")
                     CONTINUE
               ELSE
                   IF      IDX                >   4
                       MOVE    "9"                TO  RECE61-TEISEIKBN
                       MOVE    10                 TO  IDX
                   ELSE
                       IF      RECE61-KOHHKNNUM-O (IDX)  =   WRK-CHOKI
                                                        OR   WRK-CHOKI2
                           MOVE   "4"             TO  WRK-RECESKYKBN
                       ELSE
                           MOVE   RECE61-RECESKYKBN-O (IDX)
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
                   OR          RECE61-HKNCOMBI-O(IDX)  =   ZERO
               MOVE    RECE61-HKNCOMBI-O(IDX)
                                       TO  RECE62-HKNCOMBI(IDX)
               MOVE    RECE61-HKNNUMHC (IDX)
                                       TO  RECE62-HKNNUMHC(IDX)
               PERFORM VARYING IDY2    FROM    1   BY  1
                       UNTIL   IDY2    >       4
                   IF   ( RECE61-KOHNUMHC (IDX IDY2)
                                       NOT =   "027"       )
                   AND  ( RECE61-KOHNUMHC (IDX IDY2) (1:1)
                                           =   "0" OR  "9" )
                   AND  ( RECE61-KOHNUMHC (IDX IDY2) (1:2)
                                       NOT =   "99"        )
                   AND  ( RECE61-KOHNUMHC (IDX IDY2)
                                       NOT =   "970"       )
                       MOVE    1           TO  RECE62-HKNCOMBIFLG(IDX)
                       MOVE    4           TO  IDY2
                   END-IF
               END-PERFORM
      *
               MOVE    ZERO     TO   FLG-TAISYO
               PERFORM VARYING IDY2    FROM    1   BY  1
                       UNTIL   IDY2    >       4
                   IF   RECE61-KOHNUMHC (IDX IDY2) NOT = SPACE
                     IF RECE61-KOHNUMHC (IDX IDY2) = "972" OR "974"
                        IF   FLG-TAISYO  =  ZERO
                           MOVE   1     TO   FLG-TAISYO
                        END-IF
                     ELSE
                        MOVE      9     TO   FLG-TAISYO
                     END-IF
                   END-IF
               END-PERFORM
               IF   FLG-TAISYO  =  1
                        MOVE      2     TO   RECE62-HKNCOMBIFLG(IDX)
               END-IF
      *
               IF   RECE62-HKNNUMHC(IDX)  =  SPACE
                 MOVE    ZERO     TO   FLG-TAISYO
                 PERFORM VARYING IDY2    FROM    1   BY  1
                         UNTIL   IDY2    >       4
                   IF   RECE61-KOHNUMHC (IDX IDY2) NOT = SPACE
                     IF   FLG-TAISYO  =  ZERO
                        MOVE      1     TO   FLG-TAISYO
                     ELSE
                        MOVE      9     TO   FLG-TAISYO
                     END-IF
                   END-IF
                 END-PERFORM
                 IF   FLG-TAISYO  =  1
                        MOVE      3     TO   RECE62-HKNCOMBIFLG(IDX)
                 END-IF
               END-IF
      *
               PERFORM VARYING IDY2    FROM    1   BY  1
                       UNTIL   IDY2    >       4
                   IF   RECE61-KOHNUMHC(IDX IDY2) =  "970"
                       MOVE    1        TO   RECE62-DAISANFLG(IDX)
                       MOVE    RECE61-KOHIDHC(IDX IDY2)
                                        TO   RECE62-DAISAN-KOHID(IDX)
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
      *    特記事項のセット
      *    老保の編集
           MOVE    ZERO       TO     FLG-ROUJINHOKEN
           IF      RECE62-HKNNUM        =   SPACE
               IF  (   RECE61-SRYYM         >=  "200210"            )
               AND ((  RECE62-AGE           >=  75             ) OR 
                    (( RECE62-BIRTHDAY      <=  19320930 ) AND
                     ( RECE62-AGE           >=  70       )     )    )
                   ADD      1           TO  TOKKI-IDX
                   MOVE    "04"         TO  RECE62-TOKKI (TOKKI-IDX)
               ELSE
                   INITIALIZE                       PTKOHINF-REC
                   MOVE    RECE61-HOSPNUM       TO  PTKOH-HOSPNUM
                   MOVE    RECE61-PTID          TO  PTKOH-PTID
                   IF      RECE61-SRYYM     <   WRK-CONS-SRYYM-200804
                       MOVE    "027"                TO  PTKOH-KOHNUM
                   ELSE
                       MOVE    "968"                TO  PTKOH-KOHNUM
                   END-IF 
                   MOVE    PTKOHINF-REC         TO  MCPDATA-REC
                   MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                   MOVE    "key4"               TO  MCP-PATHNAME
                   PERFORM   900-DBSELECT-SEC
                   IF      MCP-RC               =       ZERO
                       MOVE    "tbl_ptkohinf"   TO  MCP-TABLE
                       MOVE    "key4"           TO  MCP-PATHNAME
                       PERFORM  PTKOHINF-READ-SEC
                       PERFORM  UNTIL   FLG-PTKOHINF    =   1
                           IF      PTKOH-TEKSTYMD(1:6)  <=  RECE61-SRYYM
                           AND     RECE61-SRYYM         <=
                                                    PTKOH-TEKEDYMD(1:6)
                           AND     PTKOH-SAKJOKBN   =  SPACE
                               ADD      1           TO  TOKKI-IDX
                               MOVE    "04"         TO  RECE62-TOKKI
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
                   MOVE    "tbl_ptkohinf"    TO  MCP-TABLE
                   MOVE    "key4"            TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               END-IF
           END-IF              
      *
      *    低所得情報編集
           IF   RECE62-RECESYUBETU  =  "8888"  OR  "6666"
             CONTINUE
           ELSE
             PERFORM   2101-TEISYOTOKU-HENSYU-SEC
           END-IF
      *
      *    高齢者判定フラグ
           IF     (( RECE62-AGE          >=  70      )   OR
                   ( RECE62-AGE          >=  65  AND
                     RECE61-KOHID    NOT  =   ZERO   )   OR
                   ( FLG-ROUJINHOKEN      =   1      )   OR
                   ( RECE62-HKNNUM        =  "039"   ))
           AND    (  RECE62-KOREIHIGAITO  =  ZERO     )
               MOVE    "1"             TO  RECE62-KOUREI-HANTEIFLG
           END-IF
      *
      *    高齢者フラグ
           MOVE     SPACE    TO    RECE62-KOUREI
           IF  (RECE62-RECESYUBETU   =   "8888" OR "6666")
           OR  (RECE62-HKNNUM        =   SPACE           )
              CONTINUE
           ELSE
              IF      RECE62-SRYYM  >=  "200210"
                IF   (( RECE62-AGE          >=  70      )   OR
                      ( RECE62-AGE          >=  65  AND
                        RECE61-KOHID    NOT  =   ZERO   )   OR
      ***             ( RECE62-AGE          >=  65  AND
      ***               FLG-ROUJINHOKEN      =   1      )   OR
      ***             ( RECE62-AGE          >=  65  AND
      ***               RECE62-HKNNUM        =  "039"   )
                      ( FLG-ROUJINHOKEN      =   1      )   OR
                      ( RECE62-HKNNUM        =  "039"   ))
                AND  (  RECE62-KOREIHIGAITO  =  ZERO     )
                   MOVE     "1"     TO   RECE62-KOUREI
                ELSE
                  IF  RECE62-SRYYM  >=  "200704"
                     MOVE   "2"     TO   RECE62-KOUREI
                  END-IF
                END-IF
              END-IF
           END-IF
      *
      *    病院・診療所区分
           MOVE    SYS-1001-HOSPSBT TO  RECE62-HOSPSBT               
      *
      *    レセプト種別不明エラー情報
           IF      RECE61-TEISEIKBN      =   "1"
               MOVE    RECE61-ERR-ERRKBN     TO  RECE62-ERR-ERRKBN
               MOVE    RECE61-ERR-DAY        TO  RECE62-ERR-DAY
               MOVE    RECE61-ERR-HKNCOMBINUM
                                             TO  RECE62-ERR-HKNCOMBINUM
               MOVE    RECE61-ERR-TEKSTYMD   TO  RECE62-ERR-TEKSTYMD
               MOVE    RECE61-ERR-TEKEDYMD   TO  RECE62-ERR-TEKEDYMD
           END-IF 
      *
           IF      RECE61-TEISEIKBN      =   "9"
               MOVE    88                    TO  RECE62-ERR-ERRKBN
           END-IF
      *
      *    入退院履歴より情報取得
           IF      IDZ             =   1               
               PERFORM 240-PTNYUINRRK-SYORI-SEC
           END-IF
      *
      *    印刷順
           IF      IDZ                 =   1
               MOVE    RECE61-PRTSEQ       TO  RECE62-PRTSEQ
           END-IF
      *
      *    レセプト請求しない地方公費情報
           IF      IDZ                 =   1
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       4
                   MOVE    RECE61-KOHSKY-KOHID-O     (IDX)
                                       TO  RECE62-KOHSKY-KOHID  (IDX)
                   MOVE    RECE61-KOHSKY-KOHHKNNUM-O (IDX)
                                       TO  RECE62-KOHSKY-KOHNUM (IDX)
      *
                   INITIALIZE                          PTKOHINF-REC
                   MOVE    RECE62-HOSPNUM          TO  PTKOH-HOSPNUM
                   MOVE    RECE62-PTID             TO  PTKOH-PTID
                   MOVE    RECE62-KOHSKY-KOHID  (IDX)
                                                   TO  PTKOH-KOHID
                   MOVE    PTKOHINF-REC            TO  MCPDATA-REC
                   MOVE    "tbl_ptkohinf"          TO  MCP-TABLE
                   MOVE    "key"                   TO  MCP-PATHNAME
                   PERFORM   900-DBSELECT-SEC
                   IF      MCP-RC                  =   ZERO
                       MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM PTKOHINF-READ-SEC
                       IF      FLG-PTKOHINF           =   ZERO
                           MOVE    PTKOH-FTNJANUM       TO
                                       RECE62-KOHSKY-KOHFTNJANUM (IDX)
                       END-IF
                   END-IF    
                   MOVE    "tbl_ptkohinf"   TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               END-PERFORM
           END-IF
      *
      *    主科設定
           IF    ( IDZ                 =   1      )
           AND   ( SYS-2009-SYUKA-KBN  =   "1"    )
           AND   ( RECE62-RECESYUBETU  
                                   NOT =   "8888" )
             IF  (     ( RECE62-TEISYUTUSAKI   =   "1" )
                   AND ( SYS-2009-SYUKA-SYAHO
                                           NOT =   "0" ) )
             OR  (     ( RECE62-TEISYUTUSAKI   =   "2" )
                   AND ( SYS-2009-SYUKA-KOKUHO
                                           NOT =   "0" ) )
             OR  (     ( RECE62-TEISYUTUSAKI   =   "6" )
                   AND ( SYS-2009-SYUKA-KOUIKI
                                           NOT =   "0" ) )
               PERFORM 2101-SYUKA-HENSYU-SEC
      *        主科未設定のとき主科取得を行なう
               IF      RECE62-SYUKA            =   "XX"
                   INITIALIZE                      ORCSSYUACCAREA
                   MOVE    RECE62-PREFNUM      TO  SPA-PREFNUM
                   MOVE    RECE62-SRYYM        TO  LNK-SYUACC-I-SRYYM
                   MOVE    RECE62-BIRTHDAY     TO  SPA-BIRTHDAY
                   MOVE    RECE62-PTID          TO  SPA-PTID
                   MOVE    RECE62-NYUGAIKBN    TO  SPA-NYUGAIKBN
                   MOVE   "05"                 TO  LNK-SYUACC-I-KBN
                   CALL   "ORCSSYUKAMAIN"      USING   SPA-AREA
                                                       ORCSSYUACCAREA
      *
                   PERFORM 2101-SYUKA-HENSYU-SEC
               END-IF
      *
      *        主科設定時のみ診療科名称を設定    
               IF  (     ( RECE62-TEISYUTUSAKI   =   "1" )
                     AND ( SYS-2009-SYUKA-SYAHO  =   "1" ) )
               OR  (     ( RECE62-TEISYUTUSAKI   =   "2" )
                     AND ( SYS-2009-SYUKA-KOKUHO =   "1" ) )
               OR  (     ( RECE62-TEISYUTUSAKI   =   "6" )
                     AND ( SYS-2009-SYUKA-KOUIKI =   "1" ) )
                   MOVE    SPACE           TO  SYSKANRI-REC
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    "1005"          TO  SYS-KANRICD
                   MOVE    RECE62-SYUKA    TO  SYS-KBNCD
                   MOVE    WRK-PARA-SRYYM  TO  SYS-STYUKYMD (1:6)              
                   MOVE    "01"            TO  SYS-STYUKYMD (7:2)
                   MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
                   PERFORM 960-SYSKANRI-READ-SEC
                   IF      FLG-SYSKANRI    =   ZERO
                       MOVE    MCPDATA-REC     TO  SYS-1005-REC
                   ELSE
                      INITIALIZE                   SYS-1005-REC
                   END-IF
                   MOVE    SYS-1005-RECESRYKA  TO  RECE62-RECESYUKA
                   MOVE    SYS-1005-SRYKANAME  TO  RECE62-SYUKANAME
               END-IF   
             END-IF
           END-IF    
      *未請求フラグ
           IF    ( IDZ             =   1   )
               MOVE    RECE61-MISEIKYU-FLG     TO  RECE62-MISEIKYU-FLG
           END-IF
      *
      *    医師別出力設定（一括作成時リアルタイムプレビュー時のみ）
           IF    ( IDZ             =   1                     )
           AND   ( SYS-25-DRCD-PRTKBN
                                   =   "1"                   )
           AND   ( RECE62-SRYYM    >=  WRK-CONS-SRYYM-200804 )
             IF    ( WRK-PARA-SRYYM  NOT =   "000000"  )
             OR    ( WRK-PARA-SHELLID    =   "RECEPTX" )
               INITIALIZE                      ORCSDRCDGETAREA
               MOVE    RECE62-HOSPNUM  TO  SPA-HOSPNUM
               MOVE    "1"             TO  LNK-DRCDGET-I-NYUGAIKBN
               MOVE    RECE62-SRYYM    TO  LNK-DRCDGET-I-SRYYM
               MOVE    RECE62-PTID     TO  LNK-DRCDGET-I-PTID
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       10
                   MOVE    RECE62-HKNCOMBI (IDX)
                                       TO  LNK-DRCDGET-I-HKNCOMBI (IDX)
               END-PERFORM
               MOVE    RECE62-NYUINYMD TO  LNK-DRCDGET-I-NYUINYMD
               MOVE    RECE62-TEKSTYMD-X
                                       TO  LNK-DRCDGET-I-TEKSTYMD
               MOVE    RECE62-TEKEDYMD-X
                                       TO  LNK-DRCDGET-I-TEKEDYMD
               CALL    "ORCSDRCDGET"   USING   SPA-AREA
                                               ORCSDRCDGETAREA
               IF      LNK-DRCDGET-O-DRCD-SRYKA
                                       NOT =   SPACE
                   MOVE    LNK-DRCDGET-O-DRCD  TO  RECE62-DRCD
                   MOVE    LNK-DRCDGET-O-DRCD-SRYKA
                                               TO  RECE62-DRCD-SRYKA
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
                                               RECE62-PTTOKKI-CD   (IDX)
                   MOVE    TBL-PTTOKKI-NAIYO
                                          (IDX)    TO  
                                               RECE62-PTTOKKI-NAIYO(IDX)
               END-PERFORM
           END-IF
      *
      *    生保まとめフラグ（公費単独）
           IF      WRK-SEIHO-FLG           =   1
               IF    ( RECE62-KOHNUM (1)       =   "012"   OR  "025"
                                               OR  "015"   OR  "016"
                                               OR  "021"   OR  "053" )
               AND   ( RECE62-KOHNUM (2)       =   SPACE   OR  "972"
                                               OR  "974"             )
                   MOVE    "1"                 TO  RECE62-SEIHOFLG
               END-IF
           END-IF
      *
      *    同日再入院（入院分）の保険組合せ、食事開始時間区分
           MOVE    RECE61-DOUJITSU-AREA    TO  RECE62-DOUJITSU-AREA
      *
      *    点検レセ０点公費フラグ
           IF      IDZ             =   1
               MOVE    RECE61-TENKEN-FLG   TO  RECE62-TENKEN-FLG
           END-IF 
      *
           PERFORM     220-DATA-WRITE-SEC
      *
           .
       2101-RECE62-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主科情報編集処理
      *****************************************************************
       2101-SYUKA-HENSYU-SEC                   SECTION.
      *
           INITIALIZE                      ORCSSYUACCAREA
           MOVE    RECE62-HOSPNUM  TO  SPA-HOSPNUM
           MOVE    RECE62-SRYYM    TO  LNK-SYUACC-I-SRYYM
           MOVE    RECE62-PTID     TO  SPA-PTID
           MOVE    RECE62-NYUGAIKBN
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
           MOVE    RECE62-RECESYUBETU
                                   TO  LNK-SYUACC-IO-RECESYUBETU (1)
           MOVE    RECE62-HKNJANUM TO  LNK-SYUACC-IO-HKNJANUM    (1)
           MOVE    RECE62-HOJOKBN-KEY
                                   TO  LNK-SYUACC-IO-HOJOKBN     (1)
           MOVE    RECE62-TEISYUTUSAKI
                                   TO  LNK-SYUACC-IO-TEISYUTUSAKI(1)
           MOVE    RECE62-KOHID-KEY
                                   TO  LNK-SYUACC-IO-KOHID       (1)
           MOVE    RECE62-RECEIPT-KBN
                                   TO  LNK-SYUACC-IO-RECEIPT-KBN (1)
      *    精神入院か生保単独複数のとき
           IF    ( RECE62-HKNNUM   =   SPACE      )
           AND   ( RECE62-TEKSTYMD
                               NOT =   "00000000" )
               MOVE    RECE62-TEKSTYMD
                                  TO  LNK-SYUACC-IO-HKNJANUM    (1)
           END-IF 
      *
           MOVE    "06"            TO  LNK-SYUACC-I-KBN
      *
      *    点検レセ０点公費記載ありのとき
           IF      RECE61-TENKEN-FLG   =   "1"
               EVALUATE    RECE62-RECESYUBETU (3:1)
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
                       IF      RECE62-RECESYUBETU (2:1)    =   "2"
                           MOVE    "08"            TO  LNK-SYUACC-I-KBN
                       END-IF
               END-EVALUATE
           END-IF
      *
           CALL    "ORCSSYUKAMAIN" USING   SPA-AREA
                                           ORCSSYUACCAREA
           IF      LNK-SYUACC-IO-H-SRYKA
                                   NOT =   SPACE
               MOVE    LNK-SYUACC-IO-H-SRYKA   TO  RECE62-SYUKA
           ELSE
               IF      LNK-SYUACC-IO-SYUKA-INPUT (1)
                                           NOT =   SPACE
                   MOVE    LNK-SYUACC-IO-SYUKA-INPUT (1)
                                                   TO  RECE62-SYUKA
               ELSE
                   IF      LNK-SYUACC-IO-SYUKA-AUTO (1)
                                               NOT =   SPACE
                       MOVE    LNK-SYUACC-IO-SYUKA-AUTO (1) 
                                                   TO  RECE62-SYUKA
                   ELSE
                       MOVE    "XX"                TO  RECE62-SYUKA
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
           MOVE     ZERO     TO    SYOTOKU-IDX-ED
      *
           IF   RECE62-SRYYM   >=  "200604"
              IF  RECE62-HKNNUM      NOT = SPACE
                IF   RECE62-TEKSTYMD(1:6)  >=  RECE62-SRYYM
                    MOVE   RECE62-TEKSTYMD(7:2)    TO   SYOTOKU-IDX
                ELSE
                    MOVE   1                       TO   SYOTOKU-IDX
                END-IF
                IF   RECE62-TEKEDYMD-X(1:6)  <=  RECE62-SRYYM
                    MOVE   RECE62-TEKEDYMD-X(7:2)  TO   SYOTOKU-IDX-ED
                ELSE
                    MOVE   31                      TO   SYOTOKU-IDX-ED
                END-IF
              ELSE
                PERFORM  VARYING   KOHFTN-IDX   FROM   1   BY   1
                          UNTIL    KOHFTN-IDX     >    4
                  IF   (RECE62-KOHNUM(KOHFTN-IDX)  NOT = SPACE)
                  AND  (RECE62-KOHNUM(KOHFTN-IDX)(1:1) = "0"  )
                    IF  RECE62-KOHTEKSTYMD(KOHFTN-IDX)(1:6)
                                           >=  RECE62-SRYYM
                      MOVE   RECE62-KOHTEKSTYMD(KOHFTN-IDX)(7:2)
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
                IF   (RECE62-KOHNUM(KOHFTN-IDX)  NOT = SPACE)
                AND  (RECE62-KOHNUM(KOHFTN-IDX)(1:1)
                                         >=  "1" AND <=  "8")
                  IF  RECE62-KOHTEKSTYMD(KOHFTN-IDX)(1:6)
                                         >=  RECE62-SRYYM
                     MOVE   RECE62-KOHTEKSTYMD(KOHFTN-IDX)(7:2)
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
                  IF   (RECE62-KOHNUM(KOHFTN-IDX)  NOT = SPACE)
                  AND  (RECE62-KOHNUM(KOHFTN-IDX)(1:1)
                                           >=  "1" AND <=  "8")
                     MOVE     1     TO   SYOTOKU-IDX2(KOHFTN-IDX)
                  END-IF
              END-PERFORM
           END-IF
      *
           INITIALIZE                ORCSSYOTOKUAREA
           MOVE  RECE62-HOSPNUM  TO  LNK-SYOTOKU-HOSPNUM
           MOVE  RECE62-PTID     TO  LNK-SYOTOKU-PTID
           MOVE  RECE62-SRYYM    TO  LNK-SYOTOKU-SRYYM
           MOVE  "1"             TO  LNK-SYOTOKU-SKJCHKFLG
           PERFORM  VARYING   SYOTOKU-IDX8   FROM   1   BY   1
                     UNTIL    SYOTOKU-IDX8     >   10
                MOVE  RECE62-HKNCOMBI(SYOTOKU-IDX8)
                                 TO  LNK-SYOTOKU-HKNCOMBI(SYOTOKU-IDX8)
           END-PERFORM
      *
           CALL    "ORCSSYOTOKU"   USING  ORCSSYOTOKUAREA
                                          SPA-AREA
      *
           IF    LNK-SYOTOKU-RC    =   ZERO
                MOVE  LNK-SYOTOKU-SYUKBN(SYOTOKU-IDX)
                                 TO  RECE62-TEISYOTOKU
                PERFORM  VARYING   KOHFTN-IDX   FROM   1   BY   1
                          UNTIL    KOHFTN-IDX     >    4
                   IF  SYOTOKU-IDX2(KOHFTN-IDX) =  99
                     MOVE  SPACE TO  RECE62-TEISYOTOKU2(KOHFTN-IDX)
                   ELSE
                     MOVE  SYOTOKU-IDX2(KOHFTN-IDX)  TO  SYOTOKU-IDX9
                     MOVE  LNK-SYOTOKU-CHIKBN(SYOTOKU-IDX9)
                                 TO  RECE62-TEISYOTOKU2(KOHFTN-IDX)
                   END-IF
                END-PERFORM
      *
                IF    RECE62-SRYYM        >=  "201501"
                   IF    LNK-SYOTOKU-SKJIDX = ZERO
                     MOVE  SPACE    TO  RECE62-TEISYOTOKU3
                   ELSE
                     MOVE  LNK-SYOTOKU-SKJKBN(LNK-SYOTOKU-SKJIDX)
                                    TO  RECE62-TEISYOTOKU3
                     MOVE  LNK-SYOTOKU-KKSKBN(LNK-SYOTOKU-SKJIDX)
                                    TO  RECE62-TEISYOTOKU3-KKSKBN
                   END-IF
                ELSE
                   MOVE  LNK-SYOTOKU-SKJKBN(SYOTOKU-IDX)
                                    TO  RECE62-TEISYOTOKU3
                   IF    (RECE62-TEISYOTOKU3  =   "2")
                   AND   (SYOTOKU-IDX-ED  NOT =  ZERO)
                     PERFORM VARYING SYOTOKU-IDX9 FROM SYOTOKU-IDX BY 1
                              UNTIL  SYOTOKU-IDX9   >  SYOTOKU-IDX-ED
                       IF  LNK-SYOTOKU-SKJKBN(SYOTOKU-IDX9) = "3"
                         MOVE  "3"  TO  RECE62-TEISYOTOKU3
                         MOVE   31  TO  SYOTOKU-IDX9
                       END-IF
                     END-PERFORM
                   END-IF
                END-IF
                PERFORM VARYING SYOTOKU-IDX9 FROM 1 BY 1
                         UNTIL  SYOTOKU-IDX9   > 31
                   MOVE  LNK-SYOTOKU-SKJKBN(SYOTOKU-IDX9)
                            TO  RECE62-TEISYOTOKU3-SKJKBN(SYOTOKU-IDX9)
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
           IF    RECE62-AGE         >=    70
             IF  (RECE62-HKNNUM       =  SPACE OR "040" OR "039"
                                               OR "068" OR "069")
             OR  (RECE62-HKNNUM(1:2)  =   "98" OR  "90" OR  "91")
                CONTINUE
             ELSE
              IF ( RECE62-HKNNUM      = "060"           )
              OR ((RECE62-HKNNUM  NOT = "060"          ) AND
                  (RECE62-HOJOKBN     = SPACE          ))
                MOVE      RECE61-SRYYM       TO   WRK-SRYYYMM
                IF   RECE62-TEKSTYMD(1:6)  >=  RECE61-SRYYM
                  MOVE  RECE62-TEKSTYMD(7:2) TO   WRK-SRYDD
                ELSE
                  MOVE  01                   TO   WRK-SRYDD
                END-IF
      *
                INITIALIZE                       PTKOHINF-REC
                MOVE    RECE61-HOSPNUM       TO  PTKOH-HOSPNUM
                MOVE    RECE61-PTID          TO  PTKOH-PTID
                MOVE    "976"                TO  PTKOH-KOHNUM
                MOVE    PTKOHINF-REC         TO  MCPDATA-REC
                MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                MOVE    "key4"               TO  MCP-PATHNAME
                PERFORM   900-DBSELECT-SEC
                IF      MCP-RC               =   ZERO
                  MOVE    "tbl_ptkohinf"     TO  MCP-TABLE
                  MOVE    "key4"             TO  MCP-PATHNAME
                  PERFORM  PTKOHINF-READ-SEC
                  PERFORM  UNTIL   FLG-PTKOHINF    =   1
                    IF    (PTKOH-TEKSTYMD  <=  WRK-SRYYMD    )
                    AND   (WRK-SRYYMD      <=  PTKOH-TEKEDYMD)
                    AND   (PTKOH-SAKJOKBN   =  SPACE         )
                       MOVE    1    TO   RECE62-KOREIHIGAITO
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
           MOVE    RECE61-HOSPNUM    TO   PTRSI-HOSPNUM
           MOVE    RECE61-PTID       TO   PTRSI-PTID
           MOVE    RECE61-KOHIDHC(IDX IDY2)
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
               IF     RECE61-SRYYM   >=  "201204"
                 IF   PTRSI-DAISAN-GENBTKBN  =  "2"
                   MOVE   1   TO   RECE62-DAISAN-GENBUTUFLG(IDX)
                   MOVE   1   TO   RECE62-DAISAN-GENBUTU-GAITO
                 END-IF
               END-IF
      *        特記事項「１０第三」記載しない
               IF     PTRSI-DAISAN-TOKKIKBN  =  "2"
                   MOVE   1   TO   RECE62-DAISAN-TOKKI
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
           MOVE    RECE61-HOSPNUM       TO      PTKOH-HOSPNUM
           MOVE    RECE61-PTID          TO      PTKOH-PTID
           MOVE    RECE61-KOHID-O(IDX)  TO      PTKOH-KOHID
           MOVE    PTKOHINF-REC         TO      MCPDATA-REC
           MOVE    "tbl_ptkohinf"       TO      MCP-TABLE
           MOVE    "key"                TO      MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_ptkohinf"   TO      MCP-TABLE
               MOVE    "key"            TO      MCP-PATHNAME
               PERFORM  PTKOHINF-READ-SEC
               IF       FLG-PTKOHINF       =   ZERO
                   ADD     1               TO  IDX2
      *
                   MOVE   PTKOH-KOHID      TO  RECE62-KOHID(IDX2)
                   MOVE   PTKOH-KOHNUM     TO  RECE62-KOHNUM(IDX2)
                   MOVE   PTKOH-TEKSTYMD   TO  RECE62-KOHTEKSTYMD(IDX2)
                   MOVE   PTKOH-TEKEDYMD   TO  RECE62-KOHTEKEDYMD(IDX2)
                   MOVE   WRK-RECESKYKBN   TO  RECE62-RECESKYKBN(IDX2)
      *            法別番号・本人−入院−内容のセット
                   INITIALIZE                  HKNNUM-REC
                   MOVE   RECE61-HOSPNUM   TO  HKN-HOSPNUM
                   MOVE   PTKOH-KOHNUM     TO  HKN-HKNNUM
                   PERFORM 900-HKNNUM-CHK-SEC
                   IF       FLG-HKNNUM     =   ZERO
                       MOVE   HKN-HBTNUM           TO
                                           RECE62-KOHHBTNUM      (IDX2)
                       MOVE   HKN-HON-NYUFTNKGN    TO
                                           RECE62-HON-NYUFTNKBN  (IDX2)
                       MOVE   HKN-HON-NYUKFTNRATE  TO
                                           RECE62-HON-NYUKFTNRATE(IDX2)
                       MOVE   HKN-HON-NYUDJGNGAK   TO
                                           RECE62-HON-NYUDJGNGAK (IDX2)
                       MOVE   HKN-HON-NYUMJGNGAK   TO
                                           RECE62-HON-NYUMJGNGAK (IDX2)
                       MOVE   HKN-HON-NYUMJGNCNT   TO
                                           RECE62-HON-NYUMJGNCNT (IDX2)
                       MOVE   HKN-HON-NYUSKJRYYKBN TO
                                           RECE62-HON-NYUSKJRYYKBN(IDX2)
                       MOVE   HKN-TSY-NYUFTNNBK    TO
                                           RECE62-TSY-NYUFTNKBN  (IDX2)
                       MOVE   HKN-TSY-NYUKFTNRATE  TO
                                           RECE62-TSY-NYUKFTNRATE(IDX2)
                       MOVE   HKN-TSY-NYUDJGNGAK   TO
                                           RECE62-TSY-NYUDJGNGAK (IDX2)
                       MOVE   HKN-TSY-NYUMJGNGAK   TO
                                           RECE62-TSY-NYUMJGNGAK (IDX2)
                       MOVE   HKN-TSY-NYUMJGNCNT   TO
                                           RECE62-TSY-NYUMJGNCNT (IDX2)
                       MOVE   HKN-TSY-NYUSKJRYYKBN TO
                                           RECE62-TSY-NYUSKJRYYKBN(IDX2)
                       MOVE   HKN-TNK-NYUFTNKBN    TO
                                           RECE62-TNK-NYUFTNKBN  (IDX2)
                       MOVE   HKN-TNK-NYUKFTNRATE  TO
                                           RECE62-TNK-NYUKFTNRATE(IDX2)
                       MOVE   HKN-TNK-NYUDJGNGAK   TO
                                           RECE62-TNK-NYUDJGNGAK (IDX2)
                       MOVE   HKN-TNK-NYUMJGNGAK   TO
                                           RECE62-TNK-NYUMJGNGAK (IDX2)
                       MOVE   HKN-TNK-NYUMJGNCNT   TO
                                           RECE62-TNK-NYUMJGNCNT (IDX2)
                       MOVE   HKN-TNK-NYUSKJRYYKBN TO
                                           RECE62-TNK-NYUSKJRYYKBN(IDX2)
                   END-IF
      *            
                   MOVE   PTKOH-FTNJANUM   TO RECE62-KOHFTNJANUM(IDX2)
                   MOVE   SPACE            TO RECE62-KOHNINIKYUKBN(IDX2)
      *             
      *            生保又は中国残留邦人等のとき月代わり受給者番号マスタ
      *            より受給者番号をセット 
                   IF      PTKOH-KOHNUM    =   WRK-CONS-SEIHO-HKNNUM
                       MOVE    RECE62-HOSPNUM  TO  MONTHLYNUM-HOSPNUM
                       MOVE    RECE62-PTID     TO  MONTHLYNUM-PTID
                       MOVE    PTKOH-KOHID     TO  MONTHLYNUM-KOHID
                       MOVE    RECE62-SRYYM    TO  MONTHLYNUM-SRYYM
                       MOVE    RECE62-NYUGAIKBN
                                               TO  MONTHLYNUM-NYUGAIKBN
                       MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
                       MOVE    "tbl_monthlynum"
                                               TO  WRK-MCP-TABLE
                       MOVE    "key4"          TO  WRK-MCP-PATHNAME
                       PERFORM 910-MONTHLYNUM-READ-SEC
                       IF    ( FLG-MONTHLYNUM          =   ZERO  )
                           MOVE    MONTHLYNUM-JKYSNUM 
                                           TO  RECE62-KOHJKYSNUM (IDX2)
                       ELSE                       
                           MOVE    RECE62-HOSPNUM  TO MONTHLYNUM-HOSPNUM
                           MOVE    RECE62-PTID     TO MONTHLYNUM-PTID
                           MOVE    ZERO            TO MONTHLYNUM-KOHID
                           MOVE    RECE62-SRYYM    TO MONTHLYNUM-SRYYM
                           MOVE    RECE62-NYUGAIKBN
                                                   TO
                                                   MONTHLYNUM-NYUGAIKBN
                           MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
                           MOVE    "tbl_monthlynum"
                                                   TO  WRK-MCP-TABLE
                           MOVE    "key4"          TO  WRK-MCP-PATHNAME
                           PERFORM 910-MONTHLYNUM-READ-SEC
                           IF    ( FLG-MONTHLYNUM          =   ZERO  )
                               MOVE    MONTHLYNUM-JKYSNUM 
                                           TO  RECE62-KOHJKYSNUM (IDX2)
                           ELSE                       
                               MOVE   PTKOH-JKYSNUM
                                           TO  RECE62-KOHJKYSNUM (IDX2)
                           END-IF
                       END-IF
                   ELSE                       
                       IF      PTKOH-KOHNUM    =
                                           WRK-CONS-ZANRYUU-HKNNUM
                           MOVE    RECE62-HOSPNUM
                                               TO  MONTHLYNUM-HOSPNUM
                           MOVE    RECE62-PTID TO  MONTHLYNUM-PTID
                           MOVE    PTKOH-KOHID TO  MONTHLYNUM-KOHID
                           MOVE    RECE62-SRYYM
                                               TO  MONTHLYNUM-SRYYM
                           MOVE    RECE62-NYUGAIKBN
                                               TO  MONTHLYNUM-NYUGAIKBN
                           MOVE    MONTHLYNUM-REC
                                               TO  MCPDATA-REC
                           MOVE    "tbl_monthlynum"
                                               TO  WRK-MCP-TABLE
                           MOVE    "key4"      TO  WRK-MCP-PATHNAME
                           PERFORM 910-MONTHLYNUM-READ-SEC
                           IF    ( FLG-MONTHLYNUM  =   ZERO  )
                               MOVE    MONTHLYNUM-JKYSNUM 
                                           TO  RECE62-KOHJKYSNUM (IDX2)
                           ELSE                       
                               MOVE   PTKOH-JKYSNUM
                                           TO  RECE62-KOHJKYSNUM (IDX2)
                           END-IF
                       ELSE    
                           MOVE   PTKOH-JKYSNUM
                                           TO  RECE62-KOHJKYSNUM (IDX2)
                       END-IF
                   END-IF    
               END-IF
           END-IF
           MOVE    "tbl_ptkohinf"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    神奈川地方公費対応
           IF    ( RECE62-PREFNUM  =   "14"           )
           AND   ( RECE62-TEISYUTUSAKI
                                   =   "2"            )
      *****AND   ( RECE62-KOHNUM      (IDX2)
      *****                        =   "180" OR "280" )
           AND   ( RECE62-KOHHBTNUM   (IDX2)
                                   =   "80"  )
           AND   ( RECE62-KOHFTNJANUM (IDX2)
                                   =   SPACE          )
               MOVE   "4"          TO  RECE62-RECESKYKBN(IDX2)
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
           IF      RECE61-TEISEIKBN     =   "1"
               GO  TO  RECESYUBETU-SET-EXT
           END-IF
      *    公費が４つを超えたとき
           IF      RECE61-TEISEIKBN     =   "9"
               GO  TO  RECESYUBETU-SET-EXT
           END-IF
      *
      *    地方公費データのとき主保険が無かったら不明分とする
           IF      IDZ                  =   2
           AND     RECE62-HKNNUM        =   SPACE
               MOVE    7                    TO  RECE62-ERR-ERRKBN
               GO  TO  RECESYUBETU-SET-EXT
           END-IF
      *
      *    平成２０年４月改正以降の種別設定
           IF      RECE62-SRYYM        >=  WRK-CONS-SRYYM-200804
               INITIALIZE                  ORCSSYUBETUGETAREA
               MOVE    1               TO  LNK-SYUBETUGET-SYOFLG
               MOVE    RECE61-NYUGAIKBN
                                       TO  LNK-SYUBETUGET-NYUGAIKBN
               MOVE    RECE62-SRYYM    TO  LNK-SYUBETUGET-SRYYM
               MOVE    RECE62-HKNNUM   TO  LNK-SYUBETUGET-HKNNUM
               MOVE    RECE62-HONKZKKBN
                                       TO  LNK-SYUBETUGET-HKN-HONKZKKBN
               MOVE    RECE62-HOJOKBN  TO  LNK-SYUBETUGET-HKN-HOJOKBN
               IF      WRK-JIHI-FLG    =   "1"
                   MOVE    "8"             TO
                                       LNK-SYUBETUGET-HKN-HKNKOHSBTKBN
               END-IF
               IF      WRK-JIHI-FLG    =   "2"
                   MOVE    "9"             TO
                                       LNK-SYUBETUGET-HKN-HKNKOHSBTKBN
               END-IF
               MOVE    WRK-NENREI      TO  LNK-SYUBETUGET-AGE
               MOVE    RECE61-SYUGAKUKBN
                                       TO  LNK-SYUBETUGET-SYUGAKUKBN
               MOVE    RECE61-PREFNUM  TO  LNK-SYUBETUGET-PREFNUM
               MOVE    RECE62-RJNHKNNUM
                                       TO  LNK-SYUBETUGET-RJNHKNNUM
               MOVE    RECE62-RJNPAYKBN
                                       TO  LNK-SYUBETUGET-RJNPAYKBN
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       4
                   MOVE    RECE62-KOHNUM (IDX)     TO
                                       LNK-SYUBETUGET-KOHNUM     (IDX)
                   MOVE    RECE62-KOHHBTNUM (IDX)  TO
                                       LNK-SYUBETUGET-KOHHBTNUM  (IDX)
                   IF      RECE62-RECESKYKBN (IDX) =   SPACE
                       MOVE    "1"                 TO
                                       LNK-SYUBETUGET-RECESKYKBN (IDX)
                   END-IF 
               END-PERFORM
               IF      RECE62-KOREIHIGAITO  =   1
                   MOVE    SPACE       TO  LNK-SYUBETUGET-HKN-HOJOKBN
               END-IF
               MOVE    RECE62-BIRTHDAY TO  LNK-SYUBETUGET-BIRTHDAY
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
                   IF      RECE62-TEKSTYMD (1:6)
                                               >=  RECE61-SRYYM
                       MOVE  RECE62-TEKSTYMD (7:2) TO  WRK-SRYDD
                   ELSE
                       MOVE  01                    TO  WRK-SRYDD
                   END-IF
      *
                   INITIALIZE                      PTKOHINF-REC
                   MOVE    RECE61-HOSPNUM      TO  PTKOH-HOSPNUM
                   MOVE    RECE61-PTID         TO  PTKOH-PTID
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
                                           TO  RECE62-RECESYUBETU
               MOVE    LNK-SYUBETUGET-RECESYUBETU-X
                                           TO  RECE62-RECESYUBETU-X
               MOVE    LNK-SYUBETUGET-RECESYUBETU-Y
                                           TO  RECE62-RECESYUBETU-Y
      *
      *        公費分で種別が設定できなかったとき不明分にしない
               IF    ( IDZ                 =   2    )
               AND   ( RECE62-RECESYUBETU  =   ZERO ) 
                   MOVE    5555                TO  RECE62-RECESYUBETU
               END-IF                 
      *
      *        DISPLAY   "RECE62-RECESYUBETU: " RECE62-RECESYUBETU
               GO  TO  RECESYUBETU-SET-EXT
           END-IF    
      *
      *    自費のときのレセプト種別を編集
           IF     (RECE62-HKNNUM        NOT =   SPACE)   AND
                  (WRK-JIHI-FLG         =   "1"      ) 
               MOVE    8888                 TO  RECE62-RECESYUBETU    
               GO  TO  RECESYUBETU-SET-EXT
           END-IF
      *         
           IF     (RECE62-HKNNUM        NOT =   SPACE)   AND
                  (WRK-JIHI-FLG         =   SPACE  OR  "2" )
               EVALUATE    RECE62-HKNNUM
                   WHEN    "060"
                   WHEN    "067"
                   WHEN    "068"
                   WHEN    "069"
                       IF      RECE62-HKNNUM   =   "060"   OR   "068"
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
           IF      RECE62-SRYYM     <      "200210" 
             IF      RECE62-RJNHKNNUM     =   "027"
                 MOVE        "1"          TO  WK-RJN
             END-IF
           END-IF
      *
           MOVE    ZERO            TO    WK-KOHI-CNT
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       4
               IF    ( RECE62-KOHNUM     (IDX) NOT =   SPACE )
               AND   ( RECE62-RECESKYKBN (IDX)     =   SPACE )
                   ADD     1               TO    WK-KOHI-CNT
               END-IF
           END-PERFORM    
      *
      *    福岡地方公費対応
           IF    ( RECE62-PREFNUM      =   "40"                  )
           AND   ( RECE62-SRYYM        <=  WRK-CONS-SRYYM-200609 )
               IF  WK-KOKUHO               =   "1" OR  "2"
                   IF   (RECE62-KOHNUM   (1)(1:1)  =   "1" OR  "2")
                   AND  (RECE62-KOHHBTNUM(1)   NOT =   "41"       )     
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
                   IF   (RECE62-KOHNUM   (2)(1:1)  =   "1" OR  "2")
                   AND  (RECE62-KOHHBTNUM(2)   NOT =   "41"       )     
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
                   IF   (RECE62-KOHNUM   (3)(1:1)  =   "1" OR  "2")
                   AND  (RECE62-KOHHBTNUM(3)   NOT =   "41"       )     
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
                   IF   (RECE62-KOHNUM   (4)(1:1)  =   "1" OR  "2")
                   AND  (RECE62-KOHHBTNUM(4)   NOT =   "41"       )     
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
               END-IF
           END-IF
      *
      *    ３歳未満の場合、ＷＫ−ＨＯＮＫＺＫＫＢＮに"３"をセット
           IF      RECE61-HKNID            NOT =   ZERO
               IF    ( RECE62-SRYYM         >=  200210                )
               AND   ( RECE62-SRYYM         <   WRK-CONS-SRYYM-200804 )
                   IF      WRK-NENREI         <     3
                               MOVE   "3"     TO   WK-HONKZKKBN
                   END-IF
               END-IF
               IF      RECE62-SRYYM        >=  WRK-CONS-SRYYM-200804
                   IF      RECE61-SYUGAKUKBN   =   1
                       MOVE   "3"          TO  WK-HONKZKKBN
                   END-IF
               END-IF
           END-IF
      *
      *    １割、２割負担の高齢者の場合ＷＫ−ＨＯＮＫＺＫＫＢＮに
      *    "９"または"８"をセット
      *    老人保険の場合はＷＫ−ＲＪＮにセットする	
           IF      RECE62-HKNID             NOT =   ZERO
             EVALUATE    TRUE
               WHEN    RECE62-SRYYM     >=  WRK-CONS-SRYYM-200804
                 EVALUATE    RECE62-HKNNUM
                   WHEN    "002" 
                       EVALUATE   RECE62-HOJOKBN
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
                       EVALUATE   RECE62-HOJOKBN
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
                       IF  (WRK-NENREI  >=   70)  AND
                           (WRK-NENREI  <=   74)
                           EVALUATE   RECE62-HOJOKBN
                               WHEN   "1"
                               WHEN   "5"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "2"
                               WHEN   "6"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                               WHEN   "3"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       END-IF     
                   WHEN    "039" 
                   WHEN    "040" 
                       EVALUATE   RECE62-HOJOKBN
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
                           EVALUATE   RECE62-HOJOKBN
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "?"   TO   WK-HONKZKKBN
                               WHEN   "7"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       END-IF
                   WHEN    OTHER 
                       EVALUATE   RECE62-HOJOKBN
                           WHEN   "9"
                               MOVE   "9"   TO   WK-HONKZKKBN
                           WHEN   "8"
                               MOVE   "?"   TO   WK-HONKZKKBN
                           WHEN   "7"
                               MOVE   "7"   TO   WK-HONKZKKBN
                       END-EVALUATE
                 END-EVALUATE
               WHEN    RECE62-SRYYM     >   WRK-CONS-SRYYM-200609
                 EVALUATE    RECE62-HKNNUM
                   WHEN    "002" 
                       EVALUATE   RECE62-HOJOKBN
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
                       EVALUATE   RECE62-HOJOKBN
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
                       IF  (WRK-NENREI  >=   70)  AND
                           (WRK-NENREI  <=   74)
                           EVALUATE   RECE62-HOJOKBN
                               WHEN   "1"
                               WHEN   "5"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "2"
                               WHEN   "6"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                               WHEN   "3"
                                   MOVE   "7"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       END-IF     
                   WHEN    OTHER 
                       EVALUATE   RECE62-HOJOKBN
                           WHEN   "9"
                               MOVE   "9"   TO   WK-HONKZKKBN
                           WHEN   "8"
                               MOVE   "?"   TO   WK-HONKZKKBN
                           WHEN   "7"
                               MOVE   "7"   TO   WK-HONKZKKBN
                       END-EVALUATE
                 END-EVALUATE
                 IF      RECE62-RJNHKNID >   ZERO
                   EVALUATE   RECE62-RJNPAYKBN
                       WHEN   "01"
                           MOVE   "9"   TO   WK-RJN
                       WHEN   "02"
                           MOVE   "?"   TO   WK-RJN
                       WHEN   "03"
                           MOVE   "7"   TO   WK-RJN
                   END-EVALUATE
                 END-IF
               WHEN    RECE62-SRYYM     >=      "200210"
                 EVALUATE    RECE62-HKNNUM
                   WHEN    "002" 
                       EVALUATE   RECE62-HOJOKBN
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
                       EVALUATE   RECE62-HOJOKBN
                           WHEN   "B"
                           WHEN   "9"   
                               MOVE   "9"   TO   WK-HONKZKKBN
                           WHEN   "E"
                           WHEN   "8"
                               MOVE   "8"   TO   WK-HONKZKKBN
                       END-EVALUATE
                   WHEN    "060" 
                   WHEN    "068" 
                       IF  (WRK-NENREI  >=   70)  AND
                           (WRK-NENREI  <=   74)
                           EVALUATE   RECE62-HOJOKBN
                               WHEN   "1"
                               WHEN   "5"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "2"
                               WHEN   "6"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       END-IF     
                   WHEN    OTHER 
                       EVALUATE   RECE62-HOJOKBN
                           WHEN   "9"
                               MOVE   "9"   TO   WK-HONKZKKBN
                           WHEN   "8"
                               MOVE   "8"   TO   WK-HONKZKKBN
                       END-EVALUATE
                 END-EVALUATE
                 IF      RECE62-RJNHKNID >   ZERO
                   EVALUATE   RECE62-RJNPAYKBN
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
           AND   ( RECE62-HONKZKKBN    =   "1"   )     
               IF      RECE62-HKNNUM   =   "063"   OR  "072"   OR
                                           "073"   OR  "074"   OR
                                           "075"
                   MOVE    "2"          TO  WK-IHO
               END-IF
           END-IF    
      *          
      * 
           INITIALIZE                  ORCSSYUBETUGETAREA  
           MOVE    WK-KOKUHO           TO  LNK-SYUBETUGET-KOKUHO
           MOVE    WK-IHO              TO  LNK-SYUBETUGET-IHO
           MOVE    WK-RJN              TO  LNK-SYUBETUGET-RJN
           MOVE    WK-KOHI-CNT         TO  LNK-SYUBETUGET-KOHI-CNT
           MOVE    WK-HONKZKKBN        TO  LNK-SYUBETUGET-HONKZKKBN
           MOVE    RECE61-NYUGAIKBN    TO  LNK-SYUBETUGET-NYUGAIKBN
           MOVE    RECE62-SRYYM        TO  LNK-SYUBETUGET-SRYYM
           MOVE    RECE62-HKNNUM       TO  LNK-SYUBETUGET-HKNNUM
           CALL    "ORCSSYUBETUGET"    USING   ORCSSYUBETUGETAREA
      *
           MOVE    LNK-SYUBETUGET-RECESYUBETU
                                       TO  RECE62-RECESYUBETU
           MOVE    LNK-SYUBETUGET-RECESYUBETU-X
                                       TO  RECE62-RECESYUBETU-X
      *
      *    公費分で種別が設定できなかったとき不明分にしない
           IF    ( IDZ                 =   2    )
           AND   ( RECE62-RECESYUBETU  =   ZERO ) 
               MOVE    5555                TO  RECE62-RECESYUBETU
           END-IF                 
      *
      *    DISPLAY   "RECE62-RECESYUBETU: " RECE62-RECESYUBETU
           .
       RECESYUBETU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主保険−給付割合のセット
      *****************************************************************
       KYURATE-SET-SEC                SECTION.
      *
      *    主保険情報の補助区分から患者の負担割合を再セット
      *   （国保のみ）
           IF      RECE62-SRYYM    <=  WRK-CONS-SRYYM-200609 
             EVALUATE    RECE62-HKNNUM       
               WHEN    "060"
               WHEN    "068"
                   IF      WRK-NENREI           <     3
                       IF      RECE62-HOJOKBN  =    "0"  OR   "1" OR
                                                     "4"  OR   "5"
                           EVALUATE   RECE62-HOJOKBN
                               WHEN   "0"
                                   MOVE   ZERO    TO   RECE62-KYURATE
                               WHEN   "1"
                                   MOVE   10      TO   RECE62-KYURATE
                               WHEN   "4"
                                   MOVE   ZERO    TO   RECE62-KYURATE
                               WHEN   "5"
                                   MOVE   10      TO   RECE62-KYURATE
                           END-EVALUATE
                       ELSE
                           MOVE  20      TO    RECE62-KYURATE
                       END-IF
                   ELSE
                       EVALUATE   RECE62-HOJOKBN
                           WHEN   "0"
                               MOVE   ZERO      TO   RECE62-KYURATE
                           WHEN   "1"
                               MOVE   10        TO   RECE62-KYURATE
                           WHEN   "2"
                               MOVE   20        TO   RECE62-KYURATE
                           WHEN   "3"
                               MOVE   30        TO   RECE62-KYURATE
                           WHEN   "4"
                               MOVE   ZERO      TO   RECE62-KYURATE
                           WHEN   "5"
                               MOVE   10        TO   RECE62-KYURATE
                           WHEN   "6"
                               MOVE   20        TO   RECE62-KYURATE
                           WHEN   "8"
                               MOVE   20        TO   RECE62-KYURATE
                           WHEN   "9"
                               MOVE   10        TO   RECE62-KYURATE
                       END-EVALUATE
                   END-IF
               WHEN    "067"
               WHEN    "069"
                   IF       WRK-NENREI           <     3
                       MOVE  20      TO    RECE62-KYURATE
                   ELSE
                       IF      RECE62-HOJOKBN        NOT =  SPACE
                           EVALUATE   RECE62-HOJOKBN
                               WHEN   "8"
                                   MOVE   20        TO   RECE62-KYURATE
                               WHEN   "9"
                                   MOVE   10        TO   RECE62-KYURATE
                           END-EVALUATE
                       ELSE
                           IF   RECE62-HKNJANUM = "67330753"
                             MOVE   20         TO  RECE62-KYURATE
                           ELSE
                             IF      RECE62-HONKZKKBN      =    "1"
                               MOVE   WRK-HKN-HON-NYUKFTNRATE
                                                   TO   RECE62-KYURATE
                             ELSE
                               MOVE   WRK-HKN-KZK-NYUKFTNRATE
                                                   TO   RECE62-KYURATE
                             END-IF
                           END-IF
                       END-IF
                   END-IF
               WHEN    SPACE
                   MOVE    ZERO            TO  RECE62-KYURATE
               WHEN    OTHER
                   IF   WRK-NENREI           <     3
                       MOVE  20      TO    RECE62-KYURATE
                   ELSE
                       IF  RECE62-HOJOKBN      =     SPACE
                           IF      RECE62-HONKZKKBN    =     "1"
                               MOVE    WRK-HKN-HON-NYUKFTNRATE
                                                  TO  RECE62-KYURATE
                           ELSE
                               MOVE    WRK-HKN-KZK-NYUKFTNRATE
                                                  TO  RECE62-KYURATE
                           END-IF
                       ELSE
                           EVALUATE   RECE62-HOJOKBN
                               WHEN   "8"
                                   MOVE   20   TO  RECE62-KYURATE
                               WHEN   "9"
                                   MOVE   10   TO  RECE62-KYURATE
                           END-EVALUATE
                       END-IF
                   END-IF
             END-EVALUATE
      *
             IF      RJN-RATE-KBN      NOT =    SPACE
               EVALUATE   RJN-RATE-KBN
                   WHEN   "1"
                       MOVE   10        TO  RECE62-KYURATE
                   WHEN   "2"
                       MOVE   20        TO  RECE62-KYURATE
               END-EVALUATE
             END-IF
           END-IF
      *
      *平成１８年１０月改正対応
           IF    ( RECE62-SRYYM    >   WRK-CONS-SRYYM-200609 )
           AND   ( RECE62-SRYYM    <   WRK-CONS-SRYYM-200804 ) 
             EVALUATE    RECE62-HKNNUM       
               WHEN    "060"
               WHEN    "068"
                   IF      WRK-NENREI           <     3
                       IF      RECE62-HOJOKBN  =    "0"  OR   "1" OR
                                                    "4"  OR   "5"
                           EVALUATE   RECE62-HOJOKBN
                               WHEN   "0"
                                   MOVE   ZERO    TO   RECE62-KYURATE
                               WHEN   "1"
                                   MOVE   10      TO   RECE62-KYURATE
                               WHEN   "4"
                                   MOVE   ZERO    TO   RECE62-KYURATE
                               WHEN   "5"
                                   MOVE   10      TO   RECE62-KYURATE
                           END-EVALUATE
                       ELSE
                           MOVE  20      TO    RECE62-KYURATE
                       END-IF
                   ELSE
                       EVALUATE   RECE62-HOJOKBN
                           WHEN   "0"
                               MOVE   ZERO      TO   RECE62-KYURATE
                           WHEN   "1"
                               MOVE   10        TO   RECE62-KYURATE
                           WHEN   "2"
                               MOVE   20        TO   RECE62-KYURATE
                           WHEN   "3"
                               MOVE   30        TO   RECE62-KYURATE
                           WHEN   "4"
                               MOVE   ZERO      TO   RECE62-KYURATE
                           WHEN   "5"
                               MOVE   10        TO   RECE62-KYURATE
                           WHEN   "6"
                               MOVE   20        TO   RECE62-KYURATE
                           WHEN   "8"
                               MOVE   20        TO   RECE62-KYURATE
                           WHEN   "9"
                               MOVE   10        TO   RECE62-KYURATE
                       END-EVALUATE
                   END-IF
               WHEN    "067"
               WHEN    "069"
                   IF       WRK-NENREI           <     3
                       MOVE  20      TO    RECE62-KYURATE
                   ELSE
                       IF      RECE62-HOJOKBN        NOT =  SPACE
                           EVALUATE   RECE62-HOJOKBN
                               WHEN   "7"
                                   MOVE   30        TO   RECE62-KYURATE
                               WHEN   "8"
                                   MOVE   20        TO   RECE62-KYURATE
                               WHEN   "9"
                                   MOVE   10        TO   RECE62-KYURATE
                           END-EVALUATE
                       ELSE
                           IF   RECE62-HKNJANUM = "67330753"
                             MOVE   20         TO  RECE62-KYURATE
                           ELSE
                             IF      RECE62-HONKZKKBN      =    "1"
                               MOVE   WRK-HKN-HON-NYUKFTNRATE
                                                   TO   RECE62-KYURATE
                             ELSE
                               MOVE   WRK-HKN-KZK-NYUKFTNRATE
                                                   TO   RECE62-KYURATE
                             END-IF
                           END-IF
                       END-IF
                   END-IF
               WHEN    SPACE
                   MOVE    ZERO            TO  RECE62-KYURATE
               WHEN    OTHER
                   IF   WRK-NENREI           <     3
                       MOVE  20      TO    RECE62-KYURATE
                   ELSE
                       IF  RECE62-HOJOKBN      =     SPACE
                           IF      RECE62-HONKZKKBN    =     "1"
                               MOVE    WRK-HKN-HON-NYUKFTNRATE
                                                  TO  RECE62-KYURATE
                           ELSE
                               MOVE    WRK-HKN-KZK-NYUKFTNRATE
                                                  TO  RECE62-KYURATE
                           END-IF
                       ELSE
                           EVALUATE   RECE62-HOJOKBN
                               WHEN   "7"
                                   MOVE   30   TO  RECE62-KYURATE
                               WHEN   "9"
                                   MOVE   10   TO  RECE62-KYURATE
                           END-EVALUATE
                       END-IF
                   END-IF
             END-EVALUATE
      *
             IF      RJN-RATE-KBN      NOT =    SPACE
               EVALUATE   RJN-RATE-KBN
                   WHEN   "1"
                       MOVE   10        TO  RECE62-KYURATE
                   WHEN   "3"
                       MOVE   30        TO  RECE62-KYURATE
               END-EVALUATE
             END-IF
           END-IF
      *
      *平成２０年４月改正対応
           IF    ( RECE62-SRYYM    >=  WRK-CONS-SRYYM-200804 ) 
             EVALUATE    RECE62-HKNNUM       
               WHEN    "060"
               WHEN    "068"
                   IF       RECE62-SYUGAKUKBN    =   1
                       IF      RECE62-HOJOKBN  =    "0"  OR   "1" OR
                                                    "4"  OR   "5"
                           EVALUATE   RECE62-HOJOKBN
                               WHEN   "0"
                                   MOVE   ZERO    TO   RECE62-KYURATE
                               WHEN   "1"
                                   MOVE   10      TO   RECE62-KYURATE
                               WHEN   "4"
                                   MOVE   ZERO    TO   RECE62-KYURATE
                               WHEN   "5"
                                   MOVE   10      TO   RECE62-KYURATE
                           END-EVALUATE
                       ELSE
                           MOVE  20      TO    RECE62-KYURATE
                       END-IF
                   ELSE
                       EVALUATE   RECE62-HOJOKBN
                           WHEN   "0"
                               MOVE   ZERO      TO   RECE62-KYURATE
                           WHEN   "1"
                               MOVE   10        TO   RECE62-KYURATE
                           WHEN   "2"
                               MOVE   20        TO   RECE62-KYURATE
                           WHEN   "3"
                               MOVE   30        TO   RECE62-KYURATE
                           WHEN   "4"
                               MOVE   ZERO      TO   RECE62-KYURATE
                           WHEN   "5"
                               MOVE   10        TO   RECE62-KYURATE
                           WHEN   "6"
                               MOVE   20        TO   RECE62-KYURATE
                           WHEN   "8"
                               MOVE   20        TO   RECE62-KYURATE
                           WHEN   "9"
                               MOVE   10        TO   RECE62-KYURATE
                       END-EVALUATE
                   END-IF
               WHEN    "067"
               WHEN    "069"
                   IF       RECE62-SYUGAKUKBN    =   1
                       MOVE  20      TO    RECE62-KYURATE
                   ELSE
                       IF      RECE62-HOJOKBN        NOT =  SPACE
                           EVALUATE   RECE62-HOJOKBN
                               WHEN   "7"
                                   MOVE   30        TO   RECE62-KYURATE
                               WHEN   "8"
                                   MOVE   20        TO   RECE62-KYURATE
                               WHEN   "9"
                                   MOVE   10        TO   RECE62-KYURATE
                           END-EVALUATE
                       ELSE
                           IF  (RECE62-HKNJANUM  = "67330753")
                           AND (RECE62-SRYYM    <= "201109"  )
                             MOVE   20         TO  RECE62-KYURATE
                           ELSE
                             IF      RECE62-HONKZKKBN      =    "1"
                               MOVE   WRK-HKN-HON-NYUKFTNRATE
                                                   TO   RECE62-KYURATE
                             ELSE
                               MOVE   WRK-HKN-KZK-NYUKFTNRATE
                                                   TO   RECE62-KYURATE
                             END-IF
                           END-IF
                       END-IF
                   END-IF
               WHEN    SPACE
                   MOVE    ZERO            TO  RECE62-KYURATE
               WHEN    "039"
               WHEN    "040"
                   EVALUATE   RECE62-HOJOKBN
                       WHEN   "1"
                           MOVE    10  TO  RECE62-KYURATE
                       WHEN   "3"
                           MOVE    30  TO  RECE62-KYURATE
                   END-EVALUATE
               WHEN    OTHER
                   IF       RECE62-SYUGAKUKBN    =   1
                       MOVE  20      TO    RECE62-KYURATE
                   ELSE
                       IF  RECE62-HOJOKBN      =     SPACE
                           IF      RECE62-HONKZKKBN    =     "1"
                               MOVE    WRK-HKN-HON-NYUKFTNRATE
                                                  TO  RECE62-KYURATE
                           ELSE
                               MOVE    WRK-HKN-KZK-NYUKFTNRATE
                                                  TO  RECE62-KYURATE
                           END-IF
                       ELSE
                           EVALUATE   RECE62-HOJOKBN
                               WHEN   "7"
                                   MOVE   30   TO  RECE62-KYURATE
                               WHEN   "9"
                                   MOVE   10   TO  RECE62-KYURATE
                               WHEN   "8"
                                   MOVE   20   TO  RECE62-KYURATE
                           END-EVALUATE
                       END-IF
                   END-IF
             END-EVALUATE
           END-IF
      *
      *    船員保険（被保険者）＋通勤災害フラグセット
           IF    RECE62-HKNNUM           =    "002"
               EVALUATE   RECE62-HOJOKBN
                    WHEN   "3"
                    WHEN   "C"
                    WHEN   "F"
                    WHEN   "I"
                        MOVE     "1"     TO     RECE62-SENIN-TUUSAI
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
           IF      RECE61-KOHNUMHC (IDX 1) =   "027"
               MOVE    2                   TO  IDY1
           ELSE
               MOVE    1                   TO  IDY1
           END-IF
           MOVE    ZERO            TO  IDX2
           PERFORM VARYING IDY2    FROM    IDY1    BY  1
                   UNTIL   IDY2    >       4
               ADD     1                   TO  IDX2  
               MOVE    RECE61-KOHNUMHC (IDX IDY2)
                                           TO  WRK-HKNNUM
               PERFORM KOHNUM-CHECK-SEC
               IF      FLG-CHK         =   1
                   MOVE RECE61-KOHNUMHC (IDX IDY2) TO
                                           RECE62-KOHNUMHC (IDX IDX2)
                   MOVE RECE61-KOHIDHC  (IDX IDY2) TO
                                           RECE62-KOHIDHC  (IDX IDX2)
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
                       OR      RECE61-KOHHKNNUM-O (IDY)  =   SPACE
                       OR      FLG-CHK     =  1
                   IF      WRK-HKNNUM      =   RECE61-KOHHKNNUM-O (IDY)
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
      *    データ出力処理
      *****************************************************************
       220-DATA-WRITE-SEC                SECTION.
      *
           WRITE   RECE62-REC
      *
           IF      STS-RECE62          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE62PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE62          TO  WRK-ERR-FILE-STS
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
       230-RECE69-HENSYU-SEC                   SECTION.
      *
           MOVE    ZERO                TO  FLG-RIREKI-HEN
      *
           IF      RECE61-TEISEIKBN             =   SPACE
             IF  (     ( RECE61-HKNID           NOT =   ZERO  )
                   AND ( RECE61-HKNNUM(1:2)     NOT =   "98" AND
                                                        "90" AND
                                                        "91"  ) )
             OR  (     ( RECE61-HKNID               =   ZERO  )
                   AND ( RECE61-KOHHKNNUM-O (1)     =   "012" 
                                                    OR  "025"
                                                    OR  "015"
                                                    OR  "016"
                                                    OR  "021"
                                                    OR  "053" )
                   AND ( RECE61-KOHHKNNUM-O (2)     =   SPACE ) )
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       10
                       OR      RECE61-TEKSTYMDX (IDX)  =   SPACE
                 MOVE    SPACE                TO  RECE69-REC
                 INITIALIZE                       RECE69-REC
                 MOVE    RECE61-HOSPNUM       TO  RECE69-HOSPNUM
                 MOVE    RECE61-SRYYM         TO  RECE69-SRYYM
                 MOVE    RECE61-NYUGAIKBN     TO  RECE69-NYUGAIKBN
                 MOVE    RECE61-PTID          TO  RECE69-PTID
                 MOVE    RECE61-RECEKA        TO  RECE69-RECEKA
                 MOVE    "000"                TO  RECE69-KOHNUM
                 IF    ( RECE61-HKNID             =   ZERO  )
                 AND   ( RECE61-KOHHKNNUM-O (1)   =   "012" ) 
                 AND   ( RECE61-KOHHKNNUM-O (2)   =   SPACE )
                     MOVE    RECE61-KOH-KOHID     TO  RECE69-KOHID
                 END-IF
                 MOVE    RECE61-TEKSTYMDX (IDX)
                                              TO  RECE69-TEKSTYMD
                 PERFORM VARYING IDYY    FROM    1   BY  1
                         UNTIL   IDYY    >       100
      *            公費単独と主保険の適用年月日が重複するとき
grpsys             IF    ( TBL-RECE69-HOSPNUM (IDYY)
                                             >   ZERO  )
                   AND   ( RECE61-HKNNUM NOT =   SPACE )
                   AND   ( TBL-RECE69-HKNNUM  (IDYY)
                                             =   SPACE )
grpsys                 IF      RECE61-TEKSTYMDX (IDX)
                                         >   TBL-RECE69-TEKEDYMDX (IDYY)
                       OR      RECE61-TEKEDYMDX (IDX)
                                         <   TBL-RECE69-TEKSTYMDX (IDYY)
                           CONTINUE
                       ELSE
                           INITIALIZE            TBL-RECE69-OCC   (IDYY)
                       END-IF
                   END-IF
      *
grpsys             IF      TBL-RECE69-HOSPNUM (IDYY)  =   ZERO
                     MOVE    RECE61-TEKSTYMDX (IDX)
                                                TO  RECE69-TEKSTYMDX
                     MOVE    RECE61-TEKEDYMDX (IDX)
                                                TO  RECE69-TEKEDYMDX
      * 
                     IF      RECE61-HKNNUM       =   "060"   OR  "067"
                                                 OR  "068"   OR  "069"
                       MOVE    2               TO  RECE69-TEISYUTUSAKI
                     ELSE
                       IF      RECE61-HKNNUM   =   "040"   OR  "039"
                           MOVE    6           TO  RECE69-TEISYUTUSAKI
                       ELSE
                           MOVE    1           TO  RECE69-TEISYUTUSAKI
                       END-IF
                     END-IF
      *                                          
                     IF      RECE61-HKNNUM   NOT =   SPACE
                         INITIALIZE                       PTHKNINF-REC
                         MOVE    RECE61-HOSPNUM       TO  PTHKN-HOSPNUM
                         MOVE    RECE61-PTID          TO  PTHKN-PTID
                         MOVE    RECE61-HKNNUM        TO  PTHKN-HKNNUM
                         MOVE    RECE61-TEKSTYMDX(IDX)
                                                      TO  PTHKN-TEKSTYMD
                         MOVE    RECE61-TEKEDYMDX(IDX)
                                                      TO  PTHKN-TEKEDYMD
                         MOVE    PTHKNINF-REC         TO  MCPDATA-REC
                         MOVE    "tbl_pthkninf"       TO  MCP-TABLE
                         MOVE    "key4"               TO  MCP-PATHNAME
                         PERFORM   900-DBSELECT-SEC
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
                                 IF    ( RECE61-AGE  >=  75    )
                                 OR    ( RECE61-AGE  =   74          AND
                                         RECE61-BIRTHDAY (5:2)
                                                     =
                                                   RECE61-SRYYM(5:2) AND 
                                         RECE61-BIRTHDAY (7:2)
                                                 NOT =   "01"  ) 
                                     MOVE    1   TO  FLG-RIREKI-HEN
                                 END-IF  
                             END-IF
      *
                             PERFORM VARYING IDZZ    FROM    1   BY  1
                                     UNTIL   IDZZ    >       100
                                     OR      FLG-RIREKI-HEN   =   1
      *                          主保険が同じ保険があるときは対象外とする
                                 IF    ( TBL-RECE69-HOSPNUM  (IDZZ)
                                                     >   ZERO          )
                                 AND   ( TBL-RECE69-HKNNUM   (IDZZ)
                                                     =   PTHKN-HKNNUM  )
                                 AND   ( TBL-RECE69-HKNJANUM (IDZZ)
                                                     =   PTHKN-HKNJANUM)
                                 AND   ( TBL-RECE69-HONKZKKBN(IDZZ)
                                                     =
                                                     PTHKN-HONKZKKBN   )
                                 AND   ( TBL-RECE69-HOJOKBN  (IDZZ)
                                                     =   PTHKN-HOJOKBN )
                                 AND   ( TBL-RECE69-KIGO     (IDZZ)
                                                     =   PTHKN-KIGO    )
                                 AND   ( TBL-RECE69-NUM      (IDZZ)
                                                     =   PTHKN-NUM     )
                                     MOVE    1   TO  FLG-RIREKI-HEN
                                 END-IF
                             END-PERFORM
      *
                             IF      FLG-RIREKI-HEN  =   ZERO
                               MOVE   PTHKN-HKNID    TO  RECE69-HKNID
                               MOVE   PTHKN-HKNNUM   TO  RECE69-HKNNUM
                               MOVE   PTHKN-HKNJANUM TO  RECE69-HKNJANUM
                               MOVE   PTHKN-HONKZKKBN
                                                     TO RECE69-HONKZKKBN
                               MOVE   PTHKN-HOJOKBN  TO  RECE69-HOJOKBN
                               MOVE   PTHKN-CONTKBN  TO  RECE69-CONTKBN
                               IF     PTHKN-HKNNUM   NOT =   "039"     
                                 MOVE   PTHKN-KIGO       TO  RECE69-KIGO
                               END-IF
                               MOVE   PTHKN-NUM      TO  RECE69-NUM
                               MOVE   SPACE          TO  RECE69-CERTNUM
      *         
                               INITIALIZE                HKNNUM-REC
                               MOVE   RECE61-HOSPNUM TO  HKN-HOSPNUM
                               MOVE   PTHKN-HKNNUM   TO  HKN-HKNNUM
                               MOVE   RECE61-TEKSTYMDX (IDX)
                                                     TO  HKN-TEKSTYMD
                               MOVE   RECE61-TEKEDYMDX (IDX)
                                                     TO  HKN-TEKEDYMD
                               MOVE   HKNNUM-REC     TO  MCPDATA-REC
                               MOVE   "tbl_hknnum"   TO  MCP-TABLE
                               MOVE   "key5"         TO  MCP-PATHNAME
                               PERFORM   900-DBSELECT-SEC
                               IF     MCP-RC         =   ZERO
                                 MOVE    "tbl_hknnum"    TO MCP-TABLE
                                 MOVE    "key5"          TO MCP-PATHNAME
                                 PERFORM HKNNUM-READ-SEC
                                 IF      FLG-HKNNUM      =   ZERO
                                     MOVE   HKN-TANSEIDONAME
                                                 TO  RECE69-TANSEIDONAME
                                     MOVE   HKN-HBTNUM
                                                 TO RECE69-HKNHBTNUM
                                 END-IF
                               END-IF
                               MOVE    "tbl_hknnum"  TO  MCP-TABLE
                               MOVE    "key5"        TO  MCP-PATHNAME
                               PERFORM 900-CLOSE-SEC
                               MOVE    1             TO  FLG-PTHKNINF
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
                           MOVE    RECE69-REC        TO
                                                 TBL-RECE69-OCC (IDYY)
                         END-IF
                         MOVE    100               TO  IDYY
                     ELSE                                            
                         INITIALIZE                      PTKOHINF-REC
                         MOVE    RECE61-HOSPNUM      TO  PTKOH-HOSPNUM
                         MOVE    RECE61-PTID         TO  PTKOH-PTID
                         MOVE    RECE61-KOHID-O (1)  TO  PTKOH-KOHID
                         MOVE    PTKOHINF-REC        TO  MCPDATA-REC
                         MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                         MOVE    "key"               TO  MCP-PATHNAME
                         PERFORM 900-DBSELECT-SEC
                         IF      MCP-RC              =   ZERO
                             MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                             MOVE    "key"           TO  MCP-PATHNAME
                             PERFORM  PTKOHINF-READ-SEC
                         ELSE
                             MOVE    1                TO  FLG-PTKOHINF  
                         END-IF
                         MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                         MOVE    "key"               TO  MCP-PATHNAME
                         PERFORM 900-CLOSE-SEC
      *
                         IF      FLG-PTKOHINF    =   ZERO
                           MOVE    PTKOH-FTNJANUM  TO  RECE69-KIGO
      *
                           IF      PTKOH-KOHNUM    =   "012"   OR  "025"
                             MOVE    RECE61-HOSPNUM  TO
                                                   MONTHLYNUM-HOSPNUM
                             MOVE    RECE61-PTID     TO MONTHLYNUM-PTID
                             MOVE    PTKOH-KOHID     TO MONTHLYNUM-KOHID
                             MOVE    RECE61-SRYYM    TO MONTHLYNUM-SRYYM
                             MOVE    RECE61-NYUGAIKBN
                                                     TO
                                                   MONTHLYNUM-NYUGAIKBN
                             MOVE    MONTHLYNUM-REC  TO MCPDATA-REC
                             MOVE    "tbl_monthlynum"
                                                     TO WRK-MCP-TABLE
                             MOVE    "key4"          TO WRK-MCP-PATHNAME
                             PERFORM 910-MONTHLYNUM-READ-SEC
                             IF      FLG-MONTHLYNUM  =   ZERO
                                 MOVE    MONTHLYNUM-JKYSNUM 
                                                     TO  RECE69-NUM
                             ELSE                       
                                 MOVE    RECE61-HOSPNUM
                                                     TO
                                                   MONTHLYNUM-HOSPNUM
                                 MOVE    RECE61-PTID TO MONTHLYNUM-PTID
                                 MOVE    ZERO        TO MONTHLYNUM-KOHID
                                 MOVE    RECE61-SRYYM
                                                     TO MONTHLYNUM-SRYYM
                                 MOVE    RECE61-NYUGAIKBN
                                                     TO
                                                   MONTHLYNUM-NYUGAIKBN
                                 MOVE    MONTHLYNUM-REC
                                                     TO MCPDATA-REC
                                 MOVE    "tbl_monthlynum"
                                                     TO WRK-MCP-TABLE
                                 MOVE    "key4"      TO WRK-MCP-PATHNAME
                                 PERFORM 910-MONTHLYNUM-READ-SEC
                                 IF      FLG-MONTHLYNUM  =   ZERO
                                     MOVE    MONTHLYNUM-JKYSNUM 
                                                         TO  RECE69-NUM
                                 ELSE                       
                                     MOVE   PTKOH-JKYSNUM
                                                         TO  RECE69-NUM
                                 END-IF
                             END-IF    
                           ELSE                       
                             MOVE   PTKOH-JKYSNUM  TO  RECE69-NUM
                           END-IF
      *         
                           INITIALIZE                  HKNNUM-REC
                           MOVE   RECE61-HOSPNUM   TO  HKN-HOSPNUM
                           MOVE   PTKOH-KOHNUM     TO  HKN-HKNNUM
                           MOVE   RECE61-TEKSTYMDX (IDX)
                                                   TO  HKN-TEKSTYMD
                           MOVE   RECE61-TEKEDYMDX (IDX)
                                                   TO  HKN-TEKEDYMD
                           MOVE   HKNNUM-REC       TO  MCPDATA-REC
                           MOVE   "tbl_hknnum"     TO  MCP-TABLE
                           MOVE   "key5"           TO  MCP-PATHNAME
                           PERFORM 900-DBSELECT-SEC
                           IF     MCP-RC           =   ZERO
                               MOVE   "tbl_hknnum" TO  MCP-TABLE
                               MOVE   "key5"       TO  MCP-PATHNAME
                               PERFORM HKNNUM-READ-SEC
                               IF      FLG-HKNNUM  =   ZERO
                                   MOVE   HKN-TANSEIDONAME
                                                 TO  RECE69-TANSEIDONAME
                               END-IF
                           END-IF
                           MOVE   "tbl_hknnum"     TO  MCP-TABLE
                           MOVE   "key5"           TO  MCP-PATHNAME
                           PERFORM 900-CLOSE-SEC
      *
                           MOVE    RECE69-REC        TO
                                                 TBL-RECE69-OCC (IDYY)
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
       230-RECE69-HENSYU-EXT.
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
           IF      RECE61-SRYYM        <   WRK-CONS-SRYYM-200804
               GO  TO   230-PTTOKKI-HENSYU-EXT
           END-IF
      * 
           INITIALIZE                      PTTOKKI-REC
           MOVE    RECE61-HOSPNUM      TO  PTTOKKI-HOSPNUM
           MOVE    RECE61-PTID         TO  PTTOKKI-PTID
           MOVE    "2"                 TO  PTTOKKI-NYUGAIKBN
           MOVE    RECE61-SRYYM        TO  PTTOKKI-STYM
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
           INITIALIZE                      WRK-BYOTOKBN-TABLE
                                           WRK-SHONUM
           MOVE      SPACE             TO  WK-PTNYUINRRK-TENSTUYMD2
           MOVE      SPACE             TO  WK-PTNYUINRRK-TENNYUYMD
           MOVE      SPACE             TO  WRK-TAIINYMD
      *
           MOVE      ZERO              TO  FLG-BUNBEN-SET
      *
      *    診療月内の入退院情報
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    RECE62-HOSPNUM      TO  PTNYUINRRK-HOSPNUM
           MOVE    RECE62-PTID         TO  PTNYUINRRK-PTID
           MOVE    RECE62-SRYYM        TO  PTNYUINRRK-NYUINYMD (1:6)
           MOVE    "31"                TO  PTNYUINRRK-NYUINYMD (7:2)
           MOVE    RECE62-SRYYM        TO  PTNYUINRRK-TAIINYMD (1:6)
           MOVE    "01"                TO  PTNYUINRRK-TAIINYMD (7:2)
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-PTNYUINRRK-KEY23-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTNYUINRRK23
           END-IF
           PERFORM                 UNTIL   FLG-PTNYUINRRK23    =   1
             IF  (   PTNYUINRRK-TENNYUYMD (1:6)
                                         >    RECE62-SRYYM         )
             OR  ( ( PTNYUINRRK-TENNYUYMD  >   RECE62-TEKEDYMD-X )
                  AND
                   ( RECE62-TEKEDYMD-X NOT =   SPACE             ) )
                 CONTINUE
             ELSE
      *
               IF   (FLG-BUNBEN-SET  =   ZERO   )
               AND  (RECE62-SRYYM   >=  "200910")
                 IF ( RECE62-HKNNUM(1:2)   =  "98"  OR "90"
                                                    OR "91"  )
                 OR ( RECE62-HKNNUM        =  "039" OR "040" )
                 OR ((RECE62-HKNNUM        =  "002") AND
                     (RECE62-HOJOKBN   NOT =  SPACE)         )
                   CONTINUE
                 ELSE
                   MOVE     RECE62-SRYYM           TO  WRK-BUNBENST-YM
                   IF   RECE62-TEKSTYMD-X(1:6)  >=  RECE62-SRYYM
                      MOVE  RECE62-TEKSTYMD-X(7:2) TO  WRK-BUNBENST-DD
                   ELSE
                      MOVE  01                     TO  WRK-BUNBENST-DD
                   END-IF
                   MOVE     RECE62-SRYYM           TO  WRK-BUNBENED-YM
                   IF   RECE62-TEKEDYMD-X(1:6)  <=  RECE62-SRYYM
                      MOVE  RECE62-TEKEDYMD-X(7:2) TO  WRK-BUNBENED-DD
                   ELSE
                      MOVE  31                     TO  WRK-BUNBENED-DD
                   END-IF
                   IF  ( PTNYUINRRK-NYUINYMD <= WRK-BUNBENED-YMD )
                   AND ((PTNYUINRRK-NYUINYMD >= WRK-BUNBENST-YMD) OR
                        (PTNYUINRRK-TAIINYMD >= WRK-BUNBENST-YMD))
                      PERFORM   2402-BUNBEN-CHECK-SEC
                   END-IF
                 END-IF
               END-IF
      *
               IF      PTNYUINRRK-NYUINYMD (1:6)
                                               <   RECE62-SRYYM
                   MOVE    1                   TO  IDX4
               ELSE
                   MOVE    PTNYUINRRK-NYUINYMD TO  WRK-SRYYMD
                   MOVE    WRK-SRYDD           TO  IDX4
               END-IF 
               IF      PTNYUINRRK-TAIINYMD (1:6)
                                               >   RECE62-SRYYM
                   MOVE    31                  TO  IDX5
               ELSE
                   MOVE    PTNYUINRRK-TAIINYMD TO  WRK-SRYYMD
                   MOVE    WRK-SRYDD           TO  IDX5
               END-IF
      *        保険組合せのチェック
      *       （会計に対象の組合せがないとき又はＥＦファイル作成時は全て）
               IF    ( RECE61-DAY-AREA     =   ALL "0" )
               OR    ( WRK-PARA-PRTKBN     =   "8"     )
                   MOVE    "1"   TO   RECE62-NYUINRYOFLG
                   PERFORM 2401-PTNYUINRRK-HENSYU-SEC
               ELSE
                   PERFORM VARYING IDX FROM    IDX4    BY  1
                           UNTIL   IDX >       IDX5
                       IF    ( IDX             =   IDX4 )
                       AND   ( RECE61-DOUJITSU-HKNCOMBI (IDX)
                                               =
                                               PTNYUINRRK-HKNCOMBINUM )
      *                    同日再入院のとき再入院分の保険組合せをチェック
                           PERFORM VARYING IDY FROM    1   BY  1
                                   UNTIL   IDY >       10
                                   OR  RECE62-HKNCOMBI (IDY)
                                                       =   ZERO
                               IF     RECE61-DOUJITSU-HKNCOMBI (IDX)
                                               =   RECE62-HKNCOMBI (IDY)
                                   PERFORM 2401-PTNYUINRRK-HENSYU-SEC
                                   MOVE    10              TO  IDY
                                   MOVE   IDX5             TO  IDX
                               END-IF
                           END-PERFORM
                       ELSE
                           PERFORM VARYING IDY FROM    1   BY  1
                                   UNTIL   IDY >       10
                                   OR  RECE62-HKNCOMBI (IDY)   =   ZERO
                               IF      RECE61-DAY (IDX)
                                               =   RECE62-HKNCOMBI (IDY)
                                   PERFORM 2401-PTNYUINRRK-HENSYU-SEC
                                   MOVE    10                  TO  IDY
                                   MOVE   IDX5                 TO  IDX
                               END-IF
      *                        入院基本料を算定しない入退院歴の記載
                               IF     SYS-25-NYUTAIINREKI-NYU-KSIKBN
                                               =   "1"
                                 IF      RECE61-DAY-1 (IDX)
                                               =   RECE62-HKNCOMBI (IDY)
                                   PERFORM 2401-PTNYUINRRK-HENSYU-SEC
                                   MOVE    10                  TO  IDY
                                   MOVE   IDX5                 TO  IDX
                                 END-IF
                               END-IF
                           END-PERFORM
                       END-IF
                   END-PERFORM
      *
      ******       自費分の入退院歴の記載
      ******       IF      SYS-25-NYUTAIINREKI-NYU-KSIKBN
      ******                             =   "1"
      ******           PERFORM VARYING IDX FROM    IDX4    BY  1
      ******                   UNTIL   IDX >       IDX5
      ******               IF      RECE61-DAY (IDX)
      ******                                       >   ZERO
      *                        保険組合せ読込み
      ******                   INITIALIZE              HKNCOMBI-REC
      ******                   MOVE    RECE62-HOSPNUM  TO  COMB-HOSPNUM
      ******                   MOVE    RECE62-PTID     TO  COMB-PTID
      ******                   MOVE    RECE61-DAY(IDX) TO
      ******                                           COMB-HKNCOMBINUM 
      ******                   MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
      ******                   PERFORM  900-HKNCOMBI-INV-SEC
      ******                   IF      COMB-HKNNUM (1:2)   =   "98"
      ******                       PERFORM 2401-PTNYUINRRK-HENSYU-SEC
      ******                       MOVE   IDX5                 TO  IDX
      ******                   END-IF
      ******               END-IF
      ******           END-PERFORM
      ******       END-IF
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
      *    診療科情報が未編集のとき
           IF      RECE62-SRYKA (1)    =   SPACE
               MOVE    RECE62-LASTSRYKA    TO  RECE62-SRYKA (1)
           END-IF
      *                                                                
      *    病棟区分並替え
      *                                                                
           PERFORM    VARYING   IDX3     FROM    1   BY  1             
                  UNTIL  ( IDX3                   >   4    )           
                    OR   ( WRK-BYOTOKBN(IDX3)     =  SPACE )           
             COMPUTE    IDX4  =  IDX3  +  1                            
             PERFORM  VARYING  IDX5  FROM  IDX4  BY  1                 
                        UNTIL  ( IDX5                >   4    )        
                          OR   ( WRK-BYOTOKBN(IDX5)  =  SPACE )        
                IF       WRK-BYOTOKBN(IDX3)  >                         
                                           WRK-BYOTOKBN(IDX5)          
                   MOVE  WRK-BYOTOKBN(IDX3) TO  WRK-BYOTOKBN-DATA      
                   MOVE  WRK-BYOTOKBN(IDX5) TO  WRK-BYOTOKBN(IDX3)     
                   MOVE  WRK-BYOTOKBN-DATA  TO  WRK-BYOTOKBN(IDX5)     
                END-IF                                                 
             END-PERFORM                                               
           END-PERFORM                                                 
      *
           MOVE    ZERO                  TO  IDX4                                                                
           PERFORM    VARYING   IDX3     FROM    1   BY  1             
                      UNTIL   IDX3    >     4
                IF      RECE62-SRYYM     <=  WRK-CONS-SRYYM-200609
                    MOVE    WRK-BYOTOKBN(IDX3)
                                         TO  RECE62-BYOTOKBN(IDX3)
                ELSE  
                    EVALUATE    WRK-BYOTOKBN(IDX3)
                        WHEN    "01"                                            
                        WHEN    "02"                                            
                        WHEN    "07"
                            ADD     1        TO  IDX4                                           
                            MOVE    WRK-BYOTOKBN(IDX3)
                                             TO  RECE62-BYOTOKBN (IDX4)  
                    END-EVALUATE
                END-IF                                                
           END-PERFORM                                                 
      *
           MOVE    WRK-TAIINYMD    TO  RECE62-TAIINYMD                                                                
           .
       240-PTNYUINRRK-SYORI-EXT.
           EXIT.
      * 
      *****************************************************************
      *    入退院履歴情報編集処理
      *****************************************************************
       2401-PTNYUINRRK-HENSYU-SEC           SECTION.
      *
      *    病棟区分セット
           PERFORM    24011-BYOTOKBN-SET-SEC
      *
      *    診療科情報のセット
      *    退院日＜診療年月、転出日の前日＜診療年月のときはセットしない
           IF    ( PTNYUINRRK-TENSTUYMD    =  PTNYUINRRK-TAIINYMD )
           AND   ( PTNYUINRRK-TENNYUYMD    =  PTNYUINRRK-NYUINYMD )
               IF      PTNYUINRRK-TAIINYMD (1:6)
                                           >=  RECE62-SRYYM
                   PERFORM VARYING IDW FROM    1   BY  1
                           UNTIL   IDW >       20
                           OR      PTNYUINRRK-NYUINKA
                                           =   RECE62-SRYKA (IDW)
                       IF      RECE62-SRYKA (IDW)  =   SPACE  
                           PERFORM VARYING IDXX    FROM    1   BY  1
                                   UNTIL   IDXX    >       10
                                   OR      RECE62-HKNCOMBI (IDXX)
                                                   =   ZERO
                               IF      RECE62-HKNCOMBI (IDXX)  =
                                               PTNYUINRRK-HKNCOMBINUM
                                   MOVE    PTNYUINRRK-NYUINKA  
                                               TO  RECE62-SRYKA (IDW)
                                   MOVE    10      TO  IDXX
                               END-IF
                           END-PERFORM
                           MOVE    20          TO  IDW
                       END-IF
                   END-PERFORM
               END-IF    
           ELSE
               IF      PTNYUINRRK-TENSTUYMD
                                   NOT =   PTNYUINRRK-TAIINYMD
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY6-AREA
                   MOVE    "61"            TO  LNK-DAY6-IRAI
                   MOVE   PTNYUINRRK-TENSTUYMD
                                           TO  LNK-DAY6-KIJUN
                   MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                   MOVE    -1              TO  LNK-DAY6-ZOGEN
                   CALL  "ORCSDAY"         USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
                   IF      LNK-DAY6-KEISAN (1:6)
                                       >=  RECE62-SRYYM
                     PERFORM VARYING IDW FROM    1   BY  1
                           UNTIL   IDW >       20
                           OR      PTNYUINRRK-NYUINKA
                                           =   RECE62-SRYKA (IDW)
                       IF      RECE62-SRYKA (IDW)  =   SPACE  
                           PERFORM VARYING IDXX    FROM    1   BY  1
                                   UNTIL   IDXX    >       10
                                   OR      RECE62-HKNCOMBI (IDXX)
                                                   =   ZERO
                               IF      RECE62-HKNCOMBI (IDXX)  =
                                               PTNYUINRRK-HKNCOMBINUM
                                   MOVE    PTNYUINRRK-NYUINKA  
                                               TO  RECE62-SRYKA (IDW)
                                   MOVE    10      TO  IDXX
                               END-IF
                           END-PERFORM
                           MOVE    20          TO  IDW
                       END-IF
                     END-PERFORM
                   END-IF
               END-IF
               IF      PTNYUINRRK-TENNYUYMD
                                   NOT =   PTNYUINRRK-NYUINYMD
                   IF      PTNYUINRRK-TENNYUYMD (1:6)
                                       <=  RECE62-SRYYM
                     PERFORM VARYING IDW FROM    1   BY  1
                           UNTIL   IDW >       20
                           OR      PTNYUINRRK-NYUINKA
                                           =   RECE62-SRYKA (IDW)
                       IF      RECE62-SRYKA (IDW)  =   SPACE  
                           PERFORM VARYING IDXX    FROM    1   BY  1
                                   UNTIL   IDXX    >       10
                                   OR      RECE62-HKNCOMBI (IDXX)
                                                   =   ZERO
                               IF      RECE62-HKNCOMBI (IDXX)  =
                                               PTNYUINRRK-HKNCOMBINUM
                                   MOVE    PTNYUINRRK-NYUINKA  
                                               TO  RECE62-SRYKA (IDW)
                                   MOVE    10      TO  IDXX
                               END-IF
                           END-PERFORM
                           MOVE    20          TO  IDW
                       END-IF
                     END-PERFORM
                   END-IF
               END-IF
           END-IF
      *
      *    入院年月日・最終科
           IF      RECE62-LASTSRYKA    =   SPACE
               MOVE    PTNYUINRRK-NYUINYMD TO  RECE62-NYUINYMD
               MOVE    PTNYUINRRK-NYUINKA  TO  RECE62-LASTSRYKA
               MOVE    PTNYUINRRK-BTUNUM   TO  RECE62-LASTBTUNUM
               MOVE    PTNYUINRRK-BRMNUM   TO  RECE62-LASTBRMNUM
               MOVE    PTNYUINRRK-SHONUM   TO  WRK-SHONUM
           END-IF
      *
      *    入退院履歴情報
           PERFORM 24012-RECE68-WRITE-SEC
      *
      *    診療月内の入退院情報の履歴
           IF      FLG-RECE68          =   1                            
               MOVE    PTNYUINRRK-REC  TO  WRK-PTNRRK-REC
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    RECE61-HOSPNUM  TO  PTNYUINRRK-HOSPNUM
               MOVE    RECE61-PTID     TO  PTNYUINRRK-PTID
               MOVE    WRK-PTNRRK-SHONUM
                                       TO  PTNYUINRRK-SHONUM
               MOVE    RECE61-SRYYM    TO  PTNYUINRRK-NYUINYMD (1:6)
               MOVE    "31"            TO  PTNYUINRRK-NYUINYMD (7:2)
               MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key51"             TO  MCP-PATHNAME
               PERFORM   900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   PERFORM 900-PTNYUINRRK-KEY22-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-PTNYUINRRK22 
               END-IF
      *
      *        入院年月日
               IF    ( PTNYUINRRK-NYUINYMD <    RECE62-NYUINYMD )
               AND   ( PTNYUINRRK-SHONUM   =    WRK-SHONUM      )
                   MOVE    PTNYUINRRK-NYUINYMD  TO  RECE62-NYUINYMD
      *
                   IF    ( PTNYUINRRK-JTIKBN        =   "5" )
                   AND   ( PTNYUINRRK-TAINRELKBN    =   "1" )
                       MOVE    "1"                  TO  RECE62-TAINFLG
                   END-IF
               END-IF
      *
               PERFORM             UNTIL   FLG-PTNYUINRRK22 =  1
                   PERFORM 24012-RECE68-WRITE-SEC
                   PERFORM 900-PTNYUINRRK-KEY22-READ-SEC
               END-PERFORM
      *                 
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key51"             TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
           .
       2401-PTNYUINRRK-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟区分セット                                              
      *****************************************************************
       24011-BYOTOKBN-SET-SEC                SECTION.
      *
      *    異動による転室等の場合は転出日を前日に置き換える
           MOVE    SPACE               TO  WK-PTNYUINRRK-TENSTUYMD
           IF      PTNYUINRRK-TENSTUYMD  =  PTNYUINRRK-TAIINYMD
              MOVE  PTNYUINRRK-TENSTUYMD  TO  WK-PTNYUINRRK-TENSTUYMD
      *
              IF  ( WK-PTNYUINRRK-TENSTUYMD2     NOT =  SPACE   )
              AND ( WK-PTNYUINRRK-TENSTUYMD(1:6) NOT = "999999" )
              AND ( WK-PTNYUINRRK-TENSTUYMD  =
                                        WK-PTNYUINRRK-TENSTUYMD2)
                 INITIALIZE                STS-AREA-DAY
                 INITIALIZE                LNK-DAY6-AREA
                 MOVE    "61"              TO  LNK-DAY6-IRAI
                 MOVE   PTNYUINRRK-TENSTUYMD
                                           TO  LNK-DAY6-KIJUN
                 MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                 MOVE    -1                TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                 MOVE    LNK-DAY6-KEISAN TO WK-PTNYUINRRK-TENSTUYMD
              END-IF
           ELSE
              INITIALIZE                STS-AREA-DAY
              INITIALIZE                LNK-DAY6-AREA
              MOVE    "61"              TO  LNK-DAY6-IRAI
              MOVE   PTNYUINRRK-TENSTUYMD
                                        TO  LNK-DAY6-KIJUN
              MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
              MOVE    -1                TO  LNK-DAY6-ZOGEN
              CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                LNK-DAY6-AREA
              MOVE    LNK-DAY6-KEISAN TO WK-PTNYUINRRK-TENSTUYMD
           END-IF
      *    病棟情報                                           
           MOVE    SPACE               TO  SYSKANRI-REC     
           INITIALIZE                      SYSKANRI-REC     
           MOVE    "5001"              TO  SYS-KANRICD      
           MOVE    PTNYUINRRK-BTUNUM   TO  SYS-KBNCD        
           MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  SYS-STYUKYMD
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD        
           PERFORM 960-SYSKANRI-READ-SEC                      
           IF      FLG-SYSKANRI        =   ZERO               
               MOVE  MCPDATA-REC           TO  SYS-5001-REC
      *
                   IF    ( WK-PTNYUINRRK-TENSTUYMD (1:6)
                                               <   RECE62-SRYYM )
                   OR    ( PTNYUINRRK-TENNYUYMD (1:6)
                                               >   RECE62-SRYYM )
                   OR    ((PTNYUINRRK-TENNYUYMD
                                               =
                                        WK-PTNYUINRRK-TENNYUYMD) AND
                          (WK-PTNYUINRRK-TENNYUYMD
                                           NOT =   SPACE       ))
                       CONTINUE
                   ELSE 
                      IF      SYS-5001-RECEDSPKBN =  SPACE
                       CONTINUE
                      ELSE
                       PERFORM VARYING IDX3    FROM    1   BY  1
                               UNTIL   IDX3    >       4
                               OR      WRK-BYOTOKBN (IDX3)
                                               =   SYS-5001-RECEDSPKBN
                           IF      WRK-BYOTOKBN (IDX3) =   SPACE
                               MOVE    SYS-5001-RECEDSPKBN
                                               TO   WRK-BYOTOKBN (IDX3)
                               MOVE    4           TO  IDX3
                           END-IF
                       END-PERFORM
                      END-IF
                      MOVE   WK-PTNYUINRRK-TENSTUYMD  TO
                                            WK-PTNYUINRRK-TENSTUYMD2
                      MOVE   PTNYUINRRK-TENNYUYMD     TO
                                            WK-PTNYUINRRK-TENNYUYMD
                   END-IF
      *        病棟番号・種別
               MOVE    RECE62-SRYYM        TO  WRK-SRYYYMM
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL   IDX3    >       31
                   IF      RECE62-BYOTONUM(IDX3)   =  SPACE
                       MOVE    IDX3            TO  WRK-SRYDD
                       IF       PTNYUINRRK-TENNYUYMD  <=  WRK-SRYYMD
                       AND      PTNYUINRRK-TENSTUYMD  >=  WRK-SRYYMD
                           MOVE  SYS-5001-BTU-NUM
                                           TO  RECE62-BYOTONUM    (IDX3)
                           MOVE  SYS-5001-BTU-SBT
                                           TO  RECE62-BYOTOSYUBETU(IDX3)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .                                                           
       24011-BYOTOKBN-SET-EXT.                   
           EXIT.                                                       
      *
      *****************************************************************
      *    入退院履歴出力処理
      *****************************************************************
       24012-RECE68-WRITE-SEC         SECTION.
      *
      *    他院歴の履歴は記載しない
           IF    ( PTNYUINRRK-JTIKBN        =   "5" )
           AND   ( PTNYUINRRK-TAINRELKBN    =   "1" )
               GO  TO  24012-RECE68-WRITE-EXT
           END-IF
      *
           INITIALIZE                      RECE68-REC
           MOVE    RECE62-KEY          TO  RECE68-KEY1
           MOVE    PTNYUINRRK-NYUINYMD TO  RECE68-NYUINYMD
           IF      PTNYUINRRK-NYUINYMD =   PTNYUINRRK-TAIINYMD
               MOVE    0                   TO  RECE68-NYUINYMD-SEQNO
           ELSE 
               IF      PTNYUINRRK-TAIINYMD =   "99999999"
                   MOVE    9                   TO  RECE68-NYUINYMD-SEQNO
               ELSE
                   MOVE    1                   TO  RECE68-NYUINYMD-SEQNO
               END-IF
           END-IF
      *
           MOVE    RECE68-REC          TO  WRK-RECE68-REC
           PERFORM 900-RECE68-READ-SEC
           IF      FLG-RECE68          =   1
               MOVE    WRK-RECE68-REC      TO  RECE68-REC
               MOVE    PTNYUINRRK-TAIINYMD TO  RECE68-TAIINYMD
               MOVE    PTNYUINRRK-NYUINCHUKBN
                                           TO  RECE68-NYUINCHUKBN
      *        入院年月日と同じ初回番号のときのみセット
               IF      PTNYUINRRK-SHONUM   =    WRK-SHONUM
                   MOVE    WRK-SHONUM           TO  RECE68-SHONUM
               END-IF
               WRITE   RECE68-REC
      *
               IF      STS-RECE68          =   "00"
                   CONTINUE
               ELSE
                   MOVE    SPACE               TO  WRK-ERR-AREA
                   INITIALIZE                      WRK-ERR-AREA
                   MOVE    3                   TO  WRK-ERR-FLG
                   MOVE    RECE68PARA          TO  WRK-ERR-FILE-NM
                   MOVE    STS-RECE68          TO  WRK-ERR-FILE-STS
                   PERFORM 500-FILE-ERR-ABORT-SEC
               END-IF
      *
               ADD     1                   TO  CNT-RECE68
           END-IF
      *
           IF      WRK-TAIINYMD        <   RECE68-TAIINYMD
               MOVE    RECE68-TAIINYMD     TO  WRK-TAIINYMD
           END-IF
           .
       24012-RECE68-WRITE-EXT.
           EXIT.
      * 
      *****************************************************************
      *    分娩テーブルチェック
      *****************************************************************
       2402-BUNBEN-CHECK-SEC           SECTION.
      *
           INITIALIZE                    BUNBEN-REC
           MOVE    RECE62-HOSPNUM    TO  BUNBEN-HOSPNUM
           MOVE    RECE62-PTID       TO  BUNBEN-PTID
           MOVE    "1"               TO  BUNBEN-NYUGAIKBN
           MOVE    PTNYUINRRK-RRKNUM TO  BUNBEN-RRKNUM
           MOVE    BUNBEN-REC        TO  MCPDATA-REC
           MOVE    "tbl_bunben"      TO  MCP-TABLE
           MOVE    "key3"            TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC            =   ZERO
             MOVE  "tbl_bunben"      TO  MCP-TABLE
             MOVE  "key3"            TO  MCP-PATHNAME
             PERFORM 900-DBFETCH-SEC
             IF    MCP-RC            =   ZERO
               MOVE   MCPDATA-REC    TO  BUNBEN-REC
               IF   (BUNBEN-RIYOUKBN    =   "1")
               AND  (BUNBEN-BUNBENKBN   =   "2")
                  ADD      1         TO  TOKKI-IDX
                  MOVE    "25"       TO  RECE62-TOKKI(TOKKI-IDX)
                  MOVE     1         TO  FLG-BUNBEN-SET
               END-IF
             END-IF
           END-IF
           MOVE    "tbl_bunben"      TO  MCP-TABLE
           MOVE    "key3"            TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       2402-BUNBEN-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費単独レセの出力処理
      *****************************************************************
       250-SEIHO-RECE61-WRITE-SEC      SECTION.
      *
      *    主保険とのまとめがなく生保、中国残留邦人等、自立支援以外との
      *    公費単独の場合
           IF    ( WRK-SEIHO-FLG           =   ZERO )
           AND   (  WRK-KOH-61-HOSPNUM >   ZERO )
               MOVE    WRK-KOH-RECE61-REC  TO  RECE61-REC
      *        ファイル編集処理
               PERFORM 2101-RECE62-HENSYU-SEC
           ELSE
               PERFORM VARYING IDWW    FROM    1   BY  1
                       UNTIL   IDWW    >       WRK-SEIHO-RECE61-MAX
                   IF      WRK-SEIHO-RECE61-PRTFLG (IDWW)
                                           =   1
                       CONTINUE
                   ELSE
                       MOVE    WRK-SEIHO-RECE61-REC (IDWW)
                                               TO  RECE61-REC
      *                ファイル編集処理
                       PERFORM 2101-RECE62-HENSYU-SEC
                   END-IF
               END-PERFORM
           END-IF
           .
       250-SEIHO-RECE61-WRITE-EXT.
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
                   PERFORM   900-CALL-ORCSJOB-SEC
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
           STRING  "ORCR0620 "             DELIMITED   BY  SIZE
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
               PERFORM   900-CALL-ORCSJOB-SEC
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
                       PERFORM   900-CALL-ORCSJOB-SEC
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
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               PERFORM   900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE61  CNT-IN  :" CNT-IN  (1) 
           DISPLAY "RECE62  CNT-OUT :" CNT-OUT (1) 
           DISPLAY "RECE68  CNT-OUT :" CNT-RECE68  
           DISPLAY "RECE69  CNT-OUT :" CNT-RECE69  
           DISPLAY "RECE61X CNT-IN  :" CNT-IN  (2) 
           DISPLAY "RECE62X CNT-OUT :" CNT-OUT (2) 
           DISPLAY "*** ORCR0620 END ***"
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
           PERFORM   900-DBFETCH-SEC
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
           PERFORM   900-DBFETCH-SEC
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
           PERFORM   900-DBFETCH-SEC
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
      *    保険番号読込
      *****************************************************************
       HKNNUM-READ-SEC           SECTION.
      *
           PERFORM   900-DBFETCH-SEC
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
                                       <=  RECE62-SRYYM                )
               AND   ( RECE62-SRYYM    <=
                               TBL-HKN-TEKEDYMD(WRK-HKNNUM-IDX)(1:6)   )
                   MOVE    TBL-HKNNUM-REC (WRK-HKNNUM-IDX)
                                       TO  HKNNUM-REC
               END-IF
           END-PERFORM
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF      HKN-HKNNUM          =   SPACE
               INITIALIZE                      HKNNUM-REC
               MOVE   RECE62-HOSPNUM       TO  HKN-HOSPNUM
               MOVE   WRK-CHK-HKNNUM       TO  HKN-HKNNUM
               MOVE   HKNNUM-REC           TO  MCPDATA-REC
               MOVE   "tbl_hknnum"         TO  MCP-TABLE
               MOVE   "key2"               TO  MCP-PATHNAME
               PERFORM   900-DBSELECT-SEC
               IF     MCP-RC               =   ZERO
                   MOVE    "tbl_hknnum"    TO  MCP-TABLE
                   MOVE    "key2"          TO  MCP-PATHNAME
                   PERFORM HKNNUM-READ-SEC
                   PERFORM UNTIL ( FLG-HKNNUM  =    1                 )
                           OR    ((HKN-TEKSTYMD (1:6)
                                               <=  RECE62-SRYYM     )
                                    AND
                                  (RECE62-SRYYM
                                               <=  HKN-TEKEDYMD(1:6)) )
                       MOVE    "tbl_hknnum"        TO  MCP-TABLE
                       MOVE    "key2"              TO  MCP-PATHNAME
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
               MOVE    "tbl_hknnum"       TO  MCP-TABLE
               MOVE    "key2"             TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
      *
       900-HKNNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       960-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       960-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
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
      *    入院履歴読込（ＫＥＹ２２）
      *****************************************************************
       900-PTNYUINRRK-KEY22-READ-SEC    SECTION.
      *    
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key51"              TO  MCP-PATHNAME
           PERFORM   900-DBFETCH-SEC
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
      *    入院履歴読込（ＫＥＹ２３）
      *****************************************************************
       900-PTNYUINRRK-KEY23-READ-SEC    SECTION.
      *    
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key23"              TO  MCP-PATHNAME
           PERFORM   900-DBFETCH-SEC
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
      *    旧姓履歴マスタ読み込み
      *****************************************************************
       900-KYUSEIRRK-READ-SEC          SECTION.
      *
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
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
      *    保険組合せマスタ読込
      *****************************************************************
       900-HKNCOMBI-INV-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
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
       900-HKNCOMBI-INV-EXT.
           EXIT.
      *
      ****************************************************************
      *    診療年月の対象となるシス管理を取得（個別処理のみ
      ****************************************************************
       900-SYSKANRI-HENSYU-SEC         SECTION.
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
           END-IF
           .
       900-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理
      *****************************************************************
       RECE61-READ-SEC            SECTION.
      *
           READ    RECE61-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-RECE61
               NOT  AT  END
                   IF      STS-RECE61          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE61PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE61          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   ADD     1     TO    CNT-IN (IDZ)
                                       CNT-IN-CANCEL
                   MOVE    RECE61-PTID
                                 TO    KEY-N-PTID
                   MOVE    RECE61-SRYYM
                                 TO    KEY-N-SRYYM
                   MOVE    RECE61-RECEIPT-KBN
                                 TO    KEY-N-RECEIPT-KBN
     *
                   DISPLAY "RECE61 " IDZ " PTID=" RECE61-PTID 
                                         " PTNUM=" RECE61-PTNUM
                           " RECEIPT-KBN=" RECE61-RECEIPT-KBN
      *
                   IF      WRK-PARA-SRYYM  =  ZERO
      *                医療機関ＩＤ編集
                       MOVE  RECE61-SRYYM     TO  WRK-SYS-SRYYMD(1:6)
                       MOVE  "01"             TO  WRK-SYS-SRYYMD(7:2)
                        PERFORM 900-SYSKANRI-HENSYU-SEC
                   END-IF
           END-READ
           .
       RECE61-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入退院履歴読込
      *****************************************************************
       900-RECE68-READ-SEC            SECTION.
      *
           READ    RECE68-FILE
               INVALID
                   MOVE    1           TO  FLG-RECE68
               NOT INVALID
                   MOVE    ZERO        TO  FLG-RECE68
           END-READ
           .
       900-RECE68-READ-EXT.
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
               STRING  "ORCR0620 select err TABLE="
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
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
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
