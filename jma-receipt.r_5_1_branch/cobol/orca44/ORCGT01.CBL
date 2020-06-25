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
       PROGRAM-ID.             ORCGT01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 総括表・公費請求書
      *  コンポーネント名  : 総括表・公費請求書作成（Ｔ０１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/08/10  再印刷の追加  
      *  01.00.02    MCC-藤原     01/09/16  請求年月日の追加
      *  01.00.03    MCC-藤原     01/09/19  帳票のエラーが０件のとき表示しない 
      *  01.02.01    MCC-藤原     01/09/21  JOINの修正      
      *  01.02.02    MCC-藤原     02/03/14  決定通知書・未請求一覧の追加      
      *  01.02.03    MCC-藤原     02/03/30  国保診療報酬請求書のパターン別印刷
      *  01.03.01    MCC-藤原     02/04/15  印刷管理・印刷ＤＢ・ジョブ管理ＤＢ
      *                                     の追加
      *  01.03.02    MCC-藤原     02/05/09  処理結果画面に頁数の追加
      *  01.03.03    MCC-藤原     02/05/15  処理前にジョブ管理を削除する
      *  01.03.04    MCC-藤原     02/05/16  再印刷ボタン追加
      *  01.03.05    MCC-藤原     02/05/21  情報削除ボタン追加
      *  01.03.05    MCC-藤原     02/06/04  レセプト処理中は処理できないようにする
      *  01.03.06    NACL-藤原    02/07/25  確定のとき自動的に情報確認画面を表示
      *  01.03.07    NACL-藤原    02/08/30  印刷指示項目の追加
      *  01.03.08    NACL-藤原    02/09/05  端末ＩＤを固定にする
      * 
      *  01.03.09    NACL-藤原    02/09/12  ディレクトリをサブプロへ
      *  01.03.10    NACL-藤原    02/10/07  ２００２年１０月改正対応
      *
      *  01.04.01    NACL-藤原    03/03/05  入院対応
      *  01.04.02    NACL-藤原    03.05.06  地方公費ＤＢ対応済フラグ
      *                                     の追加
      *                                     印刷開始・終了頁の編集の修正
      *  01.04.03    NACL-藤原    03.05.14  印刷する・しないのボタン追加
      *  01.04.04    NACL-藤原    03.05.19  国保総括に個別指定の
      *                                     保険者番号を追加
      * レセ電機能追加
      *  01.05.01    NACL-藤原    03.08.06  レセ電処理の追加
      *  01.05.02    NACL-藤原    03.09.02  メッセージコードの修正
      *  01.05.03    NACL-藤原    03.09.24  レセ電ファイルの出力先指定の修正
      *  01.05.04    NACL-藤原    04.02.17  レセ電ファイルの出力先指定
      *  01.00.05    NACL-伊藤    04.02.17  レセ電ファイルの転送エラー
      *                                     メッセージの追加
      *  01.05.06    NACL-藤原    04.03.18  レセ電時の印刷指示による処理の修正
      *  01.05.07    NACL-藤原    04.07.20  請求年月未入力のチェックの条件を修正
      *  01.05.08    NACL-藤原    04.08.10  処理開始前にジョブ管理の
      *                                     チェックを再度行なう
      *  01.05.09    NACL-藤原    04/09/17  入外区分によってレセ電の
      *                                     処理対象を限定する
      * 公費個別発行機能対応
      *  01.06.01    NACL-藤原    04/10/25  公費個別発行機能の追加
      *  01.06.02    NACL-藤原    05/02/10  レセ電総括表対応
      *                                     一括入力 
      *
      *  02.06.01    NACL-藤原    05/10/14  データ収集事業対応
      *
      *  02.07.01    NACL-藤原    05/10/31  MONFUNC対応
      *  02.07.02    NACL-藤原    05/12/02  レセ電データ複数枚対応
      *
      *  02.08.01    NACL-藤原    06/02/20  主科対応
      *
      *  03.00.01    NACL-藤原    06/05/29  印刷時のプリンター指定対応
      *
      *  03.02.01    NACL-藤原    06/07/24  マル長の別総括対応
      *  03.02.02    NACL-藤原    06/07/31  国保主科対応
      *
      *  03.04.01    NACL-藤原    06/12/05  未請求レセ対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-藤原    07/05/25  印刷データ削除設定機能追加
      *  03.05.03    NACL-藤原    07/08/01  クライアント保存対応
      *  03.05.04    NACL-藤原    07/09/11  新潟県中越沖地震対応
      *                                     ２００７年９月〜１２月対象 
      *
      *  04.01.01    NACL-藤原    07/11/22  公害レセプト対応
      *
      *  04.02.01    NACL-藤原    08/03/04  平成２０年４月改正対応
      *  04.02.02    NACL-藤原    08/03/18  レセプト個別作成後の公費請求
      *                                     書処理を可能とするためのパラ
      *                                     メタ種別の置き換え
      *  04.02.03    NACL-藤原    08/03/31  広域連合対応
      *  04.02.04    NACL-藤原    08/04/21  広域連合の保険者番号指定をしない
      *  04.02.05    NACL-藤原    08/05/14  診療年月による対象帳票の再取得
      *  04.02.06    NACL-藤原    08/06/02  公費請求書ボタンの再設定をしない
      *
      *  04.03.01    NACL-藤原    08/05/27  月遅れ分のレセ電データの有無
      *                                     チェック
      *  04.03.02    NACL-藤原    08/07/09  仮レセ作成（院外処方含む）対応
      *  04.03.03    NACL-藤原    08/07/23  宮城県、岩手県地震対応
      *                                     ２００８年６月〜対象 
      *  04.03.04    NACL-藤原    08/09/03  オンライン請求（個別請求）対応
      *
      *  04.04.01    NACL-藤原    08/11/19  レセ電ＣＤ−Ｒ出力対応
      *  04.04.02    NACL-藤原    08/11/25  光ディスク等送付書作成対応
      *  04.04.03    NACL-藤原    08/12/01  公害診療報酬請求書
      *                                     名古屋市、北九州市対応
      *  04.04.04    NACL-藤原    09/03/24  主科設定時の主科未設定データ
      *                                     の有無チェック
      *  04.04.05    NACL-藤原    09/04/10  月遅れ分のレセ電データの有無
      *                                     チェックのとき特別療養費は対
      *                                     象外とする
      *  04.04.06    NACL-藤原    09/08/17  「平成２１年７月中国・九州
      *                                     北部豪雨」対応
      *                                     ２００９年７月〜対象 
      *
      *  04.05.01    NACL-藤原    09/05/25  広域連合の保険者番号指定対応
      *  04.05.02    NACL-藤原    09/08/21  レセ電データ格納フォルダ設定
      *                                     対応
      *                                     ファイル出力先のデフォルト値
      *                                     の設定も可能にする 
      *  04.05.03    NACL-藤原    09/09/18  「平成２１年台風第９号」対応
      *                                     兵庫県・岡山県
      *                                     ２００９年８月〜２０１０年１月対象 
      *  04.05.04    NACL-藤原    10/02/26  主科設定時の主科未設定データ
      *                                     の有無チェックの修正 
      *  04.05.05    NACL-藤原    10/03/23  ＨＯＳＴ名取得方法の変更
      *  04.05.06    NACL-藤原    10/08/11  「平成２２年７月中国地方豪雨」対応
      *                                     広島県・山口県
      *                                     ２０１０年７月〜２０１０年１２月対象 
      *  04.05.07    NACL-藤原    10/11/16  「鹿児島県奄美市等における局地的大雨」対応
      *                                     鹿児島県
      *                                     ２０１０年１０月〜２０１１年３月対象 
      *  04.05.08    NACL-藤原    10/12/01  クライアント保存削除処理の修正
      *  04.05.09    NACL-門脇    11/02/17  災害対応修正（平成２３年１月〜）
      *  04.05.10    NACL-藤原    11/05/26  災害対応
      *                                     システム管理の設定に従い、災
      *                                     害該当レセプトのレセ電データ
      *                                     のチェックを追加
      *  04.05.11    NACL-藤原    12/03/27  算定日情報に係る記録条件仕様
      *                                     の変更に伴う月後れ分のチェック
      *
      *  04.06.01    NACL-藤原    11/12/12  点検レセ０点公費印字対応
      *  04.06.02    NACL-藤原    12/06/15  点検レセ０点公費と未請求解除
      *                                     のメッセージ表示を別にする
      *  04.06.03    NACL-藤原    12/12/04  出産育児一時金のＣＳＶ出力時
      *                                     の履歴作成
      *  04.06.04    NACL-藤原    12/12/04  公費ＣＳＶ、出産育児一時金の
      *                                     クライアント保存（ＣＤ−Ｒ）対応
      *
      *  04.07.01    NACL-藤原    11/09/02  労災請求書作成対応
      *  04.07.02    NACL-藤原    12/02/13  健保組合への直接請求と審査
      *                                     支払機関請求対応
      *  04.07.03    NACL-藤原    12/08/01  レセ電データチェック機能対応
      *  04.07.04    NACL-藤原    12/08/28  スクリプト起動パスサーチ対応
      *  04.07.05    NACL-藤原    12/10/31  ファイル出力先設定のデフォルト
      *                                     変更
      *  04.07.06    NACL-藤原    12/11/19  主科情報の取得方法変更
      *  04.07.07    NACL-藤原    12/12/11  確認画面表示の修正
      *  04.07.08    NACL-藤原    13/04/10  個別作成時のプレビュー画面表示
      *  04.07.09    NACL-藤原    13/02/07  労災レセ電対応
      *  04.07.10    NACL-藤原    13/05/02  プレビュー画面戻り時の公費Ｃ
      *                                     ＳＶの状態設定
      *  04.07.11    NACL-藤原    13/08/20  月遅れ分のレセ電データの有無
      *                                     チェックのとき０９５９３で始
      *                                     まるコードがあるレセプトは対
      *                                     象外とする
      *  04.07.12    NACL-藤原    13/11/01  労災レセ電データチェック機能
      *                                     対応
      *  04.07.13    NACL-藤原    13/12/09  医療情報連携基盤対応
      *  04.07.14    NACL-藤原    14/01/30  労災の光ディスク等送付書作成
      *                                     対応
      *  04.07.15    NACL-藤原    14/04/22  ＥＦファイル作成対応
      *  04.07.16    NACL-藤原    14/12/05  入力チェックの修正
      *  04.07.17    NACL-藤原    14/12/24  ＥＦファイル名修正
      *  04.07.18    NACL-藤原    15/07/17  労災診療費請求書及びアフター
      *                                     ケア委託費請求書のＯＣＲ様式
      *                                     対応
      *
      *  04.08.01    NACL-森脇    13/04/12  システムＤＢ対応
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.03    NACL-藤原    14/08/26  fdd 廃止に伴う対応
      *  04.08.04    NACL-藤原    14/08/28  ４．８ミドル対応
      *  04.08.05    NACL-太田    15/03/06  システム管理 8000番
      *                                     (印刷データ削除)の追加処理を
      *                                     バッチに移動
      *  04.08.06    NACL-藤原    15/08/07  医療情報連携基盤のクライアン
      *                                     ト保存対応
      *  04.08.07    NACL-藤原    15/10/15  様式４対応
      *  04.08.08    NACL-藤原    16/01/18  地方公費処理スクリプトの存在
      *                                     チェック対応
      *  04.08.09    NACL-藤原    16/06/13  社保診療報酬請求書の災害該当
      *                                     分のまとめ対応
      *  04.08.10    NACL-藤原    16/06/27  医療機関コード変更時のレセ電
      *                                     データ等の作成対応
      *                                      （１日からの変更のみ）
      *  04.08.11    NACL-藤原    16/10/07  自賠責ＰＤＦファイル作成対応
      *  04.08.12    NACL-藤原    17/03/08  初画面の診療年月が月途中の医療
      *                                     期間コード変更月のときの対応
      *  04.08.13    NACL-藤原    17/03/09  期間表示の設定の追加
      *  04.08.14    NACL-藤原    17/04/11  ボタン設定の修正
      *
      *  04.09.01    NACL-藤原    16/11/18  Ｄｏｃｋｅｒ対応
      *  04.09.02    NACL-藤原    17/02/24  レポジトリ名設定時は地方公費
      *                                     処理スクリプトの存在チェック
      *                                     を行わない
      *  04.09.03    NACL-藤原    17/08/01  ＨＡＯＲＩ対応
      *
      *  05.00.00    NACL-森脇    17/11/30  クライアント印刷対応
      *  05.00.01    NACL-藤原    18/10/15  ＭＯＮＢＬＯＢエラー対応
      *  05.00.02    NACL-藤原    19/01/17  Warning 対応
      *  05.00.03    NACL-藤原    19/03/25  元号改正対応
      *  05.00.04    NACL-藤原    20/03/27  令和２年４月改正未対応メッ
      *                                     セージ対応
      *  05.00.05    NACL-藤原    20/04/09  令和２年４月改正対応メッセー
      *                                     ジ対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    
      *    処理スクリプトチェック用ワークファイル
           SELECT  WRK-FILE        ASSIGN  WRKPARA
                                   FILE    STATUS  IS  STS-WRK.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    処理スクリプトチェック用ワークファイル
       FD  WRK-FILE.
       01  WRK-R                   PIC X(10).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING   //TEMPFLPARA//
                                   BY          //RECEERR//.
      *
           COPY    "CPERRFL.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    レセプト作成指示共通パラメタ
           COPY    "T01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "T01SCR-SPA".
           COPY    "CPORCSPRTLNK.INC"
                                   REPLACING  //LNK-//
                                   BY         //SPA-LNK-//.
      *
           COPY    "T06SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-WRK             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-BTPARA          PIC 9(01).
           03  FLG-SYUKA           PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-RECEDEN         PIC 9(01).
      *
           03  FLG-3004-PARAKBN    PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-RECEDEN-CHK     PIC 9(01).
           03  FLG-RECEDEN-201203-CHK
                                   PIC 9(01).
           03  FLG-SYUKA-CHK      PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
           03  IDXX                PIC 9(04).
           03  IDXY                PIC 9(04).
           03  IDYY                PIC 9(04).
           03  IDZZ                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PRTKANRI-RENNUM PIC 9(04).
           03  WRK-RECEERR         PIC X(200).
           03  WRK-SKYYM           PIC X(06).
           03  WRK-CREYMD          PIC X(08).
           03  WRK-TIDMSG          PIC X(150).
           03  WRK-TIDMSG1         PIC X(150).
           03  WRK-TIDCD-MSG       PIC X(150).
           03  WRK-YM              PIC X(07). 
           03  WRK-KBNCD           PIC 9(04).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-PAGE            PIC ZZZZZZZZZ9.
           03  WRK-WRNO-X.
               05  WRK-WRNO        PIC 9(02).
           03  WRK-SKYYM-OLD       PIC 9(06).
           03  WRK-PRTKBN-OLD      PIC X(01).
      *     
           03  WRK-SHELL-ARG1.
             05  WRK-SHELL-SRYYM   PIC  X(06).
             05  WRK-SHELL-TERMID  PIC  X(16).
             05  WRK-SHELL-SYSYMD  PIC  X(08).  
      *       
           03  WRK-SHELL-RECEKBN   PIC X(01).
           03  WRK-SHELL-SYOKBN    PIC X(01).
      *  
           03  WRK-STPAGE          PIC 9(05).
           03  WRK-EDPAGE          PIC 9(05).
      *
           03  WRK-GMN-CUR         PIC 9(02).
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-PRT-PAGE        PIC 9(10).
           03  WRK-PRT-PAGE1       PIC 9(10).
           03  WRK-PRT-PAGE2       PIC 9(06).
      *
           03  WRK-PRTNM           PIC 9(04).
           03  WRK-PRTNM-R         REDEFINES   WRK-PRTNM.
               05  WRK-PRTNM-FIL   PIC X(02).
               05  WRK-PRTNM-X     PIC X(02). 
           03  WRK-DASUPLOAD       PIC 9(01).
      *
           03  WRK-MOTOPG          PIC X(08). 
      *
      *    パラメタＤＢ編集用
           03  WRK-BTPARA-INFO-PARA.
               05  WRK-BTPARA-DATA PIC X(10).
               05  WRK-BTPARA-FILENM
                                   PIC X(20).
               05  WRK-BTPARA-DATAKBN
                                   PIC X(01).
               05  WRK-BTPARA-STNO PIC 9(02).
               05  WRK-BTPARA-EDNO PIC 9(02).
               05  WRK-BTPARA-WRNO PIC 9(02).
      *
           03  WRK-SYS-SRYYMD      PIC X(08).
      *
           03  WRK-CHK-AREA.
               05  WRK-CHK-CNT     PIC 9(02).
               05  WRK-CHK-SHELLID PIC X(20).
               05  WRK-CHK-SHORIMSG
                                   PIC X(40).
           03  WRK-Z9              PIC Z9.
      *
           03  WRK-SRYYMKBN.
               05  WRK-SRYYMKBN-ST PIC X(06).
               05  WRK-SRYYMKBN-FL PIC X(01).
               05  WRK-SRYYMKBN-ED PIC X(06).
               05  WRK-SRYYMKBN-ERR
                                   PIC X(02).
      *
           03  WRK-STYUKYMD        PIC X(08).
           03  WRK-EDYUKYMD        PIC X(08).
      *
       01  WRKPARA                 PIC X(200).
      *
      *    直接請求する保険者番号
       01  WRK-HKNJANUM-AREA.
           03  WRK-HKNJANUM-S      PIC X(08)   OCCURS  10.
      *
      *    医療機関コード別
       01  WRK-1001-TABLE.
           07  WRK-1001-OCC        OCCURS  10.
               09  WRK-1001-TSTYUKYMD
                                   PIC X(08).
               09  WRK-1001-TEDYUKYMD
                                   PIC X(08).
               09  WRK-1001-THOSPCD
                                   PIC 9(07).
           07  WRK-1001-MAX        PIC 9(02).
      *      
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  SYS-TIME            PIC X(06).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMDG3        PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *
           03  WRK-T99-MSG.
               05  WRK-T99-MSG1    PIC X(24)   VALUE
                   "処理は正常に終了しました".
               05  FILLER          PIC X(10)   VALUE
                   "　レセプト".
               05  WRK-T99-MSG-KENSU
                                   PIC ZZZZZ9.
               05  FILLER          PIC X(02)   VALUE   "件".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    プレビュー用情報
           COPY    "CPXC01LNK.INC".
      *
      *    県単独事業用シェル
       01  WRK-SHELL-KENTAN.
           03  FILLER             PIC X(15) VALUE
                                             "scripts/kentan/".
           03  WRK-SHELL-SHELL    PIC X(30).
      *                             
       01  WRK-SHELL-NAME         PIC X(50).
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".
      *
       01  WRK-CONS-AREA.
      *
           03  WRK-CONS-3004-MAX   PIC 9(03)   VALUE   30.
      *    システム管理ＤＢ（３００３）のキー値  
           03  WRK-CONS-3003-KBNCD PIC X(08)   VALUE   "044".
      *    印刷管理ＤＢのキー値  
           03  WRK-CONS-PRTKANRI-TBL-KEY
                                   PIC X(08)   VALUE   "SOKATU".
           03  WRK-CONS-PRTKANRI-TBL-GROUP
                                   PIC X(14)   VALUE   SPACE.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "SOKATU".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   ZERO.
      *     
      *    ジョブ管理ＤＢのキー値（レセプト）  
           03  WRK-CONS-JOB-SHELLID-RECEPT1
                                   PIC X(08)   VALUE   "RECEPT1".
           03  WRK-CONS-JOB-JOBID-RECEPT1
                                   PIC 9(07)   VALUE   1.
      *     
      *    ジョブ管理ＤＢのキー値（データ収集）  
           03  WRK-CONS-JOB-SHELLID-COLLECT
                                  PIC X(08)    VALUE   "COLLECT".
           03  WRK-CONS-JOB-JOBID-COLLECT
                                   PIC 9(07)   VALUE   1.
      *     
      *    パラメタＤＢのキー値（レセ電作成）  
           03  WRK-CONS-BTPARA-SHELLID
                                   PIC X(08)   VALUE   "SOKATU".
           03  WRK-CONS-BTPARA-SCRIPTID
                                   PIC X(10)   VALUE   "recept4.sh".
      *    
      *    固定端末ＩＤ
           03  WRK-CONS-TERMID.
               05  FILLER          PIC X(08)   VALUE   "0100007F". 
               05  FILLER          PIC X(56)   VALUE   ALL "Z".
      *
      *    エラーファイルＩＤ
           03  WRK-CONS-FILE-ID    PIC X(08)   VALUE   "OR044ERR".
      *
      *    労災レセ電対象年月
           03  WRK-CONS-SRYYM-201307
                                   PIC 9(06)   VALUE   201307.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK1031.INC".
      *
           COPY    "CPSK2001.INC".
      *
           COPY    "CPSK2003.INC".
      *
           COPY    "CPSK2005.INC".
           COPY    "CPSK200501.INC"
                                   REPLACING  //SYS-200501//
                                   BY         //SYS-25//.
           COPY    "CPSK200505.INC".
      *
           COPY    "CPSK2009.INC".
      *
           COPY    "CPSK3003.INC".
      *
           COPY    "CPSK3004.INC".
      *
           COPY    "CPSK4001.INC".
      *
           COPY    "CPSK4100.INC".
      *
           COPY    "CPSK9101.INC".
      *
           COPY    "CPSK9102.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    保険者
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *    主科
       01  SYUKA-REC.
           COPY    "CPSYUKA.INC".
      *
           COPY    "CPRCF010.INC".
      *
      *    レセプト電算
           COPY    "CPRECEDEN.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
           COPY    "CPORCSCOLCHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    プリンタ名取得サブ
           COPY  "CPORCSPRTNMGET.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
      *    ＣＳＶ管理ＤＢ制御サブ
           COPY    "CPORCSCSVINFO.INC".
      *
      *    クライアント保存可否情報取得サブ
           COPY    "CPORCSCLIENTSV.INC".
      *
      *    ユーザー情報取得
           COPY    "CPORCSLDUSER.INC".
      *----(05.00.00)--ADD-START---
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *----(05.00.00)--ADD-END-----
      *
           COPY    "MONBLOB".
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA44SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
                                       STS-AREA
                                       IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-T01FREE
           MOVE    SPA-WORK        TO  SPA-T01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-T01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-T01FREE     TO  SPA-FREE
      *
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "TERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
               IF      SPA-TIDCD       NOT =   SPACE
                   EVALUATE    SPA-TIDCD
                       WHEN    "2004"
                       WHEN    "2009"
                           PERFORM 520-TIDSET-SEC
                           GO  TO  100-INIT-EXT
                   END-EVALUATE
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "T01    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *
      *        元の画面へ戻す
               IF      SPA-MOTOPG      =   "XA01"  OR  "XB01"  OR
                                           "XC01"
                   MOVE    SPA-MOTOPG      TO  WRK-MOTOPG
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-T01FREE
                   IF      WRK-MOTOPG      =   "XC01"
      *                公費ＣＳＶボタンの状態設定
                       PERFORM 3102-FILE-INFO-CHECK-SEC
                   END-IF
                   INITIALIZE              SPA-T01KYOUTU
                   GO  TO  300-SCREEN-EXT
               END-IF
      *
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
               INITIALIZE              SPA-T01KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *    作表処理
               WHEN    "T99" 
                   PERFORM 320-T99-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "TID1"
                   PERFORM 330-TID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "TID2"
                   PERFORM 340-TID2-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "T02"
                   PERFORM 350-T02-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "T03"
                   GO      TO      300-SCREEN-EXT
               WHEN    "T04"
                   PERFORM 360-T04-SET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-T01FREE
      *
      *    医療情報連携基盤用情報
           INITIALIZE                  SYS-9101-REC
           MOVE    "9101"          TO  SYS-9101-KANRICD
           MOVE    "*"             TO  SYS-9101-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-9101-STYUKYMD
                                       SYS-9101-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-9101-HOSPNUM
           MOVE    SYS-9101-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE   MCPDATA-REC  TO  SYS-9101-REC
           ELSE
               MOVE   SPACE        TO  SYS-9101-REC
           END-IF
           IF      SYS-9101-COOPBASE   =   SPACE
               MOVE    "0"             TO  SYS-9101-COOPBASE
           END-IF
           MOVE    SYS-9101-COOPBASE
                                   TO  SPA-NAI-9101-COOPBASE
           MOVE    SYS-9101-FILE-DIR
                                   TO  SPA-NAI-9101-FILE-DIR
      *
      *    ＥＦファイルデータ情報
           INITIALIZE                  SYS-9102-REC
           MOVE    "9102"          TO  SYS-9102-KANRICD
           MOVE    "*"             TO  SYS-9102-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-9102-STYUKYMD
                                       SYS-9102-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-9102-HOSPNUM
           MOVE    SYS-9102-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE   MCPDATA-REC  TO  SYS-9102-REC
           ELSE
               MOVE   SPACE        TO  SYS-9102-REC
           END-IF
           IF      SYS-9102-EFOUT  =   SPACE
               MOVE    "0"             TO  SYS-9102-EFOUT
           END-IF
           MOVE    SYS-9102-EFOUT  TO  SPA-NAI-9102-EFOUT
           MOVE    SYS-9102-FILE-DIR
                                   TO  SPA-NAI-9102-FILE-DIR
      *
      *    出力先プリンタ名割り当て情報編集
           MOVE    SPACE               TO  SYS-1031-REC
           INITIALIZE                  SYS-1031-REC
           MOVE    "1031"              TO  SYS-1031-KANRICD
           MOVE    "*"                 TO  SYS-1031-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1031-STYUKYMD
                                           SYS-1031-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1031-HOSPNUM
           MOVE    SYS-1031-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1031-REC
           ELSE    
               INITIALIZE                  SYS-1031-REC
           END-IF
           MOVE    SYS-1031-PRTNM (5)
                                   TO  SPA-NAI-1031-PRTNM
      *
      *    プリンター名取得
           INITIALIZE                      ORCSPRTNMGETAREA
           MOVE    SPA-SYSYMD          TO  PRTNMGET-YMD
           CALL    "ORCSPRTNMGET"      USING   ORCSPRTNMGETAREA
grpsys                                         SPA-AREA
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       PRTNMGET-PRTNM-MAX
               COMPUTE IDY =   IDX     -   1
               MOVE    IDY                 TO  WRK-PRTNM
               MOVE    WRK-PRTNM-X         TO  SPA-GMN-T03-PRTNML (IDX)
               MOVE    PRTNMGET-PRTNM (IDX)    TO
                                           SPA-GMN-T03-PRTNMMEIL (IDX)
               MOVE    IDX                 TO  SPA-GMN-T03-PRTNM-MAX
           END-PERFORM
      *
           IF      SPA-BEDSU       >   ZERO
               MOVE    "0"             TO  SPA-GMN-NYUGAIKBNL        (1)
               MOVE    "入院・入院外"  TO  SPA-GMN-NYUGAIKBNMEIL     (1)
               MOVE    "1"             TO  SPA-GMN-NYUGAIKBNL        (2)
               MOVE    "入  院"        TO  SPA-GMN-NYUGAIKBNMEIL     (2)
               MOVE    "2"             TO  SPA-GMN-NYUGAIKBNL        (3)
               MOVE    "入院外"        TO  SPA-GMN-NYUGAIKBNMEIL     (3)
               MOVE    3               TO  SPA-GMN-NYUGAIKBN-MAX
           ELSE
               MOVE    "2"             TO  SPA-GMN-NYUGAIKBNL        (1)
               MOVE    "入院外"        TO  SPA-GMN-NYUGAIKBNMEIL     (1)
               MOVE    1               TO  SPA-GMN-NYUGAIKBN-MAX
           END-IF 
      *
           MOVE    SPA-GMN-NYUGAIKBNLST (1)
                                   TO  SPA-GMN-NYUGAIKBN-G
      *
           MOVE    "1"                 TO  SPA-GMN-PRTKBNL       (1)
           MOVE    "提出用"            TO  SPA-GMN-PRTKBNMEIL    (1)
           MOVE    "2"                 TO  SPA-GMN-PRTKBNL       (2)
           MOVE    "点検用（院外処方含）"
                                       TO  SPA-GMN-PRTKBNMEIL    (2)
           MOVE    2                   TO  SPA-GMN-PRTKBN-MAX
           IF      SPA-NAI-9101-COOPBASE
                                       =   "1" OR  "2"
               MOVE    "6"                 TO  SPA-GMN-PRTKBNL   (3)
               MOVE    "連携基盤用"        TO  SPA-GMN-PRTKBNMEIL(3)
               MOVE    "7"                 TO  SPA-GMN-PRTKBNL   (4)
               MOVE    "連携基盤用（当日分まで）"
                                           TO  SPA-GMN-PRTKBNMEIL(4)
               MOVE    4                   TO  SPA-GMN-PRTKBN-MAX
           END-IF
           IF      SPA-NAI-9102-EFOUT  =   "1"
               ADD     1                   TO  SPA-GMN-PRTKBN-MAX
               MOVE    "8"                 TO  SPA-GMN-PRTKBNL
                                                   (SPA-GMN-PRTKBN-MAX)
               MOVE    "ＥＦファイル"      TO  SPA-GMN-PRTKBNMEIL
                                                   (SPA-GMN-PRTKBN-MAX)
               IF      SPA-BEDSU       >   ZERO
                   ADD     1                   TO  SPA-GMN-PRTKBN-MAX
                   MOVE    "9"                 TO  SPA-GMN-PRTKBNL
                                                   (SPA-GMN-PRTKBN-MAX)
                   MOVE    "様式４"            TO  SPA-GMN-PRTKBNMEIL
                                                   (SPA-GMN-PRTKBN-MAX)
               END-IF
           END-IF
           MOVE    SPA-GMN-PRTKBNLST (1)
                                       TO  SPA-GMN-PRTKBN-G
      *
           MOVE    "0"                 TO  SPA-GMN-RECEDEN-CHKL    (1)
           MOVE    "チェックしない"    TO  SPA-GMN-RECEDEN-CHKMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-RECEDEN-CHKL    (2)
           MOVE    "チェックする"      TO  SPA-GMN-RECEDEN-CHKMEIL (2)
           MOVE    2                   TO  SPA-GMN-RECEDEN-CHK-MAX
      *
      *    請求年月
           IF      SPA-SYSYMD(7:2)     >   "10"
               MOVE    SPA-SYSYMD  (1:6)   TO  SPA-NAI-SKYYM
               MOVE    SPA-SYSYMDWH(1:6)   TO  SPA-GMN-SKYYM
           ELSE
               INITIALIZE                     STS-AREA-DAY
               INITIALIZE                     LNK-DAY6-AREA
               MOVE  "61"                     TO  LNK-DAY6-IRAI
               MOVE  SPA-SYSYMD               TO  LNK-DAY6-KIJUN
               MOVE  "2"                      TO  LNK-DAY6-ZOGENPTN
               MOVE  -1                       TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"                USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN(1:6)   TO  SPA-NAI-SKYYM
               MOVE    LNK-DAY6-KEISAN        TO  WRK-SYMD  
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG    (1:6)   TO  SPA-GMN-SKYYM
           END-IF
      *
      *    請求年月日
           MOVE    SPA-SYSYMD          TO  SPA-NAI-SKYYMD
           MOVE    SPA-SYSYMDWH        TO  SPA-GMN-SKYYMD
      *
      *    レセ電処理区分
           MOVE    SPACE               TO  SYS-2005-REC 
           INITIALIZE                      SYS-2005-REC
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-2005-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2005-STYUKYMD (7:2)
           MOVE    SYS-2005-STYUKYMD   TO  SYS-2005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2005-REC
           ELSE
               INITIALIZE                  SYS-2005-REC
           END-IF
           MOVE    SYS-2005-SYARECEKBN TO  SPA-NAI-2005-SYARECEKBN
           MOVE    SYS-2005-KKHRECEKBN TO  SPA-NAI-2005-KKHRECEKBN
      *
           INITIALIZE                      SYS-25-REC
           MOVE    "2005"              TO  SYS-25-KANRICD
           MOVE    "01"                TO  SYS-25-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-25-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-25-STYUKYMD (7:2)
           MOVE    SYS-25-STYUKYMD     TO  SYS-25-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-25-HOSPNUM
           MOVE    SYS-25-REC          TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE   MCPDATA-REC          TO  SYS-25-REC
           ELSE
               INITIALIZE                  SYS-25-REC
           END-IF
           IF      SYS-25-KOUIKIKBN
                                       =   SPACE
               MOVE    "0"                 TO  SYS-25-KOUIKIKBN
           END-IF 
           IF      SYS-25-KOUIKIRECEKBN
                                       =   SPACE
               MOVE    "0"                 TO  SYS-25-KOUIKIRECEKBN
           END-IF 
           IF      SYS-25-RECEDEN-CDRKBN
                                       =   SPACE
               MOVE    "0"                 TO  SYS-25-RECEDEN-CDRKBN
           END-IF 
           IF      SYS-25-RECEDEN-CDRKBN
                                       =   SPACE
               MOVE    "0"                 TO  SYS-25-RECEDEN-CDRKBN
           END-IF 
           IF      SYS-25-RECEDEN-LST1KBN
                                       =   SPACE
               MOVE    "1"                 TO
                                           SYS-25-RECEDEN-LST1KBN
           END-IF 
           IF      SYS-25-RECEDEN-LST2KBN
                                       =   SPACE
               MOVE    "0"                 TO
                                           SYS-25-RECEDEN-LST2KBN
           END-IF 
           IF      SYS-25-RECEDEN-RIREKIKBN
                                       =   SPACE
               MOVE    "0"                 TO
                                           SYS-25-RECEDEN-RIREKIKBN
           END-IF 
           IF      SYS-25-RECEDEN-DATAKBN
                                       =   SPACE
               MOVE    "4"                 TO
                                           SYS-25-RECEDEN-DATAKBN
           END-IF 
           IF     SYS-25-RECEDEN-DISASTER
                                       =   SPACE
               MOVE    "1"             TO  SYS-25-RECEDEN-DISASTER
           END-IF
           IF     SYS-25-RECEDEN-CHK-KBN
                                       =   SPACE
               MOVE    "0"             TO  SYS-25-RECEDEN-CHK-KBN
           END-IF
           IF     SYS-25-ROURECEKBN
                                       =   SPACE
               MOVE    "0"             TO  SYS-25-ROURECEKBN
           END-IF
           IF     SYS-25-ROUSAIPRTKBN  =   SPACE   OR  "1"
               MOVE    "0"             TO  SYS-25-ROUSAIPRTKBN
           END-IF
           IF     SYS-25-FRAME-ROUSAIPRTKBN
                                       =   SPACE
               MOVE    "0"             TO  SYS-25-FRAME-ROUSAIPRTKBN
           END-IF
           IF     SYS-25-DISASTER-SYAPRTKBN
                                       =   SPACE
               MOVE    "0"             TO  SYS-25-DISASTER-SYAPRTKBN
           END-IF
           IF      SYS-25-JIBAI-PDF
                                       =   SPACE
               MOVE    "0"             TO  SYS-25-JIBAI-PDF
           END-IF
           IF      SYS-25-RSI-INVOICE-KBN
                                       =   SPACE
               MOVE    "0"             TO  SYS-25-RSI-INVOICE-KBN
           END-IF
      *
           MOVE    SYS-25-KOUIKIKBN
                                       TO  SPA-NAI-200501-KOUIKIKBN
           MOVE    SYS-25-KOUIKIRECEKBN
                                       TO  SPA-NAI-200501-KOUIKIRECEKBN
           MOVE    SYS-25-RECEDEN-LST1KBN
                                       TO
                                       SPA-NAI-200501-RECEDEN-LST1KBN
           MOVE    SYS-25-RECEDEN-LST2KBN
                                       TO
                                       SPA-NAI-200501-RECEDEN-LST2KBN
           MOVE    SYS-25-RECEDEN-RIREKIKBN
                                       TO
                                       SPA-NAI-200501-RECEDEN-RIREKIKB
           MOVE    SYS-25-RECEDEN-DATAKBN
                                       TO
                                       SPA-NAI-200501-RECEDEN-DATAKBN
           MOVE    SYS-25-RECEDEN-DISASTER
                                       TO
                                       SPA-NAI-200501-RECEDEN-DISASTER
           MOVE    SYS-25-RECEDEN-CHK-KBN
                                       TO
                                       SPA-NAI-200501-RECEDEN-CHK-KBN
           MOVE    SYS-25-ROURECEKBN
                                       TO  SPA-NAI-200501-ROURECEKBN
           MOVE    SYS-25-ROUSAIPRTKBN
                                       TO  SPA-NAI-200501-ROUSAIPRTKBN
           MOVE    SYS-25-FRAME-ROUSAIPRTKBN
                                       TO
                                       SPA-NAI-200501-FRAME-ROUSAIPRTK
           MOVE    SYS-25-DISASTER-SYAPRTKBN
                                       TO
                                       SPA-NAI-200501-DISASTER-SYAPRTK
           MOVE    SYS-25-JIBAI-PDF    TO  SPA-NAI-200501-JIBAI-PDF
           MOVE    SYS-25-RSI-INVOICE-KBN
                                       TO
                                       SPA-NAI-200501-RSI-INVOICE-KBN
     *
           IF      SPA-MW              =   SPA-GINBEE
               MOVE    "0"                 TO
                                       SPA-NAI-200501-RECEDEN-RIREKIKB
           END-IF
      *
           IF      SPA-NAI-200501-RECEDEN-CHK-KBN
                                       =   "0"
               MOVE    SPA-GMN-RECEDEN-CHKLST (1)
                                           TO  SPA-GMN-RECEDEN-CHK-G
           ELSE
               MOVE    SPA-GMN-RECEDEN-CHKLST (2)
                                           TO  SPA-GMN-RECEDEN-CHK-G
           END-IF
      *
           INITIALIZE                      SYS-200505-REC
           MOVE    SPA-HOSPNUM         TO  SYS-200505-HOSPNUM
           MOVE    "2005"              TO  SYS-200505-KANRICD
           MOVE    "05"                TO  SYS-200505-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-200505-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-200505-STYUKYMD (7:2)
           MOVE    SYS-200505-STYUKYMD TO  SYS-200505-EDYUKYMD
           MOVE    SYS-200505-REC      TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE   MCPDATA-REC          TO  SYS-200505-REC
           ELSE
               INITIALIZE                      SYS-200505-REC
           END-IF
           MOVE    ZERO                TO  IDXY
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       5
                   OR      SYS-200505-HKNJANUM (IDX)
                               =   SPACE
               IF      IDXY            =   ZERO
                   MOVE    1               TO  IDXY
                   MOVE    "00000000"      TO  SPA-GMN-DIRECT-HKNJANUML
                                                                  (IDXY)
                   MOVE    "直接請求する保険者以外"
                                           TO  SPA-GMN-DIRECT-HKNJANAMEL
                                                                  (IDXY)
               END-IF
               ADD     1               TO  IDXY
               MOVE    SYS-200505-HKNJANUM    (IDX)
                                       TO  SPA-GMN-DIRECT-HKNJANUML
                                                                  (IDXY)
               MOVE    SYS-200505-HKNJANUM-63 (IDX)
                                       TO  SPA-NAI-DIRECT-HKNJANUM-63L
                                                                  (IDXY)
               INITIALIZE                  HKNJAINF-REC
               MOVE    SPA-HOSPNUM     TO  HKNJA-HOSPNUM
               MOVE    SPA-GMN-DIRECT-HKNJANUML (IDXY)
                                       TO  HKNJA-HKNJANUM
               MOVE    HKNJAINF-REC    TO  MCPDATA-REC
               PERFORM 900-HKNJAINF-INV-SEC
               MOVE    HKNJA-HKNJANAME TO  SPA-GMN-DIRECT-HKNJANAMEL
                                                                  (IDXY)
           END-PERFORM
           MOVE    IDXY                TO  SPA-GMN-DIRECT-HKNJANUM-MAX
           IF      SPA-GMN-DIRECT-HKNJANUM-MAX
                                       >   ZERO
               MOVE    SPA-GMN-DIRECT-HKNJANUMLST (1)
                                           TO  SPA-GMN-DIRECT-HKNJANUM-G
               MOVE    SPACE               TO
                                           SPA-NAI-DIRECT-HKNJANUM-63
           END-IF
      *
      *    公害医療機関情報
           MOVE    SPACE               TO  SYS-4100-REC 
           INITIALIZE                      SYS-4100-REC
           MOVE    "4100"              TO  SYS-4100-KANRICD
           MOVE    "*"                 TO  SYS-4100-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-4100-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-4100-STYUKYMD (7:2)
           MOVE    SYS-4100-STYUKYMD   TO  SYS-4100-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-4100-HOSPNUM
           MOVE    SYS-4100-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-4100-REC
           ELSE
               INITIALIZE                      SYS-4100-REC
           END-IF
           IF      SYS-4100-KGISKYKBN  =   SPACE
               MOVE    "00"                TO  SYS-4100-KGISKYKBN
           END-IF
           MOVE    SYS-4100-KGISKYKBN  TO  SPA-NAI-4100-KGISKYKBN
      *
      *    労災医療機関情報
           INITIALIZE                      SYS-4001-REC
           MOVE    SPA-HOSPNUM         TO  SYS-4001-HOSPNUM
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               PERFORM UNTIL ( FLG-SYSKANRI        =   1   )
                       OR    ( SPA-NAI-4001-MAX    >=  100 )
                   MOVE    MCPDATA-REC         TO  SYS-4001-REC
                   IF      SYS-4001-EDYUKYMD   <   "20130701"
                       CONTINUE
                   ELSE
                       IF      SYS-4001-RSIIRYOKBN =   "1"
                           MOVE    1           TO
                                           SPA-NAI-4001-RSIIRYOKBN-FLG
                       END-IF
                       ADD     1               TO  SPA-NAI-4001-MAX
                       MOVE    SYS-4001-STYUKYMD
                                               TO  SPA-NAI-4001-STYUKYMD
                                                      (SPA-NAI-4001-MAX)
                       MOVE    SYS-4001-EDYUKYMD
                                               TO  SPA-NAI-4001-EDYUKYMD
                                                      (SPA-NAI-4001-MAX)
                       MOVE    SYS-4001-RSIIRYOKBN
                                               TO
                                               SPA-NAI-4001-RSIIRYOKBN
                                                      (SPA-NAI-4001-MAX)
                   END-IF
                   MOVE    "tbl_syskanri"    TO  MCP-TABLE
                   MOVE    "key8"            TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    医療機関情報（医療機関コード別）
           INITIALIZE                      WRK-1001-TABLE
      *
           INITIALIZE                      SYS-1001-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-IF
      *
           PERFORM     UNTIL   FLG-SYSKANRI    =   1
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
      *??
           DISPLAY "SYS=" SYS-1001-HOSPCD "#" 
                            SYS-1001-STYUKYMD "#"
                                   SYS-1001-EDYUKYMD "#"
      *??
      *
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       10
                   IF      WRK-1001-THOSPCD (IDX)
                                           =   ZERO
                       MOVE    SYS-1001-STYUKYMD
                                           TO  WRK-1001-TSTYUKYMD
                                                               (IDX)
                       MOVE    SYS-1001-EDYUKYMD
                                           TO  WRK-1001-TEDYUKYMD
                                                               (IDX)
                       MOVE    SYS-1001-HOSPCD
                                           TO  WRK-1001-THOSPCD
                                                               (IDX)
                       MOVE    IDX         TO  WRK-1001-MAX
                   END-IF
                   IF      WRK-1001-THOSPCD (IDX)
                                           =   SYS-1001-HOSPCD
                       IF      SYS-1001-STYUKYMD
                                           <   WRK-1001-TSTYUKYMD
                                                               (IDX)
                           MOVE    SYS-1001-STYUKYMD
                                           TO  WRK-1001-TSTYUKYMD
                                                               (IDX)
                       END-IF
                       MOVE    100         TO  IDX
                   END-IF
               END-PERFORM
               MOVE    "tbl_syskanri"    TO  MCP-TABLE
               MOVE    "key8"            TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
                   OR      WRK-1001-THOSPCD (IDX)
                                       =   ZERO
               MOVE    SPACE           TO  WRK-STYUKYMD
                                           WRK-EDYUKYMD
      *        有効終了年月日チェック
               IF      WRK-1001-TEDYUKYMD (IDX)
                                       NOT =   "99999999"
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY7-AREA
                   MOVE    "71"                TO  LNK-DAY7-IRAI
                   MOVE    WRK-1001-TEDYUKYMD (IDX) (1:6)
                                               TO  LNK-DAY7-YM
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY7-AREA
      *            末日以外の時
                   IF      LNK-DAY7-KOYOMI NOT =
                                               WRK-1001-TEDYUKYMD (IDX)
                       MOVE    WRK-1001-TEDYUKYMD (IDX)
                                               TO  WRK-STYUKYMD
                       MOVE    "01"            TO  WRK-STYUKYMD (7:2)
                       MOVE     LNK-DAY7-KOYOMI
                                               TO  WRK-EDYUKYMD
      *                前月の末日設定
                       INITIALIZE                  STS-AREA-DAY
                       INITIALIZE                  LNK-DAY6-AREA
                       MOVE    "61"            TO  LNK-DAY6-IRAI
                       MOVE    WRK-1001-TEDYUKYMD (IDX)
                                               TO  LNK-DAY6-KIJUN
                       MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
                       MOVE    -1              TO  LNK-DAY6-ZOGEN
                       CALL    "ORCSDAY"           USING
                                                   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                       INITIALIZE                  STS-AREA-DAY
                       INITIALIZE                  LNK-DAY7-AREA
                       MOVE    "71"            TO  LNK-DAY7-IRAI
                       MOVE    LNK-DAY6-KEISAN (1:6)
                                               TO  LNK-DAY7-YM
                       CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY7-AREA
                       MOVE    LNK-DAY7-KOYOMI TO
                                               WRK-1001-TEDYUKYMD (IDX)
                   END-IF
               END-IF
      *        有効開始年月日チェック
               IF    ( WRK-1001-TSTYUKYMD (IDX)
                                           =   "00000000" )
               OR    ( WRK-1001-TSTYUKYMD (IDX) (7:2)
                                           =   "01"       )
                   CONTINUE
               ELSE
                   MOVE    WRK-1001-TSTYUKYMD (IDX)
                                           TO  WRK-STYUKYMD
                   MOVE    "01"            TO  WRK-STYUKYMD (7:2)
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY7-AREA
                   MOVE    "71"            TO  LNK-DAY7-IRAI
                   MOVE    WRK-1001-TSTYUKYMD (IDX) (1:6)
                                           TO  LNK-DAY7-YM
                   CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                   MOVE    LNK-DAY7-KOYOMI TO  WRK-EDYUKYMD
      *            次月設定
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"            TO  LNK-DAY6-IRAI
                   MOVE    WRK-1001-TSTYUKYMD (IDX)
                                           TO  LNK-DAY6-KIJUN
                   MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
                   MOVE    1               TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN TO  WRK-1001-TSTYUKYMD (IDX)
                   MOVE    "01"            TO
                                           WRK-1001-TSTYUKYMD (IDX)(7:2)
               END-IF
      *
               IF    ( WRK-1001-TSTYUKYMD (IDX)
                                       >   WRK-1001-TEDYUKYMD (IDX) )
                   CONTINUE
               ELSE
                   PERFORM VARYING IDXX FROM    1   BY  1
                           UNTIL   IDXX >       10
                       IF      SPA-NAI-1001-THOSPCD (IDXX)
                                               =   ZERO
                           MOVE    WRK-1001-TSTYUKYMD (IDX)
                                           TO  SPA-NAI-1001-TSTYUKYMD
                                                               (IDXX)
                           MOVE    WRK-1001-TEDYUKYMD (IDX)
                                           TO  SPA-NAI-1001-TEDYUKYMD
                                                               (IDXX)
                           MOVE    WRK-1001-THOSPCD   (IDX)
                                           TO  SPA-NAI-1001-THOSPCD
                                                               (IDXX)
                           MOVE    IDXX        TO  SPA-NAI-1001-MAX
                       END-IF
                       IF    ( WRK-1001-TSTYUKYMD (IDX)
                                           =   SPA-NAI-1001-TSTYUKYMD
                                                               (IDXX) )
                       AND   ( WRK-1001-TEDYUKYMD (IDX)
                                           =   SPA-NAI-1001-TEDYUKYMD
                                                               (IDXX) )
                           MOVE    10          TO  IDXX
                       END-IF
                   END-PERFORM
               END-IF
      *
               IF      WRK-STYUKYMD    NOT =   SPACE
                   PERFORM VARYING IDXX FROM    1   BY  1
                           UNTIL   IDXX >       10
                     IF      SPA-NAI-1001-THOSPCD (IDXX)
                                           =   ZERO
                       MOVE    WRK-STYUKYMD
                                           TO  SPA-NAI-1001-TSTYUKYMD
                                                               (IDXX)
                       MOVE    WRK-EDYUKYMD
                                           TO  SPA-NAI-1001-TEDYUKYMD
                                                               (IDXX)
                       MOVE    9999999     TO  SPA-NAI-1001-THOSPCD
                                                               (IDXX)
                       MOVE    IDXX        TO  SPA-NAI-1001-MAX
                     END-IF
                     IF    ( WRK-STYUKYMD  =   SPA-NAI-1001-TSTYUKYMD
                                                               (IDXX) )
                     AND   ( WRK-EDYUKYMD  =   SPA-NAI-1001-TEDYUKYMD
                                                               (IDXX) )
                       MOVE    10          TO  IDXX
                     END-IF
                   END-PERFORM
               END-IF    
           END-PERFORM
      *??
           DISPLAY "SPA-NAI-1001-MAX=" SPA-NAI-1001-MAX
      *??
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-NAI-1001-MAX
               MOVE    SPA-NAI-1001-TSTYUKYMD (IDX)
                                           TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG (1:6)   TO  SPA-NAI-1001-TSTYUKYM
                                                                 (IDX)
               MOVE    WRK-HENYMDG3(1:16)  TO  SPA-NAI-1001-TSTYUKYM-G
                                                                 (IDX)
               MOVE    SPA-NAI-1001-TEDYUKYMD (IDX)
                                           TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG (1:6)   TO  SPA-NAI-1001-TEDYUKYM
                                                                 (IDX)
               MOVE    WRK-HENYMDG3(1:16)  TO  SPA-NAI-1001-TEDYUKYM-G
                                                                 (IDX)
           END-PERFORM
      *
      *    クライアント保存可否情報取得サブ
           INITIALIZE                      ORCSCLIENTSVAREA
           CALL    "ORCSCLIENTSV"      USING   ORCSCLIENTSVAREA
                                               SPA-AREA
      *     
           MOVE    ZERO                TO  IDX
           IF      SPA-MW              =   SPA-PANDA
               MOVE    "3"             TO  SPA-GMN-DATAKBNL        (1)
               MOVE    "システム管理設定場所（フロッピィ用）"
                                       TO  SPA-GMN-DATAKBNMEIL     (1)
               MOVE    "4"             TO  SPA-GMN-DATAKBNL        (2)
               MOVE    "システム管理設定場所（ＭＯ用）"
                                       TO  SPA-GMN-DATAKBNMEIL     (2)
               MOVE    2               TO  SPA-GMN-DATAKBN-MAX
                                           IDX
           END-IF
           IF      SCLIENTSV-CLIENT-SAVE
                                   =   "1"
               ADD     1               TO  IDX
               MOVE    "5"             TO  SPA-GMN-DATAKBNL       (IDX)
               MOVE    "クライアント保存"
                                       TO  SPA-GMN-DATAKBNMEIL    (IDX)
               MOVE    IDX             TO  SPA-GMN-DATAKBN-MAX
      *
               IF      SYS-25-RECEDEN-CDRKBN   =   "1"
                   ADD     1               TO  IDX
                   MOVE    "6"             TO  SPA-GMN-DATAKBNL   (IDX)
                   MOVE    "クライアント保存（ＣＤ−Ｒ用）"
                                           TO  SPA-GMN-DATAKBNMEIL(IDX)
                   MOVE    IDX             TO  SPA-GMN-DATAKBN-MAX
               END-IF
           END-IF
      *
           MOVE    SPA-NAI-200501-RECEDEN-DATAKBN
                                   TO  SPA-GMN-DATAKBN
           PERFORM 4104-DATAKBN-CHK-SEC
           IF      SPA-NAI-200501-RECEDEN-DATAKBN
                                   =   "1" OR "2"
               MOVE    SPACE           TO  SPA-GMN-DATAKBN-G
           END-IF
      *
      *    レセプト編集区分情報（主科情報）
           MOVE    SPACE               TO  SYS-2009-REC
           INITIALIZE                      SYS-2009-REC
           MOVE    "2009"              TO  SYS-2009-KANRICD
           MOVE    "*"                 TO  SYS-2009-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-2009-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2009-STYUKYMD (7:2)
           MOVE    SYS-2009-STYUKYMD   TO  SYS-2009-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2009-HOSPNUM
           MOVE    SYS-2009-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2009-REC
           ELSE
               INITIALIZE                  SYS-2009-REC
           END-IF
           IF      SYS-2009-SYUKA-KBN
                                   =   SPACE
               MOVE    "0"             TO  SYS-2009-SYUKA-KBN
           END-IF
           IF      SYS-2009-SYUKA-KBN
                                   =   "0"
               MOVE    "0"             TO  SYS-2009-SYUKA-SYAHO
                                           SYS-2009-SYUKA-KOKUHO
                                           SYS-2009-SYUKA-KOUIKI
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
           MOVE    SYS-2009-SYUKA-SYAHO
                                       TO  SPA-NAI-2009-SYUKA-SYAHO
           MOVE    SYS-2009-SYUKA-KOKUHO
                                       TO  SPA-NAI-2009-SYUKA-KOKUHO
           MOVE    SYS-2009-SYUKA-KOUIKI
                                       TO  SPA-NAI-2009-SYUKA-KOUIKI
      *
      *    ボタン設定処理
           PERFORM 3100-STATE-SET-SEC
      *
      *    ファイル保存情報チェック
           PERFORM 3102-FILE-INFO-CHECK-SEC
      *
      *    診療年月によるシステム管理の取得処理
           PERFORM 3103-SYSKANRI-HENSYU-SEC
      *
      *    診療年月期間設定処理
           PERFORM 3105-SRYYMKBN-HENSYU-SEC
      *
      *    月途中変更の診療年月かチェック
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-SRYYMKBN-MAX
               IF      ( SPA-NAI-SRYYMKBN-ST (IDX)
                                   <=   SPA-NAI-SKYYM )
               AND     ( SPA-NAI-SRYYMKBN-ED (IDX)
                                   >=   SPA-NAI-SKYYM )
                   IF      SPA-NAI-SRYYMKBN-ERR (IDX)
                                       =   "1"
                       MOVE    ZERO    TO  SPA-GMN-SRYYMKBN-MAX
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       10
                           INITIALIZE  SPA-GMN-SRYYMKBNLST (IDX)
                       END-PERFORM
      *****************MOVE    "0051"      TO  SPA-ERRCD
      *
                       MOVE
                   "月途中に医療機関コードが変更されている診療年月です"
                                           TO  SPA-GMN-TITLE2
                   END-IF
                   MOVE    SPA-GMN-SRYYMKBN-MAX
                                           TO  IDX
               END-IF
           END-PERFORM
      *
      *    未実施の未請求解除データのチェック処理
           PERFORM 3101-SYUKA-CHECK-SEC
      *
      *    点検レセ０点公費記載の請求管理のチェック処理
           PERFORM 3104-SEIKYU-CHECK-SEC
      *
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ボタン設定処理
      *****************************************************************
       3100-STATE-SET-SEC            SECTION.
      *
           MOVE    WIDGET-NORMAL       TO  SPA-DATAKBN1-STATE
                                           SPA-DATAKBN3-STATE
                                           SPA-DATAKBN4-STATE
                                           SPA-DATAKBN5-STATE
                                           SPA-DATAKBN6-STATE
                                           SPA-DATAKBN7-STATE
                                           SPA-DATAKBN8-STATE
                                           SPA-B09-STATE
                                           SPA-B10-STATE
                                           SPA-B14-STATE
                                           SPA-B19-STATE
                                           SPA-B20-STATE
                                           SPA-B21-STATE
                                           SPA-B22-STATE
                                           SPA-SYOKBN1-STATE
           MOVE    WIDGET-INSENSITIVE  TO  SPA-B06-STATE
                                           SPA-B07-STATE
                                           SPA-B08-STATE
                                           SPA-B15-STATE
                                           SPA-B16-STATE
                                           SPA-B17-STATE
           MOVE    WIDGET-INSENSITIVE  TO  SPA-DATA-STATE
                                           SPA-NYUGAIKBN-STATE
                                           SPA-PRTKBN-STATE
                                           SPA-RECEDEN-CHK-STATE
      *
           MOVE    "T"                 TO  SPA-SYOKBN1-VALUE  
           MOVE    "F"                 TO  SPA-SYOKBN2-VALUE  
           MOVE    "T"                 TO  SPA-DATAKBN1-VALUE  
           MOVE    "T"                 TO  SPA-DATAKBN3-VALUE  
           MOVE    "T"                 TO  SPA-DATAKBN4-VALUE  
           MOVE    "T"                 TO  SPA-DATAKBN5-VALUE  
           MOVE    "T"                 TO  SPA-DATAKBN6-VALUE  
           MOVE    "F"                 TO  SPA-DATAKBN7-VALUE
           MOVE    "F"                 TO  SPA-DATAKBN8-VALUE
      *
           MOVE    "T"                 TO  SPA-CHK11-VALUE
                                           SPA-CHK12-VALUE
                                           SPA-CHK21-VALUE
                                           SPA-CHK22-VALUE
                                           SPA-CHK31-VALUE
                                           SPA-CHK32-VALUE
      *    レセ電総括表・国保は選択不可
           MOVE    "F"                 TO  SPA-DATAKBN2-VALUE  
           MOVE    WIDGET-INSENSITIVE  TO  SPA-DATAKBN2-STATE
      *
           IF      SPA-NAI-2005-SYARECEKBN 
                                       =   "1" OR  "2" OR  "3"
               MOVE    WIDGET-NORMAL       TO  SPA-B06-STATE
                                               SPA-B15-STATE
                                               SPA-B16-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  SPA-B06-STATE
           END-IF
           IF      SPA-NAI-2005-KKHRECEKBN 
                                       =   "1" OR  "2" OR  "3"
               MOVE    WIDGET-NORMAL       TO  SPA-B07-STATE
                                               SPA-B15-STATE
                                               SPA-B16-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  SPA-B07-STATE
               MOVE    WIDGET-INSENSITIVE  TO  SPA-DATAKBN6-STATE
               MOVE    "F"                 TO  SPA-DATAKBN6-VALUE  
           END-IF
           IF      SPA-NAI-200501-KOUIKIRECEKBN
                                       =   "1" OR  "2" OR  "3"
               IF      SPA-NAI-200501-KOUIKIKBN
                                           =   "0"
                   MOVE    WIDGET-NORMAL       TO  SPA-B08-STATE
                                                   SPA-B15-STATE
                                                   SPA-B16-STATE
               ELSE
                   MOVE    WIDGET-INSENSITIVE  TO  SPA-B08-STATE
               END-IF
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  SPA-B08-STATE
           END-IF
      *
           IF    ( SPA-NAI-2005-SYARECEKBN
                                       =   "1" OR  "2" OR  "3" )
           OR    ( SPA-NAI-2005-KKHRECEKBN
                                       =   "1" OR  "2" OR  "3" )
           OR    ( SPA-NAI-200501-KOUIKIRECEKBN
                                       =   "1" OR  "2" OR  "3" )
               MOVE    WIDGET-NORMAL       TO  SPA-DATA-STATE
                                               SPA-NYUGAIKBN-STATE
                                               SPA-PRTKBN-STATE
                                               SPA-RECEDEN-CHK-STATE
           END-IF
      *
           IF    ( SPA-NAI-200501-ROURECEKBN
                                       =   "1" OR  "2" OR  "3"   )
           AND   ( SPA-NAI-SKYYM       >=  WRK-CONS-SRYYM-201307 )
           AND   ( SPA-NAI-4001-RSIIRYOKBN-FLG
                                       =   1                     )
               MOVE    SPA-NAI-SKYYM       TO  WRK-SYS-SRYYMD (1:6)
               MOVE    "01"                TO  WRK-SYS-SRYYMD (7:2)
      *
               PERFORM VARYING IDZZ    FROM    1   BY  1
                       UNTIL   IDZZ    >  SPA-NAI-4001-MAX
                   IF      SPA-NAI-4001-RSIIRYOKBN (IDZZ)
                                               =   "1"
                       IF    ( SPA-NAI-4001-STYUKYMD (IDZZ)
                                                   <=  WRK-SYS-SRYYMD )
                       AND   ( SPA-NAI-4001-EDYUKYMD (IDZZ)
                                                   >=  WRK-SYS-SRYYMD )
      *                    該当診療年月が労災指定医療機関のとき
                           MOVE    WIDGET-NORMAL       TO
                                                   SPA-B17-STATE
                                                   SPA-DATA-STATE
                                                   SPA-NYUGAIKBN-STATE
                                                   SPA-PRTKBN-STATE
                           MOVE     SPA-NAI-4001-MAX   TO  IDZZ
                       ELSE
                           IF      SPA-NAI-4001-EDYUKYMD (IDZZ)
                                                   <   WRK-SYS-SRYYMD
      *                        該当診療年月以前が労災指定医療機関のとき
                               MOVE    WIDGET-NORMAL       TO
                                                   SPA-B17-STATE
                                                   SPA-DATA-STATE
                                                   SPA-NYUGAIKBN-STATE
                                                   SPA-PRTKBN-STATE
                               MOVE     SPA-NAI-4001-MAX   TO  IDZZ
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      SPA-SYUKA-KBN      =   "0"
               MOVE    WIDGET-INSENSITIVE  TO  SPA-B20-STATE
           ELSE
               IF    ( SYS-2009-SYUKA-SYAHO  =   "1" )
               OR    ( SYS-2009-SYUKA-KOKUHO =   "1" )
               OR    ( SYS-2009-SYUKA-KOUIKI =   "1" )
                   MOVE    WIDGET-NORMAL       TO  SPA-B20-STATE
               ELSE
                   MOVE    WIDGET-INSENSITIVE  TO  SPA-B20-STATE
               END-IF
           END-IF
      *
      *    クライアント保存可否情報とシステム管理の設定より
      *    自賠責ＰＤＦファイル作成のボタン設定
           IF    ( SPA-NAI-200501-JIBAI-PDF
                                       =   "0" )
           OR    ( SCLIENTSV-CLIENT-SAVE
                                   NOT =   "1" )
               MOVE    WIDGET-INSENSITIVE  TO  SPA-B14-STATE
           END-IF
           .
       3100-STATE-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    未実施の未請求解除データのチェック処理
      *****************************************************************
       3101-SYUKA-CHECK-SEC            SECTION.
      *
           INITIALIZE                  SYUKA-REC
grpsys     MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
           MOVE    "%"             TO  SYUKA-NYUGAIKBN
           MOVE    "YYYY"          TO  SYUKA-RECESYUBETU
           MOVE    "02"            TO  SYUKA-MISEIKYU-FLG
           MOVE    SPA-NAI-SKYYM   TO  SYUKA-SRYYM
           MOVE    SYUKA-REC       TO  MCPDATA-REC
           PERFORM 900-SYUKA-READ-SEC
           IF      FLG-SYUKA       =   ZERO
               MOVE    "未実施の未請求解除データがあります"
                                       TO  SPA-GMN-TITLE1
           ELSE
               MOVE    SPACE           TO  SPA-GMN-TITLE1
           END-IF
      *
           .
       3101-SYUKA-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル保存情報チェック処理
      *****************************************************************
       3102-FILE-INFO-CHECK-SEC            SECTION.
      *
           MOVE    WIDGET-INSENSITIVE  TO  SPA-B19-STATE
           IF      SPA-NAI-SKYYM       =   ZERO
               CONTINUE
           ELSE
               INITIALIZE                  ORCSFILESVAREA
               MOVE    "R"                 TO  SFILESV-MODE
               MOVE    WRK-CONS-JOB-SHELLID
                                           TO  SFILESV-TBL-KEY
               MOVE    SPA-NAI-SKYYM       TO  SFILESV-SRYYM (1)
               CALL    "ORCSFILESV"        USING
                                               ORCSFILESVAREA
                                               SPA-AREA
               IF      SFILESV-RETURN      =   ZERO         
                   PERFORM VARYING IDXY    FROM    1   BY  1
                           UNTIL   IDXY    >       100
                           OR      SFILESV-SHELLID (IDXY)
                                           =       SPACE
                       IF      SFILESV-SHELLID (IDXY)
                                           =   WRK-CONS-BTPARA-SCRIPTID
                           CONTINUE
                       ELSE
                           MOVE    WIDGET-NORMAL   TO  SPA-B19-STATE
                                                       SPA-DATA-STATE
                           MOVE    100             TO  IDXY
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       3102-FILE-INFO-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月によるシステム管理の取得処理
      *****************************************************************
       3103-SYSKANRI-HENSYU-SEC    SECTION.
      *
           INITIALIZE                      SPA-3004-AREA
                                           SPA-T02-AREA
      *
      *    医療機関ＩＤ編集
           INITIALIZE                  SYS-1001-REC
           MOVE    SPA-HOSPNUM        TO  SYS-1001-HOSPNUM
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-1001-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD (7:2)
           MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE
               INITIALIZE                      SYS-1001-REC
           END-IF
           MOVE    SYS-1001-PREFNUM    TO  SPA-NAI-1001-PREFNUM
           MOVE    SYS-1001-HOSPCD     TO  SPA-NAI-1001-HOSPCD
      *
      *    標準総括表印刷指示
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-3003-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-3003-STYUKYMD (7:2)
           MOVE    SYS-3003-STYUKYMD   TO  SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
           ELSE
               INITIALIZE                      SYS-3003-REC
           END-IF
           MOVE    SYS-3003-PRTKBN1    TO  SPA-NAI-PRTKBN1        
           IF      SYS-3003-PRTKBN1    =   SPACE
               MOVE    "有"                TO  SPA-GMN-PRTKBN1-VALUE
               MOVE   "blue"               TO  SPA-GMN-PRTKBN1-STYLE
           ELSE
               MOVE    "無"                TO  SPA-GMN-PRTKBN1-VALUE
               MOVE   "red"                TO  SPA-GMN-PRTKBN1-STYLE
           END-IF      
           MOVE    SYS-3003-PRTKBN2    TO  SPA-NAI-PRTKBN2        
           IF      SYS-3003-PRTKBN2    =   SPACE
               MOVE    "有"                TO  SPA-GMN-PRTKBN2-VALUE
               MOVE   "blue"               TO  SPA-GMN-PRTKBN2-STYLE
           ELSE
               MOVE    "無"                TO  SPA-GMN-PRTKBN2-VALUE
               MOVE   "red"                TO  SPA-GMN-PRTKBN2-STYLE
           END-IF
           MOVE    SYS-3003-PRTKBN3    TO  SPA-NAI-PRTKBN3        
           IF      SYS-3003-PRTKBN3    =   SPACE
               MOVE    "無"                TO  SPA-GMN-PRTKBN3-VALUE
               MOVE   "red"                TO  SPA-GMN-PRTKBN3-STYLE
               MOVE    WIDGET-INSENSITIVE  TO  SPA-DATAKBN6-STATE
               MOVE    "F"                 TO  SPA-DATAKBN6-VALUE  
           ELSE
               MOVE    "有"                TO  SPA-GMN-PRTKBN3-VALUE
               MOVE   "blue"               TO  SPA-GMN-PRTKBN3-STYLE
           END-IF
           MOVE    SYS-3003-PRTKBN4    TO  SPA-NAI-PRTKBN4        
           IF      SYS-3003-PRTKBN4    =   SPACE
               MOVE    "有"                TO  SPA-GMN-PRTKBN4-VALUE
               MOVE   "blue"               TO  SPA-GMN-PRTKBN4-STYLE
           ELSE
               MOVE    "無"                TO  SPA-GMN-PRTKBN4-VALUE
               MOVE   "red"                TO  SPA-GMN-PRTKBN4-STYLE
           END-IF
           MOVE    SYS-3003-PRTKBN5   TO  SPA-NAI-PRTKBN5        
           IF      SYS-3003-PRTKBN5    =   SPACE
               MOVE    "無"                TO  SPA-GMN-PRTKBN5-VALUE
               MOVE   "red"                TO  SPA-GMN-PRTKBN5-STYLE
               MOVE    WIDGET-INSENSITIVE  TO  SPA-DATAKBN2-STATE
               MOVE    "F"                 TO  SPA-DATAKBN2-VALUE  
           ELSE
               MOVE    "有"                TO  SPA-GMN-PRTKBN5-VALUE
               MOVE   "blue"               TO  SPA-GMN-PRTKBN5-STYLE
           END-IF
      *
           MOVE    ZERO                TO  IDX
                                           IDY
                                           IDZ
      *    プリンタ指定画面編集（標準総括）
           IF      SYS-3003-PRTKBN1    =   SPACE
               ADD     1               TO  IDX
               MOVE    900             TO  SPA-3004-KBNCD   (IDX)
               MOVE    "recept3.sh"    TO  SPA-3004-SHELLID (IDX) 
               MOVE    "社保・総括表（標準）"
                                       TO  SPA-3004-SHORIMSG
                                                            (IDX)
               MOVE    SPA-NAI-1031-PRTNM
                                       TO  SPA-3004-PRTNM   (IDX)
           END-IF    
           IF      SYS-3003-PRTKBN2    =   SPACE
               ADD     1               TO  IDX
               MOVE    901             TO  SPA-3004-KBNCD   (IDX)
               MOVE    "recept3.sh"    TO  SPA-3004-SHELLID (IDX) 
               MOVE    "国保・総括表（標準）"
                                       TO  SPA-3004-SHORIMSG
                                                            (IDX)
               MOVE    SPA-NAI-1031-PRTNM
                                       TO  SPA-3004-PRTNM   (IDX)
           END-IF    
           IF      SYS-3003-PRTKBN3    =   SPACE
               CONTINUE
           ELSE
               ADD     1               TO  IDX
               MOVE    902             TO  SPA-3004-KBNCD   (IDX)
               MOVE    "receden_sokatu.sh"
                                       TO  SPA-3004-SHELLID (IDX) 
               MOVE    "国保・レセ電総括表（標準）"
                                       TO  SPA-3004-SHORIMSG
                                                            (IDX)
               MOVE    SPA-NAI-1031-PRTNM
                                       TO  SPA-3004-PRTNM   (IDX)
           END-IF
      *    ★
      *****IF      SPA-NAI-SKYYM       >=  200804
               IF      SYS-3003-PRTKBN4    =   SPACE
                   ADD     1               TO  IDX
                   MOVE    904             TO  SPA-3004-KBNCD   (IDX)
                   MOVE    "recept3.sh"    TO  SPA-3004-SHELLID (IDX) 
                   MOVE    "広域・総括表（標準）"
                                           TO  SPA-3004-SHORIMSG
                                                                (IDX)
                   MOVE    SPA-NAI-1031-PRTNM
                                           TO  SPA-3004-PRTNM   (IDX)
               END-IF    
               IF      SYS-3003-PRTKBN5    =   SPACE
                   CONTINUE
               ELSE
                   ADD     1               TO  IDX
                   MOVE    905             TO  SPA-3004-KBNCD   (IDX)
                   MOVE    "receden_sokatu.sh"
                                           TO  SPA-3004-SHELLID (IDX) 
                   MOVE    "広域・レセ電総括表（標準）"
                                           TO  SPA-3004-SHORIMSG
                                                                (IDX)
                   MOVE    SPA-NAI-1031-PRTNM
                                           TO  SPA-3004-PRTNM   (IDX)
               END-IF
      *****END-IF
      *    
           ADD     1               TO  IDX
           MOVE    903             TO  SPA-3004-KBNCD   (IDX)
           MOVE    "recept16.sh"   TO  SPA-3004-SHELLID (IDX) 
           MOVE    "公害・総括表（標準）"
                                   TO  SPA-3004-SHORIMSG
                                                        (IDX)
           MOVE    SPA-NAI-1031-PRTNM
                                   TO  SPA-3004-PRTNM   (IDX)
      *    
           ADD     1               TO  IDX
           MOVE    906             TO  SPA-3004-KBNCD   (IDX)
           MOVE    "recept17.sh"   TO  SPA-3004-SHELLID (IDX) 
           MOVE    "労災・総括表（標準）"
                                   TO  SPA-3004-SHORIMSG
                                                        (IDX)
           MOVE    SPA-NAI-1031-PRTNM
                                   TO  SPA-3004-PRTNM   (IDX)
      *
      *    県単独事業
           MOVE    ZERO                TO  FLG-3004-PARAKBN
      *
           MOVE    SPACE               TO  SYS-3004-REC 
           INITIALIZE                      SYS-3004-REC
           MOVE    "3004"              TO  SYS-3004-KANRICD
           MOVE    "*"                 TO  SYS-3004-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-3004-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-3004-STYUKYMD (7:2)
           MOVE    SYS-3004-STYUKYMD   TO  SYS-3004-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3004-HOSPNUM
           MOVE    SYS-3004-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM         UNTIL   FLG-SYSKANRI    =   1
                           OR      IDX             =   WRK-CONS-3004-MAX
               MOVE    MCPDATA-REC     TO  SYS-3004-REC
               IF    ( SYS-3004-SHELLID    =   SPACE   )
               OR    ( SYS-3004-SHORIMSG   =   SPACE   )
               OR    ( SYS-3004-CHK        =   "FFFF"  )
                   CONTINUE
               ELSE
                   ADD     1               TO  IDX
                   MOVE    SYS-3004-KBNCD  (1:4)
                                           TO  WRK-KBNCD
                   MOVE    WRK-KBNCD       TO  SPA-3004-KBNCD   (IDX)
                   MOVE    SYS-3004-SHELLID
                                           TO  SPA-3004-SHELLID (IDX) 
                   MOVE    SYS-3004-CHK    TO  SPA-3004-CHK     (IDX) 
                   MOVE    SYS-3004-SHORIMSG
                                           TO  SPA-3004-SHORIMSG
                                                                (IDX)
                   MOVE    SYS-3004-NAME   TO  SPA-3004-NAME    (IDX)
                   IF      SYS-3004-PARAKBN    =   SPACE
                       MOVE    "0"             TO  SYS-3004-PARAKBN
                   END-IF
                   MOVE    SYS-3004-PARAKBN
                                           TO  SPA-3004-PARAKBN (IDX)
                   MOVE    SYS-3004-PRTNM  TO  SPA-3004-PRTNM   (IDX)
                   IF      SPA-3004-PRTNM (IDX)    =   SPACE
                       MOVE    SPA-NAI-1031-PRTNM  TO
                                               SPA-3004-PRTNM   (IDX)
                   END-IF
                   MOVE    SPA-3004-PRTNM   (IDX)
                                           TO  SPA-3004-PRTNM-BACK
                                                                (IDX)
      *
      *            公費個別設定画面編集
                   IF      SPA-3004-CHKFLG (IDX 3) =   "T"
                       IF      SYS-3004-PARAKBN   =   SPACE
                           MOVE    1       TO  FLG-3004-PARAKBN 
                       END-IF
      *
      *****************IF    ( SPA-NAI-SRYYM      =   ZERO  )
      *****************AND   ( SYS-3004-PARAKBN   =   SPACE )
                       IF      SYS-3004-PARAKBN   =   SPACE
                           CONTINUE
                       ELSE
                           ADD     1       TO  IDY
                           MOVE    IDY     TO  SPA-GMN-T02-TRENNUM1(IDY)
                           MOVE    SPA-3004-KBNCD   (IDX)
                                           TO  SPA-NAI-T02-TKBNCD  (IDY)
                           MOVE    SYS-3004-SHELLID
                                           TO  SPA-NAI-T02-TSHELLID(IDY)
                           MOVE    SYS-3004-SHORIMSG
                                           TO  SPA-GMN-T02-TSHORIMSG
                                                                   (IDY)
                           MOVE    SYS-3004-NAME
                                           TO  SPA-NAI-T02-TNAME   (IDY)
                           MOVE    SYS-3004-PARAKBN
                                           TO  SPA-NAI-T02-TPARAKBN(IDY)
                           MOVE    SPA-3004-PRTNM (IDX)
                                           TO  SPA-NAI-T02-TPRTNM  (IDY)  
                           EVALUATE    SPA-NAI-T02-TPARAKBN(IDY)
                               WHEN    "1"
                                   MOVE    "保険者番号"    TO
                                               SPA-GMN-T02-TPARAKBN(IDY)
                               WHEN    "2"
                                   MOVE    "患者番号"      TO
                                               SPA-GMN-T02-TPARAKBN(IDY)
                           END-EVALUATE
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *     
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    IDX                 TO  SPA-3004-MAX
           MOVE    IDY                 TO  SPA-GMN-T02-SHELLID-MAX
      *
           MOVE    "0"             TO  SPA-GMN-T02-SYORIFLGL   (1)
           MOVE    "処理しない"    TO  SPA-GMN-T02-SYORIFLGMEIL(1)
           MOVE    "1"             TO  SPA-GMN-T02-SYORIFLGL   (2)
           MOVE    "処理する"      TO  SPA-GMN-T02-SYORIFLGMEIL(2)
           MOVE    2               TO  SPA-GMN-T02-SYORIFLG-MAX
      *
           .
       3103-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    点検レセ０点公費記載の請求管理のチェック処理
      *****************************************************************
       3104-SEIKYU-CHECK-SEC            SECTION.
      *
           INITIALIZE                      SPA-NAI-RECEDEN-CHK
      *
      *    医保分のレセ電データを作成しないとき
           IF    ( SYS-2005-SYARECEKBN =   "1" OR  "2" OR  "3" )
           OR    ( SYS-2005-KKHRECEKBN =   "1" OR  "2" OR  "3" )
           OR    ( SYS-25-KOUIKIRECEKBN
                                       =   "1" OR  "2" OR  "3" )
               CONTINUE
           ELSE
               GO  TO  3104-SEIKYU-CHECK-EXT
           END-IF
      *
      *
      *****社保分チェック
      *     MOVE    SPACE               TO  RECE10-REC
      *    INITIALIZE                      RECE10-REC
      *     MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
      *     MOVE    SPA-NAI-SKYYM       TO  RECE10-SKYYM
      *     MOVE    "%"                 TO  RECE10-NYUGAIKBN
      *     MOVE    1                   TO  RECE10-TEISYUTUSAKI
      *     MOVE    "key78"             TO  WRK-MCP-PATHNAME
      *     PERFORM 900-SEIKYU-READ-SEC
      *
      *     IF      FLG-RECE10          =   ZERO
      *         MOVE    "1"                 TO  SPA-NAI-RECEDEN-CHK1
      *     END-IF
      *
      *    国保分チェック
      *     MOVE    SPACE               TO  RECE10-REC
      *     INITIALIZE                      RECE10-REC
      *     MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
      *     MOVE    SPA-NAI-SKYYM       TO  RECE10-SKYYM
      *     MOVE    "%"                 TO  RECE10-NYUGAIKBN
      *     MOVE    2                   TO  RECE10-TEISYUTUSAKI
      *     MOVE    "key78"             TO  WRK-MCP-PATHNAME
      *     PERFORM 900-SEIKYU-READ-SEC
      *
      *     IF      FLG-RECE10          =   ZERO
      *         MOVE    "1"                 TO  SPA-NAI-RECEDEN-CHK2
      *     END-IF
      *
      *    広域分チェック
      *     MOVE    SPACE               TO  RECE10-REC
      *     INITIALIZE                      RECE10-REC
      *     MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
      *     MOVE    SPA-NAI-SKYYM       TO  RECE10-SKYYM
      *     MOVE    "%"                 TO  RECE10-NYUGAIKBN
      *     MOVE    6                   TO  RECE10-TEISYUTUSAKI
      *     MOVE    "key78"             TO  WRK-MCP-PATHNAME
      *     PERFORM 900-SEIKYU-READ-SEC
      *
      *     IF      FLG-RECE10          =   ZERO
      *         MOVE    "1"                 TO  SPA-NAI-RECEDEN-CHK3
      ******END-IF
      *
      *    ０点公費の有無チェック
           MOVE    SPACE               TO  RECE10-REC
           INITIALIZE                      RECE10-REC
           MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
           MOVE    SPA-NAI-SKYYM       TO  RECE10-SKYYM
           MOVE    "%"                 TO  RECE10-NYUGAIKBN
           MOVE    "key79"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SEIKYU-READ-SEC
      *
           IF      FLG-RECE10          =   ZERO
               MOVE    "111"               TO  SPA-NAI-RECEDEN-CHK
           END-IF
      *
           IF      SPA-NAI-RECEDEN-CHK  =   SPACE
               MOVE    "1"                 TO  SPA-GMN-PRTKBNL       (1)
               MOVE    "提出用"            TO  SPA-GMN-PRTKBNMEIL    (1)
               MOVE    "2"                 TO  SPA-GMN-PRTKBNL       (2)
               MOVE    "点検用（院外処方含）"
                                           TO  SPA-GMN-PRTKBNMEIL    (2)
               MOVE    2                   TO  SPA-GMN-PRTKBN-MAX
               IF      SPA-NAI-9101-COOPBASE
                                           =   "1" OR  "2"
                   MOVE    "6"                 TO  SPA-GMN-PRTKBNL   (3)
                   MOVE    "連携基盤用"        TO  SPA-GMN-PRTKBNMEIL(3)
                   MOVE    "7"                 TO  SPA-GMN-PRTKBNL   (4)
                   MOVE    "連携基盤用（当日分まで）"
                                               TO  SPA-GMN-PRTKBNMEIL(4)
                   MOVE    4                   TO  SPA-GMN-PRTKBN-MAX
               END-IF
               IF      SPA-NAI-9102-EFOUT  =   "1"
                   ADD     1                   TO  SPA-GMN-PRTKBN-MAX
                   MOVE    "8"                 TO  SPA-GMN-PRTKBNL
                                                   (SPA-GMN-PRTKBN-MAX)
                   MOVE    "ＥＦファイル"      TO  SPA-GMN-PRTKBNMEIL
                                                   (SPA-GMN-PRTKBN-MAX)
                   IF      SPA-BEDSU           >   ZERO
                       ADD     1               TO  SPA-GMN-PRTKBN-MAX
                       MOVE    "9"             TO  SPA-GMN-PRTKBNL
                                                   (SPA-GMN-PRTKBN-MAX)
                       MOVE    "様式４"        TO  SPA-GMN-PRTKBNMEIL
                                                   (SPA-GMN-PRTKBN-MAX)
                   END-IF
               END-IF
      *
               MOVE    SPACE               TO  SPA-GMN-TITLE
      *
               MOVE    WIDGET-NORMAL       TO  SPA-SYOKBN1-STATE
           ELSE
               MOVE    "2"                 TO  SPA-GMN-PRTKBNL       (1)
               MOVE    "点検用（院外処方含）"
                                           TO  SPA-GMN-PRTKBNMEIL    (1)
               MOVE    1                   TO  SPA-GMN-PRTKBN-MAX
               IF      SPA-NAI-9101-COOPBASE
                                           =   "1" OR  "2"
                   MOVE    "6"                 TO  SPA-GMN-PRTKBNL   (2)
                   MOVE    "連携基盤用"        TO  SPA-GMN-PRTKBNMEIL(2)
                   MOVE    "7"                 TO  SPA-GMN-PRTKBNL   (3)
                   MOVE    "連携基盤用（当日分まで）"
                                               TO  SPA-GMN-PRTKBNMEIL(3)
                   MOVE    3                   TO  SPA-GMN-PRTKBN-MAX
               END-IF
               IF      SPA-NAI-9102-EFOUT  =   "1"
                   ADD     1                   TO  SPA-GMN-PRTKBN-MAX
                   MOVE    "8"                 TO  SPA-GMN-PRTKBNL
                                                   (SPA-GMN-PRTKBN-MAX)
                   MOVE    "ＥＦファイル"      TO  SPA-GMN-PRTKBNMEIL
                                                   (SPA-GMN-PRTKBN-MAX)
                   IF      SPA-BEDSU           >   ZERO
                       ADD     1               TO  SPA-GMN-PRTKBN-MAX
                       MOVE    "9"             TO  SPA-GMN-PRTKBNL
                                                   (SPA-GMN-PRTKBN-MAX)
                       MOVE    "様式４"        TO  SPA-GMN-PRTKBNMEIL
                                                   (SPA-GMN-PRTKBN-MAX)
                   END-IF
               END-IF
      *
               STRING  "該当の診療年月に０点公費記載ありの点検用レセ"
                                               DELIMITED  BY  SIZE
                       "プトがあるため、医保分は点検用レセ電データの"
                                               DELIMITED  BY  SIZE
                       "作成のみ可能です"      DELIMITED  BY  SIZE
                                               INTO  SPA-GMN-TITLE
               END-STRING
      *
               MOVE    WIDGET-INSENSITIVE  TO  SPA-SYOKBN1-STATE
                                               SPA-DATAKBN1-STATE
                                               SPA-DATAKBN3-STATE
                                               SPA-DATAKBN4-STATE
                                               SPA-DATAKBN5-STATE
                                               SPA-DATAKBN6-STATE
                                               SPA-B09-STATE
                                               SPA-B10-STATE
                                               SPA-B21-STATE
                                               SPA-B22-STATE
               MOVE    "F"                 TO  SPA-SYOKBN1-VALUE
                                               SPA-DATAKBN1-VALUE
                                               SPA-DATAKBN3-VALUE
                                               SPA-DATAKBN4-VALUE
                                               SPA-DATAKBN5-VALUE
                                               SPA-DATAKBN6-VALUE
           END-IF
      *
           MOVE    SPA-GMN-PRTKBNLST (1)
                                       TO  SPA-GMN-PRTKBN-G
           .
       3104-SEIKYU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月期間設定処理
      *****************************************************************
       3105-SRYYMKBN-HENSYU-SEC        SECTION.
      *
           INITIALIZE                      SPA-GMN-SRYYMKBN-G
           MOVE    ZERO                TO  SPA-GMN-SRYYMKBN-MAX
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               INITIALIZE      SPA-GMN-SRYYMKBNLST (IDX)
                               SPA-NAI-SRYYMKBNLST (IDX)
           END-PERFORM
      *
      *??? IF      SPA-NAI-SKYYM   <   201604
      *???     GO  TO  3105-SRYYMKBN-HENSYU-EXT
      *??? END-IF
      *
           IF      SPA-GMN-PRTKBN      =   "1" OR  "2"
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-NAI-1001-MAX
                 IF      SPA-NAI-1001-TSTYUKYMD (IDX) (1:6)
                                       >   SPA-NAI-SKYYM
                   CONTINUE
                 ELSE
                   ADD     1               TO  SPA-GMN-SRYYMKBN-MAX
                   INITIALIZE                  SPA-GMN-SRYYMKBNLST
                                               (SPA-GMN-SRYYMKBN-MAX)
                                               SPA-NAI-SRYYMKBNLST
                                               (SPA-GMN-SRYYMKBN-MAX)
                   MOVE    SPA-GMN-SRYYMKBN-MAX
                                           TO  WRK-WRNO
                   MOVE    WRK-WRNO-X      TO  SPA-GMN-SRYYMKBNL
                                               (SPA-GMN-SRYYMKBN-MAX)
      *
                   MOVE    SPACE           TO  WRK-SRYYMKBN
                   MOVE    SPA-NAI-1001-TSTYUKYMD (IDX) (1:6)
                                           TO  SPA-NAI-SRYYMKBN-ST
                                               (SPA-GMN-SRYYMKBN-MAX)
                   MOVE    SPA-NAI-1001-TSTYUKYM-G (IDX) 
                                           TO  SPA-NAI-SRYYMKBN-ST-PRT
                                               (SPA-GMN-SRYYMKBN-MAX)
                   MOVE    SPA-NAI-1001-TSTYUKYM (IDX) 
                                           TO  WRK-SRYYMKBN-ST
      *
                   MOVE    SPA-NAI-1001-TEDYUKYMD (IDX) (1:6)
                                           TO  SPA-NAI-SRYYMKBN-ED
                                               (SPA-GMN-SRYYMKBN-MAX)
                   MOVE    SPA-NAI-1001-TEDYUKYM-G (IDX)
                                           TO  SPA-NAI-SRYYMKBN-ED-PRT
                                               (SPA-GMN-SRYYMKBN-MAX)
                   MOVE    SPA-NAI-1001-TEDYUKYM (IDX)
                                           TO  WRK-SRYYMKBN-ED
                   MOVE    "-"             TO  WRK-SRYYMKBN-FL
                   IF      SPA-NAI-1001-THOSPCD  (IDX)
                                           =   9999999
                       MOVE    "*"             TO  WRK-SRYYMKBN-ERR
                       MOVE    "1"             TO  SPA-NAI-SRYYMKBN-ERR
                                               (SPA-GMN-SRYYMKBN-MAX)
                   END-IF
                   MOVE    WRK-SRYYMKBN    TO  SPA-GMN-SRYYMKBNNML
                                               (SPA-GMN-SRYYMKBN-MAX)
                 END-IF
               END-PERFORM
               IF      SPA-GMN-SRYYMKBN-MAX    >   1
                   PERFORM VARYING IDX     FROM    1   BY  1
                           UNTIL   IDX     >       SPA-GMN-SRYYMKBN-MAX
                       IF      SPA-NAI-SRYYMKBN-ERR (IDX)
                                               =   SPACE
                           MOVE    SPA-GMN-SRYYMKBNLST (IDX)
                                               TO  SPA-GMN-SRYYMKBN-G
                           MOVE    SPA-GMN-SRYYMKBN-MAX
                                               TO  IDX
                       END-IF
                   END-PERFORM
               ELSE
                   MOVE    ZERO                TO  SPA-GMN-SRYYMKBN-MAX
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       10
                       INITIALIZE      SPA-GMN-SRYYMKBNLST (IDX)
                   END-PERFORM
               END-IF
           END-IF
           .
       3105-SRYYMKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理結果画面（Ｔ９９）戻り処理
      *****************************************************************
       320-T99-SET-SEC            SECTION.
      *
      *    ＣＳＶ一覧から遷移したとき再度一覧を表示する
           IF      SPA-T99-MOTOPG  =   "T04"
               INITIALIZE                  SPA-T01KYOUTU
               PERFORM 480-CSV-SYORI-SEC
               GO  TO  320-T99-SET-EXT
           END-IF
      *
           IF      SPA-T99-FLG     =   "OK"
               PERFORM 3203-NEXT-GMN-SEC
               GO  TO  320-T99-SET-EXT
           END-IF
      *
      *    公費ＣＳＶボタンの状態設定
           PERFORM 3102-FILE-INFO-CHECK-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           INITIALIZE                  SPA-T01KYOUTU
           .
       320-T99-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電処理後の印刷処理
      *****************************************************************
       3202-PRINT-SEC            SECTION.
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *     
           MOVE    WRK-CONS-FILE-ID    TO  ERRFLPARA-FILE-ID
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    SPACE               TO  RECEERR
           STRING  MCP-TEMPDIR         DELIMITED   BY  SPACE
                   "/"                 DELIMITED   BY  SIZE
                   ERRFLPARA-BASENAME  DELIMITED   BY  SPACE
                                       INTO        RECEERR
           END-STRING
      *      
           MOVE    SPA-LNK-PRTKANRI-AREA
                                   TO  LNK-PRTKANRI-AREA
      *
           PERFORM 45013-PRINT-SYORI-SEC
      *
      *    ＪＯＢＩＤ更新処理
      *
           PERFORM 45012-JOBID-UPD-SEC
      *
      *    印刷データ削除処理
           PERFORM 45015-PRTDATA-DEL-SEC
      *
           MOVE    9               TO  SPA-T01-SYORIFLG
      *     
      *    情報確認画面を表示                                                   
           PERFORM 490-KAKUNIN-SEC
      *
           .
       3202-PRINT-EXT.
           EXIT. 
      *
      *****************************************************************
      *    プレビュー処理
      *****************************************************************
       3203-NEXT-GMN-SEC            SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-T01KYOUTU       TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-T01FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "T01"               TO  SPA-MOTOPG
           MOVE    "XC01"              TO  SPA-SAKIPG
      *
           INITIALIZE                  LNK-XC01-AREA
      *
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
                                           LNK-XC01-TBL-KEY
           MOVE    ZERO                TO  LNK-XC01-RENNUM
           MOVE    SPA-T01-TBL-GROUP   TO  LNK-XC01-TBL-GROUP
           MOVE    ZERO                TO  LNK-XC01-SHORI-RENNUM
           MOVE    0                   TO  LNK-XC01-ST-PAGE
           MOVE    0                   TO  LNK-XC01-ED-PAGE
           MOVE    "T01"               TO  LNK-XC01-MOTOPG
           MOVE    SPACE               TO  LNK-XC01-MODE
      *
           MOVE    LNK-XC01-AREA       TO  SPA-WORK
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   "XC01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       3203-NEXT-GMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-TID1-SET-SEC            SECTION.
      *
           IF      SPA-TID1-FLG    =   "OK"
               EVALUATE    SPA-TIDCD
                   WHEN    "6666"
                       MOVE    "JOIN"          TO  MCP-PUTTYPE
                       PERFORM 210-BACK-SYORI-SEC
                   WHEN    "1012"
                   WHEN    "1013"
                       PERFORM 480-PRINT-SEC
                   WHEN    "2002"
                       MOVE    "2004"          TO  SPA-TIDCD
                       GO  TO  330-TID1-SET-EXT
                   WHEN    "1034"
                       PERFORM 4501-JOBKANRI-CHK-SEC
                       IF      SPA-ERRCD      =   SPACE
                           PERFORM 480-JIBAI-PDF-SEC
                           GO  TO  330-TID1-SET-EXT
                       END-IF
                   WHEN    "2004"
      *                定点調査 ファイル送信なし
                       MOVE    ZERO            TO  WRK-DASUPLOAD
                       PERFORM 470-COLLECT-SYORI-SEC
                       MOVE    "JOIN"          TO  MCP-PUTTYPE
                       PERFORM 210-BACK-SYORI-SEC
               END-EVALUATE
           ELSE        
               EVALUATE    SPA-TIDCD
                   WHEN    "2002"
                       MOVE    "JOIN"          TO  MCP-PUTTYPE
                       PERFORM 210-BACK-SYORI-SEC
                   WHEN    "2004"
      *                定点調査 ファイル送信あり
                       MOVE    1               TO  WRK-DASUPLOAD
                       PERFORM 470-COLLECT-SYORI-SEC
                       MOVE    "JOIN"          TO  MCP-PUTTYPE
                       PERFORM 210-BACK-SYORI-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           INITIALIZE                  SPA-T01KYOUTU
           .
       330-TID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＴＩＤ２）ＯＫ処理
      *****************************************************************
       340-TID2-SET-SEC            SECTION.
      *
           IF      SPA-TID1-FLG    =   "OK"    OR  "NO"    OR  "PV"
               EVALUATE    SPA-TIDCD
                   WHEN    "1010"
                   WHEN    "1014"
                   WHEN    "1015"
                   WHEN    "1016"
                   WHEN    "1017"
                   WHEN    "1018"
                   WHEN    "1020"
                   WHEN    "1021"
                   WHEN    "1022"
                   WHEN    "1023"
                   WHEN    "1024"
                   WHEN    "1025"
      *                ジョブ管理チェック処理
                       PERFORM 4501-JOBKANRI-CHK-SEC
                       IF      SPA-ERRCD       =   SPACE
                           MOVE    SPA-TID1-FLG    TO  SPA-RECEDEN-FLG 
      *                    レセ電出力処理
                           PERFORM 460-RECEDEN-SEC
                           GO  TO  340-TID2-SET-EXT
                       END-IF
                   WHEN    "1027"
                   WHEN    "1028"
                   WHEN    "1029"
      *                ジョブ管理チェック処理
                       PERFORM 4501-JOBKANRI-CHK-SEC
                       IF      SPA-ERRCD       =   SPACE
                           MOVE    SPA-TID1-FLG    TO  SPA-RECEDEN-FLG 
      *                    レセ電出力処理
                           PERFORM 470-RECEDEN-SEC
                           GO  TO  340-TID2-SET-EXT
                       END-IF
                   WHEN    "1030"
                   WHEN    "1031"
                   WHEN    "1032"
      *                ジョブ管理チェック処理
                       PERFORM 4501-JOBKANRI-CHK-SEC
                       IF      SPA-ERRCD       =   SPACE
      *??                   MOVE    SPA-TID1-FLG    TO  SPA-RECEDEN-FLG 
                            MOVE    "NO"            TO  SPA-RECEDEN-FLG 
      *                    レセ電出力処理
                            PERFORM 460-RECEDEN-SEC
                           GO  TO  340-TID2-SET-EXT
                       END-IF
                   WHEN    "1033"
      *                ジョブ管理チェック処理
                       PERFORM 4501-JOBKANRI-CHK-SEC
                       IF      SPA-ERRCD       =   SPACE
                            MOVE    SPA-TID1-FLG    TO  SPA-RECEDEN-FLG 
      *                    レセ電出力処理
                            PERFORM 460-RECEDEN-SEC
                           GO  TO  340-TID2-SET-EXT
                       END-IF
                   WHEN    "1011"
                   WHEN    "1019"
                       PERFORM 4501-JOBKANRI-CHK-SEC
                       IF      SPA-ERRCD      =   SPACE
                           PERFORM 480-PRINT-SEC
                           GO  TO  340-TID2-SET-EXT
                       END-IF
                   WHEN    OTHER
      *                ジョブ管理チェック処理
                       PERFORM 4501-JOBKANRI-CHK-SEC
                       IF      SPA-ERRCD      =   SPACE
                           PERFORM 450-PRINT-SEC
                           GO  TO  340-TID2-SET-EXT
                       END-IF
               END-EVALUATE 
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           INITIALIZE                  SPA-T01KYOUTU
           .
       340-TID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書個別処理
      *****************************************************************
       350-T02-SET-SEC            SECTION.
      *
           IF      SPA-TID1-FLG    =   "OK"    OR  "NO"    OR  "PV"
      *        ジョブ管理チェック処理
               PERFORM 4501-JOBKANRI-CHK-SEC
               IF      SPA-ERRCD      =   SPACE
                   PERFORM 3501-KOBETU-PRINT-SEC
                   GO  TO  350-T02-SET-EXT
               END-IF
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           INITIALIZE                  SPA-T01KYOUTU
           .
       350-T02-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書個別処理
      *****************************************************************
       3501-KOBETU-PRINT-SEC           SECTION.
      *
           INITIALIZE                  SPA-T01-TBL-GROUP
      * 
      *    ジョブ管理削除処理
           MOVE    "ALL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "1004"              TO  SPA-ERRCD
               GO  TO  3501-KOBETU-PRINT-EXT
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  WRK-SYSYMD
           MOVE    SMCNDATE-HMS        TO  SYS-TIME 
      *
           MOVE    ZERO                TO  WRK-CONS-JOB-JOBID     
                                           WRK-PRTKANRI-RENNUM
           MOVE    SPACE               TO  WRK-CONS-PRTKANRI-TBL-GROUP                                
      *     
           MOVE    WRK-CONS-FILE-ID    TO  ERRFLPARA-FILE-ID
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    SPACE               TO  RECEERR
           STRING  MCP-TEMPDIR         DELIMITED   BY  SPACE
                   "/"                 DELIMITED   BY  SIZE
                   ERRFLPARA-BASENAME  DELIMITED   BY  SPACE
                                       INTO        RECEERR
           END-STRING
      *
      *    印刷ＤＢのＫＥＹ用     
           MOVE    WRK-SYSYMD          TO
                                   WRK-CONS-PRTKANRI-TBL-GROUP (1:8)
           MOVE    SYS-TIME            TO
                                   WRK-CONS-PRTKANRI-TBL-GROUP (9:)
      *          
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-T02-SHELLID-MAX
               IF      SPA-GMN-T02-TSYORIFLG (IDX)
                                               =   "1"
      *
      *            クライアント保存削除処理             
                   INITIALIZE                  ORCSFILESVAREA
                   MOVE    "D"             TO  SFILESV-MODE
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  SFILESV-TBL-KEY
                   MOVE    SPA-NAI-SKYYM   TO  SFILESV-SRYYM   (1)
                   MOVE    SPA-NAI-T02-TSHELLID (IDX)
                                           TO  SFILESV-SHELLID (1)
                   CALL    "ORCSFILESV"    USING
                                               ORCSFILESVAREA
                                               SPA-AREA
      *
                   INITIALIZE                  ORCSCSVINFOAREA
                   MOVE    "DEL"           TO  SCSVINFO-MODE
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  SCSVINFO-TBL-KEY
                   MOVE    SPA-NAI-SKYYM   TO  SCSVINFO-SRYYM
                   MOVE    SPA-NAI-T02-TSHELLID (IDX)
                                           TO  SCSVINFO-SHELLID
                   CALL    "ORCSCSVINFO"   USING
                                               ORCSCSVINFOAREA
                                               SPA-AREA
      *
                   COMPUTE WRK-CONS-JOB-JOBID  =
                               SPA-NAI-T02-TJOBID (IDX)    -   1
                   PERFORM 45011-JOBKANRI-HENSYU-SEC
      *             
                   MOVE    SPACE           TO  SHELLTBL
      *                                   
                   MOVE    SPA-NAI-T02-TSHELLID (IDX)
                                           TO  WRK-SHELL-SHELL
      *            ファイルデイレクトリ位置指定
                   INITIALIZE              ORCSMKPASSAREA
                   MOVE    WRK-SHELL-KENTAN
                                           TO  MKPASS-IN-01
                   CALL    "ORCSMKPASSSITE"    USING   ORCSMKPASSAREA
                   MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
      *                                     
                   MOVE    SPACE           TO  LNK-PRTKANRI-AREA
                   INITIALIZE                  LNK-PRTKANRI-AREA
                   ADD     1               TO  WRK-PRTKANRI-RENNUM
                   MOVE    WRK-PRTKANRI-RENNUM
                                           TO  LNK-PRTKANRI-RENNUM 
                   MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                           TO  LNK-PRTKANRI-TBL-KEY 
                   MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                           TO  LNK-PRTKANRI-TBL-GROUP
                   MOVE    SPA-NAI-SKYYM   TO  LNK-PRTKANRI-SRYYM 
                   MOVE    SPA-NAI-SKYYMD  TO  LNK-PRTKANRI-SKYYMD
                   MOVE    SPA-NAI-T02-TSHELLID (IDX)
                                           TO  LNK-PRTKANRI-SHELLID
                   MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
      *************MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
                   MOVE    WRK-CONS-TERMID TO  LNK-PRTKANRI-TERMID
                   MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
      *************IF      SPA-NAI-T02-TPRTNM (IDX)
      *************                    NOT =   SPACE
                       MOVE    SPA-NAI-T02-TPRTNM (IDX)
                                           TO  LNK-PRTKANRI-PRTNM
      *************ELSE       
      *************    MOVE    SPA-NAI-1031-PRTNM
      *************                        TO  LNK-PRTKANRI-PRTNM
      *************END-IF
                   MOVE    LNK-PRTKANRI-RENNUM
                                           TO  SHELLTBL-ARG1       
                   MOVE    LNK-PRTKANRI-TBL-KEY
                                           TO  SHELLTBL-ARG2     
                   MOVE    LNK-PRTKANRI-TBL-GROUP
                                           TO  SHELLTBL-ARG3       
                   MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                           TO  SHELLTBL-ARG4       
                   MOVE    LNK-PRTKANRI-SRYYM
                                           TO  SHELLTBL-ARG5       
                   MOVE    LNK-PRTKANRI-SKYYMD
                                           TO  SHELLTBL-ARG6       
                   MOVE    LNK-PRTKANRI-SHELLID
                                           TO  SHELLTBL-ARG7       
                   MOVE    LNK-PRTKANRI-PRIORITY
                                           TO  SHELLTBL-ARG8      
                   MOVE    LNK-PRTKANRI-TERMID
                                           TO  SHELLTBL-ARG9       
                   MOVE    LNK-PRTKANRI-OPID
                                           TO  SHELLTBL-ARG10      
                   MOVE    LNK-PRTKANRI-PRTNM
                                           TO  SHELLTBL-ARG11      
      *                 
                   MOVE    WRK-CONS-JOB-JOBID
                                           TO  SHELLTBL-ARG12
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  SHELLTBL-ARG13         
grpsys             MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG14
                   MOVE    ERRFLPARA-BASENAME
                                           TO  SHELLTBL-ARG15
                   MOVE    "1"             TO  SHELLTBL-ARG16
      *
                   IF      SPA-GMN-SRYYMKBN-MAX
                                           =   ZERO
                       MOVE    "999999"        TO  SHELLTBL-ARG17
                       MOVE    "999999"        TO  SHELLTBL-ARG18
                   ELSE
                       MOVE    SPA-NAI-SRYYMKBN-ST
                                               (SPA-NAI-SRYYMKBN-SELNUM)
                                               TO  SHELLTBL-ARG17
                       MOVE    SPA-NAI-SRYYMKBN-ED
                                               (SPA-NAI-SRYYMKBN-SELNUM)
                                               TO  SHELLTBL-ARG18
                   END-IF
                   IF      SPA-MW          =   SPA-GINBEE
                       MOVE    SPA-NAI-T02-TNAME (IDX)
                                           TO  SHELLTBL-REPOS-NAME
                   END-IF
      *
                   MOVE    SHELLTBL        TO  MCPDATA-REC
                   MOVE    "SHELL"         TO  MCP-FUNC
                   MOVE    "shell"         TO  MCP-TABLE
                   MOVE    "shell"         TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
               END-IF
           END-PERFORM                                                    
      *
      *
      *    印刷処理
      *
           IF      SPA-TID1-FLG        =   "OK"    OR  "PV"
               PERFORM 45011-JOBKANRI-HENSYU-SEC
      *
               MOVE    0                   TO  WRK-STPAGE
               MOVE    99999               TO  WRK-EDPAGE
      *
               MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                           TO  SPA-T01-TBL-GROUP
      *             
               MOVE    SPACE               TO  SHELLTBL
               MOVE    "print_prv.sh"      TO  SHELLTBL-NAME
      *                               
               MOVE    SPACE               TO  LNK-PRTKANRI-AREA
               INITIALIZE                      LNK-PRTKANRI-AREA
               MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                           TO  LNK-PRTKANRI-TBL-KEY 
               MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                           TO  LNK-PRTKANRI-TBL-GROUP
               MOVE    ZERO                TO  LNK-PRTKANRI-RENNUM
               MOVE    ZERO                TO  LNK-PRTKANRI-SHORI-RENNUM
               MOVE    SPA-NAI-SKYYM       TO  LNK-PRTKANRI-SRYYM 
               MOVE    SPA-NAI-SKYYMD      TO  LNK-PRTKANRI-SKYYMD
               MOVE    "print_prv.sh"      TO  LNK-PRTKANRI-SHELLID
               MOVE    ZERO                TO  LNK-PRTKANRI-PRIORITY
      *****    MOVE    SPA-TERMID          TO  LNK-PRTKANRI-TERMID
               MOVE    WRK-CONS-TERMID     TO  LNK-PRTKANRI-TERMID
               MOVE    SPA-OPID            TO  LNK-PRTKANRI-OPID
               MOVE    LNK-PRTKANRI-RENNUM TO  SHELLTBL-ARG1       
               MOVE    LNK-PRTKANRI-TBL-KEY
                                           TO  SHELLTBL-ARG2     
               MOVE    LNK-PRTKANRI-TBL-GROUP
                                           TO  SHELLTBL-ARG3       
               MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                           TO  SHELLTBL-ARG4       
               MOVE    LNK-PRTKANRI-SRYYM  TO  SHELLTBL-ARG5       
               MOVE    LNK-PRTKANRI-SKYYMD TO  SHELLTBL-ARG6       
               MOVE    LNK-PRTKANRI-SHELLID
                                           TO  SHELLTBL-ARG7       
               MOVE    LNK-PRTKANRI-PRIORITY
                                           TO  SHELLTBL-ARG8      
               MOVE    LNK-PRTKANRI-TERMID TO  SHELLTBL-ARG9       
               MOVE    LNK-PRTKANRI-OPID   TO  SHELLTBL-ARG10      
               MOVE    LNK-PRTKANRI-PRTNM  TO  SHELLTBL-ARG11      
      *                 
               MOVE    ERRFLPARA-BASENAME  TO  SHELLTBL-ARG12
               MOVE    WRK-CONS-JOB-JOBID  TO  SHELLTBL-ARG13
               MOVE    WRK-CONS-JOB-SHELLID
                                           TO  SHELLTBL-ARG14
               MOVE    WRK-STPAGE          TO  SHELLTBL-ARG15
               MOVE    WRK-EDPAGE          TO  SHELLTBL-ARG16
               EVALUATE    SPA-TID1-FLG
               WHEN    "PV"
                   MOVE    "3"             TO  SHELLTBL-ARG17
      **       WHEN    "NO"
      **           MOVE    "3"             TO  SHELLTBL-ARG17
      *----(05.00.00)--ADD-START---
               WHEN    "OK"
                   IF      SPA-CLIENT-PRT-FLG  =  2
                       MOVE    "5"         TO  SHELLTBL-ARG17
                   ELSE
                       MOVE    "2"         TO  SHELLTBL-ARG17
                   END-IF
               END-EVALUATE
               MOVE    SPA-CLIENT-SHOWDIALOG
                                           TO  SHELLTBL-ARG22
      *----(05.00.00)--ADD-END-----
grpsys         MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG18
               MOVE    SHELLTBL            TO  MCPDATA-REC
               MOVE    "SHELL"             TO  MCP-FUNC
               MOVE    "shell"             TO  MCP-TABLE
               MOVE    "allways"           TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           END-IF       
      *
      *    ＪＯＢＩＤ更新処理
      *
           PERFORM 45012-JOBID-UPD-SEC
      *
      *    印刷データ削除処理
      *
           PERFORM 45015-PRTDATA-DEL-SEC
      *     
      *    情報確認画面を表示                                                   
           PERFORM 490-KAKUNIN-SEC
           .
       3501-KOBETU-PRINT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ＣＳＶデータ作成処理
      *****************************************************************
       360-T04-SET-SEC            SECTION.
      *
           IF      SPA-TID1-FLG    =   "OK"
               MOVE    "T04"                  TO  SPA-T99-MOTOPG
      *        ジョブ管理チェック処理
               PERFORM 4501-JOBKANRI-CHK-SEC
               IF      SPA-ERRCD      =   SPACE
                   PERFORM 3601-CSV-HENSYU-SEC
                   GO  TO  360-T04-SET-EXT
               END-IF
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           INITIALIZE                  SPA-T01KYOUTU
           .
       360-T04-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    クライアント保存処理
      *****************************************************************
       3601-CSV-HENSYU-SEC           SECTION.
      * 
           MOVE    ZERO            TO  SPA-T99-FILESV-CHK-FLG
      *
      *    国保診療報酬請求書印刷区分
           MOVE    SPACE               TO  SYS-2005-REC 
           INITIALIZE                      SYS-2005-REC
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-2005-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2005-STYUKYMD (7:2)
           MOVE    SYS-2005-STYUKYMD   TO  SYS-2005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2005-REC
           ELSE
               INITIALIZE                      SYS-2005-REC
           END-IF
           IF      SYS-2005-RECEDENOUT  =   SPACE
               MOVE    "/var/tmp/"      TO  SYS-2005-RECEDENOUT
           END-IF    
           IF      SPA-GMN-DATAKBN  =  "5" OR  "6"
               MOVE    "/tmp/"     TO  SYS-2005-RECEDENOUT
           END-IF
      * 
      *    ジョブ管理削除処理
           MOVE    "ALL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "1004"              TO  SPA-ERRCD
               GO  TO  3601-CSV-HENSYU-EXT
           END-IF
      *
           MOVE    ZERO                TO  WRK-CONS-JOB-JOBID     
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *
           MOVE    WRK-CONS-FILE-ID    TO  ERRFLPARA-FILE-ID
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    SPACE               TO  RECEERR
           STRING  MCP-TEMPDIR         DELIMITED   BY  SPACE
                   "/"                 DELIMITED   BY  SIZE
                   ERRFLPARA-BASENAME  DELIMITED   BY  SPACE
                                       INTO        RECEERR
           END-STRING
           MOVE    SPACE           TO  SHELLTBL
           MOVE    "recept60.sh"   TO  SHELLTBL-NAME
      *     
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG1
           MOVE    SPA-NAI-SKYYM   TO  SHELLTBL-ARG2
           MOVE    SPA-NAI-T04-TSHELLID      (SPA-NAI-T04-SELNUM)
                                   TO  SHELLTBL-ARG3
           MOVE    SPA-NAI-T04-TSHORI-RENNUM (SPA-NAI-T04-SELNUM)
                                   TO  SHELLTBL-ARG4
           MOVE    SPA-NAI-T04-TRENNUM       (SPA-NAI-T04-SELNUM)
                                   TO  SHELLTBL-ARG5
           MOVE    SPA-GMN-DATAKBN TO  SHELLTBL-ARG6
           MOVE    SPA-NAI-T04-TTO-FOLDER   (SPA-NAI-T04-SELNUM)
                                   TO  SHELLTBL-ARG7
           MOVE    SPA-NAI-T04-TTO-DATA     (SPA-NAI-T04-SELNUM)
                                   TO  SHELLTBL-ARG8
      *    ipアドレス取得
      *****PERFORM 900-SYSTEM-GET-SEC
      *****MOVE    SLDUSER-HOST(1) TO  SHELLTBL-ARG9
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG10
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG11
           MOVE    ERRFLPARA-BASENAME
                                   TO  SHELLTBL-ARG12
           STRING  SYS-2005-RECEDENOUT DELIMITED  BY  SPACE
                   "/"                 DELIMITED  BY  SIZE
                               INTO    SHELLTBL-ARG13
           END-STRING
           MOVE    SPA-NAI-T04-TCODE-TYPE   (SPA-NAI-T04-SELNUM)
                                   TO  SHELLTBL-ARG14
           IF      SPA-NAI-T04-TSHELLID (SPA-NAI-T04-SELNUM)
                                   =   "execoprg.sh" 
      *        出産育児一時金のときは履歴保存あり
               MOVE    SPA-NAI-200501-RECEDEN-RIREKIKB
                                       TO  SHELLTBL-ARG15
               MOVE    SPA-NAI-T04-TTEISYUTUSAKI (SPA-NAI-T04-SELNUM)
                                       TO  SHELLTBL-ARG16
           ELSE
               MOVE    "0"             TO  SHELLTBL-ARG15
                                           SHELLTBL-ARG16
           END-IF
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "monthly"       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF    ( SPA-GMN-DATAKBN =   "5" OR  "6" )
               MOVE    "SV"            TO  SPA-TID1-FLG
           ELSE
               MOVE    "FI"            TO  SPA-TID1-FLG
           END-IF
      *
      *    ＪＯＢＩＤ更新処理
      *
           PERFORM 45012-JOBID-UPD-SEC
      *
      *    自動的に情報確認画面を表示
           PERFORM 490-KAKUNIN-SEC      
           .
       3601-CSV-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *     戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"         TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *     再印刷
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 450-REPRINT-SEC
      *     社保レセ電処理
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-RECEDEN-GID-SEC
      *     国保レセ電処理
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 460-RECEDEN-GID-SEC
      *     広域連合レセ電処理
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 460-RECEDEN-GID-SEC
      *     決定通知書（社保）
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 480-PRINT-GID-SEC
      *     決定通知書（国保）
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 480-PRINT-GID-SEC
      *     情報削除
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-JYOUHOU-DEL-SEC
      *     印刷
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-PRINT-GID-SEC
      *     自賠責ＰＤＦ作成
               WHEN    "CLICKED"       ALSO    "B14"
                   PERFORM 480-JIBAI-PDF-GID-SEC
      *     データ取り込み
               WHEN    "CLICKED"       ALSO    "B15"
                   PERFORM 480-DATA-EXPORT-SEC
      *     レセ電データ個別指示
               WHEN    "CLICKED"       ALSO    "B16"
                   PERFORM 480-RECEDEN-KOBETU-SEC
      *     労災レセ電処理
               WHEN    "CLICKED"       ALSO    "B17"
                   PERFORM 460-RECEDEN-GID-SEC
      *     ＥＦファイル処理
               WHEN    "CLICKED"       ALSO    "B18"
                   PERFORM 460-RECEDEN-GID-SEC
      *     公費ＣＳＶデータ作成
               WHEN    "CLICKED"       ALSO    "B19"
                   MOVE    SPACE           TO  SPA-T99-MOTOPG
                   PERFORM 480-CSV-SYORI-SEC
      *     主科未設定一覧
               WHEN    "CLICKED"       ALSO    "B20"
                   PERFORM 480-PRINT-GID-SEC
      *    公費個別指定
               WHEN    "CLICKED"       ALSO    "B21"
                   PERFORM 490-KOBETU-SEC
      *     チェック
               WHEN    "CLICKED"       ALSO    "B22"
                   PERFORM 480-PRINT-GID-SEC
      *    プリンタ設定
               WHEN    "CLICKED"       ALSO    "B23"
                   PERFORM 490-PRTNM-SET-SEC
      *    処理確認
               WHEN    "CLICKED"       ALSO    "B24"
                   PERFORM 490-KAKUNIN-SEC
      *    総括表・公費請求書
               WHEN    "CLICKED"       ALSO    "DATAKBN1"
               WHEN    "CLICKED"       ALSO    "DATAKBN2"
               WHEN    "CLICKED"       ALSO    "DATAKBN3"
               WHEN    "CLICKED"       ALSO    "DATAKBN4"
               WHEN    "CLICKED"       ALSO    "DATAKBN5"
               WHEN    "CLICKED"       ALSO    "DATAKBN6"
               WHEN    "CLICKED"       ALSO    "DATAKBN7"
               WHEN    "CLICKED"       ALSO    "DATAKBN8"
                   PERFORM 400-DATAKBN-SEC
      *    当月分・月遅れ分、返戻分
               WHEN    "CLICKED"       ALSO    "CHK11"
               WHEN    "CLICKED"       ALSO    "CHK12"
               WHEN    "CLICKED"       ALSO    "CHK21"
               WHEN    "CLICKED"       ALSO    "CHK22"
               WHEN    "CLICKED"       ALSO    "CHK31"
               WHEN    "CLICKED"       ALSO    "CHK32"
                   PERFORM 400-CHK-SEC
               WHEN    "CLICKED"       ALSO    "SYOKBN1"
               WHEN    "CLICKED"       ALSO    "SYOKBN2"
                   PERFORM 400-SYOKBN-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        一括分
               WHEN    "ACTIVATE"       ALSO    ANY
                   PERFORM 400-ENTRY-SYORI-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括分入力処理
      *****************************************************************
       400-ENTRY-SYORI-SEC             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
      *    入力チェック処理
           MOVE    ZERO                TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           .
       400-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
      *    基本チェック後ＳＰＡ再セット
           PERFORM 4104-GMN-SPA-HENSYU-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU         =   1
      *        処理開始前チェック処理
               PERFORM 4101-KAKUTEI-CHK-SEC
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
      *    入力前データ
           MOVE    SPA-GMN-PRTKBN      TO  WRK-PRTKBN-OLD
      *
           MOVE    T01-SKYYM           TO  SPA-GMN-SKYYM
           MOVE    T01-SKYYMD          TO  SPA-GMN-SKYYMD
           MOVE    T01-HKNJANUM        TO  SPA-GMN-HKNJANUM
           MOVE    T01-HKNJANUM1       TO  SPA-GMN-HKNJANUM1
           MOVE    T01-DATA            TO  SPA-GMN-DATAKBN-G
           MOVE    T01-NYUGAIKBN       TO  SPA-GMN-NYUGAIKBN-G
           MOVE    T01-PRTKBN          TO  SPA-GMN-PRTKBN-G
           MOVE    T01-DIRECT-HKNJANUM TO  SPA-GMN-DIRECT-HKNJANUM-G
           MOVE    T01-RECEDEN-CHK     TO  SPA-GMN-RECEDEN-CHK-G
           MOVE    T01-SRYYMKBN        TO  SPA-GMN-SRYYMKBN
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
           PERFORM 4101-SKYYM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4109-SRYYMKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4102-SKYYMD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4103-HKNJANUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4105-HKNJANUM1-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4106-PRTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4108-RECEDEN-CHK-KBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4104-DATAKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4105-NYUGAIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4107-DIRECT-HKNJANUM-CHK-SEC
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票区分選択処理
      *****************************************************************
       400-DATAKBN-SEC           SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "DATAKBN3"
                   MOVE    T01-DATAKBN3-VALUE  TO  SPA-DATAKBN3-VALUE
                   IF      SPA-DATAKBN3-VALUE  =   "T"
                       MOVE    "T"             TO  SPA-SYOKBN1-VALUE
                   END-IF
               WHEN    "DATAKBN4"
                   MOVE    T01-DATAKBN4-VALUE  TO  SPA-DATAKBN4-VALUE
                   IF      SPA-DATAKBN4-VALUE  =   "T"
                       MOVE    "T"             TO  SPA-CHK11-VALUE
                                                   SPA-CHK12-VALUE
                       MOVE    "T"             TO  SPA-SYOKBN1-VALUE
                   END-IF
               WHEN    "DATAKBN5"
                   MOVE    T01-DATAKBN5-VALUE  TO  SPA-DATAKBN5-VALUE
                   IF      SPA-DATAKBN5-VALUE  =   "T"
                       MOVE    "T"             TO  SPA-CHK21-VALUE
                                                   SPA-CHK22-VALUE
                       MOVE    "T"             TO  SPA-SYOKBN1-VALUE
                   END-IF
               WHEN    "DATAKBN6"
                   MOVE    T01-DATAKBN6-VALUE  TO  SPA-DATAKBN6-VALUE
                   IF      SPA-DATAKBN6-VALUE  =   "T"
                       MOVE    "T"             TO  SPA-SYOKBN1-VALUE
                   END-IF
               WHEN    "DATAKBN7"
                   MOVE    T01-DATAKBN7-VALUE  TO  SPA-DATAKBN7-VALUE
               WHEN    "DATAKBN1"
                   MOVE    T01-DATAKBN1-VALUE  TO  SPA-DATAKBN1-VALUE
                   IF      SPA-DATAKBN1-VALUE  =   "T"
                       MOVE    "T"             TO  SPA-CHK31-VALUE
                                                   SPA-CHK32-VALUE
                       MOVE    "T"             TO  SPA-SYOKBN1-VALUE
                   END-IF
               WHEN    "DATAKBN2"
                   MOVE    T01-DATAKBN2-VALUE  TO  SPA-DATAKBN2-VALUE
               WHEN    "DATAKBN8"
                   MOVE    T01-DATAKBN8-VALUE  TO  SPA-DATAKBN8-VALUE
                   IF      SPA-DATAKBN8-VALUE  =   "T"
                       MOVE    "T"             TO  SPA-SYOKBN2-VALUE
                   ELSE
                       MOVE    "F"             TO  SPA-SYOKBN2-VALUE
                   END-IF
           END-EVALUATE
      *
           IF    ( SPA-DATAKBN1-VALUE  =   "F" )
           AND   ( SPA-DATAKBN3-VALUE  =   "F" )
           AND   ( SPA-DATAKBN4-VALUE  =   "F" )
           AND   ( SPA-DATAKBN5-VALUE  =   "F" )
           AND   ( SPA-DATAKBN6-VALUE  =   "F" )
           AND   ( SPA-DATAKBN2-VALUE  =   "F" )
               MOVE    "F"             TO  SPA-SYOKBN1-VALUE
           END-IF
      *
           .
       400-DATAKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    当月分・月遅れ分、返戻分選択処理
      *****************************************************************
       400-CHK-SEC               SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "CHK11"
                   MOVE    T01-CHK11-VALUE     TO  SPA-CHK11-VALUE
               WHEN    "CHK12"
                   MOVE    T01-CHK12-VALUE     TO  SPA-CHK12-VALUE
               WHEN    "CHK21"
                   MOVE    T01-CHK21-VALUE     TO  SPA-CHK21-VALUE
               WHEN    "CHK22"
                   MOVE    T01-CHK22-VALUE     TO  SPA-CHK22-VALUE
               WHEN    "CHK31"
                   MOVE    T01-CHK31-VALUE     TO  SPA-CHK31-VALUE
               WHEN    "CHK32"
                   MOVE    T01-CHK32-VALUE     TO  SPA-CHK32-VALUE
           END-EVALUATE    
      *
           .
       400-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    医保、労災処理選択処理
      *****************************************************************
       400-SYOKBN-SEC            SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "SYOKBN1"
                   MOVE    T01-SYOKBN1-VALUE   TO  SPA-SYOKBN1-VALUE
                   IF      SPA-SYOKBN1-VALUE   =   "T"
                       MOVE    "T"             TO  SPA-CHK11-VALUE
                                                   SPA-CHK12-VALUE
                       MOVE    "T"             TO  SPA-CHK21-VALUE
                                                   SPA-CHK22-VALUE
                       MOVE    "T"             TO  SPA-CHK31-VALUE
                                                   SPA-CHK32-VALUE
                       MOVE    "T"             TO  SPA-DATAKBN1-VALUE
      *********************************************SPA-DATAKBN2-VALUE
                                                   SPA-DATAKBN3-VALUE
                                                   SPA-DATAKBN4-VALUE
                                                   SPA-DATAKBN5-VALUE
      *********************************************SPA-DATAKBN7-VALUE
                       IF      SPA-NAI-PRTKBN3 NOT =   SPACE
                           MOVE    "T"             TO
                                                   SPA-DATAKBN6-VALUE
                       END-IF
                   ELSE
                       MOVE    "F"             TO  SPA-DATAKBN1-VALUE
      *********************************************SPA-DATAKBN2-VALUE
                                                   SPA-DATAKBN3-VALUE
                                                   SPA-DATAKBN4-VALUE
                                                   SPA-DATAKBN5-VALUE
      *********************************************SPA-DATAKBN7-VALUE
                                                   SPA-DATAKBN6-VALUE
                   END-IF
               WHEN    "SYOKBN2"
                   MOVE    T01-SYOKBN2-VALUE   TO  SPA-SYOKBN2-VALUE
                   IF      SPA-SYOKBN2-VALUE   =   "T"
                       MOVE    "T"             TO  SPA-DATAKBN8-VALUE
                   ELSE
                       MOVE    "F"             TO  SPA-DATAKBN8-VALUE
                   END-IF
           END-EVALUATE    
      *
           .
       400-SYOKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求年月チェック処理
      *****************************************************************
       4101-SKYYM-CHK-SEC           SECTION.
      *
           MOVE    SPA-NAI-SKYYM    TO  WRK-SKYYM-OLD
      *
           MOVE    ZERO             TO  SPA-NAI-SKYYM
      *****MOVE    SPACE            TO  SPA-GMN-TITLE
      *
           IF      SPA-GMN-SKYYM    =   SPACE
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE 
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    "1"                 TO  SGDAY-INTYPE
               MOVE    SPA-GMN-SKYYM       TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN-SKYYM
                   MOVE    SGDAY-SDAY (1:6)    TO  SPA-NAI-SKYYM
                   MOVE    SGDAY-WDAY (2:4)    TO  SPA-NAI-SKYYM-W 
               ELSE
                   MOVE    "0003"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-NAI-SKYYM  NOT =   WRK-SKYYM-OLD
      *        ボタン設定処理
               PERFORM 3100-STATE-SET-SEC
      *
      *        診療年月によるシステム管理の取得処理
               PERFORM 3103-SYSKANRI-HENSYU-SEC
      *
      *****    診療年月期間設定処理
      *****    PERFORM 3105-SRYYMKBN-HENSYU-SEC
      *
      *        診療年月によるレセ電作成区分の設定
               IF      SPA-NAI-SKYYM      <   200804
                   MOVE    SPA-GMN-PRTKBNLST (1)
                                              TO  SPA-GMN-PRTKBN-G
               END-IF
      *
      *        ファイル保存情報
               PERFORM 3102-FILE-INFO-CHECK-SEC
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    SPACE           TO  SPA-GMN-TITLE
                                           SPA-GMN-TITLE1
                                           SPA-GMN-TITLE2
               MOVE    1               TO  SPA-GMN-CUR
           ELSE
               IF      SPA-NAI-SKYYM  NOT =   WRK-SKYYM-OLD
      *            未実施の未請求解除データのチェック処理
                   PERFORM 3101-SYUKA-CHECK-SEC
      *
      *            点検レセ０点公費記載の請求管理のチェック処理
                   PERFORM 3104-SEIKYU-CHECK-SEC
               END-IF
           END-IF
      *
           .
       4101-SKYYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求年月日チェック処理
      *****************************************************************
       4102-SKYYMD-CHK-SEC           SECTION.
      *
           MOVE    ZERO            TO  SPA-NAI-SKYYMD
      *
           IF      SPA-GMN-SKYYMD  =   SPACE
               MOVE    "0002"          TO  SPA-ERRCD
           ELSE 
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-SKYYMD      TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN-SKYYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-SKYYMD
               ELSE
                   MOVE    "0004"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    2               TO  SPA-GMN-CUR
           END-IF
      *
           .
       4102-SKYYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号チェック処理
      *****************************************************************
       4103-HKNJANUM-CHK-SEC           SECTION.
      *
           MOVE    SPACE            TO  SPA-GMN-HKNJAMEI
           MOVE    ZERO             TO  SPA-NAI-HKNJANUM 
      *
           IF      SPA-GMN-HKNJANUM =   SPACE
               CONTINUE
           ELSE 
               MOVE    SPACE           TO  HKNJAINF-REC
               INITIALIZE                  HKNJAINF-REC
grpsys         MOVE    SPA-HOSPNUM     TO  HKNJA-HOSPNUM
               MOVE    SPA-GMN-HKNJANUM
                                       TO  HKNJA-HKNJANUM
               MOVE    HKNJAINF-REC    TO  MCPDATA-REC
               PERFORM 900-HKNJAINF-INV-SEC
               IF      FLG-HKNJAINF    =   ZERO
                   MOVE    HKNJA-HKNJANAME     TO SPA-GMN-HKNJAMEI
                   MOVE    SPA-GMN-HKNJANUM    TO  SPA-NAI-HKNJANUM
               ELSE
                   MOVE    "0005"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    3               TO  SPA-GMN-CUR
           END-IF
      *
           .
       4103-HKNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号（広域連合）チェック処理
      *****************************************************************
       4105-HKNJANUM1-CHK-SEC           SECTION.
      *
           MOVE    SPACE            TO  SPA-GMN-HKNJAMEI1
           MOVE    ZERO             TO  SPA-NAI-HKNJANUM1 
      *
           IF      SPA-GMN-HKNJANUM1    =   SPACE
               CONTINUE
           ELSE 
               MOVE    SPACE           TO  HKNJAINF-REC
               INITIALIZE                  HKNJAINF-REC
grpsys         MOVE    SPA-HOSPNUM     TO  HKNJA-HOSPNUM
               MOVE    SPA-GMN-HKNJANUM1
                                       TO  HKNJA-HKNJANUM
               MOVE    HKNJAINF-REC    TO  MCPDATA-REC
               PERFORM 900-HKNJAINF-INV-SEC
               IF      FLG-HKNJAINF    =   ZERO
                   MOVE    HKNJA-HKNJANAME     TO SPA-GMN-HKNJAMEI1
                   MOVE    SPA-GMN-HKNJANUM1   TO  SPA-NAI-HKNJANUM1
               ELSE
                   MOVE    "0005"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    8               TO  SPA-GMN-CUR
           END-IF
      *
           .
       4105-HKNJANUM1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル出力先入力処理
      *****************************************************************
       4104-DATAKBN-CHK-SEC            SECTION.
      *
           IF    ( SPA-MW              =   SPA-GINBEE )
           AND   ( SPA-GMN-DATAKBN-MAX =   ZERO       )
               MOVE    SPACE               TO  SPA-GMN-DATAKBN-G
               GO  TO  4104-DATAKBN-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-DATAKBN     =   SPACE
               IF      SPA-MW              =   SPA-GINBEE
                   IF      SPA-GMN-DATAKBN-MAX >   ZERO
                       MOVE    "0017"              TO  SPA-ERRCD
                       MOVE    4                   TO  SPA-GMN-CUR
      *************ELSE
      *************    MOVE    "0021"              TO  SPA-ERRCD
                   END-IF
                   GO  TO  4104-DATAKBN-CHK-EXT
               ELSE
                   MOVE    SPA-GMN-DATAKBNLST (2)
                                               TO  SPA-GMN-DATAKBN-G
               END-IF
           ELSE
               MOVE    SPACE               TO   SPA-GMN-DATAKBNMEI
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-DATAKBN-MAX
                   IF        SPA-GMN-DATAKBN
                                   =   SPA-GMN-DATAKBNL (IDX)
                       MOVE    SPA-GMN-DATAKBNMEIL (IDX)
                                           TO  SPA-GMN-DATAKBNMEI
                       MOVE    SPA-GMN-DATAKBN-MAX
                                           TO  IDX
                   END-IF
               END-PERFORM        
               IF      SPA-GMN-DATAKBNMEI  =   SPACE  
                   IF      SPA-MW              =   SPA-GINBEE
                       MOVE    SPA-GMN-DATAKBNLST (1)
                                               TO  SPA-GMN-DATAKBN-G
                   ELSE
                       MOVE    SPA-GMN-DATAKBNLST (2)
                                               TO  SPA-GMN-DATAKBN-G
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-GMN-PRTKBN  =   "6" OR  "7" OR  "8" OR  "9"
               IF      SPA-GMN-DATAKBN     =   "4" OR  "5"
                   CONTINUE
               ELSE
                   IF      SPA-MW              =   SPA-GINBEE
                       MOVE    SPA-GMN-DATAKBNLST (1)
                                               TO  SPA-GMN-DATAKBN-G
                   ELSE
                       MOVE    SPA-GMN-DATAKBNLST (2)
                                               TO  SPA-GMN-DATAKBN-G
                   END-IF
               END-IF
           END-IF
           .
       4104-DATAKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分入力処理
      *****************************************************************
       4105-NYUGAIKBN-CHK-SEC          SECTION.
      *
           IF      SPA-GMN-NYUGAIKBN   =   SPACE
               MOVE    SPA-GMN-NYUGAIKBNLST (1)
                                           TO  SPA-GMN-NYUGAIKBN-G
           ELSE
               MOVE    SPACE               TO  SPA-GMN-NYUGAIKBNMEI
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-NYUGAIKBN-MAX
                   IF        SPA-GMN-NYUGAIKBN
                                   =   SPA-GMN-NYUGAIKBNL (IDX)
                       MOVE    SPA-GMN-NYUGAIKBNMEIL (IDX)
                                           TO  SPA-GMN-NYUGAIKBNMEI
                       MOVE    SPA-GMN-NYUGAIKBN-MAX
                                           TO  IDX
                   END-IF
               END-PERFORM        
               IF      SPA-GMN-NYUGAIKBNMEI  =   SPACE  
                   MOVE    SPA-GMN-NYUGAIKBNLST (1)
                                               TO  SPA-GMN-NYUGAIKBN-G
               END-IF
           END-IF
      *
           IF      SPA-GMN-PRTKBN  =   "9"
               MOVE    SPA-GMN-NYUGAIKBNLST (2)
                                       TO  SPA-GMN-NYUGAIKBN-G
           END-IF
           .
       4105-NYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電データ作成区分  入力処理
      *****************************************************************
       4106-PRTKBN-CHK-SEC          SECTION.
      *
           IF      SPA-GMN-PRTKBN   =   SPACE
               MOVE    SPA-GMN-PRTKBNLST (1)
                                           TO  SPA-GMN-PRTKBN-G
           ELSE
               MOVE    SPACE               TO  SPA-GMN-PRTKBNMEI
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-PRTKBN-MAX
                   IF        SPA-GMN-PRTKBN
                                   =   SPA-GMN-PRTKBNL (IDX)
                       MOVE    SPA-GMN-PRTKBNMEIL (IDX)
                                           TO  SPA-GMN-PRTKBNMEI
                       MOVE    SPA-GMN-PRTKBN-MAX
                                           TO  IDX
                   END-IF
               END-PERFORM
               IF      SPA-GMN-PRTKBNMEI  =   SPACE
                   MOVE    SPA-GMN-PRTKBNLST (1)
                                               TO  SPA-GMN-PRTKBN-G
               END-IF
           END-IF
      *
           IF      SPA-GMN-PRTKBN  =   "6" OR  "7"
               IF      SPA-GMN-DATAKBN     =   "4" OR  "5"
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-DATAKBNLST (2)
                                           TO  SPA-GMN-DATAKBN-G
               END-IF
               MOVE    SPA-GMN-RECEDEN-CHKLST (1)
                                       TO  SPA-GMN-RECEDEN-CHK-G
           END-IF
      *
           IF      SPA-GMN-PRTKBN  =   "8" OR  "9"
               IF      SPA-GMN-DATAKBN     =   "4" OR  "5"
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-DATAKBNLST (2)
                                           TO  SPA-GMN-DATAKBN-G
               END-IF
           END-IF
      *
           IF      SPA-GMN-PRTKBN  =   "9"
               MOVE    SPA-GMN-NYUGAIKBNLST (2)
                                           TO  SPA-GMN-NYUGAIKBN-G
           END-IF
           .
       4106-PRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    直接請求する保険者番号  入力処理
      *****************************************************************
       4107-DIRECT-HKNJANUM-CHK-SEC          SECTION.
      *
           IF      SPA-GMN-DIRECT-HKNJANUM-MAX
                                       =   ZERO
               GO  TO  4107-DIRECT-HKNJANUM-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-DIRECT-HKNJANUM
                                       =   SPACE
               MOVE    SPA-GMN-DIRECT-HKNJANUMLST (1)
                                           TO  SPA-GMN-DIRECT-HKNJANUM-G
               MOVE    SPACE               TO
                                           SPA-NAI-DIRECT-HKNJANUM-63
           ELSE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-DIRECT-HKNJANUM-MAX
                   IF        SPA-GMN-DIRECT-HKNJANUM
                                       =   SPA-GMN-DIRECT-HKNJANUML(IDX)
                       MOVE    SPA-GMN-DIRECT-HKNJANUMLST (IDX)
                                           TO  SPA-GMN-DIRECT-HKNJANUM-G
                       MOVE    SPA-NAI-DIRECT-HKNJANUM-63L(IDX)
                                           TO
                                           SPA-NAI-DIRECT-HKNJANUM-63
                       MOVE    SPA-GMN-DIRECT-HKNJANUM-MAX
                                           TO  IDX
                   END-IF
               END-PERFORM
               IF      SPA-GMN-DIRECT-HKNJANAME
                                       =   SPACE
                   MOVE    SPA-GMN-DIRECT-HKNJANUMLST (1)
                                           TO  SPA-GMN-DIRECT-HKNJANUM-G
                   MOVE    SPACE           TO
                                           SPA-NAI-DIRECT-HKNJANUM-63
               END-IF
           END-IF
           .
       4107-DIRECT-HKNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電データチェック区分  入力処理
      *****************************************************************
       4108-RECEDEN-CHK-KBN-CHK-SEC          SECTION.
      *
           IF      SPA-GMN-RECEDEN-CHK =   SPACE
               MOVE    SPACE               TO  SPA-GMN-RECEDEN-CHK-G
           ELSE
               MOVE    SPACE               TO  SPA-GMN-RECEDEN-CHKMEI
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-RECEDEN-CHK-MAX
                   IF        SPA-GMN-RECEDEN-CHK
                                   =   SPA-GMN-RECEDEN-CHKL (IDX)
                       MOVE    SPA-GMN-RECEDEN-CHKMEIL (IDX)
                                           TO  SPA-GMN-RECEDEN-CHKMEI
                       MOVE    SPA-GMN-RECEDEN-CHK-MAX
                                           TO  IDX
                   END-IF
               END-PERFORM
               IF      SPA-GMN-RECEDEN-CHKMEI  =   SPACE
                   MOVE    SPACE               TO  SPA-GMN-RECEDEN-CHK-G
               END-IF
           END-IF
      *    
      *    空白の時シス管より設定
           IF      SPA-GMN-RECEDEN-CHK =   SPACE
               IF      SPA-NAI-200501-RECEDEN-CHK-KBN
                                           =   "0"
                   MOVE    SPA-GMN-RECEDEN-CHKLST (1)
                                               TO  SPA-GMN-RECEDEN-CHK-G
               ELSE
                   MOVE    SPA-GMN-RECEDEN-CHKLST (2)
                                               TO  SPA-GMN-RECEDEN-CHK-G
               END-IF
           END-IF
           .
       4108-RECEDEN-CHK-KBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月期間入力処理
      *****************************************************************
       4109-SRYYMKBN-CHK-SEC          SECTION.
      *
           MOVE    ZERO               TO  SPA-NAI-SRYYMKBN-SELNUM
      *
           IF      SPA-GMN-SRYYMKBN-MAX
                                      =   ZERO
               GO  TO  4109-SRYYMKBN-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-SRYYMKBN    =   SPACE
               MOVE    SPACE               TO  SPA-GMN-SRYYMKBN-G
           ELSE
               MOVE    SPACE               TO  SPA-GMN-SRYYMKBNNM
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       SPA-GMN-SRYYMKBN-MAX
                   IF        SPA-GMN-SRYYMKBN
                                       =   SPA-GMN-SRYYMKBNL (IDX)
                       IF      SPA-NAI-SRYYMKBN-ERR (IDX)
                                           =   SPACE
                           MOVE    SPA-GMN-SRYYMKBNNML (IDX)
                                               TO  SPA-GMN-SRYYMKBNNM
                           MOVE    IDX         TO
                                               SPA-NAI-SRYYMKBN-SELNUM
                       ELSE
                           MOVE    "0050"      TO  SPA-ERRCD
                       END-IF
                       MOVE    SPA-GMN-SRYYMKBN-MAX
                                               TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      SPA-GMN-SRYYMKBNNM  =   SPACE
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       SPA-GMN-SRYYMKBN-MAX
                   IF      SPA-NAI-SRYYMKBN-ERR (IDX)
                                           =   SPACE
                       MOVE    SPA-GMN-SRYYMKBNLST (IDX)
                                               TO  SPA-GMN-SRYYMKBN-G
                       MOVE    IDX             TO
                                               SPA-NAI-SRYYMKBN-SELNUM
                       MOVE    SPA-GMN-SRYYMKBN-MAX
                                               TO  IDX
                   END-IF
               END-PERFORM
           END-IF
           .
       4109-SRYYMKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理開始前チェック処理
      *****************************************************************
       4101-KAKUTEI-CHK-SEC        SECTION.
      *
           IF      SPA-MW          =   SPA-GINBEE
               EVALUATE    MCP-WIDGET
                   WHEN    "B06"
                   WHEN    "B07"
                   WHEN    "B08"
                   WHEN    "B16"
                   WHEN    "B17"
                   WHEN    "B18"
                   WHEN    "B19"
                       IF      SPA-GMN-DATAKBN-MAX =   ZERO
                           MOVE    "0021"          TO  SPA-ERRCD
                           MOVE    4               TO  SPA-GMN-CUR
                           GO  TO  4101-KAKUTEI-CHK-EXT
                       END-IF
               END-EVALUATE
           END-IF
      *
           EVALUATE    MCP-WIDGET
               WHEN    "B12"
      *            対象帳票が未選択
                   IF      SPA-DATAKBN     =   "FFFFFFFF" 
                       MOVE    "0006"          TO  SPA-ERRCD
                   END-IF
      *
      *            総括表（社保）の対象データが未選択
                   IF      SPA-ERRCD           =   SPACE
                       IF      SPA-DATAKBN4-VALUE  =   "T"
                           IF    ( SPA-CHK11-VALUE     =   "F" )
                           AND   ( SPA-CHK12-VALUE     =   "F" )  
                               MOVE    "0007"              TO  SPA-ERRCD
                           END-IF
                       END-IF
                   END-IF    
      *
      *            総括表（国保）の対象データが未選択
                   IF      SPA-ERRCD           =   SPACE
                       IF      SPA-DATAKBN5-VALUE  =   "T"
                           IF    ( SPA-CHK21-VALUE     =   "F" )
                           AND   ( SPA-CHK22-VALUE     =   "F" )  
                               MOVE    "0008"              TO  SPA-ERRCD
                           END-IF
                       END-IF
                   END-IF    
      *
      *            総括表（広域連合）の対象データが未選択
                   IF      SPA-ERRCD           =   SPACE
                       IF      SPA-DATAKBN1-VALUE  =   "T"
                           IF    ( SPA-CHK31-VALUE     =   "F" )
                           AND   ( SPA-CHK32-VALUE     =   "F" )  
                               MOVE    "0009"              TO  SPA-ERRCD
                           END-IF
                       END-IF
                   END-IF    
               WHEN    "B17"
                   IF      SPA-T01-JOB-PARAM
                                           NOT =   SPACE
                       CONTINUE
                   ELSE
                       IF      SPA-GMN-DATAKBN =   "3"
                           MOVE    "0013"          TO  SPA-ERRCD
                           MOVE    4               TO  SPA-GMN-CUR
                       END-IF
                       IF      SPA-GMN-PRTKBN  =   "6" OR  "7" OR  "8"
                                               OR  "9"
                           MOVE    "0014"          TO  SPA-ERRCD
                           MOVE    4               TO  SPA-GMN-CUR
                       END-IF
                   END-IF
               WHEN    "B06"
               WHEN    "B07"
               WHEN    "B08"
                   IF      SPA-GMN-PRTKBN  =   "6" OR  "7"
                       IF      SPA-GMN-NYUGAIKBN   =   "0"
                           MOVE    "0015"          TO  SPA-ERRCD
                           MOVE    5               TO  SPA-GMN-CUR
                       END-IF
                       IF      SPA-GMN-DATAKBN =   "4" OR "5"
                           CONTINUE
                       ELSE
                           MOVE    "0016"          TO  SPA-ERRCD
                           MOVE    4               TO  SPA-GMN-CUR
                       END-IF
                   END-IF
                   IF      SPA-GMN-PRTKBN  =   "8" OR  "9"
                       IF      SPA-GMN-DATAKBN =   "4" OR  "5"
                           CONTINUE
                       ELSE
                           MOVE    "0016"          TO  SPA-ERRCD
                           MOVE    4               TO  SPA-GMN-CUR
                       END-IF
                   END-IF
               WHEN    "B18"
                   IF    ( SPA-NAI-1001-PREFNUM
                                           =   0 )
                   AND   ( SPA-NAI-1001-HOSPCD
                                           =   0 ) 
                       MOVE    "1007"          TO SPA-ERRCD
                       MOVE    4               TO  SPA-GMN-CUR
                   END-IF
               WHEN    "B16"
                   IF      SPA-GMN-PRTKBN  =   "6" OR  "7"
                       IF      SPA-GMN-NYUGAIKBN   =   "1" OR  "2"
                           CONTINUE
                       ELSE
                           MOVE    "0010"       TO  SPA-ERRCD
                           MOVE    5            TO  SPA-GMN-CUR
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       4101-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック後ＳＰＡ再セット
      *****************************************************************
       4104-GMN-SPA-HENSYU-SEC         SECTION.
      *
           IF    ( SPA-NAI-SKYYM  NOT =   WRK-SKYYM-OLD  )
           OR    ( SPA-GMN-PRTKBN NOT =   WRK-PRTKBN-OLD )
      *        診療年月期間設定処理
               PERFORM 3105-SRYYMKBN-HENSYU-SEC
      *
      *        月途中変更の診療年月かチェック
               MOVE    SPACE           TO  SPA-GMN-TITLE2
      *
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       SPA-GMN-SRYYMKBN-MAX
                   IF      ( SPA-NAI-SRYYMKBN-ST (IDX)
                                       <=   SPA-NAI-SKYYM )
                   AND     ( SPA-NAI-SRYYMKBN-ED (IDX)
                                       >=   SPA-NAI-SKYYM )
                       IF      SPA-NAI-SRYYMKBN-ERR (IDX)
                                           =   "1"
                           MOVE    ZERO    TO  SPA-GMN-SRYYMKBN-MAX
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX >       10
                               INITIALIZE  SPA-GMN-SRYYMKBNLST (IDX)
                           END-PERFORM
                           MOVE    "0051"      TO  SPA-ERRCD
                           MOVE    2           TO  SPA-GMN-CUR
      *
                           MOVE
                  "月途中に医療機関コードが変更されている診療年月です"
                                               TO  SPA-GMN-TITLE2
                       END-IF
                       MOVE    SPA-GMN-SRYYMKBN-MAX
                                               TO  IDX
                   END-IF
               END-PERFORM
           END-IF
     *
           .
       4104-GMN-SPA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *        
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-3003-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-3003-STYUKYMD (7:2)
           MOVE    SYS-3003-STYUKYMD   TO  SYS-3003-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SYS-3003-JOBID
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
                   IF      SJOBKANRI-RETURN
                                           =   ZERO
                       IF      JOB-ENDYMD      =   SPACE
      *********        AND     JOB-TERMID      =   SPA-TERMID
                           MOVE    "6666"          TO  SPA-TIDCD
                           GO  TO  210-BACK-EXT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *        
      *    定点調査研究事業 収集データ作成・送信チェック
           INITIALIZE                  ORCSCOLCHKAREA
           CALL    "ORCSCOLCHK"        USING
                                           ORCSCOLCHKAREA
                                           SPA-AREA
      *
      *    COLCHK-RC : 00 はメッセージ表示
      *                10 or 20 は何にもしない
      *                30 はデータ確認
      *
           IF      COLCHK-RC           =   ZERO
                   MOVE    "2002"          TO  SPA-TIDCD
                   GO  TO  210-BACK-EXT
           END-IF    
           IF      COLCHK-RC           =   30
               PERFORM 490-TEITEN-SEC
               GO  TO  210-BACK-EXT
           END-IF
      *
           PERFORM 210-BACK-SYORI-SEC         
           .
       210-BACK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      *
      *
           MOVE    "M01"               TO  SPA-SAKIPG
           MOVE    "T01"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  SPA-WORK 
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    作表処理
      *****************************************************************
       450-PRINT-GID-SEC                SECTION.
      *
      *    入力チェック
           MOVE    1               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  450-PRINT-GID-EXT
           END-IF
      *       
      *    処理スクリプトの存在チェック
           IF      SPA-ERRCD       =   SPACE
               INITIALIZE              WRK-CHK-AREA
      *
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-3004-MAX
                   IF    ( T01-DATAKBN4-VALUE      =   "T" ) 
                   AND   ( SPA-3004-CHKFLG (IDX 1) =   "T" )
                       PERFORM 4502-SCRIPT-CHECK-SEC
                   END-IF 
                   IF    ( T01-DATAKBN5-VALUE      =   "T" ) 
                   AND   ( SPA-3004-CHKFLG (IDX 2) =   "T" )
                       PERFORM 4502-SCRIPT-CHECK-SEC
                   END-IF
                   IF    ( T01-DATAKBN1-VALUE      =   "T" ) 
                   AND   ( SPA-3004-CHKFLG (IDX 4) =   "T" )
                       PERFORM 4502-SCRIPT-CHECK-SEC
                   END-IF
                   IF    ( T01-DATAKBN3-VALUE      =   "T" )
                   AND   ( SPA-3004-CHKFLG (IDX 3) =   "T" )
                       PERFORM 4502-SCRIPT-CHECK-SEC
                   END-IF
               END-PERFORM
           END-IF
      *
      *    ジョブ管理チェック処理
           IF      SPA-ERRCD       =   SPACE
               PERFORM 4501-JOBKANRI-CHK-SEC
               IF      SPA-ERRCD       =   SPACE
      *            ガイドメッセージセット
                   EVALUATE    TRUE
                       WHEN    SPA-DATAKBN     =   "TFFFFFFF" 
                           MOVE    "1003"              TO  SPA-TIDCD
                       WHEN    SPA-DATAKBN     =   "FTFFFFFF"
                           MOVE    "1004"              TO  SPA-TIDCD
                       WHEN    SPA-DATAKBN     =   "FFTFFFFF"
                           MOVE    "1005"              TO  SPA-TIDCD
                       WHEN    SPA-DATAKBN     =   "FFFTFFFF"
                           MOVE    "1006"              TO  SPA-TIDCD
                       WHEN    SPA-DATAKBN     =   "FFFFTFFF"
                           MOVE    "1007"              TO  SPA-TIDCD
                       WHEN    SPA-DATAKBN     =   "FFFFFTFF"
                           MOVE    "1008"              TO  SPA-TIDCD
                       WHEN    SPA-DATAKBN     =   "FFFFFFTF"
                           MOVE    "1009"              TO  SPA-TIDCD
                       WHEN    SPA-DATAKBN     =   "FFFFFFFT"
                           MOVE    "1026"              TO  SPA-TIDCD
                       WHEN    SPA-DATAKBN3-VALUE
                                               =   "T" 
                           MOVE    "1001"              TO  SPA-TIDCD
                       WHEN    SPA-DATAKBN3-VALUE
                                               =   "F" 
                           MOVE    "1002"              TO  SPA-TIDCD
                   END-EVALUATE
               END-IF
           END-IF
           .
       450-PRINT-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       4501-JOBKANRI-CHK-SEC       SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRCD
      *
      *    ジョブ管理チェック        
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-3003-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-3003-STYUKYMD (7:2)
           MOVE    SYS-3003-STYUKYMD   TO  SYS-3003-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SYS-3003-JOBID
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
                   IF      SJOBKANRI-RETURN
                                           =   ZERO
                       IF      JOB-ENDYMD      =   SPACE
                           MOVE    "7777"          TO  SPA-ERRCD
                           MOVE    SPACE           TO  WRK-RECEERR
                           STRING  "処理中です【"  DELIMITED  BY  SIZE
                                   JOB-SHELLMSG    DELIMITED  BY  SPACE
                                   "】"            DELIMITED  BY  SIZE      
                                                   INTO  WRK-RECEERR
                           END-STRING
                           MOVE    SYS-3003-JOBID  TO  IDX
                       END-IF
                   END-IF    
               END-PERFORM
           END-IF
      *
      *
      *    ジョブ管理チェック（レセプト）
           IF      SPA-ERRCD           =   SPACE         
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-CONS-JOB-JOBID-RECEPT1
                                       TO  JOB-JOBID
               MOVE    WRK-CONS-JOB-SHELLID-RECEPT1
                                       TO  JOB-SHELLID
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
              IF      SJOBKANRI-RETURN
                                       =   ZERO
                   IF      JOB-ENDYMD      =   SPACE
                       MOVE    "7777"          TO  SPA-ERRCD
                       MOVE    SPACE             TO  WRK-RECEERR
                       STRING  "レセプト作成処理中です【"
                                                 DELIMITED  BY  SIZE
                               JOB-SHELLMSG      DELIMITED  BY  SPACE
                               "】"              DELIMITED  BY  SIZE      
                                                 INTO  WRK-RECEERR
                       END-STRING
                   END-IF
               END-IF    
           END-IF    
           .
       4501-JOBKANRI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理スクリプトの存在チェック
      *****************************************************************
       4502-SCRIPT-CHECK-SEC              SECTION.
      *
           IF    ( SPA-MW          =   SPA-GINBEE )
           AND   ( SPA-3004-NAME (IDX)
                               NOT =   SPACE      )
               GO  TO  4502-SCRIPT-CHECK-EXT
           END-IF
      *
      *    ファイルデイレクトリ位置指定
           MOVE    SPA-3004-SHELLID (IDX)
                                   TO  WRK-SHELL-SHELL
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    WRK-SHELL-KENTAN
                                   TO  MKPASS-IN-01
           CALL    "ORCSMKPASSSITE"    USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  WRKPARA
      *
           OPEN    INPUT               WRK-FILE
           IF      STS-WRK         =   "00"
               CLOSE   WRK-FILE
           ELSE
               ADD     1               TO  WRK-CHK-CNT
               IF      SPA-ERRCD       =   SPACE
                   MOVE    "0018"          TO  SPA-ERRCD
                   MOVE    SPA-3004-SHELLID  (IDX)
                                           TO  WRK-CHK-SHELLID
                   MOVE    SPA-3004-SHORIMSG (IDX)
                                           TO  WRK-CHK-SHORIMSG
               END-IF
           END-IF
           .
       4502-SCRIPT-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       450-PRINT-SEC                   SECTION.
      *
           INITIALIZE                  SPA-T01-TBL-GROUP
      * 
      *    ジョブ管理削除処理
           MOVE    "ALL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "1004"              TO  SPA-ERRCD
               GO  TO  450-PRINT-EXT
           END-IF             
      *
      *    クライアント保存削除処理             
           IF      T01-DATAKBN3-VALUE  =   "T"     
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-3004-MAX
                   IF      SPA-3004-CHKFLG (IDX 3) =   "T"                
                       INITIALIZE                  ORCSFILESVAREA
                       MOVE    "D"             TO  SFILESV-MODE
                       MOVE    WRK-CONS-JOB-SHELLID
                                               TO  SFILESV-TBL-KEY
                       MOVE    SPA-NAI-SKYYM   TO  SFILESV-SRYYM (1)
                       MOVE    SPA-3004-SHELLID (IDX)
                                               TO  SFILESV-SHELLID (1)
                       CALL    "ORCSFILESV"    USING
                                                   ORCSFILESVAREA
                                                   SPA-AREA
      *
                       INITIALIZE                  ORCSCSVINFOAREA
                       MOVE    "DEL"           TO  SCSVINFO-MODE
                       MOVE    WRK-CONS-JOB-SHELLID
                                               TO  SCSVINFO-TBL-KEY
                       MOVE    SPA-NAI-SKYYM   TO  SCSVINFO-SRYYM
                       MOVE    SPA-3004-SHELLID (IDX)
                                               TO  SCSVINFO-SHELLID
                       CALL    "ORCSCSVINFO"   USING
                                                   ORCSCSVINFOAREA
                                                   SPA-AREA
                   END-IF
               END-PERFORM
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  WRK-SYSYMD
           MOVE    SMCNDATE-HMS        TO   SYS-TIME 
      *
           MOVE    ZERO                TO  WRK-CONS-JOB-JOBID     
                                           WRK-PRTKANRI-RENNUM
           MOVE    SPACE               TO  WRK-CONS-PRTKANRI-TBL-GROUP                                
      *     
           MOVE    WRK-CONS-FILE-ID    TO  ERRFLPARA-FILE-ID
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    SPACE               TO  RECEERR
           STRING  MCP-TEMPDIR         DELIMITED   BY  SPACE
                   "/"                 DELIMITED   BY  SIZE
                   ERRFLPARA-BASENAME  DELIMITED   BY  SPACE
                                       INTO        RECEERR
           END-STRING
      *
      *    国保診療報酬請求書印刷区分
           MOVE    SPACE               TO  SYS-2005-REC 
           INITIALIZE                      SYS-2005-REC
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-2005-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2005-STYUKYMD (7:2)
           MOVE    SYS-2005-STYUKYMD   TO  SYS-2005-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2005-REC
           ELSE
               INITIALIZE                   SYS-2005-REC
           END-IF
      *
      *    印刷ＤＢのＫＥＹ用     
           MOVE    WRK-SYSYMD          TO
                                   WRK-CONS-PRTKANRI-TBL-GROUP (1:8)
           MOVE    SYS-TIME            TO
                                   WRK-CONS-PRTKANRI-TBL-GROUP (9:)
      *     
      *    総括表印刷（社保）
           EVALUATE    TRUE
               WHEN    T01-DATAKBN4-VALUE    =   "T"
                   IF      SPA-NAI-PRTKBN1       =   SPACE
      *              直接請求する保険者の有無チェック
                     INITIALIZE                    WRK-HKNJANUM-AREA
                     MOVE    1                 TO  IDX
                     MOVE    "99999999"        TO
                                               WRK-HKNJANUM-S (IDX)
      *
                     IF      SPA-NAI-SKYYM     >=  200804
                       MOVE    SPACE             TO  RECE10-REC
                       INITIALIZE                    RECE10-REC
                       MOVE    SPA-HOSPNUM       TO  RECE10-HOSPNUM
                       MOVE    SPA-NAI-SKYYM     TO  RECE10-SKYYM
                       MOVE    "%"               TO  RECE10-NYUGAIKBN
                       MOVE    1                 TO  RECE10-TEISYUTUSAKI
                                                   RECE10-XXTEISYUTUSAKI 
                       MOVE    "1"               TO  RECE10-DIRECT-FLG
                       MOVE    RECE10-REC        TO  MCPDATA-REC
                       MOVE    "tbl_seikyu"      TO  MCP-TABLE
                       MOVE    "key81"           TO  MCP-PATHNAME
                       PERFORM 900-DBSELECT-SEC
                       IF      MCP-RC        =   ZERO
                           MOVE    "tbl_seikyu"        TO  MCP-TABLE
                           MOVE    "key81"             TO  MCP-PATHNAME
                           PERFORM 900-SEIKYU-READ-N-SEC
                       ELSE
                           MOVE    1                   TO  FLG-RECE10
                       END-IF
      *
                       PERFORM     UNTIL   FLG-RECE10  =   1
                           IF      IDX             =   1
                               MOVE    "00000000"      TO
                                                     WRK-HKNJANUM-S (1)
                           END-IF
                           ADD     1                   TO  IDX 
                           MOVE    RECE10-HKNJANUM-S   TO
                                                   WRK-HKNJANUM-S (IDX)
                           MOVE    "tbl_seikyu"        TO  MCP-TABLE
                           MOVE    "key81"             TO  MCP-PATHNAME
                           PERFORM 900-SEIKYU-READ-N-SEC
                       END-PERFORM    
                       MOVE    "tbl_seikyu"        TO  MCP-TABLE
                       MOVE    "key81"             TO  MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
                     END-IF
      *
                     PERFORM VARYING IDYY    FROM    1   BY  1
                             UNTIL   IDYY    >       10
                             OR      WRK-HKNJANUM-S (IDYY)
                                               =   SPACE
                       MOVE    "1"                 TO  WRK-SHELL-RECEKBN
                       IF      SPA-NAI-200501-DISASTER-SYAPRTK
                                               =   "1"
                           MOVE    "9"             TO  WRK-SHELL-SYOKBN
                           PERFORM 45013-SOKATU-SYORI-SEC
                       ELSE
                           MOVE    "1"             TO  WRK-SHELL-SYOKBN
                           PERFORM 45013-SOKATU-SYORI-SEC
      *
      *                    新潟県中越沖地震対応 
      *                    宮城県、岩手県地震対応 
      *                    中国・九州北部豪雨対応 
      *                    平成２１年台風第９号対応
      *                    平成２２年７月中国地方豪雨対応
      *                    鹿児島県奄美市等における局地的大雨対応
      *
                           IF  (( SPA-PREFNUM     =   "15"        ) AND
                                ( SPA-NAI-SKYYM  >=  200709       )    )
                           OR  (( SPA-PREFNUM     =   "03"   OR
                                                      "04"        ) AND
                                ( SPA-NAI-SKYYM  >=  200806       )    )
                           OR  (( SPA-PREFNUM     =   "35"   OR
                                                      "40"        ) AND
                                ( SPA-NAI-SKYYM  >=  200907       )    )
                           OR  (( SPA-PREFNUM     =   "28"   OR
                                                      "33"        ) AND
                                ( SPA-NAI-SKYYM  >=  200908       )    )
                           OR  (( SPA-PREFNUM     =   "34"        ) AND
                                ( SPA-NAI-SKYYM  >=  201007       )    )
                           OR  (( SPA-PREFNUM     =   "46"        ) AND
                                ( SPA-NAI-SKYYM  >=  201010       )    )
                           OR  (  SPA-NAI-SKYYM  >=  201101            )
                               MOVE    "1"         TO  WRK-SHELL-RECEKBN
                               MOVE    "2"         TO  WRK-SHELL-SYOKBN
                               PERFORM 45013-SOKATU-SYORI-SEC
                           END-IF
                         END-IF
                     END-PERFORM
                   END-IF     
           END-EVALUATE                                               
           MOVE    SPACE               TO  WRK-SHELL-SYOKBN
      *     
      *    総括表印刷（国保）
           EVALUATE    TRUE
               WHEN    T01-DATAKBN5-VALUE    =   "T" 
                   IF      SPA-NAI-PRTKBN2       =   SPACE
                       MOVE    "2"                 TO  WRK-SHELL-RECEKBN
                       PERFORM 45013-SOKATU-SYORI-SEC
                   END-IF    
           END-EVALUATE                                               
      *     
      *    総括表印刷（広域連合）
           EVALUATE    TRUE
               WHEN    T01-DATAKBN1-VALUE    =   "T" 
                   IF      SPA-NAI-PRTKBN4       =   SPACE
                       MOVE    "3"                 TO  WRK-SHELL-RECEKBN
                       PERFORM 45013-SOKATU-SYORI-SEC
                   END-IF    
           END-EVALUATE                                               
      *     
      *    レセ電総括表印刷（国保）
           EVALUATE    TRUE
               WHEN    T01-DATAKBN6-VALUE    =   "T" 
                   IF      SPA-NAI-PRTKBN3   NOT =   SPACE
                       MOVE    "2"                 TO  WRK-SHELL-RECEKBN
                       PERFORM 45014-RECEDEN-SOKATU-SYORI-SEC
                   END-IF    
           END-EVALUATE                                               
      *     
      *    レセ電総括表印刷（広域連合）
           EVALUATE    TRUE
               WHEN    T01-DATAKBN2-VALUE    =   "T" 
                   IF      SPA-NAI-PRTKBN5   NOT =   SPACE
                       MOVE    "3"                 TO  WRK-SHELL-RECEKBN
                       PERFORM 45014-RECEDEN-SOKATU-SYORI-SEC
                   END-IF    
           END-EVALUATE                                               
      *
      *    総括表印刷（公害）
           EVALUATE    TRUE
               WHEN    T01-DATAKBN7-VALUE    =   "T" 
                   PERFORM 45015-KOUGAI-SOKATU-SYORI-SEC
           END-EVALUATE                                               
      *
      *    総括表印刷（労災）
           EVALUATE    TRUE
               WHEN    T01-DATAKBN8-VALUE    =   "T" 
                   PERFORM 45016-RSI-SOKATU-SYORI-SEC
           END-EVALUATE                                               
      *     
      *    県単独事業（総括）
           EVALUATE    TRUE
               WHEN    T01-DATAKBN4-VALUE    =   "T" 
               WHEN    T01-DATAKBN5-VALUE    =   "T" 
               WHEN    T01-DATAKBN1-VALUE    =   "T" 
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       SPA-3004-MAX
                       IF    ( T01-DATAKBN4-VALUE      =   "T" ) 
                       AND   ( SPA-3004-CHKFLG (IDX 1) =   "T" )
                           PERFORM 45010-KENTAN-SOKATU-SYORI-SEC
                       END-IF 
                       IF    ( T01-DATAKBN5-VALUE      =   "T" ) 
                       AND   ( SPA-3004-CHKFLG (IDX 2) =   "T" )
                           PERFORM 45010-KENTAN-SOKATU-SYORI-SEC
                       END-IF
                       IF    ( T01-DATAKBN1-VALUE      =   "T" ) 
                       AND   ( SPA-3004-CHKFLG (IDX 4) =   "T" )
                           PERFORM 45010-KENTAN-SOKATU-SYORI-SEC
                       END-IF
                  END-PERFORM  
           END-EVALUATE
      *          
      *    県単独事業（請求書）        
           EVALUATE    TRUE
             WHEN    T01-DATAKBN3-VALUE    =   "T" 
      *
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-3004-MAX
                 IF      SPA-3004-CHKFLG (IDX 3) =   "T"                
                   PERFORM 45011-JOBKANRI-HENSYU-SEC
      *             
                   MOVE    SPACE           TO  SHELLTBL
      *                                   
                   MOVE    SPA-3004-SHELLID (IDX)
                                           TO  WRK-SHELL-SHELL
      *            ファイルデイレクトリ位置指定
                   INITIALIZE              ORCSMKPASSAREA
                   MOVE    WRK-SHELL-KENTAN
                                           TO  MKPASS-IN-01
                   CALL    "ORCSMKPASSSITE"    USING   ORCSMKPASSAREA
                   MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
      *                                     
                   MOVE    SPACE           TO  LNK-PRTKANRI-AREA
                   INITIALIZE                  LNK-PRTKANRI-AREA
                   ADD     1               TO  WRK-PRTKANRI-RENNUM
                   MOVE    WRK-PRTKANRI-RENNUM
                                           TO  LNK-PRTKANRI-RENNUM 
                   MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                           TO  LNK-PRTKANRI-TBL-KEY 
                   MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                           TO  LNK-PRTKANRI-TBL-GROUP
                   MOVE    SPA-NAI-SKYYM   TO  LNK-PRTKANRI-SRYYM 
                   MOVE    SPA-NAI-SKYYMD  TO  LNK-PRTKANRI-SKYYMD
                   MOVE    SPA-3004-SHELLID (IDX)
                                           TO  LNK-PRTKANRI-SHELLID
                   MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
      *************MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
                   MOVE    WRK-CONS-TERMID TO  LNK-PRTKANRI-TERMID
                   MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
      *************IF      SPA-3004-PRTNM (IDX)
      *************                    NOT =   SPACE
                       MOVE    SPA-3004-PRTNM (IDX)
                                           TO  LNK-PRTKANRI-PRTNM
      *************ELSE       
      *************    MOVE    SPA-NAI-1031-PRTNM
      *************                        TO  LNK-PRTKANRI-PRTNM
      *************END-IF
                   MOVE    LNK-PRTKANRI-RENNUM
                                           TO  SHELLTBL-ARG1       
                   MOVE    LNK-PRTKANRI-TBL-KEY
                                           TO  SHELLTBL-ARG2     
                   MOVE    LNK-PRTKANRI-TBL-GROUP
                                           TO  SHELLTBL-ARG3       
                   MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                           TO  SHELLTBL-ARG4       
                   MOVE    LNK-PRTKANRI-SRYYM
                                           TO  SHELLTBL-ARG5       
                   MOVE    LNK-PRTKANRI-SKYYMD
                                           TO  SHELLTBL-ARG6       
                   MOVE    LNK-PRTKANRI-SHELLID
                                           TO  SHELLTBL-ARG7       
                   MOVE    LNK-PRTKANRI-PRIORITY
                                           TO  SHELLTBL-ARG8      
                   MOVE    LNK-PRTKANRI-TERMID
                                           TO  SHELLTBL-ARG9       
                   MOVE    LNK-PRTKANRI-OPID
                                           TO  SHELLTBL-ARG10      
                   MOVE    LNK-PRTKANRI-PRTNM
                                           TO  SHELLTBL-ARG11      
      *                 
                   MOVE    WRK-CONS-JOB-JOBID
                                           TO  SHELLTBL-ARG12
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  SHELLTBL-ARG13         
grpsys             MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG14
                   MOVE    ERRFLPARA-BASENAME
                                           TO  SHELLTBL-ARG15
                   MOVE    "0"             TO  SHELLTBL-ARG16
      *
                   IF      SPA-GMN-SRYYMKBN-MAX
                                           =   ZERO
                       MOVE    "999999"        TO  SHELLTBL-ARG17
                       MOVE    "999999"        TO  SHELLTBL-ARG18
                   ELSE
                       MOVE    SPA-NAI-SRYYMKBN-ST
                                               (SPA-NAI-SRYYMKBN-SELNUM)
                                               TO  SHELLTBL-ARG17
                       MOVE    SPA-NAI-SRYYMKBN-ED
                                               (SPA-NAI-SRYYMKBN-SELNUM)
                                               TO  SHELLTBL-ARG18
                   END-IF
                   IF      SPA-MW          =   SPA-GINBEE
                       MOVE    SPA-3004-NAME (IDX)
                                           TO  SHELLTBL-REPOS-NAME
                   END-IF
      *
                   MOVE    SHELLTBL        TO  MCPDATA-REC
                   MOVE    "SHELL"         TO  MCP-FUNC
                   MOVE    "shell"         TO  MCP-TABLE
                   MOVE    "shell"         TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
                 END-IF
             END-PERFORM                                                    
           END-EVALUATE
      *
      *
      *    印刷処理
      *
           IF      SPA-TID1-FLG        =   "OK"    OR  "PV"
               PERFORM 45011-JOBKANRI-HENSYU-SEC
      *
               MOVE    0                   TO  WRK-STPAGE
               MOVE    99999               TO  WRK-EDPAGE
      *
               MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                           TO  SPA-T01-TBL-GROUP
      *             
               MOVE    SPACE               TO  SHELLTBL
               MOVE    "print_prv.sh"      TO  SHELLTBL-NAME
      *                               
               MOVE    SPACE               TO  LNK-PRTKANRI-AREA
               INITIALIZE                      LNK-PRTKANRI-AREA
               MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                           TO  LNK-PRTKANRI-TBL-KEY 
               MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                           TO  LNK-PRTKANRI-TBL-GROUP
               MOVE    ZERO                TO  LNK-PRTKANRI-RENNUM
               MOVE    ZERO                TO  LNK-PRTKANRI-SHORI-RENNUM
               MOVE    SPA-NAI-SKYYM       TO  LNK-PRTKANRI-SRYYM 
               MOVE    SPA-NAI-SKYYMD      TO  LNK-PRTKANRI-SKYYMD
               MOVE    "print_prv.sh"      TO  LNK-PRTKANRI-SHELLID
               MOVE    ZERO                TO  LNK-PRTKANRI-PRIORITY
      *****    MOVE    SPA-TERMID          TO  LNK-PRTKANRI-TERMID
               MOVE    WRK-CONS-TERMID     TO  LNK-PRTKANRI-TERMID
               MOVE    SPA-OPID            TO  LNK-PRTKANRI-OPID
               MOVE    LNK-PRTKANRI-RENNUM TO  SHELLTBL-ARG1       
               MOVE    LNK-PRTKANRI-TBL-KEY
                                           TO  SHELLTBL-ARG2     
               MOVE    LNK-PRTKANRI-TBL-GROUP
                                           TO  SHELLTBL-ARG3       
               MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                           TO  SHELLTBL-ARG4       
               MOVE    LNK-PRTKANRI-SRYYM  TO  SHELLTBL-ARG5       
               MOVE    LNK-PRTKANRI-SKYYMD TO  SHELLTBL-ARG6       
               MOVE    LNK-PRTKANRI-SHELLID
                                           TO  SHELLTBL-ARG7       
               MOVE    LNK-PRTKANRI-PRIORITY
                                           TO  SHELLTBL-ARG8      
               MOVE    LNK-PRTKANRI-TERMID TO  SHELLTBL-ARG9       
               MOVE    LNK-PRTKANRI-OPID   TO  SHELLTBL-ARG10      
               MOVE    LNK-PRTKANRI-PRTNM  TO  SHELLTBL-ARG11      
      *                 
               MOVE    ERRFLPARA-BASENAME  TO  SHELLTBL-ARG12
               MOVE    WRK-CONS-JOB-JOBID  TO  SHELLTBL-ARG13
               MOVE    WRK-CONS-JOB-SHELLID
                                           TO  SHELLTBL-ARG14
               MOVE    WRK-STPAGE          TO  SHELLTBL-ARG15
               MOVE    WRK-EDPAGE          TO  SHELLTBL-ARG16
               EVALUATE    SPA-TID1-FLG
               WHEN    "PV"
                   MOVE    "3"             TO  SHELLTBL-ARG17
      **       WHEN    "NO"
      **           MOVE    "3"             TO  SHELLTBL-ARG17
      *----(05.00.00)--ADD-START---
               WHEN    "OK"
                   IF      SPA-CLIENT-PRT-FLG  =  2
                       MOVE    "5"         TO  SHELLTBL-ARG17
                   ELSE
                       MOVE    "2"         TO  SHELLTBL-ARG17
                   END-IF
               END-EVALUATE
               MOVE    SPA-CLIENT-SHOWDIALOG
                                           TO  SHELLTBL-ARG22
      *----(05.00.00)--ADD-END-----
grpsys         MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG18
               MOVE    SHELLTBL            TO  MCPDATA-REC
               MOVE    "SHELL"             TO  MCP-FUNC
               MOVE    "shell"             TO  MCP-TABLE
               MOVE    "allways"           TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
           END-IF       
      *
      *    ＪＯＢＩＤ更新処理
      *
           PERFORM 45012-JOBID-UPD-SEC
      *
      *    印刷データ削除処理
      *
           PERFORM 45015-PRTDATA-DEL-SEC
      *     
      *    情報確認画面を表示                                                   
           PERFORM 490-KAKUNIN-SEC
           .
       450-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    総括処理        
      *****************************************************************
       45013-SOKATU-SYORI-SEC       SECTION.
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *             
           MOVE    SPACE           TO  SHELLTBL
           MOVE    "recept3.sh"    TO  SHELLTBL-NAME
      *                                 
           MOVE    SPACE           TO  LNK-PRTKANRI-AREA
           INITIALIZE                  LNK-PRTKANRI-AREA
           ADD     1               TO  WRK-PRTKANRI-RENNUM
           MOVE    WRK-PRTKANRI-RENNUM
                                   TO  LNK-PRTKANRI-RENNUM 
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY 
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-NAI-SKYYM   TO  LNK-PRTKANRI-SRYYM 
           MOVE    SPA-NAI-SKYYMD  TO  LNK-PRTKANRI-SKYYMD
           MOVE    "recept3.sh"    TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
      *****MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    WRK-CONS-TERMID TO  LNK-PRTKANRI-TERMID 
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
           MOVE    SPA-NAI-1031-PRTNM
                                   TO  LNK-PRTKANRI-PRTNM
           EVALUATE    WRK-SHELL-RECEKBN
               WHEN    "1"
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       5
                       IF      SPA-3004-KBNCD  (IDXX)
                                           =   900
                           MOVE    SPA-3004-PRTNM (IDXX)
                                               TO  LNK-PRTKANRI-PRTNM
                           MOVE    SPA-3004-MAX
                                               TO  IDXX
                       END-IF
                   END-PERFORM         
               WHEN    "2"
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       5
                       IF      SPA-3004-KBNCD  (IDXX)
                                           =   901
                           MOVE    SPA-3004-PRTNM (IDXX)
                                               TO  LNK-PRTKANRI-PRTNM
                           MOVE    SPA-3004-MAX
                                               TO  IDXX
                       END-IF
                   END-PERFORM         
               WHEN    "3"
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       5
                       IF      SPA-3004-KBNCD  (IDXX)
                                           =   904
                           MOVE    SPA-3004-PRTNM (IDXX)
                                               TO  LNK-PRTKANRI-PRTNM
                           MOVE    SPA-3004-MAX
                                               TO  IDXX
                       END-IF
                   END-PERFORM         
           END-EVALUATE
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG1       
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG2     
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG3       
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG4       
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG5       
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG6       
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SHELLTBL-ARG7       
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG8      
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG9       
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG10      
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SHELLTBL-ARG11      
      *                 
           MOVE    SPA-NAI-SKYYM   TO  WRK-SHELL-SRYYM 
           MOVE    WRK-CONS-TERMID TO  WRK-SHELL-TERMID
           MOVE    SPA-NAI-SKYYMD  TO  WRK-SHELL-SYSYMD    
           MOVE    WRK-SHELL-ARG1  TO  SHELLTBL-ARG12
           MOVE    WRK-SHELL-RECEKBN
                                   TO  SHELLTBL-ARG13
grpsys     MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG14
           IF      SPA-NAI-SKYYM   >=  200804
               MOVE    "0"             TO  SHELLTBL-ARG15
           ELSE
               IF      SYS-2005-KKHPRTKBN  =   "1"
                   MOVE    "1"             TO  SHELLTBL-ARG15
               ELSE
                   MOVE    "0"             TO  SHELLTBL-ARG15
               END-IF
           END-IF
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG16
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG17         
           MOVE    ERRFLPARA-BASENAME
                                   TO  SHELLTBL-ARG18
           MOVE    SPA-NAI-SKYYM   TO  SHELLTBL-ARG19
           IF      SPA-BEDSU       =   ZERO
               MOVE    "2"             TO  SHELLTBL-ARG20
           ELSE
               MOVE    "1"             TO  SHELLTBL-ARG20
           END-IF
           EVALUATE    WRK-SHELL-RECEKBN
               WHEN    "2"
                   MOVE    SPA-NAI-HKNJANUM    TO  SHELLTBL-ARG21
               WHEN    "3"
                   MOVE    SPA-NAI-HKNJANUM1   TO  SHELLTBL-ARG21
               WHEN    OTHER
                   MOVE    "00000000"          TO  SHELLTBL-ARG21
           END-EVALUATE
           EVALUATE    SPA-CHK11-VALUE ALSO    SPA-CHK12-VALUE
               WHEN    "T"             ALSO    "T"
                   MOVE    "0"             TO  SHELLTBL-ARG22
               WHEN    "T"             ALSO    "F"
                   MOVE    "1"             TO  SHELLTBL-ARG22
               WHEN    "F"             ALSO    "T"
                   MOVE    "2"             TO  SHELLTBL-ARG22
           END-EVALUATE
           EVALUATE    SPA-CHK21-VALUE ALSO    SPA-CHK22-VALUE
               WHEN    "T"             ALSO    "T"
                   MOVE    "0"             TO  SHELLTBL-ARG23
               WHEN    "T"             ALSO    "F"
                   MOVE    "1"             TO  SHELLTBL-ARG23
               WHEN    "F"             ALSO    "T"
                   MOVE    "2"             TO  SHELLTBL-ARG23
           END-EVALUATE
           IF      SYS-2005-MARUCHO-SYAPRTKBN
                                   =   "1"
               MOVE    "1"             TO  SHELLTBL-ARG24
           ELSE
               MOVE    "0"             TO  SHELLTBL-ARG24
           END-IF
      *    新潟県中越沖地震対応 
      *    宮城県、岩手県地震対応 
      *    中国・九州北部豪雨対応
      *    平成２１年台風第９号対応
           IF    ( WRK-SHELL-RECEKBN   =   "1" )
           AND   ( WRK-SHELL-SYOKBN    =   "9" )
               MOVE    "9"             TO  SHELLTBL-ARG25
           ELSE
               IF    ( WRK-SHELL-RECEKBN   =   "1" )
               AND   ( WRK-SHELL-SYOKBN    =   "2" )
                   MOVE    "1"             TO  SHELLTBL-ARG25      
               ELSE
                   MOVE    "0"             TO  SHELLTBL-ARG25
               END-IF
           END-IF 
           EVALUATE    SPA-CHK31-VALUE ALSO    SPA-CHK32-VALUE
               WHEN    "T"             ALSO    "T"
                   MOVE    "0"             TO  SHELLTBL-ARG26
               WHEN    "T"             ALSO    "F"
                   MOVE    "1"             TO  SHELLTBL-ARG26
               WHEN    "F"             ALSO    "T"
                   MOVE    "2"             TO  SHELLTBL-ARG26
           END-EVALUATE
      *
           EVALUATE    WRK-SHELL-RECEKBN
               WHEN    "1"
                   MOVE    WRK-HKNJANUM-S (IDYY)  TO  SHELLTBL-ARG27
               WHEN    OTHER
                   MOVE    "00000000"             TO  SHELLTBL-ARG27
           END-EVALUATE
      *
           IF      SPA-GMN-SRYYMKBN-MAX
                                   =   ZERO
               MOVE    "999999"        TO  SHELLTBL-ARG28
               MOVE    "999999"        TO  SHELLTBL-ARG29
           ELSE
               MOVE    SPA-NAI-SRYYMKBN-ST (SPA-NAI-SRYYMKBN-SELNUM)
                                       TO  SHELLTBL-ARG28
               MOVE    SPA-NAI-SRYYMKBN-ED (SPA-NAI-SRYYMKBN-SELNUM)
                                       TO  SHELLTBL-ARG29
           END-IF
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "monthly"       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ERROR MCP-RC=" MCP-RC 
           END-IF
      *
           .
       45013-SOKATU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    県単独事業（総括）処理        
      *****************************************************************
       45010-KENTAN-SOKATU-SYORI-SEC       SECTION.
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *            
           MOVE    SPACE       TO  SHELLTBL
      *                                   
           MOVE    SPA-3004-SHELLID (IDX)
                               TO  WRK-SHELL-SHELL
      *    ファイルデイレクトリ位置指定
           INITIALIZE              ORCSMKPASSAREA
           MOVE    WRK-SHELL-KENTAN
                               TO  MKPASS-IN-01
           CALL    "ORCSMKPASSSITE"    USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
      *
           MOVE    SPACE       TO  LNK-PRTKANRI-AREA
           INITIALIZE              LNK-PRTKANRI-AREA
           ADD     1           TO  WRK-PRTKANRI-RENNUM
           MOVE    WRK-PRTKANRI-RENNUM
                               TO  LNK-PRTKANRI-RENNUM 
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                               TO  LNK-PRTKANRI-TBL-KEY 
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                               TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-NAI-SKYYM   TO  LNK-PRTKANRI-SRYYM 
           MOVE    SPA-NAI-SKYYMD  TO  LNK-PRTKANRI-SKYYMD
           MOVE    SPA-3004-SHELLID (IDX)
                                   TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
           MOVE    WRK-CONS-TERMID TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
           MOVE    SPA-3004-PRTNM (IDX)
                                   TO  LNK-PRTKANRI-PRTNM
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG1       
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG2     
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG3       
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG4       
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG5       
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG6       
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SHELLTBL-ARG7       
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG8      
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG9       
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG10      
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SHELLTBL-ARG11      
      *                 
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG12
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG13         
           MOVE    "0"         TO  WRK-SHELL-RECEKBN
           IF      SPA-3004-CHKFLG (IDX 1)
                                       =   "T"
               MOVE    "1"         TO  WRK-SHELL-RECEKBN
           END-IF    
           IF      SPA-3004-CHKFLG (IDX 2)
                                       =   "T"
               MOVE    "2"         TO  WRK-SHELL-RECEKBN
           END-IF    
           IF      SPA-3004-CHKFLG (IDX 4)
                                       =   "T"
               MOVE    "3"         TO  WRK-SHELL-RECEKBN
           END-IF    
           MOVE    WRK-SHELL-RECEKBN
                                   TO  SHELLTBL-ARG14
grpsys     MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG15
           MOVE    ERRFLPARA-BASENAME
                                   TO  SHELLTBL-ARG16
           MOVE    SPA-NAI-SKYYM   TO  SHELLTBL-ARG17
           IF      SPA-3004-CHKFLG (IDX 2)
                                       =   "T"
               IF      SPA-DATAKBN5-VALUE  =   "T" 
                   MOVE    SPA-NAI-HKNJANUM
                                       TO  SHELLTBL-ARG18
               ELSE
                   MOVE    "00000000"  TO  SHELLTBL-ARG18
               END-IF
           END-IF
           IF      SPA-3004-CHKFLG (IDX 4)
                                       =   "T"
               IF      SPA-DATAKBN1-VALUE  =   "T" 
                   MOVE    SPA-NAI-HKNJANUM1
                                       TO  SHELLTBL-ARG18
               ELSE
                   MOVE    "00000000"  TO  SHELLTBL-ARG18
               END-IF
           END-IF
           IF      SPA-3004-CHKFLG (IDX 1)
                                       =   "T"
               EVALUATE    SPA-CHK11-VALUE ALSO    SPA-CHK12-VALUE
                   WHEN    "T"             ALSO    "T"
                       MOVE    "0"             TO  SHELLTBL-ARG19
                   WHEN    "T"             ALSO    "F"
                       MOVE    "1"             TO  SHELLTBL-ARG19
                   WHEN    "F"             ALSO    "T"
                       MOVE    "2"             TO  SHELLTBL-ARG19
               END-EVALUATE
           END-IF
           IF      SPA-3004-CHKFLG (IDX 2)
                                       =   "T"
               EVALUATE    SPA-CHK21-VALUE ALSO    SPA-CHK22-VALUE
                   WHEN    "T"             ALSO    "T"
                       MOVE    "0"             TO  SHELLTBL-ARG19
                   WHEN    "T"             ALSO    "F"
                       MOVE    "1"             TO  SHELLTBL-ARG19
                   WHEN    "F"             ALSO    "T"
                       MOVE    "2"             TO  SHELLTBL-ARG19
               END-EVALUATE
           END-IF    
           IF      SPA-3004-CHKFLG (IDX 4)
                                       =   "T"
               EVALUATE    SPA-CHK31-VALUE ALSO    SPA-CHK32-VALUE
                   WHEN    "T"             ALSO    "T"
                       MOVE    "0"             TO  SHELLTBL-ARG19
                   WHEN    "T"             ALSO    "F"
                       MOVE    "1"             TO  SHELLTBL-ARG19
                   WHEN    "F"             ALSO    "T"
                       MOVE    "2"             TO  SHELLTBL-ARG19
               END-EVALUATE
           END-IF    
      *
           IF      SPA-GMN-SRYYMKBN-MAX
                                   =   ZERO
               MOVE    "999999"        TO  SHELLTBL-ARG20
               MOVE    "999999"        TO  SHELLTBL-ARG21
           ELSE
               MOVE    SPA-NAI-SRYYMKBN-ST (SPA-NAI-SRYYMKBN-SELNUM)
                                       TO  SHELLTBL-ARG20
               MOVE    SPA-NAI-SRYYMKBN-ED (SPA-NAI-SRYYMKBN-SELNUM)
                                       TO  SHELLTBL-ARG21
           END-IF
      *
           IF      SPA-MW          =   SPA-GINBEE
               MOVE    SPA-3004-NAME (IDX)
                                       TO  SHELLTBL-REPOS-NAME
           END-IF
      *
           MOVE    SHELLTBL    TO  MCPDATA-REC
           MOVE    "SHELL"     TO  MCP-FUNC
           MOVE    "shell"     TO  MCP-TABLE
           MOVE    "shell"     TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
            .
       45010-KENTAN-SOKATU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ開始セット処理
      *****************************************************************
       45011-JOBKANRI-HENSYU-SEC       SECTION.
      *
           ADD     1               TO  WRK-CONS-JOB-JOBID
      *       
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
           MOVE    SPA-T01-JOB-KARTE-UID
                                   TO  JOB-KARTE-UID
           MOVE    SPA-T01-JOB-ORCA-UID
                                   TO  JOB-ORCA-UID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           .
       45011-JOBKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＪＯＢＩＤ更新処理
      *****************************************************************
       45012-JOBID-UPD-SEC    SECTION.
      *
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-3003-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-3003-STYUKYMD (7:2)
           MOVE    SYS-3003-STYUKYMD   TO  SYS-3003-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC
           ELSE       
               MOVE    SPACE               TO  SYS-3003-REC 
               INITIALIZE                      SYS-3003-REC
               MOVE    "3003"              TO  SYS-3003-KANRICD
               MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
               MOVE    ZERO                TO  SYS-3003-STYUKYMD
               MOVE    ALL "9"             TO  SYS-3003-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           END-IF    
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SYS-3003-TBL-KEY
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                       TO  SYS-3003-TBL-GROUP
           MOVE    WRK-CONS-JOB-JOBID  TO  SYS-3003-JOBID                                      
      *
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-SYSKANRI        =   ZERO     
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           ELSE
               MOVE    "DBINSERT"          TO  MCP-FUNC
           END-IF         
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "SYSKANRI 3003  ERR KEY = ["
                                  MCP-RC  " " SYS-3003-KEY  "]"   
      *****    MOVE    "1010"          TO  SPA-ERRCD
           END-IF
      *
           .
       45012-JOBID-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       45013-PRINT-SYORI-SEC           SECTION.
      *
           MOVE    0                   TO  WRK-STPAGE
           MOVE    99999               TO  WRK-EDPAGE
      *             
           MOVE    SPACE               TO  SHELLTBL
           MOVE    "print_prv.sh"      TO  SHELLTBL-NAME
      ***
           MOVE    SPACE               TO  LNK-PRTKANRI-AREA
           INITIALIZE                      LNK-PRTKANRI-AREA
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  LNK-PRTKANRI-TBL-KEY 
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                       TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    ZERO                TO  LNK-PRTKANRI-RENNUM
           MOVE    ZERO                TO  LNK-PRTKANRI-SHORI-RENNUM
           MOVE    SPA-NAI-SKYYM       TO  LNK-PRTKANRI-SRYYM 
           MOVE    SPA-NAI-SKYYMD      TO  LNK-PRTKANRI-SKYYMD
           MOVE    "print_prv.sh"      TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO                TO  LNK-PRTKANRI-PRIORITY
      *****MOVE    SPA-TERMID          TO  LNK-PRTKANRI-TERMID
           MOVE    WRK-CONS-TERMID     TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID            TO  LNK-PRTKANRI-OPID
      ***
           MOVE    LNK-PRTKANRI-RENNUM TO  SHELLTBL-ARG1       
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SHELLTBL-ARG2     
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SHELLTBL-ARG3       
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SHELLTBL-ARG4       
           MOVE    LNK-PRTKANRI-SRYYM  TO  SHELLTBL-ARG5       
           MOVE    LNK-PRTKANRI-SKYYMD TO  SHELLTBL-ARG6       
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SHELLTBL-ARG7       
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SHELLTBL-ARG8      
           MOVE    LNK-PRTKANRI-TERMID TO  SHELLTBL-ARG9       
           MOVE    LNK-PRTKANRI-OPID   TO  SHELLTBL-ARG10      
           MOVE    LNK-PRTKANRI-PRTNM  TO  SHELLTBL-ARG11      
      *             
           MOVE    ERRFLPARA-BASENAME  TO  SHELLTBL-ARG12
           MOVE    WRK-CONS-JOB-JOBID  TO  SHELLTBL-ARG13
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-ARG14
           MOVE    WRK-STPAGE          TO  SHELLTBL-ARG15
           MOVE    WRK-EDPAGE          TO  SHELLTBL-ARG16
               EVALUATE    SPA-TID1-FLG
               WHEN    "PV"
                   MOVE    "3"             TO  SHELLTBL-ARG17
      **       WHEN    "NO"
      **           MOVE    "3"             TO  SHELLTBL-ARG17
      *----(05.00.00)--ADD-START---
               WHEN    "OK"
                   IF      SPA-CLIENT-PRT-FLG  =  2
                       MOVE    "5"         TO  SHELLTBL-ARG17
                   ELSE
                       MOVE    "2"         TO  SHELLTBL-ARG17
                   END-IF
               END-EVALUATE
               MOVE    SPA-CLIENT-SHOWDIALOG
                                           TO  SHELLTBL-ARG22
      *----(05.00.00)--ADD-END-----
grpsys     MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG18
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       45013-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電総括（国保・広域連合）処理        
      *****************************************************************
       45014-RECEDEN-SOKATU-SYORI-SEC       SECTION.
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *             
           MOVE    SPACE           TO  SHELLTBL
           MOVE    "receden_sokatu.sh"
                                   TO  SHELLTBL-NAME
      *                                 
           MOVE    SPACE           TO  LNK-PRTKANRI-AREA
           INITIALIZE                  LNK-PRTKANRI-AREA
           ADD     1               TO  WRK-PRTKANRI-RENNUM
           MOVE    WRK-PRTKANRI-RENNUM
                                   TO  LNK-PRTKANRI-RENNUM 
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY 
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-NAI-SKYYM   TO  LNK-PRTKANRI-SRYYM 
           MOVE    SPA-NAI-SKYYMD  TO  LNK-PRTKANRI-SKYYMD
           MOVE    "receden_sokatu.sh"
                                   TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
           MOVE    WRK-CONS-TERMID TO  LNK-PRTKANRI-TERMID 
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
           EVALUATE    WRK-SHELL-RECEKBN
               WHEN    "2"
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       5
                       IF      SPA-3004-KBNCD  (IDXX)
                                               =   902
                           MOVE    SPA-3004-PRTNM (IDXX)
                                               TO  LNK-PRTKANRI-PRTNM
                           MOVE    SPA-3004-MAX    TO  IDXX
                       END-IF
               END-PERFORM         
               WHEN    "3"
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       5
                       IF      SPA-3004-KBNCD  (IDXX)
                                               =   905
                           MOVE    SPA-3004-PRTNM (IDXX)
                                               TO  LNK-PRTKANRI-PRTNM
                           MOVE    SPA-3004-MAX    TO  IDXX
                       END-IF
               END-PERFORM         
           END-EVALUATE
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG1       
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG2     
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG3       
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG4       
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG5       
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG6       
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SHELLTBL-ARG7       
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG8      
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG9       
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG10      
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SHELLTBL-ARG11      
      *                 
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG12
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG13
grpsys     MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG14
           MOVE    ERRFLPARA-BASENAME
                                   TO  SHELLTBL-ARG15
           MOVE    WRK-SHELL-RECEKBN
                                   TO  SHELLTBL-ARG16
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "monthly"       TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ERROR MCP-RC=" MCP-RC 
           END-IF
      *
           .
       45014-RECEDEN-SOKATU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    総括（公害）処理        
      *****************************************************************
       45015-KOUGAI-SOKATU-SYORI-SEC       SECTION.
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *             
           MOVE    SPACE           TO  SHELLTBL
           MOVE    "recept16.sh"   TO  SHELLTBL-NAME
      *                                 
           MOVE    SPACE           TO  LNK-PRTKANRI-AREA
           INITIALIZE                  LNK-PRTKANRI-AREA
           ADD     1               TO  WRK-PRTKANRI-RENNUM
           MOVE    WRK-PRTKANRI-RENNUM
                                   TO  LNK-PRTKANRI-RENNUM 
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY 
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-NAI-SKYYM   TO  LNK-PRTKANRI-SRYYM 
           MOVE    SPA-NAI-SKYYMD  TO  LNK-PRTKANRI-SKYYMD
           MOVE    "recept16.sh"   TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
      *****MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    WRK-CONS-TERMID TO  LNK-PRTKANRI-TERMID 
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       5
               IF      SPA-3004-KBNCD  (IDXX)
                                           =   903
                   MOVE    SPA-3004-PRTNM (IDXX)
                                           TO  LNK-PRTKANRI-PRTNM
                   MOVE    SPA-3004-MAX    TO  IDXX
               END-IF
           END-PERFORM         
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG1       
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG2     
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG3       
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG4       
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG5       
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG6       
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SHELLTBL-ARG7       
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG8      
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG9       
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG10      
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SHELLTBL-ARG11      
      *                 
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG12
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG13
grpsys     MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG14
           MOVE    ERRFLPARA-BASENAME
                                   TO  SHELLTBL-ARG15
           MOVE    SPA-NAI-4100-KGISKYKBN
                                   TO  SHELLTBL-ARG16
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "monthly"       TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ERROR MCP-RC=" MCP-RC 
           END-IF
      *
           .
       45015-KOUGAI-SOKATU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    総括（労災）処理        
      *****************************************************************
       45016-RSI-SOKATU-SYORI-SEC       SECTION.
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *             
           MOVE    SPACE           TO  SHELLTBL
           MOVE    "recept17.sh"   TO  SHELLTBL-NAME
      *                                 
           MOVE    SPACE           TO  LNK-PRTKANRI-AREA
           INITIALIZE                  LNK-PRTKANRI-AREA
           ADD     1               TO  WRK-PRTKANRI-RENNUM
           MOVE    WRK-PRTKANRI-RENNUM
                                   TO  LNK-PRTKANRI-RENNUM 
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY 
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-NAI-SKYYM   TO  LNK-PRTKANRI-SRYYM 
           MOVE    SPA-NAI-SKYYMD  TO  LNK-PRTKANRI-SKYYMD
           MOVE    "recept17.sh"   TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
           MOVE    WRK-CONS-TERMID TO  LNK-PRTKANRI-TERMID 
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       6
               IF      SPA-3004-KBNCD  (IDXX)
                                           =   906
                   MOVE    SPA-3004-PRTNM (IDXX)
                                           TO  LNK-PRTKANRI-PRTNM
                   MOVE    SPA-3004-MAX    TO  IDXX
               END-IF
           END-PERFORM         
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG1       
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG2     
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG3       
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG4       
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG5       
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG6       
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SHELLTBL-ARG7       
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG8      
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG9       
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG10      
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SHELLTBL-ARG11      
      *                 
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG12
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG13
grpsys     MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG14
           MOVE    ERRFLPARA-BASENAME
                                   TO  SHELLTBL-ARG15
           MOVE    SPA-NAI-200501-ROUSAIPRTKBN
                                   TO  SHELLTBL-ARG16
           MOVE    SPA-NAI-200501-FRAME-ROUSAIPRTK
                                   TO  SHELLTBL-ARG17
           MOVE    SPA-NAI-200501-RSI-INVOICE-KBN
                                   TO  SHELLTBL-ARG18
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "monthly"       TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ERROR MCP-RC=" MCP-RC 
           END-IF
      *
           .
       45016-RSI-SOKATU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷データ削除処理
      *****************************************************************
       45015-PRTDATA-DEL-SEC           SECTION.
      *
      *****PERFORM 45011-JOBKANRI-HENSYU-SEC
      *             
           MOVE    SPACE               TO  SHELLTBL
           MOVE    "prtdbdel.sh"       TO  SHELLTBL-NAME
           MOVE    WRK-CONS-JOB-JOBID  TO  SHELLTBL-ARG1
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-ARG2
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG3
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "monthly"           TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       45015-PRTDATA-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電処理前チェック処理
      *****************************************************************
       460-RECEDEN-GID-SEC                SECTION.
      *
      *    入力チェック
           MOVE    1               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  460-RECEDEN-GID-EXT
           END-IF
      *    
           IF      SPA-GMN-PRTKBN  =   "6" OR  "7" OR  "8" OR  "9"
             CONTINUE
           ELSE
      *      主科設定時の主科未設定データの存在チェック
             MOVE    ZERO                TO  FLG-SYUKA-CHK
      *   
             MOVE    SPACE               TO  RECE10-REC
             INITIALIZE                      RECE10-REC
             MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
             MOVE    SPA-NAI-SKYYM       TO  RECE10-SKYYM
             MOVE    "00"                TO  RECE10-RECEKA
             EVALUATE    MCP-WIDGET
               WHEN    "B06"
                   IF     SPA-NAI-2009-SYUKA-SYAHO  =   "1"
                       MOVE    1               TO  RECE10-TEISYUTUSAKI
                       PERFORM 4602-SYUKA-CHK-SEC
                   END-IF
               WHEN    "B07"
                   IF      SPA-NAI-2009-SYUKA-KOKUHO =   "1"
                       MOVE    2               TO  RECE10-TEISYUTUSAKI
                       PERFORM 4602-SYUKA-CHK-SEC
                   END-IF
                   IF    ( FLG-SYUKA-CHK             =   ZERO   )
                   AND   ( SPA-NAI-200501-KOUIKIKBN  =   "1"    )
                   AND   ( SPA-NAI-SKYYM             >=  200804 )
                   AND   ( SPA-NAI-2009-SYUKA-KOUIKI =   "1"    )
                       MOVE    SPACE           TO  RECE10-REC
                       INITIALIZE                  RECE10-REC
                       MOVE    SPA-HOSPNUM     TO  RECE10-HOSPNUM
                       MOVE    SPA-NAI-SKYYM   TO  RECE10-SKYYM
                       MOVE    "00"            TO  RECE10-RECEKA
                       MOVE    6               TO  RECE10-TEISYUTUSAKI
                       PERFORM 4602-SYUKA-CHK-SEC
                   END-IF
               WHEN    "B08"
                   IF      SPA-NAI-2009-SYUKA-KOUIKI =   "1"
                       MOVE    6               TO  RECE10-TEISYUTUSAKI
                       PERFORM 4602-SYUKA-CHK-SEC
                   END-IF
             END-EVALUATE
             IF      FLG-SYUKA-CHK       =   1
               MOVE    "0011"              TO  SPA-ERRCD
               GO  TO  460-RECEDEN-GID-EXT
             END-IF
      *    
      *      月遅れ分のレセ電データの存在チェック
             MOVE    ZERO                TO  FLG-RECEDEN-CHK
                                             FLG-RECEDEN-201203-CHK
      *   
             MOVE    SPACE               TO  RECE10-REC
             INITIALIZE                      RECE10-REC
             MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
             MOVE    SPA-NAI-SKYYM       TO  RECE10-SKYYM
             EVALUATE    MCP-WIDGET
               WHEN    "B06"
                   MOVE    1               TO  RECE10-TEISYUTUSAKI
                   EVALUATE    SPA-GMN-NYUGAIKBN
                                   ALSO    SPA-NAI-2005-SYARECEKBN
                       WHEN    "0" ALSO    "1"
                       WHEN    "2" ALSO    "1"
                       WHEN    "2" ALSO    "3"
                           MOVE    "2"         TO  RECE10-NYUGAIKBN
                       WHEN    "0" ALSO    "2"
                       WHEN    "1" ALSO    "2"
                       WHEN    "1" ALSO    "3"
                           MOVE    "1"         TO  RECE10-NYUGAIKBN
                       WHEN    "0" ALSO    "3"
                           MOVE    "%"         TO  RECE10-NYUGAIKBN
                       WHEN    OTHER
                           MOVE    SPACE       TO  RECE10-NYUGAIKBN
                   END-EVALUATE
                   PERFORM 4601-RECEDEN-CHK-SEC
               WHEN    "B07"
                   MOVE    2               TO  RECE10-TEISYUTUSAKI
                   EVALUATE    SPA-GMN-NYUGAIKBN
                                   ALSO    SPA-NAI-2005-KKHRECEKBN
                       WHEN    "0" ALSO    "1"
                       WHEN    "2" ALSO    "1"
                       WHEN    "2" ALSO    "3"
                           MOVE    "2"         TO  RECE10-NYUGAIKBN
                       WHEN    "0" ALSO    "2"
                       WHEN    "1" ALSO    "2"
                       WHEN    "1" ALSO    "3"
                           MOVE    "1"         TO  RECE10-NYUGAIKBN
                       WHEN    "0" ALSO    "3"
                           MOVE    "%"         TO  RECE10-NYUGAIKBN
                       WHEN    OTHER
                           MOVE    SPACE       TO  RECE10-NYUGAIKBN
                   END-EVALUATE
                   PERFORM 4601-RECEDEN-CHK-SEC
                   IF    ( FLG-RECEDEN-CHK           =   ZERO   )
                   AND   ( SPA-NAI-200501-KOUIKIKBN  =   "1"    )
                   AND   ( SPA-NAI-SKYYM             >=  200804 )
                       MOVE    SPACE           TO  RECE10-REC
                       INITIALIZE                  RECE10-REC
                       MOVE    SPA-HOSPNUM     TO  RECE10-HOSPNUM
                       MOVE    SPA-NAI-SKYYM   TO  RECE10-SKYYM
                       MOVE    6               TO  RECE10-TEISYUTUSAKI
                       EVALUATE    SPA-GMN-NYUGAIKBN
                                   ALSO    SPA-NAI-200501-KOUIKIRECEKBN
                           WHEN    "0" ALSO    "1"
                           WHEN    "2" ALSO    "1"
                           WHEN    "2" ALSO    "3"
                               MOVE    "2"         TO  RECE10-NYUGAIKBN
                           WHEN    "0" ALSO    "2"
                           WHEN    "1" ALSO    "2"
                           WHEN    "1" ALSO    "3"
                               MOVE    "1"         TO  RECE10-NYUGAIKBN
                           WHEN    "0" ALSO    "3"
                               MOVE    "%"         TO  RECE10-NYUGAIKBN
                           WHEN    OTHER
                               MOVE    SPACE       TO  RECE10-NYUGAIKBN
                       END-EVALUATE
                       PERFORM 4601-RECEDEN-CHK-SEC
                   END-IF
               WHEN    "B08"
                   MOVE    6               TO  RECE10-TEISYUTUSAKI
                   EVALUATE    SPA-GMN-NYUGAIKBN
                                   ALSO    SPA-NAI-200501-KOUIKIRECEKBN
                       WHEN    "0" ALSO    "1"
                       WHEN    "2" ALSO    "1"
                       WHEN    "2" ALSO    "3"
                           MOVE    "2"         TO  RECE10-NYUGAIKBN
                       WHEN    "0" ALSO    "2"
                       WHEN    "1" ALSO    "2"
                       WHEN    "1" ALSO    "3"
                           MOVE    "1"         TO  RECE10-NYUGAIKBN
                       WHEN    "0" ALSO    "3"
                           MOVE    "%"         TO  RECE10-NYUGAIKBN
                       WHEN    OTHER
                           MOVE    SPACE       TO  RECE10-NYUGAIKBN
                   END-EVALUATE
                   PERFORM 4601-RECEDEN-CHK-SEC
               WHEN    "B17"
                   MOVE    3               TO  RECE10-TEISYUTUSAKI
                   EVALUATE    SPA-GMN-NYUGAIKBN
                                   ALSO    SPA-NAI-200501-ROURECEKBN
                       WHEN    "0" ALSO    "1"
                       WHEN    "2" ALSO    "1"
                       WHEN    "2" ALSO    "3"
                           MOVE    "2"         TO  RECE10-NYUGAIKBN
                       WHEN    "0" ALSO    "2"
                       WHEN    "1" ALSO    "2"
                       WHEN    "1" ALSO    "3"
                           MOVE    "1"         TO  RECE10-NYUGAIKBN
                       WHEN    "0" ALSO    "3"
                           MOVE    "%"         TO  RECE10-NYUGAIKBN
                       WHEN    OTHER
                           MOVE    SPACE       TO  RECE10-NYUGAIKBN
                   END-EVALUATE
                   PERFORM 4601-RECEDEN-CHK-SEC
             END-EVALUATE
           END-IF
      * 
      *    ジョブ管理チェック処理
           IF      SPA-ERRCD       =   SPACE
               PERFORM 4501-JOBKANRI-CHK-SEC
               IF      SPA-ERRCD       =   SPACE
                   MOVE    MCP-WIDGET          TO  SPA-NAI-WIDGET
      *            ガイドメッセージセット
                   EVALUATE    MCP-WIDGET    ALSO    SPA-GMN-NYUGAIKBN
                       WHEN    "B06"         ALSO    ZERO
                           MOVE    "1010"              TO  SPA-TIDCD
                       WHEN    "B06"         ALSO    "1"
                           MOVE    "1015"              TO  SPA-TIDCD
                       WHEN    "B06"         ALSO    "2"
                           MOVE    "1016"              TO  SPA-TIDCD
                       WHEN    "B07"         ALSO    ZERO
                           IF    ( SPA-NAI-200501-KOUIKIKBN
                                                     =   "1"    )
                           AND   ( SPA-NAI-SKYYM     >=  200804 )
                               MOVE    "1025"              TO  SPA-TIDCD
                           ELSE
                               MOVE    "1014"              TO  SPA-TIDCD
                           END-IF
                       WHEN    "B07"         ALSO    "1"
                           IF    ( SPA-NAI-200501-KOUIKIKBN
                                                     =   "1"    )
                           AND   ( SPA-NAI-SKYYM     >=  200804 )
                               MOVE    "1023"              TO  SPA-TIDCD
                           ELSE
                               MOVE    "1017"              TO  SPA-TIDCD
                           END-IF
                       WHEN    "B07"         ALSO    "2"
                           IF    ( SPA-NAI-200501-KOUIKIKBN
                                                     =   "1"    )
                           AND   ( SPA-NAI-SKYYM     >=  200804 )
                               MOVE    "1024"              TO  SPA-TIDCD
                           ELSE
                               MOVE    "1018"              TO  SPA-TIDCD
                           END-IF
                       WHEN    "B08"         ALSO    ZERO
                           MOVE    "1022"              TO  SPA-TIDCD
                       WHEN    "B08"         ALSO    "1"
                           MOVE    "1020"              TO  SPA-TIDCD
                       WHEN    "B08"         ALSO    "2"
                           MOVE    "1021"              TO  SPA-TIDCD
                       WHEN    "B17"         ALSO    ZERO
                           MOVE    "1027"              TO  SPA-TIDCD
                       WHEN    "B17"         ALSO    "1"
                           MOVE    "1028"              TO  SPA-TIDCD
                       WHEN    "B17"         ALSO    "2"
                           MOVE    "1029"              TO  SPA-TIDCD
                       WHEN    "B18"         ALSO    ZERO
                           MOVE    "1030"              TO  SPA-TIDCD
                       WHEN    "B18"         ALSO    "1"
                           IF      SPA-GMN-PRTKBN    =   "8"
                               MOVE    "1031"              TO  SPA-TIDCD
                           ELSE
                               MOVE    "1033"              TO  SPA-TIDCD
                           END-IF
                       WHEN    "B18"         ALSO    "2"
                           MOVE    "1032"              TO  SPA-TIDCD
                   END-EVALUATE
               END-IF
           END-IF
      *
           .
       460-RECEDEN-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    月遅れ分のレセ電データの存在チェック処理
      *****************************************************************
       4601-RECEDEN-CHK-SEC            SECTION.
      *
           IF      RECE10-NYUGAIKBN    =   SPACE
               GO  TO  4601-RECEDEN-CHK-EXT
           END-IF
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key69"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key69"             TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE10
           END-IF
      *
           PERFORM                 UNTIL   FLG-RECE10      =   1
                                   OR      FLG-RECEDEN-CHK =   1
      *        未確定分をレセ電データに記録するか
               IF    (  SPA-NAI-200501-RECEDEN-DISASTER
                                           =   "0"              )
               AND   ( RECE10-ETC-YKZFTN (1)
                                           =   6   OR  7  OR  8 )
                   CONTINUE
               ELSE
      *            労災のとき
                   IF      RECE10-TEISYUTUSAKI =   "3"
      *              ０９５９３で始まるコードがあるレセプトは対象外
                     IF      RECE10-FLG (1)        =   1
                       CONTINUE
                     ELSE 
      *                平成２５年６月以前またはアフターケアと公務災害は対象外
                       IF    ( RECE10-SRYYM        <
                                               WRK-CONS-SRYYM-201307   ) 
                       OR    ( RECE10-RECESYUBETU  =   7021   OR  7051
                                                   OR  7061            )
                           CONTINUE
                       ELSE
      *                    労災指定医療機関でないときは対象外
                           MOVE    RECE10-SRYYM    TO
                                                   WRK-SYS-SRYYMD (1:6)
                           MOVE    "01"            TO
                                                   WRK-SYS-SRYYMD (7:2)
      *
                           PERFORM VARYING IDZZ    FROM    1   BY  1
                                   UNTIL   IDZZ    >  SPA-NAI-4001-MAX
                               IF    ( SPA-NAI-4001-STYUKYMD (IDZZ)
                                                   <=  WRK-SYS-SRYYMD )
                               AND   ( SPA-NAI-4001-EDYUKYMD (IDZZ)
                                                   >=  WRK-SYS-SRYYMD )
                                   IF      SPA-NAI-4001-RSIIRYOKBN
                                                               (IDZZ)
                                                       =   "0"
                                       CONTINUE
                                   ELSE
                                       PERFORM 46011-RECEDEN-CHK-SEC
                                   END-IF
                                   MOVE     SPA-NAI-4001-MAX
                                                       TO  IDZZ
                               END-IF
                           END-PERFORM
                       END-IF
                     END-IF
                   ELSE
                       PERFORM 46011-RECEDEN-CHK-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key69"             TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-READ-N-SEC
           END-PERFORM
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key69"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    平成２４年３月以前の月後れ分が改正パッチ後に再作成されているか
           IF      FLG-RECEDEN-201203-CHK
                                       =   1
               MOVE    "0020"              TO  SPA-ERRCD
           END-IF
           .
       4601-RECEDEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電データ月遅れ分存在チェック処理
      *****************************************************************
       46011-RECEDEN-CHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  RECEDEN-REC
           INITIALIZE                      RECEDEN-REC
           MOVE    RECE10-KEY          TO  RECEDEN-KEY
           MOVE    "1"                 TO  RECEDEN-RECKBN
           MOVE    RECEDEN-REC         TO  MCPDATA-REC
           PERFORM 900-RECEDEN-READ-SEC
           IF      FLG-RECEDEN         =   1
               MOVE    1                   TO  FLG-RECEDEN-CHK
           ELSE
      *        平成２４年３月以前の月後れ分が改正パッチ前の作成か
               IF    ( SPA-NAI-SKYYM   >   201203     )
               AND   ( RECE10-SRYYM    <=  201203     )
               AND   ( RECE10-CREYMD   <   "20120423" )
                  MOVE    1               TO  FLG-RECEDEN-201203-CHK
               END-IF
           END-IF
           .
       46011-RECEDEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科未設定データ存在チェック処理
      *****************************************************************
       4602-SYUKA-CHK-SEC            SECTION.
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key68"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key68"             TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE10
           END-IF
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key68"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-RECE10          =   ZERO
               MOVE    1                   TO  FLG-SYUKA-CHK
           END-IF
           .
       4602-SYUKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電出力処理（医保分）、ＥＦファイル処理
      *****************************************************************
       460-RECEDEN-SEC                SECTION.
      *
           INITIALIZE                  SPA-T01-TBL-GROUP
           MOVE    ZERO            TO  SPA-T01-TIMER-FLG
      *
      *    クライアント保存削除処理             
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "D"             TO  SFILESV-MODE
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SFILESV-TBL-KEY
           MOVE    SPA-NAI-SKYYM   TO  SFILESV-SRYYM   (1)
           MOVE    WRK-CONS-BTPARA-SCRIPTID
                                   TO  SFILESV-SHELLID (1)
           CALL    "ORCSFILESV"    USING
                                       ORCSFILESVAREA
                                       SPA-AREA
      * 
      *    ジョブ管理削除処理
           MOVE    "ALL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "1004"              TO  SPA-ERRCD
               GO  TO  460-RECEDEN-EXT
           END-IF             
      *
      *    パラメタＤＢ削除処理
           INITIALIZE                  BTPARA-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  BTPARA-SHELLID
           MOVE    SPA-HOSPNUM     TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"      TO  MCP-FUNC
           MOVE    "tbl_btpara"    TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          >   ZERO
               MOVE    "1001"          TO  SPA-ERRCD
               GO  TO  460-RECEDEN-EXT
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  WRK-SYSYMD
           MOVE    SMCNDATE-HMS        TO  SYS-TIME 
      *
      *    レセ電用パラメタＤＢ作成処理
           INITIALIZE                      BTPARA-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  BTPARA-SHELLID
           MOVE    1                   TO  BTPARA-JOBID
           MOVE    WRK-CONS-BTPARA-SCRIPTID
                                       TO  BTPARA-SCRIPTID
           INITIALIZE                      WRK-BTPARA-INFO-PARA
           MOVE    "/tmp/"             TO  WRK-BTPARA-DATA
           MOVE    "RECEDEN"           TO  WRK-BTPARA-FILENM
           MOVE    SPA-GMN-DATAKBN     TO  WRK-BTPARA-DATAKBN
           MOVE    WRK-BTPARA-INFO-PARA
                                       TO  BTPARA-INFO-PARA
           MOVE    SMCNDATE-YMD        TO  BTPARA-CREYMD
           MOVE    SMCNDATE-YMD        TO  BTPARA-UPYMD
           MOVE    SMCNDATE-HMS        TO  BTPARA-UPHMS
           MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC      NOT =   ZERO
               MOVE    "1002"          TO  SPA-ERRCD
               GO  TO  460-RECEDEN-EXT
           END-IF
      *
      *    ＨＡＯＲＩのときＭＯＮＢＬＯＢ−ＩＤを設定
           IF      SPA-T01-JOB-PARAM
                                   NOT =   SPACE
               MOVE    "MONBLOBNEW"        TO  MCP-FUNC
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "monblob"           TO  MCP-TABLE
               CALL    "MONFUNC"           USING
                                               MCPAREA
                                               MONBLOB
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "E88"               TO  SPA-ERRCD
                   GO  TO  460-RECEDEN-EXT
               END-IF
               MOVE    MONBLOB-ID          TO  SPA-T01-MONBLOB-DATA-ID
      *
               MOVE    "MONBLOBNEW"        TO  MCP-FUNC
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "monblob"           TO  MCP-TABLE
               CALL    "MONFUNC"           USING
                                               MCPAREA
                                               MONBLOB
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "E88"               TO  SPA-ERRCD
                   GO  TO  460-RECEDEN-EXT
               END-IF
               MOVE    MONBLOB-ID          TO  SPA-T01-MONBLOB-PRINT-ID
           END-IF
      *
           MOVE    ZERO                TO  WRK-CONS-JOB-JOBID
                                           WRK-PRTKANRI-RENNUM
           MOVE    SPACE               TO  WRK-CONS-PRTKANRI-TBL-GROUP
      *     
           MOVE    WRK-CONS-FILE-ID    TO  ERRFLPARA-FILE-ID
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    SPACE               TO  RECEERR
           STRING  MCP-TEMPDIR         DELIMITED   BY  SPACE
                   "/"                 DELIMITED   BY  SIZE
                   ERRFLPARA-BASENAME  DELIMITED   BY  SPACE
                                       INTO        RECEERR
           END-STRING
      *
      *    国保診療報酬請求書印刷区分
           MOVE    SPACE               TO  SYS-2005-REC 
           INITIALIZE                      SYS-2005-REC
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-2005-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2005-STYUKYMD (7:2)
           MOVE    SYS-2005-STYUKYMD   TO  SYS-2005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2005-REC
           ELSE
               INITIALIZE                   SYS-2005-REC
           END-IF
           IF      SYS-2005-RECEDENOUT =   SPACE
               MOVE    "/var/tmp/"     TO  SYS-2005-RECEDENOUT
           END-IF    
      *
      *    印刷ＤＢのＫＥＹ用     
           MOVE    WRK-SYSYMD      TO
                                   WRK-CONS-PRTKANRI-TBL-GROUP (1:8)
           MOVE    SYS-TIME        TO
                                   WRK-CONS-PRTKANRI-TBL-GROUP (9:)
      *
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                   TO  SPA-T01-TBL-GROUP
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *             
           MOVE    SPACE           TO  SHELLTBL
           IF      SPA-GMN-PRTKBN  =   "8" OR  "9"
               MOVE    "recept42.sh"   TO  SHELLTBL-NAME
           ELSE
               MOVE    "recept4.sh"    TO  SHELLTBL-NAME
           END-IF
      *                                 
           MOVE    SPACE           TO  LNK-PRTKANRI-AREA
           INITIALIZE                  LNK-PRTKANRI-AREA
           ADD     1               TO  WRK-PRTKANRI-RENNUM
           MOVE    WRK-PRTKANRI-RENNUM
                                   TO  LNK-PRTKANRI-RENNUM 
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY 
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-NAI-SKYYM   TO  LNK-PRTKANRI-SRYYM 
           MOVE    SPA-NAI-SKYYMD  TO  LNK-PRTKANRI-SKYYMD
           IF      SPA-GMN-PRTKBN  =   "8" OR  "9"
               MOVE    "recept42.sh"   TO  LNK-PRTKANRI-SHELLID
           ELSE
               MOVE    "recept4.sh"    TO  LNK-PRTKANRI-SHELLID
           END-IF
           MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
           MOVE    SPA-NAI-1031-PRTNM
                                   TO  LNK-PRTKANRI-PRTNM
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG1       
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG2     
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG3       
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG4       
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG5       
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG6       
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SHELLTBL-ARG7       
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG8      
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG9       
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG10      
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SHELLTBL-ARG11      
           IF      SPA-GMN-PRTKBN  =   "8"
               STRING      SPA-NAI-1001-PREFNUM    DELIMITED   BY  SIZE
                           SPA-NAI-1001-HOSPCD     DELIMITED   BY  SIZE
                           "_"                     DELIMITED   BY  SIZE
                           SPA-NAI-SKYYM (3:4)     DELIMITED   BY  SIZE
                                           INTO    SHELLTBL-ARG12
               END-STRING
           ELSE
             IF      SPA-GMN-PRTKBN  =   "9"
               STRING      "FF4_"                  DELIMITED   BY  SIZE
                           SPA-NAI-1001-PREFNUM    DELIMITED   BY  SIZE
                           SPA-NAI-1001-HOSPCD     DELIMITED   BY  SIZE
                           "_"                     DELIMITED   BY  SIZE
                           SPA-NAI-SKYYM (3:4)     DELIMITED   BY  SIZE
                           ".txt"                  DELIMITED   BY  SIZE
                                           INTO    SHELLTBL-ARG12
               END-STRING
             ELSE
               EVALUATE    SPA-NAI-WIDGET     
                   WHEN    "B06"
                       MOVE    "1"         TO  SHELLTBL-ARG12
                   WHEN    "B07"
                       IF    ( SPA-NAI-200501-KOUIKIKBN  =   "1"    )
                       AND   ( SPA-NAI-SKYYM             >=  200804 )
                           MOVE    "4"         TO  SHELLTBL-ARG12
                       ELSE
                           MOVE    "2"         TO  SHELLTBL-ARG12
                       END-IF
                   WHEN    "B08"
                       MOVE    "3"         TO  SHELLTBL-ARG12
               END-EVALUATE
             END-IF
           END-IF 
           IF      SPA-GMN-DATAKBN  =  "5" OR  "6"
               MOVE    "/tmp/"     TO  SYS-2005-RECEDENOUT
           END-IF
           STRING  SYS-2005-RECEDENOUT DELIMITED  BY  SPACE
                   "/"                 DELIMITED  BY  SIZE
                               INTO    SHELLTBL-ARG13
           END-STRING
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG14
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG15         
           MOVE    ERRFLPARA-BASENAME
                                   TO  SHELLTBL-ARG16
      *    ＨＡＯＲＩのとき「９」を設定
           IF      SPA-T01-JOB-PARAM
                                   NOT =   SPACE
               MOVE    "9"             TO  SHELLTBL-ARG17
           ELSE
               MOVE    SPA-GMN-DATAKBN TO  SHELLTBL-ARG17
           END-IF
           MOVE    SPA-GMN-NYUGAIKBN
                                   TO  SHELLTBL-ARG19
grpsys     MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG20
           MOVE    SPA-GMN-PRTKBN  TO  SHELLTBL-ARG21
           MOVE    SPA-NAI-200501-RECEDEN-LST1KBN
                                   TO  SHELLTBL-ARG22
           MOVE    SPA-NAI-200501-RECEDEN-LST2KBN
                                   TO  SHELLTBL-ARG23
           MOVE    SPA-NAI-200501-RECEDEN-RIREKIKB
                                   TO  SHELLTBL-ARG24
           MOVE    "99999999"      TO  SHELLTBL-ARG25
           EVALUATE    SPA-NAI-WIDGET     
               WHEN    "B06"
                   IF      SPA-GMN-DIRECT-HKNJANUM-MAX
                                               >   ZERO
                       MOVE    SPA-GMN-DIRECT-HKNJANUM
                                                   TO  SHELLTBL-ARG25
                   END-IF
           END-EVALUATE
           IF      SPA-NAI-SKYYM   <   201204
               MOVE    "0"             TO  SHELLTBL-ARG26
           ELSE
               MOVE    SPA-GMN-RECEDEN-CHK
                                       TO  SHELLTBL-ARG26
           END-IF
      *
      *    連携基盤用の時、再設定
           IF      SPA-GMN-PRTKBN  =   "6" OR  "7"
               IF      SPA-GMN-DATAKBN  =  "4"
                   MOVE    SPACE           TO  SHELLTBL-ARG13 
                   STRING  SPA-NAI-9101-FILE-DIR   DELIMITED  BY  SPACE
                           "/"                     DELIMITED  BY  SIZE
                                           INTO    SHELLTBL-ARG13
                   END-STRING
                   MOVE    "0"             TO  SHELLTBL-ARG22
                   MOVE    "0"             TO  SHELLTBL-ARG23
                   MOVE    "0"             TO  SHELLTBL-ARG24
                   MOVE    "0"             TO  SHELLTBL-ARG26
               END-IF
           END-IF 
      *
      *    ＥＦファイル、様式４の時、再設定
           IF      SPA-GMN-PRTKBN  =   "8" OR  "9"
               IF      SPA-GMN-DATAKBN  =  "4"
                   MOVE    SPACE           TO  SHELLTBL-ARG13 
                   STRING  SPA-NAI-9102-FILE-DIR   DELIMITED  BY  SPACE
                           "/"                     DELIMITED  BY  SIZE
                                           INTO    SHELLTBL-ARG13
                   END-STRING
                   MOVE    "0"             TO  SHELLTBL-ARG22
                   MOVE    "0"             TO  SHELLTBL-ARG23
                   MOVE    "0"             TO  SHELLTBL-ARG24
                   MOVE    "0"             TO  SHELLTBL-ARG26
               END-IF    
               IF      SPA-GMN-PRTKBN  =   "9"
                   MOVE    SPA-NAI-1001-PREFNUM
                                           TO  SHELLTBL-ARG24
                   MOVE    SPA-NAI-1001-HOSPCD
                                           TO  SHELLTBL-ARG26
               END-IF
           END-IF 
      *
           IF      SPA-GMN-SRYYMKBN-MAX
                                   =   ZERO
               MOVE    "999999"        TO  SHELLTBL-ARG27
               MOVE    "999999"        TO  SHELLTBL-ARG28
           ELSE
               MOVE    SPA-NAI-SRYYMKBN-ST (SPA-NAI-SRYYMKBN-SELNUM)
                                       TO  SHELLTBL-ARG27
               MOVE    SPA-NAI-SRYYMKBN-ED (SPA-NAI-SRYYMKBN-SELNUM)
                                       TO  SHELLTBL-ARG28
           END-IF
      *
      *    ＨＡＯＲＩのときＭＯＮＢＬＯＢ−ＩＤを設定
           IF      SPA-T01-JOB-PARAM
                                   NOT =   SPACE
               MOVE    SPA-T01-MONBLOB-DATA-ID
                                       TO  SHELLTBL-ARG29
           END-IF
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "monthly"       TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
      *    ＪＯＢＩＤ更新処理
      *
           PERFORM 45012-JOBID-UPD-SEC
      *
           IF    ( SPA-GMN-DATAKBN =   "5" OR  "6" )
               MOVE    "SV"            TO  SPA-TID1-FLG
           END-IF
      *
           MOVE    1                   TO  SPA-T01-WRNO
           MOVE    1                   TO  SPA-T01-SYORIFLG
           MOVE    LNK-PRTKANRI-AREA   TO  SPA-LNK-PRTKANRI-AREA
      * 
      *****MOVE    SPA-T01KYOUTU       TO  SPA-WORK
           MOVE    WRK-CONS-JOB-JOBID  TO  SPA-T01-JOB-JOBID
      *     
      *    情報確認画面を表示                                                   
           PERFORM 490-KAKUNIN-SEC
           .
       460-RECEDEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電出力処理（労災）
      *****************************************************************
       470-RECEDEN-SEC                SECTION.
      *
           INITIALIZE                  SPA-T01-TBL-GROUP
           MOVE    ZERO            TO  SPA-T01-TIMER-FLG
      *
      *    クライアント保存削除処理             
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "D"             TO  SFILESV-MODE
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SFILESV-TBL-KEY
           MOVE    SPA-NAI-SKYYM   TO  SFILESV-SRYYM   (1)
           MOVE    WRK-CONS-BTPARA-SCRIPTID
                                   TO  SFILESV-SHELLID (1)
           CALL    "ORCSFILESV"    USING
                                       ORCSFILESVAREA
                                       SPA-AREA
      * 
      *    ジョブ管理削除処理
           MOVE    "ALL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "1004"              TO  SPA-ERRCD
               GO  TO  470-RECEDEN-EXT
           END-IF             
      *
      *    パラメタＤＢ削除処理
           INITIALIZE                  BTPARA-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  BTPARA-SHELLID
           MOVE    SPA-HOSPNUM     TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"      TO  MCP-FUNC
           MOVE    "tbl_btpara"    TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          >   ZERO
               MOVE    "1001"          TO  SPA-ERRCD
               GO  TO  470-RECEDEN-EXT
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  WRK-SYSYMD
           MOVE    SMCNDATE-HMS        TO  SYS-TIME 
      *
      *    レセ電用パラメタＤＢ作成処理
           INITIALIZE                      BTPARA-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  BTPARA-SHELLID
           MOVE    1                   TO  BTPARA-JOBID
           MOVE    WRK-CONS-BTPARA-SCRIPTID
                                       TO  BTPARA-SCRIPTID
           INITIALIZE                      WRK-BTPARA-INFO-PARA
           MOVE    "/tmp/"             TO  WRK-BTPARA-DATA
           MOVE    "RECEDEN"           TO  WRK-BTPARA-FILENM
           MOVE    SPA-GMN-DATAKBN     TO  WRK-BTPARA-DATAKBN
           MOVE    WRK-BTPARA-INFO-PARA
                                       TO  BTPARA-INFO-PARA
           MOVE    SMCNDATE-YMD        TO  BTPARA-CREYMD
           MOVE    SMCNDATE-YMD        TO  BTPARA-UPYMD
           MOVE    SMCNDATE-HMS        TO  BTPARA-UPHMS
           MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC      NOT =   ZERO
               MOVE    "1002"          TO  SPA-ERRCD
               GO  TO  470-RECEDEN-EXT
           END-IF
      *
      *    ＨＡＯＲＩのときＭＯＮＢＬＯＢ−ＩＤを設定
           IF      SPA-T01-JOB-PARAM
                                   NOT =   SPACE
               MOVE    "MONBLOBNEW"        TO  MCP-FUNC
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "monblob"           TO  MCP-TABLE
               CALL    "MONFUNC"           USING
                                               MCPAREA
                                               MONBLOB
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "E88"               TO  SPA-ERRCD
                   GO  TO  470-RECEDEN-EXT
               END-IF
               MOVE    MONBLOB-ID          TO  SPA-T01-MONBLOB-DATA-ID
      *
               MOVE    "MONBLOBNEW"        TO  MCP-FUNC
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "monblob"           TO  MCP-TABLE
               CALL    "MONFUNC"           USING
                                               MCPAREA
                                               MONBLOB
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "E88"               TO  SPA-ERRCD
                   GO  TO  470-RECEDEN-EXT
               END-IF
               MOVE    MONBLOB-ID          TO  SPA-T01-MONBLOB-PRINT-ID
           END-IF
      *
           MOVE    ZERO                TO  WRK-CONS-JOB-JOBID
                                           WRK-PRTKANRI-RENNUM
           MOVE    SPACE               TO  WRK-CONS-PRTKANRI-TBL-GROUP
      *     
           MOVE    WRK-CONS-FILE-ID    TO  ERRFLPARA-FILE-ID
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    SPACE               TO  RECEERR
           STRING  MCP-TEMPDIR         DELIMITED   BY  SPACE
                   "/"                 DELIMITED   BY  SIZE
                   ERRFLPARA-BASENAME  DELIMITED   BY  SPACE
                                       INTO        RECEERR
           END-STRING
      *
           MOVE    SPACE               TO  SYS-2005-REC 
           INITIALIZE                      SYS-2005-REC
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-2005-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2005-STYUKYMD (7:2)
           MOVE    SYS-2005-STYUKYMD   TO  SYS-2005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2005-REC
           ELSE
               INITIALIZE                   SYS-2005-REC
           END-IF
           IF      SYS-2005-RECEDENOUT =   SPACE
               MOVE    "/var/tmp/"     TO  SYS-2005-RECEDENOUT
           END-IF    
      *
      *    労災指定医療機関の有無
           MOVE    SPACE               TO  SYS-4001-REC 
           INITIALIZE                      SYS-4001-REC
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-4001-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-4001-STYUKYMD (7:2)
           MOVE    SYS-4001-STYUKYMD   TO  SYS-4001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-4001-HOSPNUM
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-4001-REC
           ELSE
               INITIALIZE                      SYS-4001-REC
           END-IF
           IF      SYS-4001-RSIIRYOKBN =   SPACE
               MOVE    "0"                 TO  SYS-4001-RSIIRYOKBN
           END-IF
      *
      *    印刷ＤＢのＫＥＹ用     
           MOVE    WRK-SYSYMD      TO
                                   WRK-CONS-PRTKANRI-TBL-GROUP (1:8)
           MOVE    SYS-TIME        TO
                                   WRK-CONS-PRTKANRI-TBL-GROUP (9:)
      *
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                   TO  SPA-T01-TBL-GROUP
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *             
           MOVE    SPACE           TO  SHELLTBL
           MOVE    "recept41.sh"   TO  SHELLTBL-NAME
      *                                 
           MOVE    SPACE           TO  LNK-PRTKANRI-AREA
           INITIALIZE                  LNK-PRTKANRI-AREA
           ADD     1               TO  WRK-PRTKANRI-RENNUM
           MOVE    WRK-PRTKANRI-RENNUM
                                   TO  LNK-PRTKANRI-RENNUM 
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY 
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-NAI-SKYYM   TO  LNK-PRTKANRI-SRYYM 
           MOVE    SPA-NAI-SKYYMD  TO  LNK-PRTKANRI-SKYYMD
           MOVE    "recept41.sh"   TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
           MOVE    SPA-NAI-1031-PRTNM
                                   TO  LNK-PRTKANRI-PRTNM
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG1       
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG2     
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG3       
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG4       
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG5       
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG6       
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SHELLTBL-ARG7       
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG8      
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG9       
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG10      
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SHELLTBL-ARG11      
           MOVE    SYS-4001-RSIIRYOKBN
                                   TO  SHELLTBL-ARG12
           IF      SPA-GMN-DATAKBN  =  "5" OR  "6"
               MOVE    "/tmp/"     TO  SYS-2005-RECEDENOUT
           END-IF
           STRING  SYS-2005-RECEDENOUT DELIMITED  BY  SPACE
                   "/"                 DELIMITED  BY  SIZE
                               INTO    SHELLTBL-ARG13
           END-STRING
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG14
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG15         
           MOVE    ERRFLPARA-BASENAME
                                   TO  SHELLTBL-ARG16
      *    ＨＡＯＲＩのとき「９」を設定
           IF      SPA-T01-JOB-PARAM
                                   NOT =   SPACE
               MOVE    "9"             TO  SHELLTBL-ARG17
           ELSE
               MOVE    SPA-GMN-DATAKBN TO  SHELLTBL-ARG17
           END-IF
      *ipアドレス取得
      *****PERFORM 900-SYSTEM-GET-SEC
      *****MOVE    SLDUSER-HOST(1) TO  SHELLTBL-ARG18
           MOVE    SPA-GMN-NYUGAIKBN
                                   TO  SHELLTBL-ARG19
grpsys     MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG20
           MOVE    SPA-GMN-PRTKBN  TO  SHELLTBL-ARG21
           MOVE    SPA-NAI-200501-RECEDEN-LST1KBN
                                   TO  SHELLTBL-ARG22
           MOVE    SPA-NAI-200501-RECEDEN-LST2KBN
                                   TO  SHELLTBL-ARG23
           MOVE    SPA-NAI-200501-RECEDEN-RIREKIKB
                                   TO  SHELLTBL-ARG24
           IF      SPA-NAI-SKYYM   <   201307
               MOVE    "0"             TO  SHELLTBL-ARG26
           ELSE
               MOVE    SPA-GMN-RECEDEN-CHK
                                       TO  SHELLTBL-ARG26
           END-IF
      *
      *    ＨＡＯＲＩのときＭＯＮＢＬＯＢ−ＩＤを設定
           IF      SPA-T01-JOB-PARAM
                                   NOT =   SPACE
               MOVE    SPA-T01-MONBLOB-DATA-ID
                                       TO  SHELLTBL-ARG29
           END-IF
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "monthly"       TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
      *    ＪＯＢＩＤ更新処理
      *
           PERFORM 45012-JOBID-UPD-SEC
      *
           IF    ( SPA-GMN-DATAKBN =   "5" OR  "6" )
               MOVE    "SV"            TO  SPA-TID1-FLG
           END-IF
      *
           MOVE    1                   TO  SPA-T01-WRNO
           MOVE    1                   TO  SPA-T01-SYORIFLG
           MOVE    LNK-PRTKANRI-AREA   TO  SPA-LNK-PRTKANRI-AREA
      * 
      *****MOVE    SPA-T01KYOUTU       TO  SPA-WORK
           MOVE    WRK-CONS-JOB-JOBID  TO  SPA-T01-JOB-JOBID
      *     
      *    情報確認画面を表示                                                   
           PERFORM 490-KAKUNIN-SEC
           .
       470-RECEDEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ収集処理
      *****************************************************************
       470-COLLECT-SYORI-SEC      SECTION.
      *
      *    ジョブ開始セット処理
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID-COLLECT
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID-COLLECT
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID-COLLECT
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID-COLLECT
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           MOVE    SPACE           TO  SHELLTBL
           MOVE    "collectdata1.sh"   TO  SHELLTBL-NAME
           MOVE    SPA-TERMID      TO  SHELLTBL-ARG1
grpsys     MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG2
           MOVE    SPA-PREFNUM     TO  SHELLTBL-ARG3
           MOVE    SPA-NAI-SKYYM   TO  SHELLTBL-ARG4
           MOVE    SPA-BEDFLG      TO  SHELLTBL-ARG5
           MOVE    WRK-CONS-JOB-JOBID-COLLECT
                                   TO  SHELLTBL-ARG6
           MOVE    WRK-CONS-JOB-SHELLID-COLLECT
                                   TO  SHELLTBL-ARG7
           MOVE    WRK-DASUPLOAD   TO  SHELLTBL-ARG8
           MOVE    SPA-HOSPID      TO  SHELLTBL-ARG9
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "monthly"       TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       470-COLLECT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責ＰＤＦ作成前処理
      *****************************************************************
       480-JIBAI-PDF-GID-SEC                SECTION.
      *
      *    入力チェック
           MOVE    1               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  460-RECEDEN-GID-EXT
           END-IF
      *
      *    ジョブ管理チェック処理
           IF      SPA-ERRCD       =   SPACE
               PERFORM 4501-JOBKANRI-CHK-SEC
               IF      SPA-ERRCD       =   SPACE
                   MOVE    MCP-WIDGET          TO  SPA-NAI-WIDGET
      *            ガイドメッセージセット
                   MOVE    "1034"              TO  SPA-TIDCD
               END-IF
           END-IF
           .
       480-JIBAI-PDF-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責ＰＤＦ作成処理
      *****************************************************************
       480-JIBAI-PDF-SEC                SECTION.
      *
      *    ジョブ管理削除処理
           MOVE    "ALL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "1004"              TO  SPA-ERRCD
               GO  TO  480-JIBAI-PDF-EXT
           END-IF
      *
           INITIALIZE                  SPA-T01-TBL-GROUP
           MOVE    ZERO            TO  SPA-T01-TIMER-FLG
      *     
           MOVE    WRK-CONS-FILE-ID    TO  ERRFLPARA-FILE-ID
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    ZERO                TO  WRK-CONS-JOB-JOBID
                                           WRK-PRTKANRI-RENNUM
           MOVE    SPACE               TO  WRK-CONS-PRTKANRI-TBL-GROUP
      *
      *    印刷ＤＢのＫＥＹ用
           MOVE    SMCNDATE-YMD     TO
                                   WRK-CONS-PRTKANRI-TBL-GROUP (1:8)
           MOVE    SMCNDATE-HMS     TO
                                   WRK-CONS-PRTKANRI-TBL-GROUP (9:)
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                   TO  SPA-T01-TBL-GROUP
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *                                 
           MOVE    SPACE           TO  LNK-PRTKANRI-AREA
           INITIALIZE                  LNK-PRTKANRI-AREA
           ADD     1               TO  WRK-PRTKANRI-RENNUM
           MOVE    WRK-PRTKANRI-RENNUM
                                   TO  LNK-PRTKANRI-RENNUM 
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY 
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-NAI-SKYYM   TO  LNK-PRTKANRI-SRYYM 
           MOVE    SPA-NAI-SKYYMD  TO  LNK-PRTKANRI-SKYYMD
           MOVE    "recept43.sh"   TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
           MOVE    SPA-NAI-1031-PRTNM
                                   TO  LNK-PRTKANRI-PRTNM
      *
           MOVE    SPACE           TO  SHELLTBL
           MOVE    "recept43.sh"   TO  SHELLTBL-NAME
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG3
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG6
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG10
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SHELLTBL-ARG11
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG12
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG13
      *****MOVE    SPA-GMN-NYUGAIKBN
           MOVE    "0"             TO  SHELLTBL-ARG14
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG15
           MOVE    ERRFLPARA-BASENAME
                                   TO  SHELLTBL-ARG16
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "monthly"       TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
      *    ＪＯＢＩＤ更新処理
           PERFORM 45012-JOBID-UPD-SEC
      *
      **** MOVE    1                   TO  SPA-T01-WRNO
      **** MOVE    1                   TO  SPA-T01-SYORIFLG
      *****MOVE    LNK-PRTKANRI-AREA   TO  SPA-LNK-PRTKANRI-AREA
      * 
           MOVE    WRK-CONS-JOB-JOBID  TO  SPA-T01-JOB-JOBID
      *     
      *    情報確認画面を表示                                                   
           PERFORM 490-KAKUNIN-SEC
           .
       480-JIBAI-PDF-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷前処理（チェック・集計表）
      *****************************************************************
       480-PRINT-GID-SEC                SECTION.
      *
      *    ジョブ管理チェック（レセプト）
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID-RECEPT1
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID-RECEPT1
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN =   ZERO
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "7777"          TO  SPA-ERRCD
                   MOVE    SPACE             TO  WRK-RECEERR
                   STRING  "レセプト作成処理中です【"
                                             DELIMITED  BY  SIZE
                           JOB-SHELLMSG      DELIMITED  BY  SPACE
                           "】"              DELIMITED  BY  SIZE      
                                             INTO  WRK-RECEERR
                   END-STRING
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD           =   SPACE
      *        入力チェック
               MOVE    0               TO  FLG-TOUROKU
               PERFORM 410-INPUT-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  480-PRINT-GID-EXT
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD           =   SPACE
      *    ガイドメッセージセット
               EVALUATE    MCP-WIDGET
                   WHEN    "B22" 
                       MOVE    "1011"              TO  SPA-TIDCD
                   WHEN    "B09" 
                       MOVE    "1012"              TO  SPA-TIDCD
                   WHEN    "B10" 
                       MOVE    "1013"              TO  SPA-TIDCD
                   WHEN    "B20" 
                       MOVE    "1019"              TO  SPA-TIDCD
               END-EVALUATE
           END-IF
           .
       480-PRINT-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理（チェック・集計表）
      *****************************************************************
       480-PRINT-SEC                 SECTION.
      *
           INITIALIZE                  SPA-T01-TBL-GROUP
      *     
           MOVE    WRK-CONS-FILE-ID    TO  ERRFLPARA-FILE-ID
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    SPACE               TO  RECEERR
           STRING  MCP-TEMPDIR         DELIMITED   BY  SPACE
                   "/"                 DELIMITED   BY  SIZE
                   ERRFLPARA-BASENAME  DELIMITED   BY  SPACE
                                       INTO        RECEERR
           END-STRING
      *
           OPEN    OUTPUT   RECEERR-FILE
           CLOSE   RECEERR-FILE
      *
           EVALUATE    SPA-TIDCD
               WHEN    "1011"
               WHEN    "1019"
      *            ジョブ管理削除処理
                   MOVE    "ALL"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
                   IF      SJOBKANRI-RETURN    =   ZERO
                       CONTINUE
                   ELSE
                       MOVE    "1004"              TO  SPA-ERRCD
                       GO  TO  480-PRINT-EXT
                   END-IF             
      *
      *            印刷ＤＢのＫＥＹ用     
      *
      *            更新日取得
                   INITIALIZE                  ORCSMCNDATEAREA
                    CALL    "ORCSMCNDATE"      USING
                                               ORCSMCNDATEAREA
                   MOVE    SMCNDATE-YMD        TO  WRK-SYSYMD
                   MOVE    SMCNDATE-HMS        TO   SYS-TIME 
                   MOVE    SPACE               TO
                                           WRK-CONS-PRTKANRI-TBL-GROUP                                
                   MOVE    WRK-SYSYMD          TO
                                   WRK-CONS-PRTKANRI-TBL-GROUP (1:8)
                   MOVE    SYS-TIME            TO
                                   WRK-CONS-PRTKANRI-TBL-GROUP (9:)
      *
                   MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                               TO  SPA-T01-TBL-GROUP
      *
                   MOVE    ZERO                TO  WRK-CONS-JOB-JOBID     
                                                   WRK-PRTKANRI-RENNUM
                   PERFORM 45011-JOBKANRI-HENSYU-SEC
      *             
                   MOVE    SPACE           TO  SHELLTBL
                   MOVE    "recept5.sh"    TO  SHELLTBL-NAME
      *                                 
                   MOVE    SPACE           TO  LNK-PRTKANRI-AREA
                   INITIALIZE                  LNK-PRTKANRI-AREA
                   ADD     1               TO  WRK-PRTKANRI-RENNUM
                   MOVE    WRK-PRTKANRI-RENNUM
                                           TO  LNK-PRTKANRI-RENNUM 
                   MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                           TO  LNK-PRTKANRI-TBL-KEY 
                   MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                           TO  LNK-PRTKANRI-TBL-GROUP
                   MOVE    SPA-NAI-SKYYM   TO  LNK-PRTKANRI-SRYYM 
                   MOVE    SPA-SYSYMD      TO  LNK-PRTKANRI-SKYYMD
                   MOVE    "recept5.sh"    TO  LNK-PRTKANRI-SHELLID
                   MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
                   MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID 
                   MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
                   MOVE    SPA-NAI-1031-PRTNM
                                           TO  LNK-PRTKANRI-PRTNM
                   MOVE    LNK-PRTKANRI-RENNUM
                                           TO  SHELLTBL-ARG1       
                   MOVE    LNK-PRTKANRI-TBL-KEY
                                           TO  SHELLTBL-ARG2     
                   MOVE    LNK-PRTKANRI-TBL-GROUP
                                           TO  SHELLTBL-ARG3       
                   MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                           TO  SHELLTBL-ARG4       
                   MOVE    LNK-PRTKANRI-SRYYM
                                           TO  SHELLTBL-ARG5       
                   MOVE    LNK-PRTKANRI-SKYYMD
                                           TO  SHELLTBL-ARG6       
                   MOVE    LNK-PRTKANRI-SHELLID
                                           TO  SHELLTBL-ARG7       
                   MOVE    LNK-PRTKANRI-PRIORITY
                                           TO  SHELLTBL-ARG8      
                   MOVE    LNK-PRTKANRI-TERMID
                                           TO  SHELLTBL-ARG9       
                   MOVE    LNK-PRTKANRI-OPID
                                           TO  SHELLTBL-ARG10      
                   MOVE    LNK-PRTKANRI-PRTNM
                                           TO  SHELLTBL-ARG11      
grpsys             MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG12
                   MOVE    WRK-CONS-JOB-JOBID
                                           TO  SHELLTBL-ARG13
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  SHELLTBL-ARG14         
                   MOVE    ERRFLPARA-BASENAME
                                           TO  SHELLTBL-ARG15
                   IF       SPA-TIDCD      =   "1011"
                       MOVE    "1"             TO  SHELLTBL-ARG16
                   ELSE
                       MOVE    "2"             TO  SHELLTBL-ARG16
                   END-IF
                   MOVE    SHELLTBL        TO  MCPDATA-REC
                   MOVE    "SHELL"         TO  MCP-FUNC
                   MOVE    "shell"         TO  MCP-TABLE
                   MOVE    "monthly"       TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "ERROR MCP-RC=" MCP-RC 
                   END-IF
      *
      *            印刷処理
                   IF      SPA-TID1-FLG    =   "OK"    OR  "PV"
                       PERFORM 45011-JOBKANRI-HENSYU-SEC
                       PERFORM 45013-PRINT-SYORI-SEC
                   END-IF       
      *
      *            ＪＯＢＩＤ更新処理
                   PERFORM 45012-JOBID-UPD-SEC
      *
      *            印刷データ削除処理
                   PERFORM 45015-PRTDATA-DEL-SEC
      *     
      *            情報確認画面を表示                                                   
                   PERFORM 490-KAKUNIN-SEC
               WHEN    OTHER
                   MOVE    SPACE               TO  SHELLTBL
                   EVALUATE    SPA-TIDCD 
                       WHEN    "1012"
                           MOVE    "1"             TO  WRK-SHELL-RECEKBN
                       WHEN    "1013"
                           MOVE    "2"             TO  WRK-SHELL-RECEKBN
                   END-EVALUATE
                   MOVE    "sokatsu.sh"        TO  SHELLTBL-NAME
                   MOVE    SPA-NAI-SKYYM       TO  WRK-SHELL-SRYYM 
                   MOVE    SPA-TERMID          TO  WRK-SHELL-TERMID
                   MOVE    SPA-SYSYMD          TO  WRK-SHELL-SYSYMD    
                   MOVE    WRK-SHELL-ARG1      TO  SHELLTBL-ARG1
                   MOVE    WRK-SHELL-RECEKBN   TO  SHELLTBL-ARG2
                   MOVE    ERRFLPARA-BASENAME  TO  SHELLTBL-ARG3
grpsys             MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG4
                   MOVE    SPA-TERMID          TO  SHELLTBL-ARG5
                   MOVE    SHELLTBL            TO  MCPDATA-REC
                   MOVE    "SHELL"             TO  MCP-FUNC
                   MOVE    "shell"             TO  MCP-TABLE
                   MOVE    "monthly"           TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
           END-EVALUATE
          .
       480-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-T01FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "T01"               TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SPA-JOB-SHELLID
      *          
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XB01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-JYOUHOU-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    収集データ確認画面へ
      *****************************************************************
       490-TEITEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-T01FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "T01"               TO  SPA-MOTOPG
           MOVE    "T91"               TO  SPA-SAKIPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "T91    "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-TEITEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    再印刷画面へ
      *****************************************************************
       450-REPRINT-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-T01FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "T01"               TO  SPA-MOTOPG
           MOVE    "XA01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
      *          
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XA01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       450-REPRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費ＣＳＶ作成画面
      *****************************************************************
       480-CSV-SYORI-SEC             SECTION.
      *
      *    入力チェック
           MOVE    1               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  480-CSV-SYORI-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-T01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-T01FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGT04"           USING
                                           MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-T01FREE
           MOVE    SPA-WORK            TO  SPA-T01KYOUTU
      *
           MOVE    "T01"                TO  SPA-MOTOPG
           MOVE    "T04"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "T04"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       480-CSV-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ取り込み画面
      *****************************************************************
       480-DATA-EXPORT-SEC             SECTION.
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-T01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-T01FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGT05"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-T01FREE
           MOVE    SPA-WORK            TO  SPA-T01KYOUTU
      *
           MOVE    "T01"                TO  SPA-MOTOPG
           MOVE    "T05"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "T05"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       480-DATA-EXPORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電データ個別指示画面
      *****************************************************************
       480-RECEDEN-KOBETU-SEC             SECTION.
      *
      *    入力チェック
           MOVE    1               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  480-RECEDEN-KOBETU-EXT
           END-IF
      *
           IF      SPA-GMN-DIRECT-HKNJANUM-MAX
                                       >   ZERO
               MOVE    SPA-GMN-DIRECT-HKNJANUM
                                           TO  SPA-T01-DIRECT-HKNJANUM
               MOVE    SPA-GMN-DIRECT-HKNJANAME
                                           TO  SPA-T01-DIRECT-HKNJANAME
               MOVE    SPA-NAI-DIRECT-HKNJANUM-63
                                           TO
                                           SPA-T01-DIRECT-HKNJANUM-63
               MOVE    ZERO                TO  IDY 
               PERFORM VARYING IDX FROM    2   BY  1
                       UNTIL   IDX >       SPA-GMN-DIRECT-HKNJANUM-MAX
                   ADD     1               TO  IDY
                   MOVE    SPA-GMN-DIRECT-HKNJANUML (IDX)
                                           TO  SPA-T01-DIRECT-HKNJANUML
                                                                   (IDY)
                   MOVE    SPA-NAI-DIRECT-HKNJANUM-63L (IDX)
                                           TO
                                           SPA-T01-DIRECT-HKNJANUM-63L
                                                                   (IDY)
               END-PERFORM
           ELSE
               INITIALIZE                  SPA-T01-DIRECT-HKNJANUM-AREA
           END-IF
      *
           MOVE    SPA-GMN-RECEDEN-CHK TO  SPA-T01-RECEDEN-CHK
           MOVE    SPA-GMN-PRTKBN      TO  SPA-T01-PRTKBN
           MOVE    SPA-GMN-NYUGAIKBN   TO  SPA-T01-NYUGAIKBN
           MOVE    SPA-NAI-9101-FILE-DIR
                                       TO  SPA-T01-9101-FILE-DIR
           MOVE    SPA-NAI-1001-T      TO  SPA-T01-1001-T
      *
           MOVE    SPA-T01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-T06             TO  SPA-FREE
      *
           MOVE    "T01"               TO  SPA-MOTOPG
           MOVE    "T06"               TO  SPA-SAKIPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       480-RECEDEN-KOBETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費個別指定画面
      *****************************************************************
       490-KOBETU-SEC             SECTION.
      *
      *    入力チェック
           MOVE    0               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  490-KOBETU-EXT
           END-IF
      *
      *    パラメタＤＢ削除処理
           INITIALIZE                  BTPARA-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  BTPARA-SHELLID
           MOVE    SPA-HOSPNUM     TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"      TO  MCP-FUNC
           MOVE    "tbl_btpara"    TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          >   ZERO
               MOVE    "1001"          TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  SPA-MOTOPG
               MOVE    SPA-T01KYOUTU       TO  SPA-WORK
               MOVE    SPA-AREA            TO  SPA-COMMON
               MOVE    SPA-T01FREE         TO  SPA-FREE
      *
               MOVE    "LINK"              TO  MCP-STATUS
               MOVE    SPACE               TO  MCP-EVENT
      *
               CALL    "ORCGT02"           USING
                                           MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *
               MOVE    SPA-COMMON          TO  SPA-AREA
               MOVE    SPA-FREE            TO  SPA-T01FREE
               MOVE    SPA-WORK            TO  SPA-T01KYOUTU
      *
               MOVE    "T01"                TO  SPA-MOTOPG
               MOVE    "T02"                TO  SPA-SAKIPG
      *
               MOVE    "NEW"                TO  MCP-PUTTYPE
               MOVE    "T02"                TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                    TO  FLG-END
           END-IF              
      *
           .
       490-KOBETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    プリンタ設定画面
      *****************************************************************
       490-PRTNM-SET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-T01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-T01FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGT03"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-T01FREE
           MOVE    SPA-WORK            TO  SPA-T01KYOUTU
      *
           MOVE    "T01"                TO  SPA-MOTOPG
           MOVE    "T03"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "T03"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       490-PRTNM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-PRT-PAGE
                                           WRK-PRT-PAGE1
                                           WRK-PRT-PAGE2
      *
           INITIALIZE                      T99
           MOVE    ZERO                TO  T99-COUNT
           MOVE    "black"             TO  T99-MSG-STYLE
      *
           MOVE    5                   TO  T99-DURATION
           MOVE    WIDGET-INSENSITIVE  TO  T99-B12-STATE
           IF    ( SPA-TID1-FLG    =   "PV" ) 
           OR    ( SPA-RECEDEN-FLG =   "PV" ) 
               IF      SPA-T99-MOTOPG  NOT =   "T04"
                   MOVE    "F12:プレビュー"    TO  T99-B12-LABEL
               END-IF
           END-IF 
      *        
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-3003-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-3003-STYUKYMD (7:2)
           MOVE    SYS-3003-STYUKYMD   TO  SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SYS-3003-JOBID
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
                   IF      SJOBKANRI-RETURN    =   ZERO
                       MOVE    JOB-JOBID       TO  T99-RENNUM   (IDX)
                       MOVE    JOB-SHELLMSG    TO  T99-SHORINM  (IDX)
                       MOVE    JOB-STARTTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-STARTTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-STARTTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  T99-STARTTIME(IDX)
                       MOVE    JOB-ENDTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-ENDTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-ENDTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  T99-ENDTIME  (IDX)
                       MOVE    JOB-UPDCNT      TO  WRK-PAGE
                       MOVE    WRK-PAGE        TO  T99-PAGE     (IDX)
                       MOVE    JOB-YOBI        TO  T99-ERRMSG   (IDX)
                       IF      JOB-ENDYMD      =   SPACE
                           MOVE    "処理中です"    TO  T99-MSG
                       ELSE
                           IF      JOB-ERRCD       =   SPACE
                               IF      JOB-PGID        =   "ORCBPRT1"
                                   CONTINUE
                               ELSE
      *                            レセ電データチェックリストの有無
                                   IF      JOB-UPDCNT1 >   ZERO
                                       MOVE
                                   "【チェックリストが作成されました】"
                                                 TO  T99-ERRMSG   (IDX)
                                   END-IF  
                               END-IF
                           ELSE
                               MOVE    SPACE       TO  T99-MSG
                               PERFORM 4901-JOB-ERRSET-SEC
                               IF      T99-MSG     =   SPACE
                                 STRING  JOB-YOBI    DELIMITED BY SPACE
                                         "【"        DELIMITED BY SIZE
                                        JOB-SHELLMSG DELIMITED BY SPACE
                                        "】"         DELIMITED BY SIZE
                                                     INTO  T99-MSG
                                 END-STRING   
                               END-IF
                           END-IF                          
                       END-IF    
                       MOVE    IDX                     TO  T99-COUNT    
                       IF    ( JOB-PGID        =   "ORCBPRT1" )
                           MOVE    JOB-UPDCNT  TO  WRK-PRT-PAGE
                       ELSE
      *                    レセ電データチェックリストの枚数
                           MOVE    JOB-UPDCNT1 TO  WRK-PRT-PAGE1
      *                    レセ電データのレセプト件数
                           MOVE    JOB-UPDCNT2 TO  WRK-PRT-PAGE2
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           IF      T99-MSG              =   SPACE
               MOVE    "B01"                TO  MCP-WIDGET
               IF      T99-COUNT            >   ZERO    
      *            メッセージ編集処理
                   PERFORM 4900-MSG-HENSYU-SEC
               ELSE
                   MOVE    "該当する処理はありません"
                                                TO  T99-MSG
               END-IF                                 
               MOVE    WIDGET-NORMAL       TO  T99-B01-STATE
           ELSE                                        
               MOVE    "B11"               TO  MCP-WIDGET
               MOVE    WIDGET-NORMAL       TO  T99-B01-STATE
           END-IF                                 
      *
      *
           MOVE    "T01"                TO  SPA-MOTOPG
           MOVE    "T99"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "T99"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    メッセージ編集処理
      *****************************************************************
       4900-MSG-HENSYU-SEC         SECTION.
      *
           IF    ( WRK-PRT-PAGE1   =   ZERO )
           AND   ( WRK-PRT-PAGE2   =   ZERO )
               MOVE    WRK-T99-MSG1    TO  T99-MSG
           ELSE
      *        レセ電データ作成時のレセプト件数
               IF      WRK-PRT-PAGE2   >   ZERO
                   MOVE    WRK-PRT-PAGE2   TO  WRK-T99-MSG-KENSU
                   MOVE    WRK-T99-MSG     TO  T99-MSG
               END-IF
      *
      *        レセ電データチェック時のエラーリストの有無
               IF      WRK-PRT-PAGE1   >   ZERO
                   MOVE    "red"           TO  T99-MSG-STYLE
               END-IF
           END-IF
      *
           .
       4900-MSG-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージセット処理
      *****************************************************************
       4901-JOB-ERRSET-SEC             SECTION.
      *
           EVALUATE    JOB-ERRCD
               WHEN    "0001"
                   MOVE    "【ファイルがありません】" 
                                               TO  T99-MSG
               WHEN    "0002"
                   MOVE    "【ｆｄｄが起動されていません】" 
                                               TO  T99-MSG
               WHEN    "0003"
               WHEN    "0004"
                   MOVE    "【ｆｄｄで処理が失敗しました】" 
                                               TO  T99-MSG
               WHEN    "0011"
                   MOVE    "【媒体がマウントできませんでした】" 
                                               TO  T99-MSG
               WHEN    "0012"
                   MOVE    "【媒体がアンマウントできませんでした】" 
                                               TO  T99-MSG
               WHEN    "0021"
                   MOVE    "【ファイルコピーでエラーが発生しました】" 
                                               TO  T99-MSG
               WHEN    "0127"
                   MOVE    "【スクリプトが実行できませんでした】" 
                                               TO  T99-MSG
           END-EVALUATE
      *
           .
       4901-JOB-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
                                           WRK-HENYMDG3
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE OR
                                           ZERO
               MOVE    WRK-SYMD (1:8)      TO  WRK-HENYMDG
               EVALUATE    WRK-SYMD (1:6)
                   WHEN    "000000"
                       MOVE    "００００００００"  TO  WRK-HENYMDG3
                   WHEN    "999999"
                       MOVE    "９９９９９９９９"  TO  WRK-HENYMDG3
               END-EVALUATE
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG3
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-TIDCD       NOT =   SPACE
               EVALUATE    SPA-TIDCD
                   WHEN    "1010"
                   WHEN    "1014"
                   WHEN    "1015"
                   WHEN    "1016"
                   WHEN    "1017"
                   WHEN    "1018"
                   WHEN    "1020"
                   WHEN    "1021"
                   WHEN    "1022"
                   WHEN    "1023"
                   WHEN    "1024"
                   WHEN    "1025"
      *                レセ電データ作成時 
                       PERFORM 530-TID2SET-SEC
                       GO  TO  500-SET-SCREEN-EXT
                   WHEN    "1027"
                   WHEN    "1028"
                   WHEN    "1029"
      *                レセ電データ（労災）作成時 
                       PERFORM 530-TID2SET-SEC
                       GO  TO  500-SET-SCREEN-EXT
                   WHEN    "1030"
                   WHEN    "1031"
                   WHEN    "1032"
                   WHEN    "1033"
      *                ＥＦファイル、様式４作成時 
                       PERFORM 530-TID2SET-SEC
                       GO  TO  500-SET-SCREEN-EXT
                   WHEN    "1034"
      *                自賠責ＰＤＦファイル作成時 
                       PERFORM 520-TIDSET-SEC
                       GO  TO  500-SET-SCREEN-EXT
                   WHEN    "1001"
                   WHEN    "1002"
                   WHEN    "1003"
                   WHEN    "1004"
                   WHEN    "1005"
                   WHEN    "1006"
                   WHEN    "1007"
                   WHEN    "1008"
                   WHEN    "1009"
                   WHEN    "1011"
                   WHEN    "1026"
                   WHEN    "1019"  
      *                総括表作成時 
                       PERFORM 530-TID2SET-SEC
                       GO  TO  500-SET-SCREEN-EXT
                   WHEN    OTHER
      *                チェックリスト等作成時 
                       PERFORM 520-TIDSET-SEC
                       GO  TO  500-SET-SCREEN-EXT
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "T01    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE                      T01
      *
           MOVE    SPA-GMN-SKYYM       TO  T01-SKYYM
           MOVE    SPA-GMN-SKYYMD      TO  T01-SKYYMD
           MOVE    SPA-GMN-HKNJANUM    TO  T01-HKNJANUM
           MOVE    SPA-GMN-HKNJAMEI    TO  T01-HKNJAMEI
           MOVE    SPA-GMN-HKNJANUM1   TO  T01-HKNJANUM1
           MOVE    SPA-GMN-HKNJAMEI1   TO  T01-HKNJAMEI1
           MOVE    SPA-GMN-TITLE       TO  T01-TITLE-VALUE
           MOVE    SPA-GMN-TITLE1      TO  T01-TITLE1-VALUE
           MOVE    SPA-GMN-TITLE2      TO  T01-TITLE2-VALUE
           MOVE   "red"                TO  T01-TITLE-STYLE
                                           T01-TITLE1-STYLE
                                           T01-TITLE2-STYLE
      *
           MOVE    SPA-B06-STATE       TO  T01-B06-STATE
           MOVE    SPA-B07-STATE       TO  T01-B07-STATE
           MOVE    SPA-B08-STATE       TO  T01-B08-STATE
           MOVE    SPA-B14-STATE       TO  T01-B14-STATE
           MOVE    SPA-B15-STATE       TO  T01-B15-STATE
           MOVE    SPA-B16-STATE       TO  T01-B16-STATE
           MOVE    SPA-B17-STATE       TO  T01-B17-STATE
           MOVE    SPA-B19-STATE       TO  T01-B19-STATE
           MOVE    SPA-B20-STATE       TO  T01-B20-STATE
           MOVE    SPA-B21-STATE       TO  T01-B21-STATE
           MOVE    SPA-DATA-STATE      TO  T01-DATA-STATE
           MOVE    SPA-NYUGAIKBN-STATE TO  T01-NYUGAIKBN-STATE
           MOVE    SPA-PRTKBN-STATE    TO  T01-PRTKBN-STATE
           MOVE    SPA-B09-STATE       TO  T01-B09-STATE
           MOVE    SPA-B10-STATE       TO  T01-B10-STATE
           MOVE    SPA-B22-STATE       TO  T01-B22-STATE
           MOVE    SPA-SYOKBN1-STATE   TO  T01-SYOKBN1-STATE
           MOVE    SPA-RECEDEN-CHK-STATE
                                       TO  T01-RECEDEN-CHK-STATE
      *
           MOVE    SPA-DATAKBN1-STATE  TO  T01-DATAKBN1-STATE
           MOVE    SPA-DATAKBN2-STATE  TO  T01-DATAKBN2-STATE
           MOVE    SPA-DATAKBN3-STATE  TO  T01-DATAKBN3-STATE
           MOVE    SPA-DATAKBN4-STATE  TO  T01-DATAKBN4-STATE
           MOVE    SPA-DATAKBN5-STATE  TO  T01-DATAKBN5-STATE
           MOVE    SPA-DATAKBN6-STATE  TO  T01-DATAKBN6-STATE
           MOVE    SPA-DATAKBN7-STATE  TO  T01-DATAKBN7-STATE
           MOVE    SPA-DATAKBN8-STATE  TO  T01-DATAKBN8-STATE
           MOVE    SPA-DATAKBN1-VALUE  TO  T01-DATAKBN1-VALUE
           MOVE    SPA-DATAKBN2-VALUE  TO  T01-DATAKBN2-VALUE
           MOVE    SPA-DATAKBN3-VALUE  TO  T01-DATAKBN3-VALUE
           MOVE    SPA-DATAKBN4-VALUE  TO  T01-DATAKBN4-VALUE
           MOVE    SPA-DATAKBN5-VALUE  TO  T01-DATAKBN5-VALUE
           MOVE    SPA-DATAKBN6-VALUE  TO  T01-DATAKBN6-VALUE
           MOVE    SPA-DATAKBN7-VALUE  TO  T01-DATAKBN7-VALUE
           MOVE    SPA-DATAKBN8-VALUE  TO  T01-DATAKBN8-VALUE
      *
           EVALUATE    SPA-DATAKBN4-VALUE
               WHEN    "T"
                   MOVE    WIDGET-NORMAL       TO  T01-CHK11-STATE
                                                   T01-CHK12-STATE
               WHEN    "F"
                   MOVE    WIDGET-INSENSITIVE  TO  T01-CHK11-STATE
                                                   T01-CHK12-STATE
           END-EVALUATE
           EVALUATE    SPA-DATAKBN5-VALUE
               WHEN    "T"
                   MOVE    WIDGET-NORMAL       TO  T01-CHK21-STATE
                                                   T01-CHK22-STATE
               WHEN    "F"
                   MOVE    WIDGET-INSENSITIVE  TO  T01-CHK21-STATE
                                                   T01-CHK22-STATE
           END-EVALUATE
           EVALUATE    SPA-DATAKBN1-VALUE
               WHEN    "T"
                   MOVE    WIDGET-NORMAL       TO  T01-CHK31-STATE
                                                   T01-CHK32-STATE
               WHEN    "F"
                   MOVE    WIDGET-INSENSITIVE  TO  T01-CHK31-STATE
                                                   T01-CHK32-STATE
           END-EVALUATE
           MOVE    SPA-CHK11-VALUE     TO  T01-CHK11-VALUE
           MOVE    SPA-CHK12-VALUE     TO  T01-CHK12-VALUE
           MOVE    SPA-CHK21-VALUE     TO  T01-CHK21-VALUE
           MOVE    SPA-CHK22-VALUE     TO  T01-CHK22-VALUE
           MOVE    SPA-CHK31-VALUE     TO  T01-CHK31-VALUE
           MOVE    SPA-CHK32-VALUE     TO  T01-CHK32-VALUE
      *
           MOVE    SPA-SYOKBN1-VALUE   TO  T01-SYOKBN1-VALUE
           MOVE    SPA-SYOKBN2-VALUE   TO  T01-SYOKBN2-VALUE
      *
           MOVE    SPA-GMN-DATAKBN-G   TO  T01-DATA
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-DATAKBN-MAX
               MOVE    SPA-GMN-DATAKBNLST (IDX)
                                           TO  T01-DATA-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-DATAKBN-MAX TO  T01-DATA-COUNT
      *
           MOVE    SPA-GMN-NYUGAIKBN-G TO  T01-NYUGAIKBN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-NYUGAIKBN-MAX
               MOVE    SPA-GMN-NYUGAIKBNLST (IDX)
                                           TO  T01-NYUGAIKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-NYUGAIKBN-MAX
                                       TO  T01-NYUGAIKBN-COUNT
      *
           MOVE    SPA-GMN-PRTKBN-G    TO  T01-PRTKBN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-PRTKBN-MAX
               MOVE    SPA-GMN-PRTKBNLST (IDX)
                                           TO  T01-PRTKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-PRTKBN-MAX  TO  T01-PRTKBN-COUNT
      *
           MOVE    SPA-GMN-RECEDEN-CHK-G   TO  T01-RECEDEN-CHK
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-RECEDEN-CHK-MAX
               MOVE    SPA-GMN-RECEDEN-CHKLST (IDX)
                                           TO  T01-RECEDEN-CHK-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-RECEDEN-CHK-MAX
                                       TO  T01-RECEDEN-CHK-COUNT
      *
           MOVE    SPA-GMN-PRTKBN1-VALUE
                                       TO  T01-PRTKBN1-VALUE
           MOVE    SPA-GMN-PRTKBN1-STYLE
                                       TO  T01-PRTKBN1-STYLE
           MOVE    SPA-GMN-PRTKBN2-VALUE
                                       TO  T01-PRTKBN2-VALUE
           MOVE    SPA-GMN-PRTKBN2-STYLE
                                       TO  T01-PRTKBN2-STYLE
           MOVE    SPA-GMN-PRTKBN3-VALUE
                                       TO  T01-PRTKBN3-VALUE
           MOVE    SPA-GMN-PRTKBN3-STYLE
                                       TO  T01-PRTKBN3-STYLE
           MOVE    SPA-GMN-PRTKBN4-VALUE
                                       TO  T01-PRTKBN4-VALUE
           MOVE    SPA-GMN-PRTKBN4-STYLE
                                       TO  T01-PRTKBN4-STYLE
           MOVE    SPA-GMN-PRTKBN5-VALUE
                                       TO  T01-PRTKBN5-VALUE
           MOVE    SPA-GMN-PRTKBN5-STYLE
                                       TO  T01-PRTKBN5-STYLE
      *
           MOVE    WIDGET-NORMAL       TO  T01-HKNJANUM1-STATE
      *
           IF      SPA-GMN-PRTKBN      =   "8" OR  "9"
               MOVE    WIDGET-NORMAL       TO  T01-B18-STATE
               MOVE    WIDGET-INSENSITIVE  TO  T01-B17-STATE
                                               T01-B06-STATE
                                               T01-B07-STATE
                                               T01-B08-STATE
                                               T01-B16-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  T01-B18-STATE
           END-IF
      *
           IF      SPA-NAI-SKYYM       <   200804
               MOVE    WIDGET-INSENSITIVE  TO  T01-CHK31-STATE
                                               T01-CHK32-STATE
                                               T01-HKNJANUM1-STATE
                                               T01-DATAKBN1-STATE
                                               T01-DATAKBN2-STATE
                                               T01-B08-STATE
               MOVE    "F"                 TO  T01-DATAKBN1-VALUE
                                               T01-DATAKBN2-VALUE
           END-IF
      *****MOVE    WIDGET-INSENSITIVE  TO  T01-HKNJANUM1-STATE
      *
           IF      SPA-GMN-DIRECT-HKNJANUM-MAX
                                       =   ZERO
               MOVE    "F"         TO  T01-DIRECT-HKNJANUM-COM-VISIBLE
                                       T01-HKNJANUM-TITLE-VISIBLE
           ELSE
               MOVE    "T"         TO  T01-DIRECT-HKNJANUM-COM-VISIBLE
                                       T01-HKNJANUM-TITLE-VISIBLE
               MOVE    "保険者番号"
                                   TO   T01-HKNJANUM-TITLE
               MOVE    SPA-GMN-DIRECT-HKNJANUM-G
                                   TO  T01-DIRECT-HKNJANUM
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-DIRECT-HKNJANUM-MAX
                   MOVE    SPA-GMN-DIRECT-HKNJANUMLST (IDX)
                                       TO  T01-DIRECT-HKNJANUM-ITEM(IDX)
               END-PERFORM
           END-IF
           MOVE    SPA-GMN-DIRECT-HKNJANUM-MAX
                                   TO  T01-DIRECT-HKNJANUM-COUNT
      *
           IF      SPA-GMN-SRYYMKBN-MAX
                                       =   ZERO
               MOVE    "F"                 TO  T01-SRYYMKBN-COM-VISIBLE
               MOVE    WIDGET-INSENSITIVE  TO  T01-SRYYMKBN-COM-STATE
           ELSE
               MOVE    "T"                 TO  T01-SRYYMKBN-COM-VISIBLE
               MOVE    WIDGET-NORMAL       TO  T01-SRYYMKBN-COM-STATE
               MOVE    SPA-GMN-SRYYMKBN-G
                                   TO  T01-SRYYMKBN
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-SRYYMKBN-MAX
                   MOVE    SPA-GMN-SRYYMKBNLST (IDX)
                                       TO  T01-SRYYMKBN-ITEM(IDX)
               END-PERFORM
           END-IF
           MOVE    SPA-GMN-SRYYMKBN-MAX
                                   TO  T01-SRYYMKBN-COUNT
      *
           IF      SPA-NAI-9102-EFOUT  =   "1"
               MOVE    "Ｅ・Ｆ"            TO  T01-B18-LABEL
               IF      SPA-GMN-PRTKBN      =   "9"
                   MOVE    "様式４"            TO  T01-B18-LABEL
               END-IF
           END-IF
      *
           STRING  "＜令和２年度記載要領対応プログラム適用済＞"
                                       DELIMITED  BY  SIZE
                                       INTO    T01-MSG-VALUE
           END-STRING
           MOVE    "blue"              TO  T01-MSG-STYLE
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *
           IF    ( SPA-ERRCD           =   SPACE)
           AND   ( SPA-GMN-CUR         =   ZERO )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
         EVALUATE    SPA-GMN-CUR
             WHEN    1
                    MOVE  "SKYYM"      TO  MCP-WIDGET   
             WHEN    2
                    MOVE  "SKYYMD"     TO  MCP-WIDGET   
             WHEN    3
                    MOVE  "HKNJANUM"   TO  MCP-WIDGET   
             WHEN    4
                    MOVE  "DATA"       TO  MCP-WIDGET   
             WHEN    5
                    MOVE  "NYUGAIKBN"  TO  MCP-WIDGET   
             WHEN    6
                    MOVE  "PRTKBN"     TO  MCP-WIDGET   
             WHEN    7
                    MOVE  "B12"        TO  MCP-WIDGET   
             WHEN    8
                    MOVE  "HKNJANUM1"  TO  MCP-WIDGET   
             WHEN    9
                    MOVE  "DIRECT_HKNJANUM"
                                       TO  MCP-WIDGET   
             WHEN   10
                    MOVE  "RECEDEN_CHK"
                                       TO  MCP-WIDGET   
             WHEN   11
                    MOVE  "SRYYMKBN"   TO  MCP-WIDGET
         END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC             SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SKYYM"
                   IF      SPA-GMN-SRYYMKBN-MAX    >   ZERO
                       MOVE    11              TO  SPA-GMN-CUR
                   ELSE
                       MOVE    2               TO  SPA-GMN-CUR
                   END-IF
               WHEN    "SRYYMKBN"
                   MOVE    2               TO  SPA-GMN-CUR
               WHEN    "SKYYMD"
                   MOVE    7               TO  SPA-GMN-CUR
               WHEN    "HKNJANUM"
                   MOVE    7               TO  SPA-GMN-CUR
               WHEN    "DATA"
                   MOVE    5               TO  SPA-GMN-CUR
               WHEN    "NYUGAIKBN"
                   MOVE    7               TO  SPA-GMN-CUR
               WHEN    "HKNJANUM1"
                   MOVE    7               TO  SPA-GMN-CUR
               WHEN    "PRTKBN"
                   MOVE    7               TO  SPA-GMN-CUR
               WHEN    "DIRECT_HKNJANUM"
                   MOVE    7               TO  SPA-GMN-CUR
               WHEN    "RECEDEN_CHK"
                   MOVE    7               TO  SPA-GMN-CUR
           END-EVALUATE
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           MOVE    SPACE                TO  TERR
           MOVE    SPA-ERRCD            TO  TERR-ERRCODE
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "診療年月が未入力です" 
                                               TO  TERR-ERRMSG
               WHEN    "0002"
                   MOVE    "請求年月日が未入力です" 
                                               TO  TERR-ERRMSG
               WHEN    "0003"
                   MOVE    "診療年月入力エラー" 
                                               TO  TERR-ERRMSG
               WHEN    "0004"
                   MOVE    "請求年月日入力エラー" 
                                               TO  TERR-ERRMSG
               WHEN    "0005"
                   MOVE    "保険者番号入力エラー" 
                                               TO  TERR-ERRMSG
               WHEN    "0006"
                   MOVE    "処理対象の帳票が選択してありません" 
                                               TO  TERR-ERRMSG
               WHEN    "0007"
                   MOVE
                      "総括表・社保の処理対象データが選択してありません"
                                               TO  TERR-ERRMSG
               WHEN    "0008"
                   MOVE
                      "総括表・国保の処理対象データが選択してありません"
                                               TO  TERR-ERRMSG
               WHEN    "0009"
                   MOVE
                  "総括表・広域の処理対象データが選択してありません"
                                               TO  TERR-ERRMSG
               WHEN    "0010"
                   MOVE
                      "連携基盤用のときは入外区分を設定してください"
                                               TO  TERR-ERRMSG
               WHEN    "0011"
                   STRING  "主科未設定データがあります"
                                               DELIMITED  BY  SIZE
                           "（主科未設定リストで確認可能です）"
                                               DELIMITED  BY  SIZE
                                               INTO  TERR-ERRMSG
                   END-STRING
               WHEN    "0012"
                   MOVE    "再印刷できない帳票です"    TO  TERR-ERRMSG
               WHEN    "0013"
                   STRING  "選択したファイル出力先は労災のときは"
                                               DELIMITED  BY  SIZE
                           "選択できません"    DELIMITED  BY  SIZE
                                               INTO  TERR-ERRMSG
                   END-STRING
               WHEN    "0014"
                   MOVE    "労災は選択できません"
                                               TO  TERR-ERRMSG
               WHEN    "0015"
                   MOVE    "入院・入院外は選択できません"
                                               TO  TERR-ERRMSG
               WHEN    "0016"
                   MOVE    "選択したファイル出力先は選択できません"
                                               TO  TERR-ERRMSG
               WHEN    "0017"
                   MOVE    "ファイル出力先入力エラー" 
                                               TO  TERR-ERRMSG
               WHEN    "0018"
                   IF      WRK-CHK-CNT         =   1
                       STRING  WRK-CHK-SHORIMSG 
                                               DELIMITED   BY  SPACE
                               "の処理スクリプトが存在しません"
                                               DELIMITED   BY  SIZE
                               "、システム管理で確認して下さい"
                                               DELIMITED   BY  SIZE
                                               INTO    TERR-ERRMSG
                       END-STRING
                   ELSE
                       MOVE    WRK-CHK-CNT     TO  WRK-Z9
                       STRING  WRK-CHK-SHORIMSG
                                               DELIMITED   BY  SPACE
                               "を含む"        DELIMITED   BY  SIZE
                               WRK-Z9          DELIMITED   BY  SIZE
                               "個の処理スクリプトが存在しません"
                                               DELIMITED   BY  SIZE
                                               INTO    TERR-ERRMSG
                       END-STRING
                   END-IF
               WHEN    "0020"
                   STRING  "改正パッチ提供以降に平成２４年３月以前の"
                                                   DELIMITED  BY  SIZE
                     "月遅れ分についてレセプト再処理が行われていません"
                                                   DELIMITED  BY  SIZE
                                                   INTO    TERR-ERRMSG
                   END-STRING
               WHEN    "0021"
                   MOVE
                       "職員情報でクライアント保存の設定をして下さい"
                                                       TO  TERR-ERRMSG
      *
               WHEN    "0050"
                   STRING
                       "月途中に医療機関コードが変更されているので、"
                                                   DELIMITED  BY  SIZE
                       "この期間の処理はできません"
                                                   DELIMITED  BY  SIZE
                                                   INTO    TERR-ERRMSG
                   END-STRING
               WHEN    "0051"
                   STRING
                       "月途中に医療機関コードが変更されている診療年月"
                                                   DELIMITED  BY  SIZE
                       "のため、期間指定による処理はできません"
                                                   DELIMITED  BY  SIZE
                                                   INTO    TERR-ERRMSG
                   END-STRING
      *
               WHEN    "0200"
                   MOVE
                       "平成２０年４月改正未対応のため処理できません"
                                                       TO  TERR-ERRMSG
      *
               WHEN    "1001"
                   MOVE    "パラメタＤＢの削除処理でエラーとなりました" 
                                                       TO  TERR-ERRMSG
               WHEN    "1002"
                   MOVE    "パラメタＤＢの更新処理でエラーとなりました" 
                                                       TO  TERR-ERRMSG
               WHEN    "1003"
                   MOVE    "パラメタＤＢが存在しません" 
                                                       TO  TERR-ERRMSG
               WHEN    "1004"
                   MOVE    "ジョブ管理ＤＢが削除できませんでした"
                                                       TO  TERR-ERRMSG
               WHEN    "1005"
                   MOVE
                   "システム管理ＤＢ（３００３）が削除できませんでした"
                                                       TO  TERR-ERRMSG
               WHEN    "1006"
                   MOVE    "システム管理の更新処理でエラーとなりました"
                                                       TO  TERR-ERRMSG
               WHEN    "1007"
                   MOVE
                       "該当の診療年月で医療機関情報が取得できません"
                                                       TO  TERR-ERRMSG
               WHEN    "7777"
                   MOVE    WRK-RECEERR                 TO  TERR-ERRMSG
               WHEN    "9999"
                   MOVE    WRK-RECEERR                 TO  TERR-ERRMSG
      *
               WHEN    "E88"
                   MOVE    "Ｄａｔａ＿Ｉｄが取得できません。"
                                                       TO  TERR-ERRMSG
           END-EVALUATE
      *
           MOVE    TERR-ERRMSG          TO  SPA-ERRMSG
      *
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "T01"                TO  SPA-MOTOPG
           MOVE    "TERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "TERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ１表示処理
      *****************************************************************
       520-TIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-TIDMSG
      *
           EVALUATE    SPA-TIDCD
               WHEN    "1012"
                   MOVE    "集計表（社保）の印刷処理を行います"
                                       TO  WRK-TIDMSG
               WHEN    "1013"
                   MOVE    "集計表（国保）の印刷処理を行います"
                                       TO  WRK-TIDMSG
               WHEN    "1034"
                   MOVE    "自賠責ＰＤＦファイルを作成します"
                                       TO  WRK-TIDMSG
               WHEN    "2002"
                   STRING   "定点調査研究事業（データ収集）用の"
                                       DELIMITED  BY  SIZE
                            "ファイルを作成しますか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-TIDMSG
                   END-STRING
               WHEN    "2004"
                   STRING   "定点調査収集サーバへアップロードする"
                                       DELIMITED  BY  SIZE
                            "前にデータを確認しますか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-TIDMSG
                   END-STRING
               WHEN    "6666"
                   MOVE    "印刷処理中です"
                                            TO  WRK-TIDMSG
               WHEN    "8888"
                   MOVE    "処理が正常終了しました"
                                            TO  WRK-TIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  TID1
           MOVE    SPA-TIDCD            TO  TID1-ID1CODE
           MOVE    WRK-TIDMSG           TO  TID1-ID1MSG
      *
           EVALUATE    SPA-TIDCD
               WHEN    "2002"
               WHEN    "2004"
               WHEN    "2007"
                   MOVE    "いいえ"         TO  TID1-B01-LABEL
                   MOVE    "は  い"         TO  TID1-B12-LABEL
                   MOVE    "B01"            TO  MCP-WIDGET
               WHEN    OTHER
                   MOVE    "戻る"           TO  TID1-B01-LABEL
                   MOVE    "ＯＫ"           TO  TID1-B12-LABEL
                   MOVE    "B12"                TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "T01"                TO  SPA-MOTOPG
           MOVE    "TID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "TID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-TIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ２表示処理
      *****************************************************************
       530-TID2SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-TIDMSG
      *
           EVALUATE    SPA-TIDCD
               WHEN    "1001"
                   MOVE    "総括表・請求書の印刷処理を行います"
                                       TO  WRK-TIDMSG
               WHEN    "1002"
                   MOVE    "総括表の印刷処理を行います"
                                       TO  WRK-TIDMSG
               WHEN    "1003"
                   MOVE    "請求書の印刷処理を行います"
                                       TO  WRK-TIDMSG
               WHEN    "1004"
                   MOVE    "総括表（社保）の印刷処理を行います"
                                       TO  WRK-TIDMSG
               WHEN    "1005"
                   MOVE    "総括表（国保）の印刷処理を行います" 
                                       TO  WRK-TIDMSG
               WHEN    "1006"
                   MOVE    "レセ電総括表（国保）の印刷処理を行います" 
                                       TO  WRK-TIDMSG
               WHEN    "1007"
                   MOVE    "総括表（公害）の印刷処理を行います" 
                                       TO  WRK-TIDMSG
               WHEN    "1008"
                   MOVE    "総括表（広域）の印刷処理を行います" 
                                       TO  WRK-TIDMSG
               WHEN    "1009"
                   MOVE
                       "レセ電総括表（広域）の印刷処理を行います" 
                                       TO  WRK-TIDMSG
               WHEN    "1010"
                   MOVE    "社保分のレセプト電算データを作成します"
                                       TO  WRK-TIDMSG
               WHEN    "1011"
                   MOVE    "チェックリストの印刷処理を行います"
                                       TO  WRK-TIDMSG
               WHEN    "1014"
                   MOVE    "国保分のレセプト電算データを作成します"
                                       TO  WRK-TIDMSG
               WHEN    "1015"
                   MOVE
                       "社保分（入院）のレセプト電算データを作成します"
                                       TO  WRK-TIDMSG
               WHEN    "1016"
                   MOVE
                   "社保分（入院外）のレセプト電算データを作成します"
                                       TO  WRK-TIDMSG
               WHEN    "1017"
                   MOVE
                       "国保分（入院）のレセプト電算データを作成します"
                                       TO  WRK-TIDMSG
               WHEN    "1018"
                   MOVE
                   "国保分（入院外）のレセプト電算データを作成します"
                                       TO  WRK-TIDMSG
               WHEN    "1019"
                   MOVE    "主科未設定リストの印刷処理を行います"
                                       TO  WRK-TIDMSG
               WHEN    "1020"
                   MOVE
                       "広域分（入院）のレセプト電算データを作成します"
                                       TO  WRK-TIDMSG
               WHEN    "1021"
                   MOVE
                   "広域分（入院外）のレセプト電算データを作成します"
                                       TO  WRK-TIDMSG
               WHEN    "1022"
                   MOVE    "広域分のレセプト電算データを作成します"
                                       TO  WRK-TIDMSG
               WHEN    "1023"
                   STRING  "国保・広域分（入院）のレセプト電算データを"
                                           DELIMITED  BY  SIZE
                           "作成します"    DELIMITED  BY  SIZE
                                           INTO  WRK-TIDMSG
                   END-STRING
               WHEN    "1024"
                   STRING
                       "国保・広域分（入院外）のレセプト電算データを"
                                           DELIMITED  BY  SIZE
                           "作成します"    DELIMITED  BY  SIZE
                                           INTO  WRK-TIDMSG
                   END-STRING
               WHEN    "1025"
                   MOVE
                       "国保・広域分のレセプト電算データを作成します"
                                       TO  WRK-TIDMSG
               WHEN    "1026"
                   MOVE    "総括表（労災）の印刷処理を行います" 
                                       TO  WRK-TIDMSG
               WHEN    "1027"
                   MOVE    "労災分のレセプト電算データを作成します"
                                       TO  WRK-TIDMSG
               WHEN    "1028"
                   MOVE
                       "労災分（入院）のレセプト電算データを作成します"
                                       TO  WRK-TIDMSG
               WHEN    "1029"
                   MOVE
                   "労災分（入院外）のレセプト電算データを作成します"
                                       TO  WRK-TIDMSG
               WHEN    "1030"
                   MOVE    "ＥＦファイルを作成します"
                                       TO  WRK-TIDMSG
               WHEN    "1031"
                   MOVE    "ＥＦファイル（入院）を作成します"
                                       TO  WRK-TIDMSG
               WHEN    "1032"
                   MOVE    "ＥＦファイル（入院外）を作成します"
                                       TO  WRK-TIDMSG
               WHEN    "1033"
                   MOVE    "様式４を作成します（調査票の印刷を指定）"
                                       TO  WRK-TIDMSG
           END-EVALUATE
      *
           EVALUATE    SPA-TIDCD
               WHEN    "1010"
               WHEN    "1014"
               WHEN    "1015"
               WHEN    "1016"
               WHEN    "1017"
               WHEN    "1018"
               WHEN    "1020"
               WHEN    "1021"
               WHEN    "1022"
               WHEN    "1023"
               WHEN    "1024"
               WHEN    "1025"
               WHEN    "1027"
               WHEN    "1028"
               WHEN    "1029"
               IF      FLG-RECEDEN-CHK      =   1
                   STRING  "月遅れ分のレセ電データがありません。"
                                               DELIMITED  BY  SIZE
                           "このままレセプト電算データを作成しますか"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-TIDMSG
                   END-STRING
               END-IF
           END-EVALUATE
      *
           INITIALIZE                  TID2
      *
           MOVE    "black"         TO  TID2-ID1MSG-STYLE
           MOVE    "blue"          TO  TID2-LABEL-STYLE
      *
           MOVE    SPA-TIDCD       TO  TID2-ID1CODE
      *
           EVALUATE    SPA-TIDCD
               WHEN    "1010"
               WHEN    "1014"
               WHEN    "1015"
               WHEN    "1016"
               WHEN    "1017"
               WHEN    "1018"
               WHEN    "1020"
               WHEN    "1021"
               WHEN    "1022"
               WHEN    "1023"
               WHEN    "1024"
               WHEN    "1025"
                   IF      SPA-GMN-PRTKBN    =   "6" OR  "7"
                     MOVE    WRK-TIDMSG      TO  TID2-ID1MSG
                   ELSE
                     IF  (   SPA-NAI-200501-RECEDEN-LST1KBN
                                           =   "1"         )
                     OR  ( ( SPA-NAI-200501-RECEDEN-LST2KBN
                                           =   "1"    )    AND
                           ( SPA-GMN-PRTKBN  =   "1"    )        )
                     OR  ( ( SPA-GMN-RECEDEN-CHK
                                           =   "1"    )    AND
                           ( SPA-NAI-SKYYM   >=  201204 )        )
                       MOVE    SPACE           TO  WRK-TIDMSG1
                                                   WRK-TIDCD-MSG
      *
                       IF      SPA-NAI-200501-RECEDEN-LST1KBN
                                               =   "1"
                           MOVE    "、症状詳記"      TO  WRK-TIDMSG1
                       END-IF
                       IF  ( ( SPA-NAI-200501-RECEDEN-LST2KBN
                                               =   "1" )   AND
                             ( SPA-GMN-PRTKBN  =   "1" )       )
                           STRING  WRK-TIDMSG1     DELIMITED  BY  SPACE
                                   "、光ディスク等送付書"
                                                   DELIMITED  BY  SIZE
                                                   INTO  WRK-TIDCD-MSG
                           END-STRING
                           MOVE    WRK-TIDCD-MSG   TO  WRK-TIDMSG1
                           MOVE    SPACE           TO  WRK-TIDCD-MSG
                         END-IF
                       IF    ( SPA-GMN-RECEDEN-CHK
                                               =   "1"    )
                       AND   ( SPA-NAI-SKYYM   >=  201204 )
                           STRING  WRK-TIDMSG1     DELIMITED  BY  SPACE
                                  "、レセ電チェックリスト"
                                                   DELIMITED  BY  SIZE
                                                   INTO  WRK-TIDCD-MSG
                           END-STRING
                           MOVE    WRK-TIDCD-MSG   TO  WRK-TIDMSG1
                           MOVE    SPACE           TO  WRK-TIDCD-MSG
                       END-IF
      * 
                       STRING   WRK-TIDMSG         DELIMITED  BY  SPACE
                                "（"               DELIMITED  BY  SIZE
                                WRK-TIDMSG1 (3:)   DELIMITED  BY  SPACE
                                  "の印刷を指定）" DELIMITED  BY  SIZE
                                                   INTO    TID2-ID1MSG
                       END-STRING
                     ELSE
                       MOVE    WRK-TIDMSG      TO  TID2-ID1MSG
                     END-IF
                   END-IF
                   IF      FLG-RECEDEN-CHK =   1
                       MOVE    "red"           TO  TID2-ID1MSG-STYLE
                   END-IF
                   MOVE    "blue"              TO  TID2-LABEL-STYLE
                   IF      SPA-GMN-PRTKBN      =   "2"
                       MOVE    "【点検用】"        TO  TID2-LABEL
                   END-IF
                   IF      SPA-GMN-PRTKBN      =   "6" OR  "7"
                       IF      SPA-NAI-9101-COOPBASE   =   "2"
                           MOVE    "【連携用（全件：未同意分含）】"
                                                       TO  TID2-LABEL
                       ELSE
                           MOVE    "【連携用】"        TO  TID2-LABEL
                       END-IF
                   END-IF
      *
                   EVALUATE    SPA-TIDCD
                       WHEN    "1010"
                       WHEN    "1015"
                       WHEN    "1016"
                           IF      SPA-GMN-DIRECT-HKNJANUM-MAX
                                                       >   ZERO
                               STRING   TID2-LABEL DELIMITED  BY  SPACE
                                        SPA-GMN-DIRECT-HKNJANAME
                                                   DELIMITED  BY  SPACE
                                                   INTO       TID2-LABEL
                               END-STRING  
                           END-IF
                   END-EVALUATE
               WHEN    "1027"
               WHEN    "1028"
               WHEN    "1029"
                   IF ( ( SPA-NAI-200501-RECEDEN-LST2KBN
                                           =   "1"    )    AND
                         ( SPA-GMN-PRTKBN  =   "1"    )        )
                   OR  ( ( SPA-GMN-RECEDEN-CHK
                                           =   "1"    )    AND
                         ( SPA-NAI-SKYYM   >=  201307 )        )
                       MOVE    SPACE           TO  WRK-TIDMSG1
                                                   WRK-TIDCD-MSG
      *
                       IF  ( ( SPA-NAI-200501-RECEDEN-LST2KBN
                                               =   "1" )   AND
                             ( SPA-GMN-PRTKBN  =   "1" )       )
                           MOVE    "、光ディスク等送付書"
                                                   TO  WRK-TIDMSG1
                       END-IF
                       IF  ( ( SPA-GMN-RECEDEN-CHK
                                               =   "1"    )    AND
                             ( SPA-NAI-SKYYM   >=  201307 )        )
                           STRING  WRK-TIDMSG1     DELIMITED  BY  SPACE
                                  "、レセ電チェックリスト"
                                                   DELIMITED  BY  SIZE
                                                   INTO  WRK-TIDCD-MSG
                           END-STRING
                           MOVE    WRK-TIDCD-MSG   TO  WRK-TIDMSG1
                           MOVE    SPACE           TO  WRK-TIDCD-MSG
                       END-IF
      * 
                       STRING   WRK-TIDMSG         DELIMITED  BY  SPACE
                                "（"               DELIMITED  BY  SIZE
                                WRK-TIDMSG1 (3:)   DELIMITED  BY  SPACE
                                  "の印刷を指定）" DELIMITED  BY  SIZE
                                                   INTO    TID2-ID1MSG
                       END-STRING
                   ELSE
                       MOVE    WRK-TIDMSG      TO  TID2-ID1MSG
                   END-IF
                   IF      FLG-RECEDEN-CHK =   1
                       MOVE    "red"           TO  TID2-ID1MSG-STYLE
                   END-IF
                   MOVE    "blue"              TO  TID2-LABEL-STYLE
                   IF      SPA-GMN-PRTKBN      =   "2"
                       MOVE    "【点検用】"        TO  TID2-LABEL
                   END-IF
               WHEN    "1030"
               WHEN    "1031"
               WHEN    "1032"
               WHEN    "1033"
                   MOVE    WRK-TIDMSG      TO  TID2-ID1MSG
      ***          MOVE    "【ＥＦファイル】"  TO  TID2-LABEL
               WHEN    OTHER
                   MOVE    WRK-TIDMSG      TO  TID2-ID1MSG
           END-EVALUATE
      *
           MOVE    "F10:プレビュー"
                                   TO  TID2-B10-LABEL
           MOVE    "F11:印刷しない"
                                   TO  TID2-B11-LABEL
           MOVE    "T"             TO  TID2-B10-VISIBLE
           MOVE    "T"             TO  TID2-B11-VISIBLE
           MOVE    "F12:印刷する"  TO  TID2-B12-LABEL
      *
           EVALUATE    SPA-TIDCD
               WHEN    "1010"
               WHEN    "1014"
               WHEN    "1015"
               WHEN    "1016"
               WHEN    "1017"
               WHEN    "1018"
               WHEN    "1020"
               WHEN    "1021"
               WHEN    "1022"
               WHEN    "1023"
               WHEN    "1024"
               WHEN    "1025"
                   IF      SPA-GMN-PRTKBN    =   "6" OR  "7"
                     MOVE    "F"             TO  TID2-B10-VISIBLE
                     MOVE    "F"             TO  TID2-B11-VISIBLE
                     MOVE    "F12:作成する"  TO  TID2-B12-LABEL
                   ELSE
                     IF  (   SPA-NAI-200501-RECEDEN-LST1KBN
                                             =   "1"             )
                     OR  ( ( SPA-NAI-200501-RECEDEN-LST2KBN
                                             =   "1"    )   AND
                           ( SPA-GMN-PRTKBN  =   "1"    )        )
                     OR  ( ( SPA-GMN-RECEDEN-CHK
                                             =   "1"    )    AND
                           ( SPA-NAI-SKYYM   >=  201204 )        )
                       MOVE    "T"             TO  TID2-B10-VISIBLE
                       MOVE    "T"             TO  TID2-B11-VISIBLE
                       MOVE    "F12:印刷する"  TO  TID2-B12-LABEL
                     ELSE
                       MOVE    "F"             TO  TID2-B10-VISIBLE
                       MOVE    "F"             TO  TID2-B11-VISIBLE
                       MOVE    "F12:作成する"  TO  TID2-B12-LABEL
                     END-IF
                   END-IF
               WHEN    "1027"
               WHEN    "1028"
               WHEN    "1029"
                   IF ( ( SPA-NAI-200501-RECEDEN-LST2KBN
                                           =   "1"    )    AND
                         ( SPA-GMN-PRTKBN  =   "1"    )        )
                   OR  ( ( SPA-GMN-RECEDEN-CHK
                                           =   "1"    )    AND
                         ( SPA-NAI-SKYYM   >=  201307 )        )
                       MOVE    "T"             TO  TID2-B10-VISIBLE
                       MOVE    "T"             TO  TID2-B11-VISIBLE
                       MOVE    "F12:印刷する"  TO  TID2-B12-LABEL
                   ELSE
                       MOVE    "F"             TO  TID2-B10-VISIBLE
                       MOVE    "F"             TO  TID2-B11-VISIBLE
                       MOVE    "F12:作成する"  TO  TID2-B12-LABEL
                   END-IF
               WHEN    "1030"
               WHEN    "1031"
               WHEN    "1032"
                   MOVE    "F"             TO  TID2-B10-VISIBLE
                   MOVE    "F"             TO  TID2-B11-VISIBLE
                   MOVE    "F12:作成する"  TO  TID2-B12-LABEL
               WHEN    "1033"
                   MOVE    "T"             TO  TID2-B10-VISIBLE
                   MOVE    "T"             TO  TID2-B11-VISIBLE
                   MOVE    "F12:印刷する"  TO  TID2-B12-LABEL
           END-EVALUATE
      *
           EVALUATE    SPA-TIDCD
               WHEN    "1010"
               WHEN    "1014"
               WHEN    "1015"
               WHEN    "1016"
               WHEN    "1017"
               WHEN    "1018"
               WHEN    "1020"
               WHEN    "1021"
               WHEN    "1022"
               WHEN    "1023"
               WHEN    "1024"
               WHEN    "1025"
      *            レセ電データ作成時
                   IF      SPA-GMN-PRTKBN      =   "6" OR  "7"
                       CONTINUE
                   ELSE 
                       IF      SPA-GMN-SRYYMKBN-MAX
                                                   >   ZERO
                           STRING   TID2-LABEL     DELIMITED  BY  SPACE
                                    "　【"         DELIMITED  BY  SPACE
                                    SPA-NAI-SRYYMKBN-ST-PRT
                                               (SPA-NAI-SRYYMKBN-SELNUM)
                                                   DELIMITED  BY  SPACE
                                    "〜"           DELIMITED  BY  SPACE
                                    SPA-NAI-SRYYMKBN-ED-PRT
                                               (SPA-NAI-SRYYMKBN-SELNUM)
                                                   DELIMITED  BY  SPACE
                                    "】"           DELIMITED  BY  SPACE
                                                   INTO       TID2-LABEL
                           END-STRING
                       END-IF
                   END-IF
               WHEN    "1001"
               WHEN    "1002"
               WHEN    "1003"  
               WHEN    "1004"
               WHEN    "1005"
               WHEN    "1008"
      *            総括表作成時 
                   IF      SPA-GMN-SRYYMKBN-MAX
                                               >   ZERO
                       STRING   TID2-LABEL     DELIMITED  BY  SPACE
                                "　【"         DELIMITED  BY  SPACE
                                SPA-NAI-SRYYMKBN-ST-PRT
                                               (SPA-NAI-SRYYMKBN-SELNUM)
                                               DELIMITED  BY  SPACE
                                "〜"           DELIMITED  BY  SPACE
                                SPA-NAI-SRYYMKBN-ED-PRT
                                               (SPA-NAI-SRYYMKBN-SELNUM)
                                               DELIMITED  BY  SPACE
                                "】"           DELIMITED  BY  SPACE
                                               INTO       TID2-LABEL
                       END-STRING
                   END-IF     
           END-EVALUATE
      *
           MOVE    WIDGET-NORMAL   TO  TID2-B10-STATE
                                       TID2-B11-STATE
      *
           MOVE    ZERO            TO  FLG-RECEDEN-CHK
      *
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "T01"           TO  SPA-MOTOPG
           MOVE    "TID2"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "TID2"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       530-TID2SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ipアドレス取得
      *****************************************************************
       900-SYSTEM-GET-SEC          SECTION.
      *
           INITIALIZE                  SLDUSER-AREA
           MOVE    2               TO  SLDUSER-KBN
           MOVE    MCP-TERM        TO  SLDUSER-ID
           CALL    "ORCSLDUSER"        USING
                                       SLDUSER-AREA
                                       SPA-AREA
      *
           .
       900-SYSTEM-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
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
      *    保険者マスター読込（キー）
      *****************************************************************
       900-HKNJAINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNJAINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-HKNJAINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科情報読込
      *****************************************************************
       900-SYUKA-READ-SEC         SECTION.
      *
           MOVE    "tbl_syuka"         TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syuka"         TO  MCP-TABLE
               MOVE    "key16"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYUKA
                   MOVE    MCPDATA-REC         TO  SYUKA-REC
               ELSE
                   MOVE    1                   TO  FLG-SYUKA
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYUKA
           END-IF
      *
           MOVE    "tbl_syuka"         TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYUKA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-READ-N-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-RECE10
               MOVE    MCPDATA-REC         TO  RECE10-REC
               IF      RECE10-RECESYUBETU  =    "6666"
                   GO  TO  900-SEIKYU-READ-N-SEC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE10
           END-IF
           .
       900-SEIKYU-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-READ-SEC             SECTION.
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE10
           END-IF
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電ＤＢ読込
      *****************************************************************
       900-RECEDEN-READ-SEC             SECTION.
      *
           MOVE    "tbl_receden"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_receden"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECEDEN
               ELSE
                   MOVE    1                   TO  FLG-RECEDEN
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECEDEN
           END-IF
      *
           MOVE    "tbl_receden"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-RECEDEN-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
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
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
               CALL    "coblog" USING "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
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
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *
