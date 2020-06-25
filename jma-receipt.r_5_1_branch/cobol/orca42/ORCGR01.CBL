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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGR01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : レセプト作成-個別指示（Ｒ０１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/05/24  患者番号ゼロ詰め処理
      *  01.00.02    MCC-藤原     01/06/27  追加・変更時の項目チェック追加
      *  01.00.03    MCC-藤原     01/07/16  前回表示・クリアのボタン追加
      *  01.00.04    MCC-藤原     01/07/16  日付画面用チェックサブ追加
      *  01.00.05    MCC-小豆沢   01/08/02  ＤＢＣＯＭＭＩＴ追加
      *  01.00.06    MCC-藤原     01/09/11  番号の編集追加
      *  01.01.01    MCC-藤原     01/09/21  JOINの修正
      *  01.01.02    MCC-小豆沢   01/11/28  SPA-TERMIDの桁数変更に伴う
      *                                     部分参照に変更      
      *  01.01.03    MCC-藤原     01/11/28  氏名検索への遷移の修正
      *  01.01.04    MCC-藤原     02/01/31  ジョブ管理ＤＢの追加
      *  01.01.05    MCC-藤原     02/02/19  処理中でも処理を終了できるように
      *                                     修正
      *  01.01.06    MCC-藤原     02/02/21  処理中のときのメッセージに
      *                                     プログラム名も表示
      *  01.01.07    MCC-藤原     02/02/26  レセプト管理ファイルを
      *                                     索引ファイルからＤＢへ変更
      *  01.01.08    MCC-藤原     02/04/03  県単用項目編集プログラム名の追加
      *  01.01.09    MCC-藤原     02/04/10  SRYACCT-KEY7をKEY9に変更
      *  01.01.10    MCC-藤原     02/05/30  情報削除ボタン追加
      *  01.01.11    MCC-藤原     02/06/06  処理結果画面追加
      *  01.01.12    MCC-藤原     02/06/17  労災・自賠責の処理を追加
      *  01.01.13    MCC-藤原     02/07/05  労災・医保の対象があるかチェック
      *  01.01.14    NACL-藤原    02/07/18  確定のとき自動的に情報確認画面を表示
      *  01.01.15    NACL-藤原    02.09.06  タイマー機能の追加
      *
      *  01.02.01    NACL-藤原    02/09/12  ディレクトリをサブプロへ
      *  01.02.02    NACL-藤原    02/09/24  ２００２．１０改正対応
      *  01.02.03    NACL-藤原    02/11/06  前回作成のときの年月のチェック
      *　入院機能追加　
      *  01.03.01    NACL-藤原    02/12/11  入外区分追加 
      *  01.03.02    NACL-藤原    03/02/27  入院用県単用項目編集プログラム名の追加
      *
      *  ２００３．４改正対応
      *  01.03.01    NACL-藤原    03/04/17  対象がないときエラーを表示する
      *  01.03.02    NACL-藤原    03/05/22  県番号をパラメタに追加
      *
      *  労災・自賠責入院機能追加
      *  01.04.01    NACL-藤原    03/06/05  労災・自賠責の入院対象が
      *                                     あるかチェック
      *  01.04.02    NACL-藤原    03/08/05  外来レセ電処理の追加
      *  01.04.03    NACL-門脇    04/01/14  診療行為の包括分は除く
      *                                     SRYACCT9→30
      *  01.04.04    NACL-藤原    04/03/08  コラムリストの行制御
      *  01.04.05    NACL-藤原    04/05/06  保険組合せが無いときの
      *                                     エラーを追加
      *  01.04.06    NACL-藤原    04/05/07  処理開始前にジョブ管理の
      *                                     チェックを再度行なう
      *  01.04.07    NACL-藤原    04/07/26  会計を残して入院取消を行った
      *                                     ままの入院会計のエラー対応
      *  中止処理追加
      *  01.05.01    NACL-藤原    04/09/28 中止ボタンを追加
      *  01.05.02    NACL-藤原    04/12/20 受診日指定
      *                                    ファンクションキーの変更
      *  01.05.03    NACL-藤原    05/01/30 訂正日指定
      *
      *  02.06.01    NACL-藤原    05/06/22 自賠責レセプトの様式追加
      *  02.06.02    NACL-藤原    05/08/30 定期請求との排他制御処理
      *
      *  02.07.01    NACL-藤原    05/10/31  MONFUNC対応
      *  02.07.02    NACL-藤原    05/12/12  国保レセプト編綴順対応
      *  02.07.03    NACL-藤原    06/02/09  エラー時のカーソル位置の変更
      *
      *  02.08.01    NACL-藤原    06/02/20  主科対応
      *
      *  03.02.01    NACL-藤原    06/07/26  自賠責の枠印刷選択機能対応
      *
      *  03.02.01    NACL-藤原    06/08/01  国保主科対応
      *
      *  03.03.01    NACL-藤原    06/09/27  ２００６年１０月改正対応
      *                                     個別作成時のパラメタへの診療
      *                                     年月の設定の変更 
      *                                     労災のとき２００６年１０月改
      *                                     正前
      *
      *  03.04.01    NACL-藤原    06/11/01  照会機能連携対応
      *                                     最大行を５００に変更 
      *  03.04.02    NACL-藤原    06/12/05  未請求レセ対応
      *  03.04.03    NACL-藤原    06/12/22  自賠責健保準拠対応
      *  03.04.04    NACL-藤原    07/04/26  削除処理時の画面編集の修正
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/19  自賠責の様式選択対応
      *                                     （平成１９年４月改正様式）
      *  03.05.03    NACL-藤原    07/07/13  診療行為がない請求データの
      *                                     削除対応
      *
      *  04.01.01    NACL-藤原    07/10/25  公務災害対応
      *  04.01.02    NACL-藤原    07/11/22  公害レセプト対応
      *
      *  04.02.01    NACL-藤原    08/03/04  平成２０年４月改正対応
      *  04.02.02    NACL-藤原    08/04/11  未請求解除時の処理
      *
      *  04.03.01    NACL-藤原    08/06/20  データ入力中の未請求設定遷移
      *                                     を不可とする
      *  04.03.02    NACL-藤原    08/06/19  医師別出力対応
      *  04.03.03    NACL-藤原    08/07/09  仮レセ作成（院外処方含む）対応
      *  04.03.04    NACL-藤原    08/08/07  労災の枠印刷選択機能対応
      *  04.03.05    NACL-藤原    08/08/11  コメント記載箇所選択機能対応
      *
      *  04.05.01    NACL-藤原    09/06/11  病棟別出力対応
      *  04.05.02    NACL-藤原    09/06/18  社保レセプト遍綴順対応
      *  04.05.03    NACL-藤原    09/08/31  未請求設定チェックを医保分のみ
      *                                     行う
      *  04.05.04    NACL-小豆沢  10/02/18  スクリプト起動パスサーチ対応
      *  04.05.05    NACL-藤原    10/03/03  平成２２年４月改正対応
      *                                     平成２２年４月分以降の訂正日
      *                                     の処理は行わない
      *  04.05.06    NACL-藤原    10/03/29  平成２２年４月改正対応
      *                                     平成２２年４月分以降の訂正日
      *                                     の処理を元に戻す
      *  04.05.07    NACL-藤原    11/08/02  労災新様式対応
      *  04.05.08    NACL-藤原    11/10/24  「請求しない」設定の請求管理
      *                                     の削除処理の有無対応
      *  04.05.09    NACL-藤原    11/11/22  同日再入院がある場合のチェック
      *                                     追加
      *
      *  04.06.01    NACL-藤原    10/06/23  レセ電請求時の受付エラー分の
      *                                     種別不明レセ対応
      *                                     不明分ありのとき赤字でエラー
      *                                     表示を行う
      *  04.06.02    NACL-藤原    11/12/12  点検レセ０点公費印字対応
      *
      *  04.07.01    NACL-藤原    11/10/26  労災請求書作成対応
      *  04.07.02    NACL-藤原    12/02/13  健保組合への直接請求と審査
      *                                     支払機関請求対応
      *  04.07.03    NACL-藤原    12/11/20  リアルタイムプレビュー時の
      *                                     一画面対応
      *  04.07.04    NACL-藤原    12/12/03  ターミナルデジットによる出力対応
      *  04.07.05    NACL-藤原    12/11/21  労災レセ電作成対応
      *  04.07.06    NACL-藤原    13/12/09  医療情報連携基盤対応
      *  04.07.07    NACL-藤原    13/09/20  第三者行為対応
      *  04.07.08    NACL-藤原    13/12/04  自賠責（労災準拠）のレセプト
      *                                     様式追加対応
      *  04.07.09    NACL-藤原    14/04/22  ＥＦファイル作成対応
      *                                     医療情報連携基盤対応
      *  04.07.10    NACL-藤原    14/06/02  医保分で受診日指定時、医保＋
      *                                     第三者行為も対象とする
      *  04.07.11    NACL-藤原    15/04/10  自賠責の請求管理削除処理に条件
      *                                     を追加
      *  04.07.12    NACL-藤原    15/05/14  「自賠責・様式選択」の初期表
      *                                     示変更対応
      *
      *  04.08.01    NACL-藤原    12/01/08  包括診療分の点検用レセプト記
      *                                     載対応
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.03    NACL-藤原    15/10/09  削除処理を行わない場合の表示
      *  04.08.04    NACL-藤原    16/05/20  情報削除から遷移時の共通スパ
      *                                     設定の修正
      *  04.08.05    NACL-藤原    16/08/24  持参薬対応
      *  04.08.06    NACL-藤原    16/10/10  個別作成の最大を２０００に変更
      *  04.08.07    NACL-藤原    17/04/20  ＥＦファイル、連携基盤のときの
      *                                     診療会計のチェックを変更
      *  04.08.08    NACL-藤原    17/04/10  患者単位での点数算定設定
      *                                     （労災準拠又は健保準拠）対応
      *
      *  05.00.01    NACL-藤原    16/12/12  ＨＡＯＲＩ対応
      *  05.00.02    NACL-藤原    19/03/25  元号改正対応
      *                                     Warning 対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル名称
           COPY    "CPTEMPFL.INC"   REPLACING  //TEMPFLPARA//
                                    BY         //RECEERR//.
      *
      *    エラーファイル名称領域 
           COPY    "CPERRFL.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    レセプト作成指示共通パラメタ
           COPY    "R01COMMON-SPA".
      *
      *   
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
           COPY    "R01SCR-SPA".
      *
       01  SPA-RECE07-REC. 
           COPY    "CPRCF007.INC"  REPLACING  //RECE07//
                                   BY         //SPA-RECE07//.
           03  SPA-RECE07-SRYKA-KOKUHO
                                   PIC X(02).
           03  SPA-RECE07-SRYKA-KOUIKI
                                   PIC X(02).
           03  SPA-RECE07-FUMEI-PAGE
                                   PIC 9(05).
           03  SPA-RECE07-HKNJANUM-SYAHO-T.
               05  SPA-RECE07-HKNJANUM-SRT-SYAHO
                                   PIC X(01).
               05  SPA-RECE07-HKNJANUM-SYAHO-MAX
                                   PIC  9(02).
               05  SPA-RECE07-HKNJANUM-SYAHO
                                   PIC X(08)   OCCURS  10.
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-RECE07          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01). 
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-BTPARA          PIC 9(01).
           03  FLG-SYUKA           PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-ROW             PIC 9(01).
           03  FLG-ZAISKBKBN       PIC 9(01). 
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-SEIKYU-UMU      PIC 9(01).
           03  FLG-TAISYO          PIC 9(01).
           03  FLG-4001-RSIIRYOKBN PIC X(01).
      *
           03  FLG-END2            PIC 9(01).
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-SPATMP          PIC 9(01).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDXX                PIC 9(04).
      *
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-PTID                          PIC 9(10).
               05  KEY-N-HKNCOMBI                      PIC 9(04).
               05  KEY-N-SRYKA                         PIC X(02).
           03  KEY-OLD.
               05  KEY-O-PTID                          PIC 9(10).
               05  KEY-O-HKNCOMBI                      PIC 9(04).
               05  KEY-O-SRYKA                         PIC X(02).
      *
      *    画面用ワーク（画面ＳＰＡにあわせる）
       01  WRK-GMN-SORT.
           03  WRK-GMN-TBL.
               05  WRK-GMN-TBL-G           OCCURS   2000.
                   07  WRK-GMN-TRENNUM PIC 9(04).
                   07  WRK-GMN-TTSTPTNUMKBN
                                       PIC X(02).
                   07  WRK-GMN-TPTNUM  PIC X(20).
                   07  WRK-GMN-TNAME   PIC X(50).
                   07  WRK-GMN-TSRYYM  PIC X(06).
                   07  WRK-GMN-TNYUGAIKBN
                                       PIC X(01).
                   07  WRK-GMN-TTAISYO PIC X(02).
                   07  WRK-GMN-TIHO    PIC X(04).
                   07  WRK-GMN-TRSI    PIC X(04).
                   07  WRK-GMN-TJBI    PIC X(04).
                   07  WRK-GMN-TKGI    PIC X(04).
                   07  WRK-GMN-TDAISAN PIC X(04).
      * 
           03  WRK-NAI-TBL.
               05  WRK-NAI-TBL-G           OCCURS   2000.
                   07  WRK-NAI-TPTID       PIC 9(10).
                   07  WRK-NAI-TSRYYM      PIC 9(06).
                   07  WRK-NAI-TTAISYO     PIC 9(01).
                   07  WRK-NAI-TDELETE     PIC 9(01).
                   07  WRK-NAI-TCHK        PIC 9(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MOTOPG          PIC X(08).
           03  WRK-SELNO           PIC 9(03).
           03  WRK-NUM             PIC ZZZ9.
           03  WRK-GMN-CUR         PIC 9(02).
           03  WRK-RECEERR         PIC X(200).
           03  WRK-RIDMSG          PIC X(200).
           03  WRK-YM              PIC X(07). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SHELL-ARG1.
             05  WRK-SHELL-SRYYM   PIC X(06).
             05  WRK-SHELL-TERMID  PIC X(16).
             05  WRK-SHELL-SYSYMD  PIC X(08).  
           03  WRK-RECEKBN         PIC X(20).
           03  WRK-NYUGAIKBN       PIC X(10).
           03  WRK-PRTKBN          PIC X(52).
           03  WRK-ERRCD           PIC X(04).
      *
           03  WRK-KBNCD-X.
               05  WRK-KBNCD-9     PIC 9(04).
      * 
           03  WRK-PREFNUM         PIC 9(02).
      *
           03  WRK-R01-ROW         PIC S9(09).
      *
           03  WRK-HENFLG          PIC 9(01).
      *
           03  WRK-SYORI           PIC X(01).
      *
           03  WRK-KENSU           PIC 9(05).
           03  WRK-KENSUZ          PIC ZZZZ9.
           03  WRK-KENSU-X         PIC X(10).
      *
           03  WRK-BTPARA-KENSU.
               05  WRK-BTPARA-KENSU1   PIC ZZZ9.
               05  WRK-BTPARA-FL       PIC X(01).
               05  WRK-BTPARA-KENSU2   PIC ZZZ9.
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
           03  WRK-MCP-SRYACCT-PATHNAME
                                   PIC X(64).
      *
           03  WRK-SRYYM           PIC 9(06).
      *
           03  WRK-ACCT-SRYYM      PIC 9(06).
           03  WRK-ACCT-PTID       PIC 9(10).
           03  WRK-ACCT-NYUGAIKBN  PIC X(01).
      *
           03  WRK-FILEMEI         PIC X(20).
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
      *    スパ領域（ワーク）
           COPY    "COMMON-SPA"    REPLACING  //SPA-//
                                   BY         //WRK-SPA-//.
      *                             
       01  WRK-SHELL-NAME.
      **     05  FILLER             PIC X(16) VALUE
      **                                       "scripts/monthly/".
           05  WRK-SHELL-SHELL    PIC X(20).         
      *
       01  WRK-CONS-AREA.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "RECEPT1".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   1.
           03  WRK-CONS-JOB-SHELLID1
                                   PIC X(08)   VALUE   "TEIKI1".
           03  WRK-CONS-JOB-JOBID1 PIC 9(07)   VALUE   1.
           03  WRK-CONS-BTPARA-SHELLID
                                   PIC X(08)   VALUE   "ORCBQ01".
           03  WRK-CONS-BTPARA-SCRIPTID
                                   PIC X(20)   VALUE   "ORCBQ02.sh".
      *
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM
                                   PIC X(03)   VALUE   "971". 
      *
      *    自賠責保険番号 
           03  WRK-CONS-JIBAISEKI-HKNNUM
                                   PIC X(03)   VALUE   "973". 
      *
      *    公害保険番号 
           03  WRK-CONS-KOUGAI-HKNNUM
                                   PIC X(03)   VALUE   "975". 
      *
      *    第三者行為
           03  WRK-CONS-DAISANSYA-HKNNUM
                                   PIC X(03)   VALUE   "970". 
      *
           03  WRK-CONS-LINE-MAX   PIC 9(04)   VALUE   2000.
      *
      *    エラーファイルＩＤ
           03  WRK-CONS-FILE-ID    PIC X(08)   VALUE   "OR042ERR".
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "CPQTORMAIN.INC".
      *
           COPY    "CPQTORSUB.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK2001.INC".
      *
           COPY    "CPSK2002.INC".
      *
           COPY    "CPSK2003.INC".
      *
           COPY    "CPSK2005.INC".
           COPY    "CPSK200501.INC"
                                   REPLACING  //SYS-200501//
                                   BY         //SYS-25//.
      *
           COPY    "CPSK4001.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    レセプト管理     
       01  RECE07-REC. 
           COPY    "CPRCF007.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *    定期請求ジョブチェックサブ
           COPY    "CPORCSTEIKIJOB.INC".
      *
      *    主科
       01  SYUKA-REC.
           COPY    "CPSYUKA.INC".
      *
           COPY    "CPRCF010.INC".
      *
           COPY    "CPKOHSKY.INC".
      *
      *    レセプト電算
           COPY    "CPRECEDEN.INC".
      *
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
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
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    保険分類サブ
           COPY    "CPORCSS001.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *     
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPDATA2.INC".
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA42SCRAREA.INC".
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
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
      *****MOVE    SPA-FREE        TO  SPA-R01FREE
           MOVE    SPA-WORK        TO  SPA-R01KYOUTU
      *
      *    画面ＳＰＡ読込み
           PERFORM 000-R01SPA-SET-SEC
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
      *    画面ＳＰＡ書込み
           IF      FLG-END2            =   ZERO
               PERFORM 000-R01SPA-OUT-SEC
           END-IF
      *
           MOVE    SPA-R01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
      *****MOVE    SPA-R01FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    画面ＳＰＡ読込
      *****************************************************************
       000-R01SPA-SET-SEC             SECTION.
      *
           INITIALIZE                      SPA-R01-AREA
      *
           MOVE    "SPAGMN"            TO  WRK-FILEMEI
           PERFORM 000-SPATMP-SEL-SEC
      *
           MOVE    "SPANAI"            TO  WRK-FILEMEI
           PERFORM 000-SPATMP-SEL-SEC
           .
       000-R01SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ読込
      *****************************************************************
       000-SPATMP-SEL-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-SPASCR
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "R01"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    WRK-FILEMEI         TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
      *
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-SPATMP-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-SPATMP-READ-SEC
               IF      FLG-SPATMP          =   ZERO
                   IF      WRK-FILEMEI         =   "SPAGMN"
                       MOVE    SPATMP-DATA-REC     TO   SPA-R01-GMN
                   ELSE
                       MOVE    SPATMP-DATA-REC     TO   SPA-R01-NAI
                   END-IF
               END-IF
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
      *
           MOVE    "key"              TO  MCP-PATHNAME
           PERFORM 900-SPATMP-DBCLOSE-SEC
           .
       000-SPATMP-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ書込
      *****************************************************************
       000-R01SPA-OUT-SEC             SECTION.
      *
      *    一時保存テーブル削除処理
           PERFORM 900-SPATMP-DBDELETE-SEC
      *
           MOVE    "SPAGMN"            TO  WRK-FILEMEI
           PERFORM 000-SPATMP-OUT-SEC
      *
           MOVE    "SPANAI"            TO  WRK-FILEMEI
           PERFORM 000-SPATMP-OUT-SEC
      *
           .
       000-R01SPA-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ書込
      *****************************************************************
       000-SPATMP-OUT-SEC                SECTION.
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "R01"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    WRK-FILEMEI         TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           IF      WRK-FILEMEI         =   "SPAGMN"
               MOVE    SPA-R01-GMN         TO  SPATMP-DATA-REC
           ELSE
               MOVE    SPA-R01-NAI         TO  SPATMP-DATA-REC
           END-IF
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-SPATMP-CALL-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "R01 SPATMP INSERT ERR:"  MCP-RC
                           ",KEY:" SPATMP-KEY
               MOVE    "9999"          TO  SPA-ERRCD
           END-IF
           .
       000-SPATMP-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "RERR"
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
               IF      SPA-RIDCD       NOT =   SPACE
                   PERFORM 520-RIDSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
               IF      SPA-RIDCD4      NOT =   SPACE
                   PERFORM 540-RIDSET4-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "R01    "           TO  MCP-WINDOW
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
      *        画面ＳＰＡ読込み
               PERFORM 000-R01SPA-SET-SEC
      *
      *        元の画面へ戻す
               IF      SPA-MOTOPG      =   "XB01"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
      *****        MOVE    LNK-SCRSPA      TO  SPA-R01FREE
                   MOVE    SPA-WORK        TO  SPA-R01KYOUTU
                   GO  TO  300-SCREEN-EXT
               END-IF
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
               IF      WRK-MOTOPG(1:3)     =  "P97"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
      *****        MOVE    LNK-SCRSPA      TO  SPA-R01FREE
                   MOVE    SPA-WORK        TO  SPA-R01KYOUTU
                   PERFORM 3101-U01SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
      *         
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *    作表処理
               WHEN    "RID1"
                   PERFORM 330-RID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "R99"
                   PERFORM 340-RID2-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "R96"
                   PERFORM 340-RID2-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "RID4"
                   PERFORM 350-RID4-SET-SEC
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
      *    ＬＤ外の項目入力編集処理
      *****************************************************************
       3101-U01SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *    受付、患者検索より
           IF      WRK-SPA-PTNUM   NOT =   SPACE
                   MOVE    WRK-SPA-PTNUM   TO  SPA-GMN-R01-PTNUM
               PERFORM 4103-PTNUM-CHK-SEC
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-R01-CUR
           END-IF
           .
       3101-U01SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-R01-AREA
      *
      *    点検用レセプト作成時
           EVALUATE    SPA-R01-PRTKBN
               WHEN    "2"
                   MOVE    "【点検用】"    TO  WRK-PRTKBN
               WHEN    "3"
                   MOVE    "【点検用（０点公費記載設定）】"
                                           TO  WRK-PRTKBN
               WHEN    "4"
                   MOVE    "【点検用（包括診療分・持参薬記載設定）】"
                                           TO  WRK-PRTKBN
               WHEN    "5"
                   MOVE
                   "【点検用（０点公費・包括診療分・持参薬記載設定）】"
                                           TO  WRK-PRTKBN
               WHEN    "8"
                   MOVE    "【ＥＦファイル】"  TO  WRK-PRTKBN
               WHEN    "9"
                   MOVE    "【連係基盤用】"    TO  WRK-PRTKBN
               WHEN    OTHER
                   MOVE    SPACE           TO  WRK-PRTKBN
           END-EVALUATE
      *
           EVALUATE    SPA-R01-RECEKBN
               WHEN    "0"
                   MOVE    "医　保"    TO  WRK-RECEKBN
               WHEN    "3"
                   MOVE    "労　災"    TO  WRK-RECEKBN
               WHEN    "4"
                   MOVE    "自賠責（新様式）"
                                       TO  WRK-RECEKBN
               WHEN    "5"
                   MOVE    "自賠責（従来様式）" 
                                       TO  WRK-RECEKBN
               WHEN    "6"
                   MOVE    "公　害"    TO  WRK-RECEKBN
               WHEN    "8"
                   MOVE    "自賠責（第三者行為）" 
                                       TO  WRK-RECEKBN
           END-EVALUATE
           EVALUATE    SPA-R01-NYUGAIKBN
               WHEN    "1"
                   MOVE    "（入院）"        TO  WRK-NYUGAIKBN
               WHEN    "2"
                   MOVE    "（入院外）"      TO  WRK-NYUGAIKBN
           END-EVALUATE  
           STRING  WRK-PRTKBN         DELIMITED  BY  SPACE 
                   WRK-RECEKBN        DELIMITED  BY  SPACE
                   WRK-NYUGAIKBN      DELIMITED  BY  SPACE
                   INTO    SPA-GMN-R01-SYOKBN
           END-STRING
      *             
           IF      SPA-SYSYMD(7:2)     >   "10"
               MOVE    SPA-SYSYMD(1:6)     TO  SPA-NAI-R01-HYO-SRYYM
               MOVE    SPA-SYSYMDWH(1:6)   TO  SPA-NAI-R01-HYO-SRYYMH
           ELSE
               INITIALIZE                     STS-AREA-DAY
               INITIALIZE                     LNK-DAY6-AREA
               MOVE  "61"                     TO  LNK-DAY6-IRAI
               MOVE  SPA-SYSYMD               TO  LNK-DAY6-KIJUN
               MOVE  "2"                      TO  LNK-DAY6-ZOGENPTN
               MOVE  -1                       TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"                USING   STS-AREA-DAY
                                              LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN(1:6)   TO  SPA-NAI-R01-HYO-SRYYM
               MOVE    LNK-DAY6-KEISAN        TO  WRK-SYMD  
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG(1:6)       TO  SPA-NAI-R01-HYO-SRYYMH
           END-IF
      *
           IF      SPA-R01-PRTKBN  =   "8" OR  "9"
               MOVE    "1"             TO  SPA-GMN-R01-SYORIL    (1)
               MOVE    "個　別"        TO  SPA-GMN-R01-SYORIMEIL (1)
               MOVE    1               TO  SPA-GMN-R01-SYORILST-MAX
           ELSE
             EVALUATE    SPA-R01-NYUGAIKBN
               WHEN    "1"
                   MOVE    "1"             TO  SPA-GMN-R01-SYORIL    (1)
                   MOVE    "個　別"        TO  SPA-GMN-R01-SYORIMEIL (1)
                   MOVE    "4"             TO  SPA-GMN-R01-SYORIL    (2)
                   MOVE    "照会連携"      TO  SPA-GMN-R01-SYORIMEIL (2)
                   MOVE    2               TO  SPA-GMN-R01-SYORILST-MAX
               WHEN    "2"
                   MOVE    "1"             TO  SPA-GMN-R01-SYORIL    (1)
                   MOVE    "個　別"        TO  SPA-GMN-R01-SYORIMEIL (1)
                   MOVE    "2"             TO  SPA-GMN-R01-SYORIL    (2)
                   MOVE    "訂正日"        TO  SPA-GMN-R01-SYORIMEIL (2)
                   MOVE    "3"             TO  SPA-GMN-R01-SYORIL    (3)
                   MOVE    "受診日"        TO  SPA-GMN-R01-SYORIMEIL (3)
                   MOVE    "4"             TO  SPA-GMN-R01-SYORIL    (4)
                   MOVE    "照会連携"      TO  SPA-GMN-R01-SYORIMEIL (4)
                   MOVE    4               TO  SPA-GMN-R01-SYORILST-MAX
             END-EVALUATE
             EVALUATE    SPA-R01-RECEKBN
               WHEN    "0"
                   ADD     1               TO  SPA-GMN-R01-SYORILST-MAX
                   MOVE    "5"             TO  SPA-GMN-R01-SYORIL
                                              (SPA-GMN-R01-SYORILST-MAX)
                   MOVE    "未請求解除"    TO  SPA-GMN-R01-SYORIMEIL
                                              (SPA-GMN-R01-SYORILST-MAX)
             END-EVALUATE
          END-IF
      *
      *    パラメタＤＢ読み込み
           INITIALIZE                      BTPARA-REC
grpsys     MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  BTPARA-SHELLID
           MOVE    WRK-CONS-JOB-JOBID  TO  BTPARA-JOBID
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-BTPARA-READ-SEC
           EVALUATE    SPA-R01-NYUGAIKBN
               WHEN    "1"
                   EVALUATE    BTPARA-INFO-KBN
                       WHEN    "05"
                           MOVE    SPA-GMN-R01-SYORILST (3)
                                               TO  SPA-GMN-R01-SYORI-G
                       WHEN    "04"
                           MOVE    SPA-GMN-R01-SYORILST (2)
                                               TO  SPA-GMN-R01-SYORI-G
                       WHEN    OTHER
                           MOVE    SPA-GMN-R01-SYORILST (1)
                                               TO  SPA-GMN-R01-SYORI-G
                   END-EVALUATE
               WHEN    "2"
                   EVALUATE    BTPARA-INFO-KBN
                       WHEN    "05"
                           MOVE    SPA-GMN-R01-SYORILST (5)
                                               TO  SPA-GMN-R01-SYORI-G
                       WHEN    "04"
                           MOVE    SPA-GMN-R01-SYORILST (4)
                                               TO  SPA-GMN-R01-SYORI-G
                       WHEN    "03"
                           MOVE    SPA-GMN-R01-SYORILST (3)
                                               TO  SPA-GMN-R01-SYORI-G
                       WHEN    "02"
                           MOVE    SPA-GMN-R01-SYORILST (2)
                                               TO  SPA-GMN-R01-SYORI-G
                       WHEN    OTHER
                           MOVE    SPA-GMN-R01-SYORILST (1)
                                               TO  SPA-GMN-R01-SYORI-G
                   END-EVALUATE
           END-EVALUATE
           MOVE    SPA-GMN-R01-SYORI   TO  SPA-GMN-R01-SYORI-OLD
      *
           EVALUATE    SPA-GMN-R01-SYORI
               WHEN    "2"    
                   MOVE    "訂正日"        TO  SPA-GMN-R01-LABEL
                   MOVE    "対象診療年月"  TO  SPA-GMN-R01-LABEL1
                   MOVE    "一括作成年月日"
                                           TO  SPA-GMN-R01-LABEL2
               WHEN    "3"    
                   MOVE    "受診日"        TO  SPA-GMN-R01-LABEL
           END-EVALUATE
      *
           IF      SPA-GMN-R01-SYORI   =   "2"
               MOVE    SPACE           TO  SPA-GMN-R01-TEISEIYM
               PERFORM 4110-TEISEIYM-SEC
               IF      SPA-ERRCD       =   SPACE
                   MOVE    8               TO  SPA-GMN-R01-CUR
               ELSE
                   MOVE    SPACE           TO  SPA-ERRCD
                   MOVE    8               TO  SPA-GMN-R01-CUR
               END-IF
           ELSE
               EVALUATE    SPA-R01-NYUGAIKBN
                   WHEN    "1"
                       MOVE    8               TO  SPA-GMN-R01-CUR
                   WHEN    "2"
                       MOVE    8               TO  SPA-GMN-R01-CUR
               END-EVALUATE
           END-IF    
      *                 
           MOVE    1               TO  SPA-GMN-R01-PAGE
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面１戻り処理
      *****************************************************************
       330-RID1-SET-SEC            SECTION.
      *
           IF      SPA-RID1-FLG    =   "OK"
               EVALUATE    SPA-RIDCD
                   WHEN    "6666"
                       MOVE    "JOIN"          TO  MCP-PUTTYPE
                       PERFORM 210-BACK-SYORI-SEC
                   WHEN    "1001"
      *                未請求解除データが診療なしのみの場合の更新処理
                       PERFORM 4502-KAIJYO-DATA-KOUSIN-SEC
                       IF      SPA-ERRCD       =   SPACE
                           PERFORM 41072-MISEIKYU-DATA-HENSYU-SEC
                           MOVE    SPACE               TO  SPA-ERRCD
                       END-IF
                   WHEN    "1002"
                   WHEN    "1006"
                   WHEN    "1007"
      *                ジョブ管理チェック処理
                       PERFORM 4501-JOBKANRI-CHK-SEC
                       IF      SPA-ERRCD      =   SPACE
                           PERFORM 450-KOUSHIN-SEC
                       END-IF
                   WHEN    "1003"
                       PERFORM 460-CLEAR-SEC
                   WHEN    "1005"
                       PERFORM 460-CLEAR-SEC
                       MOVE    SPA-GMN-R01-SYORI-G
                                           TO  SPA-GMN-R01-SYORI-OLD-G
                       EVALUATE    SPA-GMN-R01-SYORI
                           WHEN    "4"
                               PERFORM 41071-RENKEI-DATA-HENSYU-SEC
                           WHEN    "5"
                               PERFORM 41072-MISEIKYU-DATA-HENSYU-SEC
                       END-EVALUATE
                   WHEN    "1008"
                   WHEN    "1009"
                   WHEN    "1010"
                       MOVE    1               TO  SPA-NAI-R01-DELETE
      *
                       MOVE    SPA-NAI-R01-SRYYM
                                               TO  WRK-ACCT-SRYYM
                       MOVE    SPA-GMN-R01-NYUGAIKBN
                                               TO  WRK-ACCT-NYUGAIKBN
                       MOVE    SPA-NAI-R01-PTID
                                               TO  WRK-ACCT-PTID
                       PERFORM 480-DBDELETE-SEC
                       IF      SPA-ERRCD   NOT =   SPACE
                           GO  TO  330-RID1-SET-EXT
                       END-IF 
                   WHEN    "1011"
                   WHEN    "1012"
                   WHEN    "1013"
                       MOVE    SPA-NAI-R01-2002-TPTID 
                                                (SPA-NAI-R01-2002-INDEX)
                                               TO  WRK-ACCT-PTID
                       MOVE    SPA-NAI-R01-2002-TSRYYM
                                                (SPA-NAI-R01-2002-INDEX)
                                               TO  WRK-ACCT-SRYYM
                       MOVE    SPA-R01-NYUGAIKBN
                                               TO  WRK-ACCT-NYUGAIKBN
                       PERFORM 4801-DBDELETE-SYORI-SEC
                       MOVE    1               TO
                                            SPA-NAI-R01-2002-TDELETE
                                                (SPA-NAI-R01-2002-INDEX)
                       IF      SPA-ERRCD   NOT =   SPACE
                           GO  TO  330-RID1-SET-EXT
                       END-IF 
                   WHEN    OTHER
                       PERFORM 480-ZENKAI-HYOUJI-SEC
                       INITIALIZE                  SPA-RID1-AREA
                       IF      SPA-RIDCD   NOT =   SPACE
                           GO  TO  330-RID1-SET-EXT
                       END-IF 
               END-EVALUATE
           ELSE
               EVALUATE    SPA-RIDCD
                   WHEN    "1005"
                       MOVE    SPA-GMN-R01-SYORI-OLD-G
                                           TO  SPA-GMN-R01-SYORI-G
                       MOVE    8           TO  SPA-GMN-R01-CUR
                   WHEN    "1011"
                   WHEN    "1012"
                   WHEN    "1013"
                       MOVE    1        TO  SPA-NAI-R01-2002-TTAISYO
                                                (SPA-NAI-R01-2002-INDEX)
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-RIDCD
               WHEN    "1011"
               WHEN    "1012"
               WHEN    "1013"
                   INITIALIZE                  SPA-RID1-AREA
                                               SPA-RIDCD-AREA
                   PERFORM UNTIL   SPA-NAI-R01-2002-INDEX
                                           >=  SPA-NAI-R01-2002-MAX
                           OR      SPA-RIDCD   NOT =   SPACE
                       PERFORM 4801-ZENKAI-SYSKANRI-CHECK-SEC
                   END-PERFORM    
      *
                   IF      SPA-RIDCD       =   SPACE
      *                前回データ対象編集処理
                       PERFORM 4802-ZENKAI-SYSKANRI-HEN-SEC   
                   END-IF
               WHEN    OTHER
                   INITIALIZE                  SPA-RID1-AREA
                                               SPA-RIDCD-AREA
           END-EVALUATE
      *
           MOVE    ZERO           TO  SPA-NAI-R01-PRTKBN-FLG  
      *
           .
       330-RID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＲＩＤ２）戻り処理
      *****************************************************************
       340-RID2-SET-SEC            SECTION.
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "R99"
                   IF      SPA-R99-FLG     =   "OK"
                       PERFORM 4501-NEXT-GMN-SEC
                   ELSE
                       IF      SPA-GMN-R01-SYORI   =   "5"
                           PERFORM 41072-MISEIKYU-DATA-HENSYU-SEC
                           MOVE    SPACE               TO  SPA-ERRCD
                       END-IF    
                   END-IF    
               WHEN    "R96"
                   IF      SPA-GMN-R01-SYORI   =   "5"
                       PERFORM 41072-MISEIKYU-DATA-HENSYU-SEC
                       MOVE    SPACE               TO  SPA-ERRCD
                   END-IF    
           END-EVALUATE     
      *
           INITIALIZE                  SPA-RID1-AREA
                                       SPA-RIDCD-AREA
           MOVE    ZERO            TO  SPA-R01-JOBCNT       
                                       SPA-R01-STOPFLG    
           .
       340-RID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＲＩＤ４）戻り処理
      *****************************************************************
       350-RID4-SET-SEC            SECTION.
      *
           IF      SPA-RID4-FLG    =   "CHG"   OR  "OK"
               EVALUATE    SPA-RIDCD4
                   WHEN    "1002"
                   WHEN    "1006"
                   WHEN    "1007"
      *                ジョブ管理チェック処理
                       PERFORM 4501-JOBKANRI-CHK-SEC
                       IF      SPA-ERRCD      =   SPACE
                           PERFORM 450-KOUSHIN-SEC
                       END-IF
               END-EVALUATE
           END-IF
      *
           INITIALIZE                  SPA-RID1-AREA
                                       SPA-RIDCD-AREA
      *
           MOVE    ZERO           TO  SPA-NAI-R01-PRTKBN-FLG  
      *
           .
       350-RID4-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    "CHANGE"            TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 460-CLEAR-RID-SEC
      *    削除
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 440-SAKUJYO-SEC
      *    照会連携表示
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 480-RENKEI-HYOUJI-SEC
      *    前回作成分表示
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 480-ZENKAI-HYOUJI-RID-SEC
      *    氏名検索
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 480-KENSAKU-SEC
      *     情報削除
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-JYOUHOU-DEL-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-TOUROKU-SEC
      *    処理確認
               WHEN    "CLICKED"       ALSO    "B13"
                   PERFORM 490-KAKUNIN-SEC
      *    未請求患者設定
               WHEN    "CLICKED"       ALSO    "B22"
                   PERFORM 490-MISEIKYU-ENTRY-SEC
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
               WHEN    "ACTIVATE"      ALSO    "RENNUM"
                   PERFORM 4102-RENNUM-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SYORI"
                   PERFORM 4107-SYORI-SEC
               WHEN    "ACTIVATE"       ALSO    ANY
                   PERFORM 400-ENTRY-SYORI-SEC
               WHEN    ANY             ALSO    "RECELST"
                   PERFORM 4106-RECESEL-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択　入力処理
      *****************************************************************
       4102-RENNUM-CHK-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-R01-KOUMOKU
                                       SPA-NAI-R01-KOUMOKU
           MOVE    ZERO                TO  SPA-NAI-R01-SELNO
      *
           MOVE    3                   TO  SPA-GMN-R01-CUR
      *
           MOVE    R01-RENNUM          TO  SPA-GMN-R01-RENNUM
      *
           IF      SPA-GMN-R01-RENNUM  =   ZERO
               IF      SPA-GMN-R01-MAX     =   WRK-CONS-LINE-MAX
                   MOVE    "0002"              TO  SPA-ERRCD
               ELSE
                   MOVE    4                   TO  SPA-GMN-R01-CUR 
               END-IF
               GO  TO  4102-RENNUM-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-R01-RENNUM  >   SPA-GMN-R01-MAX
               MOVE    "0023"          TO  SPA-ERRCD
               GO      TO      4102-RENNUM-CHK-EXT
           END-IF
      *
           IF      SPA-NAI-R01-TDELETE (SPA-GMN-R01-RENNUM)
                                       =   1
               MOVE    "0037"          TO  SPA-ERRCD
               GO      TO      4102-RENNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-R01-RENNUM  TO  SPA-NAI-R01-SELNO
      *
           MOVE    SPA-GMN-R01-RENNUM  TO  R01-ROW
      *
           PERFORM 41021-RENNUM-HENSYU-SEC
      *
           .
       4102-RENNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択項目編集処理
      *****************************************************************
       41021-RENNUM-HENSYU-SEC            SECTION.
      *
                
           MOVE    SPA-GMN-R01-TPTNUM     (SPA-GMN-R01-RENNUM)
                                       TO  SPA-GMN-R01-PTNUM
           MOVE    SPA-NAI-R01-TPTID      (SPA-GMN-R01-RENNUM)
                                       TO  SPA-NAI-R01-PTID
           MOVE    SPA-GMN-R01-TNAME      (SPA-GMN-R01-RENNUM)
                                       TO  SPA-GMN-R01-NAME
           MOVE    SPA-GMN-R01-TSRYYM     (SPA-GMN-R01-RENNUM)
                                       TO  SPA-GMN-R01-SRYYM
           MOVE    SPA-NAI-R01-TSRYYM     (SPA-GMN-R01-RENNUM)
                                       TO  SPA-NAI-R01-SRYYM
           MOVE    SPA-GMN-R01-TNYUGAIKBN (SPA-GMN-R01-RENNUM)
                                       TO  SPA-GMN-R01-NYUGAIKBN
           MOVE    SPA-NAI-R01-TCHK       (SPA-GMN-R01-RENNUM)
                                       TO  SPA-NAI-R01-CHK
      *
           MOVE    4                   TO  SPA-GMN-R01-CUR 
      *
           .
       41021-RENNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       4106-RECESEL-SEC       SECTION.
      *
           IF      SPA-GMN-R01-SYORI       =   "1"
      *        行選択
               MOVE    ZERO                TO  WRK-SELNO
      *     
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >       WRK-CONS-LINE-MAX
                   IF      R01-SELECT (IDX)    =   "T"
                       IF      IDX             =   SPA-NAI-R01-SELNO
                           CONTINUE
                       ELSE        
                           MOVE    IDX             TO  WRK-SELNO
                           MOVE    WRK-CONS-LINE-MAX
                                                   TO  IDX
                       END-IF
                   END-IF    
               END-PERFORM
      *
               IF      WRK-SELNO           >   ZERO
                   MOVE    WRK-SELNO           TO  SPA-NAI-R01-SELNO
               END-IF
      *
               IF      SPA-NAI-R01-TDELETE (SPA-NAI-R01-SELNO)
                                       =   1
                   MOVE    "0037"          TO  SPA-ERRCD
                   INITIALIZE                  SPA-GMN-R01-KOUMOKU
                                               SPA-NAI-R01-KOUMOKU
                   MOVE    ZERO            TO  SPA-NAI-R01-SELNO                           
                   MOVE    3               TO  SPA-GMN-R01-CUR
                   GO      TO      4106-RECESEL-EXT
               END-IF
      *
               INITIALIZE                  SPA-GMN-R01-KOUMOKU
               INITIALIZE                  SPA-NAI-R01-KOUMOKU
      *
               MOVE    SPA-GMN-R01-TRENNUM (SPA-NAI-R01-SELNO)
                                       TO  SPA-GMN-R01-RENNUM
      *
               PERFORM 41021-RENNUM-HENSYU-SEC
           ELSE
               MOVE    "0020"              TO  SPA-ERRCD
           END-IF
           .
       4106-RECESEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理区分入力処理
      *****************************************************************
       4107-SYORI-SEC                 SECTION.
      *
           MOVE    8                  TO  SPA-GMN-R01-CUR
      * 
           MOVE    R01-SYORI          TO  SPA-GMN-R01-SYORI-G
      *
           IF      SPA-GMN-R01-SYORIMEI   =   SPACE
               MOVE    "0017"              TO  SPA-ERRCD
           ELSE    
               MOVE    ZERO                 TO  WRK-HENFLG
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-R01-SYORILST-MAX
                   IF      SPA-GMN-R01-SYORI
                                           =   SPA-GMN-R01-SYORIL (IDX)
                       MOVE    SPA-GMN-R01-SYORILST (IDX)
                                               TO  SPA-GMN-R01-SYORI-G 
                       MOVE    1               TO  WRK-HENFLG
                       MOVE    SPA-GMN-R01-SYORILST-MAX
                                               TO  IDX
                   END-IF
               END-PERFORM
               IF      WRK-HENFLG          =   ZERO     
                   MOVE    "0017"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF    ( SPA-GMN-R01-SYORI-OLD
                                       NOT =   SPA-GMN-R01-SYORI )
               AND   ( SPA-GMN-R01-SYORI-OLD
                                       NOT =   SPACE             )
               AND   ( SPA-GMN-R01-MAX     >   ZERO              )
                   MOVE    "1005"              TO  SPA-RIDCD
               ELSE
                   MOVE    SPA-GMN-R01-SYORI-G TO
                                               SPA-GMN-R01-SYORI-OLD-G
                   PERFORM 460-CLEAR-SEC
                   EVALUATE    SPA-GMN-R01-SYORI
                       WHEN    "4"
                           PERFORM 41071-RENKEI-DATA-HENSYU-SEC
                       WHEN    "5"
                           PERFORM 41072-MISEIKYU-DATA-HENSYU-SEC
                   END-EVALUATE
               END-IF
           ELSE
               MOVE   SPACE            TO  SPA-GMN-R01-SYORI-OLD-G
           END-IF  
           .
       4107-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    照会連携データ編集処理
      *****************************************************************
       41071-RENKEI-DATA-HENSYU-SEC       SECTION.
      *
           INITIALIZE                      SPA-NAI-R01-BTPARA-AREA
                                           SPA-GMN-R01-BTPARA-AREA
           MOVE    ZERO                TO  SPA-NAI-R01-BTPARA-MAX
                                           SPA-NAI-R01-BTPARA-TAISYO-MAX
      *
           INITIALIZE                      BTPARA-REC
           MOVE    WRK-CONS-BTPARA-SHELLID
                                       TO  BTPARA-SHELLID
           MOVE    WRK-CONS-BTPARA-SCRIPTID
                                       TO  BTPARA-SCRIPTID
grpsys     MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           MOVE    BTPARA-REC          TO  MCPDATA-REC
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-BTPARA-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM                     UNTIL   FLG-BTPARA  =   1
              EVALUATE    BTPARA-DSP-RENNUM
                                   ALSO    BTPARA-RENNUM
                   WHEN    1       ALSO    1
                       MOVE    BTPARA-INFO-PARA
                                           TO  QTORMAIN-AREA
                       MOVE    QTORMAIN-AREA
                                           TO
                                       SPA-NAI-R01-BTPARA-QTORMAIN-AR
                       MOVE    QTORM-SRYYM TO  WRK-SYMD (1:6)
                       MOVE    "01"        TO  WRK-SYMD (7:2) 
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    WRK-HENYMDG TO  SPA-NAI-R01-BTPARA-SRYYMH
                       STRING  WRK-HENYMDG3 (1:16) DELIMITED  BY  SIZE
                               "診療分"            DELIMITED  BY  SIZE
                                       INTO    SPA-GMN-R01-BTPARA-SRYYMH
                       END-STRING
                       MOVE    QTORM-CREYMD
                                           TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    WRK-HENYMDG TO  SPA-GMN-R01-BTPARA-CREYMD
                       EVALUATE    QTORM-NYUGAIKBN
                           WHEN    SPACE
                               MOVE    "入院・入院外"
                                       TO  SPA-GMN-R01-BTPARA-NYUGAIKBNH
                           WHEN    "1"
                               MOVE    "入院"
                                       TO  SPA-GMN-R01-BTPARA-NYUGAIKBNH
                           WHEN    "2"
                               MOVE    "入院外"
                                       TO  SPA-GMN-R01-BTPARA-NYUGAIKBNH
                       END-EVALUATE
                   WHEN    1       ALSO    2
                       MOVE    BTPARA-INFO-PARA
                                       TO  SPA-GMN-R01-BTPARA-HYODAI
                   WHEN    1       ALSO    3
                       MOVE    BTPARA-INFO-PARA
                                       TO  SPA-GMN-R01-BTPARA-JYOKEN1
                   WHEN    1       ALSO    4
                       MOVE    BTPARA-INFO-PARA
                                       TO  SPA-GMN-R01-BTPARA-JYOKEN2
                   WHEN    2       ALSO    ANY
                       ADD     1           TO  IDX
                       MOVE    BTPARA-INFO-PARA
                                           TO  QTORSUB-AREA
                       MOVE    QTORSUB-AREA
                                           TO
                                   SPA-NAI-R01-BTPARA-TQTORSUB-AR(IDX)
                       INITIALIZE                  ORCSPTIDAREA
grpsys                 MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
                       MOVE    QTORS-PTID      TO  SPTID-PTID
                       CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                                       SPA-AREA
                       IF      SPTID-RC        =   ZERO
                           MOVE    SPTID-PTNUM TO
                                       SPA-NAI-R01-BTPARA-TPTNUM (IDX)
                       END-IF
      *
grpsys                 MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
                       MOVE    QTORS-PTID      TO  PTINF-PTID
                       PERFORM 900-PTINF-READ-SEC
                       MOVE    PTINF-NAME      TO
                                           SPA-NAI-R01-BTPARA-TNAME(IDX)
      *
                       MOVE    ZERO            TO  FLG-CHK
                       EVALUATE    SPA-R01-NYUGAIKBN
                                       ALSO    SPA-R01-RECEKBN
                           WHEN    "1" ALSO    "0"
                               IF      QTORS-NIHO-FLG      =   1
                                   MOVE    1       TO
                                       SPA-NAI-R01-BTPARA-TTAISYO (IDX)
                               END-IF          
                           WHEN    "1" ALSO    "3"     
                               IF      QTORS-NRSI-FLG      =   1
                                   MOVE    1       TO
                                       SPA-NAI-R01-BTPARA-TTAISYO (IDX)
                               END-IF          
                           WHEN    "1" ALSO    "4"
                               IF      SPA-NAI-R01-BTPARA-SRYYMH
                                                           >=  "201707"
      *                            自賠責（新様式）のとき健保準拠かチェック
                                   IF      QTORS-NJBIR-FLG  =   1
                                       MOVE    1            TO
                                          SPA-NAI-R01-BTPARA-TTAISYO
                                                                   (IDX)
                                   END-IF
                               ELSE
                                   IF      QTORS-NJBI-FLG      =   1
                                       MOVE    1       TO
                                           SPA-NAI-R01-BTPARA-TTAISYO 
                                                                   (IDX)
                                   END-IF
                               END-IF
                           WHEN    "1" ALSO    "5"
                               IF      QTORS-NJBI-FLG      =   1
                                   MOVE    1       TO
                                       SPA-NAI-R01-BTPARA-TTAISYO (IDX)
                               END-IF          
                           WHEN    "1" ALSO    "6"
                               IF      QTORS-NKGI-FLG      =   1
                                   MOVE    1       TO
                                       SPA-NAI-R01-BTPARA-TTAISYO (IDX)
                               END-IF
                           WHEN    "1" ALSO    "8"
                               IF      QTORS-NDI3-FLG      =   1
                                   MOVE    1       TO
                                       SPA-NAI-R01-BTPARA-TTAISYO (IDX)
                               END-IF          
                           WHEN    "2" ALSO    "0"
                               IF      QTORS-GIHO-FLG      =   1
                                   MOVE    1       TO
                                       SPA-NAI-R01-BTPARA-TTAISYO (IDX)
                               END-IF          
                           WHEN    "2" ALSO    "3"     
                               IF      QTORS-GRSI-FLG      =   1
                                   MOVE    1       TO
                                       SPA-NAI-R01-BTPARA-TTAISYO (IDX)
                               END-IF          
                           WHEN    "2" ALSO    "4"     
                               IF      SPA-NAI-R01-BTPARA-SRYYMH
                                                           >=  "201707"
      *                            自賠責（新様式）のとき健保準拠かチェック
                                   IF      QTORS-GJBIR-FLG  =   1
                                       MOVE    1            TO
                                          SPA-NAI-R01-BTPARA-TTAISYO
                                                                   (IDX)
                                   END-IF
                               ELSE
                                   IF      QTORS-GJBI-FLG      =   1
                                       MOVE    1       TO
                                           SPA-NAI-R01-BTPARA-TTAISYO
                                                                   (IDX)
                                   END-IF
                               END-IF
                           WHEN    "2" ALSO    "5"
                               IF      QTORS-GJBI-FLG      =   1
                                   MOVE    1       TO
                                       SPA-NAI-R01-BTPARA-TTAISYO (IDX)
                               END-IF          
                           WHEN    "2" ALSO    "6"
                               IF      QTORS-GKGI-FLG      =   1
                                   MOVE    1       TO
                                       SPA-NAI-R01-BTPARA-TTAISYO (IDX)
                               END-IF
                          WHEN    "2" ALSO    "8"
                               IF      QTORS-GDI3-FLG      =   1
                                   MOVE    1       TO
                                        SPA-NAI-R01-BTPARA-TTAISYO (IDX)
                               END-IF          
                       END-EVALUATE     
                       IF      SPA-NAI-R01-BTPARA-TTAISYO (IDX)
                                                       =   1
                           ADD     1       TO
                                       SPA-NAI-R01-BTPARA-TAISYO-MAX
      *
      *                    請求しない設定の存在チェック
                           INITIALIZE                  RECE10-REC
                           MOVE    SPA-HOSPNUM     TO  RECE10-HOSPNUM
                           MOVE    QTORM-SRYYM      TO  RECE10-SRYYM
                           MOVE    SPA-R01-NYUGAIKBN
                                                    TO  RECE10-NYUGAIKBN
                           MOVE    QTORS-PTID       TO  RECE10-PTID
                           PERFORM 900-SEIKYU-SKYKBN-CHECK-SEC
                           IF      FLG-SEIKYU-UMU      =   1
                               MOVE    1                   TO
                                               SPA-NAI-R01-SEIKYU-CHK
                           END-IF
                       END-IF          
               END-EVALUATE
      *
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-BTPARA-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    IDX                 TO  SPA-NAI-R01-BTPARA-MAX
      *
           IF      SPA-NAI-R01-BTPARA-MAX  =   ZERO
               MOVE    "0031"              TO  SPA-ERRCD
               MOVE    8                   TO  SPA-GMN-R01-CUR
           ELSE
      *        照会連携データ表示処理
               MOVE    1                   TO
                                           SPA-NAI-R01-BTPARA-HYOUJI-FLG
               PERFORM 410711-RENKEI-DATA-JYOKEN-SEC
           END-IF 
           .
       41071-RENKEI-DATA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    照会連携データ表示処理
      *****************************************************************
       410711-RENKEI-DATA-JYOKEN-SEC       SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-R01-MAX
                                       SPA-GMN-R01-DELETE-MAX
           INITIALIZE                  SPA-GMN-R01-TBL
                                       SPA-NAI-R01-TBL
           MOVE    ZERO            TO  SPA-NAI-R01-SELNO
      *
      *    レセプト連携データ（主）
           MOVE    SPA-NAI-R01-BTPARA-QTORMAIN-AR
                                           TO  QTORMAIN-AREA
      *
           MOVE    ZERO                TO  IDY
                                           IDZ
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   SPA-NAI-R01-BTPARA-MAX
      *        レセプト連携データ（患者情報）
               MOVE    SPA-NAI-R01-BTPARA-TQTORSUB-AR (IDX)
                                           TO  QTORSUB-AREA
      *
      *        対象データか判定
               IF    ( SPA-NAI-R01-BTPARA-HYOUJI-FLG   =   1 )
               AND   ( SPA-NAI-R01-BTPARA-TTAISYO (IDX)
                                                       =   0 )
                   CONTINUE
               ELSE
                   ADD     1               TO  IDY
                   MOVE    IDY             TO  SPA-GMN-R01-TRENNUM (IDY)
      *
                   MOVE    QTORS-PTID      TO  SPA-NAI-R01-TPTID   (IDY)
                   MOVE    SPA-NAI-R01-BTPARA-TPTNUM (IDX)
                                           TO  SPA-GMN-R01-TPTNUM  (IDY)
                   MOVE    SPA-NAI-R01-BTPARA-TNAME (IDX)
                                           TO  SPA-GMN-R01-TNAME   (IDY)
                   MOVE    QTORM-SRYYM     TO  SPA-NAI-R01-TSRYYM  (IDY)
                   MOVE    SPA-NAI-R01-BTPARA-SRYYMH
                                           TO  SPA-GMN-R01-TSRYYM  (IDY)
                   IF      QTORS-TSTPTNUMKBN   =   "1"
                       MOVE    "テ"        TO
                                           SPA-GMN-R01-TTSTPTNUMKBN(IDY)
                   END-IF
                   IF      QTORS-NIHO-FLG  =   1
                       MOVE    "入"    TO   SPA-GMN-R01-TIHO (IDY) (3:2)
                   ELSE
                       MOVE    "　"    TO   SPA-GMN-R01-TIHO (IDY) (3:2)
                   END-IF          
                   IF      QTORS-NRSI-FLG  =   1
                       MOVE    "入"    TO   SPA-GMN-R01-TRSI (IDY) (3:2)
                   ELSE
                       MOVE    "　"    TO   SPA-GMN-R01-TRSI (IDY) (3:2)
                   END-IF          
                   IF      QTORS-NJBI-FLG  =   1
                       MOVE    "入"    TO   SPA-GMN-R01-TJBI (IDY) (3:2)
                   ELSE
                       MOVE    "　"    TO   SPA-GMN-R01-TJBI (IDY) (3:2)
                   END-IF          
                   IF      QTORS-NKGI-FLG  =   1
                       MOVE    "入"    TO   SPA-GMN-R01-TKGI (IDY) (3:2)
                   ELSE
                       MOVE    "　"    TO   SPA-GMN-R01-TKGI (IDY) (3:2)
                   END-IF          
                   IF      QTORS-NDI3-FLG  =   1
                       MOVE    "入"    TO   SPA-GMN-R01-TDAISAN
                                                             (IDY) (3:2)
                   ELSE
                       MOVE    "　"    TO   SPA-GMN-R01-TDAISAN
                                                             (IDY) (3:2)
                   END-IF          
                   IF      QTORS-GIHO-FLG  =   1
                       MOVE    "外"    TO   SPA-GMN-R01-TIHO (IDY) (1:2)
                   ELSE
                       MOVE    "　"    TO   SPA-GMN-R01-TIHO (IDY) (1:2)
                   END-IF          
                   IF      QTORS-GRSI-FLG  =   1
                       MOVE    "外"    TO   SPA-GMN-R01-TRSI (IDY) (1:2)
                   ELSE
                       MOVE    "　"    TO   SPA-GMN-R01-TRSI (IDY) (1:2)
                   END-IF          
                   IF      QTORS-GJBI-FLG  =   1
                       MOVE    "外"    TO   SPA-GMN-R01-TJBI (IDY) (1:2)
                   ELSE
                       MOVE    "　"    TO   SPA-GMN-R01-TJBI (IDY) (1:2)
                   END-IF          
                   IF      QTORS-GKGI-FLG  =   1
                       MOVE    "外"    TO   SPA-GMN-R01-TKGI (IDY) (1:2)
                   ELSE
                       MOVE    "　"    TO   SPA-GMN-R01-TKGI (IDY) (1:2)
                   END-IF          
                   IF      QTORS-GDI3-FLG  =   1
                       MOVE    "外"    TO   SPA-GMN-R01-TDAISAN
                                                             (IDY) (1:2)
                   ELSE
                       MOVE    "　"    TO   SPA-GMN-R01-TDAISAN
                                                             (IDY) (1:2)
                   END-IF          
                   IF      SPA-NAI-R01-BTPARA-TTAISYO (IDX)
                                           =   1
                       ADD     1       TO  IDZ
                       MOVE    1       TO  SPA-NAI-R01-TTAISYO (IDY)
                       IF      SPA-NAI-R01-BTPARA-HYOUJI-FLG   =   0
                           MOVE    "○"    TO   SPA-GMN-R01-TTAISYO(IDY)
                       END-IF
                   END-IF
                   IF      SPA-NAI-R01-BTPARA-HYOUJI-FLG   =   1
                       MOVE    SPA-R01-NYUGAIKBN
                                       TO  SPA-GMN-R01-TNYUGAIKBN(IDY)
                   END-IF 
               END-IF
           END-PERFORM
           MOVE    IDY                 TO  SPA-GMN-R01-MAX
      *
      *   件数表示
           MOVE    SPACE               TO  SPA-GMN-R01-BTPARA-KENSU
           MOVE    IDZ                 TO  WRK-BTPARA-KENSU1
           MOVE    SPA-NAI-R01-BTPARA-MAX  TO  WRK-BTPARA-KENSU2
           MOVE    "/"                 TO  WRK-BTPARA-FL
           MOVE    WRK-BTPARA-KENSU    TO  SPA-GMN-R01-BTPARA-KENSU
      *
           IF      SPA-NAI-R01-BTPARA-MAX  =   ZERO
               MOVE    "0031"              TO  SPA-ERRCD
           ELSE
               IF    ( SPA-NAI-R01-BTPARA-HYOUJI-FLG   =   1    )
               AND   ( SPA-NAI-R01-BTPARA-TAISYO-MAX   =   ZERO )
                   MOVE    "0030"              TO  SPA-ERRCD
                   MOVE    8                   TO  SPA-GMN-R01-CUR
               ELSE
                   MOVE    7                   TO  SPA-GMN-R01-CUR
               END-IF
           END-IF
           .
       410711-RENKEI-DATA-JYOKEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求解除データ編集処理
      *****************************************************************
       41072-MISEIKYU-DATA-HENSYU-SEC       SECTION.
      *
           INITIALIZE                  SPA-NAI-R01-BTPARA-AREA
                                       SPA-GMN-R01-BTPARA-AREA
           MOVE    ZERO            TO  SPA-NAI-R01-BTPARA-MAX
                                       SPA-NAI-R01-BTPARA-TAISYO-MAX
      *
           MOVE    ZERO            TO  SPA-GMN-R01-MAX
                                       SPA-GMN-R01-DELETE-MAX 
           INITIALIZE                  SPA-GMN-R01-TBL
                                       SPA-NAI-R01-TBL
           MOVE    ZERO            TO  SPA-NAI-R01-SELNO
           MOVE    SPACE           TO  SPA-NAI-R01-SYOKBN
           MOVE    ZERO            TO  FLG-TAISYO 
      *
           INITIALIZE                  SYUKA-REC
grpsys     MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
           MOVE    SPA-R01-NYUGAIKBN
                                   TO  SYUKA-NYUGAIKBN
           MOVE    "YYYY"          TO  SYUKA-RECESYUBETU
           MOVE    "02"            TO  SYUKA-MISEIKYU-FLG
           MOVE    SYUKA-REC       TO  MCPDATA-REC
           MOVE    "tbl_syuka"     TO  MCP-TABLE
           MOVE    "key14"         TO  MCP-PATHNAME
      *
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syuka"         TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 910-SYUKA-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SYUKA
           END-IF
      *
           MOVE    ZERO            TO  IDX
           PERFORM UNTIL   FLG-SYUKA   =   1
               ADD     1               TO  IDX
      *
               INITIALIZE                  QTORSUB-AREA
      *
               MOVE    SYUKA-PTID      TO  QTORS-PTID
      *
               INITIALIZE                  ORCSPTIDAREA
grpsys         MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    SYUKA-PTID      TO  SPTID-PTID
               CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                               SPA-AREA
               MOVE    SPTID-PTNUM     TO
                                       SPA-NAI-R01-BTPARA-TPTNUM (IDX)
      *
grpsys         MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
               MOVE    SYUKA-PTID      TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               MOVE    PTINF-NAME      TO
                                       SPA-NAI-R01-BTPARA-TNAME  (IDX)
               MOVE    PTINF-TSTPTNUMKBN
                                       TO  QTORS-TSTPTNUMKBN
      *
               MOVE    SYUKA-SRYYM     TO
                                       SPA-NAI-R01-BTPARA-TSRYYM (IDX)
                                           WRK-SYMD (1:6)
               MOVE    "01"            TO  WRK-SYMD (7:2) 
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG     TO
                                       SPA-NAI-R01-BTPARA-TSRYYMH (IDX)
      *
               INITIALIZE                  SS001-AREA
grpsys         MOVE    SPA-HOSPNUM     TO  SS001-HOSPNUM
               MOVE    SYUKA-PTID      TO  SS001-PTID
               MOVE    SYUKA-SRYYM     TO  SS001-SRYYM
               CALL    "ORCSS001"      USING   SS001-AREA
                                               SPA-AREA
               MOVE    SS001-GIHO-FLG  TO  QTORS-GIHO-FLG
               MOVE    SS001-GRSI-FLG  TO  QTORS-GRSI-FLG
               MOVE    SS001-GJBI-FLG  TO  QTORS-GJBI-FLG
               MOVE    SS001-GKGI-FLG  TO  QTORS-GKGI-FLG
               MOVE    SS001-GDI3-FLG  TO  QTORS-GDI3-FLG
               MOVE    SS001-NIHO-FLG  TO  QTORS-NIHO-FLG
               MOVE    SS001-NRSI-FLG  TO  QTORS-NRSI-FLG
               MOVE    SS001-NJBI-FLG  TO  QTORS-NJBI-FLG
               MOVE    SS001-NKGI-FLG  TO  QTORS-NKGI-FLG
               MOVE    SS001-NDI3-FLG  TO  QTORS-NDI3-FLG
               IF    ( QTORS-NIHO-FLG  =   ZERO )
               AND   ( QTORS-GIHO-FLG  =   ZERO )
      *************MOVE    1               TO
      *************                    SPA-NAI-R01-BTPARA-TSINRYO (IDX)
                   CONTINUE
               ELSE
                   EVALUATE    SPA-R01-NYUGAIKBN
                                       ALSO    SPA-R01-RECEKBN
                       WHEN    "1" ALSO    "0"
                           IF      QTORS-NIHO-FLG      =   1
                               MOVE    1       TO
                                       SPA-NAI-R01-BTPARA-TTAISYO (IDX)
                           END-IF          
                       WHEN    "2" ALSO    "0"
                           IF      QTORS-GIHO-FLG      =   1
                               MOVE    1       TO
                                       SPA-NAI-R01-BTPARA-TTAISYO (IDX)
                           END-IF          
                   END-EVALUATE
               END-IF
      *      
               IF      SPA-NAI-R01-BTPARA-TTAISYO (IDX)
                                               =   1
      *            対象データの一件目の診療年月取得
                   IF      SPA-NAI-R01-SYOKBN      =   SPACE
                       IF      SPA-NAI-R01-BTPARA-TSRYYM (IDX)
                                                <   200804
                           MOVE    "1"          TO  SPA-NAI-R01-SYOKBN
                       END-IF
                   END-IF
      *
                   EVALUATE    SPA-NAI-R01-SYOKBN
                       WHEN    "1"
                           IF      SPA-NAI-R01-BTPARA-TSRYYM (IDX)
                                                   >=  200804
                               MOVE    2               TO  
                                       SPA-NAI-R01-BTPARA-TTAISYO (IDX)
                               MOVE    1               TO  FLG-TAISYO 
                           END-IF
                   END-EVALUATE
               END-IF
      *
               IF      SPA-NAI-R01-BTPARA-TTAISYO (IDX)
                                               =   1
                   ADD     1       TO  SPA-NAI-R01-BTPARA-TAISYO-MAX
               END-IF          
      *
               MOVE    QTORSUB-AREA
                                   TO
                                   SPA-NAI-R01-BTPARA-TQTORSUB-AR(IDX)
      *
               MOVE    "tbl_syuka"     TO  MCP-TABLE
               MOVE    "key14"         TO  MCP-PATHNAME
               PERFORM 910-SYUKA-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_syuka"         TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    IDX                 TO  SPA-NAI-R01-BTPARA-MAX
      *
           IF      SPA-NAI-R01-BTPARA-MAX  =   ZERO
               MOVE    "0033"          TO  SPA-ERRCD
               MOVE    8               TO  SPA-GMN-R01-CUR
           ELSE
      *        未請求解除データ表示処理
               IF      SPA-NAI-R01-BTPARA-TAISYO-MAX   =   ZERO
                   MOVE    ZERO        TO  SPA-NAI-R01-BTPARA-HYOUJI-FLG
                   PERFORM 410721-MISEIKYU-DATA-JYOKEN-SEC
                   MOVE    "0032"              TO  SPA-ERRCD
                   MOVE    7                   TO  SPA-GMN-R01-CUR
               ELSE
                   MOVE    1           TO  SPA-NAI-R01-BTPARA-HYOUJI-FLG
                   PERFORM 410721-MISEIKYU-DATA-JYOKEN-SEC
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  SPA-NAI-R01-SYOKBN
      *
           IF      FLG-TAISYO          =   1
               MOVE    "0040"              TO  SPA-ERRCD
           END-IF
           .
       41072-MISEIKYU-DATA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求解除データ表示処理
      *****************************************************************
       410721-MISEIKYU-DATA-JYOKEN-SEC       SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-R01-MAX
                                       SPA-GMN-R01-DELETE-MAX 
           INITIALIZE                  SPA-GMN-R01-TBL
                                       SPA-NAI-R01-TBL
           MOVE    ZERO            TO  SPA-NAI-R01-SELNO
      *
           MOVE    ZERO                TO  IDY
                                           IDZ
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   SPA-NAI-R01-BTPARA-MAX
      *
      *        対象データか判定
               IF    ( SPA-NAI-R01-BTPARA-HYOUJI-FLG
                                           =   1 )
               AND   ( SPA-NAI-R01-BTPARA-TTAISYO (IDX)
                                       NOT =   1 )
                   CONTINUE
               ELSE
                   MOVE    SPA-NAI-R01-BTPARA-TQTORSUB-AR (IDX)
                                           TO  QTORSUB-AREA
      *
                   ADD     1               TO  IDY
      *
                   MOVE    IDY             TO  SPA-GMN-R01-TRENNUM (IDY)
                   MOVE    QTORS-PTID      TO  SPA-NAI-R01-TPTID   (IDY)
                   MOVE    SPA-NAI-R01-BTPARA-TPTNUM (IDX)
                                           TO  SPA-GMN-R01-TPTNUM  (IDY)
                   MOVE    SPA-NAI-R01-BTPARA-TNAME  (IDX)
                                           TO  SPA-GMN-R01-TNAME   (IDY)
                   MOVE    SPA-NAI-R01-BTPARA-TSRYYM (IDX)
                                           TO  SPA-NAI-R01-TSRYYM  (IDY)
                   MOVE    SPA-NAI-R01-BTPARA-TSRYYMH  (IDX)
                                           TO  SPA-GMN-R01-TSRYYM  (IDY)
                   IF      QTORS-TSTPTNUMKBN   =   "1"
                       MOVE    "テ"        TO
                                           SPA-GMN-R01-TTSTPTNUMKBN(IDY)
                   END-IF
                   IF      SPA-NAI-R01-BTPARA-TTAISYO (IDX)
                                                       =   1
                       ADD     1           TO  IDZ
                       MOVE    1           TO   SPA-NAI-R01-TTAISYO(IDY)
                       IF      SPA-NAI-R01-BTPARA-HYOUJI-FLG   =   0
                           MOVE    "○"    TO   SPA-GMN-R01-TTAISYO(IDY)
                       END-IF
                   END-IF
                   IF      SPA-NAI-R01-BTPARA-HYOUJI-FLG   =   1
                       MOVE    SPA-R01-NYUGAIKBN
                                           TO
                                           SPA-GMN-R01-TNYUGAIKBN(IDY)
                   END-IF 
               END-IF
           END-PERFORM
           MOVE    IDY                     TO  SPA-GMN-R01-MAX
      *
      *   件数表示
           MOVE    SPACE               TO  SPA-GMN-R01-BTPARA-KENSU
           MOVE    IDZ                 TO  WRK-BTPARA-KENSU1
           MOVE    SPA-NAI-R01-BTPARA-MAX
                                       TO  WRK-BTPARA-KENSU2
           MOVE    "/"                 TO  WRK-BTPARA-FL
           MOVE    WRK-BTPARA-KENSU    TO  SPA-GMN-R01-BTPARA-KENSU
      *
           IF      SPA-NAI-R01-BTPARA-MAX  =   ZERO
               MOVE    "0033"              TO  SPA-ERRCD
           ELSE
               IF    ( SPA-NAI-R01-BTPARA-HYOUJI-FLG   =   1    )
               AND   ( SPA-NAI-R01-BTPARA-TAISYO-MAX   =   ZERO )
                   MOVE    "0032"              TO  SPA-ERRCD
                   MOVE    8                   TO  SPA-GMN-R01-CUR
               ELSE
                   MOVE    7                   TO  SPA-GMN-R01-CUR
               END-IF
           END-IF
           .
       410721-MISEIKYU-DATA-JYOKEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括入力処理
      *****************************************************************
       400-ENTRY-SYORI-SEC       SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-R01-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
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
           IF      FLG-TOUROKU         =   1
      *        更新前
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
           MOVE    R01-PTNUM       TO  SPA-GMN-R01-PTNUM
           MOVE    R01-SRYYM       TO  SPA-GMN-R01-SRYYM
           MOVE    R01-NYUGAIKBN   TO  SPA-GMN-R01-NYUGAIKBN
           MOVE    R01-STJYUSINYMD TO  SPA-GMN-R01-STJYUSINYMD
           MOVE    R01-EDJYUSINYMD TO  SPA-GMN-R01-EDJYUSINYMD
           MOVE    R01-TEISEIYM    TO  SPA-GMN-R01-TEISEIYM
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
           EVALUATE    SPA-GMN-R01-SYORI
               WHEN    "1"
                   IF      FLG-TOUROKU         =   ZERO
                       PERFORM 4103-PTNUM-CHK-SEC
                       IF      SPA-ERRCD       NOT =   SPACE
                           GO      TO      4102-KIHON-CHK-EXT
                       END-IF
                       PERFORM 4104-SRYYM-CHK-SEC
                       IF      SPA-ERRCD       NOT =   SPACE
                           GO      TO      4102-KIHON-CHK-EXT
                       END-IF
                       PERFORM 4105-NYUGAIKBN-CHK-SEC
                       IF      SPA-ERRCD       NOT =   SPACE
                           GO      TO      4102-KIHON-CHK-EXT
                       END-IF
      *
      *               更新処理
                      IF    ( SPA-NAI-R01-SRYYM   >   ZERO  )
                      AND   ( SPA-NAI-R01-PTID    >   ZERO  )
                      AND   (  SPA-GMN-R01-NYUGAIKBN
                                          NOT =   SPACE )
                          IF      SPA-GMN-R01-RENNUM  =   ZERO
                              PERFORM 41041-TUIKA-SEC
                          ELSE
                              PERFORM 41042-HENKOU-SEC
                          END-IF
                       END-IF
                   END-IF
               WHEN    "2"
                   PERFORM 4110-TEISEIYM-SEC
               WHEN    "3"
                   PERFORM 4108-STJYUSINYMD-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       GO      TO      4102-KIHON-CHK-EXT
                   END-IF
                   PERFORM 4109-EDJYUSINYMD-SEC
           END-EVALUATE 
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       4103-PTNUM-CHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-R01-NAME
           MOVE    ZERO                TO  SPA-NAI-R01-PTID
                                           SPA-NAI-R01-DELETE
      *
           INITIALIZE                  ORCSPTIDAREA
grpsys     MOVE    SPA-HOSPNUM             TO  SPTID-HOSPNUM
           MOVE    SPA-GMN-R01-PTNUM       TO  SPTID-PTNUM
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               MOVE    "0003"          TO  SPA-ERRCD
           ELSE
               MOVE    SPTID-PTNUM         TO  SPA-GMN-R01-PTNUM
               MOVE    SPTID-PTID          TO  SPA-NAI-R01-PTID
      *
      *        患者マスター存在チェック
grpsys         MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    SPA-NAI-R01-PTID    TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF       NOT =   ZERO
                   MOVE    "0003"              TO  SPA-ERRCD
               ELSE
                   MOVE    PTINF-NAME          TO  SPA-GMN-R01-NAME
                   MOVE    PTINF-TSTPTNUMKBN   TO
                                               SPA-NAI-R01-TSTPTNUMKBN
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    4               TO  SPA-GMN-R01-CUR
           END-IF
           .
       4103-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月チェック処理
      *****************************************************************
       4104-SRYYM-CHK-SEC          SECTION.
      *
      *    患者番号入力時に診療年月が未入力のとき
           IF    ( WRK-MCP-WIDGET     =   "PTNUM" )
           AND   ( SPA-GMN-R01-SRYYM      =   SPACE   )
               GO  TO  4104-SRYYM-CHK-EXT
           END-IF
      *  
           MOVE    ZERO               TO  SPA-NAI-R01-SRYYM
      *
           IF      SPA-GMN-R01-SRYYM      =   SPACE
               MOVE    SPA-NAI-R01-HYO-SRYYM   TO  SPA-NAI-R01-SRYYM
               MOVE    SPA-NAI-R01-HYO-SRYYMH  TO  SPA-GMN-R01-SRYYM
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-R01-SRYYM       TO  SGDAY-INDAY
               MOVE    "1"                 TO  SGDAY-INTYPE
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN-R01-SRYYM
                   MOVE    SGDAY-SDAY (1:6)    TO  SPA-NAI-R01-SRYYM
               ELSE
                   MOVE    "0009"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    5                   TO  SPA-GMN-R01-CUR
           END-IF
      *
           .
       4104-SRYYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分  入力処理
      *****************************************************************
       4105-NYUGAIKBN-CHK-SEC                SECTION.
      *
      *    患者番号入力時に診療年月が未入力のとき
           IF    ( WRK-MCP-WIDGET     =   "PTNUM" )
           AND   ( SPA-GMN-R01-SRYYM      =   SPACE   )
           AND   ( SPA-GMN-R01-NYUGAIKBN  =   SPACE   )
               GO  TO  4105-NYUGAIKBN-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-R01-NYUGAIKBN    =   SPACE
               MOVE    SPA-R01-NYUGAIKBN    TO  SPA-GMN-R01-NYUGAIKBN
           END-IF
      *
           IF      SPA-GMN-R01-NYUGAIKBN    =   SPA-R01-NYUGAIKBN
               CONTINUE
           ELSE
               MOVE    "0001"               TO  SPA-ERRCD
           END-IF 
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    6                   TO  SPA-GMN-R01-CUR
           END-IF
           .
       4105-NYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加処理
      *****************************************************************
       41041-TUIKA-SEC                SECTION.
      *
           IF      SPA-GMN-R01-MAX     =   WRK-CONS-LINE-MAX
               MOVE    "0002"              TO  SPA-ERRCD
               GO      TO      41041-TUIKA-EXT
           END-IF
      *
           PERFORM 4203-KOMOKUCHK-SEC
           IF    ( SPA-ERRCD    NOT =   SPACE )
           OR    ( SPA-RIDCD    NOT =   SPACE )
               GO  TO  41041-TUIKA-EXT
           END-IF    
      *
           EVALUATE    SPA-R01-RECEKBN
               WHEN    "0"
               WHEN    "3"
                   PERFORM 4204-SRYYM-CHK-SEC
                   IF      SPA-ERRCD    NOT =   SPACE
                       GO  TO  41041-TUIKA-EXT
                   END-IF
           END-EVALUATE    
      *
           COMPUTE IDY     =   SPA-GMN-R01-MAX  +   1
           PERFORM 4202-INTBL-CHK-SEC
           IF      SPA-ERRCD        NOT =   SPACE
              GO  TO  41041-TUIKA-EXT
           END-IF
      *    
           ADD     1                TO  SPA-GMN-R01-MAX
           MOVE    SPA-GMN-R01-MAX      TO
                                SPA-GMN-R01-TRENNUM   (SPA-GMN-R01-MAX)
           MOVE    SPA-GMN-R01-MAX          TO  IDX
           PERFORM 4201-INTBL-SET-SEC
      *
           MOVE    SPA-GMN-R01-MAX  TO  R01-ROW
      *
           MOVE    4                TO  SPA-GMN-R01-CUR  
      *
           .
       41041-TUIKA-EXT.
           EXIT.
      *
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4203-KOMOKUCHK-SEC           SECTION.
      *
           MOVE    SPACE                TO  SPA-ERRCD
           MOVE    SPA-GMN-R01-CUR          TO  WRK-GMN-CUR
      * 
      *****PERFORM 4103-PTNUM-CHK-SEC
      *
      *****IF      SPA-GMN-R01-NYUGAIKBN    =   SPACE
      *****    MOVE    SPA-R01-NYUGAIKBN    TO  SPA-GMN-R01-NYUGAIKBN
      *****END-IF
      *
      *    自賠責（新様式）のとき健保準拠かチェック
      *    平成２９年７月診療分以降はエラーとしない 
           IF    ( SPA-R01-RECEKBN     =   "4" )
               INITIALIZE                  SYS-4001-REC
               MOVE    "4001"              TO  SYS-4001-KANRICD
               MOVE    "*"                 TO  SYS-4001-KBNCD
               MOVE    SPA-NAI-R01-SRYYM   TO  SYS-4001-STYUKYMD(1:6)
               MOVE    "01"                TO  SYS-4001-STYUKYMD(7:2)
               MOVE    SYS-4001-STYUKYMD   TO  SYS-4001-EDYUKYMD
grpsys         MOVE    SPA-HOSPNUM         TO  SYS-4001-HOSPNUM
               MOVE    SYS-4001-REC        TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-4001-REC
               ELSE
                  INITIALIZE                   SYS-4001-REC
               END-IF
               IF    ( SYS-4001-JIBAIKBN   =   "2"    )
               AND   ( SPA-NAI-R01-SRYYM   <   201707 )
                   MOVE    "0036"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN-R01-CUR
                   GO  TO  4203-KOMOKUCHK-EXT
               END-IF
           END-IF
      *
      *    診療会計チェック
           MOVE    SPA-NAI-R01-PTID    TO  WRK-ACCT-PTID
           MOVE    SPA-NAI-R01-SRYYM   TO  WRK-ACCT-SRYYM
           MOVE    SPA-GMN-R01-NYUGAIKBN
                                       TO  WRK-ACCT-NYUGAIKBN
           PERFORM 42031-SRYACCT-CHK-SEC
      *
      *    診療会計が存在しないとき入院会計をチェック（入院のみ）
           IF    ( FLG-ERR             =   ZERO )
           AND   ( SPA-R01-NYUGAIKBN   =   "1"  )
               PERFORM 42032-NYUINACCT-CHK-SEC
           END-IF
      *
           IF      FLG-ERR             =   1
               MOVE    "0008"              TO  SPA-ERRCD
           ELSE 
               IF      FLG-CHK             =   ZERO
                   MOVE    "0006"              TO  SPA-ERRCD
               ELSE 
                   IF      FLG-CHK             =   2
                       MOVE    "0043"              TO  SPA-ERRCD
                       MOVE    5                   TO  SPA-GMN-R01-CUR
                   ELSE
                       IF    ( SPA-R01-NYUGAIKBN   =   "1"  )
                       AND   ( FLG-ZAISKBKBN       =   ZERO )
                           MOVE    "0014"              TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-R01-PRTKBN       =   "9" OR  "8"
             CONTINUE
           ELSE
             IF      SPA-ERRCD            =   SPACE
      *        未請求設定のチェック（医保）
               IF      SPA-R01-RECEKBN     =   "0"
                   INITIALIZE                  SYUKA-REC
                   MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
                   MOVE    SPA-NAI-R01-PTID
                                           TO  SYUKA-PTID
                   MOVE    SPA-NAI-R01-SRYYM
                                           TO  SYUKA-SRYYM
                   MOVE    SPA-GMN-R01-NYUGAIKBN
                                           TO  SYUKA-NYUGAIKBN
                   MOVE    "YYYY"          TO  SYUKA-RECESYUBETU
                   MOVE    SYUKA-REC       TO  MCPDATA-REC
                   MOVE    "tbl_syuka"     TO  WRK-MCP-TABLE
                   MOVE    "key15"         TO  WRK-MCP-PATHNAME
                   PERFORM 900-SYUKA-READ-SEC
                   IF      FLG-SYUKA       =   ZERO
                       IF      SYUKA-MISEIKYU-FLG   =   "01"
                           MOVE    "0035"               TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *        請求しない設定の存在チェック
               IF      SPA-ERRCD           =   SPACE
                   INITIALIZE                      RECE10-REC
                   MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
                   MOVE    SPA-NAI-R01-SRYYM   TO  RECE10-SRYYM
                   MOVE    SPA-GMN-R01-NYUGAIKBN
                                               TO  RECE10-NYUGAIKBN
                   MOVE    SPA-NAI-R01-PTID    TO  RECE10-PTID
                   PERFORM 900-SEIKYU-SKYKBN-CHECK-SEC
                   IF      FLG-SEIKYU-UMU      =   1
                       MOVE    1                   TO  SPA-NAI-R01-CHK
                   ELSE
                       MOVE    ZERO                TO  SPA-NAI-R01-CHK
                   END-IF
               END-IF
             ELSE
      *        請求管理テーブルの存在チェック
      *********IF      SPA-R01-RECEKBN     =   "0" OR  "6"
                   EVALUATE    SPA-ERRCD
                       WHEN    "0006"
                       WHEN    "0008"
                       WHEN    "0014"
                           INITIALIZE                  RECE10-REC
                           MOVE    SPA-HOSPNUM     TO  RECE10-HOSPNUM
                           MOVE    SPA-NAI-R01-SRYYM
                                                   TO  RECE10-SRYYM
                           MOVE    SPA-GMN-R01-NYUGAIKBN
                                                   TO  RECE10-NYUGAIKBN
                           MOVE    SPA-NAI-R01-PTID
                                                   TO  RECE10-PTID
                           PERFORM 900-SEIKYU-READ-SEC
                           IF      FLG-SEIKYU-UMU  =   1
                               EVALUATE    SPA-ERRCD
                                   WHEN    "0006"
                                       MOVE    "1008"      TO  SPA-RIDCD
                                   WHEN    "0008"
                                       MOVE    "1009"      TO  SPA-RIDCD
                                   WHEN    "0014"
                                       MOVE    "1010"      TO  SPA-RIDCD
                               END-EVALUATE
                               MOVE    SPACE    TO  SPA-ERRCD
      *****                    MOVE    1        TO  SPA-NAI-R01-DELETE
      *
                               IF      SPA-GMN-R01-MAX
                                                =   WRK-CONS-LINE-MAX
                                   MOVE    "0002"      TO  SPA-ERRCD
                                   MOVE    SPACE       TO  SPA-RIDCD
                               END-IF
                           END-IF
      *
                           IF    ( SPA-GMN-R01-MAX
                                                <   WRK-CONS-LINE-MAX )
                           AND   ( SPA-RIDCD    =   SPACE             )
                               MOVE    SPA-ERRCD        TO  WRK-ERRCD
                               COMPUTE IDY =   SPA-GMN-R01-MAX  +   1
                               PERFORM 4202-INTBL-CHK-SEC
                               IF      SPA-ERRCD        =   SPACE
                                   MOVE    WRK-ERRCD    TO  SPA-ERRCD
                               ELSE
                                   MOVE    "0038"       TO  SPA-ERRCD
                               END-IF
                           END-IF    
                   END-EVALUATE
      *********END-IF
             END-IF
           END-IF
      *
           IF      SPA-ERRCD            =   SPACE
               MOVE    WRK-GMN-CUR          TO  SPA-GMN-R01-CUR
           ELSE
               MOVE    5                    TO  SPA-GMN-R01-CUR
           END-IF             
           .
       4203-KOMOKUCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計チェック処理
      *****************************************************************
       42031-SRYACCT-CHK-SEC           SECTION.
      * 
           MOVE    ZERO                TO  FLG-CHK
                                           FLG-ERR
                                           FLG-ZAISKBKBN
      *     
           INITIALIZE                      SRYACCT-REC
grpsys     MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    WRK-ACCT-PTID       TO  ACCT-PTID
           MOVE    WRK-ACCT-SRYYM      TO  ACCT-SRYYM
           MOVE    WRK-ACCT-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           IF      SPA-R01-PRTKBN  =   "8" OR  "9"
               MOVE    "key68"         TO  WRK-MCP-SRYACCT-PATHNAME
           ELSE
               MOVE    "key30"         TO  WRK-MCP-SRYACCT-PATHNAME
           END-IF
           MOVE    WRK-MCP-SRYACCT-PATHNAME
                                       TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_sryacct"   TO  MCP-TABLE
               MOVE    WRK-MCP-SRYACCT-PATHNAME
                                       TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           PERFORM             UNTIL   FLG-SRYACCT     =   1
                               OR      FLG-CHK         =   1
                               OR      FLG-ERR         =   1
               MOVE    KEY-NEW             TO  KEY-OLD
      *     
      *        保険組合せ読込み
               INITIALIZE                  HKNCOMBI-REC
grpsys         MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
               MOVE    ACCT-PTID           TO  COMB-PTID
               MOVE    ACCT-HKNCOMBI       TO  COMB-HKNCOMBINUM 
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               PERFORM  910-HKNCOMBI-INV-SEC
      *
               IF      FLG-HKNCOMBI        =   ZERO
                   EVALUATE    SPA-R01-RECEKBN
                       WHEN    "0"
                           IF    ( COMB-HKNNUM =
                                           WRK-CONS-ROUSAI-HKNNUM    )
                           OR    ( COMB-HKNNUM =
                                           WRK-CONS-JIBAISEKI-HKNNUM )
                           OR    ( COMB-HKNNUM =
                                           WRK-CONS-KOUGAI-HKNNUM    )
                               CONTINUE
                           ELSE    
                               MOVE    1           TO  FLG-CHK
                           END-IF
                       WHEN    "3"
                           IF      COMB-HKNNUM =
                                           WRK-CONS-ROUSAI-HKNNUM
                               MOVE    1           TO  FLG-CHK
                           END-IF
                       WHEN    "4"
                           IF      COMB-HKNNUM =
                                           WRK-CONS-JIBAISEKI-HKNNUM
                               MOVE    1           TO  FLG-CHK
      *                        自賠責（新様式）のとき健保準拠かチェック
                               PERFORM 420311-JIBAI-CHK-SEC
                           END-IF
                       WHEN    "5"
                           IF      COMB-HKNNUM =
                                           WRK-CONS-JIBAISEKI-HKNNUM
                               MOVE    1           TO  FLG-CHK
                           END-IF
                       WHEN    "6"
                           IF      COMB-HKNNUM =
                                           WRK-CONS-KOUGAI-HKNNUM
                               MOVE    1           TO  FLG-CHK
                           END-IF
                       WHEN    "8"
                           IF      COMB-KOHHKNNUM (1)
                                               =
                                           WRK-CONS-DAISANSYA-HKNNUM
                               MOVE    1           TO  FLG-CHK
                           END-IF
                   END-EVALUATE
               ELSE
                   MOVE    1                   TO  FLG-ERR
               END-IF
      *
               PERFORM UNTIL   FLG-SRYACCT     =   1
                       OR      FLG-CHK         =   1
                       OR      FLG-ERR         =   1 
                       OR      KEY-NEW     NOT =   KEY-OLD
                   MOVE    "tbl_sryacct"   TO  MCP-TABLE
                   MOVE    WRK-MCP-SRYACCT-PATHNAME
                                           TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               END-PERFORM
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    WRK-MCP-SRYACCT-PATHNAME
                                   TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       42031-SRYACCT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責（新様式）のとき健保準拠のチェック処理
      *****************************************************************
       420311-JIBAI-CHK-SEC            SECTION.
      *
           IF      WRK-ACCT-SRYYM   >=  "201707"
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
               MOVE    WRK-ACCT-PTID       TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               PERFORM 900-PTRSIINF-INV-SEC
      *
               IF      PTRSI-JIBAIKBN      =   SPACE   OR  "0"
                   MOVE    SYS-4001-JIBAIKBN   TO  PTRSI-JIBAIKBN
               END-IF
               IF      PTRSI-JIBAIKBN      =   "2"
                   MOVE    2                   TO  FLG-CHK
               END-IF
           END-IF
           .
       420311-JIBAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計チェック処理
      *****************************************************************
       42032-NYUINACCT-CHK-SEC           SECTION.
      *     
      *    入院会計読込み（基本）
           INITIALIZE                  NYUINACCT-REC
grpsys     MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    WRK-ACCT-PTID       TO  NACCT-PTID
           MOVE    WRK-ACCT-SRYYM      TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT    
           END-IF
      *
           PERFORM             UNTIL    FLG-NYUINACCT  =   1
                               OR       FLG-ERR        =   1      
      *        保険組合せ読込み
               IF      NACCT-ZAISKBKBN     =   "1"
                   MOVE    1                   TO  FLG-ZAISKBKBN
               END-IF
               IF      NACCT-ZAISKBKBN     =   "5"
                 PERFORM VARYING IDXX    FROM    1   BY  1
                         UNTIL   IDXX    >       31
                         OR      FLG-CHK =       1
                         OR      FLG-ERR =       1     
                   IF      NACCT-DAY (IDXX)      >   ZERO    
                       INITIALIZE                  HKNCOMBI-REC
grpsys                 MOVE    NACCT-HOSPNUM       TO  COMB-HOSPNUM
                       MOVE    NACCT-PTID          TO  COMB-PTID
                       MOVE    NACCT-DAY (IDXX)    TO  COMB-HKNCOMBINUM 
                       MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                       PERFORM  910-HKNCOMBI-INV-SEC
      *
                       IF      FLG-HKNCOMBI        =   ZERO
                         EVALUATE    SPA-R01-RECEKBN
                           WHEN    "0"
                               IF      COMB-HKNNUM =
                                               WRK-CONS-ROUSAI-HKNNUM
                               OR      COMB-HKNNUM =
                                               WRK-CONS-JIBAISEKI-HKNNUM
                               OR      COMB-HKNNUM =
                                               WRK-CONS-KOUGAI-HKNNUM
                                   CONTINUE
                               ELSE    
                                   MOVE    1           TO  FLG-CHK
                               END-IF
                           WHEN    "3"
                               IF      COMB-HKNNUM =
                                               WRK-CONS-ROUSAI-HKNNUM
                                   MOVE    1           TO  FLG-CHK
                               END-IF
                           WHEN    "4"
                               IF      COMB-HKNNUM =
                                               WRK-CONS-JIBAISEKI-HKNNUM
                                   MOVE    1           TO  FLG-CHK
      *                            自賠責（新様式）のとき健保準拠かチェック
                                   PERFORM 420311-JIBAI-CHK-SEC
                               END-IF
                           WHEN    "5"
                               IF      COMB-HKNNUM =
                                               WRK-CONS-JIBAISEKI-HKNNUM
                                   MOVE    1           TO  FLG-CHK
                               END-IF
                           WHEN    "6"
                               IF      COMB-HKNNUM =
                                               WRK-CONS-KOUGAI-HKNNUM
                                   MOVE    1           TO  FLG-CHK
                               END-IF
                           WHEN    "8"
                               IF      COMB-KOHHKNNUM (1)
                                                   =
                                               WRK-CONS-DAISANSYA-HKNNUM
                                   MOVE    1           TO  FLG-CHK
                               END-IF
                         END-EVALUATE 
                       ELSE
                         MOVE    1                 TO  FLG-ERR
                       END-IF
                   END-IF
                 END-PERFORM
                 MOVE    1                   TO  FLG-NYUINACCT 
               ELSE            
                 MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                 MOVE    "key14"             TO  MCP-PATHNAME
                 PERFORM  900-NYUINACCT-READ-SEC
               END-IF
           END-PERFORM    
      *    カーソルクロース
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    会計が見つからないとき、同日再入院があれば保険組合せ読込みをチェック
           IF    ( FLG-ERR             =   ZERO )
           AND   ( FLG-CHK             =   ZERO )
               INITIALIZE                      ORCSNYUINDAY-AREA
               MOVE    SPA-HOSPNUM         TO  LNK-NYUINDAY-HOSPNUM
               MOVE    WRK-ACCT-PTID       TO  LNK-NYUINDAY-PTID
               MOVE    WRK-ACCT-SRYYM      TO  LNK-NYUINDAY-SANTEIYM
               CALL    "ORCSNYUINDAY"      USING   ORCSNYUINDAY-AREA
                                                   SPA-AREA
               IF      LNK-NYUINDAY-RC >   0
                   GO  TO  42032-NYUINACCT-CHK-EXT
               END-IF
      *
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       31
                       OR      FLG-CHK =       1
                       OR      FLG-ERR =       1     
                   IF      LNK-NYUINDAY-DOUJITSU-KBN (IDXX)
                                               =   "1"
                       INITIALIZE                      HKNCOMBI-REC
                       MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
                       MOVE    WRK-ACCT-PTID       TO  COMB-PTID
                       MOVE    LNK-NYUINDAY-DOUJITSU-HKNCOMBI (IDXX)
                                                   TO  COMB-HKNCOMBINUM 
                       MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                       PERFORM 910-HKNCOMBI-INV-SEC
      *
                       IF      FLG-HKNCOMBI        =   ZERO
                         EVALUATE    SPA-R01-RECEKBN
                           WHEN    "0"
                               IF      COMB-HKNNUM =
                                               WRK-CONS-ROUSAI-HKNNUM
                               OR      COMB-HKNNUM =
                                               WRK-CONS-JIBAISEKI-HKNNUM
                               OR      COMB-HKNNUM =
                                               WRK-CONS-KOUGAI-HKNNUM
                                   CONTINUE
                               ELSE    
                                   MOVE    1           TO  FLG-CHK
                               END-IF
                           WHEN    "3"
                               IF      COMB-HKNNUM =
                                               WRK-CONS-ROUSAI-HKNNUM
                                   MOVE    1           TO  FLG-CHK
                               END-IF
                           WHEN    "4"
                               IF      COMB-HKNNUM =
                                               WRK-CONS-JIBAISEKI-HKNNUM
                                   MOVE    1           TO  FLG-CHK
      *                            自賠責（新様式）のとき健保準拠かチェック
                                   PERFORM 420311-JIBAI-CHK-SEC
                               END-IF
                           WHEN    "5"
                               IF      COMB-HKNNUM =
                                               WRK-CONS-JIBAISEKI-HKNNUM
                                   MOVE    1           TO  FLG-CHK
                               END-IF
                           WHEN    "6"
                               IF      COMB-HKNNUM =
                                               WRK-CONS-KOUGAI-HKNNUM
                                   MOVE    1           TO  FLG-CHK
                               END-IF
                           WHEN    "8"
                               IF      COMB-KOHHKNNUM (1)
                                                   =
                                               WRK-CONS-DAISANSYA-HKNNUM
                                   MOVE    1           TO  FLG-CHK
                               END-IF
                         END-EVALUATE 
                       ELSE
                         MOVE    1                 TO  FLG-ERR
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .
       42032-NYUINACCT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       診療年月の改正前・後の重複チェック処理
      *****************************************************************
       4204-SRYYM-CHK-SEC           SECTION.
      *
           EVALUATE    SPA-R01-RECEKBN ALSO    SPA-R01-NYUGAIKBN
               WHEN    "0"             ALSO    ANY
                   IF      SPA-NAI-R01-DELETE  =   1
                       CONTINUE
                   ELSE
      *                追加のときまたは１行
                       IF    ( SPA-GMN-R01-MAX      =   ZERO         )
                       OR    ( SPA-GMN-R01-MAX      =   1       AND 
                               SPA-GMN-R01-RENNUM   =   1            )
                           IF      SPA-NAI-R01-SRYYM    <   200210
                               MOVE    "1"      TO  SPA-NAI-R01-SYOKBN
                           ELSE
                               IF      SPA-NAI-R01-SRYYM    <   200804
                                   MOVE    "2"  TO  SPA-NAI-R01-SYOKBN
                               ELSE
                                   MOVE    "3"  TO  SPA-NAI-R01-SYOKBN
                               END-IF
                           END-IF
                       ELSE
                           EVALUATE    SPA-NAI-R01-SYOKBN
                               WHEN    "1"
                                   IF      SPA-NAI-R01-SRYYM
                                                           <   200210
                                       CONTINUE
                                   ELSE
                                       IF      SPA-NAI-R01-SRYYM
                                                           >=  200804
                                           MOVE    "0039"  TO  SPA-ERRCD
                                       ELSE 
                                           MOVE    "0007"  TO  SPA-ERRCD
                                       END-IF
                                   END-IF                                  
                               WHEN    "2"
                                   IF      SPA-NAI-R01-SRYYM
                                                           <   200210
                                       MOVE    "0007"      TO  SPA-ERRCD
                                   ELSE
                                       IF      SPA-NAI-R01-SRYYM
                                                           >=  200804
                                           MOVE    "0039"  TO  SPA-ERRCD
                                       END-IF
                                   END-IF
                               WHEN    "3"
                                   IF      SPA-NAI-R01-SRYYM
                                                           <   200804
                                       MOVE    "0039"      TO  SPA-ERRCD
                                   END-IF                                  
                           END-EVALUATE
                       END-IF
                   END-IF                  
               WHEN    "3"             ALSO    "1"
      *            追加のときまたは１行
                   IF    ( SPA-GMN-R01-MAX      =   ZERO         )
                   OR    ( SPA-GMN-R01-MAX      =   1       AND 
                           SPA-GMN-R01-RENNUM   =   1            )
                       IF      SPA-NAI-R01-SRYYM    <   200610
                           MOVE    "1"          TO  SPA-NAI-R01-SYOKBN
                       ELSE
                           MOVE    "2"          TO  SPA-NAI-R01-SYOKBN
                       END-IF
                   ELSE
                       IF      SPA-NAI-R01-SYOKBN   =   "1"
                           IF      SPA-NAI-R01-SRYYM    <   200610
                               CONTINUE
                           ELSE
                               MOVE    "0029"           TO  SPA-ERRCD
                           END-IF                                  
                       ELSE
                           IF      SPA-NAI-R01-SRYYM    >=  200610
                               CONTINUE
                           ELSE
                               MOVE    "0029"           TO  SPA-ERRCD
                           END-IF                                  
                       END-IF
                   END-IF 
           END-EVALUATE              
      *
           IF      SPA-ERRCD            =   SPACE
               CONTINUE
           ELSE
               MOVE    5                    TO  SPA-GMN-R01-CUR
           END-IF             
           .
       4204-SRYYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目重複チェック処理
      *****************************************************************
       4202-INTBL-CHK-SEC                SECTION.
      *
           MOVE    SPACE                   TO  SPA-ERRCD
      * 
           PERFORM VARYING IDX     FROM    1   BY  1
      *************UNTIL   IDX     >       WRK-CONS-LINE-MAX
                   UNTIL   IDX     >       SPA-GMN-R01-MAX
                   OR      SPA-ERRCD   NOT =   SPACE
               IF      IDX         =   IDY 
                   CONTINUE
               ELSE
                   IF    ( SPA-NAI-R01-TPTID     (IDX)
                                           =   SPA-NAI-R01-PTID      )
                   AND   ( SPA-GMN-R01-TSRYYM    (IDX)
                                           =   SPA-GMN-R01-SRYYM     )
                   AND   ( SPA-GMN-R01-TNYUGAIKBN(IDX)
                                           =   SPA-GMN-R01-NYUGAIKBN )
                       MOVE    "0005"              TO  SPA-ERRCD
                       MOVE    WRK-CONS-LINE-MAX   TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD            =   SPACE
               CONTINUE
           ELSE
               MOVE    5                    TO  SPA-GMN-R01-CUR
           END-IF             
           .                   
       4202-INTBL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目編集処理
      *****************************************************************
       4201-INTBL-SET-SEC                SECTION.
      *
           MOVE    SPA-GMN-R01-PTNUM   TO  SPA-GMN-R01-TPTNUM    (IDX)
           MOVE    SPA-NAI-R01-PTID    TO  SPA-NAI-R01-TPTID     (IDX)
           MOVE    SPA-GMN-R01-NAME    TO  SPA-GMN-R01-TNAME     (IDX)
           MOVE    SPA-GMN-R01-SRYYM   TO  SPA-GMN-R01-TSRYYM    (IDX)
           MOVE    SPA-NAI-R01-SRYYM   TO  SPA-NAI-R01-TSRYYM    (IDX)
           MOVE    SPA-GMN-R01-NYUGAIKBN
                                       TO  SPA-GMN-R01-TNYUGAIKBN(IDX)
           IF      SPA-NAI-R01-TSTPTNUMKBN  =   "1"
               MOVE    "テ"            TO  SPA-GMN-R01-TTSTPTNUMKBN(IDX)
           END-IF
           MOVE    SPA-NAI-R01-DELETE  TO  SPA-NAI-R01-TDELETE   (IDX)
           IF      SPA-NAI-R01-DELETE  =   1
               STRING  "［削除］"             DELIMITED  BY  SIZE
                       SPA-GMN-R01-NAME       DELIMITED  BY  SPACE
                                       INTO    SPA-GMN-R01-TNAME (IDX)      
               END-STRING
           END-IF
           MOVE    SPA-NAI-R01-CHK    TO  SPA-NAI-R01-TCHK (IDX)
      * 
           INITIALIZE                  SPA-GMN-R01-KOUMOKU
                                       SPA-NAI-R01-KOUMOKU
           MOVE    ZERO                TO  SPA-NAI-R01-SELNO
      *
          .
       4201-INTBL-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更　処理
      *****************************************************************
       41042-HENKOU-SEC              SECTION.
      *
           PERFORM 4203-KOMOKUCHK-SEC
           IF    ( SPA-ERRCD    NOT =   SPACE )
           OR    ( SPA-RIDCD    NOT =   SPACE )
               GO  TO  41042-HENKOU-EXT
           END-IF    
      *
           PERFORM 4204-SRYYM-CHK-SEC
           IF      SPA-ERRCD    NOT =   SPACE
               GO  TO  41042-HENKOU-EXT
           END-IF    
      * 
           MOVE    SPA-GMN-R01-RENNUM  TO  IDY
      *
           PERFORM 4202-INTBL-CHK-SEC
           IF      SPA-ERRCD        NOT =   SPACE
              GO  TO  41042-HENKOU-EXT
           END-IF
      *
           MOVE    SPA-GMN-R01-RENNUM  TO  IDX
      *
           PERFORM 4201-INTBL-SET-SEC
      *
           MOVE    4                   TO  SPA-GMN-R01-CUR 
      *
          .
       41042-HENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診日（開始）　入力処理
      *****************************************************************
       4108-STJYUSINYMD-SEC            SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-R01-STJYUSINYMD
      *
           IF      SPA-GMN-R01-STJYUSINYMD =   SPACE
               MOVE    "0016"              TO  SPA-ERRCD
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-R01-STJYUSINYMD TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY     TO  SPA-GMN-R01-STJYUSINYMD
                   MOVE    SGDAY-SDAY      TO  SPA-NAI-R01-STJYUSINYMD
               ELSE
                   MOVE    "0016"          TO  SPA-ERRCD
               END-IF
           END-IF
      *             
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-R01-STJYUSINYMD (1:6)
                                   NOT =   SPA-NAI-R01-EDJYUSINYMD (1:6)
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY7-AREA
                   MOVE  "71"              TO  LNK-DAY7-IRAI
                   MOVE  SPA-NAI-R01-STJYUSINYMD (1:6)
                                           TO  LNK-DAY7-YM
                   CALL  "ORCSDAY"         USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                   MOVE    LNK-DAY7-KOYOMI TO  SPA-NAI-R01-EDJYUSINYMD
                   MOVE    SPA-NAI-R01-EDJYUSINYMD
                                           TO  WRK-SYMD  
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG     TO  SPA-GMN-R01-EDJYUSINYMD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    9               TO  SPA-GMN-R01-CUR
           END-IF
      *
           .
       4108-STJYUSINYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診日（終了）　入力処理
      *****************************************************************
       4109-EDJYUSINYMD-SEC            SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-R01-EDJYUSINYMD
      *
           IF      SPA-GMN-R01-EDJYUSINYMD =   SPACE
               MOVE    "0016"              TO  SPA-ERRCD
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-R01-EDJYUSINYMD TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY     TO  SPA-GMN-R01-EDJYUSINYMD
                   MOVE    SGDAY-SDAY      TO  SPA-NAI-R01-EDJYUSINYMD
               ELSE
                   MOVE    "0016"          TO  SPA-ERRCD
               END-IF
           END-IF
      *             
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-R01-STJYUSINYMD (1:6)
                                   NOT =   SPA-NAI-R01-EDJYUSINYMD (1:6)
                   MOVE    "0018"              TO  SPA-ERRCD
               ELSE
                   IF      SPA-NAI-R01-STJYUSINYMD
                                           >   SPA-NAI-R01-EDJYUSINYMD
                       MOVE    "0021"              TO  SPA-ERRCD
                   END-IF
               END-IF 
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    10              TO  SPA-GMN-R01-CUR
           END-IF
      *
           .
       4109-EDJYUSINYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正日入力処理
      *****************************************************************
       4110-TEISEIYM-SEC               SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-R01-CREYMD
                                           SPA-GMN-R01-STJYUSINYMD
                                           SPA-GMN-R01-EDJYUSINYMD
           MOVE    ZERO                TO  SPA-NAI-R01-STJYUSINYMD
                                           SPA-NAI-R01-EDJYUSINYMD
                                           SPA-NAI-R01-TEISEIYM
      *
           IF      SPA-GMN-R01-TEISEIYM =  SPACE
               IF      FLG-TOUROKU     =   1
                   MOVE    "0024"          TO  SPA-ERRCD
               ELSE
                   MOVE    SPA-NAI-R01-HYO-SRYYM
                                           TO  SPA-NAI-R01-TEISEIYM
                   MOVE    SPA-NAI-R01-HYO-SRYYMH
                                           TO  SPA-GMN-R01-TEISEIYM
               END-IF 
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-R01-TEISEIYM    TO  SGDAY-INDAY
               MOVE    "1"                 TO  SGDAY-INTYPE
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN-R01-TEISEIYM
                   MOVE    SGDAY-SDAY (1:6)    TO  SPA-NAI-R01-TEISEIYM
               ELSE
                   MOVE    "0024"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               INITIALIZE                  RECE07-REC
               IF      SPA-R01-RECEKBN     =   "0" OR  "1" OR  "2"
                   MOVE    "HC06"              TO  RECE07-PRTID
               ELSE
                   IF      SPA-R01-RECEKBN     =   "3"
                       MOVE    "HC21"              TO  RECE07-PRTID
                   END-IF
                   IF      SPA-R01-RECEKBN     =   "4" OR  "5" OR  "8"
                       MOVE    "HC23"              TO  RECE07-PRTID
                   END-IF
                   IF      SPA-R01-RECEKBN     =   "6"
                       MOVE    "HC25"              TO  RECE07-PRTID
                   END-IF
               END-IF            
               MOVE    SPA-NAI-R01-TEISEIYM    TO  RECE07-SRYYM
               MOVE    "1"                 TO  RECE07-SYOKBN
               MOVE    SPA-R01-NYUGAIKBN   TO  RECE07-NYUGAIKBN
               MOVE    SPA-R01-RECEKBN     TO  RECE07-RECEKBN
grpsys         MOVE    SPA-HOSPNUM         TO  RECE07-HOSPNUM
               MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
               MOVE    RECE07-REC          TO  MCPDATA-REC
               PERFORM 900-RECE07-INV-SEC
               IF      FLG-RECE07          =   ZERO
                   MOVE    RECE07-CREYMD   TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3
                                           TO  SPA-GMN-R01-CREYMD
      *
                   IF      RECE07-CREYMD       =   SPA-SYSYMD
                       MOVE    "0027"          TO  SPA-ERRCD
                   ELSE
                       INITIALIZE                  STS-AREA-DAY
                       INITIALIZE                  LNK-DAY6-AREA
                       MOVE    "61"            TO  LNK-DAY6-IRAI
                       MOVE    RECE07-CREYMD   TO  LNK-DAY6-KIJUN
                       MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                       MOVE    1               TO  LNK-DAY6-ZOGEN
                       CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN TO
                                               SPA-NAI-R01-STJYUSINYMD
                                               WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    WRK-HENYMDG     TO
                                               SPA-GMN-R01-STJYUSINYMD
      *
                       MOVE    SPA-SYSYMDWH    TO
                                               SPA-GMN-R01-EDJYUSINYMD
                       MOVE    SPA-SYSYMD      TO
                                               SPA-NAI-R01-EDJYUSINYMD
                       IF      RECE07-CREYMD   >   SPA-SYSYMD
                           MOVE    "0028"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               ELSE
                   MOVE    "0025"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    11              TO  SPA-GMN-R01-CUR
           END-IF
      *
           .
       4110-TEISEIYM-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定ボタン押下時チェック処理
      *****************************************************************
       4101-KAKUTEI-CHK-SEC        SECTION.
      *
           EVALUATE    SPA-GMN-R01-SYORI
               WHEN    "1" 
                   IF      SPA-GMN-R01-MAX        =   ZERO
                       MOVE    "0010"             TO  SPA-ERRCD
                       MOVE    4                  TO  SPA-GMN-R01-CUR
                       GO  TO  4101-KAKUTEI-CHK-EXT
                   ELSE 
                       IF      SPA-GMN-R01-MAX    =
                                                  SPA-GMN-R01-DELETE-MAX
                           MOVE    "0010"         TO  SPA-ERRCD
                           MOVE    4              TO  SPA-GMN-R01-CUR
                           GO  TO  4101-KAKUTEI-CHK-EXT
                       END-IF
                  END-IF
           END-EVALUATE        
      *
           .
       4101-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
      *********AND     JOB-TERMID      =   SPA-TERMID
                   MOVE    "6666"          TO  SPA-RIDCD
                   GO  TO  210-BACK-EXT
               END-IF
           END-IF
      *
           PERFORM 210-BACK-SYORI-SEC         
           .
       210-BACK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      *
      *    一時保存テーブル削除処理
           MOVE    1                   TO  FLG-END2
           PERFORM 900-SPATMP-DBDELETE-SEC
      *
           INITIALIZE                  SPA-RID1-AREA
                                       SPA-RIDCD-AREA
           MOVE    ZERO            TO  SPA-R01-JOBCNT       
                                       SPA-R01-STOPFLG    
      *
           MOVE    "R03"               TO  SPA-SAKIPG
           MOVE    "R01"               TO  SPA-MOTOPG
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
      *    削除　処理
      *****************************************************************
       440-SAKUJYO-SEC             SECTION.
      *
           IF      SPA-GMN-R01-RENNUM  =   ZERO
               MOVE    "0004"              TO  SPA-ERRCD
               GO  TO  440-SAKUJYO-EXT
           END-IF
      *
           MOVE    SPA-GMN-R01-RENNUM  TO  IDX
           INITIALIZE              SPA-GMN-R01-TBL-G (IDX)
           INITIALIZE              SPA-NAI-R01-TBL-G (IDX)
      *
           INITIALIZE              WRK-GMN-SORT
           MOVE    SPA-GMN-R01-TBL     TO  WRK-GMN-TBL
           MOVE    SPA-NAI-R01-TBL     TO  WRK-NAI-TBL
           INITIALIZE              SPA-GMN-R01-TBL
           INITIALIZE              SPA-NAI-R01-TBL
           MOVE    ZERO            TO  IDX2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-CONS-LINE-MAX
               IF      WRK-GMN-TRENNUM  (IDX)     =   ZERO
                   CONTINUE
               ELSE
                   ADD     1                   TO  IDX2
                   MOVE    WRK-GMN-TBL-G (IDX) TO
                                               SPA-GMN-R01-TBL-G (IDX2)
                   MOVE    WRK-NAI-TBL-G (IDX) TO
                                               SPA-NAI-R01-TBL-G (IDX2)
                   MOVE    IDX2                TO
                                               SPA-GMN-R01-TRENNUM(IDX2)
               END-IF
           END-PERFORM
           MOVE    IDX2                TO  SPA-GMN-R01-MAX
      *
           IF      SPA-GMN-R01-MAX     =   ZERO
               MOVE    ZERO                TO  FLG-ROW
           ELSE
               COMPUTE IDX     =   SPA-GMN-R01-RENNUM  -   1
               MOVE    IDX                 TO  R01-ROW
           END-IF
      *
           INITIALIZE                  SPA-GMN-R01-KOUMOKU
                                       SPA-NAI-R01-KOUMOKU
           MOVE    ZERO                TO  SPA-NAI-R01-SELNO
      *
           MOVE    4                   TO  SPA-GMN-R01-CUR  
      *
          .
       440-SAKUJYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア　確認処理
      *****************************************************************
       460-CLEAR-RID-SEC             SECTION.
      *
           MOVE    "1003"             TO  SPA-RIDCD
          .
       460-CLEAR-RID-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア　処理
      *****************************************************************
       460-CLEAR-SEC             SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-R01-MAX
                                       SPA-GMN-R01-DELETE-MAX
                                       SPA-NAI-R01-BTPARA-MAX
                                       SPA-NAI-R01-BTPARA-TAISYO-MAX
           INITIALIZE                  SPA-GMN-R01-TBL
                                       SPA-NAI-R01-TBL
                                       SPA-GMN-R01-KOUMOKU
                                       SPA-NAI-R01-KOUMOKU
                                       SPA-GMN-R01-LABEL
                                       SPA-GMN-R01-LABEL1
                                       SPA-GMN-R01-LABEL2
                                       SPA-NAI-R01-BTPARA-AREA
                                       SPA-GMN-R01-BTPARA-AREA
           MOVE    ZERO            TO  SPA-NAI-R01-SELNO
           MOVE    1               TO  SPA-NAI-R01-BTPARA-HYOUJI-FLG
           MOVE    ZERO            TO SPA-NAI-R01-SEIKYU-CHK
      *
           EVALUATE    SPA-GMN-R01-SYORI
               WHEN    "1"
                   MOVE    4               TO  SPA-GMN-R01-CUR
               WHEN    "2"
                   MOVE    "訂正日"        TO  SPA-GMN-R01-LABEL
                   MOVE    "対象診療年月"  TO  SPA-GMN-R01-LABEL1
                   MOVE    "一括作成年月日"
                                           TO  SPA-GMN-R01-LABEL2
                   MOVE    SPACE           TO  SPA-GMN-R01-TEISEIYM
                   PERFORM 4110-TEISEIYM-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    SPACE           TO  SPA-ERRCD
                   END-IF
                   MOVE    11              TO  SPA-GMN-R01-CUR
               WHEN    "3"
                   MOVE    "受診日"        TO  SPA-GMN-R01-LABEL
                   MOVE    9               TO  SPA-GMN-R01-CUR
               WHEN    "4"
               WHEN    "5"
                   MOVE    7               TO  SPA-GMN-R01-CUR
           END-EVALUATE                
      *
          .
       460-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    照会連携表示処理（全体表示・対象表示）
      *****************************************************************
       480-RENKEI-HYOUJI-SEC      SECTION.
      *
      *    照会連携データ表示処理
           EVALUATE    SPA-NAI-R01-BTPARA-HYOUJI-FLG
               WHEN    0
                   MOVE    1           TO  SPA-NAI-R01-BTPARA-HYOUJI-FLG
               WHEN    1
                   MOVE    0           TO  SPA-NAI-R01-BTPARA-HYOUJI-FLG
           END-EVALUATE
      *     
           EVALUATE    SPA-GMN-R01-SYORI
               WHEN    "4"
                   PERFORM 410711-RENKEI-DATA-JYOKEN-SEC
               WHEN    "5"
                   PERFORM 410721-MISEIKYU-DATA-JYOKEN-SEC
           END-EVALUATE
           .
       480-RENKEI-HYOUJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回作成分表示確認処理
      *****************************************************************
       480-ZENKAI-HYOUJI-RID-SEC      SECTION.
      *
           IF      SPA-GMN-R01-SYORI  NOT =   "1"
               GO  TO  480-ZENKAI-HYOUJI-RID-EXT
           END-IF
      *     
      *    レセプト指示コード
           INITIALIZE                  SYS-2001-REC
           MOVE    "2001"              TO  SYS-2001-KANRICD
           MOVE    "*"                 TO  SYS-2001-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-2001-STYUKYMD
                                           SYS-2001-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-2001-HOSPNUM
           MOVE    SYS-2001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2001-REC
               IF      SYS-2001-SYOKBN     =   "1"    
                   MOVE    "0011"                 TO  SPA-ERRCD
                   GO  TO  480-ZENKAI-HYOUJI-RID-EXT
               ELSE
                   IF      SYS-2001-RECEKBN    =   SPA-R01-RECEKBN
                       CONTINUE
                   ELSE     
                       IF    ( SYS-2001-RECEKBN    =   "4" OR  "5" )
                       AND   ( SPA-R01-RECEKBN     =   "4" OR  "5" )
                           CONTINUE
                       ELSE     
                           MOVE    "0012"                 TO  SPA-ERRCD
                           GO  TO  480-ZENKAI-HYOUJI-RID-EXT
                       END-IF
                   END-IF 
                   IF      SYS-2001-PRTKBN     =   "8" OR  "9"
                       IF      SPA-R01-PRTKBN      =   SYS-2001-PRTKBN
                           CONTINUE
                       ELSE     
                           MOVE    "0042"              TO  SPA-ERRCD
                           GO  TO  480-ZENKAI-HYOUJI-RID-EXT
                       END-IF
                   ELSE
                       IF      SPA-R01-PRTKBN      =   "8" OR  "9"
                           MOVE    "0042"              TO  SPA-ERRCD
                           GO  TO  480-ZENKAI-HYOUJI-RID-EXT
                       END-IF
                   END-IF
                   IF      SYS-2001-NYUGAIKBN  =
                                           SPA-R01-NYUGAIKBN
                       CONTINUE
                   ELSE     
                       MOVE    "0013"                 TO  SPA-ERRCD
                       GO  TO  480-ZENKAI-HYOUJI-RID-EXT
                   END-IF    
                   IF      SYS-2001-SYOSBT     =   SPACE   OR  "1"  
                       CONTINUE
                   ELSE     
                       MOVE    "0034"                 TO  SPA-ERRCD
                       GO  TO  480-ZENKAI-HYOUJI-RID-EXT
                   END-IF    
               END-IF
           END-IF
      *
           MOVE    "1004"             TO  SPA-RIDCD
           .
       480-ZENKAI-HYOUJI-RID-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回作成分表示処理
      *****************************************************************
       480-ZENKAI-HYOUJI-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-R01-SELNO
      *
           MOVE    ZERO                TO  SPA-GMN-R01-MAX
                                           SPA-GMN-R01-DELETE-MAX
                                           SPA-NAI-R01-SEIKYU-CHK
           INITIALIZE                      SPA-GMN-R01-TBL
                                           SPA-NAI-R01-TBL
                                           SPA-GMN-R01-KOUMOKU
                                           SPA-NAI-R01-KOUMOKU
      *
           INITIALIZE                      SPA-NAI-R01-2002-TBL
           MOVE    ZERO                TO  IDX
      *
      *        レセプト指示（個別指示）コード読み込み
           INITIALIZE                      SYS-2002-REC
           MOVE    "2002"              TO  SYS-2002-KANRICD
           MOVE    ZERO                TO  SYS-2002-STYUKYMD
                                           SYS-2002-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-2002-HOSPNUM
           MOVE    SYS-2002-REC        TO  MCPDATA-REC
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
           PERFORM             UNTIL       FLG-SYSKANRI    =   1
               MOVE    MCPDATA-REC     TO  SYS-2002-REC
      *
               ADD     1               TO  IDX
               MOVE    SYS-2002-PTID   TO  SPA-NAI-R01-2002-TPTID  (IDX)
               MOVE    SYS-2002-PTNUM  TO  SPA-NAI-R01-2002-TPTNUM (IDX)
               MOVE    SYS-2002-SRYYM  TO  SPA-NAI-R01-2002-TSRYYM (IDX)
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    IDX             TO  SPA-NAI-R01-2002-MAX
      *
      *    請求データ存在チェック
      *****IF      SPA-R01-RECEKBN =   "0" OR  "6"
               MOVE    SPACE               TO  SPA-RIDCD
               PERFORM UNTIL   SPA-NAI-R01-2002-INDEX
                                               >=  SPA-NAI-R01-2002-MAX
                       OR      SPA-RIDCD   NOT =   SPACE
                   PERFORM 4801-ZENKAI-SYSKANRI-CHECK-SEC
               END-PERFORM    
      *
               IF      SPA-RIDCD       NOT =   SPACE
                   GO  TO  480-ZENKAI-HYOUJI-EXT
               END-IF
      *****ELSE
      *****    MOVE    SPACE               TO  SPA-RIDCD
      *****END-IF
      *
      *    前回データ対象編集処理
           PERFORM 4802-ZENKAI-SYSKANRI-HEN-SEC   
          .
       480-ZENKAI-HYOUJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回データ請求データ存在チェック処理
      *****************************************************************
       4801-ZENKAI-SYSKANRI-CHECK-SEC             SECTION.
      *
           ADD     1                TO  SPA-NAI-R01-2002-INDEX
      *
      *    診療会計チェック
           MOVE    SPA-NAI-R01-2002-TPTID  (SPA-NAI-R01-2002-INDEX)
                                    TO  WRK-ACCT-PTID
           MOVE    SPA-NAI-R01-2002-TSRYYM (SPA-NAI-R01-2002-INDEX)
                                    TO  WRK-ACCT-SRYYM
           MOVE    SPA-R01-NYUGAIKBN
                                    TO  WRK-ACCT-NYUGAIKBN
           PERFORM 42031-SRYACCT-CHK-SEC
      *
      *    診療会計が存在しないとき入院会計をチェック（入院のみ）
           IF    ( FLG-ERR             =   ZERO )
           AND   ( SPA-R01-NYUGAIKBN   =   "1"  )
               PERFORM 42032-NYUINACCT-CHK-SEC
           END-IF
      *          
           IF      FLG-ERR             =   1
               MOVE    "0008"              TO  SPA-ERRCD
           ELSE 
               IF      FLG-CHK             =   ZERO
                   MOVE    "0006"              TO  SPA-ERRCD
               ELSE 
                   IF    ( SPA-R01-NYUGAIKBN   =   "1"  )
                   AND   ( FLG-ZAISKBKBN       =   ZERO )
                       MOVE    "0014"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        未請求設定のチェック（医保）
               IF      SPA-R01-RECEKBN     =   "0"
                   INITIALIZE                  SYUKA-REC
                   MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
                   MOVE    WRK-ACCT-PTID   TO  SYUKA-PTID
                   MOVE    WRK-ACCT-SRYYM  TO  SYUKA-SRYYM
                   MOVE    WRK-ACCT-NYUGAIKBN
                                           TO  SYUKA-NYUGAIKBN
                   MOVE    "YYYY"          TO  SYUKA-RECESYUBETU
                   MOVE    SYUKA-REC       TO  MCPDATA-REC
                   MOVE    "tbl_syuka"     TO  WRK-MCP-TABLE
                   MOVE    "key15"         TO  WRK-MCP-PATHNAME
                   PERFORM 900-SYUKA-READ-SEC
                   IF      FLG-SYUKA       =   ZERO
                       IF      SYUKA-MISEIKYU-FLG   =   "01"
      *********************MOVE    "0035"               TO  SPA-ERRCD
                           MOVE    1        TO  SPA-NAI-R01-2002-TTAISYO
                                                (SPA-NAI-R01-2002-INDEX)
                       END-IF
                   END-IF
               END-IF
      *        請求しない設定の存在チェック
               IF      SPA-ERRCD           =   SPACE
                   INITIALIZE                      RECE10-REC
                   MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
                   MOVE    WRK-ACCT-SRYYM      TO  RECE10-SRYYM
                   MOVE    WRK-ACCT-NYUGAIKBN  TO  RECE10-NYUGAIKBN
                   MOVE    WRK-ACCT-PTID       TO  RECE10-PTID
                   PERFORM 900-SEIKYU-SKYKBN-CHECK-SEC
                   IF      FLG-SEIKYU-UMU      =   1
                       MOVE    1                   TO  SPA-NAI-R01-TCHK
                                                (SPA-NAI-R01-2002-INDEX)
                   ELSE
                       MOVE    ZERO                TO  SPA-NAI-R01-TCHK
                                                (SPA-NAI-R01-2002-INDEX)
                   END-IF
               END-IF
           ELSE
      *        請求管理テーブルの存在チェック
               INITIALIZE                  RECE10-REC
               MOVE    SPA-HOSPNUM     TO  RECE10-HOSPNUM
               MOVE    WRK-ACCT-SRYYM
                                       TO  RECE10-SRYYM
               MOVE    WRK-ACCT-NYUGAIKBN
                                       TO  RECE10-NYUGAIKBN
               MOVE    WRK-ACCT-PTID   TO  RECE10-PTID
               PERFORM 900-SEIKYU-READ-SEC
               IF      FLG-SEIKYU-UMU  =   1
                   EVALUATE    SPA-ERRCD
                       WHEN    "0006"
                           MOVE    "1011"      TO  SPA-RIDCD
                       WHEN    "0008"
                           MOVE    "1012"      TO  SPA-RIDCD
                       WHEN    "0014"
                           MOVE    "1013"      TO  SPA-RIDCD
                   END-EVALUATE
               ELSE
                   MOVE    1        TO  SPA-NAI-R01-2002-TTAISYO
                                                (SPA-NAI-R01-2002-INDEX)
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  SPA-ERRCD
           .
       4801-ZENKAI-SYSKANRI-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回データ対象編集処理
      *****************************************************************
       4802-ZENKAI-SYSKANRI-HEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-R01-MAX
                                           SPA-GMN-R01-DELETE-MAX   
           PERFORM     VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       SPA-NAI-R01-2002-MAX
               IF      SPA-NAI-R01-2002-TTAISYO (IDX)
                                       =   1
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-R01-MAX
                   IF      SPA-NAI-R01-2002-TDELETE (IDX)
                                           =   1
                       ADD     1           TO  SPA-GMN-R01-DELETE-MAX
                   END-IF
                   MOVE    SPA-GMN-R01-MAX TO  SPA-GMN-R01-TRENNUM
                                                       (SPA-GMN-R01-MAX)
                   MOVE    SPA-R01-NYUGAIKBN
                                           TO  SPA-GMN-R01-TNYUGAIKBN
                                                       (SPA-GMN-R01-MAX)
      *
                   MOVE    SPA-NAI-R01-2002-TPTID (IDX)
                                           TO  SPA-NAI-R01-TPTID
                                                       (SPA-GMN-R01-MAX)
                   MOVE    SPA-NAI-R01-2002-TPTNUM (IDX)
                                           TO  SPA-GMN-R01-TPTNUM
                                                       (SPA-GMN-R01-MAX)
grpsys             MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
                   MOVE    SPA-NAI-R01-TPTID (SPA-GMN-R01-MAX)
                                           TO  PTINF-PTID
                   PERFORM 900-PTINF-READ-SEC
                   IF      FLG-PTINF       =   ZERO
                       MOVE    PTINF-NAME  TO  SPA-GMN-R01-TNAME
                                                       (SPA-GMN-R01-MAX)
                       IF      PTINF-TSTPTNUMKBN   =   "1"
                           MOVE    "テ"        TO
                                           SPA-GMN-R01-TTSTPTNUMKBN
                                                       (SPA-GMN-R01-MAX)
                       END-IF
                   END-IF
                   MOVE    SPA-NAI-R01-2002-TDELETE (IDX)
                                           TO  SPA-NAI-R01-TDELETE
                                                       (SPA-GMN-R01-MAX)
                   IF      SPA-NAI-R01-2002-TDELETE (IDX) =   1
                       STRING  "［削除］"          DELIMITED  BY  SIZE
                               PTINF-NAME          DELIMITED  BY  SPACE
                                           INTO    SPA-GMN-R01-TNAME
                                                       (SPA-GMN-R01-MAX)   
                       END-STRING
                   END-IF
      *
                   MOVE    SPA-NAI-R01-2002-TSRYYM (IDX)
                                           TO  SPA-NAI-R01-TSRYYM
                                                       (SPA-GMN-R01-MAX)
                   MOVE    SPA-NAI-R01-TSRYYM (SPA-GMN-R01-MAX)
                                           TO  WRK-SYMD(1:6)
                   MOVE    "01"            TO  WRK-SYMD(7:2)
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG(1:6)
                                           TO  SPA-GMN-R01-TSRYYM
                                                       (SPA-GMN-R01-MAX)
               END-IF
           END-PERFORM
      *
      *    １行目のとき
           IF      SPA-GMN-R01-MAX      >   ZERO 
               IF      SPA-NAI-R01-TSRYYM (1)
                                        <   200210
                   MOVE    "1"              TO  SPA-NAI-R01-SYOKBN
               ELSE
                   IF      SPA-NAI-R01-TSRYYM (1)
                                            >=  200804
                       MOVE    "3"              TO  SPA-NAI-R01-SYOKBN
                   ELSE 
                       MOVE    "2"              TO  SPA-NAI-R01-SYOKBN
                   END-IF
               END-IF
           END-IF
      *
           MOVE    1                   TO  FLG-ROW
      *
           MOVE    3                   TO  SPA-GMN-R01-CUR 
      *
          .
       4802-ZENKAI-SYSKANRI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求データ削除処理
      *****************************************************************
       480-DBDELETE-SEC             SECTION.
      *
           PERFORM 4801-DBDELETE-SYORI-SEC
      *
           ADD     1                TO  SPA-GMN-R01-MAX
           IF      SPA-NAI-R01-DELETE
                                    =   1
               ADD     1                TO  SPA-GMN-R01-DELETE-MAX
           END-IF
           MOVE    SPA-GMN-R01-MAX      TO
                                SPA-GMN-R01-TRENNUM   (SPA-GMN-R01-MAX)
           MOVE    SPA-GMN-R01-MAX          TO  IDX
           PERFORM 4201-INTBL-SET-SEC
      *
           MOVE    SPA-GMN-R01-MAX      TO  R01-ROW
           MOVE    4               TO  SPA-GMN-R01-CUR 
           .
      *
       480-DBDELETE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    請求データ削除処理
      *****************************************************************
       4801-DBDELETE-SYORI-SEC         SECTION.
      *
      *    請求管理削除
           INITIALIZE                  RECE10-REC
           MOVE    SPA-HOSPNUM     TO  RECE10-HOSPNUM
           MOVE    WRK-ACCT-SRYYM  TO  RECE10-SRYYM
           MOVE    WRK-ACCT-NYUGAIKBN
                                   TO  RECE10-NYUGAIKBN
           MOVE    WRK-ACCT-PTID   TO  RECE10-PTID
           EVALUATE    SPA-R01-RECEKBN
               WHEN    "0"
                   MOVE    1               TO  RECE10-TEISYUTUSAKI
                   MOVE    2               TO  RECE10-XXTEISYUTUSAKI
               WHEN    "3"
                   MOVE    3               TO  RECE10-TEISYUTUSAKI
                   MOVE    3               TO  RECE10-XXTEISYUTUSAKI 
               WHEN    "4"
               WHEN    "5"
               WHEN    "8"
                   MOVE    4               TO  RECE10-TEISYUTUSAKI
                   MOVE    4               TO  RECE10-XXTEISYUTUSAKI 
                   IF      SPA-R01-RECEKBN =   "8"
                       MOVE    "970"           TO  RECE10-HKNNUM
                   ELSE
                       MOVE    "973"           TO  RECE10-HKNNUM
                   END-IF
               WHEN    "6"
                   MOVE    5               TO  RECE10-TEISYUTUSAKI
                   MOVE    5               TO  RECE10-XXTEISYUTUSAKI 
           END-EVALUATE
           MOVE    RECE10-REC      TO  MCPDATA-REC
           MOVE    "tbl_seikyu"    TO  MCP-TABLE
           EVALUATE    SPA-R01-RECEKBN
               WHEN    "4"
               WHEN    "5"
               WHEN    "8"
                   MOVE    "del10"         TO  MCP-PATHNAME
               WHEN    OTHER
                   MOVE    "del7"          TO  MCP-PATHNAME
           END-EVALUATE
           MOVE    "DBDELETE"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC      NOT =   ZERO
               DISPLAY "SEIKYU KEY=" RECE10-KEY
               MOVE    "1002"          TO  SPA-ERRCD
               GO  TO  4801-DBDELETE-SYORI-EXT
           END-IF
      *
           IF      SPA-R01-RECEKBN =   "0"
               INITIALIZE                  RECE10-REC
               MOVE    SPA-HOSPNUM     TO  RECE10-HOSPNUM
               MOVE    WRK-ACCT-SRYYM  TO  RECE10-SRYYM
               MOVE    WRK-ACCT-NYUGAIKBN
                                       TO  RECE10-NYUGAIKBN
               MOVE    WRK-ACCT-PTID   TO  RECE10-PTID
               MOVE    6               TO  RECE10-TEISYUTUSAKI
               MOVE    6               TO  RECE10-XXTEISYUTUSAKI
               MOVE    RECE10-REC      TO  MCPDATA-REC
               MOVE    "tbl_seikyu"    TO  MCP-TABLE
               MOVE    "del7"          TO  MCP-PATHNAME
               MOVE    "DBDELETE"      TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC      NOT =   ZERO
                   DISPLAY "SEIKYU KEY=" RECE10-KEY
                   MOVE    "1002"          TO  SPA-ERRCD
                   GO  TO  4801-DBDELETE-SYORI-EXT
               END-IF
           END-IF     
      *
           IF      SPA-R01-RECEKBN     =   "0"
      *        公費請求書削除
               INITIALIZE                  KOHSKY-REC
               MOVE    SPA-HOSPNUM     TO  KOHSKY-HOSPNUM
               MOVE    WRK-ACCT-SRYYM  TO  KOHSKY-SRYYM
               MOVE    WRK-ACCT-NYUGAIKBN
                                       TO  KOHSKY-NYUGAIKBN
               MOVE    WRK-ACCT-PTID   TO  KOHSKY-PTID
               MOVE    KOHSKY-REC      TO  MCPDATA-REC
               MOVE    "tbl_kohsky"    TO  MCP-TABLE
               MOVE    "del4"          TO  MCP-PATHNAME
               MOVE    "DBDELETE"      TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC      NOT =   ZERO
                   DISPLAY "KOHKSY KEY=" KOHSKY-KEY
                   MOVE    "1003"          TO  SPA-ERRCD
                   GO  TO  4801-DBDELETE-SYORI-EXT
               END-IF
      *        レセ電削除
               INITIALIZE                  RECEDEN-REC
               MOVE    SPA-HOSPNUM     TO  RECEDEN-HOSPNUM
               MOVE    WRK-ACCT-SRYYM  TO  RECEDEN-SRYYM
               MOVE    WRK-ACCT-NYUGAIKBN
                                       TO  RECEDEN-NYUGAIKBN
               MOVE    WRK-ACCT-PTID   TO  RECEDEN-PTID
               MOVE    1               TO  RECEDEN-TEISYUTUSAKI
               MOVE    RECEDEN-REC     TO  MCPDATA-REC
               MOVE    "tbl_receden"   TO  MCP-TABLE
               MOVE    "del4"          TO  MCP-PATHNAME
               MOVE    "DBDELETE"      TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC      NOT =   ZERO
                   DISPLAY "RECEDEN KEY=" RECEDEN-KEY
                   MOVE    "1004"          TO  SPA-ERRCD
                   GO  TO  4801-DBDELETE-SYORI-EXT
               END-IF
      *
               INITIALIZE                  RECEDEN-REC
               MOVE    SPA-HOSPNUM     TO  RECEDEN-HOSPNUM
               MOVE    WRK-ACCT-SRYYM  TO  RECEDEN-SRYYM
               MOVE    WRK-ACCT-NYUGAIKBN
                                       TO  RECEDEN-NYUGAIKBN
               MOVE    WRK-ACCT-PTID   TO  RECEDEN-PTID
               MOVE    2               TO  RECEDEN-TEISYUTUSAKI
               MOVE    RECEDEN-REC     TO  MCPDATA-REC
               MOVE    "tbl_receden"   TO  MCP-TABLE
               MOVE    "del4"          TO  MCP-PATHNAME
               MOVE    "DBDELETE"      TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC      NOT =   ZERO
                   DISPLAY "RECEDEN KEY=" RECEDEN-KEY
                   MOVE    "1004"          TO  SPA-ERRCD
                   GO  TO  4801-DBDELETE-SYORI-EXT
               END-IF
      *
               INITIALIZE                  RECEDEN-REC
               MOVE    SPA-HOSPNUM     TO  RECEDEN-HOSPNUM
               MOVE    WRK-ACCT-SRYYM  TO  RECEDEN-SRYYM
               MOVE    WRK-ACCT-NYUGAIKBN
                                       TO  RECEDEN-NYUGAIKBN
               MOVE    WRK-ACCT-PTID   TO  RECEDEN-PTID
               MOVE    6               TO  RECEDEN-TEISYUTUSAKI
               MOVE    RECEDEN-REC     TO  MCPDATA-REC
               MOVE    "tbl_receden"   TO  MCP-TABLE
               MOVE    "del4"          TO  MCP-PATHNAME
               MOVE    "DBDELETE"      TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC      NOT =   ZERO
                   DISPLAY "RECEDEN KEY=" RECEDEN-KEY
                   MOVE    "1004"          TO  SPA-ERRCD
                   GO  TO  4801-DBDELETE-SYORI-EXT
               END-IF
           END-IF     
      *
           IF    ( SPA-R01-RECEKBN     =   "3"      )
           AND   ( WRK-ACCT-SRYYM      >=  "201307" )
      *        レセ電削除
               INITIALIZE                  RECEDEN-REC
               MOVE    SPA-HOSPNUM     TO  RECEDEN-HOSPNUM
               MOVE    WRK-ACCT-SRYYM  TO  RECEDEN-SRYYM
               MOVE    WRK-ACCT-NYUGAIKBN
                                       TO  RECEDEN-NYUGAIKBN
               MOVE    WRK-ACCT-PTID   TO  RECEDEN-PTID
               MOVE    3               TO  RECEDEN-TEISYUTUSAKI
               MOVE    RECEDEN-REC     TO  MCPDATA-REC
               MOVE    "tbl_receden"   TO  MCP-TABLE
               MOVE    "del4"          TO  MCP-PATHNAME
               MOVE    "DBDELETE"      TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC      NOT =   ZERO
                   DISPLAY "RECEDEN KEY=" RECEDEN-KEY
                   MOVE    "1004"          TO  SPA-ERRCD
                   GO  TO  4801-DBDELETE-SYORI-EXT
               END-IF
           END-IF
           .
      *
       4801-DBDELETE-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    作表処理
      *****************************************************************
       450-TOUROKU-SEC                SECTION.
      *
      *    処理区分チェック
           IF      SPA-GMN-R01-SYORIMEI    =   SPACE
               MOVE    8                   TO  SPA-GMN-R01-CUR
               MOVE    "0003"              TO  SPA-ERRCD
               GO  TO  450-TOUROKU-EXT
           END-IF
      *
           IF      SPA-GMN-R01-SYORI       =   "1"
               IF    ( SPA-GMN-R01-RENNUM      >   ZERO  )
               OR    ( SPA-GMN-R01-PTNUM   NOT =   SPACE )
               OR    ( SPA-GMN-R01-SRYYM   NOT =   SPACE )
                   MOVE    "0022"              TO  SPA-ERRCD
                   GO  TO  450-TOUROKU-EXT
               END-IF
           END-IF
      *
           EVALUATE    SPA-GMN-R01-SYORI
               WHEN    "4"
                   IF      SPA-NAI-R01-BTPARA-MAX  =   ZERO
                       MOVE    "0031"              TO  SPA-ERRCD
                       MOVE    8                   TO  SPA-GMN-R01-CUR
                       GO  TO  450-TOUROKU-EXT
                   END-IF
                   IF      SPA-NAI-R01-BTPARA-TAISYO-MAX
                                               =   ZERO
                       MOVE    "0030"              TO  SPA-ERRCD
                       MOVE    8                   TO  SPA-GMN-R01-CUR
                       GO  TO  450-TOUROKU-EXT
                   END-IF
               WHEN    "5"
                   IF      SPA-NAI-R01-BTPARA-MAX  =   ZERO
                       MOVE    "0033"              TO  SPA-ERRCD
                       MOVE    8                   TO  SPA-GMN-R01-CUR
                       GO  TO  450-TOUROKU-EXT
                   END-IF
      *******      IF      SPA-NAI-R01-BTPARA-TAISYO-MAX
      *******                                  =   ZERO
      *******          MOVE    "0032"              TO  SPA-ERRCD
      *******          MOVE    8                   TO  SPA-GMN-R01-CUR
      *******          GO  TO  450-TOUROKU-EXT
      *******      END-IF
           END-EVALUATE
      *
      *    入力チェック
           MOVE    1               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  450-TOUROKU-EXT
           END-IF
      *
      *    ジョブ管理チェック処理
           PERFORM 4501-JOBKANRI-CHK-SEC
           IF      SPA-ERRCD      NOT =   SPACE
               GO  TO  450-TOUROKU-EXT
           END-IF    
      *
           EVALUATE    SPA-GMN-R01-SYORI
               WHEN    "1" 
                   MOVE    "1002"             TO  SPA-RIDCD
      *
                   MOVE    ZERO               TO  SPA-NAI-R01-SEIKYU-CHK
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       SPA-GMN-R01-MAX
                           OR      SPA-NAI-R01-SEIKYU-CHK
                                           =       1
                       IF      SPA-NAI-R01-TCHK (IDX)  =   1
                           MOVE    1       TO  SPA-NAI-R01-SEIKYU-CHK
                       END-IF
                   END-PERFORM
               WHEN    "4" 
                   MOVE    "1002"             TO  SPA-RIDCD
               WHEN    "5" 
                   IF      SPA-NAI-R01-BTPARA-TAISYO-MAX
                                               =   ZERO
                       MOVE    "1001"             TO  SPA-RIDCD
                   ELSE
                       MOVE    "1002"             TO  SPA-RIDCD
                   END-IF
               WHEN    "2"
               WHEN    "3"
                   MOVE    ZERO               TO  SPA-NAI-R01-SEIKYU-CHK
                   MOVE    ZERO               TO  WRK-KENSU
                   MOVE    SPACE              TO  WRK-KENSU-X 
      *
                   INITIALIZE                     PTINF-REC
grpsys             MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                   MOVE    SPA-NAI-R01-STJYUSINYMD
                                              TO  PTINF-ST-SRYYMD    
                   MOVE    SPA-NAI-R01-EDJYUSINYMD
                                              TO  PTINF-ED-SRYYMD
                   MOVE    SPA-R01-NYUGAIKBN  TO  PTINF-NYUGAIKBN
                   EVALUATE    SPA-R01-RECEKBN
                       WHEN    "0"
                           MOVE    ZERO           TO  PTINF-HKNKBN
                           MOVE    "7"            TO  PTINF-HKNKBN2
                       WHEN    "3"
                           MOVE    "1"            TO  PTINF-HKNKBN
                           MOVE    "5"            TO  PTINF-HKNKBN2
                       WHEN    "4"
                           MOVE    "2"            TO  PTINF-HKNKBN
                       WHEN    "5"
                           MOVE    "2"            TO  PTINF-HKNKBN
                           MOVE    "4"            TO  PTINF-HKNKBN2
                       WHEN    "6"
                           MOVE    "6"            TO  PTINF-HKNKBN
                       WHEN    "8"
                           MOVE    "7"            TO  PTINF-HKNKBN
                   END-EVALUATE
                   IF      SPA-GMN-R01-SYORI       =   "2"
                       MOVE    SPA-NAI-R01-TEISEIYM
                                                   TO  PTINF-SRYYM
                       MOVE    SPA-NAI-R01-TEISEIYM
                                                   TO  PTINF-SRYYMD(1:6)
                       MOVE    '__'                TO  PTINF-SRYYMD(7:2)
                   END-IF 
                   MOVE    PTINF-REC           TO  MCPDATA-REC
                   EVALUATE    SPA-R01-RECEKBN
                       WHEN    "0"
                           IF      SPA-GMN-R01-SYORI   =   "2"
                               MOVE    "tbl_ptinf" TO  WRK-MCP-TABLE
                               MOVE    "key13"     TO  WRK-MCP-PATHNAME
                           ELSE
                               MOVE    "tbl_ptinf" TO  WRK-MCP-TABLE
                               MOVE    "key16"     TO  WRK-MCP-PATHNAME
                           END-IF
                       WHEN    "3"
                       WHEN    "5"
                           IF      SPA-GMN-R01-SYORI   =   "2"
                               MOVE    "tbl_ptinf" TO  WRK-MCP-TABLE
                               MOVE    "key18"     TO  WRK-MCP-PATHNAME
                           ELSE
                               MOVE    "tbl_ptinf" TO  WRK-MCP-TABLE
                               MOVE    "key16"     TO  WRK-MCP-PATHNAME
                           END-IF
                       WHEN    OTHER
                           IF      SPA-GMN-R01-SYORI   =   "2"
                               MOVE    "tbl_ptinf" TO  WRK-MCP-TABLE
                               MOVE    "key13"     TO  WRK-MCP-PATHNAME
                           ELSE
                               MOVE    "tbl_ptinf" TO  WRK-MCP-TABLE
                               MOVE    "key11"     TO  WRK-MCP-PATHNAME
                           END-IF
                   END-EVALUATE
                   MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys             PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                       MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                       PERFORM 900-PTINF-READ-N-SEC
                   ELSE
                       MOVE    1                  TO  FLG-PTINF
                   END-IF
                   PERFORM                 UNTIL   FLG-PTINF   =   1
                       ADD     1                   TO  WRK-KENSU
      *
      *                請求しない設定の存在チェック
                       INITIALIZE                      RECE10-REC
                       MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
                       IF      SPA-GMN-R01-SYORI       =   "2"
                           MOVE    SPA-NAI-R01-TEISEIYM
                                                       TO  RECE10-SRYYM
                       ELSE
                           MOVE    SPA-NAI-R01-STJYUSINYMD (1:6)
                                                       TO  RECE10-SRYYM
                       END-IF
                       MOVE    SPA-R01-NYUGAIKBN   TO  RECE10-NYUGAIKBN
                       MOVE    PTINF-PTID          TO  RECE10-PTID
                       PERFORM 900-SEIKYU-SKYKBN-CHECK-SEC
                       IF      FLG-SEIKYU-UMU      =   1
                           MOVE    1               TO
                                                SPA-NAI-R01-SEIKYU-CHK
                       END-IF
      *
                       MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                       MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                       PERFORM 900-PTINF-READ-N-SEC
                   END-PERFORM
                   MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
      *
                   IF      WRK-KENSU           =   ZERO
                       IF      SPA-GMN-R01-SYORI       =   "2"
                           MOVE    "0026"          TO  SPA-ERRCD
                       ELSE
                           MOVE    "0019"          TO  SPA-ERRCD
                       END-IF
                   ELSE
                       MOVE    WRK-KENSU       TO  WRK-KENSUZ
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    WRK-KENSUZ      TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING ORCSKANACHKAREA
                       IF    ( KANACHK-RC      =   ZERO )
                       AND   ( KANACHK-SYUBETU =   "2"  )
                           MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                                   TO  WRK-KENSU-X
                           IF      SPA-GMN-R01-SYORI   =   "2"
                               MOVE    "1007"          TO  SPA-RIDCD
                           ELSE
                               MOVE    "1006"          TO  SPA-RIDCD
                           END-IF
                       END-IF 
                   END-IF
           END-EVALUATE
      *
           MOVE    ZERO                TO  SPA-NAI-R01-PRTKBN-FLG
           IF    ( SPA-ERRCD           =   SPACE       )
           AND   ( SPA-R01-PRTKBN      =   "2" OR  "3"
                                       OR  "4" OR  "5" )
               EVALUATE    SPA-GMN-R01-SYORI
                   WHEN    "1"
                   WHEN    "4"
                   WHEN    "5"
                   WHEN    "6"
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       SPA-GMN-R01-MAX
                               OR      SPA-NAI-R01-PRTKBN-FLG
                                           =       1
                           IF    ( SPA-GMN-R01-SYORI
                                                   =   "4" OR  "5" )
                           AND   ( SPA-NAI-R01-TTAISYO (IDX)
                                                   =   0           )
                               CONTINUE
                           ELSE  
                               IF   ( SPA-NAI-R01-TDELETE (IDX)
                                                   =   1           )
                                   CONTINUE
                               ELSE
                                   IF      SPA-NAI-R01-TSRYYM (IDX)
                                                       <   200804
                                       MOVE    1       TO
                                               SPA-NAI-R01-PRTKBN-FLG
                                   END-IF  
                               END-IF  
                           END-IF  
                       END-PERFORM  
                   WHEN    "2"
                       IF      SPA-NAI-R01-TEISEIYM    <   200804
                           MOVE    1       TO  SPA-NAI-R01-PRTKBN-FLG
                       END-IF  
                   WHEN    "3"
                       IF      SPA-NAI-R01-STJYUSINYMD (1:6)
                                                       <   "200804"
                           MOVE    1       TO  SPA-NAI-R01-PRTKBN-FLG
                       END-IF  
               END-EVALUATE
           END-IF
      *
      *    請求しない設定ありのとき
           EVALUATE    SPA-RIDCD
               WHEN    "1002"
               WHEN    "1006"
               WHEN    "1007"
                   IF     SPA-NAI-R01-SEIKYU-CHK   =   1
                       MOVE    SPA-RIDCD       TO  SPA-RIDCD4
                       MOVE    SPACE           TO  SPA-RIDCD
                   END-IF
           END-EVALUATE
           .
       450-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       4501-JOBKANRI-CHK-SEC       SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRCD
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
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
      *
      *    入院の処理のとき定期請求のチェックをする
           IF    ( SPA-ERRCD       =   SPACE )
           AND   ( SPA-R01-NYUGAIKBN   
                                   =   "1"   ) 
               INITIALIZE                  STEIKIJOB-AREA
               MOVE    "1"             TO  STEIKIJOB-RECEKBN
               MOVE    SPA-R01-NYUGAIKBN
                                       TO  STEIKIJOB-NYUGAIKBN
               MOVE    "2"             TO  STEIKIJOB-SYOKBN
      *
               CALL    "ORCSTEIKIJOB"      USING   STEIKIJOB-AREA
                                                   SPA-AREA
               IF    ( STEIKIJOB-ERRCD
                                   NOT =   SPACE )
                   MOVE    STEIKIJOB-ERRCD     TO  SPA-ERRCD
                   MOVE    STEIKIJOB-ERRMSG    TO  WRK-RECEERR
               END-IF
           END-IF
           .
       4501-JOBKANRI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    再印刷チェック処理
      *****************************************************************
       4501-REPRINT-CHK-SEC         SECTION.
      *
           INITIALIZE              SPA-RECE07-REC
      *
           INITIALIZE                  RECE07-REC
grpsys     MOVE    SPA-HOSPNUM         TO  RECE07-HOSPNUM
           IF      SPA-R01-RECEKBN     =   "0" OR  "1" OR  "2"
               MOVE    "HC06"              TO  RECE07-PRTID
           ELSE
               IF      SPA-R01-RECEKBN     =   "3"
                   MOVE    "HC21"              TO  RECE07-PRTID
               END-IF
               IF      SPA-R01-RECEKBN     =   "4" OR  "5" OR  "8"
                   MOVE    "HC23"              TO  RECE07-PRTID
               END-IF
               IF      SPA-R01-RECEKBN     =   "6"
                   MOVE    "HC25"              TO  RECE07-PRTID
               END-IF
           END-IF            
           IF      SPA-R01-FILEKBN         =   "1"
               MOVE    SPA-R01-SRYYM       TO  RECE07-SRYYM
               MOVE    "1"                 TO  RECE07-SYOKBN
               MOVE    SPA-R01-NYUGAIKBN   TO  RECE07-NYUGAIKBN
               MOVE    SPA-R01-RECEKBN     TO  RECE07-RECEKBN
               MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "2"                 TO  RECE07-SYOKBN
               MOVE    SPA-R01-NYUGAIKBN   TO  RECE07-NYUGAIKBN
               MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
               MOVE    "key5"              TO  WRK-MCP-PATHNAME
           END-IF
           MOVE    RECE07-REC          TO  MCPDATA-REC
           PERFORM 900-RECE07-INV-SEC
           IF      FLG-RECE07          =   ZERO
               MOVE    RECE07-REC          TO  SPA-RECE07-REC
               MOVE    "99999999"          TO  SPA-RECE07-HKNJANUM 
               MOVE    "00"                TO  SPA-RECE07-SRYKA 
                                               SPA-RECE07-SRYKA-KOKUHO
                                               SPA-RECE07-SRYKA-KOUIKI
               MOVE    ZERO                TO  SPA-RECE07-FUMEI-PAGE
               INITIALIZE                  SPA-RECE07-HKNJANUM-SYAHO-T
               MOVE    "%%%%%%%%"      TO  SPA-RECE07-HKNJANUM-SYAHO (1)
               MOVE    1               TO  SPA-RECE07-HKNJANUM-SYAHO-MAX
               MOVE    "9"             TO  SPA-RECE07-HKNJANUM-SRT-SYAHO
      *
      *        一括作成のとき主科設定の有無チェック
               IF    ( SPA-SYUKA-KBN           =   "1"         )
                   MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
                   MOVE    "key11"             TO  WRK-MCP-PATHNAME
                   MOVE    "1"                 TO  RECE07-TEISYUTUSAKI
                   MOVE    RECE07-REC          TO  MCPDATA-REC
                   PERFORM 900-RECE07-INV-SEC
                   IF      FLG-RECE07          =   ZERO
                       MOVE    RECE07-SRYKA    TO  SPA-RECE07-SRYKA
                   END-IF
                   MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
                   MOVE    "key11"             TO  WRK-MCP-PATHNAME
                   MOVE    "2"                 TO  RECE07-TEISYUTUSAKI
                   MOVE    RECE07-REC          TO  MCPDATA-REC
                   PERFORM 900-RECE07-INV-SEC
                   IF      FLG-RECE07          =   ZERO
                       MOVE    RECE07-SRYKA    TO
                                               SPA-RECE07-SRYKA-KOKUHO
                   END-IF
                   MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
                   MOVE    "key11"             TO  WRK-MCP-PATHNAME
                   MOVE    "6"                 TO  RECE07-TEISYUTUSAKI
                   MOVE    RECE07-REC          TO  MCPDATA-REC
                   PERFORM 900-RECE07-INV-SEC
                   IF      FLG-RECE07          =   ZERO
                       MOVE    RECE07-SRYKA    TO
                                               SPA-RECE07-SRYKA-KOUIKI
                   END-IF
               END-IF  
      *
      *        不明分の有無チェック
               MOVE    SPA-RECE07-KEY      TO  RECE07-KEY
               MOVE    "%"                 TO  RECE07-TEISYUTUSAKI
               MOVE    "0000"              TO  RECE07-RECESYUBETU
               MOVE    "%%"                TO  RECE07-SRYKA 
               MOVE    RECE07-REC          TO  MCPDATA-REC                             
               MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
               MOVE    "key13"             TO  WRK-MCP-PATHNAME
               MOVE    RECE07-REC          TO  MCPDATA-REC
               PERFORM 900-RECE07-INV-SEC
               IF      FLG-RECE07          =   ZERO
                   MOVE    RECE07-PAGE         TO  SPA-RECE07-FUMEI-PAGE
               ELSE
                   MOVE    ZERO                TO  SPA-RECE07-FUMEI-PAGE
               END-IF
           END-IF
      *
           .
       4501-REPRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       450-KOUSHIN-SEC                 SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    フラグ初期化
           MOVE    ZERO            TO  SPA-R01-JOBCNT       
                                       SPA-R01-STOPFLG    
           MOVE    SPACE           TO  SPA-R99-CANCEL
      *
      *    点検用レセプトで平成２００８年３月以前のとき
           IF    ( SPA-NAI-R01-PRTKBN-FLG  =   1           )
           AND   ( SPA-R01-PRTKBN          =   "2" OR  "3"
                                           OR  "4" OR  "5" )
               MOVE    "1"                     TO  SPA-R01-PRTKBN
           END-IF
           MOVE    ZERO           TO  SPA-NAI-R01-PRTKBN-FLG
      *
      *    レセプト指示更新
           INITIALIZE                  SYS-2001-REC
           MOVE    "2001"              TO  SYS-2001-KANRICD
           MOVE    "*"                 TO  SYS-2001-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-2001-STYUKYMD
                                           SYS-2001-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-2001-HOSPNUM
           MOVE    SYS-2001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2001-REC
           ELSE
               INITIALIZE                      SYS-2001-REC
               MOVE    "2001"              TO  SYS-2001-KANRICD
               MOVE    "*"                 TO  SYS-2001-KBNCD
               MOVE    ALL ZERO            TO  SYS-2001-STYUKYMD
               MOVE    ALL "9"             TO  SYS-2001-EDYUKYMD
grpsys         MOVE    SPA-HOSPNUM         TO  SYS-2001-HOSPNUM
           END-IF 
           INITIALIZE                      SYS-2001-TBL 
           MOVE    "2"                 TO  SYS-2001-SYOKBN
           MOVE    "000000"            TO  SYS-2001-SRYYM
           MOVE    SPA-R01-RECEKBN     TO  SYS-2001-RECEKBN
           MOVE    SPA-R01-NYUGAIKBN   TO  SYS-2001-NYUGAIKBN
           MOVE    SPA-GMN-R01-SYORI   TO  SYS-2001-SYOSBT 
           IF      SPA-R01-PRTKBN      =   "1"
               MOVE    SPACE               TO  SYS-2001-PRTKBN
           ELSE
               IF      SPA-R01-PRTKBN      =   "8" OR  "9"
                   MOVE    SPA-R01-PRTKBN      TO  SYS-2001-PRTKBN
               ELSE
                   MOVE    "1"                 TO  SYS-2001-PRTKBN
               END-IF
           END-IF
           MOVE    SPA-TERMID          TO  SYS-2001-TERMID
           MOVE    SPA-OPID            TO  SYS-2001-OPID
           MOVE    SMCNDATE-YMD        TO  SYS-2001-CREYMD
           MOVE    SMCNDATE-YMD        TO  SYS-2001-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYS-2001-UPHMS
           MOVE    SYS-2001-REC        TO  MCPDATA-REC
           IF      FLG-SYSKANRI        =   1
               MOVE    "DBINSERT"          TO  MCP-FUNC
           ELSE
              MOVE    "DBUPDATE"          TO  MCP-FUNC
           END-IF
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1003"              TO  SPA-ERRCD
               DISPLAY "2001 SYSKANRI-KEY =>" SYS-2001-KEY 
               GO  TO  450-KOUSHIN-EXT
           END-IF
      *
           IF      SPA-GMN-R01-SYORI       =   "1" OR  "4" OR  "5"
      *        個別指示削除処理
               INITIALIZE                  SYS-2002-REC
               MOVE    "2002"              TO  SYS-2002-KANRICD
               MOVE    "001"               TO  SYS-2002-KBNCD
               MOVE    SPA-SYSYMD          TO  SYS-2002-STYUKYMD
                                               SYS-2002-EDYUKYMD
grpsys         MOVE    SPA-HOSPNUM         TO  SYS-2002-HOSPNUM
               MOVE    SYS-2002-REC        TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   1
                   INITIALIZE                  SYS-2002-REC
                   MOVE    "2002"              TO  SYS-2002-KANRICD
                   MOVE    "0001"              TO  SYS-2002-KBNCD
                   MOVE    SPA-SYSYMD          TO  SYS-2002-STYUKYMD
                                                   SYS-2002-EDYUKYMD
grpsys             MOVE    SPA-HOSPNUM         TO  SYS-2002-HOSPNUM
                   MOVE    SYS-2002-REC        TO  MCPDATA-REC
                   PERFORM 900-SYSKANRI-INV-SEC
               END-IF
               IF      FLG-SYSKANRI        =   ZERO
                   INITIALIZE                      SYS-2002-REC
                   MOVE    "2002"                  TO  SYS-2002-KANRICD
                   MOVE    SPA-SYSYMD              TO  SYS-2002-STYUKYMD
                                                       SYS-2002-EDYUKYMD
grpsys             MOVE    SPA-HOSPNUM             TO  SYS-2002-HOSPNUM
                   MOVE    SYS-2002-REC            TO  MCPDATA-REC
                   MOVE    "DBDELETE"              TO  MCP-FUNC
                   MOVE    "tbl_syskanri"          TO  MCP-TABLE
                   MOVE    "del11"                 TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
      *
                   IF      MCP-RC              NOT =   ZERO 
                       MOVE    "1001"                  TO  SPA-ERRCD
                       DISPLAY "SYSKANRI-KEY =>" SYS-2002-KEY 
                       GO  TO  450-KOUSHIN-EXT
                   END-IF
               END-IF
      *
      *        個別指示登録処理
               MOVE    ZERO                TO  WRK-KBNCD-9
               MOVE    SPA-NAI-R01-TSRYYM (1)  TO  WRK-SRYYM
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-R01-MAX
                 IF    ( SPA-GMN-R01-SYORI     =   "4" OR  "5" )
                 AND   ( SPA-NAI-R01-TTAISYO (IDX)
                                               =   0           )
                    CONTINUE
                 ELSE  
                  IF   ( SPA-NAI-R01-TDELETE (IDX)
                                               =   1           )
                      CONTINUE
                  ELSE
                   IF      WRK-SRYYM       <   SPA-NAI-R01-TSRYYM(IDX)
                       MOVE    SPA-NAI-R01-TSRYYM(IDX)  TO  WRK-SRYYM
                   END-IF
      * 
                   INITIALIZE                  SYS-2002-REC
                   MOVE    "2002"              TO  SYS-2002-KANRICD
      *************MOVE    IDX (2:3)           TO  SYS-2002-KBNCD
                   ADD     1                   TO  WRK-KBNCD-9
                   MOVE    WRK-KBNCD-X         TO  SYS-2002-KBNCD 
                   MOVE    ZERO                TO  SYS-2002-STYUKYMD
                   MOVE    ALL "9"             TO  SYS-2002-EDYUKYMD
                   MOVE    SPA-GMN-R01-TPTNUM(IDX) TO  SYS-2002-PTNUM
                   MOVE    SPA-NAI-R01-TPTID (IDX) TO  SYS-2002-PTID
                   MOVE    SPA-NAI-R01-TSRYYM(IDX) TO  SYS-2002-SRYYM
      *************MOVE    SPA-GMN-R01-TNYUGAIKBN(IDX)
                   MOVE    SPA-R01-NYUGAIKBN   TO  SYS-2002-NYUGAIKBN
                   MOVE    SPA-TERMID          TO  SYS-2002-TERMID
                   MOVE    SPA-OPID            TO  SYS-2002-OPID
                   MOVE    SMCNDATE-YMD        TO  SYS-2002-CREYMD
                   MOVE    SMCNDATE-YMD        TO  SYS-2002-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SYS-2002-UPHMS
grpsys             MOVE    SPA-HOSPNUM         TO  SYS-2002-HOSPNUM
                   MOVE    SYS-2002-REC        TO  MCPDATA-REC
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE    "tbl_syskanri"          TO  MCP-TABLE
                   MOVE    "key"                   TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
      *
                   IF      MCP-RC          NOT =   ZERO 
                       MOVE    "1002"              TO  SPA-ERRCD
                       DISPLAY "2002 SYSKANRI-KEY =>" SYS-2002-KEY 
                       GO  TO  450-KOUSHIN-EXT
                   END-IF
                  END-IF
                 END-IF
               END-PERFORM
           END-IF
      *
      *    未請求解除データで診療なしのデータの更新処理
           IF      SPA-GMN-R01-SYORI     =   "5"
               PERFORM 4502-KAIJYO-DATA-KOUSIN-SEC
               IF      SPA-ERRCD       NOT =   SPACE 
                   GO  TO  450-KOUSHIN-EXT
               END-IF
           END-IF
      *
      *    パラメタＤＢ更新
           INITIALIZE                  BTPARA-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  BTPARA-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"      TO  MCP-FUNC
           MOVE    "tbl_btpara"    TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           INITIALIZE                      BTPARA-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  BTPARA-SHELLID
           MOVE    WRK-CONS-JOB-JOBID  TO  BTPARA-JOBID
           EVALUATE    SPA-GMN-R01-SYORI
               WHEN    "1"
                   MOVE    "01"                TO  BTPARA-INFO-KBN
               WHEN    "2"
                   MOVE    "02"                TO  BTPARA-INFO-KBN
                   MOVE    SPA-NAI-R01-STJYUSINYMD
                                               TO  BTPARA-INFO-PARA(1:)
                   MOVE    SPA-NAI-R01-EDJYUSINYMD
                                               TO  BTPARA-INFO-PARA(9:)
                   MOVE    SPA-NAI-R01-TEISEIYM
                                               TO  BTPARA-INFO-PARA(17:)
               WHEN    "3"
                   MOVE    "03"                TO  BTPARA-INFO-KBN
                   MOVE    SPA-NAI-R01-STJYUSINYMD
                                               TO  BTPARA-INFO-PARA(1:)
                   MOVE    SPA-NAI-R01-EDJYUSINYMD
                                               TO  BTPARA-INFO-PARA(9:)
               WHEN    "4"
                   MOVE    "04"                TO  BTPARA-INFO-KBN
                   MOVE    SPA-NAI-R01-BTPARA-QTORMAIN-AR
                                               TO  QTORMAIN-AREA
                   MOVE    QTORM-SRYYM         TO  BTPARA-INFO-PARA(1:)
               WHEN    "5"
                   MOVE    "05"                TO  BTPARA-INFO-KBN
           END-EVALUATE
           MOVE    SPA-TERMID          TO  BTPARA-TERMID
           MOVE    SPA-OPID            TO  BTPARA-OPID
           MOVE    SMCNDATE-YMD        TO  BTPARA-CREYMD
           MOVE    SMCNDATE-YMD        TO  BTPARA-UPYMD
           MOVE    SMCNDATE-HMS        TO  BTPARA-UPHMS
grpsys     MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC      NOT =   ZERO
               MOVE    "1004"          TO  SPA-ERRCD
               GO  TO  450-KOUSHIN-EXT
           END-IF
      *
      *    ジョブ開始セット
           MOVE    "DEL"               TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID  TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           MOVE    "JBS"               TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID  TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  JOB-SHELLID
           MOVE    SPA-R01-NYUGAIKBN   TO  JOB-NYUGAIKBN
           MOVE    "2"                 TO  JOB-SYOKBN
           MOVE    SPA-R01-SRYYM       TO  JOB-SRYYM
           MOVE    SPA-TERMID          TO  JOB-TERMID
           MOVE    SPA-R01-JOB-KARTE-UID
                                       TO  JOB-KARTE-UID
           MOVE    SPA-R01-JOB-ORCA-UID
                                       TO  JOB-ORCA-UID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
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
      *    県番号の取得
           MOVE    ZERO                TO  WRK-PREFNUM
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1001"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               PERFORM  UNTIL   FLG-SYSKANRI   =    1
                   MOVE    MCPDATA-REC   TO  SYS-1001-REC
                   EVALUATE    SYS-1001-PREFNUM
                       WHEN    "40"
                       WHEN    "06"
                           MOVE    SYS-1001-PREFNUM    TO  WRK-PREFNUM
                           MOVE    1                   TO  FLG-SYSKANRI
                   END-EVALUATE    
      *
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
      *
               END-PERFORM
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    カスタマイズＰＧ・レセ電情報の設定
           INITIALIZE                  SYS-2005-REC
           MOVE    "2005"          TO  SYS-2005-KANRICD
           MOVE    "*"             TO  SYS-2005-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-2005-STYUKYMD
                                       SYS-2005-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-2005-HOSPNUM
           MOVE    SYS-2005-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE   MCPDATA-REC  TO  SYS-2005-REC
           ELSE
               MOVE   SPACE        TO  SYS-2005-REC
           END-IF
      *
      *    区分コード０１のデータ読み込み
           INITIALIZE                  SYS-25-REC
           MOVE    "2005"          TO  SYS-25-KANRICD
           MOVE    "01"            TO  SYS-25-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-25-STYUKYMD
                                       SYS-25-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-25-HOSPNUM
           MOVE    SYS-25-REC      TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE   MCPDATA-REC  TO  SYS-25-REC
           ELSE
               INITIALIZE              SYS-25-REC
               MOVE    SYS-2005-TERMID (1:4)
                                   TO  SYS-25-TBL (1:4)
           END-IF
      *
           IF      SYS-2005-PREFPRGID (1) (1:1)
                                   NOT =  "S"
                   MOVE  "NOPG"    TO  SYS-2005-PREFPRGID (1)
           END-IF
           IF      SYS-2005-PREFPRGID (2) (1:1)
                                   NOT =  "S"
                   MOVE  "NOPG"    TO  SYS-2005-PREFPRGID (2)
           END-IF
           IF      SYS-2005-KKHRECEKBN =   SPACE
               MOVE    ZERO            TO  SYS-2005-KKHRECEKBN      
           END-IF
           IF      SYS-2005-SYARECEKBN =   SPACE
               MOVE    ZERO            TO  SYS-2005-SYARECEKBN      
           END-IF
           IF      SYS-2005-FRAME-JIBAIPRTKBN
                                       =   SPACE
               MOVE    ZERO            TO  SYS-2005-FRAME-JIBAIPRTKBN
           END-IF
           IF      SYS-2005-FRAME-JIBAIPRTKBN-N
                                       =   SPACE
               MOVE    ZERO            TO  SYS-2005-FRAME-JIBAIPRTKBN-N
           END-IF
           IF      SYS-25-JIBAIPRTKBN
                                       =   SPACE   OR  "0"
               MOVE    "1"             TO  SYS-25-JIBAIPRTKBN
           END-IF
           IF      SYS-25-JIBAIPRTKBN-N
                                       =   SPACE   OR  "0"
               MOVE    "1"             TO  SYS-25-JIBAIPRTKBN-N
           END-IF
           IF      SYS-25-JIBAIPRTKBN1
                                       =   SPACE   OR  "0"
               MOVE    "2"             TO  SYS-25-JIBAIPRTKBN1
           END-IF
           IF      SYS-25-JIBAIPRTKBN1-N
                                       =   SPACE   OR  "0"
               MOVE    "2"             TO  SYS-25-JIBAIPRTKBN1-N
           END-IF
           IF      SYS-25-KOUIKIRECEKBN
                                       =   SPACE
               MOVE    ZERO            TO  SYS-25-KOUIKIRECEKBN
           END-IF
           IF      SYS-25-FRAME-ROUPRTKBN
                                       =   SPACE
               MOVE    ZERO            TO  SYS-25-FRAME-ROUPRTKBN
           END-IF
           IF      SYS-25-FRAME-ROUPRTKBN-N
                                       =   SPACE
               MOVE    ZERO            TO  SYS-25-FRAME-ROUPRTKBN-N
           END-IF
           IF      SYS-25-COMMENT-ROU-KSIKBN
                                       =   SPACE
               MOVE    ZERO            TO  SYS-25-COMMENT-ROU-KSIKBN
           END-IF
           IF      SYS-25-ROUPRTKBN
                                       =   SPACE   OR  "1"
               MOVE    ZERO            TO  SYS-25-ROUPRTKBN
           END-IF
           IF      SYS-25-ROUPRTKBN-N
                                       =   SPACE   OR  "1"
               MOVE    ZERO            TO  SYS-25-ROUPRTKBN-N
           END-IF
           IF      SYS-25-ROURECEKBN
                                       =   SPACE
               MOVE    "0"             TO  SYS-25-ROURECEKBN
           END-IF
           IF      SYS-25-RSI-RECEIPT-KBN
                                       =   SPACE
               MOVE    "0"             TO  SYS-25-RSI-RECEIPT-KBN
           END-IF 
      *
           MOVE    ZERO                TO  FLG-4001-RSIIRYOKBN
           INITIALIZE                      SYS-4001-REC
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-4001-HOSPNUM
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               PERFORM  UNTIL   FLG-SYSKANRI   =    1
                  MOVE    MCPDATA-REC     TO  SYS-4001-REC
                  IF    ( SYS-4001-EDYUKYMD   >=  20130701 )
                  AND   ( SYS-4001-RSIIRYOKBN =   "1"      )
                      MOVE    SYS-4001-RSIIRYOKBN
                                      TO  FLG-4001-RSIIRYOKBN
                      MOVE    1       TO  FLG-SYSKANRI
                  END-IF
      *
                  MOVE    "tbl_syskanri"  TO  MCP-TABLE
                  MOVE    "key8"          TO  MCP-PATHNAME
                  PERFORM 900-SYSKANRI-READ-SEC
              END-PERFORM
          END-IF
          MOVE    "tbl_syskanri"      TO  MCP-TABLE
          MOVE    "key8"              TO  MCP-PATHNAME
          PERFORM 900-CLOSE-SEC
      *
           MOVE    SPACE           TO  SHELLTBL
           EVALUATE    SPA-R01-RECEKBN
               WHEN    "0"
                   IF      SPA-R01-NYUGAIKBN   =   "1"
                       MOVE    "recept9.sh"    TO  WRK-SHELL-SHELL
                   ELSE
                       MOVE    "recept1.sh"    TO  WRK-SHELL-SHELL
                   END-IF                        
               WHEN    "3"
                   IF      SPA-R01-NYUGAIKBN   =   "1"
                       MOVE    "recept10.sh"   TO  WRK-SHELL-SHELL
                   ELSE
                       MOVE    "recept6.sh"    TO  WRK-SHELL-SHELL
                   END-IF
               WHEN    "4"
                   IF      SPA-R01-NYUGAIKBN   =   "1"
                       MOVE    "recept11.sh"   TO  WRK-SHELL-SHELL
                   ELSE
                       MOVE    "recept8.sh"    TO  WRK-SHELL-SHELL
                   END-IF
               WHEN    "5"
               WHEN    "8"
                   IF      SPA-R01-NYUGAIKBN   =   "1"
                       MOVE    "recept13.sh"   TO  WRK-SHELL-SHELL
                   ELSE
                       MOVE    "recept12.sh"
                                               TO  WRK-SHELL-SHELL
                   END-IF
               WHEN    "6"
                   IF      SPA-R01-NYUGAIKBN   =   "1"
                       MOVE    "recept15.sh"   TO  WRK-SHELL-SHELL
                   ELSE
                       MOVE    "recept14.sh"   TO  WRK-SHELL-SHELL
                   END-IF
           END-EVALUATE        
      *    ファイルデイレクトリ位置指定
      **     INITIALIZE              ORCSMKPASSAREA
      **     MOVE    WRK-SHELL-NAME  TO  MKPASS-IN-01
      **     CALL    "ORCSMKPASS"    USING   ORCSMKPASSAREA
      **     MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
      *
           MOVE    WRK-SHELL-NAME  TO  SHELLTBL-NAME
      *     
           MOVE    ZERO            TO  WRK-SHELL-SRYYM
           MOVE    SPA-TERMID      TO  WRK-SHELL-TERMID
      *****MOVE    SPA-SYSYMD      TO  WRK-SHELL-SYSYMD
           MOVE    SMCNDATE-YMD    TO  WRK-SHELL-SYSYMD
           MOVE    WRK-SHELL-ARG1  TO  SHELLTBL-ARG1
           MOVE    "2"             TO  SHELLTBL-ARG2
           MOVE    SPA-R01-RECEKBN TO  SHELLTBL-ARG3
           MOVE    ERRFLPARA-BASENAME
                                   TO  SHELLTBL-ARG4
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG5
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG6
           IF      SPA-R01-NYUGAIKBN   =   "1"
               MOVE    SYS-2005-PREFPRGID (2)
                                       TO  SHELLTBL-ARG7
           ELSE
               MOVE    SYS-2005-PREFPRGID (1)
                                       TO  SHELLTBL-ARG7
           END-IF
           EVALUATE    SPA-GMN-R01-SYORI
               WHEN    "1"
               WHEN    "4"
               WHEN    "5"
               WHEN    "6"
                   MOVE    WRK-SRYYM       TO  SHELLTBL-ARG8
               WHEN    "2"
                   MOVE    SPA-NAI-R01-TEISEIYM
                                           TO  SHELLTBL-ARG8
               WHEN    "3"
                   MOVE    SPA-NAI-R01-STJYUSINYMD (1:6)
                                           TO  SHELLTBL-ARG8
           END-EVALUATE
           MOVE    SMCNDATE-YMD    TO  SHELLTBL-ARG9
           MOVE    SMCNDATE-HMS    TO  SHELLTBL-ARG10 (1:6)
           MOVE    "00"            TO  SHELLTBL-ARG10 (7:2)
           MOVE    "00"            TO  SHELLTBL-ARG11
           EVALUATE    SPA-R01-RECEKBN
               WHEN    "0"
                   IF      SYS-2005-SYARECEKBN
                                           =   ZERO
                       MOVE    "0"             TO  SHELLTBL-ARG12
                   ELSE
                       MOVE    SYS-2005-SYARECEKBN
                                               TO  SHELLTBL-ARG12
                   END-IF
                   IF      SYS-2005-KKHRECEKBN
                                           =   ZERO
                       MOVE    "0"             TO  SHELLTBL-ARG13
                   ELSE
                       MOVE    SYS-2005-KKHRECEKBN
                                               TO  SHELLTBL-ARG13
                   END-IF
               WHEN    "3"
                   MOVE    SYS-25-ROURECEKBN
                                               TO  SHELLTBL-ARG12
                   MOVE    FLG-4001-RSIIRYOKBN TO  SHELLTBL-ARG13
               WHEN    OTHER
                   MOVE    "0"                 TO  SHELLTBL-ARG12
                                                   SHELLTBL-ARG13
           END-EVALUATE
           EVALUATE    SPA-R01-RECEKBN
               WHEN    "3"
                   MOVE    SYS-25-COMMENT-ROU-KSIKBN
                                           TO  SHELLTBL-ARG14
               WHEN    OTHER
                   MOVE    WRK-PREFNUM     TO  SHELLTBL-ARG14
           END-EVALUATE
           EVALUATE    SPA-R01-RECEKBN
               WHEN    "4"
                   IF      SPA-R01-NYUGAIKBN   =   "1"
                       MOVE    SYS-2005-FRAME-JIBAIPRTKBN-N
                                                   TO  SHELLTBL-ARG15
                   ELSE
                       MOVE    SYS-2005-FRAME-JIBAIPRTKBN
                                                   TO  SHELLTBL-ARG15
                   END-IF
               WHEN    "3"
                   IF      SPA-R01-NYUGAIKBN   =   "1"
                       MOVE    SYS-25-FRAME-ROUPRTKBN-N
                                                   TO  SHELLTBL-ARG15
                   ELSE
                       MOVE    SYS-25-FRAME-ROUPRTKBN
                                                   TO  SHELLTBL-ARG15
                   END-IF
           END-EVALUATE
grpsys     MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG16
           EVALUATE    SPA-R01-RECEKBN
               WHEN    "4"
                   IF      SPA-R01-NYUGAIKBN   =   "1"
                       MOVE    SYS-25-JIBAIPRTKBN1-N
                                                   TO  SHELLTBL-ARG17
                   ELSE
                       MOVE    SYS-25-JIBAIPRTKBN1
                                                   TO  SHELLTBL-ARG17
                   END-IF
               WHEN    "5"
               WHEN    "8"
                   IF      SPA-R01-NYUGAIKBN   =   "1"
                       MOVE    SYS-25-JIBAIPRTKBN-N
                                                   TO  SHELLTBL-ARG17
                   ELSE
                       MOVE    SYS-25-JIBAIPRTKBN
                                                   TO  SHELLTBL-ARG17
                   END-IF
               WHEN    "3"
                   IF      SPA-R01-NYUGAIKBN   =   "1"
                       MOVE    SYS-25-ROUPRTKBN-N
                                                   TO  SHELLTBL-ARG17
                   ELSE
                       MOVE    SYS-25-ROUPRTKBN
                                                   TO  SHELLTBL-ARG17
                   END-IF
           END-EVALUATE
           MOVE    SPA-R01-PRTKBN      TO  SHELLTBL-ARG18
           IF      SPA-RID4-FLG    =   "CHG"
               MOVE    "1"             TO  SHELLTBL-ARG19
           ELSE
               MOVE    "0"             TO  SHELLTBL-ARG19
           END-IF
           MOVE    "0"                 TO  SHELLTBL-ARG20
           IF      SPA-R01-RECEKBN =   "3"
                 MOVE        SYS-25-RSI-RECEIPT-KBN
                                           TO  SHELLTBL-ARG20
           END-IF
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "monthly"       TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
      *    自動的に情報確認画面を表示
           PERFORM 490-KAKUNIN-SEC      
           .
       450-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷画面へ遷移
      *****************************************************************
       4501-NEXT-GMN-SEC            SECTION.
      *
      *    画面移行用のパラメタ変更
      *
           MOVE    SPA-NAI-R01-TSRYYM (1)  TO  SPA-R01-SRYYM
      *
           MOVE    SPA-R01KYOUTU       TO  SPA-WORK       
      * 
           MOVE    "R01"               TO  SPA-MOTOPG
      *
           IF      SPA-R01-RECEKBN     =   "0" OR  "1" OR  "2"
               MOVE    "R02"               TO  SPA-SAKIPG
           ELSE  
               MOVE    "R04"               TO  SPA-SAKIPG
           END-IF    
      *
      ***  MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4501-NEXT-GMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求解除データで診療なしのデータの更新処理
      *****************************************************************
       4502-KAIJYO-DATA-KOUSIN-SEC     SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-NAI-R01-BTPARA-MAX
               IF      SPA-NAI-R01-BTPARA-TTAISYO (IDX)   =   ZERO
                   MOVE    SPA-NAI-R01-BTPARA-TQTORSUB-AR (IDX)
                                           TO  QTORSUB-AREA
      *
                   INITIALIZE                  SYUKA-REC
grpsys             MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
                   MOVE    QTORS-PTID      TO  SYUKA-PTID
                   MOVE    SPA-NAI-R01-BTPARA-TSRYYM     (IDX)
                                           TO  SYUKA-SRYYM
                   MOVE    SPA-R01-NYUGAIKBN
                                           TO  SYUKA-NYUGAIKBN
                   MOVE    "YYYY"          TO  SYUKA-RECESYUBETU
                   MOVE    "03"            TO  SYUKA-MISEIKYU-FLG
                   MOVE    SMCNDATE-YMD    TO  SYUKA-UPYMD
                   MOVE    SMCNDATE-HMS    TO  SYUKA-UPHMS
                   MOVE    SPA-TERMID      TO  SYUKA-TERMID
                   MOVE    SPA-OPID        TO  SYUKA-OPID
                   MOVE    SYUKA-REC       TO  MCPDATA-REC
                   MOVE    "DBUPDATE"      TO  MCP-FUNC
                   MOVE    "tbl_syuka"     TO  MCP-TABLE
                   MOVE    "upd4"          TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
      *
                   IF      MCP-RC      NOT =   ZERO 
                       MOVE    "1005"              TO  SPA-ERRCD
                       DISPLAY "SYUKA SYUKA-KEY =>" SYUKA-KEY 
                       GO  TO  4502-KAIJYO-DATA-KOUSIN-EXT
                   END-IF
               END-IF    
           END-PERFORM
      *
           .
       4502-KAIJYO-DATA-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
           MOVE    SPACE               TO  R99
           INITIALIZE                      R99
           MOVE    ZERO                TO  R99-COUNT
                                           R99-PROGRESSBAR-VALUE
           MOVE    "F12:印    刷"      TO  R99-B12-LABEL
           MOVE    "black"             TO  R99-MSG-STYLE
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               MOVE    1               TO  IDX                     
               MOVE    JOB-JOBID       TO  R99-RENNUM   (IDX)
               MOVE    JOB-SHELLMSG    TO  R99-SHORINM  (IDX)
               MOVE    JOB-STARTTIME(1:2)
                                       TO  WRK-HH
               MOVE    JOB-STARTTIME(3:2)
                                       TO  WRK-MM
               MOVE    JOB-STARTTIME(5:2)
                                       TO  WRK-SS
               MOVE    WRK-HENTIME     TO  R99-STARTTIME(IDX)
               MOVE    JOB-ENDTIME(1:2)
                                       TO  WRK-HH
               MOVE    JOB-ENDTIME(3:2)
                                       TO  WRK-MM
               MOVE    JOB-ENDTIME(5:2)
                                       TO  WRK-SS
               MOVE    WRK-HENTIME     TO  R99-ENDTIME  (IDX)
               MOVE    JOB-YOBI        TO  R99-ERRMSG   (IDX)
               COMPUTE R99-PROGRESSBAR-VALUE  =
                           JOB-STEPCNT  /   13     *   100
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "処理中です"    TO  R99-MSG
               ELSE
                   IF      JOB-ERRCD       =   SPACE
                     IF      JOB-UPDCNT      =   ZERO    
                         MOVE    "対象データがありませんでした"
                                                       TO  R99-MSG
                     ELSE
                       PERFORM 4501-REPRINT-CHK-SEC
                       IF      SPA-RECE07-PRTID    =   SPACE
                           MOVE    "対象のレセプトが作成されていません"
                                               TO  R99-MSG
                       ELSE                    
                           MOVE    SPA-RECE07-KEY
                                          TO  SPA-R01-RECE07-KEY
                           MOVE    SPA-RECE07-RECEKBN
                                          TO  SPA-R01-RECE07-RECEKBN
                           MOVE    SPA-RECE07-SRYYM
                                          TO  SPA-R01-RECE07-SRYYM
                           MOVE    SPA-RECE07-NYUGAIKBN
                                          TO  SPA-R01-RECE07-NYUGAIKBN
                           MOVE    SPA-RECE07-SYORISBT
                                          TO  SPA-R01-RECE07-SYORISBT
                           MOVE    SPA-RECE07-STYMD
                                          TO  SPA-R01-RECE07-STYMD
                           MOVE    SPA-RECE07-EDYMD
                                          TO  SPA-R01-RECE07-EDYMD
                           MOVE    SPA-RECE07-HSEQNUM
                                          TO  SPA-R01-RECE07-HSEQNUM
                           MOVE    SPA-RECE07-HKNJANUM
                                          TO  SPA-R01-RECE07-HKNJANUM
                           MOVE    SPA-RECE07-SRYKA-KOKUHO
                                          TO SPA-R01-RECE07-SRYKA-KOKUHO
                           MOVE    SPA-RECE07-SRYKA-KOUIKI
                                          TO SPA-R01-RECE07-SRYKA-KOUIKI
                           INITIALIZE     SPA-R01-RECE07-RECE-PUTH(1)
                                          SPA-R01-RECE07-RECE-PUTH(2)
                           MOVE    SPACE  TO  SPA-R01-RECE08-DRCD-SRYKA
                                              SPA-R01-RECE08-SORT-1
                                              SPA-R01-RECE08-SORT-2
                                          SPA-R01-RECE08-RECESYUBETU-SRT
                           MOVE    SPA-RECE07-FUMEI-PAGE
                                           TO  SPA-R01-RECE07-FUMEI-PAGE
                           MOVE    SPA-RECE07-HKNJANUM-SYAHO-T
                                          TO
                                       SPA-R01-RECE07-HKNJANUM-SYAHO-T
                       END-IF
                     END-IF
                   ELSE
                       STRING  JOB-YOBI    DELIMITED  BY  SPACE
                               "【"        DELIMITED  BY  SIZE
                              JOB-SHELLMSG DELIMITED  BY  SPACE
                              "】"         DELIMITED  BY  SIZE      
                                           INTO  R99-MSG
                       END-STRING
                   END-IF                          
               END-IF    
               MOVE    IDX                     TO  R99-COUNT    
           END-IF
      *     
           MOVE    WIDGET-INSENSITIVE   TO  R99-B21-STATE
           MOVE    WIDGET-INSENSITIVE   TO  R99-B12-STATE
           IF      R99-MSG              =   SPACE
               IF      R99-COUNT            >   ZERO    
                   MOVE    WIDGET-NORMAL        TO  R99-B12-STATE
                   MOVE    "B12"                TO  MCP-WIDGET
                   IF    ( SPA-R01-RECE07-SRYKA    =   "XX" )
                   OR    ( SPA-R01-RECE07-SRYKA-KOKUHO
                                                   =   "XX" )
                   OR    ( SPA-R01-RECE07-SRYKA-KOUIKI
                                                   =   "XX" )
                   OR    ( SPA-R01-RECE07-FUMEI-PAGE
                                                   >   ZERO )
                       STRING  "主科未設定分又は種別不明分がありま"
                                                DELIMITED   BY  SIZE
                               "すので印刷画面で確認して下さい"
                                                DELIMITED   BY  SIZE
                                                INTO    R99-MSG
                       END-STRING
                       MOVE    "red"        TO  R99-MSG-STYLE 
                   ELSE
                       MOVE    "処理は正常に終了しました"
                                                TO  R99-MSG
                   END-IF
                   MOVE   100                   TO
                                            R99-PROGRESSBAR-VALUE   
               ELSE
                   MOVE    "B01"                TO  MCP-WIDGET
                   MOVE    "該当する処理はありません"
                                                TO  R99-MSG
               END-IF                                 
           ELSE                                        
               IF      JOB-STOPFLG          =   ZERO
                   MOVE    WIDGET-NORMAL        TO  R99-B21-STATE
               END-IF
               MOVE    "B11"                TO  MCP-WIDGET
           END-IF                                 
      *
           MOVE    "R01"                TO  SPA-MOTOPG
           MOVE    "R99"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "R99"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       480-KENSAKU-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
      *****MOVE    SPA-R01FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "R01"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *
      *    氏名をクリアする
           MOVE    SPACE               TO  SPA-NAME
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P97"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       480-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
      *****MOVE    SPA-R01FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "R01"               TO  SPA-MOTOPG
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
      *    未請求設定処理
      *****************************************************************
       490-MISEIKYU-ENTRY-SEC       SECTION.
      *
           IF      SPA-GMN-R01-MAX     >   ZERO
               MOVE    "0041"              TO  SPA-ERRCD
               GO  TO  490-MISEIKYU-ENTRY-EXT
      *
           END-IF
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "7777"          TO  SPA-ERRCD
                   MOVE    SPACE             TO  WRK-RECEERR
                   STRING  "処理中です【"
                                             DELIMITED  BY  SIZE
                           JOB-SHELLMSG      DELIMITED  BY  SPACE
                           "】"              DELIMITED  BY  SIZE      
                                             INTO  WRK-RECEERR
                   END-STRING                          
                   GO  TO  490-MISEIKYU-ENTRY-EXT
               END-IF
           END-IF    
      * 
           MOVE    "R01"               TO  SPA-MOTOPG
           MOVE    "R96"               TO  SPA-SAKIPG
      *
      *    画面移行用のパラメタ変更
           MOVE    "R01"               TO  SPA-R01-MOTOPG
      *
           MOVE    SPA-R01KYOUTU       TO  SPA-WORK
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-MISEIKYU-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
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
           IF      SPA-RIDCD       NOT =   SPACE
               PERFORM 520-RIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-RIDCD2      NOT =   SPACE
               PERFORM 530-RIDSET2-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-RIDCD4      NOT =   SPACE
               PERFORM 540-RIDSET4-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "R01    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           IF      FLG-ROW             =   1
               MOVE    ZERO                TO  WRK-R01-ROW
           ELSE
               MOVE    R01-ROW             TO  WRK-R01-ROW
           END-IF
      * 
           MOVE    SPACE               TO  R01
      *
           INITIALIZE                      R01
           MOVE    SPA-GMN-R01-SYOKBN  TO  R01-SYOKBN
           MOVE    "red"               TO  R01-SYOKBN-STYLE
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX                 >   SPA-GMN-R01-MAX
               IF      SPA-GMN-R01-TRENNUM    (IDX)    NOT =   ZERO
                   MOVE    SPA-GMN-R01-TRENNUM    (IDX)
                                               TO  WRK-NUM
                   MOVE    WRK-NUM             TO  R01-TRENNUM   (IDX)
               END-IF
               MOVE    SPA-GMN-R01-TSRYYM     (IDX)
                                               TO  R01-TSRYYM    (IDX)
               MOVE    SPA-GMN-R01-TPTNUM     (IDX)
                                               TO  R01-TPTNUM    (IDX)
               MOVE    SPA-GMN-R01-TNAME      (IDX)
                                               TO  R01-TNAME     (IDX)
               MOVE    SPA-GMN-R01-TNYUGAIKBN (IDX)
                                               TO  R01-TNYUGAIKBN(IDX)
               MOVE    SPA-GMN-R01-TTSTPTNUMKBN (IDX)
                                               TO  R01-TTSTPTNUMKBN(IDX)
               MOVE    SPA-GMN-R01-TTAISYO    (IDX)
                                               TO  R01-TTAISYO   (IDX)
               MOVE    SPA-GMN-R01-TIHO       (IDX)
                                               TO  R01-TIHO      (IDX)
               MOVE    SPA-GMN-R01-TRSI       (IDX)
                                               TO  R01-TRSI      (IDX)
               MOVE    SPA-GMN-R01-TJBI       (IDX)
                                               TO  R01-TJBI      (IDX)
               MOVE    SPA-GMN-R01-TKGI       (IDX)
                                               TO  R01-TKOU      (IDX)
               MOVE    SPA-GMN-R01-TDAISAN    (IDX)
                                               TO  R01-TDAISAN   (IDX)
      *         
               IF      IDX             =   SPA-NAI-R01-SELNO
                   MOVE    "T"             TO  R01-SELECT (IDX)
               ELSE
                   MOVE    "F"             TO  R01-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-R01-MAX         TO  R01-COUNT
           MOVE    ZERO                TO  R01-ROW
           IF      WRK-R01-ROW         >   ZERO
               MOVE    WRK-R01-ROW         TO  R01-ROW
           END-IF
      *
           MOVE    SPA-GMN-R01-SYORI-G     TO  R01-SYORI
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-R01-SYORILST-MAX
               MOVE    SPA-GMN-R01-SYORILST(IDX)
                                           TO  R01-SYORILST (IDX)
               MOVE    IDX                 TO  R01-SYORI-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-R01-LABEL       TO  R01-LABEL
           MOVE    SPA-GMN-R01-LABEL1      TO  R01-LABEL1
           MOVE    SPA-GMN-R01-LABEL2      TO  R01-LABEL2
           MOVE    SPA-GMN-R01-BTPARA-HYODAI
                                           TO  R01-HYODAI
           MOVE    SPA-GMN-R01-BTPARA-JYOKEN1
                                           TO  R01-JYOKEN1
           MOVE    SPA-GMN-R01-BTPARA-SRYYMH
                                           TO  R01-SRYYMH
           MOVE    SPA-GMN-R01-BTPARA-NYUGAIKBNH
                                           TO  R01-NYUGAIKBNH
           MOVE    SPA-GMN-R01-BTPARA-CREYMD
                                           TO  R01-CREYMDH
           IF      SPA-GMN-R01-BTPARA-CREYMD
                                        NOT =   SPACE
               MOVE    "作成"                   TO  R01-CREYMDH (10:)
           END-IF
           MOVE    SPA-GMN-R01-BTPARA-KENSU
                                          TO  R01-KENSU
      *
           MOVE    "blue"                 TO  R01-HYODAI-STYLE
                                              R01-JYOKEN1-STYLE
                                              R01-SRYYMH-STYLE
                                              R01-NYUGAIKBNH-STYLE
                                              R01-CREYMDH-STYLE
      *
      **     IF      SPA-R01-NYUGAIKBN   =   "1"
      **         MOVE    WIDGET-INSENSITIVE  TO  R01-SYORI-STATE
      **     ELSE
               MOVE    WIDGET-NORMAL       TO  R01-SYORI-STATE
      **     END-IF
      *
           MOVE    WIDGET-INSENSITIVE  TO  R01-STJYUSINYMD-STATE
                                           R01-EDJYUSINYMD-STATE
                                           R01-RENNUM-STATE
                                           R01-PTNUM-STATE
                                           R01-SRYYM-STATE
                                           R01-NYUGAIKBN-STATE
                                           R01-TEISEIYM-STATE
                                           R01-B09-STATE
                                           R01-B02-STATE
                                           R01-B04-STATE
                                           R01-B08-STATE
                                           R01-B05-STATE
                                           R01-B22-STATE
           EVALUATE    SPA-GMN-R01-SYORI
               WHEN    "1"
                   MOVE    WIDGET-NORMAL       TO  R01-RENNUM-STATE
                                                   R01-PTNUM-STATE
                                                   R01-SRYYM-STATE
                                                   R01-NYUGAIKBN-STATE
                                                   R01-B09-STATE
                                                   R01-B02-STATE
                                                   R01-B04-STATE
                                                   R01-B08-STATE
               WHEN    "2"
                   MOVE    WIDGET-NORMAL       TO  R01-TEISEIYM-STATE
               WHEN    "3" 
                   MOVE    WIDGET-NORMAL       TO  R01-STJYUSINYMD-STATE
                                                   R01-EDJYUSINYMD-STATE
               WHEN    "4"
                   IF      SPA-NAI-R01-BTPARA-MAX  >   ZERO
                       MOVE    WIDGET-NORMAL       TO  R01-B05-STATE
                   END-IF 
               WHEN    "5"
                   IF    ( SPA-NAI-R01-BTPARA-MAX  >   ZERO )
                   AND   ( SPA-NAI-R01-BTPARA-TAISYO-MAX
                                                   >   ZERO )
                       MOVE    WIDGET-NORMAL       TO  R01-B05-STATE
                   END-IF
           END-EVALUATE
           IF      SPA-R01-RECEKBN     =   "0"
               MOVE    WIDGET-NORMAL       TO  R01-B22-STATE
           END-IF
      *
           EVALUATE   SPA-NAI-R01-BTPARA-HYOUJI-FLG
               WHEN    1
                   MOVE    "全体表示"      TO  R01-B05-VALUE
               WHEN    ZERO
                   MOVE    "対象表示"      TO  R01-B05-VALUE
           END-EVALUATE
      *
           MOVE    SPA-GMN-R01-TEISEIYM    TO  R01-TEISEIYM
           MOVE    SPA-GMN-R01-STJYUSINYMD TO  R01-STJYUSINYMD
           MOVE    SPA-GMN-R01-EDJYUSINYMD TO  R01-EDJYUSINYMD
           MOVE    SPA-GMN-R01-RENNUM      TO  R01-RENNUM
           MOVE    SPA-GMN-R01-PTNUM       TO  R01-PTNUM
           MOVE    SPA-GMN-R01-NAME        TO  R01-NAME
           MOVE    SPA-GMN-R01-SRYYM       TO  R01-SRYYM
           MOVE    SPA-GMN-R01-NYUGAIKBN   TO  R01-NYUGAIKBN
           MOVE    SPA-GMN-R01-CREYMD      TO  R01-CREYMD
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
           IF    ( SPA-ERRCD           =   SPACE)
           AND   ( SPA-GMN-R01-CUR         =   ZERO )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-R01-CUR
               WHEN    3
                   MOVE  "RENNUM"          TO  MCP-WIDGET
               WHEN    4
                   MOVE  "PTNUM"           TO  MCP-WIDGET
               WHEN    5
                   MOVE  "SRYYM"           TO  MCP-WIDGET
               WHEN    6
                   MOVE  "NYUGAIKBN"       TO  MCP-WIDGET
               WHEN    7
                   MOVE  "B12"             TO  MCP-WIDGET   
               WHEN    8
                   MOVE  "SYORI"           TO  MCP-WIDGET   
               WHEN    9
                   MOVE  "STJYUSINYMD"     TO  MCP-WIDGET
               WHEN    10
                   MOVE  "EDJYUSINYMD"     TO  MCP-WIDGET
               WHEN    11
                   MOVE  "TEISEIYM"        TO  MCP-WIDGET
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
               WHEN    "SYORI"
                   IF      SPA-GMN-R01-SYORI   =   "1"
                       MOVE    4               TO  SPA-GMN-R01-CUR
                   ELSE
                       MOVE    9               TO  SPA-GMN-R01-CUR
                   END-IF 
               WHEN    "RENNUM"
                   MOVE    4               TO  SPA-GMN-R01-CUR
               WHEN    "PTNUM"
                   MOVE    5               TO  SPA-GMN-R01-CUR
               WHEN    "SRYYM"
                   MOVE    6               TO  SPA-GMN-R01-CUR
               WHEN    "NYUGAIKBN"
                   MOVE    4               TO  SPA-GMN-R01-CUR
               WHEN    "STJYUSINYMD"
                   MOVE    10              TO  SPA-GMN-R01-CUR
               WHEN    "EDJYUSINYMD"
                   MOVE    7               TO  SPA-GMN-R01-CUR
               WHEN    "TEISEIYM"
                   MOVE    7               TO  SPA-GMN-R01-CUR
           END-EVALUATE
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入外区分入力エラー"        TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "これ以上データの追加はできません"
                                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "該当の患者が存在しません"  TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "対象のデータを選択してありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "同じ診療年月で既に入力済です"
                                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "対象の診療会計がありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0007"
                   STRING  "改正前と改正後（２００２年１０月）の"
                                           DELIMITED   BY  SIZE
                           "レセプトは同時に処理できません"
                                           DELIMITED   BY  SIZE
                                           INTO        SPA-ERRMSG
                   END-STRING
               WHEN    "0008"
                   MOVE    "対象の保険組合せが無い会計があります"
                                                       TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "診療年月入力エラー"        TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE
                       "レセプトの対象となるデータが入力してありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "前回は個別作成のデータではありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "前回とはレセプトの種類が違います"
                                                       TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "前回とは入外区分が違います"
                                                       TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "入院会計の基本情報がありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "対象のレセプトが作成されていません"
                                                       TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "受診日入力エラー"          TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "処理区分エラー"            TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "受診日は同月で入力して下さい"
                                                       TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "受診日に該当する患者がありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE    "個別指定の時のみ選択できます"
                                                       TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "開始日≦終了日で入力して下さい"
                                                       TO  SPA-ERRMSG
               WHEN    "0022"
                   MOVE    "項目の入力途中です"        TO  SPA-ERRMSG
               WHEN    "0023"
                   MOVE    "選択番号入力エラー"        TO  SPA-ERRMSG
               WHEN    "0024"
                   MOVE    "対象診療年月入力エラー"    TO  SPA-ERRMSG
               WHEN    "0025"
                   MOVE    "対象診療年月の一括作成が行われていません"
                                                       TO  SPA-ERRMSG
               WHEN    "0026"
                   MOVE    "訂正日に該当する患者がありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0027"
                   MOVE    "一括作成した日に訂正日指定はできません"
                                                       TO  SPA-ERRMSG
               WHEN    "0028"
                   MOVE    "訂正日の範囲が誤っています"
                                                       TO  SPA-ERRMSG
               WHEN    "0029"
                   STRING  "様式改正前と改正後（２００６年１０月診療分"
                                           DELIMITED   BY  SIZE
                           "以降）のレセプトは同時に処理できません"
                                           DELIMITED   BY  SIZE
                                           INTO        SPA-ERRMSG
                   END-STRING
               WHEN    "0030"
                   MOVE    "対象の連携データがありません"
                                                     TO  SPA-ERRMSG
               WHEN    "0031"
                   MOVE    "連携データがありません"  TO  SPA-ERRMSG
               WHEN    "0032"
                   MOVE    "明細書対象の未設定解除データがありません"
                                                     TO  SPA-ERRMSG
               WHEN    "0033"
                   MOVE    "未請求解除データがありません"
                                                     TO  SPA-ERRMSG
               WHEN    "0034"
                   MOVE    "前回と処理区分が違います"  TO  SPA-ERRMSG
               WHEN    "0035"
                   MOVE    "未請求設定の患者です"    TO  SPA-ERRMSG
               WHEN    "0036"
                   STRING  "該当診療年月は健保準拠の設定ですので"
                                           DELIMITED   BY  SIZE  
                           "自賠責（新様式）では処理できません"
                                           DELIMITED   BY  SIZE  
                                           INTO        SPA-ERRMSG
                   END-STRING
               WHEN    "0037"
                   MOVE    "請求データ削除分のデータは選択できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0038"
                   STRING  "請求データ削除済データとして既に"
                                           DELIMITED   BY  SIZE  
                           "入力済です"    DELIMITED   BY  SIZE  
                                           INTO        SPA-ERRMSG
                   END-STRING
               WHEN    "0039"
                   STRING  "改正前と改正後（２００８年４月）の"
                                           DELIMITED   BY  SIZE
                           "レセプトは同時に処理できません"
                                           DELIMITED   BY  SIZE
                                           INTO        SPA-ERRMSG
                   END-STRING
               WHEN    "0040"
                   STRING
                   "改正前後（平成２０年４月）が混在しているため"
                                           DELIMITED   BY  SIZE
                   "一旦改正前のみ処理対象とします"
                                           DELIMITED   BY  SIZE
                                           INTO        SPA-ERRMSG
                   END-STRING
               WHEN    "0041"
                   MOVE    "データが入力してあるので遷移できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0042"
                   MOVE    "前回とはレセプトの種類が違います"
                                                       TO  SPA-ERRMSG
               WHEN    "0043"
                   STRING  "該当診療年月に労災準拠の算定がありませんの"
                                           DELIMITED   BY  SIZE  
                           "で、自賠責（新様式）では処理できません"
                                           DELIMITED   BY  SIZE  
                                           INTO        SPA-ERRMSG
                   END-STRING
               WHEN    "1001"
                   MOVE    "レセプト指示コード（個別）が削除できません"
                                                       TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "レセプト指示コード（個別）が更新できません"
                                                       TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "レセプト指示コード（管理）が更新できません"
                                                       TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "パラメタＤＢが更新できません"
                                                       TO  SPA-ERRMSG
               WHEN    "1005"
                   MOVE    "主科ＤＢが更新できません"  TO  SPA-ERRMSG
               WHEN    "7777"
                   MOVE    WRK-RECEERR                 TO  SPA-ERRMSG
               WHEN    "9999"
                   MOVE    WRK-RECEERR                 TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  RERR
           MOVE    SPA-ERRCD            TO  RERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  RERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "R01"                TO  SPA-MOTOPG
           MOVE    "RERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "RERR"               TO  MCP-WINDOW
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
      *    確認画面１表示処理
      *****************************************************************
       520-RIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-RIDMSG
      *
           EVALUATE    SPA-RIDCD
               WHEN    "1001"
                   MOVE    "解除データの更新を行います"  TO  WRK-RIDMSG
               WHEN    "1002"
                   MOVE    "個別作成で作表します"        TO  WRK-RIDMSG
               WHEN    "1003"
                   MOVE    "入力をすべてクリアします"    TO  WRK-RIDMSG
               WHEN    "1004"
                   MOVE    "前回個別作成分を表示します"  TO  WRK-RIDMSG
               WHEN    "1005"
                   MOVE
               "処理区分が変更されました。入力された内容はクリアします"
                                                         TO  WRK-RIDMSG
               WHEN    "1006"
                   STRING  "受診日に該当する患者は" DELIMITED  BY  SIZE
                           WRK-KENSU-X              DELIMITED  BY  SPACE
                           "人です。受診日指定で作表します"
                                                    DELIMITED  BY  SIZE
                           INTO   WRK-RIDMSG
                   END-STRING
               WHEN    "1007"
                   STRING  "訂正期間に該当する患者は"
                                                    DELIMITED  BY  SIZE
                           WRK-KENSU-X              DELIMITED  BY  SPACE
                           "人です。訂正日指定で作表します"
                                                    DELIMITED  BY  SIZE
                           INTO   WRK-RIDMSG
                   END-STRING
               WHEN    "1008"
                   STRING
                       "対象の診療会計がないので作成済の請求"
                                                    DELIMITED  BY  SIZE
                       "データを削除します。よろしいですか"
                                                    DELIMITED  BY  SIZE
                           INTO   WRK-RIDMSG
                   END-STRING                   
               WHEN    "1009"
                   STRING
                       "対象の保険組合せが無い会計があるので作成"
                                                    DELIMITED  BY  SIZE
                       "済の請求データを削除します。よろしいですか"
                                                    DELIMITED  BY  SIZE
                           INTO   WRK-RIDMSG
                   END-STRING                   
               WHEN    "1010"
                   STRING
                       "入院会計の基本情報がないので作成済の"
                                                    DELIMITED  BY  SIZE
                       "請求データを削除します。よろしいですか"
                                                    DELIMITED  BY  SIZE
                           INTO   WRK-RIDMSG
                   END-STRING                   
               WHEN    "1011"
                   STRING
                       "対象の診療会計がないので作成済の請求"
                                                    DELIMITED  BY  SIZE
                       "データを削除します。対象患者番号＝"
                                                    DELIMITED  BY  SIZE
                       SPA-NAI-R01-2002-TPTNUM (SPA-NAI-R01-2002-INDEX)
                                                    DELIMITED  BY  SIZE
                           INTO   WRK-RIDMSG
                   END-STRING                   
               WHEN    "1012"
                   STRING
                       "対象の保険組合せが無い会計があるので作成済の"
                                                    DELIMITED  BY  SIZE
                       "請求データを削除します。対象患者番号＝"
                                                    DELIMITED  BY  SIZE
                       SPA-NAI-R01-2002-TPTNUM (SPA-NAI-R01-2002-INDEX)
                                                    DELIMITED  BY  SIZE
                           INTO   WRK-RIDMSG
                   END-STRING                   
               WHEN    "1013"
                   STRING
                       "入院会計の基本情報がないので作成済の請求"
                                                    DELIMITED  BY  SIZE
                       "データを削除します。対象患者番号＝"
                                                    DELIMITED  BY  SIZE
                       SPA-NAI-R01-2002-TPTNUM (SPA-NAI-R01-2002-INDEX)
                                                    DELIMITED  BY  SIZE
                           INTO   WRK-RIDMSG
                   END-STRING                   
               WHEN    "6666"
                   MOVE    "レセプト作成処理中です"    TO  WRK-RIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  RID1
           INITIALIZE                      RID1
           MOVE    SPA-RIDCD           TO  RID1-ID1CODE
           MOVE    WRK-RIDMSG          TO  RID1-ID1MSG
           EVALUATE    SPA-RIDCD
               WHEN    "1002"
               WHEN    "1006"
               WHEN    "1007"
                   IF      SPA-R01-PRTKBN       =   "2" OR  "3"
                                                OR  "4" OR  "5"
                       MOVE    "【点検用レセプト】"     TO  RID1-LABEL
                   END-IF
                   IF      SPA-NAI-R01-PRTKBN-FLG       =   1
                       STRING  "【提出用レセプト】に変更します"
                                               DELIMITED  BY  SIZE
                           "（対象の診療年月が平成２０年３月以前です）"
                                               DELIMITED  BY  SIZE
                                               INTO    RID1-LABEL
                       END-STRING
                    END-IF
           END-EVALUATE
           MOVE    "blue"              TO  RID1-LABEL-STYLE
      *
           EVALUATE    SPA-RIDCD
               WHEN    "1008"
               WHEN    "1009"
               WHEN    "1010"
               WHEN    "1011"
               WHEN    "1012"
               WHEN    "1013"
                   MOVE    "ＮＯ"               TO  RID1-B01-LABEL
               WHEN    OTHER 
                   MOVE    "戻る"               TO  RID1-B01-LABEL
           END-EVALUATE
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "R01"                TO  SPA-MOTOPG
           MOVE    "RID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "RID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-RIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面２表示処理
      *****************************************************************
       530-RIDSET2-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-RIDMSG
      *
           EVALUATE    SPA-RIDCD2
               WHEN    "8888"
                   MOVE    "レセプト作成が正常終了しました"
                                                       TO  WRK-RIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  RID2
           INITIALIZE                       RID2
           MOVE    SPA-RIDCD2           TO  RID2-ID1CODE
           MOVE    WRK-RIDMSG           TO  RID2-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "R01"                TO  SPA-MOTOPG
           MOVE    "RID2"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "RID2"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       530-RIDSET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面４表示処理
      *****************************************************************
       540-RIDSET4-SEC              SECTION.
      *
           MOVE    SPACE               TO  RID4
           INITIALIZE                      RID4
           MOVE    SPA-RIDCD4          TO  RID4-ID1CODE
      *
           MOVE    SPACE               TO  WRK-RIDMSG
           EVALUATE    SPA-RIDCD4
               WHEN    "1002"
                   MOVE    "個別作成で作表します"      TO  WRK-RIDMSG
               WHEN    "1006"
                   STRING  "受診日に該当する患者は" DELIMITED  BY  SIZE
                           WRK-KENSU-X              DELIMITED  BY  SPACE
                           "人です。受診日指定で作表します"
                                                    DELIMITED  BY  SIZE
                                                    INTO   WRK-RIDMSG
                   END-STRING
               WHEN    "1007"
                   STRING  "訂正期間に該当する患者は"
                                                    DELIMITED  BY  SIZE
                           WRK-KENSU-X              DELIMITED  BY  SPACE
                           "人です。訂正日指定で作表します"
                                                    DELIMITED  BY  SIZE
                                                    INTO   WRK-RIDMSG
                   END-STRING                   
           END-EVALUATE
           MOVE    WRK-RIDMSG          TO  RID4-ID1MSG
      *
           EVALUATE    SPA-RIDCD4
               WHEN    "1002"
               WHEN    "1006"
               WHEN    "1007"
                   IF      SPA-R01-PRTKBN       =   "2" OR  "3"
                                                OR  "4" OR  "5"
                       MOVE    "【点検用レセプト】"     TO  RID4-LABEL
                   END-IF
                   IF      SPA-NAI-R01-PRTKBN-FLG       =   1
                       STRING  "【提出用レセプト】に変更します"
                                               DELIMITED  BY  SIZE
                           "（対象の診療年月が平成２０年３月以前です）"
                                               DELIMITED  BY  SIZE
                                               INTO    RID4-LABEL
                       END-STRING
                   END-IF
      *
                   STRING  "請求しない設定された請求管理データが"
                                                    DELIMITED  BY  SIZE
                           "あります。このデータを削除し新たに請求管理"
                                                    DELIMITED  BY  SIZE
                           "データを作成しますか"   DELIMITED  BY  SIZE
                                                    INTO  RID4-LABEL1
                   END-STRING 
           END-EVALUATE
      *
           MOVE    "blue"              TO  RID4-LABEL-STYLE
                                           RID4-LABEL1-STYLE
      *
           MOVE    "戻る"              TO  RID4-B01-LABEL
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "R01"               TO  SPA-MOTOPG
           MOVE    "RID4"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "RID4"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       540-RIDSET4-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト管理読込（キー）
      *****************************************************************
       900-RECE07-INV-SEC         SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
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
      *
           .
       900-RECE07-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                      PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-N-SEC       SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
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
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
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
      *    診療行為マスタ　ＲＥＡＤ
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYACCT
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           IF      FLG-SRYACCT         =   ZERO
               MOVE    ACCT-PTID           TO  KEY-N-PTID 
               MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
               MOVE    ACCT-SRYKA          TO  KEY-N-SRYKA
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
      *
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    NACCT-PTID          TO  KEY-N-PTID 
      *********MOVE    NACCT-HKNCOMBI      TO  KEY-N-HKNCOMBI
               MOVE    NACCT-SRYKA         TO  KEY-N-SRYKA
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
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-SEC         SECTION.
      *
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
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
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
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
      *    主科情報読込
      *****************************************************************
       900-SYUKA-READ-SEC         SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-SYUKA-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SYUKA
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYUKA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科情報読込
      *****************************************************************
       910-SYUKA-NEXT-SEC         SECTION.
      * 
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC               =   ZERO
               MOVE    ZERO                 TO  FLG-SYUKA
               MOVE    MCPDATA-REC          TO  SYUKA-REC
           ELSE
               MOVE    1                    TO  FLG-SYUKA
               INITIALIZE                       SYUKA-REC
           END-IF
      *
           .
       910-SYUKA-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報マスタ読込
      *****************************************************************
       900-PTRSIINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTRSIINF
                   MOVE    MCPDATA-REC         TO  PTRSIINF-REC
                   IF      PTRSI-SAKUJOKBN     =   "1"
                       MOVE    1               TO  FLG-PTRSIINF
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-PTRSIINF
                   INITIALIZE                      PTRSIINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTRSIINF
               INITIALIZE                      PTRSIINF-REC
           END-IF
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTRSIINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込（対象請求管理データの存在チェック）
      *****************************************************************
       900-SEIKYU-READ-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SEIKYU-UMU
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key59"             TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key59"             TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           PERFORM                 UNTIL   FLG-SEIKYU      =   1
                                   OR      FLG-SEIKYU-UMU  =   1
               EVALUATE    SPA-R01-RECEKBN
                   WHEN    "0"
                       IF      RECE10-TEISYUTUSAKI =   1   OR  2   OR  6
                           MOVE    1               TO  FLG-SEIKYU-UMU
                       END-IF
                   WHEN    "3"
                       IF      RECE10-TEISYUTUSAKI =   3
                           MOVE    1               TO  FLG-SEIKYU-UMU
                       END-IF
                   WHEN    "4"
                   WHEN    "5"
                       IF    ( RECE10-TEISYUTUSAKI =   4             )
                       AND   ( RECE10-HKNNUM       =
                                           WRK-CONS-JIBAISEKI-HKNNUM )
                           MOVE    1               TO  FLG-SEIKYU-UMU
                       END-IF
                   WHEN    "6"
                       IF      RECE10-TEISYUTUSAKI =   5
                           MOVE    1               TO  FLG-SEIKYU-UMU
                       END-IF
                   WHEN    "8"
                       IF    ( RECE10-TEISYUTUSAKI =   4             )
                       AND   ( RECE10-HKNNUM       =
                                           WRK-CONS-DAISANSYA-HKNNUM )
                           MOVE    1               TO  FLG-SEIKYU-UMU
                       END-IF
               END-EVALUATE
      * 
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key59"             TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-NEXT-SEC
           END-PERFORM
      * 
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key59"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-NEXT-SEC         SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SEIKYU
               MOVE    MCPDATA-REC     TO  RECE10-REC
           ELSE
               MOVE    1               TO  FLG-SEIKYU
           END-IF
      *
           .
       900-SEIKYU-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込（対象請求管理データの請求しない設定存在チェック）
      *****************************************************************
       900-SEIKYU-SKYKBN-CHECK-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-SEIKYU-UMU
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key59"             TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key59"             TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           PERFORM                 UNTIL   FLG-SEIKYU      =   1
                                   OR      FLG-SEIKYU-UMU  =   1
               IF      RECE10-SKYKBN       =   "0"
                   EVALUATE    SPA-R01-RECEKBN
                     WHEN    "0"
                       IF      RECE10-TEISYUTUSAKI =   1   OR  2   OR  6
                           MOVE    1               TO  FLG-SEIKYU-UMU
                       END-IF
                     WHEN    "3"
                       IF      RECE10-TEISYUTUSAKI =   3
                           MOVE    1               TO  FLG-SEIKYU-UMU
                       END-IF
                     WHEN    "4"
                     WHEN    "5"
                       IF    ( RECE10-TEISYUTUSAKI =   4            )
                       AND   ( RECE10-HKNNUM       =
                                           WRK-CONS-JIBAISEKI-HKNNUM )
                           MOVE    1               TO  FLG-SEIKYU-UMU
                       END-IF
                     WHEN    "6"
                       IF      RECE10-TEISYUTUSAKI =   5
                           MOVE    1               TO  FLG-SEIKYU-UMU
                       END-IF
                     WHEN    "8"
                       IF    ( RECE10-TEISYUTUSAKI =   4             )
                       AND   ( RECE10-HKNNUM       =
                                           WRK-CONS-DAISANSYA-HKNNUM )
                           MOVE    1               TO  FLG-SEIKYU-UMU
                       END-IF
                   END-EVALUATE
               END-IF
      * 
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key59"             TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-NEXT-SEC
           END-PERFORM
      * 
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key59"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *
           .
       900-SEIKYU-SKYKBN-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブルＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-SPATMP-DBSELECT-SEC                SECTION.
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-SPATMP-CALL-SEC
      *
           .
       900-SPATMP-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブルＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-SPATMP-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-SPATMP-CALL-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA2-REC    TO  SPATMP-REC
               MOVE    ZERO            TO  FLG-SPATMP
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
      *
           .
       900-SPATMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブルクローズ処理
      *****************************************************************
       900-SPATMP-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-SPATMP-CALL-SEC
      *
           .
       900-SPATMP-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブル削除処理
      *****************************************************************
       900-SPATMP-DBDELETE-SEC         SECTION.
      *
           INITIALIZE                      SPATMP-REC
           MOVE    "R01"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-SPATMP-CALL-SEC
      *
           .
       900-SPATMP-DBDELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブル検索処理
      *****************************************************************
       900-SPATMP-CALL-SEC                SECTION.
      *
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
           CALL    "ORCDBSPATMP"       USING   MCPAREA
                                               MCPDATA2-REC
                                               SPA-AREA
      *
           .
       900-SPATMP-CALL-EXT.
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
