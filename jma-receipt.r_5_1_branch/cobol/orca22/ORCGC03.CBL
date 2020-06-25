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
       PROGRAM-ID.             ORCGC03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 病名登録
      *  コンポーネント名  : 自院病名登録（Ｃ０３）
      *  管理者            : 
      *  01/12/11    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/12/19  修飾語コードの入力方法の変更    
      *                                     病名コード未入力のとき病名へ移動
      *  01.00.02    MCC-藤原     02/01/31  病名よりレセ電コードに変換
      *  01.00.03    MCC-藤原     02/02/05  コラムリスト４００に変更
      *  01.00.04    MCC-藤原     02/04/11  8002"の疑い"の病名コードのとき
      *                                     疑いフラグに１をセット
      *                                     病名コードに"//"入力の時病名検索へ
      *  01.00.05    MCC-森脇     02/04/25  リスト出力処理を追加
      *  01.00.06    MCC-藤原     02/05/08  半角チェック追加
      *  01.00.07    MCC-多々納   02/05/08  半角チェック時、半角空白を
      *                                     全角空白へ置換えの追加
      *  01.00.08    MCC-藤原     02/05/17  コラムリスト700に変更
      *  01.00.09    MCC-藤原     02/07/05  病名コード空白の時コードをつめる
      *  01.00.10    MCC-藤原     02/07/08  皮膚科特定疾患（３）をコンボから削除
      *  01.00.11    NACL-藤原     2/08/08  入力コードの重複チェックを
      *                                     入力時にも追加
      *
      *  01.01.01    NACL-藤原    02/08/20  新傷病名マスタ対応
      *  01.01.02    NACL-藤原    02/09/12  ディレクトリをサブプロへ
      *  01.01.03    NACL-藤原    02/11/25  ＳＰＡパラメタファイルの
      *                                     端末ＩＤ変更
      *  01.01.04    NACL-藤原    03/06/03  未コード化傷病名コードのとき
      *                                     病名よりコード検索をする
      *                                     病名からのコード検索方法の修正
      *  01.01.05    NACL-藤原    03/06/25  確認画面からの戻りのとき画面が
      *                                     残らないように修正
      *  01.01.06    NACL-藤原    03/07/17  入力ＣＤの長さを更新前にチェ
      *                                     ックするよう追加
      *  01.01.07    NACL-藤原    03/08/27  病名から検索したコードがエラー
      *                                     のときの確認画面を表示
      *  01.01.08    NACL-藤原    03/08/28  病名から検索したコードが廃止病名
      *                                     のときの処理を修正
      *
      *  新患者病名マスタ対応
      *  01.02.01    NACL-藤原    03/08/28  登録数の制限をなくす
      *                                     修飾語のみの登録を可能にする 
      *                                     病名種別の追加
      *  01.02.02    NACL-藤原    04/02/28  病名コード欄の追加
      *                                     分類選択時の前次頁処理の修正
      *  01.02.03    NACL-藤原    04/03/15  病名入力後の入力コード入力時の
      *                                     項目クリアをやめる
      *  00.02.04    NACL-藤原    04/11/01  病名が修飾語コードのみで構成
      *                                     されている場合種別を病名に変
      *                                     更できるようにする
      *  00.02.05    NACL-藤原    05/02/22  コード入力時に予約コードの
      *                                     とき、編集病名にする
      *  ver.2.6.0
      *  00.02.06    NACL-藤原    05/04/20  移行病名警告メッセージの表示、
      *                                     廃止年月日警告メッセージ表示
      *                                     をシス管の設定より行う
      *  00.02.07    NACL-藤原    05/07/07  未コード化傷病名以外の編集病
      *                                     の種別を変更できるようにする
      *
      *  02.07.01    NACL-藤原    05/10/31  MONFUNC対応
      *
      *  02.08.01    NACL-藤原    06/03/22  改正対応
      *                                     疾患区分の表示を変更
      *  02.08.02    NACL-森脇    06/03/22  リスト出力順設定
      *
      *  03.00.01    NACL-藤原    06/06/19   C06,C07,C08 削除
      *
      *  03.01.01    NACL-藤原    06/06/01  病名入力コードのアルファベッ
      *                                     ト７文字対応
      *  03.01.02    NACL-藤原    06/08/01  選択後すぐに登録したときに
      *                                     エラーとならないようにする
      *  03.01.03    NACL-森脇    06/08/21  ＣＳＶ出力対応、MONFUNC対応
      *  03.01.04    NACL-森脇    06/08/31  ジョブ管理削除、確認ボタン追加
      *
      *  03.04.01    NACL-藤原    06/11/27   ＳＰＡデータの削除
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.02.01    NACL-藤原    08/03/03  特定疾患区分07,08の追加
      *
      *  04.03.01    NACL-藤原    08/06/11  慢性と急性のチェック
      *
      *  04.06.01    NACL-藤原    10/06/14  自院病名コード変更対応
      *  04.06.02    NACL-藤原    10/06/16  疑いフラグ設定対応
      *  04.06.03    NACL-藤原    10/06/21  自院病名に不明な病名コードが
      *                                     登録された場合のチェックを追加
      *  04.06.04    NACL-藤原    10/08/25  一括入力対応
      *  04.06.05    NACL-藤原    10/10/20  難病外来指導管理料対応
      *  04.06.06    NACL-藤原    10/10/26  同義語変換対応
      *  04.06.07    NACL-藤原    11/07/04  廃止病名入力時の病名種別の表示
      *  04.06.08    NACL-藤原    11/11/22  種別変更時の登録前チェックの
      *                                     変更
      *  04.06.09    NACL-藤原    12/02/10  入力コード欄よりコード入力後
      *                                     自院病名コード変更時の削除処
      *                                     理を行うようにする
      *  04.06.10    NACL-藤原    12/10/01  入力病名の入力ＣＤ登録対応
      *
      *  04.07.01    NACL-藤原    11/06/10  未コード化病名となる病名の別
      *                                     組み立てへの移行を可能にする
      *  04.07.02    NACL-藤原    13/06/20  廃止病名警告表示の条件変更
      *  04.07.03    NACL-藤原    13/07/05  病名の複数自院病名登録対応
      *  04.07.04    NACL-藤原    13/07/30  自院病名のセット登録対応
      *  04.07.05    NACL-藤原    13/11/15  病名入力時のコード置換え画面
      *                                     の表示
      *  04.07.06    NACL-藤原    13/11/15  更新時のopid 設定
      *  04.07.07    NACL-藤原    13/12/26  病名入力時のコード置換え画面
      *                                     の表示時の移行先のチェック
      *  04.07.08    NACL-藤原    14/01/29  病名入力時のコード置換え画面
      *                                     の表示時のチェック追加
      *  04.07.09    NACL-藤原    14/03/14  複数自院病名登録時のコード
      *                                     存在チェックの修正
      *
      *  04.08.01    NACL-藤原    13/02/05  編集病名の強調表示
      *  04.08.02    NACL-藤原    15/12/02  病名入力時の廃止年月日変換の
      *                                     条件追加
      *  04.08.03    NACL-藤原    16/02/10  自院病名への補足コメント追加
      *                                     対応
      *  04.08.04    NACL-藤原    17/07/10  移行、置換時の特定疾患の表示
      *
      *  05.00.01    NACL-藤原    18/11/22  Warning 対応
      *  05.00.02    NACL-藤原    18/12/06  入力コードのチェック変更
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *   
           COPY    "ENUM-VALUE".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    病名登録用共通パラメタ
           COPY    "C01COMMON-SPA".
      *
      *    病名登録用画面ＳＰＡ
           COPY    "C02SCR-SPA".
           02  SPA-C02-PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING  //PTBYO-//
                                   BY         //SPA-C02-PTBYO-//.
           02  WRK-C02-HAISIYMD-OCC.
               03  WRK-C02-HAISIYMD-O  PIC 9(08)   OCCURS  6.
           02  WRK-C02-PTBYOMEI-REC    OCCURS  6.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING   //PTBYO-//
                                   BY          //WRK-C02-PTBYO-//.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-BYOMEI          PIC 9(01).
           03  FLG-USERBYOMEI      PIC 9(01).
           03  FLG-SYNONYM         PIC 9(01).
           03  FLG-JOBEND          PIC 9(01).
           03  FLG-BYOMEIINPUTSET  PIC 9(01).
      *
           03  FLG-ROW             PIC 9(01).
           03  FLG-MANSEI          PIC 9(01).
           03  FLG-KYUSEI          PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-BYOMEICD-CHK    PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDW                 PIC 9(04).
           03  IDX1                PIC 9(04). 
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDXX                PIC 9(04). 
           03  IDYY                PIC 9(04).
           03  IDZZ                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-CIDMSG          PIC X(100).
           03  WRK-NUM             PIC ZZZZ9.
           03  WRK-HENFLG          PIC 9(01).
           03  WRK-SELNO           PIC 9(05). 
           03  WRK-RENNUM          PIC 9(02).
           03  WRK-GMN-TBL         PIC X(1000).
           03  WRK-NAI-TBL         PIC X(1000). 
           03  WRK-WIDGET.
               05  WRK-WIDGET1     PIC X(09).
               05  WRK-WIDGET2     PIC 9(01).  
           03  WRK-LENGTH          PIC 9(02).
           03  WRK-BYOMEI-AREA.
               05  WRK-BYOMEI      PIC X(100).
               05  WRK-CHARTBYOMEI PIC X(100).
               05  WRK-UTAGAIFLG   PIC X(01).
               05  WRK-BYOMEISBT   PIC 9(01).
               05  WRK-RECECD-TBL.
                   07  WRK-RECECD  OCCURS  4
                                   PIC X(01).
               05  WRK-MOJISU      PIC 9(03).
               05  WRK-BYOMEI1     PIC X(800).
      *
           03  WRK-STR             PIC 9(05).
      *                             
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *     
           03  WRK-HAISIYMDH       PIC X(22).
      * 
           03  WRK-SYOKBN          PIC 9(01).
           03  WRK-UPDFLG-TBL.
               05  WRK-UPDFLG      PIC 9(01)   OCCURS  3.
           03  WRK-BANGO           PIC S9(05).
      *
           03  WRK-C03-BYO-ROW     PIC S9(09). 
      * 
           03  WRK-SEIGYO          PIC X(01).
      *
           03  WRK-BYOMEICD        PIC X(08).
      *
           03  WRK-ERRCD           PIC X(04).
      *
           03  WRK-LNK-BYO-INDEX   PIC 9(02).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-SHORIERR        PIC X(200).
      *
           03  WRK-ERR-BYOMEI      PIC X(80).
      *
           03  WRK-BYOMEIINPUTCD   PIC X(10).
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-GMN-C03-BUNRUICD-OLD
                                   PIC 9(03).
           03  WRK-GMN-C03-BUNRUIMEI-OLD
                                   PIC X(30).
      *
      *    退避警告メッセージ
           03  WRK-SPA-CIDCD-AREA.
               05  WRK-SPA-CIDCD   PIC X(04).
               05  WRK-SPA-CIDCD2  PIC X(04).
               05  WRK-SPA-CIDCD3  PIC X(04).
               05  WRK-SPA-CIDCD4  PIC X(04).
               05  WRK-SPA-CIDCD5  PIC X(04).
               05  WRK-SPA-CIDCD6  PIC X(04).
           03  WRK-SPA-CID1-AREA.
               05  WRK-SPA-CID1-FLG
                                   PIC X(02).
               05  WRK-SPA-CID2-FLG
                                   PIC X(02).
               05  WRK-SPA-CID3-FLG
                                   PIC X(03).
               05  WRK-SPA-CID4-FLG
                                   PIC X(03).
               05  WRK-SPA-CID5-FLG
                                   PIC X(03).
               05  WRK-SPA-CID6-FLG
                                   PIC X(03).
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    ワーク病名マスタ
       01  WRK-BYOMEI-REC.
           COPY    "CPBYOMEI.INC"  REPLACING   //BYO-//
                                   BY          //WRK-BYO-//.
      *
       01  WRK-CONS-AREA.
      *    病名コード数
           03  WRK-CONS-BYOMEICD-MAX
                                   PIC 9(02)   VALUE   21.
      *    自院コード一覧行数
           03  WRK-TBL-MAX         PIC 9(03)   VALUE   100.
      *    分類一覧行数
           03  WRK-BUNRUI-MAX      PIC 9(03)   VALUE   100.
      *    未コード化傷病名コード
           03  WRK-CONS-BYOMEICD   PIC X(07)   VALUE   "0000999".                                     
      *    の疑い
           03  WRK-CONS-UTAGAI     PIC X(07)   VALUE   "ZZZ8002".                                     
      *
       01  SPAN-COLOR-AREA.
           03  SPAN-COLOR-RED      PIC X(18)    VALUE
                                                "<span color=""red"">".  
           03  SPAN-COLOR-BLUE     PIC X(19)    VALUE
                                                "<span color=""blue"">".  
           03  SPAN-COLOR-GREEN    PIC X(20)    VALUE
                                               "<span color=""green"">".  
           03  SPAN-COLOR-END      PIC X(07)    VALUE
                                               "</span>".  
      *
       01  WRK-HEN-AREA.
           03  WRK-NENGO           PIC X(04).
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *   印刷用呼出SHELL
       01  SHELL-AREA.
           03  SHELL-NAME          PIC X(27)   VALUE
               "scripts/allways/orhcml09.sh".
           03  SHELL-TERMID        PIC X(64).
           03  SHELL-SYSYMD        PIC X(8).
           03  SHELL-HOSPID        PIC X(24).   
      *
      *    病名システム予約コードテーブル
           COPY    "CMBYOMEICD.INC".
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-JOBID         PIC 9(07)   VALUE   1.
           03  CONST-JOB-SHELLID   PIC X(08)   VALUE   "ORHCML09".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    病名マスタ−
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    自院病名マスタ−
       01  USERBYOMEI-REC.
           COPY    "CPUSERBYOMEI.INC".
      *
      *    同義語マスタ
       01  SYNONYM-REC.
           COPY    "CPSYNONYM.INC".
      *
      *    病名入力セット
       01  BYOMEIINPUTSET-REC.
           COPY    "CPBYOMEIINPUTSET.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    病名コード検索サブ
           COPY    "CPORCSBYOMEI.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    病名組立サブ
           COPY    "CPORCSMKBYOMEI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    MCPAREA.
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA22SCRAREA.INC".
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
           MOVE    SPA-WORK        TO  SPA-C01KYOUTU
           MOVE    SPA-FREE        TO  SPA-C02FREE
      *
           MOVE    SPA-ERRCD       TO  WRK-ERRCD
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"         ALSO    "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-C01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-C02FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面又はコード一覧より戻る
           IF      SPA-MOTOPG      =   "C09"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
               IF      SPA-MOTOPG      =   "CERR"
                   MOVE    SPACE           TO  SPA-MOTOPG
                   IF      WRK-ERRCD       =   "0074"
                       MOVE    SPA-C03-HAISI-INDEX
                                               TO  IDZ
                       INITIALIZE                  SPA-C03-HAISI-AREA
                       PERFORM 41021-BYOMEICD-HENSYU-SEC
                       IF      SPA-ERRCD       NOT =   SPACE
                           PERFORM 510-ERRSET-SEC
                           GO  TO  100-INIT-EXT
                       END-IF
                   ELSE
                       IF      SPA-C03-ERR-INDEX   >   ZERO
                           COMPUTE SPA-GMN-C03-CUR
                                           =   4   +   SPA-C03-ERR-INDEX
                       END-IF
                   END-IF
                   INITIALIZE                  SPA-C03-HAISI-AREA
                   MOVE    ZERO            TO  SPA-C03-ERR-INDEX
                   PERFORM 5001-MAPCUR-SEC
               ELSE
      *            初期画面セット
                   PERFORM 300-SCREEN-SEC
                   IF      FLG-END         =   1
                       GO  TO  100-INIT-EXT
                   END-IF
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                       GO  TO  100-INIT-EXT
                   END-IF
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    SPACE           TO  MCP-PUTTYPE
           MOVE    "C03"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *
      *        元の画面へ戻す
               IF    (  SPA-MOTOPG     =   "XB01" )
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-C02FREE
                   INITIALIZE                  SPA-C01KYOUTU
                   GO  TO  300-SCREEN-EXT
               END-IF
           END-IF
      *
           IF    SPA-MOTOPG        =   "C99"
               GO  TO  300-SCREEN-EXT
           END-IF
      *
           EVALUATE    TRUE
      *            病名検索より
               WHEN    SPA-MOTOPG(1:3)  =  "C05"
                   PERFORM 320-C05SPASET-SEC
                   MOVE    12              TO  SPA-GMN-C03-CUR
                   GO  TO  300-SCREEN-EXT
      *            確認画面より
               WHEN    SPA-MOTOPG(1:4)  =  "CID1"
                   PERFORM 330-CID1-SET-SEC
                   GO  TO  300-SCREEN-EXT
      *            病名移行確認画面より
               WHEN    SPA-MOTOPG(1:4)  =  "CID2"
                   PERFORM 340-CID2-SET-SEC
                   GO  TO  300-SCREEN-EXT
      *            確認画面３より
               WHEN    SPA-MOTOPG(1:4)  =  "CID3"
                   PERFORM 350-CID3-SET-SEC
                   GO  TO  300-SCREEN-EXT
      *            確認画面４より
               WHEN    SPA-MOTOPG(1:4)  =  "CID4"
                   PERFORM 360-CID4-SET-SEC
                   GO  TO  300-SCREEN-EXT
      *            確認画面５より
               WHEN    SPA-MOTOPG(1:4)  =  "CID5"
                   PERFORM 360-CID5-SET-SEC
                   GO  TO  300-SCREEN-EXT
      *            確認画面６より
               WHEN    SPA-MOTOPG(1:4)  =  "CID6"
                   PERFORM 370-CID6-SET-SEC
                   GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面よりの処理
      *****************************************************************
       330-CID1-SET-SEC            SECTION.
      *
           EVALUATE    CID1-ID1CODE
               WHEN    "0101"
      *            戻るの確認
                   IF      SPA-CID1-FLG        =   "OK"
                       MOVE    SPACE               TO  SPA-C01KYOUTU
                       MOVE    "JOIN"              TO  MCP-PUTTYPE
                       PERFORM 210-BACK
                   END-IF
               WHEN    "0105"
               WHEN    "0106"
      *            慢性と急性の混在または分類名警告時の更新の確認
                   IF      SPA-CID1-FLG        =   "OK"
                       PERFORM 4502-TOUROKU-SYORI-SEC
                   END-IF    
           END-EVALUATE
      *     
           MOVE    1               TO  SPA-GMN-C03-CUR
      *     
           INITIALIZE                  SPA-CIDCD-AREA
                                       SPA-CID1-AREA  
           INITIALIZE                  SPA-C03-IKOSAKI-AREA
                                       SPA-C03-CHANGE-AREA
                                       SPA-C03-HAISI-AREA
           .
       330-CID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     病名移行確認画面よりの処理
      *****************************************************************
       340-CID2-SET-SEC            SECTION.
      *
           MOVE    SPA-CIDCD-AREA  TO  WRK-SPA-CIDCD-AREA
           MOVE    SPA-CID1-AREA   TO  WRK-SPA-CID1-AREA
      *
           EVALUATE    SPA-CIDCD2
               WHEN    "0101"
               WHEN    "0102"
      *            選択病名移行先コード処理
                   IF      SPA-CID2-FLG    =   "OK"
                       MOVE    SPA-C03-IKOSAKI-INDEX
                                           TO  IDZ
                       MOVE    SPA-C03-IKOSAKI-BYOMEICD
                                           TO  SPA-NAI-C03-BYOMEICD(IDZ)
                       IF      SPA-GMN-C03-BYOMEICD(IDZ)
                                           NOT =   SPACE
                          MOVE    SPA-NAI-C03-BYOMEICD(IDZ)
                                           TO  SPA-GMN-C03-BYOMEICD(IDZ)
                       END-IF
                       MOVE    SPA-NAI-C03-BYOMEICD(IDZ)
                                           TO  BYO-BYOMEICD
                       PERFORM 410111-BYOMEI-HENSYU-SEC
                   END-IF              
                   PERFORM 4101111-BYOMEI-SELECT-HEN-SEC
                   PERFORM 41021-BYOMEI-HENSYU-SEC
               WHEN    "0103"
               WHEN    "0104"
      *            入力病名コード移行先コード処理
                   IF      SPA-CID2-FLG    =   "OK"
                       MOVE    SPA-C03-IKOSAKI-INDEX
                                           TO  IDZ
                       MOVE    SPA-C03-IKOSAKI-BYOMEICD
                                           TO  SPA-GMN-C03-BYOMEICD(IDZ)
                       MOVE    SPA-GMN-C03-BYOMEICD(IDZ)
                                           TO  BYO-BYOMEICD
                       PERFORM 410111-BYOMEI-HENSYU-SEC
                   END-IF
                   MOVE    SPACE           TO  SPA-CIDCD2
                   MOVE    SPA-C03-IKOSAKI-INDEX
                                           TO  IDZ
                   PERFORM 41021-BYOMEICD-HENSYU-SEC
               WHEN    "0105"
               WHEN    "0106"
      *            検索病名コード移行先コード処理
                   IF      SPA-CID2-FLG    =   "OK"
                       MOVE    SPA-C03-IKOSAKI-INDEX
                                           TO  IDZ
                       MOVE    SPA-C03-IKOSAKI-BYOMEICD
                                           TO  SPA-NAI-C03-BYOMEICD(IDZ)
                       MOVE    SPA-NAI-C03-BYOMEICD(IDZ)
                                           TO  BYO-BYOMEICD
                       PERFORM 410111-BYOMEI-HENSYU-SEC
                   END-IF
                   PERFORM 41021-BYOMEI-HENSYU-SEC
                   MOVE    12                  TO  SPA-GMN-C03-CUR
           END-EVALUATE
      *     
           INITIALIZE                  SPA-CIDCD-AREA
                                       SPA-CID1-AREA
           INITIALIZE                  SPA-C03-IKOSAKI-AREA
                                       SPA-C03-CHANGE-AREA
                                       SPA-C03-HAISI-AREA
           INITIALIZE                  WRK-SPA-CIDCD-AREA
                                       WRK-SPA-CID1-AREA
           .
       340-CID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面３よりの処理
      *****************************************************************
       350-CID3-SET-SEC            SECTION.
      *
           IF      SPA-CIDCD3          =   "0101"
               EVALUATE    SPA-CID3-FLG
      *            分類名の削除
                   WHEN    "OK "
                       INITIALIZE                  USBYO-KEY
      *****************MOVE    SPA-HOSPID          TO  USBYO-HOSPID
                       MOVE    SPA-GMN-C03-BUNRUICD    TO
                                                   USBYO-BYOMEIINPUTCD
                       MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
                       MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
                       MOVE    "DBDELETE"          TO  MCP-FUNC
                       MOVE    "tbl_userbyomei"    TO  MCP-TABLE
                       MOVE    "del1"              TO  MCP-PATHNAME
                       CALL    "ORCDBMAIN"         USING
                                                       MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                       IF      MCP-RC          NOT =   ZERO 
                           MOVE    "1002"              TO  SPA-ERRCD
                           DISPLAY "USERBYOMEI-KEY =>" USBYO-KEY 
                           GO  TO  350-CID3-SET-EXT
                       END-IF
      *            分類名と分類の属する病名全ての削除
                   WHEN    "ALL"
                       INITIALIZE                  USBYO-KEY
      *****************MOVE    SPA-HOSPID          TO  USBYO-HOSPID
                       MOVE    SPA-GMN-C03-BUNRUICD
                                                   TO
                                                   USBYO-BYOMEIINPUTCD
                       MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
                       MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
                       MOVE    "DBDELETE"          TO  MCP-FUNC
                       MOVE    "tbl_userbyomei"    TO  MCP-TABLE
                       MOVE    "del1"              TO  MCP-PATHNAME
                       CALL    "ORCDBMAIN"         USING
                                                       MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
      *
                       INITIALIZE                  USBYO-KEY
      *****************MOVE    SPA-HOSPID          TO  USBYO-HOSPID
                       MOVE    SPA-GMN-C03-BUNRUICD
                                                   TO  USBYO-BUNRUICD
                       MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
                       MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
                       MOVE    "DBDELETE"          TO  MCP-FUNC
                       MOVE    "tbl_userbyomei"    TO  MCP-TABLE
                       MOVE    "del2"              TO  MCP-PATHNAME
                       CALL    "ORCDBMAIN"         USING
                                                       MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                       IF      MCP-RC          NOT =   ZERO 
                           MOVE    "1002"              TO  SPA-ERRCD
                           DISPLAY "USERBYOMEI-KEY =>" USBYO-KEY 
                           GO  TO  350-CID3-SET-EXT
                       END-IF
               END-EVALUATE
           END-IF
      *     
           EVALUATE    SPA-CID3-FLG
      *        分類名の削除
               WHEN    "OK "
               WHEN    "ALL"
                   INITIALIZE                  SPA-GMN-C03-KOUMOKU
                                               SPA-NAI-C03-KOUMOKU
                                               SPA-NAI-C03-SELNO
                                               SPA-NAI-C03-BUNRUI-SELNO
      *
      *            分類一覧
                   INITIALIZE                  SPA-GMN-C03-BUNRUICD-TBL
                   PERFORM 3102-BUNRUI-LST-SEC
      *
      *            自院病名一覧
      *************IF      SPA-CID3-FLG        =   "ALL"
                       INITIALIZE              SPA-GMN-C03-BYOMEI-TBL
                                               SPA-NAI-C03-TBL
                       MOVE    ZERO                TO  SPA-GMN-C03-MAX
                       MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
                       MOVE    "key3"              TO  WRK-MCP-PATHNAME
                       INITIALIZE                  USERBYOMEI-REC
      *****************MOVE    SPA-HOSPID          TO  USBYO-HOSPID
                       PERFORM 3101-USERBYOMEI-LST-SEC
      *************END-IF
      *
                   MOVE    1               TO  SPA-GMN-C03-CUR
           END-EVALUATE
      *     
           INITIALIZE                  SPA-CIDCD-AREA
                                       SPA-CID1-AREA
           INITIALIZE                  SPA-C03-IKOSAKI-AREA
                                       SPA-C03-CHANGE-AREA
                                       SPA-C03-HAISI-AREA
           .
       350-CID3-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面４よりの処理
      *****************************************************************
       360-CID4-SET-SEC            SECTION.
      *
           EVALUATE    SPA-CIDCD4
               WHEN    "0103"
      *            分類名の削除の確認
                   IF      SPA-CID4-FLG        =   "OK"
                       INITIALIZE                  USBYO-KEY
                       MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
                       MOVE    SPA-GMN-C03-BUNRUICD    TO
                                                   USBYO-BYOMEIINPUTCD
                       MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
                       MOVE    "DBDELETE"          TO  MCP-FUNC
                       MOVE    "tbl_userbyomei"    TO  MCP-TABLE
                       MOVE    "del1"              TO  MCP-PATHNAME
                       CALL    "ORCDBMAIN"         USING
                                                       MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                       IF      MCP-RC          NOT =   ZERO 
                           MOVE    "1002"              TO  SPA-ERRCD
                           DISPLAY "USERBYOMEI-KEY =>" USBYO-KEY 
                           GO  TO  360-CID4-SET-EXT
                       END-IF
      *                分類一覧
                       INITIALIZE              SPA-GMN-C03-BUNRUICD-TBL
                                               SPA-NAI-C03-BUNRUI-SELNO
                       PERFORM 3102-BUNRUI-LST-SEC
      *                自院病名一覧
                       INITIALIZE              SPA-GMN-C03-BYOMEI-TBL
                                               SPA-NAI-C03-TBL
                                               SPA-GMN-C03-KOUMOKU
                                               SPA-NAI-C03-KOUMOKU
                                               SPA-NAI-C03-SELNO
                       MOVE    ZERO                TO  SPA-GMN-C03-MAX
                       MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
                       MOVE    "key3"              TO  WRK-MCP-PATHNAME
                       INITIALIZE                  USERBYOMEI-REC
      *****************MOVE    SPA-HOSPID          TO  USBYO-HOSPID
                       PERFORM 3101-USERBYOMEI-LST-SEC
                   END-IF
               WHEN    "0104"
      *            自院病名の削除の確認
                   IF      SPA-CID4-FLG        =   "OK"
                       INITIALIZE                  USBYO-KEY
      *****************MOVE    SPA-HOSPID          TO  USBYO-HOSPID
                       MOVE    SPA-NAI-C03-CDSYU         (1)
                                                   TO  USBYO-CDSYU
                       MOVE    SPA-GMN-C03-BYOMEIINPUTCD (1)
                                               TO  USBYO-BYOMEIINPUTCD 
                       MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
                       MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
                       MOVE    "DBDELETE"          TO  MCP-FUNC
                       MOVE    "tbl_userbyomei"    TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       CALL    "ORCDBMAIN"         USING
                                                       MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                       IF      MCP-RC      NOT =   ZERO 
                           MOVE    "1002"          TO  SPA-ERRCD
                           DISPLAY "USERBYOMEI-KEY =>" USBYO-KEY 
                           GO  TO  360-CID4-SET-EXT
                       END-IF
      *                自院病名一覧
                       INITIALIZE              SPA-GMN-C03-BYOMEI-TBL
                                               SPA-NAI-C03-TBL
                                               SPA-GMN-C03-KOUMOKU
                                               SPA-NAI-C03-KOUMOKU
                                               SPA-NAI-C03-SELNO
                       MOVE    ZERO            TO  SPA-GMN-C03-MAX
                       INITIALIZE              USERBYOMEI-REC
      *****************MOVE    SPA-HOSPID          TO  USBYO-HOSPID
                       IF      SPA-NAI-C03-BUNRUI-SELNO    >   ZERO
                           MOVE
                               SPA-GMN-C03-BBUNRUICD
                                           (SPA-NAI-C03-BUNRUI-SELNO)
                                                       TO
                                                       USBYO-BUNRUICD
                           MOVE    "tbl_userbyomei"    TO
                                                       WRK-MCP-TABLE
                           MOVE    "key7"              TO
                                                       WRK-MCP-PATHNAME
                       ELSE
                           MOVE    "tbl_userbyomei"    TO
                                                       WRK-MCP-TABLE
                           MOVE    "key3"              TO
                                                       WRK-MCP-PATHNAME
                       END-IF
                       PERFORM 3101-USERBYOMEI-LST-SEC
                       IF    ( SPA-GMN-C03-MAX         =   ZERO )
                       AND   ( SPA-GMN-C03-PAGE        >   1    )
                           COMPUTE SPA-GMN-C03-PAGE
                                                   =   SPA-GMN-C03-PAGE
                                                   -   1
                           INITIALIZE                  USERBYOMEI-REC
      *********************MOVE    SPA-HOSPID          TO  USBYO-HOSPID
                           IF      SPA-NAI-C03-BUNRUI-SELNO    >   ZERO
                               MOVE    SPA-GMN-C03-BBUNRUICD 
                                           (SPA-NAI-C03-BUNRUI-SELNO)
                                                           TO
                                                       USBYO-BUNRUICD
                               MOVE    "tbl_userbyomei"    TO
                                                       WRK-MCP-TABLE
                               MOVE    "key7"              TO
                                                       WRK-MCP-PATHNAME
                           ELSE
                               MOVE    "tbl_userbyomei"    TO
                                                       WRK-MCP-TABLE
                               MOVE    "key3"              TO
                                                       WRK-MCP-PATHNAME
                           END-IF
                           PERFORM 3101-USERBYOMEI-LST-SEC
                       END-IF
                   END-IF
           END-EVALUATE
      *     
           MOVE    1               TO  SPA-GMN-C03-CUR
      *
           INITIALIZE                  SPA-CIDCD-AREA
                                       SPA-CID1-AREA
           INITIALIZE                  SPA-C03-IKOSAKI-AREA
                                       SPA-C03-CHANGE-AREA
                                       SPA-C03-HAISI-AREA
           .
       360-CID4-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面５よりの処理
      *****************************************************************
       360-CID5-SET-SEC            SECTION.
      *
           IF      SPA-CIDCD5          =   "0101"
               IF      SPA-CID5-FLG    NOT =   SPACE
                   PERFORM 490-PRINT-SEC
               END-IF
           END-IF
      *
           INITIALIZE                  SPA-CIDCD-AREA
                                       SPA-CID1-AREA
           INITIALIZE                  SPA-C03-IKOSAKI-AREA
                                       SPA-C03-CHANGE-AREA
                                       SPA-C03-HAISI-AREA
           .
       360-CID5-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面６からの戻り処理
      *****************************************************************
       370-CID6-SET-SEC            SECTION.
      *
           MOVE    SPA-CIDCD-AREA  TO  WRK-SPA-CIDCD-AREA
           MOVE    SPA-CID1-AREA   TO  WRK-SPA-CID1-AREA
      *
           EVALUATE    SPA-CIDCD6
               WHEN    "0101"
               WHEN    "0102"
      *            選択病名移行先コード処理
               WHEN    "0108"
      *            選択病名コード未コード化病名処理
               WHEN    "0109"
      *            選択病名コード廃止コード処理
                   IF      SPA-CID6-FLG    =   "OK"
                       MOVE    SPA-C03-IKOSAKI-INDEX
                                           TO  IDZ
                       MOVE    SPA-C03-IKOSAKI-BYOMEICD
                                           TO  SPA-NAI-C03-BYOMEICD(IDZ)
                       IF      SPA-GMN-C03-BYOMEICD(IDZ)
                                           NOT =   SPACE
                          MOVE    SPA-NAI-C03-BYOMEICD(IDZ)
                                           TO  SPA-GMN-C03-BYOMEICD(IDZ)
                       END-IF
                       MOVE    SPA-NAI-C03-BYOMEICD(IDZ)
                                           TO  BYO-BYOMEICD
                       PERFORM 410111-BYOMEI-HENSYU-SEC
                   END-IF              
                   IF      SPA-CID6-FLG    =   "CHG"
                       INITIALIZE              SPA-GMN-C03-BYOMEICD-G
                                               SPA-NAI-C03-BYOMEI-TBL
                       PERFORM VARYING IDXX    FROM   1   BY  1
                               UNTIL   IDXX    >      SPA-C03-CHANGE-MAX
                           MOVE    IDXX    TO  IDZ
                           MOVE    SPA-C03-CHANGE-BYOMEICD (IDZ)
                                           TO
                                           SPA-NAI-C03-BYOMEICD(IDZ)
                           IF    ( SPA-C03-CHANGE-MAX
                                                   <=  6     )
                              MOVE    SPA-NAI-C03-BYOMEICD(IDZ)
                                           TO  SPA-GMN-C03-BYOMEICD(IDZ)
                           END-IF
                           MOVE    SPA-NAI-C03-BYOMEICD(IDZ)
                                           TO  BYO-BYOMEICD
                           PERFORM 410111-BYOMEI-HENSYU-SEC
                       END-PERFORM
                   END-IF
                   PERFORM 4101111-BYOMEI-SELECT-HEN-SEC
                   PERFORM 41021-BYOMEI-HENSYU-SEC
               WHEN    "0103"
               WHEN    "0104"
      *            入力病名コード移行先、推奨コード処理
                   MOVE    SPA-C03-IKOSAKI-INDEX
                                           TO  IDZ
      *
                   IF      SPA-CID6-FLG    =   "OK"
                       MOVE    SPA-C03-IKOSAKI-INDEX
                                           TO  IDZ
                       MOVE    SPA-C03-IKOSAKI-BYOMEICD
                                           TO  SPA-GMN-C03-BYOMEICD(IDZ)
                       MOVE    SPA-GMN-C03-BYOMEICD(IDZ)
                                           TO  BYO-BYOMEICD
                       PERFORM 410111-BYOMEI-HENSYU-SEC
                   END-IF
                   IF      SPA-CID6-FLG    =   "CHG"
                       INITIALIZE              SPA-GMN-C03-BYOMEICD-G
                                               SPA-NAI-C03-BYOMEI-TBL
                       PERFORM VARYING IDXX    FROM   1   BY  1
                               UNTIL   IDXX    >      SPA-C03-CHANGE-MAX
                           MOVE    IDXX    TO  IDZ
                           MOVE    SPA-C03-CHANGE-BYOMEICD (IDZ)
                                           TO
                                           SPA-NAI-C03-BYOMEICD(IDZ)
                           IF      SPA-C03-CHANGE-MAX
                                                   <=  6
                               MOVE    SPA-C03-CHANGE-BYOMEICD (IDZ)
                                           TO
                                           SPA-GMN-C03-BYOMEICD(IDZ)
                           END-IF 
                           MOVE    SPA-NAI-C03-BYOMEICD (IDZ)
                                           TO  BYO-BYOMEICD
                           PERFORM 410111-BYOMEI-HENSYU-SEC
                       END-PERFORM
                       IF      SPA-C03-CHANGE-MAX
                                               <=  6
                           COMPUTE SPA-GMN-C03-CUR =   4 +
                                                   SPA-C03-CHANGE-MAX
                       ELSE
                           MOVE    10          TO  SPA-GMN-C03-CUR
                       END-IF
                   END-IF
                   MOVE    SPACE           TO  SPA-CIDCD6
                   INITIALIZE                  SPA-C03-IKOSAKI-AREA
                   PERFORM 41021-BYOMEICD-HENSYU-SEC
               WHEN    "0115"
               WHEN    "0116"
      *            入力病名移行先、推奨コード処理
                   MOVE    SPA-C03-IKOSAKI-INDEX
                                           TO  IDZ
      *
                   IF      SPA-CID6-FLG    =   "OK"
                       MOVE    SPA-C03-IKOSAKI-INDEX
                                           TO  IDZ
                       MOVE    SPA-C03-IKOSAKI-BYOMEICD
                                           TO  SPA-NAI-C03-BYOMEICD(IDZ)
                       MOVE    SPA-NAI-C03-BYOMEICD(IDZ)
                                           TO  BYO-BYOMEICD
                       PERFORM 410111-BYOMEI-HENSYU-SEC
                   END-IF
                   IF      SPA-CID6-FLG    =   "CHG"
                       INITIALIZE              SPA-NAI-C03-BYOMEI-TBL
                       PERFORM VARYING IDXX    FROM   1   BY  1
                               UNTIL   IDXX    >      SPA-C03-CHANGE-MAX
                           MOVE    IDXX    TO  IDZ
                           MOVE    SPA-C03-CHANGE-BYOMEICD (IDZ)
                                           TO
                                           SPA-NAI-C03-BYOMEICD(IDZ)
                           MOVE    SPA-NAI-C03-BYOMEICD (IDZ)
                                           TO  BYO-BYOMEICD
                           PERFORM 410111-BYOMEI-HENSYU-SEC
                       END-PERFORM
                       MOVE    12          TO  SPA-GMN-C03-CUR
                   END-IF
                   MOVE    SPACE           TO  SPA-CIDCD6
                   INITIALIZE                  SPA-C03-IKOSAKI-AREA
                   PERFORM 41021-BYOMEICD-HENSYU-SEC
               WHEN    "0107"
      *            入力病名コード廃止コード処理
                   MOVE    SPA-C03-HAISI-INDEX
                                           TO  IDZ
      *
                   IF      SPA-CID6-FLG    =   "CHG"
                       INITIALIZE              SPA-GMN-C03-BYOMEICD-G
                                               SPA-NAI-C03-BYOMEI-TBL
                       PERFORM VARYING IDXX    FROM   1   BY  1
                               UNTIL   IDXX    >      SPA-C03-CHANGE-MAX
                           MOVE    IDXX    TO  IDZ
                           MOVE    SPA-C03-CHANGE-BYOMEICD (IDZ)
                                           TO
                                           SPA-NAI-C03-BYOMEICD(IDZ)
                           IF      SPA-C03-CHANGE-MAX
                                                   <=  6
                               MOVE    SPA-C03-CHANGE-BYOMEICD (IDZ)
                                           TO
                                           SPA-GMN-C03-BYOMEICD(IDZ)
                           END-IF 
                           MOVE    SPA-NAI-C03-BYOMEICD (IDZ)
                                           TO  BYO-BYOMEICD
                           PERFORM 410111-BYOMEI-HENSYU-SEC
                       END-PERFORM
                       IF      SPA-C03-CHANGE-MAX
                                               <=  6
                           COMPUTE SPA-GMN-C03-CUR =   4 +
                                                   SPA-C03-CHANGE-MAX
                       ELSE
                           MOVE    10          TO  SPA-GMN-C03-CUR
                       END-IF
                   END-IF
                   MOVE    SPACE           TO  SPA-CIDCD6
                   INITIALIZE                  SPA-C03-HAISI-AREA
                   PERFORM 41021-BYOMEICD-HENSYU-SEC
               WHEN    "0117"
      *            入力病名廃止コード処理
                   MOVE    SPA-C03-HAISI-INDEX
                                           TO  IDZ
      *
                   IF      SPA-CID6-FLG    =   "CHG"
                       INITIALIZE              SPA-NAI-C03-BYOMEI-TBL
                       PERFORM VARYING IDXX    FROM   1   BY  1
                               UNTIL   IDXX    >      SPA-C03-CHANGE-MAX
                           MOVE    IDXX    TO  IDZ
                           MOVE    SPA-C03-CHANGE-BYOMEICD (IDZ)
                                           TO
                                           SPA-NAI-C03-BYOMEICD(IDZ)
                           MOVE    SPA-NAI-C03-BYOMEICD (IDZ)
                                           TO  BYO-BYOMEICD
                           PERFORM 410111-BYOMEI-HENSYU-SEC
                       END-PERFORM
                       MOVE    12          TO  SPA-GMN-C03-CUR
                   END-IF
                   MOVE    SPACE           TO  SPA-CIDCD6
                   INITIALIZE                  SPA-C03-HAISI-AREA
                   PERFORM 41021-BYOMEICD-HENSYU-SEC
           END-EVALUATE
      *     
           INITIALIZE                  SPA-CIDCD-AREA
                                       SPA-CID1-AREA
           INITIALIZE                  SPA-C03-IKOSAKI-AREA
                                       SPA-C03-CHANGE-AREA
                                       SPA-C03-HAISI-AREA
           INITIALIZE                  WRK-SPA-CIDCD-AREA
                                       WRK-SPA-CID1-AREA
           .
       370-CID6-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-C03-AREA
      *
      *    自院病名更新フラグ
           MOVE    SPA-C01-HENFLG  TO  SPA-NAI-C03-HENFLG
      *
      *    疑い
           MOVE    "1"             TO  SPA-GMN-C03-UTAGAIFLGL       (1)
           MOVE    "疑い"          TO  SPA-GMN-C03-UTAGAIFLGMEIL    (1)
           MOVE    "2"             TO  SPA-GMN-C03-UTAGAIFLGL       (2)
           MOVE    "急性"          TO  SPA-GMN-C03-UTAGAIFLGMEIL    (2)
           MOVE    "3"             TO  SPA-GMN-C03-UTAGAIFLGL       (3)
           MOVE    "急性かつ疑い"  TO  SPA-GMN-C03-UTAGAIFLGMEIL    (3)
           MOVE    3               TO  SPA-GMN-C03-UTAGAIFLG-MAX
      *
           MOVE    "03"            TO  SPA-GMN-C03-MANSEIKBNL       (1)
           MOVE    "皮膚科特定疾患指導管理料(１)"
                                   TO  SPA-GMN-C03-MANSEIKBNMEIL    (1)
           MOVE    "皮膚（１）"    TO  SPA-GMN-C03-MANSEIKBNTANL    (1)
           MOVE    "04"            TO  SPA-GMN-C03-MANSEIKBNL       (2)
           MOVE    "皮膚科特定疾患指導管理料(２)"
                                   TO  SPA-GMN-C03-MANSEIKBNMEIL    (2)
           MOVE    "皮膚（２）"    TO  SPA-GMN-C03-MANSEIKBNTANL    (2)
           MOVE    "05"            TO  SPA-GMN-C03-MANSEIKBNL       (3)
           MOVE    "特定疾患療養管理料"
                                   TO  SPA-GMN-C03-MANSEIKBNMEIL    (3)
           MOVE    "特定疾患"      TO  SPA-GMN-C03-MANSEIKBNTANL    (3)
           MOVE    "07"            TO  SPA-GMN-C03-MANSEIKBNL       (4)
           MOVE    "てんかん指導料"
                                   TO  SPA-GMN-C03-MANSEIKBNMEIL    (4)
           MOVE    "てんかん"      TO  SPA-GMN-C03-MANSEIKBNTANL    (4)
           MOVE    "08"            TO  SPA-GMN-C03-MANSEIKBNL       (5)
           MOVE    "特定疾患療養管理料又はてんかん指導料"
                                   TO  SPA-GMN-C03-MANSEIKBNMEIL    (5)
           MOVE    "特定・てん"    TO  SPA-GMN-C03-MANSEIKBNTANL    (5)
           MOVE    "09"            TO  SPA-GMN-C03-MANSEIKBNL       (6)
           MOVE    "難病外来指導管理料"
                                   TO  SPA-GMN-C03-MANSEIKBNMEIL    (6)
           MOVE    "難病外来"      TO  SPA-GMN-C03-MANSEIKBNTANL    (6)
           MOVE    6               TO  SPA-GMN-C03-MANSEIKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-C03-HKNBYOMEIFLGL    (1)
           MOVE    "保険病名"      TO  SPA-GMN-C03-HKNBYOMEIFLGMEIL (1)
           MOVE    1               TO  SPA-GMN-C03-HKNBYOMEIFLG-MAX
      *
           MOVE    "0"             TO  SPA-GMN-C03-BYOMEISBTL    (1)
           MOVE    "病名"          TO  SPA-GMN-C03-BYOMEISBTMEIL (1)
           MOVE    "1"             TO  SPA-GMN-C03-BYOMEISBTL    (2)
           MOVE    "部位"          TO  SPA-GMN-C03-BYOMEISBTMEIL (2)
           MOVE    "7"             TO  SPA-GMN-C03-BYOMEISBTL    (3)
           MOVE    "接頭語"        TO  SPA-GMN-C03-BYOMEISBTMEIL (3)
           MOVE    "8"             TO  SPA-GMN-C03-BYOMEISBTL    (4)
           MOVE    "接尾語"        TO  SPA-GMN-C03-BYOMEISBTMEIL (4)
           MOVE    4               TO  SPA-GMN-C03-BYOMEISBT-MAX
      *
      *    分類一覧
           PERFORM 3102-BUNRUI-LST-SEC
      *
      *    自院病名一覧
           MOVE    1                   TO  SPA-GMN-C03-PAGE
      *
           MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
           MOVE    "key3"              TO  WRK-MCP-PATHNAME
           INITIALIZE                  USERBYOMEI-REC
      *****MOVE    SPA-HOSPID          TO  USBYO-HOSPID
           PERFORM 3101-USERBYOMEI-LST-SEC
      *
           IF      SPA-NAI-C03-HENFLG  =   1
      *        患者登録画面からの入力病名を編集
               PERFORM 3103-INPUT-BYOMEI-HENSYU-SEC
           ELSE
               MOVE    1                   TO  SPA-GMN-C03-CUR
           END-IF
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名一覧 編集処理
      *****************************************************************
       3101-USERBYOMEI-LST-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-USBYO-READ-SEC
           ELSE
               INITIALIZE                  USERBYOMEI-REC
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
      *
           COMPUTE WRK-STR         = ( SPA-GMN-C03-PAGE  -  1 )
                                   *   WRK-TBL-MAX
                                   +   1
      *
           MOVE    ZERO                TO  IDX
                                           IDX2
                                           SPA-GMN-C03-MAX
           PERFORM UNTIL ( IDX             >=  WRK-TBL-MAX ) OR
                         ( FLG-USERBYOMEI  =   1           )
      *
               ADD     1               TO  IDX2
               IF      IDX2            >=  WRK-STR
                   PERFORM 31011-USERBYOMEI-SPA-SEC
               END-IF
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-USBYO-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF    ( IDX                 >=  WRK-TBL-MAX  )  AND
                 ( FLG-USERBYOMEI      =   ZERO         )
               MOVE    1                   TO  SPA-GMN-C03-NEXT
           ELSE
               MOVE    ZERO                TO  SPA-GMN-C03-NEXT
           END-IF
      *
           MOVE    1                   TO  FLG-ROW
      *
           .
       3101-USERBYOMEI-LST-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名マスター情報 編集処理
      *****************************************************************
       31011-USERBYOMEI-SPA-SEC     SECTION.
      *
           ADD     1                   TO  IDX
      *  
           IF      IDX                 <=  WRK-TBL-MAX
      *
               MOVE    IDX2            TO  SPA-GMN-C03-TBANGO      (IDX)
      *    コード種別
               MOVE    USBYO-CDSYU     TO  SPA-NAI-C03-TCDSYU      (IDX)
      *    病名入力コード
               MOVE    USBYO-BYOMEIINPUTCD TO
                                       SPA-GMN-C03-TBYOMEIINPUTCD  (IDX)
      *    表示連番
               MOVE    USBYO-DSPSEQ    TO  SPA-GMN-C03-TDSPSEQ     (IDX)
      *    病名＋補足コメント
               INITIALIZE              ORCSMKBYOMEIAREA
               MOVE    USBYO-BYOMEI
                                       TO  SMKBYO-BYOMEI
               MOVE    USBYO-HOSOKU-COMMENT
                                       TO  SMKBYO-HOSOKU-COMMENT
               CALL    "ORCSMKBYOMEI"  USING   ORCSMKBYOMEIAREA
               MOVE    SMKBYO-BYOMEI   TO  SPA-GMN-C03-TBYOMEI     (IDX)
      *    カルテ病名
               MOVE    USBYO-CHARTBYOMEI
                                       TO  SPA-GMN-C03-TCHARTBYOMEI(IDX)
      *
      *    分類名
               IF      USBYO-BUNRUICD  NOT =   SPACE
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >  SPA-GMN-C03-BUNRUI-MAX
                       IF      SPA-GMN-C03-BBUNRUICD(IDY)  
                                           =   USBYO-BUNRUICD
                           MOVE    SPA-GMN-C03-BBUNRUIMEI(IDY)
                                       TO  SPA-GMN-C03-TBUNRUIMEI(IDX)
                           MOVE    SPA-GMN-C03-BUNRUI-MAX  TO  IDY
                       END-IF
                   END-PERFORM
               END-IF
      *
      *    疑いフラグ
               IF      USBYO-UTAGAIFLG  NOT =   SPACE
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >
                                               SPA-GMN-C03-UTAGAIFLG-MAX
                       IF      SPA-GMN-C03-UTAGAIFLGLST(IDY)(1:1)
                                               =   USBYO-UTAGAIFLG
                           MOVE    SPA-GMN-C03-UTAGAIFLGLST(IDY)(3:)
                                       TO  SPA-GMN-C03-TUTAGAIFLG (IDX)
                           MOVE    SPA-GMN-C03-UTAGAIFLG-MAX  TO  IDY
                       END-IF
                   END-PERFORM
               END-IF
      *
      *    慢性
               IF      USBYO-MANSEIKBN  NOT =   SPACE
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >
                                               SPA-GMN-C03-MANSEIKBN-MAX
                       IF      SPA-GMN-C03-MANSEIKBNLST(IDY)(1:2)
                                               =   USBYO-MANSEIKBN
                           MOVE    SPA-GMN-C03-MANSEIKBNTANL(IDY)
                                       TO  SPA-GMN-C03-TMANSEIKBN (IDX)
                           MOVE    SPA-GMN-C03-MANSEIKBN-MAX   TO  IDY
                       END-IF
                   END-PERFORM
               END-IF
      *
      *    保険病名フラグ
               IF      USBYO-HKNBYOMEIFLG  =   "1"
                   MOVE    "保険病名"  TO
                                       SPA-GMN-C03-THKNBYOMEIFLG (IDX)
               END-IF
      *
               MOVE    IDX             TO  SPA-GMN-C03-MAX
      *
      *    病名種別
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   SPA-GMN-C03-BYOMEISBT-MAX
                   IF      SPA-GMN-C03-BYOMEISBTL (IDY)
                                           =   USBYO-BYOMEISBT
                       MOVE    SPA-GMN-C03-BYOMEISBTMEIL(IDY)
                                       TO  SPA-GMN-C03-TBYOMEISBT (IDX)
                       MOVE    SPA-GMN-C03-BYOMEISBT-MAX   TO  IDY
                   END-IF
               END-PERFORM
      *
               MOVE    USBYO-BYOMEIHENFLG  TO  SPA-NAI-C03-TBYOMEIHENFLG
                                                                  (IDX)
           END-IF
      *
           .
       31011-USERBYOMEI-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    分類一覧 編集処理
      *****************************************************************
       3102-BUNRUI-LST-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
      *
      *    分類コード ’Ｃ’のもの
           INITIALIZE                  USERBYOMEI-REC
      *****MOVE    SPA-HOSPID          TO  USBYO-HOSPID
           MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_userbyomei"    TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-USBYO-READ-SEC
           ELSE
               INITIALIZE                  USERBYOMEI-REC
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
      *
           PERFORM UNTIL       (FLG-USERBYOMEI   =   1           )  OR
                               (IDX              >=  WRK-BUNRUI-MAX )
               ADD     1                   TO  IDX
               IF      USBYO-BYOMEIINPUTCD NOT =   SPACE
                   MOVE    USBYO-BYOMEIINPUTCD(1:3)
                                           TO
                                           SPA-GMN-C03-BBUNRUICD (IDX)
               END-IF                            
               MOVE    USBYO-BYOMEI        TO
                                           SPA-GMN-C03-BBUNRUIMEI(IDX)
      *
               MOVE    "tbl_userbyomei"    TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-USBYO-READ-SEC
           END-PERFORM
      *
           MOVE    IDX                 TO  SPA-GMN-C03-BUNRUI-MAX
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       3102-BUNRUI-LST-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者登録画面からの入力病名を編集処理
      *****************************************************************
       3103-INPUT-BYOMEI-HENSYU-SEC    SECTION.
      *
           MOVE    SPA-NAI-C02-BYOMEIHENFLG
                                       TO  SPA-NAI-C03-BYOMEIHENFLG
           IF      SPA-NAI-C03-BYOMEIHENFLG  =   "1" 
               MOVE    "病名編集"      TO  SPA-GMN-C03-BYOMEIHENFLG
           END-IF
      *
           MOVE    ZERO                TO  IDZ 
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       WRK-CONS-BYOMEICD-MAX
               IF          SPA-NAI-C02-BYOMEICD (IDY)
                                       NOT =   SPACE
                   ADD     1               TO  IDZ
                   MOVE    SPA-NAI-C02-BYOMEICD (IDY)
                                           TO  SPA-NAI-C03-BYOMEICD(IDZ)
                   MOVE    SPA-NAI-C03-BYOMEICD (IDZ)
                                           TO  BYO-BYOMEICD
                   PERFORM 410111-BYOMEI-HENSYU-SEC
                   IF      SPA-NAI-C03-BYOMEICD (IDZ)
                                       =   WRK-CONS-BYOMEICD
                       MOVE    SPA-GMN-C02-BYOMEI  TO
                                               SPA-NAI-C03-BYOMEI (IDZ)
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      SPA-NAI-C03-BYOMEICD (7)
                                           =   SPACE
               MOVE    ZERO                    TO  IDY 
               PERFORM VARYING IDZ FROM    1   BY  1
                       UNTIL   IDZ >       6
                   IF          SPA-GMN-C02-BYOMEICD (IDZ)
                                           NOT =   SPACE
                       ADD     1               TO  IDY
                       MOVE    SPA-GMN-C02-BYOMEICD (IDZ)
                                           TO  SPA-GMN-C03-BYOMEICD(IDY)
                   END-IF
               END-PERFORM
      *        入力病名コードと内部病名コードが違うときは画面表示しない
               IF    ( SPA-GMN-C03-BYOMEICD(1)
                                           =   SPA-NAI-C03-BYOMEICD(1) )
               AND   ( SPA-GMN-C03-BYOMEICD(2)
                                           =   SPA-NAI-C03-BYOMEICD(2) )
               AND   ( SPA-GMN-C03-BYOMEICD(3)
                                           =   SPA-NAI-C03-BYOMEICD(3) )
               AND   ( SPA-GMN-C03-BYOMEICD(4)
                                           =   SPA-NAI-C03-BYOMEICD(4) )
               AND   ( SPA-GMN-C03-BYOMEICD(5)
                                           =   SPA-NAI-C03-BYOMEICD(5) )
               AND   ( SPA-GMN-C03-BYOMEICD(6)
                                           =   SPA-NAI-C03-BYOMEICD(6) )
                   CONTINUE
               ELSE
                   INITIALIZE   SPA-GMN-C03-BYOMEICD-G
               END-IF
           END-IF
      *      
           MOVE    SPA-GMN-C02-BYOMEI  TO  SPA-GMN-C03-BYOMEI
      *                                 
           MOVE    SPA-NAI-C02-BYOMEIMOJI
                                       TO  SPA-NAI-C03-BYOMEIMOJI
      *                                 
           MOVE    SPA-GMN-C02-HOSOKU-COMMENT
                                       TO  SPA-GMN-C03-HOSOKU-COMMENT
      *
           MOVE    SPA-GMN-C02-CHARTBYOMEI
                                       TO  SPA-GMN-C03-CHARTBYOMEI
      *                                 
           MOVE    SPA-NAI-C02-CHARTBYOMEIMOJI
                                       TO  SPA-NAI-C03-CHARTBYOMEIMOJI
      *
           MOVE    SPA-GMN-C02-UTAGAIFLG
                                       TO  SPA-GMN-C03-UTAGAIFLG
           PERFORM 5001-UTAGAIFLG-HENSYU-SEC
      *
           MOVE    SPA-GMN-C02-MANSEIKBN
                                       TO  SPA-GMN-C03-MANSEIKBN
           PERFORM 5001-MANSEIKBN-HENSYU-SEC
      *
           MOVE    SPA-GMN-C02-HKNBYOMEIFLG
                                       TO  SPA-GMN-C03-HKNBYOMEIFLG 
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-C03-HKNBYOMEIFLG-MAX
               IF      SPA-GMN-C03-HKNBYOMEIFLG 
                                  =   SPA-GMN-C03-HKNBYOMEIFLGL (IDY)
                   MOVE    SPA-GMN-C03-HKNBYOMEIFLGLST (IDY)
                                      TO  SPA-GMN-C03-HKNBYOMEIFLG-G
               END-IF
           END-PERFORM
      *
           MOVE    50                  TO  SPA-GMN-C03-DSPSEQ
      *
           MOVE    SPA-NAI-C02-MANSEIKBN
                                       TO  SPA-NAI-C03-MANSEIKBN
           MOVE    SPA-NAI-C02-NANBYOCD
                                       TO  SPA-NAI-C03-NANBYOCD
      *    
           PERFORM 5001-HYOUJI-HENSYU-SEC
      * 
      *    病名種別のセット
           IF      SPA-GMN-C03-BYOMEICD(1)
                                   NOT =   SPACE     
               MOVE    SPACE                   TO  WRK-RECECD-TBL
      * 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-CONS-BYOMEICD-MAX
                 IF      SPA-NAI-C03-BYOMEICD (IDX)
                                           =   SPACE
                   CONTINUE
                 ELSE
                   EVALUATE    SPA-NAI-C03-BYOMEICD(IDX)(1:1)
                               ALSO    SPA-NAI-C03-BYOMEICD(IDX)(4:1)
                       WHEN    "Z"         ALSO    "1"  THRU    "7"
                           MOVE    SPA-NAI-C03-BYOMEICD(IDX)(4:1)
                                                 TO  WRK-RECECD (1)
                       WHEN    "Z"         ALSO    "8"  THRU    "8"
                           IF      WRK-RECECD (2)    NOT =   SPACE
                               MOVE    "1"           TO  WRK-RECECD (3)
                           ELSE
                               IF      WRK-RECECD (1)    NOT =   SPACE
                                   CONTINUE
                               ELSE
                                   MOVE   SPA-NAI-C03-BYOMEICD(IDX)(4:1)
                                                     TO  WRK-RECECD (3)
                               END-IF
                           END-IF
                       WHEN    "Z"         ALSO    ANY
                           CONTINUE
                       WHEN    OTHER                 
                           IF      WRK-RECECD (3)    NOT =   SPACE
                               CONTINUE
                           ELSE
                               MOVE    "1"           TO  WRK-RECECD (2)
                           END-IF
                   END-EVALUATE
                 END-IF
               END-PERFORM
               IF      WRK-RECECD-TBL        =   SPACE
                 MOVE    SPACE               TO  SPA-GMN-C03-BYOMEISBT-G
               ELSE
                 IF      WRK-RECECD (2)    NOT =   SPACE
                   MOVE    ZERO              TO  SPA-GMN-C03-BYOMEISBT
                 ELSE
                   IF      WRK-RECECD (1)    NOT =   SPACE
                       EVALUATE    WRK-RECECD (1)
                           WHEN    "1"
                               MOVE    "1"     TO  SPA-GMN-C03-BYOMEISBT
                           WHEN    OTHER 
                               MOVE    "7"     TO  SPA-GMN-C03-BYOMEISBT
                       END-EVALUATE    
                   ELSE
                       MOVE    "8"             TO  SPA-GMN-C03-BYOMEISBT
                   END-IF
                 END-IF                 
                 PERFORM 5001-BYOMEISBT-HENSYU-SEC
               END-IF
           ELSE
               MOVE    SPACE               TO  ORCSBYOMEIAREA
               INITIALIZE                      ORCSBYOMEIAREA
               MOVE    "1"                 TO  LNK-BYO-SYORI
               MOVE    SPA-GMN-C03-BYOMEI  TO  LNK-BYO-BYOMEI
               CALL    "ORCSBYOMEI"        USING   ORCSBYOMEIAREA
                                                   SPA-AREA
               MOVE    LNK-BYO-BYOMEISBT   TO  SPA-GMN-C03-BYOMEISBT
               PERFORM 5001-BYOMEISBT-HENSYU-SEC
           END-IF
      *
           MOVE    2                   TO  SPA-GMN-C03-CUR
      *
           .
       3103-INPUT-BYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名検索戻り編集処理
      *****************************************************************
       320-C05SPASET-SEC          SECTION.
      *
           IF      SPA-C01KYOUTU       =   SPACE
               GO  TO  320-C05SPASET-EXT
           END-IF
      *
           INITIALIZE                  SPA-GMN-C03-BYOMEICD-G
      *
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       WRK-CONS-BYOMEICD-MAX
               MOVE    SPA-C01-TBYOMEICD (IDZ)
                                       TO  SPA-NAI-C03-BYOMEICD(IDZ)
               MOVE    SPA-NAI-C03-BYOMEICD (IDZ)
                                           TO  BYO-BYOMEICD
               PERFORM 410111-BYOMEI-HENSYU-SEC
           END-PERFORM
      *
           PERFORM 41021-BYOMEI-HENSYU-SEC
      *
           MOVE    9                   TO  SPA-GMN-C03-CUR     
           .
       320-C05SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    SPACE       TO  SPA-C01KYOUTU
                   MOVE    "CHANGE"    TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 420-CLEAR-SEC
      *    全体表示
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 440-HYOUJI-SEC
      *    病名検索
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 450-C05-SYORI-SEC
      *    前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-MAEGMN-SEC
      *    次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 470-ATOGMN-SEC
      *    削除
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 480-SAKUJYO-SEC
      *    リスト
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 490-PRINT-GID-SEC
      *    コード表示
               WHEN    "CLICKED"       ALSO    "B22"
                   PERFORM 490-C09-SYORI-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-TOUROKU-SEC
      *    情報削除
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 480-JYOUHOU-DEL-SEC
      *    処理確認
               WHEN    "CLICKED"       ALSO    "B32"
                   PERFORM 490-KAKUNIN-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力箇所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-C03-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "BANGO"
                   PERFORM 41001-BANGO-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "BYOMEIINPUTCD"
               WHEN    "ACTIVATE"      ALSO    "BYOMEIINPUTCD_1"
               WHEN    "ACTIVATE"      ALSO    "BYOMEIINPUTCD_2"
                   PERFORM 41002-BYOMEIINPUT-CHK-SEC
               WHEN    ANY             ALSO    "BYOMEILST"
                   PERFORM 41020-BYOMEISEL-SEC
               WHEN    OTHER
                   PERFORM 400-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    行番号選択処理
      *****************************************************************
       41001-BANGO-CHK-SEC            SECTION.
      *
           PERFORM 420-CLEAR-SEC
      *
           MOVE    1                   TO  SPA-GMN-C03-CUR
      *
           MOVE    ZERO                TO  SPA-NAI-C03-SELNO
      *
           MOVE    C03-BANGO           TO  SPA-GMN-C03-BANGO
      *
           IF      SPA-GMN-C03-BANGO       =   ZERO
               MOVE    2                   TO  SPA-GMN-C03-CUR
               GO  TO  41001-BANGO-CHK-EXT
           END-IF
      *
           COMPUTE WRK-BANGO       =   SPA-GMN-C03-BANGO
                                   - ( SPA-GMN-C03-PAGE    -   1 )
                                       *   WRK-TBL-MAX
      *
           IF    ( WRK-BANGO       >   SPA-GMN-C03-MAX )
           OR    ( WRK-BANGO       <   1           )
               MOVE    "0012"          TO  SPA-ERRCD
               GO      TO      41001-BANGO-CHK-EXT
           END-IF
      *
           MOVE    WRK-BANGO       TO  SPA-NAI-C03-SELNO
                                       C03-BYOMEILST-ROW
      *
           PERFORM 410111-BYOMEI-SELECT-SEC
      *
           .
       41001-BANGO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択病名編集処理
      *****************************************************************
       410111-BYOMEI-SELECT-SEC         SECTION.
      *      
           MOVE    SPA-NAI-C03-TCDSYU         (SPA-NAI-C03-SELNO)
                                       TO  SPA-NAI-C03-CDSYU         (1)
                                           SPA-NAI-C03-CDSYU-OLD
           MOVE    SPA-GMN-C03-TBYOMEIINPUTCD (SPA-NAI-C03-SELNO)
                                       TO  SPA-GMN-C03-BYOMEIINPUTCD (1)
                                           SPA-NAI-C03-BYOMEIINPUTCD (1)
                                           SPA-NAI-C03-BYOMEIINPUTCD-OLD
           PERFORM 410111-USERBYOMEI-HENSYU-SEC
      *
           .
       410111-BYOMEI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択病名編集処理
      *****************************************************************
       410111-USERBYOMEI-HENSYU-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-HAISIYMDH
      *
           INITIALIZE              USERBYOMEI-REC
      *****MOVE    SPA-HOSPID      TO  USBYO-HOSPID
           MOVE    SPA-NAI-C03-CDSYU         (1)
                                   TO  USBYO-CDSYU
           MOVE    SPA-GMN-C03-BYOMEIINPUTCD (1)
                                   TO  USBYO-BYOMEIINPUTCD
           PERFORM 900-USBYO-INV-SEC
      *
           IF      FLG-USERBYOMEI  =   ZERO    
               MOVE    1               TO  SPA-NAI-C03-UPDFLG                                                          
      *
               PERFORM VARYING IDZ     FROM    1   BY  1
                       UNTIL   IDZ     >       WRK-CONS-BYOMEICD-MAX
                       OR      USBYO-BYOMEICD (IDZ)
                                       =   SPACE 
                 IF      USBYO-BYOMEICD (IDZ)  =   ALL "9"
                   MOVE    WRK-CONS-BYOMEICD-MAX   TO  IDZ
                   INITIALIZE                  SPA-GMN-C03-BYOMEICD-G
                 ELSE
                   MOVE    USBYO-BYOMEICD (IDZ)
                                       TO  SPA-NAI-C03-BYOMEICD(IDZ)
                   IF      IDZ             <   7           
                       MOVE    USBYO-BYOMEICD (IDZ)    TO
                                           SPA-GMN-C03-BYOMEICD(IDZ)
                   ELSE
                       INITIALIZE              SPA-GMN-C03-BYOMEICD-G
                   END-IF
                   MOVE    SPA-NAI-C03-BYOMEICD (IDZ)
                                       TO  BYO-BYOMEICD
                   PERFORM 410111-BYOMEI-HENSYU-SEC
                   IF      SPA-NAI-C03-BYOMEICD (IDZ)
                                       =   WRK-CONS-BYOMEICD
                       MOVE    USBYO-BYOMEI    TO
                                               SPA-NAI-C03-BYOMEI (IDZ)
                   END-IF
                   IF      FLG-BYOMEI          =   ZERO
                       IF    ( BYO-BYOMEICD (1:1)  NOT =   "Z"    )
                       AND   ( USBYO-BYOMEIHENFLG      =   SPACE  )
                           MOVE    BYOMEI-REC  TO  WRK-BYOMEI-REC
      *
                           IF    ( BYO-IKOSAKICD       NOT =   SPACE )
                           AND   ( SPA-C02-1042-MSG2-DSPKBN
                                                           =   ZERO  )
                             MOVE    BYO-IKOSAKICD   TO  BYO-BYOMEICD
                             MOVE    ZERO            TO  FLG-BYOMEI
                             PERFORM 900-BYOMEI-READ-SEC
                             IF      FLG-BYOMEI          =   ZERO
                               MOVE    SPA-NAI-C03-BYOMEICD (IDZ)
                                                   TO
                                               SPA-C03-IKOMOTO-BYOMEICD
                               MOVE    SPA-NAI-C03-BYOMEI   (IDZ)
                                                   TO
                                               SPA-C03-IKOMOTO-BYOMEI
                               MOVE    WRK-BYO-HAISIYMD
                                                   TO  WRK-SYMD
                               PERFORM 5002-HIZUKE-HEN-SEC
                               MOVE    LNK-DAY2-EDTYMD3
                                                   TO
                                               SPA-C03-IKOMOTO-HAISIYMD
                               MOVE    BYO-BYOMEICD    TO
                                               SPA-C03-IKOSAKI-BYOMEICD
                               MOVE    BYO-BYOMEI      TO
                                               SPA-C03-IKOSAKI-BYOMEI
                               MOVE    IDZ             TO
                                               SPA-C03-IKOSAKI-INDEX 
                               MOVE    "0101"          TO  SPA-CIDCD2
                             END-IF    
                           ELSE
                             IF      BYO-HAISIYMD        =   ALL "9"
                                                         OR  SPACE
                               CONTINUE
                             ELSE
                               MOVE    SPA-NAI-C03-BYOMEICD (IDZ)
                                                   TO
                                               SPA-C03-IKOMOTO-BYOMEICD
                               MOVE    SPA-NAI-C03-BYOMEI   (IDZ)
                                                   TO
                                               SPA-C03-IKOMOTO-BYOMEI
                               MOVE    WRK-BYO-HAISIYMD
                                                   TO  WRK-SYMD
                               PERFORM 5002-HIZUKE-HEN-SEC
                               MOVE    LNK-DAY2-EDTYMD3
                                                   TO
                                               SPA-C03-IKOMOTO-HAISIYMD
                               MOVE    IDZ     TO
                                               SPA-C03-IKOSAKI-INDEX 
                               PERFORM 5001-SYNONYM-CHK-SEC
                               IF      FLG-SYNONYM =   ZERO
                                   MOVE    "0102"      TO  SPA-CIDCD2
                               ELSE 
                                   IF      SPA-C02-1042-MSG3-DSPKBN
                                                           =   ZERO
                                       MOVE    WRK-BYO-HAISIYMD
                                                       TO  WRK-SYMD
                                       PERFORM 5002-HIZUKE-HEN-SEC
                                       MOVE    LNK-DAY2-EDTYMD3
                                                       TO  WRK-HAISIYMDH
                                       MOVE    BYO-BYOMEI
                                                       TO
                                                   SPA-C03-HAISI-BYOMEI
                                   END-IF
                               END-IF
                             END-IF        
                           END-IF
                       ELSE
                           IF      SPA-NAI-C03-BYOMEICD (IDZ)
                                                   =   WRK-CONS-BYOMEICD
                               MOVE    SPA-NAI-C03-BYOMEICD (IDZ)
                                                   TO
                                               SPA-C03-IKOMOTO-BYOMEICD
                               MOVE    SPA-NAI-C03-BYOMEI   (IDZ)
                                                   TO
                                               SPA-C03-IKOMOTO-BYOMEI
                               MOVE    IDZ     TO
                                               SPA-C03-IKOSAKI-INDEX 
                               PERFORM 5001-SYNONYM-CHK-SEC
                               IF      FLG-SYNONYM =   ZERO
                                   MOVE    "0102"      TO  SPA-CIDCD2
                               END-IF
                           END-IF
                       END-IF
                   ELSE
                       MOVE    "0076"              TO  SPA-ERRCD
                   END-IF
                 END-IF                 
               END-PERFORM
      *
               MOVE    ZERO                TO  SPA-NAI-C03-NANBYOCD
                                               SPA-NAI-C03-MANSEIKBN
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       21
                   IF      SPA-NAI-C03-NANBYOCD-O (IDX)  NOT =   ZERO
                       MOVE    SPA-NAI-C03-NANBYOCD-O (IDX)
                                               TO  SPA-NAI-C03-NANBYOCD
                       MOVE    21              TO  IDX
                   END-IF   
               END-PERFORM
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       21
                   IF      SPA-NAI-C03-TOKSKNCD (IDX)   NOT =   ZERO
                       MOVE    SPA-NAI-C03-TOKSKNCD (IDX)
                                           TO  SPA-NAI-C03-MANSEIKBN
                       MOVE    21              TO  IDX
                   END-IF   
               END-PERFORM
      *
               IF      SPA-ERRCD           =   SPACE 
                IF    ( SPA-C02-1042-MSG2-DSPKBN =   "1" )
                AND   ( SPA-C02-1042-MSG3-DSPKBN =   "1" )
                     CONTINUE
                ELSE
                 IF     ( SPA-CIDCD2      NOT =   SPACE )
                 OR     ( WRK-HAISIYMDH   NOT =   SPACE )
                 OR     ( USBYO-BYOMEIHENFLG  =   "1"   )
      *            別のコードでの組み立てを検索
                   MOVE    SPACE               TO  ORCSBYOMEIAREA
                   INITIALIZE                      ORCSBYOMEIAREA
                   MOVE    "1"                 TO  LNK-BYO-SYORI
                   STRING  SPA-NAI-C03-BYOMEI(1)   DELIMITED   BY  SPACE
                           SPA-NAI-C03-BYOMEI(2)   DELIMITED   BY  SPACE
                           SPA-NAI-C03-BYOMEI(3)   DELIMITED   BY  SPACE
                           SPA-NAI-C03-BYOMEI(4)   DELIMITED   BY  SPACE
                           SPA-NAI-C03-BYOMEI(5)   DELIMITED   BY  SPACE
                           SPA-NAI-C03-BYOMEI(6)   DELIMITED   BY  SPACE
                                               INTO    LNK-BYO-BYOMEI
                   END-STRING
                   CALL    "ORCSBYOMEI"        USING   ORCSBYOMEIAREA
                                                       SPA-AREA
                   IF    ( LNK-BYO-BYOMEIHENFLG    =   SPACE )
                   AND   ( LNK-BYO-IKOSAKICD       =   SPACE )
                   AND   ( LNK-BYO-HAISIYMD-ERR    =   ZERO  )
                       MOVE    LNK-BYO-BYOMEI-MAX  TO SPA-C03-CHANGE-MAX
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >       LNK-BYO-BYOMEI-MAX
                           MOVE    LNK-BYO-TBYOMEICD (IDY) TO
                                           SPA-C03-CHANGE-BYOMEICD(IDY) 
                           MOVE    LNK-BYO-TBYOMEI   (IDY) TO
                                           SPA-C03-CHANGE-BYOMEI  (IDY) 
                       END-PERFORM
      *
                       IF      SPA-CIDCD2     NOT =   SPACE
                           MOVE    SPA-CIDCD2     TO  SPA-CIDCD6
                           MOVE    SPACE          TO  SPA-CIDCD2
                       ELSE
                           IF    ( WRK-HAISIYMDH   NOT =   SPACE )
                           AND   ( SPA-C02-1042-MSG3-DSPKBN
                                                       =   "0"   )
                               MOVE    "0109"          TO  SPA-CIDCD6
                           ELSE
                               IF      SPA-C02-1042-MSG2-DSPKBN
                                                       =   "0"
                                   MOVE    "0108"      TO  SPA-CIDCD6
                               END-IF
                           END-IF
                       END-IF
                   END-IF
                 END-IF
                END-IF
               END-IF
      *
               IF    ( SPA-CIDCD2          =   SPACE )
               AND   ( SPA-CIDCD6          =   SPACE ) 
               AND   ( SPA-ERRCD           =   SPACE ) 
                   PERFORM 4101111-BYOMEI-SELECT-HEN-SEC              
                   IF    ( WRK-HAISIYMDH       =   SPACE )
                   OR    ( SPA-C02-1042-MSG3-DSPKBN
                                               =   "1"   )
                       CONTINUE
                   ELSE
                       MOVE    "0075"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF       
           .
       410111-USERBYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択病名編集処理
      *****************************************************************
       4101111-BYOMEI-SELECT-HEN-SEC   SECTION.
      *
           MOVE    USBYO-BUNRUICD      TO  SPA-GMN-C03-BUNRUICD
           IF      USBYO-BUNRUICD  NOT =   SPACE
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   SPA-GMN-C03-BUNRUI-MAX
                   IF      SPA-GMN-C03-BBUNRUICD(IDY)  
                                       =   USBYO-BUNRUICD
                       MOVE    SPA-GMN-C03-BBUNRUIMEI(IDY)
                                       TO  SPA-GMN-C03-BUNRUIMEI
                       MOVE    SPA-GMN-C03-BUNRUI-MAX  TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    USBYO-DSPSEQ        TO  SPA-GMN-C03-DSPSEQ
      *
           MOVE    USBYO-UTAGAIFLG     TO  SPA-GMN-C03-UTAGAIFLG
           PERFORM 5001-UTAGAIFLG-HENSYU-SEC
      *
           IF      USBYO-MANSEIKBN     NOT =   ZERO
               MOVE    USBYO-MANSEIKBN     TO  SPA-GMN-C03-MANSEIKBN
               PERFORM 5001-MANSEIKBN-HENSYU-SEC
           END-IF
      *      
           MOVE    USBYO-BYOMEI        TO  SPA-GMN-C03-BYOMEI
      *                                 
           MOVE    USBYO-BYOMEIMOJI    TO  SPA-NAI-C03-BYOMEIMOJI
      *
           MOVE    USBYO-HOSOKU-COMMENT
                                       TO  SPA-GMN-C03-HOSOKU-COMMENT
      *      
           MOVE    USBYO-CHARTBYOMEI   TO  SPA-GMN-C03-CHARTBYOMEI
      *                                 
           MOVE    USBYO-CHARTBYOMEIMOJI
                                       TO  SPA-NAI-C03-CHARTBYOMEIMOJI
      *
      *                                 
           MOVE    USBYO-BYOMEIHENFLG  TO  SPA-NAI-C03-BYOMEIHENFLG
           IF      SPA-NAI-C03-BYOMEIHENFLG  =   "1" 
               MOVE    "病名編集"      TO  SPA-GMN-C03-BYOMEIHENFLG
           END-IF    
      *                                 
           MOVE    USBYO-HKNBYOMEIFLG   TO  SPA-GMN-C03-HKNBYOMEIFLG 
           PERFORM VARYING IDY FROM   1   BY  1
                   UNTIL   IDY >      SPA-GMN-C03-HKNBYOMEIFLG-MAX
               IF      SPA-GMN-C03-HKNBYOMEIFLG 
                                  =   SPA-GMN-C03-HKNBYOMEIFLGL (IDY)
                   MOVE    SPA-GMN-C03-HKNBYOMEIFLGLST (IDY)
                                      TO  SPA-GMN-C03-HKNBYOMEIFLG-G
               END-IF
           END-PERFORM
      *                                 
           MOVE    USBYO-BYOMEISBT    TO  SPA-GMN-C03-BYOMEISBT
           PERFORM 5001-BYOMEISBT-HENSYU-SEC
      *    
           PERFORM 5001-HYOUJI-HENSYU-SEC
      *
           MOVE    3                   TO  SPA-GMN-C03-CUR 
      *
           .
       4101111-BYOMEI-SELECT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名マスタ編集処理
      *****************************************************************
       410111-BYOMEI-HENSYU-SEC          SECTION.
      *
           INITIALIZE                    SPA-NAI-C03-BYOMEI-G(IDZ)
      *
           MOVE    BYO-BYOMEICD          TO  SPA-NAI-C03-BYOMEICD (IDZ)
      *
           MOVE    ZERO                  TO  FLG-BYOMEI
      *
      *    
           IF      BYO-BYOMEICD          =   "0000999"
               GO  TO  410111-BYOMEI-HENSYU-EXT
           END-IF
      *
      *    予約コードのチェック
           IF      BYO-BYOMEICD (1:1)    =   "Z"
               SET     TBL-BYOMEICD-IDX  TO  1
               SEARCH  TBL-BYOMEICD-OCC  VARYING TBL-BYOMEICD-IDX
                   AT  END
                       MOVE    ZERO            TO  FLG-BYOMEICD-OK
                   WHEN    BYO-BYOMEICD    =   TBL-YYK-BYOMEICD 
                                                   (TBL-BYOMEICD-IDX)
                       MOVE    TBL-YYK-BYOMEI (TBL-BYOMEICD-IDX)
                                               TO
                                           SPA-NAI-C03-BYOMEI     (IDZ)
                       MOVE    "1"             TO
                                           SPA-NAI-C03-BYOMEIHENFLG-O
                                                                  (IDZ) 
                       MOVE    1               TO  FLG-BYOMEICD-OK
                       GO  TO  410111-BYOMEI-HENSYU-EXT
               END-SEARCH
           END-IF
      *  
           IF      BYO-BYOMEICD      NOT =   SPACE             
               PERFORM 900-BYOMEI-READ-SEC
      *
               IF      FLG-BYOMEI        =   ZERO
                   IF      BYO-BYOMEICD  =   WRK-CONS-BYOMEICD
                       MOVE    SPACE         TO  SPA-NAI-C03-BYOMEI(IDZ)
                   ELSE                           
                       MOVE    BYO-BYOMEI    TO  SPA-NAI-C03-BYOMEI(IDZ)
                   END-IF    
                   MOVE    BYO-UTAGAIFLG TO  SPA-NAI-C03-UTAGAIFLG(IDZ)
                   IF      BYO-BYOMEICD  =   WRK-CONS-UTAGAI
                       MOVE    "1"       TO  SPA-NAI-C03-UTAGAIFLG(IDZ)
                   END-IF    
                   MOVE    BYO-TOKSKNCD  TO  SPA-NAI-C03-TOKSKNCD (IDZ)
                   MOVE    BYO-NANBYOCD  TO  SPA-NAI-C03-NANBYOCD-O(IDZ)
                   MOVE    SPACE         TO  SPA-NAI-C03-BYOMEIHENFLG-O
                                                              (IDZ) 
               ELSE
                   MOVE    "■■■"      TO  SPA-NAI-C03-BYOMEI   (IDZ)
               END-IF
           END-IF 
           .
       410111-BYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名入力コードチェック処理
      *****************************************************************
       41002-BYOMEIINPUT-CHK-SEC       SECTION.
      *
           INITIALIZE                  SPA-NAI-C03-BYOMEIINPUTCD-CHK
                                       SPA-C03-BYOMEIINPUTCD-ERR-INDEX
      *
      *    カーソル位置設定
           EVALUATE    MCP-WIDGET
               WHEN    "BYOMEIINPUTCD"
                   MOVE    2                   TO  SPA-GMN-C03-CUR
                   MOVE    1                   TO  IDZ
               WHEN    "BYOMEIINPUTCD_1"
                   MOVE    18                  TO  SPA-GMN-C03-CUR
                   MOVE    2                   TO  IDZ
               WHEN    "BYOMEIINPUTCD_2"
                   MOVE    19                  TO  SPA-GMN-C03-CUR
                   MOVE    3                   TO  IDZ
           END-EVALUATE
      *
           PERFORM VARYING IDYY    FROM    1   BY  1
                   UNTIL   IDYY    >       3
                   OR      SPA-ERRCD   NOT =   SPACE
               IF      SPA-NAI-C03-BYOMEIINPUTCD (IDYY)
                                       NOT =   C03-BYOMEIINPUTCD (IDYY)
               OR      C03-BYOMEIINPUTCD (IDYY)
                                           =   SPACE
                   PERFORM 410021-BYOMEIINPUT-CHECK-SEC
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
      *        病名入力コードエラー表示処理
               PERFORM 410022-BYOMEIINPUTCD-ERR-SEC
      *        病名入力コード編集処理
               EVALUATE    MCP-WIDGET
                   WHEN    "BYOMEIINPUTCD"
                       MOVE    1                   TO  IDZ
                   WHEN    "BYOMEIINPUTCD_1"
                       MOVE    2                   TO  IDZ
                   WHEN    "BYOMEIINPUTCD_2"
                       MOVE    3                   TO  IDZ
               END-EVALUATE
               PERFORM 410023-BYOMEIINPUTCD-HENSYU-SEC
           END-IF
           .
       41002-BYOMEIINPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名入力コードチェック処理
      *****************************************************************
       410021-BYOMEIINPUT-CHECK-SEC    SECTION.
      *
           IF      C03-BYOMEIINPUTCD (IDYY)
                                       =   SPACE
               INITIALIZE SPA-NAI-C03-BYOMEIINPUTCD-OCC    (IDYY)
                          SPA-GMN-C03-BYOMEIINPUTCD        (IDYY)
               MOVE    "-"                 TO
                                   SPA-NAI-C03-BYOMEIINPUTCD-FLG (IDYY)
               MOVE    3                   TO  SPA-GMN-C03-CUR
               GO  TO  410021-BYOMEIINPUT-CHECK-EXT
           END-IF
      *
      *    半角チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI 
           MOVE    C03-BYOMEIINPUTCD (IDYY)
                                       TO  KANACHK-MAE-INPUT
           MOVE    10                  TO  KANACHK-MAX-CNT 
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
           AND     KANACHK-RC2         =   ZERO
               MOVE    KANACHK-OUT-INPUT   TO  C03-BYOMEIINPUTCD (IDYY)
           ELSE
               MOVE    "0072"              TO
                                   SPA-NAI-C03-BYOMEIINPUTCD-ERRCD(IDYY)
               GO  TO  410021-BYOMEIINPUT-CHECK-EXT
           END-IF
      *
      *    病名入力コード変更時の存在チェック
           IF    ( SPA-NAI-C03-BYOMEIINPUTCD-OLD
                                       NOT =   SPACE )
               IF    ( C03-BYOMEIINPUTCD (IDYY) NOT =
                                       SPA-GMN-C03-BYOMEIINPUTCD (IDYY))
               AND   ( C03-BYOMEIINPUTCD (IDYY) NOT =
                                       SPA-NAI-C03-BYOMEIINPUTCD-OLD   )
                   INITIALIZE              USERBYOMEI-REC
                   IF      KANACHK-SYUBETU =   "2"    
                       MOVE    "J"             TO  USBYO-CDSYU
                   ELSE
                       MOVE    "A"             TO  USBYO-CDSYU
                   END-IF           
                   MOVE    C03-BYOMEIINPUTCD (IDYY)
                                           TO  USBYO-BYOMEIINPUTCD
                   PERFORM 900-USBYO-INV-SEC
                   IF      FLG-USERBYOMEI  =   ZERO    
      **************   IF      IDYY            =   1
      **************       MOVE    "0054"          TO
      **************               SPA-NAI-C03-BYOMEIINPUTCD-ERRCD(IDYY)
      **************   ELSE
                           MOVE    "0002"          TO
                                   SPA-NAI-C03-BYOMEIINPUTCD-ERRCD(IDYY)
      **************   END-IF
                       GO  TO  410021-BYOMEIINPUT-CHECK-EXT
                   END-IF
               END-IF
           ELSE
               IF      IDYY                    =   1
                   IF    ( SPA-GMN-C03-BYOMEICD-G  NOT =   SPACE )
                   OR    ( SPA-GMN-C03-BYOMEI      NOT =   SPACE )
      *                コード変更時のチェック
                       INITIALIZE              USERBYOMEI-REC
                       IF      KANACHK-SYUBETU =   "2"    
                           MOVE    "J"             TO  USBYO-CDSYU
                       ELSE
                           MOVE    "A"             TO  USBYO-CDSYU
                       END-IF           
                       MOVE    C03-BYOMEIINPUTCD (IDYY)
                                               TO  USBYO-BYOMEIINPUTCD
                       PERFORM 900-USBYO-INV-SEC
                       IF      FLG-USERBYOMEI  =   ZERO    
                           MOVE    "0002"          TO
                                   SPA-NAI-C03-BYOMEIINPUTCD-ERRCD(IDYY)
                           GO  TO  410021-BYOMEIINPUT-CHECK-EXT
                       END-IF
                   END-IF
               ELSE    
      *            複数コード設定時のチェック
                   INITIALIZE              USERBYOMEI-REC
                   IF      KANACHK-SYUBETU =   "2"    
                       MOVE    "J"             TO  USBYO-CDSYU
                   ELSE
                       MOVE    "A"             TO  USBYO-CDSYU
                   END-IF           
                   MOVE    C03-BYOMEIINPUTCD (IDYY)
                                               TO  USBYO-BYOMEIINPUTCD
                   PERFORM 900-USBYO-INV-SEC
                   IF      FLG-USERBYOMEI  =   ZERO    
                       MOVE    "0002"          TO
                                   SPA-NAI-C03-BYOMEIINPUTCD-ERRCD(IDYY)
                       GO  TO  410021-BYOMEIINPUT-CHECK-EXT
                   END-IF
               END-IF
           END-IF
      *
           INITIALIZE SPA-NAI-C03-BYOMEIINPUTCD-OCC    (IDYY)
           MOVE    C03-BYOMEIINPUTCD (IDYY)
                                       TO  SPA-GMN-C03-BYOMEIINPUTCD
                                                       (IDYY)
      *
           MOVE    ZERO                TO  WRK-LENGTH
      *
           PERFORM VARYING IDX FROM    10  BY  -1
                   UNTIL   IDX <       1
               IF      SPA-GMN-C03-BYOMEIINPUTCD (IDYY) (IDX:1)
                                   NOT =   SPACE
                   MOVE    IDX             TO  WRK-LENGTH
                   MOVE    1               TO  IDX
               END-IF
           END-PERFORM                                    
      *
      *    「.」「／」で始まる又は「+」「-」で終わるコードは入力不可
           IF      KANACHK-SYUBETU  =   "2"
               COMPUTE IDX     =   WRK-LENGTH  -   1
               IF      SPA-GMN-C03-BYOMEIINPUTCD (IDYY) (IDX:2)
                                        =   "＋"    OR  "−"
                   MOVE    "0036"              TO
                                   SPA-NAI-C03-BYOMEIINPUTCD-ERRCD(IDYY)
               END-IF
           ELSE    
               IF      SPA-GMN-C03-BYOMEIINPUTCD (IDYY) (1:1)
                                        =   "." OR  "/"
                   MOVE    "0030"              TO
                                   SPA-NAI-C03-BYOMEIINPUTCD-ERRCD(IDYY)
               ELSE
                   IF      SPA-GMN-C03-BYOMEIINPUTCD(IDYY)(WRK-LENGTH:1)
                                            =   "-" OR  "+"
                       MOVE    "0036"              TO
                                   SPA-NAI-C03-BYOMEIINPUTCD-ERRCD(IDYY)
                   END-IF
               END-IF
      *
      *       ７桁のとき数字のみ又は病名マスタ・予約コードに存在するか
               IF    ( SPA-NAI-C03-BYOMEIINPUTCD-ERRCD(IDYY)
                                           =   SPACE ) 
               AND   ( WRK-LENGTH          =   7     )
                   INITIALIZE                  ORCSNUMAREA
                   MOVE    SPA-GMN-C03-BYOMEIINPUTCD (IDYY)
                                               TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF      SNUM-RC             =   ZERO
                       MOVE    "0001"              TO
                                   SPA-NAI-C03-BYOMEIINPUTCD-ERRCD(IDYY)
                   ELSE
                       MOVE    ZERO        TO  FLG-BYOMEI
                       IF  (   SPA-GMN-C03-BYOMEIINPUTCD (IDYY) (1:3)
                                                       =   "ZZZ"    )
                       AND ( ( SPA-GMN-C03-BYOMEIINPUTCD (IDYY) (4:1)
                                                       <   "1" ) OR 
                             ( SPA-GMN-C03-BYOMEIINPUTCD (IDYY) (4:1)
                                                       =   "C" )    )
      *                    予約病名コード検索
                           SET     TBL-BYOMEICD-IDX  TO  1
                           SEARCH  TBL-BYOMEICD-OCC
                                           VARYING TBL-BYOMEICD-IDX
                               AT  END
                                   MOVE    1       TO  FLG-BYOMEI
                               WHEN    SPA-GMN-C03-BYOMEIINPUTCD (IDYY)
                                                       =
                                               TBL-YYK-BYOMEICD
                                                      (TBL-BYOMEICD-IDX)
                                  MOVE    ZERO     TO  FLG-BYOMEI
                           END-SEARCH
                       ELSE
      *                    病名マスタ検索
                           MOVE    SPA-GMN-C03-BYOMEIINPUTCD (IDYY)
                                                   TO  BYO-BYOMEICD
                           PERFORM 900-BYOMEI-READ-SEC
                       END-IF
                       IF      FLG-BYOMEI          =   ZERO
                           MOVE    "0031"              TO
                                   SPA-NAI-C03-BYOMEIINPUTCD-ERRCD(IDYY)
                       END-IF
                   END-IF
               END-IF    
           END-IF
      *
           IF      SPA-NAI-C03-BYOMEIINPUTCD-ERRCD(IDYY)
                                   =   SPACE
      *        病名入力セット検索
               INITIALIZE                      BYOMEIINPUTSET-REC
               MOVE    SPA-HOSPNUM         TO  BYOSET-HOSPNUM
               MOVE    SPA-GMN-C03-BYOMEIINPUTCD (IDYY)
                                           TO   BYOSET-BYOMEIINPUTSET
               MOVE    BYOMEIINPUTSET-REC  TO  MCPDATA-REC
               PERFORM 900-BYOMEIINPUTSET-READ-SEC
               IF      FLG-BYOMEIINPUTSET  =   ZERO
                   MOVE    "0031"              TO
                                   SPA-NAI-C03-BYOMEIINPUTCD-ERRCD(IDYY)
               END-IF
           END-IF 
      *
           IF      SPA-NAI-C03-BYOMEIINPUTCD-ERRCD(IDYY)
                                NOT =   SPACE     
               GO  TO  410021-BYOMEIINPUT-CHECK-EXT
           END-IF
      *
           IF      KANACHK-SYUBETU  =   "2"    
              MOVE    "J"               TO  SPA-NAI-C03-CDSYU (IDYY)
           ELSE
              MOVE    "A"               TO  SPA-NAI-C03-CDSYU (IDYY)
           END-IF           
      *
           IF      SPA-GMN-C03-BANGO   =   ZERO
               IF    ( SPA-GMN-C03-BYOMEICD-G  NOT =   SPACE )
               OR    ( SPA-GMN-C03-BYOMEI      NOT =   SPACE )
                   CONTINUE
               ELSE
      *            登録済かどうかのチェック
                   PERFORM 410111-USERBYOMEI-HENSYU-SEC
                   IF      FLG-USERBYOMEI  =   ZERO
                       MOVE    SPA-GMN-C03-BYOMEIINPUTCD (IDYY)
                                       TO  SPA-NAI-C03-BYOMEIINPUTCD-OLD
                       MOVE    SPA-NAI-C03-CDSYU         (IDYY)
                                       TO  SPA-NAI-C03-CDSYU-OLD 
      *                一覧にあれば行選択する
                       PERFORM VARYING     IDX     FROM    1   BY  1
                               UNTIL       IDX     >   SPA-GMN-C03-MAX
                           IF      SPA-GMN-C03-BYOMEIINPUTCD (IDYY)  =
                                       SPA-GMN-C03-TBYOMEIINPUTCD (IDX)
                               MOVE    IDX         TO  SPA-NAI-C03-SELNO
                               MOVE    SPA-GMN-C03-TBANGO
                                                   (SPA-NAI-C03-SELNO)
                                                   TO  SPA-GMN-C03-BANGO
                               MOVE    SPA-NAI-C03-SELNO   TO
                                                       C03-BYOMEILST-ROW
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
           END-IF      
      *
           MOVE    SPA-GMN-C03-BYOMEIINPUTCD (IDYY)
                                       TO  SPA-NAI-C03-BYOMEIINPUTCD
                                                           (IDYY)
           .
       410021-BYOMEIINPUT-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名入力コードエラー表示処理
      *****************************************************************
       410022-BYOMEIINPUTCD-ERR-SEC    SECTION.
      *
      *    エラーなしのとき
           IF    ( SPA-NAI-C03-BYOMEIINPUTCD-ERRCD (1)
                                       =   SPACE )
           AND   ( SPA-NAI-C03-BYOMEIINPUTCD-ERRCD (2)
                                       =   SPACE )
           AND   ( SPA-NAI-C03-BYOMEIINPUTCD-ERRCD (3)
                                       =   SPACE )
               GO  TO  410022-BYOMEIINPUTCD-ERR-EXT
           END-IF
      *
      *    エラーコード表示
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       3
                   OR      SPA-ERRCD   NOT =   SPACE
               IF      SPA-NAI-C03-BYOMEIINPUTCD-ERRCD (IDX)
                                       NOT =   SPACE
                   MOVE    SPA-NAI-C03-BYOMEIINPUTCD-ERRCD (IDX)
                                           TO  SPA-ERRCD
                   EVALUATE    IDX
                       WHEN    1
                           MOVE    2               TO  SPA-GMN-C03-CUR
                       WHEN    28
                           MOVE    18              TO  SPA-GMN-C03-CUR
                       WHEN    3
                           MOVE    19              TO  SPA-GMN-C03-CUR
                   END-EVALUATE
                   MOVE    IDX             TO
                                   SPA-C03-BYOMEIINPUTCD-ERR-INDEX
                   PERFORM VARYING IDY FROM    IDX BY  1
                           UNTIL   IDY >       3
                       IF      SPA-ERRCD       =
                                       SPA-NAI-C03-BYOMEIINPUTCD-ERRCD
                                                               (IDY)
                           MOVE    SPACE           TO
                                       SPA-NAI-C03-BYOMEIINPUTCD-ERRCD
                                                               (IDY)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       410022-BYOMEIINPUTCD-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名入力コード編集処理
      *****************************************************************
       410023-BYOMEIINPUTCD-HENSYU-SEC      SECTION.
      *       
      *    病名入力コード空白時の制御
           MOVE    1                  TO  IDX
           PERFORM            UNTIL   IDX >   3
               IF      SPA-NAI-C03-BYOMEIINPUTCD-FLG (IDX)
                                      =   "-"
                   MOVE    SPACE               TO
                                   SPA-NAI-C03-BYOMEIINPUTCD-FLG (IDX) 
                   PERFORM VARYING IDY FROM    IDX BY  1
                           UNTIL   IDY >       3
                       IF      IDY             =   3
                           INITIALIZE SPA-NAI-C03-BYOMEIINPUTCD-OCC
                                                                   (IDY)
                                      SPA-NAI-C03-BYOMEIINPUTCD-CHK-O
                                                                   (IDY)
                                      SPA-GMN-C03-BYOMEIINPUTCD(IDY)
                       ELSE
                           COMPUTE IDW     =   IDY     +   1
                           MOVE    SPA-GMN-C03-BYOMEIINPUTCD
                                                           (IDW)
                                   TO  SPA-GMN-C03-BYOMEIINPUTCD   (IDY)
                           MOVE    SPA-NAI-C03-BYOMEIINPUTCD-OCC
                                                           (IDW)
                                   TO  SPA-NAI-C03-BYOMEIINPUTCD-OCC
                                                                   (IDY)
                           MOVE    SPA-NAI-C03-BYOMEIINPUTCD-CHK-O
                                                           (IDW)
                                   TO  SPA-NAI-C03-BYOMEIINPUTCD-CHK-O
                                                                   (IDY)
                           IF      SPA-C03-BYOMEIINPUTCD-ERR-INDEX
                                                   =   IDW
                               MOVE    IDY             TO
                                       SPA-C03-BYOMEIINPUTCD-ERR-INDEX
                           END-IF 
                       END-IF
                   END-PERFORM
               ELSE
                   ADD     1               TO  IDX
               END-IF 
            END-PERFORM
      *
           IF      SPA-ERRCD       =   SPACE
               EVALUATE    SPA-GMN-C03-CUR
                   WHEN    2
                       MOVE    18              TO  SPA-GMN-C03-CUR
                  WHEN    18
                       MOVE    19              TO  SPA-GMN-C03-CUR
                   WHEN    19
                       MOVE    3               TO  SPA-GMN-C03-CUR
               END-EVALUATE
      *      
      *       病名入力コード未入力時のとき分類コードへ遷移
               IF      SPA-GMN-C03-BYOMEICD (IDZ)  =   SPACE                                    
                   PERFORM VARYING IDX3    FROM    IDZ   BY  1
                           UNTIL   IDX3    >       3
                       IF       SPA-GMN-C03-BYOMEIINPUTCD (IDX3)
                                           NOT =   SPACE
                           GO  TO  410023-BYOMEIINPUTCD-HENSYU-EXT
                       END-IF
                   END-PERFORM 
                   MOVE    3                   TO  SPA-GMN-C03-CUR
               END-IF
           ELSE
                EVALUATE   SPA-C03-BYOMEIINPUTCD-ERR-INDEX
                   WHEN    1
                       MOVE    2               TO  SPA-GMN-C03-CUR
                   WHEN    2
                       MOVE    18              TO  SPA-GMN-C03-CUR
                  WHEN     3
                       MOVE    19              TO  SPA-GMN-C03-CUR
               END-EVALUATE
           END-IF
      *
           .
       410023-BYOMEIINPUTCD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41021-BUNRUILST-SEC       SECTION.
      *          
      *        行選択
           MOVE    ZERO                TO  WRK-SELNO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C03-BUNRUI-MAX
               IF      C03-BUNRUI-SELECT (IDX) =   "T"
                   MOVE    SPA-GMN-C03-BBUNRUICD (IDX)
                                               TO  SPA-GMN-C03-BUNRUICD
                   MOVE    SPA-GMN-C03-BBUNRUIMEI(IDX)
                                           TO  SPA-GMN-C03-BUNRUIMEI                           
      *
                   IF      IDX             =   SPA-NAI-C03-BUNRUI-SELNO
                       CONTINUE
                   ELSE        
                       MOVE    IDX                 TO  WRK-SELNO
                       MOVE    SPA-GMN-C03-BUNRUI-MAX  TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNO           >   ZERO
      *        分類コードのみ入力のとき
               IF    ( SPA-GMN-C03-BYOMEIINPUTCD-G
                                              =   SPACE )
               AND   ( SPA-GMN-C03-BYOMEI     =   SPACE )
               AND   ( SPA-GMN-C03-BYOMEICD-G =   SPACE )
               AND   ( SPA-GMN-C03-BUNRUICD
                                          NOT =   ZERO  )
                   MOVE    WRK-SELNO       TO  SPA-NAI-C03-BUNRUI-SELNO
               END-IF
           END-IF
      *
      *    分類コードのみ入力のとき分類名に属する病名の表示
           IF    ( SPA-GMN-C03-BYOMEIINPUTCD-G
                                          =   SPACE )
           AND   ( SPA-GMN-C03-BYOMEI     =   SPACE )
           AND   ( SPA-GMN-C03-BYOMEICD-G =   SPACE )
           AND   ( SPA-GMN-C03-BUNRUICD
                                      NOT =   ZERO  )
               INITIALIZE                  SPA-GMN-C03-BYOMEI-TBL
                                           SPA-NAI-C03-TBL
                                           SPA-NAI-C03-SELNO
               MOVE    ZERO                TO  SPA-GMN-C03-MAX
               MOVE    1                   TO  SPA-GMN-C03-PAGE 
      *
               MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
               INITIALIZE                  USERBYOMEI-REC
               MOVE    SPA-GMN-C03-BUNRUICD
                                           TO  USBYO-BUNRUICD
               PERFORM 3101-USERBYOMEI-LST-SEC
           END-IF
      *
           MOVE    5               TO  SPA-GMN-C03-CUR
           .
       41021-BUNRUISEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41020-BYOMEISEL-SEC       SECTION.
      *          
      *        行選択
           MOVE    ZERO                TO  WRK-SELNO
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C03-MAX
               IF      C03-BYOMEILST-SELECT (IDX)    =   "T"
                   IF      IDX             =   SPA-NAI-C03-SELNO
                       CONTINUE
                   ELSE        
                       MOVE    IDX             TO  WRK-SELNO
                       MOVE    SPA-GMN-C03-MAX     TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNO           >   ZERO
               MOVE    WRK-SELNO           TO  SPA-NAI-C03-SELNO
           END-IF
      *
           INITIALIZE                 SPA-GMN-C03-KOUMOKU
                                      SPA-NAI-C03-KOUMOKU
      *
           MOVE    SPA-GMN-C03-TBANGO (SPA-NAI-C03-SELNO)
                                       TO  SPA-GMN-C03-BANGO
      *                                 
           PERFORM 410111-BYOMEI-SELECT-SEC     
           .
       41020-BYOMEISEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       400-INPUT-CHK-SEC              SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    病名、病名コードのチェック
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "BYOMEICD_1"
               WHEN    "ACTIVATE"      ALSO    "BYOMEICD_2"
               WHEN    "ACTIVATE"      ALSO    "BYOMEICD_3"
               WHEN    "ACTIVATE"      ALSO    "BYOMEICD_4"
               WHEN    "ACTIVATE"      ALSO    "BYOMEICD_5"
               WHEN    "ACTIVATE"      ALSO    "BYOMEICD_6"
                   MOVE    MCP-WIDGET      TO  WRK-WIDGET
                   PERFORM 4102-BYOMEICD-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "BYOMEI"
                   PERFORM 41013-BYOMEI-CHK-SEC
               WHEN    OTHER
                   IF    ( C03-BYOMEI  NOT =   SPA-GMN-C03-BYOMEI      )
                   AND   ( C03-BYOMEICD (1)
                                           =   SPA-GMN-C03-BYOMEICD (1))
                   AND   ( C03-BYOMEICD (2)
                                           =   SPA-GMN-C03-BYOMEICD (2))
                   AND   ( C03-BYOMEICD (3)
                                           =   SPA-GMN-C03-BYOMEICD (3))
                   AND   ( C03-BYOMEICD (4)
                                           =   SPA-GMN-C03-BYOMEICD (4))
                   AND   ( C03-BYOMEICD (5)
                                           =   SPA-GMN-C03-BYOMEICD (5))
                   AND   ( C03-BYOMEICD (6)
                                           =   SPA-GMN-C03-BYOMEICD (6))
                       PERFORM 41013-BYOMEI-CHK-SEC
                   ELSE
                       IF    ( C03-BYOMEI  =   SPA-GMN-C03-BYOMEI      )
                       AND   ( C03-BYOMEICD (1)
                                       NOT =   SPA-GMN-C03-BYOMEICD (1)
                            OR C03-BYOMEICD (2)
                                       NOT =   SPA-GMN-C03-BYOMEICD (2)
                            OR C03-BYOMEICD (3)
                                       NOT =   SPA-GMN-C03-BYOMEICD (3)
                            OR C03-BYOMEICD (4)
                                       NOT =   SPA-GMN-C03-BYOMEICD (4)
                            OR C03-BYOMEICD (5)
                                       NOT =   SPA-GMN-C03-BYOMEICD (5)
                            OR C03-BYOMEICD (6)
                                       NOT =   SPA-GMN-C03-BYOMEICD (6))
                           MOVE    "BYOMEICD_1"    TO  WRK-WIDGET
                           PERFORM 4102-BYOMEICD-CHK-SEC
                       END-IF
                   END-IF
           END-EVALUATE
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    ANY             ALSO    "BUNRUILST"
      *            分類一覧選択
                   PERFORM 41021-BUNRUILST-SEC
           END-EVALUATE
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
           OR    ( SPA-CIDCD2      NOT =   SPACE )
           OR    ( SPA-CIDCD6      NOT =   SPACE )
               GO      TO      400-INPUT-CHK-EXT
           END-IF
      *
      *    入力箇所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
           OR    ( SPA-CIDCD2      NOT =   SPACE )
           OR    ( SPA-CIDCD6      NOT =   SPACE )
               GO      TO      400-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU         =   1
      *        更新前
               PERFORM 4501-KEIKOKU-CHK-SEC
           END-IF
      *
           .
       400-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    SPA-GMN-C03-BUNRUIMEI
                                       TO  WRK-GMN-C03-BUNRUIMEI-OLD
      *
           MOVE    C03-BYOMEISBT       TO  SPA-GMN-C03-BYOMEISBT-G
           MOVE    C03-BUNRUICD        TO  SPA-GMN-C03-BUNRUICD
           MOVE    C03-BUNRUIMEI       TO  SPA-GMN-C03-BUNRUIMEI
           MOVE    C03-HOSOKU-COMMENT  TO  SPA-GMN-C03-HOSOKU-COMMENT
           MOVE    C03-CHARTBYOMEI     TO  SPA-GMN-C03-CHARTBYOMEI
           MOVE    C03-UTAGAIFLG       TO  SPA-GMN-C03-UTAGAIFLG-G
           MOVE    C03-MANSEIFLG       TO  SPA-GMN-C03-MANSEIKBN-G
           MOVE    C03-HKNBYOMEIFLG    TO  SPA-GMN-C03-HKNBYOMEIFLG-G
           MOVE    C03-DSPSEQ          TO  SPA-GMN-C03-DSPSEQ
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
           PERFORM 41015-BYOMEISBT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41003-BUNRUICD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41004-BUNRUIMEI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41005-HOSOKU-COMMENT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41022-CHARTBYOMEI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41007-UTAGAI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41006-MANSEI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41014-HKNBYOMEIFLG-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41010-DSPSEQ-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    分類コードチェック処理
      *****************************************************************
       41003-BUNRUICD-CHK-SEC          SECTION.
      *
      *****MOVE    SPACE              TO  SPA-GMN-C03-BUNRUIMEI
      *
           IF      SPA-GMN-C03-BUNRUICD   =   ZERO
               CONTINUE
           ELSE
               MOVE    ZERO                TO  WRK-SELNO
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-C03-BUNRUI-MAX
                   IF      SPA-GMN-C03-BUNRUICD
                                       =   SPA-GMN-C03-BBUNRUICD (IDX)
                       IF    ( MCP-EVENT      =   "ACTIVATE" )
                       AND   ( MCP-WIDGET     =   "BUNRUICD" )
                           MOVE    SPA-GMN-C03-BBUNRUIMEI  (IDX)
                                               TO  SPA-GMN-C03-BUNRUIMEI
                       ELSE
                           IF      SPA-GMN-C03-BUNRUIMEI
                                           =   SPACE
                               MOVE    SPA-GMN-C03-BBUNRUIMEI  (IDX)
                                               TO  SPA-GMN-C03-BUNRUIMEI
                           END-IF
                       END-IF
                       IF    ( SPA-GMN-C03-BYOMEIINPUTCD-G
                                               =   SPACE )
                       AND   ( SPA-GMN-C03-BYOMEI
                                               =   SPACE )
                       AND   ( SPA-GMN-C03-BYOMEICD-G
                                               =   SPACE )
                       AND   ( SPA-GMN-C03-BUNRUICD
                                           NOT =   ZERO  )
                           MOVE    IDX            TO  WRK-SELNO
                       END-IF
                       MOVE    SPA-GMN-C03-BUNRUI-MAX  
                                               TO  IDX
                   END-IF
               END-PERFORM
               IF      WRK-SELNO           >   ZERO
                   MOVE    WRK-SELNO       TO  SPA-NAI-C03-BUNRUI-SELNO
               END-IF
           END-IF    
      *
      *    分類コードのみ入力のとき分類名に属する病名の表示
           IF    ( SPA-GMN-C03-BYOMEIINPUTCD-G
                                          =   SPACE )
           AND   ( SPA-GMN-C03-BYOMEI     =   SPACE )
           AND   ( SPA-GMN-C03-BYOMEICD-G =   SPACE )
           AND   ( SPA-GMN-C03-BUNRUICD
                                      NOT =   ZERO  )
           AND   ( SPA-NAI-C03-BUNRUI-SELNO
                                          >   ZERO  )  
               INITIALIZE                  SPA-GMN-C03-BYOMEI-TBL
                                           SPA-NAI-C03-TBL
                                           SPA-NAI-C03-SELNO
               MOVE    ZERO                TO  SPA-GMN-C03-MAX
               MOVE    1                   TO  SPA-GMN-C03-PAGE
      *
               MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
               INITIALIZE                  USERBYOMEI-REC
               MOVE    SPA-GMN-C03-BBUNRUICD (SPA-NAI-C03-BUNRUI-SELNO)
                                           TO  USBYO-BUNRUICD
               PERFORM 3101-USERBYOMEI-LST-SEC
           END-IF
           .
       41003-BUNRUICD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    分類名チェック処理
      *****************************************************************
       41004-BUNRUIMEI-CHK-SEC          SECTION.
      *
           IF      SPA-GMN-C03-BUNRUIMEI
                                        =   SPACE
               CONTINUE
           ELSE    
      *       半角チェック
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    SPA-GMN-C03-BUNRUIMEI
                                           TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
               AND     KANACHK-SYUBETU     =   "2"
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-C03-BUNRUIMEI
               ELSE
                   MOVE    "0071"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-GMN-C03-CUR
                   MOVE   WRK-GMN-C03-BUNRUIMEI-OLD
                                               TO  SPA-GMN-C03-BUNRUIMEI
               END-IF
           END-IF
           .
       41004-BUNRUIMEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名コードチェック処理
      *****************************************************************
       4102-BYOMEICD-CHK-SEC          SECTION.
      *
      *    カーソル位置設定
           IF      MCP-WIDGET (1:8)   =   "BYOMEICD"
               MOVE    MCP-WIDGET      TO  WRK-WIDGET
           ELSE
               EVALUATE    TRUE 
                   WHEN    C03-BYOMEICD (1)
                                       NOT =   SPA-GMN-C03-BYOMEICD (1)
                       MOVE    1               TO  WRK-WIDGET2
                   WHEN    C03-BYOMEICD (2)
                                       NOT =   SPA-GMN-C03-BYOMEICD (2)
                       MOVE    2               TO  WRK-WIDGET2
                   WHEN    C03-BYOMEICD (3)
                                       NOT =   SPA-GMN-C03-BYOMEICD (3)
                       MOVE    3               TO  WRK-WIDGET2
                   WHEN    C03-BYOMEICD (4)
                                       NOT =   SPA-GMN-C03-BYOMEICD (4)
                       MOVE    4               TO  WRK-WIDGET2
                   WHEN    C03-BYOMEICD (5)
                                       NOT =   SPA-GMN-C03-BYOMEICD (5)
                       MOVE    5               TO  WRK-WIDGET2
                   WHEN    C03-BYOMEICD (6)
                                       NOT =   SPA-GMN-C03-BYOMEICD (6)
                       MOVE    6               TO  WRK-WIDGET2
               END-EVALUATE
           END-IF
      *
           MOVE    WRK-WIDGET2     TO  IDZZ
           COMPUTE SPA-GMN-C03-CUR =   4   +   IDZZ
      *
           MOVE    SPACE           TO  WRK-HAISIYMDH
                                       WRK-SEIGYO
      *
           IF    ( SPA-GMN-C03-BYOMEICD-G
                                   =   SPACE )
               IF    ( C03-BYOMEICD(1) =   SPACE )
               AND   ( C03-BYOMEICD(2) =   SPACE )
               AND   ( C03-BYOMEICD(3) =   SPACE )
               AND   ( C03-BYOMEICD(4) =   SPACE )
               AND   ( C03-BYOMEICD(5) =   SPACE )
               AND   ( C03-BYOMEICD(6) =   SPACE )
      *            病名コード欄未入力のとき病名欄へ遷移
                   MOVE    11              TO  SPA-GMN-C03-CUR
                   GO  TO  4102-BYOMEICD-CHK-EXT
               ELSE
      *            病名欄入力後コード入力した時の処理
                   MOVE    ZERO            TO  FLG-BYOMEICD-CHK
                   PERFORM VARYING IDZ     FROM    1   BY  1
                           UNTIL   IDZ     >       6
                       IF      C03-BYOMEICD (IDZ)  NOT =   SPACE
                           IF      C03-BYOMEICD (IDZ)(1:2) =   "//"
                               CONTINUE
                           ELSE
                               MOVE    1           TO  FLG-BYOMEICD-CHK
                           END-IF
                       END-IF 
                   END-PERFORM
      *            コード入力があるとき内部情報クリア
                   IF      FLG-BYOMEICD-CHK    =   1
                       INITIALIZE              SPA-NAI-C03-BYOMEI-TBL
                       MOVE    WRK-WIDGET2     TO  IDZZ
                   END-IF 
               END-IF
           END-IF 
      *
      *    病名コード格納前チェック処理
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >       6
                   OR      SPA-ERRCD   NOT =   SPACE
               IF    ( SPA-GMN-C03-BYOMEICD (IDZ)
                                       NOT =   C03-BYOMEICD (IDZ) )
               OR    ( IDZ                 =   IDZZ               )
                   PERFORM 41021-BYOMEICD-CHECK-SEC
               END-IF
           END-PERFORM
      *
           IF    ( SPA-ERRCD           =   SPACE )
           AND   ( SPA-CIDCD2          =   SPACE )
      *        エラー表示処理
               PERFORM 41023-BYOMEICD-ERR-HYOUJI-SEC
      *        病名コード編集処理
               IF      SPA-CIDCD6          =   SPACE
                   IF      SPA-CIDCD2          =   SPACE   OR  "9999"
                       MOVE    WRK-WIDGET2         TO  IDZ
                       PERFORM 41021-BYOMEICD-HENSYU-SEC
                       IF      SPA-ERRCD   NOT =   SPACE
                           MOVE    SPACE               TO  SPA-CIDCD2
                       END-IF         
                   END-IF
                   IF      SPA-CIDCD2          =   "9999"
      *                病名検索へ遷移
                       MOVE    SPACE               TO  SPA-CIDCD2
                       PERFORM 450-C05-SYORI-SEC
                   END-IF
               END-IF    
           END-IF
           .
       4102-BYOMEICD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名コード格納前チェック処理
      *****************************************************************
       41021-BYOMEICD-CHECK-SEC       SECTION.
      *
           INITIALIZE          SPA-NAI-C03-BYOMEICD-CHK (IDZ)
      *
      *    病名検索へ遷移
           IF    ( C03-BYOMEICD (IDZ) (1:2) =   "//"  )
               MOVE    "//"            TO  SPA-NAI-C03-BYOMEICD-FLG(IDZ)
              MOVE    SPACE           TO  C03-BYOMEICD (IDZ)
            GO  TO  41021-BYOMEICD-CHECK-EXT
           END-IF
      *
      *    コードの挿入・削除の制御文字取得
           MOVE    SPACE           TO  WRK-SEIGYO
      *
           IF      C03-BYOMEICD (IDZ)  NOT =   SPACE
               MOVE    C03-BYOMEICD (IDZ)  TO  WRK-BYOMEICD
               PERFORM VARYING IDX FROM    8   BY  -1
                       UNTIL   IDX <       2
                   EVALUATE    WRK-BYOMEICD (IDX:1)
                       WHEN    "+"
      *                追加のとき
                           MOVE    WRK-BYOMEICD(IDX:1) TO  WRK-SEIGYO
                           IF      SPA-GMN-C03-BYOMEICD (6)
                                                   NOT =   SPACE
                               MOVE    SPACE           TO  WRK-SEIGYO
                           END-IF
                           IF      IDZ         >   1
                               COMPUTE IDW     =   IDZ     -   1
                               IF      SPA-NAI-C03-BYOMEICD (IDW)
                                                           =   SPACE 
                                   MOVE    SPACE       TO  WRK-SEIGYO
                               END-IF
                           END-IF
                           IF      WRK-SEIGYO  NOT =   SPACE
                               MOVE    WRK-SEIGYO
                                           TO
                                           SPA-NAI-C03-BYOMEICD-FLG(IDZ)
                               COMPUTE IDW     =   IDX -   1
                               MOVE    WRK-BYOMEICD (1:IDW)
                                               TO  C03-BYOMEICD (IDZ)
                           ELSE
                               GO  TO  41021-BYOMEICD-CHECK-EXT
                           END-IF
      *
                           MOVE    2           TO  IDX
                       WHEN    "-"
      *                削除のとき
                           MOVE    WRK-BYOMEICD(IDX:1) TO
                                           WRK-SEIGYO
                                           SPA-NAI-C03-BYOMEICD-FLG(IDZ)
                           MOVE    SPACE   TO  C03-BYOMEICD (IDZ)
      *
                           MOVE    2           TO  IDX
                   END-EVALUATE
               END-PERFORM
           END-IF
      *
      *    未コード化傷病名入力のとき
           IF      C03-BYOMEICD (IDZ)
                                   =   WRK-CONS-BYOMEICD
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       6
                   IF    ( SPA-GMN-C03-BYOMEICD (IDX)
                                          NOT =  SPACE )
                   AND   ( IDX            NOT =   IDZ  )
                       MOVE    "0062"      TO  SPA-ERRCD
                       COMPUTE SPA-GMN-C03-CUR =   4   +   IDZ
                       MOVE    6           TO  IDX
                   END-IF
               END-PERFORM                                    
           ELSE
      *        未コード化傷病名が既に入力のとき
               IF      C03-BYOMEICD (IDZ)
                                   NOT =   SPACE
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       6
                       IF      SPA-GMN-C03-BYOMEICD(IDX)
                                           =   WRK-CONS-BYOMEICD
                           MOVE    "0063"      TO  SPA-ERRCD
                           COMPUTE SPA-GMN-C03-CUR =   4   +   IDZ
                           MOVE    6           TO  IDX
                       END-IF
                   END-PERFORM
               END-IF    
           END-IF
      *             
           IF      SPA-ERRCD       NOT =   SPACE
               INITIALIZE          SPA-NAI-C03-BYOMEICD-CHK (1)
                                   SPA-NAI-C03-BYOMEICD-CHK (2)
                                   SPA-NAI-C03-BYOMEICD-CHK (3)
                                   SPA-NAI-C03-BYOMEICD-CHK (4)
                                   SPA-NAI-C03-BYOMEICD-CHK (5)
                                   SPA-NAI-C03-BYOMEICD-CHK (6)
               GO  TO  41021-BYOMEICD-CHECK-EXT
           END-IF
      *
           INITIALIZE          SPA-NAI-C03-BYOMEI-G (IDZ)
      *
           MOVE    C03-BYOMEICD (IDZ)  TO  SPA-GMN-C03-BYOMEICD(IDZ)
      *
           IF      SPA-GMN-C03-BYOMEICD(IDZ)(1:1)
                                       =   "."
               MOVE    "ZZZ"           TO
                                       SPA-GMN-C03-BYOMEICD(IDZ)(1:3)
               MOVE    C03-BYOMEICD(IDZ)(2:)
                                       TO
                                        SPA-GMN-C03-BYOMEICD(IDZ)(4:) 
           END-IF
           MOVE    SPA-GMN-C03-BYOMEICD(IDZ)
                                       TO  SPA-NAI-C03-BYOMEICD(IDZ)                                                
      *
           MOVE    ZERO                TO  WRK-LENGTH                    
           PERFORM VARYING IDX FROM    8   BY  -1
                   UNTIL   IDX <       1
               IF      SPA-GMN-C03-BYOMEICD(IDZ)(IDX:1)
                                   NOT =   SPACE
                   MOVE    IDX             TO  WRK-LENGTH
                   MOVE    1               TO  IDX
               END-IF
           END-PERFORM
      *
      *     空白の時コードをずらす
           EVALUATE    WRK-LENGTH
               WHEN    ZERO         
                   MOVE    "-"         TO  SPA-NAI-C03-BYOMEICD-FLG(IDZ)
               WHEN    7
      *            病名マスタ読み込み
                   MOVE    SPA-GMN-C03-BYOMEICD (IDZ) TO  BYO-BYOMEICD
                   PERFORM 410111-BYOMEI-HENSYU-SEC
                   IF      FLG-BYOMEI       NOT =   ZERO
                       MOVE    "1000"           TO
                                       SPA-NAI-C03-BYOMEICD-ERRCD (IDZ)
                   ELSE
                       MOVE    BYOMEI-REC  TO  WRK-BYOMEI-REC
      *
                       IF    ( BYO-IKOSAKICD   NOT =   SPACE )
                       AND   ( SPA-C02-1042-MSG2-DSPKBN
                                                   =   ZERO  )
                           MOVE    BYO-IKOSAKICD   TO  BYO-BYOMEICD
                           MOVE    ZERO            TO  FLG-BYOMEI
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      FLG-BYOMEI      =   ZERO
                               MOVE    BYO-BYOMEICD    TO
                                           SPA-NAI-C03-IKOSAKI-BYOMEICD
                                                            (IDZ)
                               MOVE    BYO-BYOMEI      TO
                                           SPA-NAI-C03-IKOSAKI-BYOMEI
                                                            (IDZ)
                               MOVE    WRK-BYO-HAISIYMD
                                                       TO  WRK-SYMD
                               PERFORM 5002-HIZUKE-HEN-SEC
                               MOVE    LNK-DAY2-EDTYMD3
                                                       TO
                                               SPA-NAI-C03-HAISIYMD(IDZ)
                               MOVE    "0103"          TO
                                           SPA-NAI-C03-IKOSAKI-ERRCD
                                                            (IDZ)
                           END-IF    
                       ELSE
                           IF      BYO-HAISIYMD    =   ALL "9" OR  SPACE
                               CONTINUE
                           ELSE
                               MOVE    WRK-BYO-HAISIYMD
                                                       TO  WRK-SYMD
                               PERFORM 5002-HIZUKE-HEN-SEC
      *
                               IF      SPA-C02-1042-MSG3-DSPKBN
                                                           =   ZERO
                                   MOVE    LNK-DAY2-EDTYMD3
                                                           TO
                                               SPA-NAI-C03-HAISIYMD(IDZ)
                               END-IF
      *
                               INITIALIZE              SYNONYM-REC
                               MOVE    WRK-BYO-BYOMEI
                                                   TO  SYNONYM-BYOMEI
                               PERFORM 5002-SYNONYM-CHK-SEC
                               IF      FLG-SYNONYM =   ZERO
                                   MOVE    SYNONYM-BYOMEICD
                                           TO
                                           SPA-NAI-C03-IKOSAKI-BYOMEICD
                                                            (IDZ)
                                   MOVE    BYO-BYOMEI      TO
                                           SPA-NAI-C03-IKOSAKI-BYOMEI
                                                            (IDZ)
                                   MOVE    "0104"          TO
                                           SPA-NAI-C03-IKOSAKI-ERRCD
                                                            (IDZ)
                                   MOVE    LNK-DAY2-EDTYMD3
                                                           TO
                                           SPA-NAI-C03-HAISIYMD(IDZ)
                               END-IF
                           END-IF        
                      END-IF        
                   END-IF
               WHEN    OTHER
                   MOVE    "■■■"        TO  SPA-NAI-C03-BYOMEI (IDZ)
                   MOVE    "0078"          TO
                                           SPA-NAI-C03-BYOMEICD-ERRCD
                                                                  (IDZ)
           END-EVALUATE
      *
           .
       41021-BYOMEICD-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名コード編集処理
      *****************************************************************
       41021-BYOMEICD-HENSYU-SEC      SECTION.
      *       
      *    コードの挿入・削除の制御
           MOVE    1                  TO  IDX
           PERFORM            UNTIL   IDX >   6
               IF      SPA-NAI-C03-BYOMEICD-FLG (IDX)
                                       NOT =   SPACE
                   EVALUATE    SPA-NAI-C03-BYOMEICD-FLG (IDX)
                       WHEN    "-"
                           MOVE    SPACE               TO
                                       SPA-NAI-C03-BYOMEICD-FLG (IDX)
                           PERFORM VARYING IDY FROM    IDX BY  1
                                   UNTIL   IDY >       6
                               IF      IDY             =   6
                                   INITIALIZE
                                           SPA-NAI-C03-BYOMEI-OCC(IDY)
                                           SPA-GMN-C03-BYOMEICD  (IDY)
                               ELSE
                                   COMPUTE IDW     =   IDY     +   1
                                   MOVE    SPA-NAI-C03-BYOMEI-OCC (IDW)
                                       TO  SPA-NAI-C03-BYOMEI-OCC (IDY)
                                   MOVE    SPA-GMN-C03-BYOMEICD   (IDW)
                                       TO  SPA-GMN-C03-BYOMEICD   (IDY)
                               END-IF                     
                           END-PERFORM                           
                       WHEN    "+"
                           MOVE    SPACE               TO
                                       SPA-NAI-C03-BYOMEICD-FLG (IDX)
                           PERFORM VARYING IDY FROM    6   BY  -1
                                   UNTIL   IDY <       IDX
                               IF      IDY             =   IDX
                                   INITIALIZE
                                           SPA-NAI-C03-BYOMEI-OCC(IDY)
                                           SPA-GMN-C03-BYOMEICD  (IDY)
                               ELSE
                                   COMPUTE IDW     =   IDY     -   1
                                   MOVE    SPA-NAI-C03-BYOMEI-OCC (IDW)
                                       TO  SPA-NAI-C03-BYOMEI-OCC (IDY)
                                   MOVE    SPA-GMN-C03-BYOMEICD   (IDW)
                                       TO  SPA-GMN-C03-BYOMEICD   (IDY)
                                   IF      SPA-C03-IKOSAKI-INDEX
                                                   =   IDW
                                       MOVE    IDY             TO
                                                   SPA-C03-IKOSAKI-INDEX
                                   END-IF 
                                   IF      SPA-C03-HAISI-INDEX
                                                   =   IDW
                                       MOVE    IDY             TO
                                                   SPA-C03-HAISI-INDEX
                                   END-IF 
                                   IF      SPA-C03-ERR-INDEX
                                                   =   IDW
                                       MOVE    IDY             TO
                                                   SPA-C03-ERR-INDEX
                                   END-IF 
                               END-IF
                           END-PERFORM
                           ADD     1               TO  IDX
                       WHEN    OTHER
                           ADD     1               TO  IDX
                   END-EVALUATE
               ELSE
                   ADD     1               TO  IDX
               END-IF 
           END-PERFORM
      *
      *    病名編集処理
           PERFORM 41021-BYOMEI-HENSYU-SEC
      *
           IF    ( SPA-ERRCD           NOT =   SPACE )
           OR    ( SPA-CIDCD2          NOT =   SPACE )
               GO  TO  41021-BYOMEICD-HENSYU-EXT
           END-IF
      *
      *    全ての病名コード未入力又は未コード化傷病名コードのとき病名へ遷移
           IF      SPA-GMN-C03-CUR         =   5
               IF    ( SPA-GMN-C03-BYOMEICD-G  =   SPACE             )
               OR    ( SPA-GMN-C03-BYOMEICD(1) =   WRK-CONS-BYOMEICD )
                   MOVE    11                  TO  SPA-GMN-C03-CUR
                   GO  TO  41021-BYOMEICD-HENSYU-EXT
               END-IF 
           END-IF 
      *      
      *    病名コード未入力のときカルテ病名へ遷移
           IF      SPA-GMN-C03-BYOMEICD (IDZ)  =   SPACE                                    
               PERFORM VARYING IDX3    FROM    IDZ   BY  1
                       UNTIL   IDX3    >       6
                   IF       SPA-GMN-C03-BYOMEICD (IDX3)
                                           NOT =   SPACE
                       GO  TO  41021-BYOMEICD-HENSYU-EXT
                   END-IF
               END-PERFORM 
               MOVE    12                  TO  SPA-GMN-C03-CUR
           ELSE
               ADD     1               TO  SPA-GMN-C03-CUR
               IF      SPA-GMN-C03-CUR     =   11
                   MOVE    12              TO  SPA-GMN-C03-CUR
               END-IF
           END-IF
      *
           .
       41021-BYOMEICD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名コード編集チェック処理
      *****************************************************************
       41021-BYOMEI-HENSYU-SEC        SECTION.
      *
      *    未コード化傷病名のとき
           IF      SPA-NAI-C03-BYOMEICD (1)
                                   =   WRK-CONS-BYOMEICD
               GO  TO  41021-BYOMEI-HENSYU-EXT
           END-IF
      *
           MOVE    ZERO                TO  SPA-NAI-C03-BYOMEIMOJI
      *    病名編集
           MOVE    SPA-NAI-C03-BYOMEI(1)
                                       TO  WRK-BYOMEI
           PERFORM VARYING IDX FROM    2   BY  1
                   UNTIL   IDX >       WRK-CONS-BYOMEICD-MAX
               STRING  WRK-BYOMEI              DELIMITED   BY  SPACE
                       SPA-NAI-C03-BYOMEI(IDX)     DELIMITED   BY  SPACE
                                       INTO    WRK-BYOMEI
               END-STRING
           END-PERFORM
      *
      *    疑いフラグ編集
      *****IF      WRK-BYOMEI          NOT =   SPA-GMN-C03-BYOMEI
      *      疑いフラグ設定区分による初期化
             IF      SPA-C02-1042-UTAGAIFLG-KBN
                                           =   "9"
                 MOVE    SPACE               TO  SPA-GMN-C03-UTAGAIFLG-G
             END-IF 
             PERFORM VARYING IDX FROM    1   BY  1
                     UNTIL   IDX >         WRK-CONS-BYOMEICD-MAX
                   OR      SPA-GMN-C03-UTAGAIFLG   =   "3"
               EVALUATE    SPA-NAI-C03-UTAGAIFLG   (IDX)
                   WHEN    "1"
                       IF      SPA-GMN-C03-UTAGAIFLG   =   "2"
                           MOVE    "3"     TO  SPA-GMN-C03-UTAGAIFLG
                       ELSE
                           MOVE    SPA-NAI-C03-UTAGAIFLG(IDX)
                                           TO  SPA-GMN-C03-UTAGAIFLG
                       END-IF    
                   WHEN    "2"
                       IF      SPA-GMN-C03-UTAGAIFLG   =   "1"
                           MOVE    "3"     TO  SPA-GMN-C03-UTAGAIFLG
                       ELSE
                           MOVE    SPA-NAI-C03-UTAGAIFLG(IDX)
                                           TO  SPA-GMN-C03-UTAGAIFLG
                       END-IF
                   WHEN    "3"
                       MOVE    "3"         TO  SPA-GMN-C03-UTAGAIFLG
                END-EVALUATE
             END-PERFORM
             PERFORM 5001-UTAGAIFLG-HENSYU-SEC
      *            
      *    慢性区分編集
             IF  ( ( WRK-SPA-CIDCD2  NOT =   SPACE )
                 AND
                   ( WRK-SPA-CID2-FLG    =   SPACE ) )
             OR  ( ( WRK-SPA-CIDCD6  NOT =   SPACE )
                 AND
                   ( WRK-SPA-CID6-FLG    =   SPACE ) )
      *          移行等の病名をそのまま表示するときは初期化しない  
                 CONTINUE
             ELSE
                 MOVE    SPACE               TO  SPA-GMN-C03-MANSEIKBN-G
                 MOVE    ZERO                TO  SPA-NAI-C03-NANBYOCD
                                                 SPA-NAI-C03-MANSEIKBN
             END-IF
             PERFORM VARYING IDX FROM    1   BY  1
                     UNTIL   IDX >       WRK-CONS-BYOMEICD-MAX
                 IF      SPA-NAI-C03-NANBYOCD-O (IDX)  NOT =   ZERO
                     MOVE    SPA-NAI-C03-NANBYOCD-O (IDX)
                                             TO  SPA-GMN-C03-MANSEIKBN
                                                 SPA-NAI-C03-NANBYOCD
                     MOVE    WRK-CONS-BYOMEICD-MAX
                                             TO  IDX
                 END-IF   
             END-PERFORM
             PERFORM VARYING IDX   FROM    1   BY  1
                     UNTIL   IDX   >   WRK-CONS-BYOMEICD-MAX
                 IF      SPA-NAI-C03-TOKSKNCD (IDX)   NOT =   ZERO
                     MOVE    SPA-NAI-C03-TOKSKNCD (IDX)
                                         TO  SPA-NAI-C03-MANSEIKBN
                     IF      SPA-GMN-C03-MANSEIKBN
                                           =   SPACE
                         MOVE    SPA-NAI-C03-TOKSKNCD (IDX)
                                           TO  SPA-GMN-C03-MANSEIKBN
                         MOVE    WRK-CONS-BYOMEICD-MAX
                                           TO  IDX
                     END-IF   
                 END-IF   
             END-PERFORM
             PERFORM 5001-MANSEIKBN-HENSYU-SEC
      *****END-IF
      *    
           PERFORM 5001-HYOUJI-HENSYU-SEC
      *
           MOVE    WRK-BYOMEI          TO  SPA-GMN-C03-BYOMEI
      *
      *    病名コードエラーの時以下の処理はしない
           IF      SPA-ERRCD           =   SPACE  OR  "0074"
               CONTINUE
           ELSE      
               GO  TO  41021-BYOMEI-HENSYU-EXT
           END-IF
      * 
      *    接尾語・接頭語チェック
           MOVE    SPACE                   TO  WRK-RECECD-TBL
      * 
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-CONS-BYOMEICD-MAX
      ***********OR      SPA-NAI-C03-BYOMEICD (IDX)
      ***********                        =   SPACE
      ***********OR      SPA-ERRCD   NOT =   SPACE 
               IF      SPA-NAI-C03-BYOMEICD (IDX)
                                           =   SPACE
                   CONTINUE
               ELSE
                 EVALUATE    SPA-NAI-C03-BYOMEICD(IDX)(1:1)
                               ALSO    SPA-NAI-C03-BYOMEICD(IDX)(4:1)
                   WHEN    "Z"         ALSO    "1"  THRU    "7"
                       IF    ( WRK-RECECD (2)    NOT =   SPACE )   OR
                             ( WRK-RECECD (3)    NOT =   SPACE )
                           MOVE    "0006"        TO  SPA-ERRCD
                           MOVE    WRK-CONS-BYOMEICD-MAX
                                                 TO  IDX
                       ELSE
                           MOVE    SPA-NAI-C03-BYOMEICD(IDX)(4:1)
                                                 TO  WRK-RECECD (1)
                       END-IF            
                   WHEN    "Z"         ALSO    "8"  THRU    "8"
                       IF      WRK-RECECD (2)    NOT =   SPACE
                           MOVE    "1"           TO  WRK-RECECD (3)
                       ELSE
                           IF      WRK-RECECD (1)    NOT =   SPACE
                               MOVE    "0006"        TO  SPA-ERRCD
                               MOVE    WRK-CONS-BYOMEICD-MAX
                                                     TO  IDX
                           ELSE
                               MOVE    SPA-NAI-C03-BYOMEICD(IDX)(4:1)
                                                     TO  WRK-RECECD (3)
                           END-IF
                       END-IF
                   WHEN    "Z"         ALSO    ANY
                       CONTINUE
                   WHEN    OTHER                 
                       IF      WRK-RECECD (3)    NOT =   SPACE
                           MOVE    "0006"        TO  SPA-ERRCD
                           MOVE    WRK-CONS-BYOMEICD-MAX
                                                 TO  IDX
                       ELSE
                           MOVE    "1"           TO  WRK-RECECD (2)
                       END-IF
                 END-EVALUATE
               END-IF
           END-PERFORM
           IF      SPA-ERRCD           =   SPACE  OR  "0074"
               CONTINUE
           ELSE      
               GO  TO  41021-BYOMEI-HENSYU-EXT
           END-IF
      *
      *    病名種別のセット
           IF      WRK-RECECD-TBL        =   SPACE
               MOVE    SPACE                 TO  SPA-GMN-C03-BYOMEISBT-G
           ELSE
               IF      WRK-RECECD (2)    NOT =   SPACE
                   MOVE    ZERO              TO  SPA-GMN-C03-BYOMEISBT
               ELSE
                   IF      WRK-RECECD (1)    NOT =   SPACE
                       EVALUATE    WRK-RECECD (1)
                           WHEN    "1"
                               MOVE    "1"     TO  SPA-GMN-C03-BYOMEISBT
                           WHEN    OTHER 
                               MOVE    "7"     TO  SPA-GMN-C03-BYOMEISBT
                       END-EVALUATE    
                   ELSE
                       MOVE    "8"             TO  SPA-GMN-C03-BYOMEISBT
                   END-IF
               END-IF
               PERFORM 5001-BYOMEISBT-HENSYU-SEC
           END-IF
      *
      *   入力病名コード数カウント
           MOVE    ZERO                TO  IDX1
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-CONS-BYOMEICD-MAX
               IF      SPA-NAI-C03-BYOMEICD(IDX)
                                       NOT =   SPACE
               AND     SPA-NAI-C03-BYOMEICD(IDX)(1:1)
                                       NOT =   "Z"                        
                   ADD     1               TO  IDX1
               END-IF
           END-PERFORM                                    
           IF     IDX1                    >   1
               MOVE    "0056"             TO  SPA-ERRCD
               GO  TO  41021-BYOMEI-HENSYU-EXT
           END-IF             
      *
      *    文字チェック     
           PERFORM VARYING     IDX     FROM    1   BY  2
                   UNTIL       IDX     >       100
                   OR  WRK-BYOMEI(IDX:2)       =   SPACE
               IF      WRK-BYOMEI(IDX:2)   =   "■"
                   MOVE    "0035"          TO  SPA-ERRCD
                   GO  TO  41021-BYOMEI-HENSYU-EXT
              END-IF
           END-PERFORM
      *
      *    文字数チェック     
           MOVE    ZERO                TO  WRK-MOJISU
           PERFORM VARYING     IDX     FROM    100 BY  -1
                   UNTIL       IDX     <       1
               IF      WRK-BYOMEI(IDX:1)   NOT =   SPACE
                   MOVE    IDX                 TO  WRK-MOJISU 
                   MOVE    1                   TO  IDX
               END-IF  
           END-PERFORM
           IF      WRK-MOJISU              >   80
               MOVE    "0007"          TO  SPA-ERRCD
               GO  TO  41021-BYOMEI-HENSYU-EXT
           END-IF
      *
      *    病名文字数編集
           COMPUTE WRK-MOJISU   =   WRK-MOJISU  /   2        
           MOVE    WRK-MOJISU          TO  SPA-NAI-C03-BYOMEIMOJI
      *
      *    病名編集フラグセット
           MOVE    SPACE                   TO  SPA-GMN-C03-BYOMEIHENFLG
                                               SPA-NAI-C03-BYOMEIHENFLG
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-CONS-BYOMEICD-MAX
               IF    ( SPA-NAI-C03-BYOMEICD(IDX)
                                       NOT =   SPACE )
               AND   ( SPA-NAI-C03-BYOMEIHENFLG-O (IDX)
                                           =   "1"   )
                   MOVE    "病名編集"      TO  SPA-GMN-C03-BYOMEIHENFLG
                   MOVE    "1"             TO  SPA-NAI-C03-BYOMEIHENFLG
                   MOVE    WRK-CONS-BYOMEICD-MAX
                                           TO  IDX
               END-IF
           END-PERFORM                                    
           .
       41021-BYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名コードエラー表示処理
      *****************************************************************
       41023-BYOMEICD-ERR-HYOUJI-SEC  SECTION.
      *
      *    設定なしのとき
           IF    ( SPA-NAI-C03-BYOMEICD-CHK (1)
                                       =   SPACE )
           AND   ( SPA-NAI-C03-BYOMEICD-CHK (2)
                                       =   SPACE )
           AND   ( SPA-NAI-C03-BYOMEICD-CHK (3)
                                       =   SPACE )
           AND   ( SPA-NAI-C03-BYOMEICD-CHK (4)
                                       =   SPACE )
           AND   ( SPA-NAI-C03-BYOMEICD-CHK (5)
                                       =   SPACE )
           AND   ( SPA-NAI-C03-BYOMEICD-CHK (6)
                                       =   SPACE )
               GO  TO  41023-BYOMEICD-ERR-HYOUJI-EXT
           END-IF
      *
      *    エラーコード表示
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
                   OR      SPA-ERRCD   NOT =   SPACE
               IF      SPA-NAI-C03-BYOMEICD-ERRCD (IDX)
                                       NOT =   SPACE
                   MOVE    SPA-NAI-C03-BYOMEICD-ERRCD (IDX)
                                           TO  SPA-ERRCD
                   COMPUTE SPA-GMN-C03-CUR     =   4   +   IDX
                   MOVE    IDX             TO  SPA-C03-ERR-INDEX
                   PERFORM VARYING IDY FROM    IDX BY  1
                           UNTIL   IDY >       6
                       IF      SPA-ERRCD       =
                                       SPA-NAI-C03-BYOMEICD-ERRCD (IDY)
                           MOVE    SPACE           TO
                                       SPA-NAI-C03-BYOMEICD-ERRCD (IDY)
                       END-IF
                   END-PERFORM
      *************GO  TO  41023-BYOMEICD-ERR-HYOUJI-EXT
               END-IF
           END-PERFORM
      *
      *    別のコードでの組み立てを検索
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
                   OR      SPA-ERRCD   NOT =   SPACE
               IF    ( SPA-NAI-C03-HAISIYMD (IDX)
                                       NOT =   SPACE )      
               OR    ( SPA-NAI-C03-IKOSAKI-BYOMEICD (IDX)
                                       NOT =   SPACE )
                   MOVE    SPACE               TO  ORCSBYOMEIAREA
                   INITIALIZE                      ORCSBYOMEIAREA
                   MOVE    "1"                 TO  LNK-BYO-SYORI
                   STRING  SPA-NAI-C03-BYOMEI(1)   DELIMITED   BY  SPACE
                           SPA-NAI-C03-BYOMEI(2)   DELIMITED   BY  SPACE
                           SPA-NAI-C03-BYOMEI(3)   DELIMITED   BY  SPACE
                           SPA-NAI-C03-BYOMEI(4)   DELIMITED   BY  SPACE
                           SPA-NAI-C03-BYOMEI(5)   DELIMITED   BY  SPACE
                           SPA-NAI-C03-BYOMEI(6)   DELIMITED   BY  SPACE
                                               INTO    LNK-BYO-BYOMEI
                   END-STRING
                   CALL    "ORCSBYOMEI"        USING   ORCSBYOMEIAREA
                                                       SPA-AREA
                   IF    ( LNK-BYO-BYOMEIHENFLG    =   SPACE )
                   AND   ( LNK-BYO-IKOSAKICD       =   SPACE )
                   AND   ( LNK-BYO-HAISIYMD-ERR    =   ZERO  )
                       MOVE    LNK-BYO-BYOMEI-MAX  TO SPA-C03-CHANGE-MAX
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >       LNK-BYO-BYOMEI-MAX
                           MOVE    LNK-BYO-TBYOMEICD (IDY) TO
                                           SPA-C03-CHANGE-BYOMEICD(IDY) 
                           MOVE    LNK-BYO-TBYOMEI   (IDY) TO
                                           SPA-C03-CHANGE-BYOMEI  (IDY) 
                       END-PERFORM
      *
                       IF      SPA-NAI-C03-HAISIYMD (IDX)
                                               NOT =   SPACE
                           MOVE    SPA-NAI-C03-BYOMEICD        (IDX)
                                           TO  SPA-C03-IKOMOTO-BYOMEICD
                           MOVE    SPA-NAI-C03-BYOMEI          (IDX)
                                           TO  SPA-C03-IKOMOTO-BYOMEI
                           MOVE    SPA-NAI-C03-HAISIYMD(IDX)
                                           TO  WRK-HAISIYMDH
      ****************                         SPA-C03-IKOMOTO-HAISIYMD
                           MOVE    IDX     TO  SPA-C03-HAISI-INDEX 
      *
                           MOVE    "0107"          TO  SPA-CIDCD6
                       END-IF
                       IF      SPA-NAI-C03-IKOSAKI-BYOMEICD (IDX)
                                               NOT =   SPACE
                           MOVE    SPA-NAI-C03-BYOMEICD        (IDX)
                                           TO  SPA-C03-IKOMOTO-BYOMEICD
                           MOVE    SPA-NAI-C03-BYOMEI          (IDX)
                                           TO  SPA-C03-IKOMOTO-BYOMEI
                           MOVE    SPA-NAI-C03-HAISIYMD(IDX)
                                           TO  SPA-C03-IKOMOTO-HAISIYMD
                           MOVE    SPA-NAI-C03-IKOSAKI-BYOMEICD(IDX)
                                           TO  SPA-C03-IKOSAKI-BYOMEICD
                           MOVE    SPA-NAI-C03-IKOSAKI-BYOMEI  (IDX)
                                           TO  SPA-C03-IKOSAKI-BYOMEI
                           MOVE    IDX     TO  SPA-C03-IKOSAKI-INDEX
      *
                           MOVE    SPA-NAI-C03-IKOSAKI-ERRCD (IDX)
                                           TO  SPA-CIDCD6
                       END-IF
                       INITIALIZE          SPA-NAI-C03-HAISI (IDX)
                   END-IF
               END-IF
           END-PERFORM
      *
      *    廃止病名警告表示
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
                   OR      SPA-ERRCD   NOT =   SPACE
               IF    ( SPA-NAI-C03-HAISIYMD (IDX)
                                       NOT =   SPACE )
               AND   ( SPA-NAI-C03-IKOSAKI-BYOMEICD (IDX)
                                           =   SPACE )
                   MOVE    "0074"              TO  SPA-ERRCD
                   COMPUTE SPA-GMN-C03-CUR     =   4   +   IDX
                   MOVE    SPA-NAI-C03-HAISIYMD(IDX)
                                           TO  WRK-HAISIYMDH
                   MOVE    SPA-NAI-C03-BYOMEI  (IDX)
                                           TO  SPA-C03-HAISI-BYOMEI
                   MOVE    IDX             TO  SPA-C03-HAISI-INDEX 
                   MOVE    SPACE           TO  SPA-NAI-C03-HAISIYMD(IDX)
      *************GO  TO  41023-BYOMEICD-ERR-HYOUJI-EXT
               END-IF
           END-PERFORM
      *
      *    移行病名確認表示
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
                   OR      SPA-ERRCD   NOT =   SPACE
                   OR      SPA-CIDCD2  NOT =   SPACE
               IF      SPA-NAI-C03-IKOSAKI-BYOMEICD (IDX)
                                       NOT =   SPACE
                   MOVE    SPA-NAI-C03-BYOMEICD        (IDX)
                                           TO  SPA-C03-IKOMOTO-BYOMEICD
                   MOVE    SPA-NAI-C03-BYOMEI          (IDX)
                                           TO  SPA-C03-IKOMOTO-BYOMEI
                   MOVE    SPA-NAI-C03-IKOSAKI-BYOMEICD(IDX)
                                           TO  SPA-C03-IKOSAKI-BYOMEICD
                   MOVE    SPA-NAI-C03-IKOSAKI-BYOMEI  (IDX)
                                           TO  SPA-C03-IKOSAKI-BYOMEI
                   MOVE    IDX             TO  SPA-C03-IKOSAKI-INDEX 
                   MOVE    SPA-NAI-C03-IKOSAKI-ERRCD   (IDX)
                                           TO  SPA-CIDCD2
                   INITIALIZE          SPA-NAI-C03-IKOSAKI-BYOMEICD(IDX)
                                       SPA-NAI-C03-IKOSAKI-BYOMEI  (IDX)
                                       SPA-NAI-C03-IKOSAKI-ERRCD   (IDX)
      *************GO  TO  41023-BYOMEICD-ERR-HYOUJI-EXT
               END-IF
           END-PERFORM
      *           
      *    エラー等があるとき病名検索はしない
           IF    ( SPA-ERRCD       NOT =   SPACE )
           OR    ( SPA-CIDCD2      NOT =   SPACE )
           OR    ( SPA-CIDCD6      NOT =   SPACE )
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       6
                   IF      SPA-NAI-C03-BYOMEICD-FLG (IDX)
                                               =   "//"
                       INITIALIZE  SPA-NAI-C03-BYOMEICD-FLG (IDX)
                   END-IF
               END-PERFORM
           ELSE
      *        病名検索へ
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       6
                   IF      SPA-NAI-C03-BYOMEICD-FLG (IDX)
                                               =   "//"
                       PERFORM VARYING IDY FROM    IDX BY  1
                               UNTIL   IDY >       6
                           IF      SPA-NAI-C03-BYOMEICD-FLG (IDY)
                                               =   "//"
                               INITIALIZE  SPA-NAI-C03-BYOMEICD-FLG(IDY)
                           END-IF
                       END-PERFORM     
                       MOVE    "9999"          TO  SPA-CIDCD2
                       GO  TO  41023-BYOMEICD-ERR-HYOUJI-EXT
                   END-IF
               END-PERFORM
           END-IF
           .
       41023-BYOMEICD-ERR-HYOUJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    補足コメント入力処理
      *****************************************************************
       41005-HOSOKU-COMMENT-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-C03-HOSOKU-COMMENT
                                       =   SPACE
               CONTINUE
           ELSE
      *        全角チェック
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    SPA-GMN-C03-HOSOKU-COMMENT
                                           TO  KANACHK-MAE-INPUT
               MOVE    40                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-SYUBETU     =   "2"  )
                   MOVE    KANACHK-OUT-INPUT   TO
                                           SPA-GMN-C03-HOSOKU-COMMENT
               ELSE
                   MOVE    "0071"              TO  SPA-ERRCD
               END-IF
      *
      *        前後の（）チェック
               IF      SPA-ERRCD           =   SPACE
                   COMPUTE IDX3    =   KANACHK-MAX     -   1
                   IF    ( SPA-GMN-C03-HOSOKU-COMMENT (1:2)
                                               =   "（" )
                   OR    ( SPA-GMN-C03-HOSOKU-COMMENT (IDX3:2)
                                               =   "）" )
                       MOVE    "0092"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    12                  TO  SPA-GMN-C03-CUR
           END-IF
           .
       41005-HOSOKU-COMMENT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    慢性入力処理
      *****************************************************************
       41006-MANSEI-CHK-SEC                SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-C03-HYOUJI
      *
           PERFORM 5001-MANSEIKBN-HENSYU-SEC
           IF      WRK-HENFLG           =   ZERO
               MOVE    "0021"               TO  SPA-ERRCD
               MOVE    14                   TO  SPA-GMN-C03-CUR
               GO  TO  41006-MANSEI-CHK-EXT
           END-IF    
      *    
           PERFORM 5001-HYOUJI-HENSYU-SEC
           .
       41006-MANSEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    疑い入力処理
      *****************************************************************
       41007-UTAGAI-CHK-SEC                SECTION.
      *
           PERFORM 5001-UTAGAIFLG-HENSYU-SEC
           IF      WRK-HENFLG          =   ZERO
               MOVE    "0018"              TO  SPA-ERRCD
               MOVE    13                  TO  SPA-GMN-C03-CUR
           END-IF    
           .
       41007-UTAGAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    表示連番チェック処理
      *****************************************************************
       41010-DSPSEQ-CHK-SEC          SECTION.
      *
           IF      SPA-GMN-C03-DSPSEQ     =   ZERO
               MOVE    50                 TO  SPA-GMN-C03-DSPSEQ
           END-IF              
           .
       41010-DSPSEQ-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名  入力処理
      *****************************************************************
       41013-BYOMEI-CHK-SEC             SECTION.
      *
           MOVE    11                   TO  SPA-GMN-C03-CUR
      *
           MOVE    C03-BYOMEI          TO  SPA-GMN-C03-BYOMEI
      *     
           INITIALIZE                      SPA-GMN-C03-BYOMEICD-G
                                           SPA-NAI-C03-BYOMEI-TBL
                                           SPA-NAI-C03-BYOMEIHENFLG
                                           SPA-GMN-C03-BYOMEIHENFLG
                                           SPA-GMN-C03-UTAGAIFLG-G
                                           SPA-GMN-C03-MANSEIKBN-G
                                           SPA-GMN-C03-BYOMEISBT-G
           MOVE    ZERO                TO  SPA-NAI-C03-BYOMEIMOJI
      *
           IF      SPA-GMN-C03-BYOMEI      =   SPACE
               MOVE    12                  TO  SPA-GMN-C03-CUR
               GO  TO  41013-BYOMEI-CHK-EXT
           END-IF
      *     
      *    病名コード検索処理
           MOVE    SPACE               TO  ORCSBYOMEIAREA
           INITIALIZE                      ORCSBYOMEIAREA
           MOVE    "1"                 TO  LNK-BYO-SYORI
           MOVE    SPA-GMN-C03-BYOMEI      TO  LNK-BYO-BYOMEI
           CALL    "ORCSBYOMEI"        USING   ORCSBYOMEIAREA
grpsys                                         SPA-AREA
      *
      *    半角チェックエラーのとき
           IF      LNK-BYO-KANACHK-ERR =   1 
               MOVE    "0071"              TO  SPA-ERRCD
               GO  TO  41013-BYOMEI-CHK-EXT
           END-IF
           IF      LNK-BYO-KANACHK-ERR =   2 
               MOVE    "0007"              TO  SPA-ERRCD
               GO  TO  41013-BYOMEI-CHK-EXT
           END-IF
      *
           MOVE    ZERO             TO  WRK-LNK-BYO-INDEX
           IF  (     ( SPA-C02-1042-MSG2-DSPKBN
                                       =   "1" )
                 AND ( SPA-C02-1042-MSG3-DSPKBN
                                       =   "1" ) )
           OR  (       LNK-BYOMEI-KENSAKU-MAX
                                       =   1     )
      *        移行・推奨病名、廃止病名の警告表示をしないとき
      *        または、検索結果がひとつのとき
               CONTINUE
           ELSE
      *        編集病名でなく移行等があるとき
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       LNK-BYOMEI-KENSAKU-MAX
                   IF      LNK-BYO-O-BYOMEIHENFLG(IDXX) =   SPACE
                     IF    ( LNK-BYO-O-IKOSAKICD   (IDXX)
                                             NOT =   SPACE           )
                     AND   ( LNK-BYO-O-IKOSAKICD   (IDXX)
                                                 =
                                                 LNK-BYO-KHNBYOMEICD )
                     AND   ( LNK-BYO-BYOMEICDSU  =  1                )
      *                移行先病名が元の病名と同じ時は対象としない
                       CONTINUE
                     ELSE  
                       IF      LNK-BYO-O-IKOSAKICD   (IDXX)
                                               NOT =   SPACE
                           IF      SPA-C02-1042-MSG2-DSPKBN
                                                   =   "0"
                               MOVE    IDXX        TO  WRK-LNK-BYO-INDEX
                               MOVE    LNK-BYOMEI-KENSAKU-MAX
                                                   TO  IDXX
                           END-IF
                       END-IF
                       IF      LNK-BYO-O-HAISIYMD-ERR(IDXX)
                                               NOT =   ZERO
      **************       IF      SPA-C02-1042-MSG3-DSPKBN
      **************                               =   "0"
      **************           MOVE    IDXX        TO  WRK-LNK-BYO-INDEX
      **************           MOVE    LNK-BYOMEI-KENSAKU-MAX
      **************                               TO  IDXX
      **************       END-IF
                           IF      SPA-C02-1042-MSG2-DSPKBN
                                                   =   "0"
                               INITIALIZE          SYNONYM-REC
                               MOVE    LNK-BYO-BYOMEI
                                                   TO  SYNONYM-BYOMEI
      *                        同義語マスタから病名検索処理
                               PERFORM 5002-SYNONYM-CHK-SEC
                               IF      FLG-SYNONYM =   ZERO
                                   MOVE    IDXX    TO  WRK-LNK-BYO-INDEX
                                   MOVE    LNK-BYOMEI-KENSAKU-MAX
                                                   TO  IDXX
                               END-IF
                           END-IF
                       END-IF 
                     END-IF
                   END-IF
               END-PERFORM
      *
               IF      WRK-LNK-BYO-INDEX   >   ZERO
      *            編集病名でなく移行等もないとき置換病名に設定
                   IF    ( LNK-BYO-BYOMEIHENFLG    =   SPACE )
                   AND   ( LNK-BYO-IKOSAKICD       =   SPACE )
                   AND   ( LNK-BYO-HAISIYMD-ERR    =   ZERO  )
                       MOVE    LNK-BYO-BYOMEI-MAX  TO
                                                   SPA-C03-CHANGE-MAX
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >       LNK-BYO-BYOMEI-MAX
                           MOVE    LNK-BYO-TBYOMEICD (IDY) TO
                                           SPA-C03-CHANGE-BYOMEICD(IDY) 
                           MOVE    LNK-BYO-TBYOMEI   (IDY) TO
                                           SPA-C03-CHANGE-BYOMEI  (IDY) 
                       END-PERFORM
                       MOVE    "0107"          TO  SPA-CIDCD6
      *
      *                編集病名でなく移行等があるコードと置き換える
                       INITIALIZE              LNK-BYOMEI-AREA
                       MOVE    LNK-BYOMEI-AREA-OCC (WRK-LNK-BYO-INDEX)
                                               TO  LNK-BYOMEI-AREA
                   END-IF
               END-IF
           END-IF
      *
           MOVE    LNK-BYO-BYOMEI      TO  SPA-GMN-C03-BYOMEI
           MOVE    LNK-BYO-UTAGAIFLG   TO  SPA-GMN-C03-UTAGAIFLG
           PERFORM 5001-UTAGAIFLG-HENSYU-SEC
           IF      LNK-BYO-NANBYOCD
                                   NOT =   ZERO
               MOVE    LNK-BYO-NANBYOCD    TO  SPA-GMN-C03-MANSEIKBN
               PERFORM 5001-MANSEIKBN-HENSYU-SEC                                
           ELSE
               IF      LNK-BYO-MANSEIKBN   NOT =   ZERO
                   MOVE    LNK-BYO-MANSEIKBN   TO  SPA-GMN-C03-MANSEIKBN
                   PERFORM 5001-MANSEIKBN-HENSYU-SEC
               END-IF
           END-IF
           MOVE    LNK-BYO-MANSEIKBN   TO  SPA-NAI-C03-MANSEIKBN
           MOVE    LNK-BYO-NANBYOCD    TO  SPA-NAI-C03-NANBYOCD
           PERFORM 5001-HYOUJI-HENSYU-SEC
           MOVE    LNK-BYO-BYOMEIMOJI  TO  SPA-NAI-C03-BYOMEIMOJI
           IF      LNK-BYO-BYOMEICD-ERR    =   1
               CONTINUE
           ELSE
               MOVE    LNK-BYO-BYOMEIHENFLG
                                           TO  SPA-NAI-C03-BYOMEIHENFLG
           END-IF
           IF      SPA-NAI-C03-BYOMEIHENFLG    
                                       =   "1"
               MOVE    "病名編集"          TO  SPA-GMN-C03-BYOMEIHENFLG
           END-IF   
           IF    ( SPA-GMN-C03-BYOMEISBT    =   SPACE )
           OR    ( SPA-NAI-C03-BYOMEIHENFLG =   SPACE )
               MOVE    LNK-BYO-BYOMEISBT   TO  SPA-GMN-C03-BYOMEISBT
           END-IF
           PERFORM 5001-BYOMEISBT-HENSYU-SEC
      *
           MOVE    ZERO                TO  IDZ
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       LNK-BYO-BYOMEI-MAX
               ADD     1               TO  IDZ
               MOVE    LNK-BYO-TBYOMEICD (IDX)
                                       TO  SPA-NAI-C03-BYOMEICD (IDZ)
               MOVE    LNK-BYO-TBYOMEI (IDX)
                                       TO  SPA-NAI-C03-BYOMEI   (IDZ)
               MOVE    LNK-BYO-TUTAGAIFLG (IDX)
                                       TO  SPA-NAI-C03-UTAGAIFLG(IDZ)
               MOVE    LNK-BYO-TTOKSKNCD (IDX)
                                       TO  SPA-NAI-C03-TOKSKNCD (IDZ)
               MOVE    LNK-BYO-TNANBYOCD  (IDX)
                                       TO  SPA-NAI-C03-NANBYOCD-O(IDZ)
               IF      LNK-BYO-THAISIYMD (IDX)
                                       =   ALL "9" OR  SPACE
                   CONTINUE
               ELSE
                   MOVE    LNK-BYO-THAISIYMD (IDX)
                                           TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3
                                           TO  WRK-HAISIYMDH
               END-IF
           END-PERFORM
      *
      *    病名コード最大個数オーバー
           IF      LNK-BYO-BYOMEICD-MAX-ERR =   1
               MOVE    "0077"           TO  SPA-ERRCD
               MOVE    SPACE            TO  SPA-CIDCD2
                                            SPA-CIDCD6
               MOVE    12               TO  SPA-GMN-C03-CUR
               GO  TO  41013-BYOMEI-CHK-EXT
           END-IF
      *
      *    病名順エラー
           IF      LNK-BYO-BYOMEICD-ERR     =   2
               MOVE    "0103"           TO  SPA-ERRCD
               MOVE    SPACE            TO  SPA-CIDCD2
                                            SPA-CIDCD6
               MOVE    12               TO  SPA-GMN-C03-CUR
               GO  TO  41013-BYOMEI-CHK-EXT
           END-IF
      *
      *    病名順エラー
           IF      LNK-BYO-BYOMEICD-ERR     =   9
               MOVE    "0101"           TO  SPA-ERRCD
               MOVE    SPACE            TO  SPA-CIDCD2
                                            SPA-CIDCD6
               MOVE    12               TO  SPA-GMN-C03-CUR
               GO  TO  41013-BYOMEI-CHK-EXT
           END-IF
      *
      *    移行先コード又は廃止病名のとき（編集病名以外）
           IF    ( SPA-C02-1042-MSG2-DSPKBN
                                       =   "1" )
           AND   ( SPA-C02-1042-MSG3-DSPKBN
                                       =   "1" )
               CONTINUE
           ELSE
               IF    ( LNK-BYO-IKOSAKICD    =   SPACE )
               AND   ( LNK-BYO-HAISIYMD-ERR =   ZERO  )
                   IF      LNK-BYO-TBYOMEICD (1)
                                               =   WRK-CONS-BYOMEICD
                       MOVE    SPA-NAI-C03-BYOMEICD (1)
                                           TO  SPA-C03-IKOMOTO-BYOMEICD
                       MOVE    SPA-NAI-C03-BYOMEI   (1)
                                           TO  SPA-C03-IKOMOTO-BYOMEI
                       MOVE    1           TO  SPA-C03-IKOSAKI-INDEX                    
                       PERFORM 5001-SYNONYM-CHK-SEC
                       IF      FLG-SYNONYM     =   ZERO
                           MOVE    "0106"          TO  SPA-CIDCD2
                       END-IF
                   END-IF
               ELSE
                   IF      LNK-BYO-BYOMEIHENFLG    =   SPACE                          
                       MOVE    ZERO         TO  IDZ
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >   LNK-BYO-BYOMEI-MAX
                           IF      LNK-BYO-THAISIYMD (IDX)
                                                       =   ALL "9"
                                                           OR  SPACE  
                               CONTINUE
                           ELSE
                               MOVE    LNK-BYO-THAISIYMD (IDX)
                                                   TO  WRK-SYMD
                               PERFORM 5002-HIZUKE-HEN-SEC
                               MOVE    LNK-DAY2-EDTYMD3
                                                   TO
                                               SPA-C03-IKOMOTO-HAISIYMD
                                               WRK-HAISIYMDH
      *
                               MOVE    SPA-NAI-C03-BYOMEICD (IDX)
                                           TO  SPA-C03-IKOMOTO-BYOMEICD
                               MOVE    SPA-NAI-C03-BYOMEI   (IDX)
                                           TO  SPA-C03-IKOMOTO-BYOMEI
                               MOVE    IDX TO  SPA-C03-IKOSAKI-INDEX                    
                               PERFORM 5001-SYNONYM-CHK-SEC
                               IF      FLG-SYNONYM =   ZERO
                                   MOVE    "0106"      TO  SPA-CIDCD2
                               END-IF
                           END-IF
      *        
                           IF    ( LNK-BYO-TIKOSAKICD (IDX)
                                                   NOT =   SPACE )
                           AND   ( SPA-C02-1042-MSG2-DSPKBN
                                                       =   ZERO  )
                               MOVE    SPA-NAI-C03-BYOMEICD (IDX)
                                           TO  SPA-C03-IKOMOTO-BYOMEICD
                               MOVE    SPA-NAI-C03-BYOMEI   (IDX)
                                           TO  SPA-C03-IKOMOTO-BYOMEI
                               MOVE    LNK-BYO-TIKOSAKICD (IDX)
                                           TO  SPA-C03-IKOSAKI-BYOMEICD
                               MOVE    LNK-BYO-IKOSAKICD-BYOMEI
                                           TO  SPA-C03-IKOSAKI-BYOMEI
                               MOVE    IDX TO  SPA-C03-IKOSAKI-INDEX                    
                               MOVE    "0105"
                                           TO  SPA-CIDCD2
                           END-IF
                       END-PERFORM
                   END-IF
      *
                   IF      SPA-CIDCD6          =   "0107"
                       IF      SPA-CIDCD2          =   "0105"
                           MOVE    "0115"              TO  SPA-CIDCD6
                       ELSE
                           IF      SPA-CIDCD2          =   "0106"
                               MOVE    "0116"          TO  SPA-CIDCD6
                           ELSE
                               MOVE    "0117"          TO  SPA-CIDCD6 
                           END-IF
                       END-IF
                       MOVE    SPACE              TO  SPA-CIDCD2
                   END-IF
      *
      *            廃止病名のとき（移行先がない場合）
                   IF    ( SPA-CIDCD2          =   SPACE )
                   AND   ( SPA-CIDCD6          =   SPACE )
                   AND   ( WRK-HAISIYMDH   NOT =   SPACE )
                   AND   ( SPA-C02-1042-MSG3-DSPKBN
                                               =   ZERO  )
                       MOVE    "0075"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           MOVE    12                   TO  SPA-GMN-C03-CUR
           .
       41013-BYOMEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    カルテ病名  入力処理
      *****************************************************************
       41022-CHARTBYOMEI-CHK-SEC             SECTION.
      *
           MOVE    ZERO                 TO  SPA-NAI-C03-CHARTBYOMEIMOJI
      
      *    半角チェック
      *
           IF      SPA-GMN-C03-CHARTBYOMEI  =   SPACE
               CONTINUE
           ELSE    
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    SPA-GMN-C03-CHARTBYOMEI
                                           TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
               AND     KANACHK-SYUBETU     =   "2"
                   MOVE    KANACHK-OUT-INPUT   TO
                                               SPA-GMN-C03-CHARTBYOMEI
               ELSE
                   MOVE    "0071"              TO  SPA-ERRCD
                   MOVE    20                  TO  SPA-GMN-C03-CUR
                   GO  TO 41022-CHARTBYOMEI-CHK-EXT
               END-IF
           END-IF    
      *
      *    病名文字数編集
           MOVE    ZERO                TO  WRK-MOJISU
           PERFORM VARYING     IDX     FROM    80  BY  -1
                   UNTIL       IDX     <       1
               IF      SPA-GMN-C03-CHARTBYOMEI(IDX:1)   NOT =   SPACE
                   MOVE    IDX                 TO  WRK-MOJISU 
                   MOVE    1                   TO  IDX
               END-IF  
           END-PERFORM
      *
           COMPUTE WRK-MOJISU   =   WRK-MOJISU  /   2        
           MOVE    WRK-MOJISU           TO  SPA-NAI-C03-CHARTBYOMEIMOJI
           .
       41022-CHARTBYOMEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険病名フラグ　入力処理
      *****************************************************************
       41014-HKNBYOMEIFLG-CHK-SEC                SECTION.
      *
           IF      SPA-GMN-C03-HKNBYOMEIFLG   =   SPACE
               CONTINUE
           ELSE
               MOVE    ZERO               TO  WRK-HENFLG
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      5
                       OR      WRK-HENFLG =   1
                   IF      SPA-GMN-C03-HKNBYOMEIFLG 
                               =   SPA-GMN-C03-HKNBYOMEIFLGL (IDY)
                       MOVE    SPA-GMN-C03-HKNBYOMEIFLGLST (IDY)
                                       TO  SPA-GMN-C03-HKNBYOMEIFLG-G
                       MOVE    1       TO  WRK-HENFLG
                   END-IF
               END-PERFORM
               IF      WRK-HENFLG          =   ZERO
                   MOVE    "0028"              TO  SPA-ERRCD
                   MOVE    15                  TO  SPA-GMN-C03-CUR
                   GO  TO  41014-HKNBYOMEIFLG-CHK-EXT
               END-IF
           END-IF
           .
       41014-HKNBYOMEIFLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名種別入力処理
      *****************************************************************
       41015-BYOMEISBT-CHK-SEC                SECTION.
      *
      *    分類コードのみ入力のとき設定しない
           IF    ( SPA-GMN-C03-BYOMEIINPUTCD-G
                                          =   SPACE )
           AND   ( SPA-GMN-C03-BYOMEI     =   SPACE )
           AND   ( SPA-GMN-C03-BYOMEICD-G =   SPACE )
           AND   ( SPA-GMN-C03-BUNRUICD
                                      NOT =   ZERO  )
               MOVE    SPACE              TO  SPA-GMN-C03-BYOMEISBT-G
               GO  TO  41015-BYOMEISBT-CHK-EXT
           END-IF
      *
      *    病名未入力のときチェックしない
           IF      SPA-NAI-C03-BYOMEIMOJI   =   ZERO
      *****    MOVE    SPACE              TO  SPA-GMN-C03-BYOMEISBT-G
               GO  TO  41015-BYOMEISBT-CHK-EXT
           END-IF
      *
           PERFORM 5001-BYOMEISBT-HENSYU-SEC
      *
           IF      WRK-HENFLG          =   ZERO
              MOVE    "0052"               TO  SPA-ERRCD
              MOVE    17                   TO  SPA-GMN-C03-CUR
              GO  TO  41015-BYOMEISBT-CHK-EXT
           END-IF
      *
      *    種別と病名コードの関連チェック
           IF    ( SPA-NAI-C03-BYOMEICD (1)    =   WRK-CONS-BYOMEICD )
      *****OR    ( SPA-NAI-C03-BYOMEIHENFLG    =   "1"               )
               CONTINUE
           ELSE 
               INITIALIZE                      WRK-RECECD-TBL
                                               WRK-BYOMEISBT
      * 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-CONS-BYOMEICD-MAX
                       OR      SPA-ERRCD   NOT =   SPACE
                       OR      WRK-BYOMEISBT   =   9
                   IF      SPA-NAI-C03-BYOMEICD(IDX)     NOT =   SPACE 
                       EVALUATE    SPA-NAI-C03-BYOMEICD (IDX) (1:1)
                                   ALSO  SPA-NAI-C03-BYOMEICD (IDX)(4:1)
                           WHEN    "Z"     ALSO    "1"
                               IF    ( WRK-RECECD (2)    NOT =   SPACE )
                               OR    ( WRK-RECECD (3)    NOT =   SPACE )
                                   MOVE    9          TO  WRK-BYOMEISBT
                               ELSE
                                   MOVE
                                      SPA-NAI-C03-BYOMEICD (IDX)(4:1)
                                                      TO  WRK-RECECD (1)
                                   MOVE    1          TO  WRK-BYOMEISBT
                               END-IF 
                           WHEN    "Z"     ALSO    "2"  THRU    "7"
                               IF    ( WRK-RECECD (2)    NOT =   SPACE )
                               OR    ( WRK-RECECD (3)    NOT =   SPACE )
                                   MOVE    9          TO  WRK-BYOMEISBT
                               ELSE
                                   MOVE
                                       SPA-NAI-C03-BYOMEICD (IDX)(4:1)
                                                      TO  WRK-RECECD (1)
                                   MOVE    7          TO  WRK-BYOMEISBT
                               END-IF
                           WHEN    "Z"     ALSO    "8"  THRU    "8"
                               IF      WRK-RECECD (2)    NOT =   SPACE
                                   MOVE   SPA-NAI-C03-BYOMEICD(IDX)(4:1)
                                                       TO  WRK-RECECD(3)
                                   MOVE    8          TO  WRK-BYOMEISBT
                               ELSE
                                   IF      WRK-RECECD (1)
                                               NOT =   SPACE
                                       MOVE    9       TO  WRK-BYOMEISBT
                                   ELSE
                                       MOVE
                                          SPA-NAI-C03-BYOMEICD(IDX)(4:1)
                                                       TO  WRK-RECECD(3)
                                       MOVE    8       TO  WRK-BYOMEISBT
                                   END-IF
                               END-IF    
                           WHEN    "Z"       ALSO    ANY
                               MOVE    9          TO  WRK-BYOMEISBT
                           WHEN    OTHER                 
                               IF      WRK-RECECD (3)    NOT =   SPACE
                                   MOVE    9           TO  WRK-BYOMEISBT
                               ELSE
                                   MOVE    "1"         TO  WRK-RECECD(2)
                               END-IF
                       END-EVALUATE
                   END-IF
               END-PERFORM
      *
      *        予約コードがある場合はチェックなし
               IF      WRK-BYOMEISBT           =   9
                   CONTINUE
                ELSE          
                   IF      WRK-RECECD (2)      NOT =   SPACE
                       MOVE    ZERO                TO  WRK-BYOMEISBT
                   END-IF
      *
                   EVALUATE    SPA-GMN-C03-BYOMEISBT
                       WHEN    ZERO
                           IF      WRK-RECECD (2)      =   SPACE
                               IF      SPA-NAI-C03-BYOMEIHENFLG
                                                           =   SPACE
                                   MOVE    "1"             TO
                                               SPA-NAI-C03-BYOMEIHENFLG
                                   MOVE    "病名編集"      TO
                                               SPA-GMN-C03-BYOMEIHENFLG
                                   MOVE    "0103"          TO  SPA-ERRCD
                               END-IF
                           END-IF 
                       WHEN    "1"
                           IF      WRK-BYOMEISBT       =   1
                               MOVE    SPACE           TO
                                               SPA-NAI-C03-BYOMEIHENFLG
                                               SPA-GMN-C03-BYOMEIHENFLG
                           ELSE 
                               MOVE    "0053"          TO  SPA-ERRCD
                           END-IF 
                       WHEN    "7"
                           IF      WRK-BYOMEISBT       =   7
                               MOVE    SPACE           TO
                                               SPA-NAI-C03-BYOMEIHENFLG
                                               SPA-GMN-C03-BYOMEIHENFLG
                           ELSE 
                               MOVE    "0053"          TO  SPA-ERRCD
                           END-IF 
                       WHEN    "8"
                           IF      WRK-BYOMEISBT       =   8
                               IF    ( WRK-RECECD (1)        =   SPACE )
                               AND   ( WRK-RECECD (2)        =   SPACE )
                               AND   ( WRK-RECECD (3)    NOT =   SPACE )
                                   MOVE    SPACE           TO
                                               SPA-NAI-C03-BYOMEIHENFLG
                                               SPA-GMN-C03-BYOMEIHENFLG
                               ELSE  
                                   IF      SPA-NAI-C03-BYOMEIHENFLG
                                                           =   SPACE
                                       MOVE    "1"             TO
                                               SPA-NAI-C03-BYOMEIHENFLG
                                       MOVE    "病名編集"      TO
                                               SPA-GMN-C03-BYOMEIHENFLG
                                       MOVE    "0103"          TO
                                               SPA-ERRCD
                                   END-IF
                               END-IF
                           ELSE 
                               MOVE    "0053"          TO  SPA-ERRCD
                           END-IF 
                   END-EVALUATE
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD       NOT =   SPACE
              MOVE    17                   TO  SPA-GMN-C03-CUR
           END-IF
          .
       41015-BYOMEISBT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    ジョブ管理チェック処理
           PERFORM 900-JOBKANRI-CHK-SEC
           IF    ( FLG-JOBEND   =   ZERO )
               MOVE    "6666"          TO  SPA-CIDCD
               STRING  "処理中です【"  DELIMITED  BY  SIZE
                       JOB-SHELLMSG    DELIMITED  BY  SPACE
                       "】"            DELIMITED  BY  SIZE
               INTO  WRK-SHORIERR
               END-STRING
               GO  TO  210-BACK-EXT
           END-IF
      *
           MOVE    "C02"               TO  SPA-SAKIPG
           MOVE    "C03"               TO  SPA-MOTOPG
      *
           MOVE    SPA-C01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       480-SAKUJYO-SEC             SECTION.
      *
      *    未入力のとき
           IF    ( SPA-GMN-C03-BYOMEIINPUTCD-G
                                       =   SPACE )
           AND   ( SPA-GMN-C03-BUNRUICD    =   ZERO  )
               GO  TO  480-SAKUJYO-EXT
           END-IF
      *
      *    分類コードのみ入力のとき
           IF    ( SPA-GMN-C03-BYOMEIINPUTCD-G
                                       =   SPACE )
           AND   ( SPA-GMN-C03-BUNRUICD
                                   NOT =   ZERO  )
               IF      SPA-GMN-C03-MAX         >  ZERO
                   MOVE    "0101"              TO  SPA-CIDCD3
               ELSE
                   MOVE    "0103"              TO  SPA-CIDCD4
               END-IF
               GO  TO  480-SAKUJYO-EXT
           END-IF
      *
           IF      SPA-NAI-C03-UPDFLG      =   ZERO
               MOVE    "0004"              TO  SPA-ERRCD
           ELSE
               MOVE    "0104"              TO  SPA-CIDCD4
           END-IF
      *
          .
       480-SAKUJYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       450-TOUROKU-SEC                SECTION.
      *
      *    入力チェック
           MOVE    1                   TO  FLG-TOUROKU
           PERFORM 400-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
           OR    ( SPA-CIDCD       NOT =   SPACE )  
           OR    ( SPA-CIDCD2      NOT =   SPACE )
           OR    ( SPA-CIDCD6      NOT =   SPACE )
               GO  TO  450-TOUROKU-EXT
           END-IF
      *
      *    登録処理
           PERFORM 4502-TOUROKU-SYORI-SEC
      *    患者病名画面より自院コード登録時は戻る
           IF    ( SPA-ERRCD           =   SPACE              )
           AND   ( SPA-NAI-C03-HENFLG  =   1                  )
               MOVE    SPACE               TO  SPA-C01KYOUTU
               IF      SPA-GMN-C03-BYOMEI  =   SPA-GMN-C02-BYOMEI
                   MOVE    SPA-GMN-C03-BYOMEIINPUTCD (1)
                                               TO  SPA-C01-BYOMEIINPUTCD
               END-IF
               MOVE    "CHANGE"            TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
           .
       450-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       4502-TOUROKU-SYORI-SEC                SECTION.
      *
           MOVE    ZERO               TO  WRK-SYOKBN
           INITIALIZE                     WRK-UPDFLG-TBL
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    自院病名マスター（分類名登録）
      *
           IF      SPA-GMN-C03-BUNRUICD    NOT =   ZERO
               INITIALIZE              USERBYOMEI-REC
      *********MOVE    SPA-HOSPID          TO  USBYO-HOSPID
               MOVE    "C"                 TO  USBYO-CDSYU
               MOVE    SPA-GMN-C03-BUNRUICD    TO  USBYO-BYOMEIINPUTCD
               PERFORM 900-USBYO-INV-SEC
               IF      FLG-USERBYOMEI      =   1
                   INITIALIZE              USERBYOMEI-REC
      *************MOVE    SPA-HOSPID      TO  USBYO-HOSPID
                   MOVE    "C"             TO  USBYO-CDSYU
                   MOVE    SPA-GMN-C03-BUNRUICD
                                           TO  USBYO-BYOMEIINPUTCD
                   MOVE    SPA-GMN-C03-BUNRUIMEI
                                           TO  USBYO-BYOMEI
      *
                   MOVE    SPA-OPID        TO  USBYO-OPID
                   MOVE    SMCNDATE-YMD    TO  USBYO-CREYMD
                   MOVE    SMCNDATE-YMD    TO  USBYO-UPYMD
                   MOVE    SMCNDATE-HMS    TO  USBYO-UPHMS
      *
                   MOVE    "DBINSERT"      TO  MCP-FUNC
                   MOVE    SPA-HOSPNUM     TO  USBYO-HOSPNUM
                   MOVE    USERBYOMEI-REC  TO  MCPDATA-REC
                   MOVE    "tbl_userbyomei"
                                           TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"     USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
                   IF      MCP-RC          NOT =   ZERO 
                       MOVE    "1005"              TO  SPA-ERRCD
                       DISPLAY "UPD USBYOMEI-KEY =>" USBYO-KEY 
                       GO  TO  4502-TOUROKU-SYORI-EXT
                   END-IF
                   MOVE    1               TO  WRK-SYOKBN 
               ELSE
                   IF      SPA-GMN-C03-BUNRUIMEI
                                           NOT =   USBYO-BYOMEI
                       MOVE    SPA-GMN-C03-BUNRUIMEI
                                               TO  USBYO-BYOMEI
      *
                       MOVE    SPA-OPID        TO  USBYO-OPID
                       MOVE    SMCNDATE-YMD    TO  USBYO-UPYMD
                       MOVE    SMCNDATE-HMS    TO  USBYO-UPHMS
      *
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
                       MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
                       MOVE    "tbl_userbyomei"    TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       CALL    "ORCDBMAIN"         USING
                                                       MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                       IF      MCP-RC          NOT =   ZERO 
                           MOVE    "1005"              TO  SPA-ERRCD
                           DISPLAY "UPD USBYOMEI-KEY =>" USBYO-KEY 
                           GO  TO  4502-TOUROKU-SYORI-EXT
                       END-IF
                       MOVE    1               TO  WRK-SYOKBN 
                   END-IF
               END-IF
           END-IF
      *
      *    自院病名マスター（自院病名登録）
      *
           IF      SPA-GMN-C03-BYOMEIINPUTCD-G
                                   NOT =   SPACE
            PERFORM VARYING IDYY    FROM    1   BY  1
                    UNTIL   IDYY    >       3
             IF      SPA-GMN-C03-BYOMEIINPUTCD (IDYY)
                                   NOT =   SPACE
               INITIALIZE                  USERBYOMEI-REC
               MOVE    SPA-NAI-C03-CDSYU         (IDYY)
                                           TO  USBYO-CDSYU
               MOVE    SPA-GMN-C03-BYOMEIINPUTCD (IDYY)
                                           TO  USBYO-BYOMEIINPUTCD
               PERFORM 900-USBYO-INV-SEC
               IF      FLG-USERBYOMEI      =   1
                   INITIALIZE                  USERBYOMEI-REC
                   MOVE    SPA-NAI-C03-CDSYU         (IDYY)
                                               TO  USBYO-CDSYU
                   MOVE    SPA-GMN-C03-BYOMEIINPUTCD (IDYY)
                                               TO  USBYO-BYOMEIINPUTCD
               END-IF
      *
               IF      SPA-GMN-C03-BUNRUICD    NOT =   ZERO
                   MOVE    SPA-GMN-C03-BUNRUICD    TO  USBYO-BUNRUICD
               ELSE
                   MOVE    SPACE               TO  USBYO-BUNRUICD
               END-IF
               IF      SPA-GMN-C03-DSPSEQ      =   ZERO
                   MOVE    50                  TO  USBYO-DSPSEQ
               ELSE
                   MOVE    SPA-GMN-C03-DSPSEQ      TO  USBYO-DSPSEQ
               END-IF
      *                                 
               MOVE    ZERO                TO  IDZ 
               MOVE    ZERO                TO  USBYO-BYOMEICDSU
               MOVE    SPA-NAI-C03-BYOMEIHENFLG
                                           TO  USBYO-BYOMEIHENFLG
               MOVE    SPACE               TO  USBYO-BYOMEICD-G
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >       WRK-CONS-BYOMEICD-MAX
                   IF      SPA-NAI-C03-BYOMEICD (IDY) 
                                               NOT =   SPACE
                       ADD     1               TO  USBYO-BYOMEICDSU
                       ADD     1               TO  IDZ 
                       MOVE    SPA-NAI-C03-BYOMEICD (IDY)
                                               TO  USBYO-BYOMEICD (IDZ)
                   END-IF
               END-PERFORM
               IF      SPA-GMN-C03-BYOMEICD-G  =   SPACE
                   IF      USBYO-BYOMEICDSU    <   21   
                       ADD     1               TO  IDZ 
                       MOVE    ALL "9"         TO  USBYO-BYOMEICD (IDZ)
                   END-IF 
               END-IF
               MOVE    SPA-GMN-C03-UTAGAIFLG   TO  USBYO-UTAGAIFLG
               MOVE    SPA-GMN-C03-MANSEIKBN   TO  USBYO-MANSEIKBN
               MOVE    SPA-GMN-C03-BYOMEI      TO  USBYO-BYOMEI
               MOVE    SPA-NAI-C03-BYOMEIMOJI  TO  USBYO-BYOMEIMOJI
               MOVE    SPA-GMN-C03-HOSOKU-COMMENT
                                               TO  USBYO-HOSOKU-COMMENT
               MOVE    SPA-GMN-C03-CHARTBYOMEI TO  USBYO-CHARTBYOMEI
               MOVE    SPA-NAI-C03-CHARTBYOMEIMOJI
                                           TO  USBYO-CHARTBYOMEIMOJI
               MOVE    SPA-GMN-C03-HKNBYOMEIFLG
                                           TO  USBYO-HKNBYOMEIFLG
               MOVE    SPA-GMN-C03-BYOMEISBT   TO  USBYO-BYOMEISBT
               MOVE    SPA-OPID                TO  USBYO-OPID
               IF      FLG-USERBYOMEI      =   1
                   MOVE    SMCNDATE-YMD        TO  USBYO-CREYMD
                   MOVE    SMCNDATE-YMD        TO  USBYO-UPYMD
                   MOVE    SMCNDATE-HMS        TO  USBYO-UPHMS
                   MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
                   MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE    2                   TO  WRK-SYOKBN
                                                   WRK-UPDFLG (IDYY)
               ELSE
                   MOVE    SMCNDATE-YMD        TO  USBYO-UPYMD
                   MOVE    SMCNDATE-HMS        TO  USBYO-UPHMS
                   MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
                   MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   IF      WRK-SYOKBN          =   ZERO
                       MOVE    1                   TO  WRK-SYOKBN
                   END-IF
               END-IF
               MOVE    "tbl_userbyomei"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1004"              TO  SPA-ERRCD
                   DISPLAY MCP-FUNC "USERBYOMEI-KEY =>" USBYO-KEY 
                   GO  TO  4502-TOUROKU-SYORI-EXT
               END-IF
             END-IF
            END-PERFORM
      *
      *     自院病名コード変更時、変更前のコードを削除する
            IF      SPA-NAI-C03-BYOMEIINPUTCD-OLD
                                NOT =   SPACE
              IF      SPA-NAI-C03-BYOMEIINPUTCD-OLD
                                        =   SPA-NAI-C03-BYOMEIINPUTCD
                                                                   (1)
                                        OR  SPA-NAI-C03-BYOMEIINPUTCD
                                                                   (2)
                                        OR  SPA-NAI-C03-BYOMEIINPUTCD
                                                                   (3)
               CONTINUE
              ELSE
               INITIALIZE                  USBYO-KEY
               MOVE    SPA-NAI-C03-CDSYU-OLD
                                           TO  USBYO-CDSYU
               MOVE    SPA-NAI-C03-BYOMEIINPUTCD-OLD
                                           TO  USBYO-BYOMEIINPUTCD 
               MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
               MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_userbyomei"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC      NOT =   ZERO 
                   MOVE    "1002"          TO  SPA-ERRCD
                   DISPLAY MCP-FUNC "USERBYOMEI-KEY =>" USBYO-KEY 
                   GO  TO  4502-TOUROKU-SYORI-EXT
               END-IF
              END-IF
            END-IF
           END-IF
      *
      *    患者病名画面から病名遷移時
           IF      SPA-NAI-C03-HENFLG  =   1
               GO  TO  4502-TOUROKU-SYORI-EXT
           END-IF
      *
      *    分類一覧
           INITIALIZE                  SPA-GMN-C03-BUNRUICD-TBL
           PERFORM 3102-BUNRUI-LST-SEC
      *
      *    自院病名一覧
           INITIALIZE                  SPA-GMN-C03-BYOMEI-TBL
                                       SPA-NAI-C03-TBL
           MOVE    ZERO                TO  SPA-GMN-C03-MAX
           IF      WRK-SYOKBN          =   2
      *        追加のみ表示
               PERFORM 45021-USERBYOMEI-LST-SEC
           ELSE
               INITIALIZE                  USERBYOMEI-REC
               IF      SPA-NAI-C03-BUNRUI-SELNO    >   ZERO
                   MOVE
                       SPA-GMN-C03-BBUNRUICD (SPA-NAI-C03-BUNRUI-SELNO)
                                               TO  USBYO-BUNRUICD
                   MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
                   MOVE    "key7"              TO  WRK-MCP-PATHNAME
               ELSE
                   MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
                   MOVE    "key3"              TO  WRK-MCP-PATHNAME
               END-IF
               PERFORM 3101-USERBYOMEI-LST-SEC
           END-IF
      *
           PERFORM 420-CLEAR-SEC
           .
       4502-TOUROKU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名一覧（追加のみ表示）
      *****************************************************************
       45021-USERBYOMEI-LST-SEC        SECTION.
      *
           MOVE    1                   TO  SPA-GMN-C03-PAGE
           COMPUTE WRK-STR         = ( SPA-GMN-C03-PAGE  -  1 )
                                   *   WRK-TBL-MAX
                                   +   1
      *
           MOVE    ZERO                TO  IDX
                                           IDX2
                                           SPA-GMN-C03-MAX
      *
           PERFORM VARYING IDYY    FROM    1   BY  1
                   UNTIL   IDYY    >       3
               IF      WRK-UPDFLG (IDYY)   =   2
                   INITIALIZE                  USERBYOMEI-REC
                   MOVE    SPA-NAI-C03-CDSYU         (IDYY)
                                               TO  USBYO-CDSYU
                   MOVE    SPA-GMN-C03-BYOMEIINPUTCD (IDYY)
                                               TO  USBYO-BYOMEIINPUTCD
                   PERFORM 900-USBYO-INV-SEC
                   IF      FLG-USERBYOMEI      =   0
                       ADD     1               TO  IDX2
                       IF      IDX2            >=  WRK-STR
                           PERFORM 31011-USERBYOMEI-SPA-SEC
                       END-IF
                   END-IF    
               END-IF
           END-PERFORM    
      *
           IF    ( IDX                 >=  WRK-TBL-MAX  )  AND
                 ( FLG-USERBYOMEI      =   ZERO         )
               MOVE    1                   TO  SPA-GMN-C03-NEXT
           ELSE
               MOVE    ZERO                TO  SPA-GMN-C03-NEXT
           END-IF
      *
           MOVE    1                   TO  FLG-ROW
      *
           .
       45021-USERBYOMEI-LST-EXT.
           EXIT.
      *
      *****************************************************************
      *    警告チェック処理
      *****************************************************************
       4501-KEIKOKU-CHK-SEC                SECTION.
      *
      *    未入力項目のチェック
           IF      SPA-GMN-C03-BUNRUICD       =   ZERO
           AND     SPA-GMN-C03-BYOMEIINPUTCD-G
                                              =   SPACE
               MOVE    "0008"               TO  SPA-ERRCD
               MOVE    2                    TO  SPA-GMN-C03-CUR
               GO  TO  4501-KEIKOKU-CHK-EXT
           END-IF
           IF      SPA-GMN-C03-BUNRUICD       =   ZERO
               IF      SPA-GMN-C03-BUNRUIMEI  NOT =   SPACE
                   MOVE    "0008"               TO  SPA-ERRCD
                   MOVE    3                    TO  SPA-GMN-C03-CUR
                   GO  TO  4501-KEIKOKU-CHK-EXT
               END-IF
           ELSE
               IF      SPA-GMN-C03-BUNRUIMEI      =   SPACE
                   MOVE    "0008"               TO  SPA-ERRCD
                   MOVE    4                    TO  SPA-GMN-C03-CUR
                   GO  TO  4501-KEIKOKU-CHK-EXT
               END-IF
           END-IF
           IF      SPA-GMN-C03-BYOMEIINPUTCD-G =   SPACE
               IF      SPA-GMN-C03-BYOMEI         NOT =   SPACE
                   MOVE    "0008"               TO  SPA-ERRCD
                   MOVE    2                    TO  SPA-GMN-C03-CUR
                   GO  TO  4501-KEIKOKU-CHK-EXT
               END-IF
           END-IF
      *
      *    入力コードのチェック
           IF      SPA-GMN-C03-BYOMEIINPUTCD (1)
                                           NOT =
                                       SPA-NAI-C03-BYOMEIINPUTCD (1)
               MOVE    "0032"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-C03-CUR
               GO  TO  4501-KEIKOKU-CHK-EXT
           END-IF
           IF      SPA-GMN-C03-BYOMEIINPUTCD (2)
                                           NOT =
                                       SPA-NAI-C03-BYOMEIINPUTCD (2)
               MOVE    "0032"              TO  SPA-ERRCD
               MOVE    18                  TO  SPA-GMN-C03-CUR
               GO  TO  4501-KEIKOKU-CHK-EXT
           END-IF
           IF      SPA-GMN-C03-BYOMEIINPUTCD (3)
                                           NOT =
                                       SPA-NAI-C03-BYOMEIINPUTCD (3)
               MOVE    "0032"              TO  SPA-ERRCD
               MOVE    19                  TO  SPA-GMN-C03-CUR
               GO  TO  4501-KEIKOKU-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-C03-BYOMEIINPUTCD-G  =   SPACE
               CONTINUE
           ELSE
               IF      SPA-NAI-C03-BYOMEIMOJI   =   ZERO
                   IF      SPA-GMN-C03-BYOMEI       =   SPACE
                       MOVE    "0010"               TO  SPA-ERRCD
                   ELSE    
                       MOVE    "0011"               TO  SPA-ERRCD
                   END-IF    
                   GO  TO  4501-KEIKOKU-CHK-EXT
               END-IF
      *
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       21
                       OR      SPA-NAI-C03-BYOMEI   (IDX)
                                   =       SPACE
                   IF      SPA-NAI-C03-BYOMEI   (IDX)
                                           =   "■■■"
                       MOVE    "0076"               TO  SPA-ERRCD
                       GO  TO  4501-KEIKOKU-CHK-EXT
                   END-IF    
               END-PERFORM
           END-IF    
      *
      *    入力コードの重複入力のチェック
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       3
               IF      SPA-GMN-C03-BYOMEIINPUTCD (IDX)
                                           NOT =   SPACE
                   PERFORM VARYING IDYY    FROM    IDX     BY  1
                           UNTIL   IDYY    >       3
                       IF    ( SPA-GMN-C03-BYOMEIINPUTCD (IDYY)
                                                   NOT =   SPACE )
                       AND   ( IDYY                    >    IDX  )
                           IF      SPA-GMN-C03-BYOMEIINPUTCD (IDX)
                                   =   SPA-GMN-C03-BYOMEIINPUTCD(IDYY)
                               MOVE    2           TO  SPA-GMN-C03-CUR
                               MOVE    "0079"      TO  SPA-ERRCD
                               GO  TO  4501-KEIKOKU-CHK-EXT
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      * 
           MOVE    SPACE           TO  WRK-ERR-BYOMEI
      * 
      *    慢性と急性の警告チェック
           IF    ( SPA-GMN-C03-UTAGAIFLG       =   "2" OR  "3" ) 
               MOVE    ZERO                TO  FLG-MANSEI
                                               FLG-KYUSEI
               PERFORM VARYING IDZZ    FROM    1   BY  2
                       UNTIL   IDZZ    >       79
                   IF      SPA-GMN-C03-BYOMEI (IDZZ:4)
                                                   =   "慢性"
                       MOVE    1               TO  FLG-MANSEI
                   END-IF
                   IF      SPA-GMN-C03-BYOMEI (IDZZ:4)
                                                   =   "急性"
                       MOVE    1               TO  FLG-KYUSEI
                   END-IF
                   IF    ( FLG-MANSEI              =   1 )
                   AND   ( FLG-KYUSEI              =   1 )
                       MOVE    80                      TO  IDZZ
                   END-IF
               END-PERFORM    
      *
               IF      FLG-MANSEI              =   1
                   IF      FLG-KYUSEI              =   1
                       CONTINUE
                   ELSE
                       MOVE    SPA-GMN-C03-BYOMEI
                                               TO  WRK-ERR-BYOMEI
                       IF      SPA-CIDCD       =   SPACE
                           MOVE    "0105"          TO  SPA-CIDCD
                           GO  TO  4501-KEIKOKU-CHK-EXT
                       END-IF 
                   END-IF
               END-IF
           END-IF
      *
      *    分類名の警告チェック
           IF      SPA-GMN-C03-BYOMEIINPUTCD-G
                                       NOT =   SPACE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-C03-BUNRUI-MAX
                   IF    ( SPA-GMN-C03-BUNRUICD
                                       =   SPA-GMN-C03-BBUNRUICD (IDX) )
                   AND   ( SPA-GMN-C03-BUNRUIMEI
                                   NOT =   SPA-GMN-C03-BBUNRUIMEI(IDX) )
                       IF      SPA-CIDCD       =   SPACE
                           MOVE    "0106"          TO  SPA-CIDCD
                           GO  TO  4501-KEIKOKU-CHK-EXT
                       END-IF
                   END-IF     
               END-PERFORM
           END-IF 
           .
       4501-KEIKOKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       420-CLEAR-SEC                   SECTION.
      *
           INITIALIZE               SPA-GMN-C03-KOUMOKU
                                    SPA-NAI-C03-KOUMOKU
                                    SPA-NAI-C03-SELNO
      *
           IF      SPA-NAI-C03-BUNRUI-SELNO    >   ZERO
               MOVE    SPA-GMN-C03-BBUNRUICD (SPA-NAI-C03-BUNRUI-SELNO)
                                           TO  SPA-GMN-C03-BUNRUICD
               MOVE    SPA-GMN-C03-BBUNRUIMEI(SPA-NAI-C03-BUNRUI-SELNO)
                                           TO  SPA-GMN-C03-BUNRUIMEI                           
           END-IF
      *
           MOVE    SPACE            TO  WRK-HAISIYMDH
      *                                   
           MOVE    1                TO  SPA-GMN-C03-CUR
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    全体表示処理
      *****************************************************************
       440-HYOUJI-SEC             SECTION.
      *
      **     MOVE    ZERO                TO  SPA-GMN-C03-NEXT
           MOVE    1                   TO  SPA-GMN-C03-PAGE
      *
           INITIALIZE                  SPA-GMN-C03-BYOMEI-TBL
                                       SPA-NAI-C03-TBL
                                       SPA-NAI-C03-SELNO
                                       SPA-NAI-C03-BUNRUI-SELNO
                                       SPA-GMN-C03-KOUMOKU
                                       SPA-NAI-C03-KOUMOKU
           MOVE    ZERO                TO  SPA-GMN-C03-MAX
      *    検索処理
           MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
           MOVE    "key3"              TO  WRK-MCP-PATHNAME
           INITIALIZE                  USERBYOMEI-REC
           PERFORM 3101-USERBYOMEI-LST-SEC
      *
           .
       440-HYOUJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名検索へ
      *****************************************************************
       450-C05-SYORI-SEC           SECTION.
      *
           IF      SPA-NAI-C03-BYOMEIHENFLG    =  "1"
               GO  TO  450-C05-SYORI-EXT
           END-IF
      *
      *   入力病名コード数カウント
           MOVE    ZERO                TO  IDX1
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-CONS-BYOMEICD-MAX
               IF      SPA-NAI-C03-BYOMEICD(IDX)
                                       NOT =   SPACE
               AND     SPA-NAI-C03-BYOMEICD(IDX)(1:1)
                                       NOT =   "Z"                        
                   ADD     1               TO  IDX1
               END-IF
           END-PERFORM                                    
           IF     IDX1                    >   1
               MOVE    "0056"             TO  SPA-ERRCD
               GO  TO  450-C05-SYORI-EXT
           END-IF             
      *
      *    文字チェック
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
               IF    ( SPA-GMN-C03-BYOMEICD (IDX)  NOT =   SPACE )
               AND   ( SPA-NAI-C03-BYOMEI   (IDX)(1:2) =   "■"  )
                   MOVE    "0035"          TO  SPA-ERRCD
                   GO  TO  450-C05-SYORI-EXT
               END-IF
           END-PERFORM
      *
      *    画面移行用のパラメタ変更
      *    受付と患者検索以外の元をセット
           MOVE    "C05"               TO  SPA-SAKIPG
      *
           INITIALIZE                  SPA-C01KYOUTU
           MOVE    "C03"               TO  SPA-C01-MOTOPG
                                           SPA-MOTOPG
           MOVE    ZERO                TO  IDY
      * 
           IF      SPA-NAI-C03-BYOMEIMOJI  >   ZERO
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-CONS-BYOMEICD-MAX
                   IF      SPA-NAI-C03-BYOMEICD (IDX)  NOT =   SPACE
                       ADD     1               TO  IDY              
                       MOVE    SPA-NAI-C03-BYOMEICD (IDX)
                                           TO  SPA-C01-TBYOMEICD (IDY)
                       MOVE    SPA-NAI-C03-BYOMEI   (IDX)
                                           TO  SPA-C01-TBYOMEI   (IDY)
                   END-IF                              
               END-PERFORM
           END-IF 
      *
           MOVE    SPA-C01KYOUTU       TO  SPA-WORK
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "C05    "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       450-C05-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名コード表示へ
      *****************************************************************
       490-C09-SYORI-SEC           SECTION.
      *
           IF    ( SPA-GMN-C03-BYOMEI  =   SPACE         )
      *****OR    ( SPA-NAI-C03-BYOMEICD (1)
      *****                        =   WRK-CONS-BYOMEICD )
               GO  TO  490-C09-SYORI-EXT
           END-IF
      *  
           MOVE    SPACE                TO  C09
      *
           INITIALIZE                       C09
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-CONS-BYOMEICD-MAX
                   OR      SPA-NAI-C03-BYOMEICD (IDX)  =   SPACE
               MOVE    IDX                     TO  WRK-NUM
               MOVE    WRK-NUM (4:2)           TO  C09-TBANGO    (IDX)
               MOVE    SPA-NAI-C03-BYOMEICD (IDX)
                                               TO  C09-TBYOMEICD (IDX)
               MOVE    SPA-NAI-C03-BYOMEI   (IDX)
                                               TO  C09-TBYOMEI   (IDX)
               MOVE    IDX                     TO  C09-COUNT
           END-PERFORM
      *
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "C03"                TO  SPA-MOTOPG
           MOVE    "C09"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
           .
       490-C09-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       460-MAEGMN-SEC             SECTION.
      *
           IF      SPA-GMN-C03-PAGE        =   1
               GO      TO      460-MAEGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-C03-PAGE    =   SPA-GMN-C03-PAGE    -   1
      *
           INITIALIZE                  SPA-GMN-C03-BYOMEI-TBL
                                       SPA-NAI-C03-TBL
                                       SPA-NAI-C03-SELNO
           MOVE    ZERO                TO  SPA-GMN-C03-MAX
      *    検索処理
      *
      *    分類コードのみ入力のとき分類名に属する病名の表示
           INITIALIZE                  USERBYOMEI-REC
      *****MOVE    SPA-HOSPID          TO  USBYO-HOSPID
           IF      SPA-NAI-C03-BUNRUI-SELNO
                                      >   ZERO
               MOVE
                   SPA-GMN-C03-BBUNRUICD   (SPA-NAI-C03-BUNRUI-SELNO)
                                           TO  USBYO-BUNRUICD
               MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
               MOVE    "key3"              TO  WRK-MCP-PATHNAME
           END-IF
           PERFORM 3101-USERBYOMEI-LST-SEC
      *
           .
       460-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁処理
      *****************************************************************
       470-ATOGMN-SEC             SECTION.
      *
           IF      SPA-GMN-C03-NEXT        =   ZERO
               GO      TO      470-ATOGMN-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-C03-PAGE
      *
           INITIALIZE                  SPA-GMN-C03-BYOMEI-TBL
                                       SPA-NAI-C03-TBL
                                       SPA-NAI-C03-SELNO
           MOVE    ZERO                TO  SPA-GMN-C03-MAX
      *    検索処理
           INITIALIZE                  USERBYOMEI-REC
      *****MOVE    SPA-HOSPID          TO  USBYO-HOSPID
           IF      SPA-NAI-C03-BUNRUI-SELNO
                                      >   ZERO
               MOVE    SPA-GMN-C03-BBUNRUICD (SPA-NAI-C03-BUNRUI-SELNO)
                                           TO  USBYO-BUNRUICD
               MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "tbl_userbyomei"    TO  WRK-MCP-TABLE
               MOVE    "key3"              TO  WRK-MCP-PATHNAME
           END-IF
           PERFORM 3101-USERBYOMEI-LST-SEC
      *
           .
       470-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    リスト出力前処理
      *****************************************************************
       490-PRINT-GID-SEC             SECTION.
      *
      *    ジョブ管理チェック処理
           PERFORM 900-JOBKANRI-CHK-SEC
           IF    ( FLG-JOBEND   =   ZERO )
               MOVE    "7777"          TO  SPA-ERRCD
               MOVE    SPACE           TO  WRK-SHORIERR
               STRING  "処理中です【"  DELIMITED  BY  SIZE
                       JOB-SHELLMSG    DELIMITED  BY  SPACE
                       "】"            DELIMITED  BY  SIZE
               INTO  WRK-SHORIERR
               END-STRING
               GO  TO  490-PRINT-GID-EXT
           END-IF
      *
           IF  SPA-GMN-C03-MAX             >   ZERO
               MOVE    "0101"              TO  SPA-CIDCD5
           ELSE
               MOVE    "1003"              TO  SPA-ERRCD
           END-IF
      *
           .
       490-PRINT-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名一覧出力処理
      *****************************************************************
       490-PRINT-SEC             SECTION.
      *
      *    ジョブ開始セット処理
           PERFORM 900-JOBKANRI-HENSYU-SEC
      *
      *    ファイルデイレクトリ位置指定
           INITIALIZE              ORCSMKPASSAREA
           MOVE    SHELL-NAME      TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"    USING   ORCSMKPASSAREA
      *
           MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
           MOVE    SPA-TERMID      TO  SHELLTBL-ARG1
           MOVE    SPA-SYSYMD      TO  SHELLTBL-ARG2
grpsys     MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG3
      *
           EVALUATE    SPA-CID5-FLG
               WHEN    "OK"
                   MOVE    "0"         TO  SHELLTBL-ARG4
               WHEN    "CSV"
                   MOVE    "1"         TO  SHELLTBL-ARG4
           END-EVALUATE
      *
           MOVE    CONST-JOBID         TO  SHELLTBL-ARG5
           MOVE    CONST-JOB-SHELLID   TO  SHELLTBL-ARG6
      *
           MOVE    SPA-STAFFCD         TO  SHELLTBL-ARG7
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "shell"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    SPACE           TO  SPA-MOTOPG
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGC99"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
      *
      *
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "C03"           TO  SPA-MOTOPG
           MOVE    "C99"           TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "C99"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
           .
      *
       490-PRINT-EXT.
           EXIT.
      *
      *----(03.01.04)--ADD-START---
      *****************************************************************
      *    情報削除へ
      *****************************************************************
       480-JYOUHOU-DEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-C02FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "C03"               TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
      *
           MOVE    CONST-JOB-SHELLID   TO  SPA-JOB-SHELLID
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XB01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       480-JYOUHOU-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認画面
      *****************************************************************
       490-KAKUNIN-SEC                 SECTION.
      *
           MOVE    SPACE           TO  SPA-MOTOPG
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGC99"       USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
      *
      *
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "C03"           TO  SPA-MOTOPG
           MOVE    "C99"           TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "C99"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    同義語マスタチェック処理
      *****************************************************************
       5001-SYNONYM-CHK-SEC            SECTION.
      * 
           INITIALIZE                  SYNONYM-REC
           MOVE    SPA-C03-IKOMOTO-BYOMEI
                                   TO  SYNONYM-BYOMEI
      *    同義語マスタから病名検索処理
           PERFORM 5002-SYNONYM-CHK-SEC
      *
           IF      FLG-SYNONYM     =   ZERO
               MOVE    BYO-BYOMEICD    TO  SPA-C03-IKOSAKI-BYOMEICD
               MOVE    BYO-BYOMEI      TO  SPA-C03-IKOSAKI-BYOMEI
           ELSE
      *********INITIALIZE                  SPA-C03-IKOSAKI-AREA
               MOVE    SPACE           TO  SPA-CIDCD2
           END-IF
      *
           .
       5001-SYNONYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    同義語マスタから病名検索処理
      *****************************************************************
       5002-SYNONYM-CHK-SEC            SECTION.
      *
           IF      SPA-C02-1042-MSG2-DSPKBN
                                       =   "1"
               MOVE    1                   TO  FLG-SYNONYM
               GO  TO  5002-SYNONYM-CHK-EXT
           END-IF
      *
           PERFORM 900-SYNONYM-READ-SEC
      *
           IF      FLG-SYNONYM         =   ZERO
               MOVE    SYNONYM-BYOMEICD    TO  BYO-BYOMEICD
               MOVE    ZERO                TO  FLG-BYOMEI
               PERFORM 900-BYOMEI-READ-SEC
               IF      FLG-BYOMEI          =   1
                   MOVE    1                   TO  FLG-SYNONYM
               ELSE
                   IF      BYO-HAISIYMD        =   SPACE   OR  ALL "9"
                       CONTINUE
                   ELSE    
                       MOVE    1                   TO  FLG-SYNONYM
                   END-IF
               END-IF
           END-IF
      *
           .
       5002-SYNONYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    疑いフラグ編集処理
      *****************************************************************
       5001-UTAGAIFLG-HENSYU-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-HENFLG
      * 
           MOVE    SPACE               TO  SPA-GMN-C03-UTAGAIFLGMEI    
           IF      SPA-GMN-C03-UTAGAIFLG   =   SPACE
               MOVE    1                   TO  WRK-HENFLG
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-C03-UTAGAIFLG-MAX
                       OR      WRK-HENFLG  =   1
                   IF      SPA-GMN-C03-UTAGAIFLG 
                                  =   SPA-GMN-C03-UTAGAIFLGL (IDY)
                       MOVE    SPA-GMN-C03-UTAGAIFLGLST (IDY)
                                          TO  SPA-GMN-C03-UTAGAIFLG-G
                       MOVE    1          TO  WRK-HENFLG
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-UTAGAIFLG-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    慢性フラグ編集処理
      *****************************************************************
       5001-MANSEIKBN-HENSYU-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-HENFLG
      *     
           MOVE    SPACE               TO  SPA-GMN-C03-MANSEIKBNMEI    
           IF      SPA-GMN-C03-MANSEIKBN   =   SPACE
               MOVE    1                   TO  WRK-HENFLG
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-C03-MANSEIKBN-MAX
                       OR      WRK-HENFLG  =   1
                   IF      SPA-GMN-C03-MANSEIKBN 
                                   =   SPA-GMN-C03-MANSEIKBNL (IDY)
                       MOVE    SPA-GMN-C03-MANSEIKBNLST (IDY)
                                           TO  SPA-GMN-C03-MANSEIKBN-G
                       MOVE    1           TO  WRK-HENFLG
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-MANSEIKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名種別編集処理
      *****************************************************************
       5001-BYOMEISBT-HENSYU-SEC       SECTION.
      *
           MOVE    ZERO               TO  WRK-HENFLG
      *
           MOVE    SPACE              TO  SPA-GMN-C03-BYOMEISBTMEI     
           PERFORM VARYING IDY FROM   1   BY  1
                   UNTIL   IDY >      SPA-GMN-C03-BYOMEISBT-MAX
                   OR      WRK-HENFLG =   1
               IF      SPA-GMN-C03-BYOMEISBT 
                                  =   SPA-GMN-C03-BYOMEISBTL (IDY)
                   MOVE    SPA-GMN-C03-BYOMEISBTLST (IDY)
                                      TO  SPA-GMN-C03-BYOMEISBT-G
                   MOVE    1          TO  WRK-HENFLG
               END-IF
           END-PERFORM
      *
           .
       5001-BYOMEISBT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定疾患対象区分、難病外来対象区分編集処理
      *****************************************************************
       5001-HYOUJI-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE               TO  SPA-GMN-C03-HYOUJI
      *
           EVALUATE    SPA-GMN-C03-MANSEIKBN
      ***      WHEN    SPACE
      ***          IF    ( SPA-NAI-C03-NANBYOCD
      ***                                  NOT =   ZERO             )
      ***          AND   ( SPA-NAI-C03-MANSEIKBN
      ***                                      =   "05"    OR  "08" )
      ***              MOVE    "特定疾患・難病外来対象"
      ***                                      TO  SPA-GMN-C03-HYOUJI
      ***          ELSE
      ***              IF      SPA-NAI-C03-MANSEIKBN
      ***                                      =   "05"    OR  "08"
      ***                  MOVE    "特定疾患対象"  TO
      ***                                          SPA-GMN-C03-HYOUJI
      ***              END-IF
      ***              IF      SPA-NAI-C03-NANBYOCD
      ***                                      NOT =   ZERO
      ***                  MOVE    "難病外来対象"  TO
      ***                                          SPA-GMN-C03-HYOUJI
      ***              END-IF
      ***          END-IF
               WHEN    "09"
                   IF      SPA-NAI-C03-MANSEIKBN
                                               =   "05"    OR  "08"
                       MOVE    "特定疾患対象"  TO  SPA-GMN-C03-HYOUJI
                   END-IF
               WHEN    OTHER    
                   IF      SPA-NAI-C03-NANBYOCD
                                           NOT =   ZERO
                       MOVE    "難病外来対象"  TO  SPA-GMN-C03-HYOUJI
                   END-IF
           END-EVALUATE
           .
       5001-HYOUJI-HENSYU-EXT.
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
           IF      SPA-CIDCD       NOT =   SPACE
               PERFORM 520-CIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-CIDCD2      NOT =   SPACE
               PERFORM 530-CIDSET2-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-CIDCD3      NOT =   SPACE
               PERFORM 540-CIDSET3-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-CIDCD4      NOT =   SPACE
               PERFORM 540-CIDSET4-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-CIDCD5      NOT =   SPACE
               PERFORM 550-CIDSET5-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-CIDCD6      NOT =   SPACE
               PERFORM 560-CIDSET6-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "C03    "           TO  MCP-WINDOW
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
           IF      FLG-ROW             =   1
               MOVE    ZERO                TO  WRK-C03-BYO-ROW
           ELSE
               MOVE    C03-BYOMEILST-ROW   TO  WRK-C03-BYO-ROW
           END-IF
      * 
           MOVE    SPACE               TO  C03
           INITIALIZE                      C03
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C03-MAX
               MOVE    SPA-GMN-C03-TBANGO    (IDX)
                                           TO  WRK-NUM
               MOVE    WRK-NUM             TO  C03-TBANGO        (IDX)
               MOVE    SPA-GMN-C03-TBUNRUIMEI    (IDX)
                                           TO  C03-TBUNRUIMEI    (IDX)
               MOVE    SPA-GMN-C03-TBYOMEIINPUTCD(IDX)
                                           TO  C03-TBYOMEIINPUTCD(IDX)
               MOVE    SPA-GMN-C03-TBYOMEISBT (IDX)
                                           TO  C03-TBYOMEISBT    (IDX)
               MOVE    SPA-GMN-C03-TBYOMEI    (IDX)
                                           TO  C03-TBYOMEI       (IDX)
               MOVE    SPA-GMN-C03-TCHARTBYOMEI (IDX)
                                           TO  C03-TCHARTBYOMEI  (IDX)
               MOVE    SPA-GMN-C03-TMANSEIKBN (IDX)
                                           TO  C03-TMANSEIFLG    (IDX)
               MOVE    SPA-GMN-C03-TUTAGAIFLG (IDX)
                                           TO  C03-TUTAGAIFLG    (IDX)
               MOVE    SPA-GMN-C03-THKNBYOMEIFLG (IDX)
                                           TO  C03-THKNBYOMEIFLG (IDX)
               MOVE    SPA-GMN-C03-TDSPSEQ (IDX)
                                           TO  C03-TDSPSEQ       (IDX)
      *        
               IF      IDX             =   SPA-NAI-C03-SELNO
                   MOVE    "T"         TO  C03-BYOMEILST-SELECT (IDX)
               ELSE
                   MOVE    "F"         TO  C03-BYOMEILST-SELECT (IDX)
               END-IF
      *
               IF      SPA-NAI-C03-TBYOMEIHENFLG (IDX)
                                           =   "1"
                   MOVE    "#FFCC99"    TO  C03-BYOMEILST-BGCOLOR (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-C03-MAX         TO  C03-BYOMEILST-COUNT
           MOVE    ZERO                TO  C03-BYOMEILST-ROW
           IF      WRK-C03-BYO-ROW     >   ZERO
               MOVE    WRK-C03-BYO-ROW     TO  C03-BYOMEILST-ROW
           END-IF
      *
           MOVE    SPA-GMN-C03-BANGO          TO  C03-BANGO
           MOVE    SPA-GMN-C03-BUNRUICD       TO  C03-BUNRUICD
           MOVE    SPA-GMN-C03-BUNRUIMEI      TO  C03-BUNRUIMEI
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >      3
               MOVE    SPA-GMN-C03-BYOMEIINPUTCD (IDX)
                                                  TO  C03-BYOMEIINPUTCD
                                                                   (IDX)
           END-PERFORM
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >      6
               MOVE    SPA-GMN-C03-BYOMEICD(IDX)  TO  C03-BYOMEICD (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C03-BYOMEI         TO  C03-BYOMEI
           MOVE    SPA-GMN-C03-HOSOKU-COMMENT TO  C03-HOSOKU-COMMENT
           MOVE    SPA-GMN-C03-CHARTBYOMEI    TO  C03-CHARTBYOMEI
           MOVE    SPA-GMN-C03-BYOMEIHENFLG   TO  C03-BYOMEIHENFLG
           MOVE    SPA-GMN-C03-DSPSEQ         TO  C03-DSPSEQ
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-C03-BUNRUI-MAX
               MOVE    SPA-GMN-C03-BBUNRUICD (IDX)
                                               TO  C03-BBUNRUICD (IDX)
               MOVE    SPA-GMN-C03-BBUNRUIMEI(IDX)
                                               TO  C03-BBUNRUIMEI(IDX)
      *        
               IF      IDX             =   SPA-NAI-C03-BUNRUI-SELNO
                   MOVE    "T"         TO  C03-BUNRUI-SELECT (IDX)
               ELSE
                   MOVE    "F"         TO  C03-BUNRUI-SELECT (IDX)
               END-IF
           END-PERFORM    
           MOVE    SPA-GMN-C03-BUNRUI-MAX    TO  C03-BUNRUI-COUNT
           MOVE    ZERO                  TO  C03-BUNRUI-ROW
      *
           MOVE    SPA-GMN-C03-UTAGAIFLG-G TO  C03-UTAGAIFLG
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   5
                   OR          SPA-GMN-C03-UTAGAIFLGL  (IDX)  =   SPACE
               MOVE    SPA-GMN-C03-UTAGAIFLGLST (IDX)
                                           TO  C03-UTAGAIFLG-ITEM (IDX)
               MOVE    IDX                 TO  C03-UTAGAIFLG-COUNT    
           END-PERFORM
      *
           MOVE    SPA-GMN-C03-MANSEIKBN-G TO  C03-MANSEIFLG
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C03-MANSEIKBN-MAX
                   OR          SPA-GMN-C03-MANSEIKBNL  (IDX)  =   SPACE
               MOVE    SPA-GMN-C03-MANSEIKBNLST (IDX)
                                           TO  C03-MANSEIFLG-ITEM (IDX)
               MOVE    IDX                 TO  C03-MANSEIFLG-COUNT
           END-PERFORM
      *
           MOVE    "blue"                  TO  C03-HYOUJI-STYLE
           MOVE    SPA-GMN-C03-HYOUJI      TO  C03-HYOUJI
      *
           MOVE    SPA-GMN-C03-HKNBYOMEIFLG-G TO  C03-HKNBYOMEIFLG
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX     >   5
                   OR          SPA-GMN-C03-HKNBYOMEIFLGL(IDX) =   SPACE
               MOVE    SPA-GMN-C03-HKNBYOMEIFLGLST (IDX)
                                       TO  C03-HKNBYOMEIFLG-ITEM (IDX)
               MOVE    IDX             TO  C03-HKNBYOMEIFLG-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-C03-BYOMEISBT-G TO  C03-BYOMEISBT
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX     >   SPA-GMN-C03-BYOMEISBT-MAX
                   OR          SPA-GMN-C03-BYOMEISBTL(IDX) =   SPACE
               MOVE    SPA-GMN-C03-BYOMEISBTLST (IDX)
                                       TO  C03-BYOMEISBT-ITEM (IDX)
               MOVE    IDX             TO  C03-BYOMEISBT-COUNT
           END-PERFORM
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
             AND ( SPA-GMN-C03-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
         EVALUATE    SPA-GMN-C03-CUR
             WHEN    1
                     MOVE  "BANGO"      TO  MCP-WIDGET
             WHEN    2
                     MOVE  "BYOMEIINPUTCD"
                                        TO  MCP-WIDGET
             WHEN    3
                     MOVE  "BUNRUICD"   TO  MCP-WIDGET
             WHEN    4
                     MOVE  "BUNRUIMEI"  TO  MCP-WIDGET
             WHEN    5
                     MOVE  "BYOMEICD_1" TO  MCP-WIDGET
             WHEN    6
                     MOVE  "BYOMEICD_2" TO  MCP-WIDGET
             WHEN    7
                     MOVE  "BYOMEICD_3" TO  MCP-WIDGET
             WHEN    8
                     MOVE  "BYOMEICD_4" TO  MCP-WIDGET
             WHEN    9
                     MOVE  "BYOMEICD_5" TO  MCP-WIDGET
             WHEN    10
                     MOVE  "BYOMEICD_6" TO  MCP-WIDGET
             WHEN    11
                     MOVE  "BYOMEI"     TO  MCP-WIDGET
             WHEN    20
                     MOVE  "CHARTBYOMEI"
                                        TO  MCP-WIDGET
             WHEN    13
                     MOVE  "UTAGAIFLG"  TO  MCP-WIDGET
             WHEN    14
                     MOVE  "MANSEIFLG"  TO  MCP-WIDGET
             WHEN    15
                     MOVE  "HKNBYOMEIFLG"
                                        TO  MCP-WIDGET
             WHEN    16
                     MOVE  "DSPSEQ"     TO  MCP-WIDGET
             WHEN    17
                     MOVE  "BYOMEISBT"  TO  MCP-WIDGET
             WHEN    18
                     MOVE  "BYOMEIINPUTCD_1"
                                        TO  MCP-WIDGET
             WHEN    19
                     MOVE  "BYOMEIINPUTCD_2"
                                        TO  MCP-WIDGET
             WHEN    12
                     MOVE  "HOSOKU_COMMENT"
                                        TO  MCP-WIDGET
         END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "BANGO"
                   MOVE    3                   TO  SPA-GMN-C03-CUR
               WHEN    "BYOMEIINPUTCD"
                   MOVE    18                  TO  SPA-GMN-C03-CUR
               WHEN    "BYOMEIINPUTCD_1"
                   MOVE    19                  TO  SPA-GMN-C03-CUR
               WHEN    "BYOMEIINPUTCD_2"
                   MOVE    3                   TO  SPA-GMN-C03-CUR
               WHEN    "BUNRUICD"
                   MOVE    4                   TO  SPA-GMN-C03-CUR
               WHEN    "BUNRUIMEI"
                   MOVE    5                   TO  SPA-GMN-C03-CUR
               WHEN    "BYOMEICD_1"
                   MOVE    6                   TO  SPA-GMN-C03-CUR
               WHEN    "BYOMEICD_2"
                   MOVE    7                   TO  SPA-GMN-C03-CUR
               WHEN    "BYOMEICD_3"
                   MOVE    8                   TO  SPA-GMN-C03-CUR
               WHEN    "BYOMEICD_4"
                   MOVE    9                   TO  SPA-GMN-C03-CUR
               WHEN    "BYOMEICD_5"
                   MOVE    10                  TO  SPA-GMN-C03-CUR
               WHEN    "BYOMEICD_6"
                   MOVE    11                  TO  SPA-GMN-C03-CUR
               WHEN    "BYOMEI"
                   MOVE    12                  TO  SPA-GMN-C03-CUR
               WHEN    "HOSOKU_COMMENT"
                   MOVE    20                  TO  SPA-GMN-C03-CUR
               WHEN    "CHARTBYOMEI"
                   MOVE    13                  TO  SPA-GMN-C03-CUR
               WHEN    "UTAGAIFLG"
                   MOVE    14                  TO  SPA-GMN-C03-CUR
               WHEN    "MANSEIFLG"
                   MOVE    15                  TO  SPA-GMN-C03-CUR
               WHEN    "HKNBYOMEIFLG"
                   MOVE    16                  TO  SPA-GMN-C03-CUR
               WHEN    "DSPSEQ"
                   MOVE    17                  TO  SPA-GMN-C03-CUR
               WHEN    "BYOMEISBT"
                   MOVE    3                   TO  SPA-GMN-C03-CUR
               WHEN    "B12"
                   MOVE    99                  TO  SPA-GMN-C03-CUR
           END-EVALUATE
           .
      *
       5011-INPUT-CUR-EXT.
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
                   MOVE
                       "数字のみ７桁での入力はできません"
                                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "既に同一病名入力コードが存在します" 
                                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "対象のデータを選択してありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "対象のデータが選択してあります"
                                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "病名コード順がエラーです"  TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "病名が長すぎます"          TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "項目に入力がありません"    TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "病名入力コードは半角７桁は入力できません" 
                                                       TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "病名が入力されていません"  TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE
                       "病名コードまたは病名の入力にエラーがあります"
                                                       TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "病名番号入力エラー"        TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "数字で入力してください"    TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "疑い入力エラー"            TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "慢性区分入力エラー"        TO  SPA-ERRMSG
               WHEN    "0028"
                   MOVE    "保険病名入力エラー"        TO  SPA-ERRMSG
               WHEN    "0029"
                   MOVE    "基本病名が入力してありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0030"
                   MOVE
                      "先頭の文字が入力できない文字です"
                                                       TO  SPA-ERRMSG
               WHEN    "0031"
                   STRING   "病名マスタ、病名セットコード"
                                               DELIMITED   BY  SIZE
                            "又は予約病名コードに存在するコードです"
                                               DELIMITED   BY  SIZE 
                                               INTO        SPA-ERRMSG
                   END-STRING
               WHEN    "0032"
                   MOVE
                       "登録できない入力コードです"    TO  SPA-ERRMSG
               WHEN    "0034"
                   MOVE    "これ以上追加できません"    TO  SPA-ERRMSG
               WHEN    "0035"
                   MOVE    "不明の病名コードが入力されています"
                                                       TO  SPA-ERRMSG
               WHEN    "0036"
                   MOVE
                      "最後の文字が入力できない文字です"
                                                       TO  SPA-ERRMSG
               WHEN    "0049"
                   MOVE    "病名入力コードが未入力です"
                                                       TO  SPA-ERRMSG
               WHEN    "0051"
                   MOVE    "入力の途中です"            TO  SPA-ERRMSG
               WHEN    "0052"
                   MOVE    "病名種別入力エラー"        TO  SPA-ERRMSG
               WHEN    "0053"
                   MOVE    "病名コードと病名種別が違います"
                                                       TO  SPA-ERRMSG
               WHEN    "0054"
                   MOVE
                       "登録済みの病名入力コードへの変更はできません"
                                                       TO  SPA-ERRMSG
               WHEN    "0056"
                   MOVE    "複数病名の入力はできません"
                                                       TO  SPA-ERRMSG
               WHEN    "0062"
                   MOVE
               "未コード化傷病名は他のコードといっしょに入力できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0063"
                   MOVE
                 "未コード化傷病名が入力されているので入力できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0071"
                   MOVE    "全角入力して下さい"        TO  SPA-ERRMSG
               WHEN    "0072"
                   MOVE    "全角または半角のみで入力して下さい" 
                                                       TO  SPA-ERRMSG
               WHEN    "0074"
               WHEN    "0075"
                   STRING
                       "入力された病名はマスター上で"
                                               DELIMITED   BY  SIZE 
                       WRK-HAISIYMDH           DELIMITED   BY  SIZE 
                       "で廃止の扱いとなっています。"
                                               DELIMITED   BY  SIZE 
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0076"
                   STRING
                       "存在しない病名コードが登録されています、"
                                                DELIMITED   BY  SIZE 
                       "ＣＤ表示で確認して下さい"
                                                DELIMITED   BY  SIZE 
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0077"
                   MOVE
              "病名コードが２２個以上になりますので、編集病名とします。"
                                       TO  SPA-ERRMSG
               WHEN    "0078"
                   MOVE    "病名コードの長さが違います"
                                       TO  SPA-ERRMSG
               WHEN    "0079"
                   MOVE    "同じ入力コードが複数入力されています"
                                               TO  SPA-ERRMSG
               WHEN    "0092"
                   MOVE    "補足コメントの前後に（）があります"
                                               TO  SPA-ERRMSG
               WHEN    "0056"
               WHEN    "0101"
                   MOVE
              "修飾語以外のコードを複数検索しましたので編集病名とします"
                                       TO  SPA-ERRMSG
               WHEN    "0103"
                   MOVE    "病名コードでの組み立てができませんでした。"
                                       TO  SPA-ERRMSG
                   MOVE    "編集病名とします"
                                       TO  SPA-ERRMSG (43:)
               WHEN    "1000"
                   MOVE    "病名コードが有りません"    TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "更新処理でエラーになりました"
                                                       TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除処理でエラーになりました"
                                                       TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "自院病名マスタが存在しません"
                                                       TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "追加処理でエラーになりました"
                                                       TO  SPA-ERRMSG
               WHEN    "1005"
                   MOVE    "分類名の更新処理でエラーになりました"
                                                       TO  SPA-ERRMSG
               WHEN    "7777"
                   MOVE    WRK-SHORIERR    TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  CERR
           MOVE    SPA-ERRCD            TO  CERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  CERR-ERRMSG
           EVALUATE    SPA-ERRCD
               WHEN    "0074"
               WHEN    "0075"
                   MOVE    SPA-C03-HAISI-BYOMEI TO  CERR-BYOMEI
           END-EVALUATE
           MOVE    SPACE                TO  SPA-C03-HAISI-BYOMEI
      *
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "C03"                TO  SPA-MOTOPG
           MOVE    "CERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "CERR"               TO  MCP-WINDOW
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
       520-CIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-CIDCD
               WHEN    "0101"
                   MOVE    "内容が修正されています。"
                                       TO  WRK-CIDMSG
                   MOVE    "ＯＫで修正分は反映されずに終了します"
                                       TO  WRK-CIDMSG (25:)
               WHEN    "0102"
                   MOVE    "自院病名一覧を出力します。"
                                       TO  WRK-CIDMSG
               WHEN    "0103"
                   MOVE    "分類名を削除します。よろしいですか？"
                                       TO  WRK-CIDMSG
               WHEN    "0104"
                   MOVE    "自院病名を削除します。よろしいですか？"
                                       TO  WRK-CIDMSG
               WHEN    "0105"
                   STRING
                       "この病名は「急性」と「慢性」が混在して"
                                                DELIMITED   BY  SIZE 
                       "レセプトに記載されます。更新しますか？"
                                                DELIMITED   BY  SIZE 
                                               INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0106"
                   MOVE
                     "登録済の分類名が変更になります。よろしいですか？"
                                       TO  WRK-CIDMSG
               WHEN    "6666"
                   MOVE    WRK-SHORIERR    TO  WRK-CIDMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD
                                       TO  WRK-CIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-CID1-FLG
      *
           MOVE    SPACE               TO  CID1
           INITIALIZE                      CID1
           EVALUATE    SPA-CIDCD
               WHEN    "0105"
                   MOVE    WRK-ERR-BYOMEI
                                       TO  CID1-BYOMEI
           END-EVALUATE 
           MOVE    SPA-CIDCD           TO  CID1-ID1CODE
           MOVE    WRK-CIDMSG          TO  CID1-ID1MSG
           EVALUATE    SPA-CIDCD
               WHEN    "0103"
               WHEN    "0104"
                   MOVE    "B01"               TO  MCP-WIDGET
               WHEN    OTHER
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "C03"               TO  SPA-MOTOPG
           MOVE    "CID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "CID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-CIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名移行確認画面表示処理
      *****************************************************************
       530-CIDSET2-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-CID2-FLG
                                           WRK-CIDMSG
      *
           EVALUATE    SPA-CIDCD2
               WHEN    "0101"
                   STRING  "選択された病名はマスター上で移行"
                                       DELIMITED  BY  SIZE
                           "病名の扱いとなっています。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0102"
                   STRING  "選択された病名は「傷病名コードの統一の"
                                       DELIMITED  BY  SIZE
                           "推進」による推奨病名があります。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0103"
               WHEN    "0105"
                   STRING  "入力された病名はマスター上で移行"
                                       DELIMITED  BY  SIZE
                           "病名の扱いとなっています。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0104"
               WHEN    "0106"
                   STRING  "入力された病名は「傷病名コードの統一の"
                                       DELIMITED  BY  SIZE
                           "推進」による推奨病名があります。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-CIDMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE               TO  CID2
           INITIALIZE                      CID2
           MOVE    WRK-CIDMSG          TO  CID2-MSG
           MOVE    SPA-C03-IKOMOTO-BYOMEICD
                                       TO  CID2-BYOMEICD1
           MOVE    SPA-C03-IKOMOTO-BYOMEI  
                                       TO  CID2-BYOMEI1
           MOVE    SPA-C03-IKOSAKI-BYOMEICD
                                       TO  CID2-BYOMEICD2
           MOVE    SPA-C03-IKOSAKI-BYOMEI
                                       TO  CID2-BYOMEI2
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "C03"               TO  SPA-MOTOPG
           MOVE    "CID2"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "CID2"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       530-CIDSET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面３表示処理
      *****************************************************************
       540-CIDSET3-SEC              SECTION.
      *
           EVALUATE    SPA-CIDCD3
               WHEN    "0101"
                   MOVE    "分類名に属する病名が存在します。"
                                       TO  WRK-CIDMSG
                   MOVE    "次のいずれかの方法により選択して下さい。"
                                       TO  WRK-CIDMSG (33:)
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-CID3-FLG
      *
           MOVE    SPACE               TO  CID3
           INITIALIZE                      CID3
           MOVE    SPA-CIDCD3          TO  CID3-ID1CODE
           MOVE    WRK-CIDMSG          TO  CID3-MSG
           MOVE    "F11:分類名のみ削除"  
                                       TO  CID3-B11-VALUE  
           MOVE    "F12:分類名・病名とも削除" 
                                       TO  CID3-B12-VALUE  
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "C03"               TO  SPA-MOTOPG
           MOVE    "CID3"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "CID3"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       540-CIDSET3-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面４表示処理
      *****************************************************************
       540-CIDSET4-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-CID4-FLG
      *
           MOVE    SPACE               TO  CID4
           INITIALIZE                      CID4
           MOVE    SPA-CIDCD4          TO  CID4-ID1CODE
           EVALUATE    SPA-CIDCD4
               WHEN    "0103"
                   MOVE    "分類名を削除します。よろしいですか？"
                                       TO  CID4-ID1MSG
               WHEN    "0104"
                   MOVE    "自院病名を削除します。よろしいですか？"
                                       TO  CID4-ID1MSG
           END-EVALUATE
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "C03"               TO  SPA-MOTOPG
           MOVE    "CID4"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "CID4"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       540-CIDSET4-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面５表示処理
      *****************************************************************
       550-CIDSET5-SEC              SECTION.
      *
           EVALUATE    SPA-CIDCD5
               WHEN    "0101"
                   MOVE    "次のいずれかの方法により選択して下さい。"
                                       TO  WRK-CIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-CID5-FLG
      *
           MOVE    SPACE               TO  CID5
           INITIALIZE                      CID5
           MOVE    SPA-CIDCD5          TO  CID5-ID1CODE
           MOVE    WRK-CIDMSG          TO  CID5-MSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "C03"               TO  SPA-MOTOPG
           MOVE    "CID5"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "CID5"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       550-CIDSET5-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名移行確認画面表示処理
      *****************************************************************
       560-CIDSET6-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-CID6-FLG
                                           WRK-CIDMSG
      *
           INITIALIZE                  CID6
      *
           EVALUATE    SPA-CIDCD6
               WHEN    "0101"
                   STRING
                       "選択された病名は"      DELIMITED   BY  SIZE 
                       "移行病名の扱いとなっていますが、"
                                               DELIMITED   BY  SIZE 
                      "別のコードで登録が可能です"
                                               DELIMITED   BY  SIZE
                                       INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0102"
                   STRING
                       "選択された病名は「傷病名コードの統一の推進"
                                               DELIMITED   BY  SIZE
                       "」による推奨病名がありますが、"
                                               DELIMITED   BY  SIZE
                      "別のコードで登録が可能です"
                                               DELIMITED   BY  SIZE
                                       INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0103"
               WHEN    "0115"
                   STRING
                       "入力された病名は"      DELIMITED   BY  SIZE 
                       "移行病名の扱いとなっていますが、"
                                               DELIMITED   BY  SIZE 
                      "別のコードで登録が可能です"
                                               DELIMITED   BY  SIZE
                                       INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0104"
               WHEN    "0116"
                   STRING
                       "入力された病名は「傷病名コードの統一の推進"
                                               DELIMITED   BY  SIZE
                       "」による推奨病名がありますが、"
                                               DELIMITED   BY  SIZE
                       "別のコードで登録が可能です"
                                               DELIMITED   BY  SIZE 
                                       INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0107"
               WHEN    "0117"
                   STRING
                       "入力された病名は"      DELIMITED   BY  SIZE 
                       "廃止の扱いとなっていますが、"
                                               DELIMITED   BY  SIZE 
                       "別のコードで登録が可能です"
                                               DELIMITED   BY  SIZE 
                                       INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0108"
                   STRING
                       "選択された病名は"      DELIMITED   BY  SIZE 
                      "別のコードで登録が可能です"
                                               DELIMITED   BY  SIZE 
                                       INTO    WRK-CIDMSG
                   END-STRING
               WHEN    "0109"
                   STRING
                       "選択された病名は"      DELIMITED   BY  SIZE 
                       "廃止の扱いとなっていますが、"
                                               DELIMITED   BY  SIZE 
                       "別のコードで登録が可能です"
                                               DELIMITED   BY  SIZE 
                                       INTO    WRK-CIDMSG
                   END-STRING
           END-EVALUATE
           MOVE    WRK-CIDMSG          TO  CID6-MSG
      *
           MOVE    SPA-C03-IKOMOTO-BYOMEICD
                                       TO  CID6-BYOMEICD1
           MOVE    SPA-C03-IKOMOTO-BYOMEI  
                                       TO  CID6-BYOMEI1
      *
           EVALUATE    SPA-CIDCD6
               WHEN    "0108"
                   MOVE    WIDGET-INSENSITIVE  TO  CID6-B10-STATE
                   MOVE    "別のコードでの登録を行いますか"
                                               TO  CID6-MSG1 
               WHEN    "0107"
               WHEN    "0109"
               WHEN    "0117"
                   MOVE    WIDGET-INSENSITIVE  TO  CID6-B10-STATE
                   MOVE    "別のコードでの登録を行いますか"
                                               TO  CID6-MSG1 
                   MOVE    "廃止年月日"        TO  CID6-HAISIYMD-H
                   MOVE    WRK-HAISIYMDH       TO  CID6-HAISIYMD
               WHEN    OTHER
                   MOVE    SPA-C03-IKOSAKI-BYOMEICD
                                               TO  CID6-BYOMEICD2
                   MOVE    SPA-C03-IKOSAKI-BYOMEI
                                               TO  CID6-BYOMEI2
                   MOVE    WIDGET-NORMAL       TO  CID6-B10-STATE
                   IF      SPA-C03-IKOMOTO-HAISIYMD
                                               NOT =   SPACE
                       MOVE    "廃止年月日"        TO  CID6-HAISIYMD-H
                       MOVE    SPA-C03-IKOMOTO-HAISIYMD
                                                   TO  CID6-HAISIYMD
                   END-IF
                   EVALUATE    SPA-CIDCD6
                       WHEN    "0102"
                       WHEN    "0104"
                       WHEN    "0116"
                           STRING
                               "推奨する病名へ変更しますか、"
                                                   DELIMITED   BY  SIZE  
                               "別のコードでの登録を行いますか"
                                                   DELIMITED   BY  SIZE  
                                                   INTO  CID6-MSG1
                           END-STRING 
                           MOVE    "推奨する病名"  TO  CID6-LABEL1
                           MOVE    "F10:推奨する病名へ変更"
                                                   TO  CID6-B10-LABEL
                       WHEN    OTHER
                           STRING
                               "移行先病名へ変更しますか、"
                                                   DELIMITED   BY  SIZE  
                               "別のコードでの登録を行いますか"
                                                   DELIMITED   BY  SIZE  
                                                   INTO  CID6-MSG1
                           END-STRING 
                           MOVE    "移行先病名"    TO  CID6-LABEL1
                           MOVE    "F10:移行先病名へ変更"
                                                   TO  CID6-B10-LABEL
                   END-EVALUATE
           END-EVALUATE
      *
           MOVE    SPACE               TO  WRK-HAISIYMDH
      *
           MOVE    "B12"               TO  MCP-WIDGET
           MOVE    WIDGET-NORMAL       TO  CID6-B12-STATE
           MOVE    "red"               TO  CID6-MSG2-STYLE
      *
           IF      SPA-C03-CHANGE-MAX  >   6
               STRING  "別のコードで登録を行う場合は"
                                           DELIMITED   BY  SIZE 
                       "病名コード欄に表示を行いません"
                                           DELIMITED   BY  SIZE 
                                           INTO    CID6-MSG2
               END-STRING
           END-IF
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX     >   6
               MOVE    SPA-C03-CHANGE-BYOMEICD (IDX)
                                       TO  CID6-BYOMEICD (IDX)
               MOVE    "black"         TO  CID6-BYOMEICD-STYLE (IDX)
               IF      SPA-C03-CHANGE-MAX  <=  6
                   EVALUATE    IDX
                       WHEN    1
                       WHEN    3
                       WHEN    5
                           MOVE    "black" TO  CID6-BYOMEICD-STYLE (IDX)
                       WHEN    OTHER
                           MOVE    "blue"  TO  CID6-BYOMEICD-STYLE (IDX)
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  WRK-BYOMEI1
           MOVE    "black"             TO  CID6-BYOMEI10-STYLE
           IF      SPA-C03-CHANGE-MAX  <=  6
               STRING  SPA-C03-CHANGE-BYOMEI (1)
                                           DELIMITED   BY  SPACE
                       SPAN-COLOR-BLUE     DELIMITED   BY  SIZE
                       SPA-C03-CHANGE-BYOMEI (2)
                                           DELIMITED   BY  SPACE
                       SPAN-COLOR-END      DELIMITED   BY  SIZE
                       SPA-C03-CHANGE-BYOMEI (3)
                                           DELIMITED   BY  SPACE
                       SPAN-COLOR-BLUE     DELIMITED   BY  SIZE
                       SPA-C03-CHANGE-BYOMEI (4)
                                           DELIMITED   BY  SPACE
                       SPAN-COLOR-END      DELIMITED   BY  SIZE
                       SPA-C03-CHANGE-BYOMEI (5)
                                           DELIMITED   BY  SPACE
                       SPAN-COLOR-BLUE     DELIMITED   BY  SIZE
                       SPA-C03-CHANGE-BYOMEI (6)
                                           DELIMITED   BY  SPACE
                       SPAN-COLOR-END      DELIMITED   BY  SIZE
                                           INTO    WRK-BYOMEI1
               END-STRING
           ELSE    
               MOVE    SPA-C03-CHANGE-BYOMEI (1)
                                           TO  WRK-BYOMEI1
               PERFORM VARYING IDX FROM    2   BY  1
                       UNTIL   IDX >       SPA-C03-CHANGE-MAX
                   STRING  WRK-BYOMEI1         DELIMITED   BY  SPACE
                           SPA-C03-CHANGE-BYOMEI (IDX)
                                               DELIMITED   BY  SPACE
                                               INTO    WRK-BYOMEI1
                   END-STRING
               END-PERFORM
           END-IF
           MOVE    WRK-BYOMEI1         TO  CID6-BYOMEI10
      *
           MOVE    SPACE               TO  WRK-BYOMEI
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       21
                   OR      SPA-NAI-C03-BYOMEI(IDX) =   SPACE
               STRING  WRK-BYOMEI          DELIMITED   BY  SPACE
                       SPA-NAI-C03-BYOMEI(IDX)
                                           DELIMITED   BY  SPACE
                                           INTO    WRK-BYOMEI
                END-STRING
           END-PERFORM
           MOVE    WRK-BYOMEI          TO  CID6-H-BYOMEI1
           MOVE    "blue"              TO  CID6-H-BYOMEI1-STYLE
      *
           MOVE    SPACE               TO  WRK-BYOMEI
           EVALUATE    SPA-CIDCD6
               WHEN    "0107"
               WHEN    "0108"
               WHEN    "0109"
               WHEN    "0117"
                   CONTINUE
               WHEN    OTHER
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       21
                           OR      SPA-NAI-C03-BYOMEI(IDX) =   SPACE
                       IF      IDX         =   SPA-C03-IKOSAKI-INDEX
                           STRING  WRK-BYOMEI      DELIMITED   BY  SPACE
                                    SPA-C03-IKOSAKI-BYOMEI
                                                   DELIMITED   BY  SPACE
                                                   INTO    WRK-BYOMEI
                           END-STRING
                       ELSE
                           STRING  WRK-BYOMEI      DELIMITED   BY  SPACE
                                   SPA-NAI-C03-BYOMEI (IDX)
                                                   DELIMITED   BY  SPACE
                                                   INTO    WRK-BYOMEI
                           END-STRING
                       END-IF  
                   END-PERFORM
           END-EVALUATE
           MOVE    WRK-BYOMEI          TO  CID6-H-BYOMEI2
           MOVE    "blue"              TO  CID6-H-BYOMEI2-STYLE
      *
           MOVE    "C03"               TO  SPA-MOTOPG
           MOVE    "CID6"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "CID6"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       560-CIDSET6-EXT.
           EXIT.
      *
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
               MOVE    SPACE               TO  WRK-NENGO
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
               MOVE    LNK-DAY2-EDTYMD2-G  TO  WRK-NENGO
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名マスター読込（主キー）
      *****************************************************************
       900-USBYO-INV-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_userbyomei"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-USBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-USERBYOMEI
               INITIALIZE                  USERBYOMEI-REC
           END-IF
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-USBYO-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスター読込
      *****************************************************************
       900-USBYO-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  USERBYOMEI-REC
               MOVE    ZERO            TO  FLG-USERBYOMEI
           ELSE
               INITIALIZE                  USERBYOMEI-REC
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
      *
           .
       900-USBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名マスター読込
      *****************************************************************
       900-BYOMEI-READ-SEC         SECTION.
      *
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_byomei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-BYOMEI-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BYOMEI-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    病名マスター読込
      *****************************************************************
       900-BYOMEI-READ-N-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-BYOMEI
               MOVE    MCPDATA-REC     TO  BYOMEI-REC
           ELSE
               MOVE    1               TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
           .
       900-BYOMEI-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    同義語マスタ読込
      *****************************************************************
       900-SYNONYM-READ-SEC         SECTION.
      *
           MOVE    SYNONYM-REC         TO  MCPDATA-REC
           MOVE    "tbl_synonym"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_synonym"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYNONYM-REC
                   MOVE    ZERO            TO  FLG-SYNONYM
               ELSE
                   INITIALIZE                  SYNONYM-REC
                   MOVE    1               TO  FLG-SYNONYM
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYNONYM
               INITIALIZE                      SYNONYM-REC
           END-IF
      *
           MOVE    "tbl_synonym"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYNONYM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名入力セット読込
      *****************************************************************
       900-BYOMEIINPUTSET-READ-SEC     SECTION.
      *
           MOVE    "tbl_byomeiinputset"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_byomeiinputset"
                                           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  BYOMEIINPUTSET-REC
                   MOVE    ZERO                TO  FLG-BYOMEIINPUTSET
               ELSE
                   INITIALIZE                      BYOMEIINPUTSET-REC
                   MOVE    1                   TO  FLG-BYOMEIINPUTSET
               END-IF
           ELSE
               INITIALIZE                      BYOMEIINPUTSET-REC
               MOVE    1                   TO  FLG-BYOMEIINPUTSET
           END-IF
      *
           MOVE    "tbl_byomeiinputset"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BYOMEIINPUTSET-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ開始セット処理
      *****************************************************************
       900-JOBKANRI-HENSYU-SEC         SECTION.
      *
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-TERMID      TO  JOB-TERMID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           .
       900-JOBKANRI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       900-JOBKANRI-CHK-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-JOBEND
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               IF    ( JOB-ENDYMD      =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-JOBEND
                   GO  TO  900-JOBKANRI-CHK-EXT
               END-IF
           ELSE
               MOVE    2           TO  FLG-JOBEND
           END-IF
      *
           .
       900-JOBKANRI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    CONST-JOBID     TO  JOB-JOBID
           MOVE    CONST-JOB-SHELLID
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
