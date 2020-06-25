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
       PROGRAM-ID.             ORCGM00.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 業務選択
      *  コンポーネント名  : マスタメニュー（Ｍ００）
      *  管理者            : 
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/05/22  自動算定情報の追加
      *  01.00.02    MCC-藤原     01/05/23  患者番号構成管理の追加
      *  01.00.03    MCC-藤原     01/06/05  自動算定情報のセット中止
      *  01.00.04    MCC-藤原     01/06/29  システム日付のセット追加
      *  01.00.05    MCC-森脇     01/07/30  MID1の追加
      *  01.00.06    MCC-藤原     01/09/13  終了のときの処理修正
      *  01.01.01    MCC-多々納   01/09/20  JOINの修正      
      *  01.01.02    MCC-藤原     01/09/27  MVERの追加  
      *  01.01.03    MCC-多々納   01/10/03  自由構成の桁数をＳＰＡへ追加
      *  01.01.04    MCC-多々納   01/11/27  自由構成区分をＳＰＡへ追加
      *                                     SPAの端末ＩＤを３６桁ヘ
      *  01.02.01    MCC-藤原     01/11/21  ORCMCPSUBへ変更  
      *  01.02.02    MCC-多々納   01/12/11  拡張構成の追加
      *  01.02.03    MCC-多々納   02/02/13  デモ用項目の削除
      *  01.02.04    MCC-多々納   02/02/22  MID1の追加
      *  01.02.05    NACL-多々納  02/10/22  病床数追加
      *  01.02.06    NACL-多々納  02/12/19  入院時日付変更修正
      *  01.02.07    NACL-多々納  03/02/10  病床数（一般）追加
      *  01.02.08    NACL-藤原    03/03/01  排他制御 追加
      *  01.02.09    NACL-伊藤    03/03/10  端末ＩＤ６４バイトへ拡張
      *  01.02.10    NACL-多々納  03/06/27  職員コード追加
      *  01.02.11    NACL-多々納  03/08/25  県番号追加
      *  01.02.13    NACL-太田    04/01/13  職員情報取得失敗時の処理修正
      *  01.02.14    NACL-多々納  04/02/02  環境設定に外来時間外区分追加
      *  01.02.15    NACL-伊藤    04/02/05  M97の追加
      *  01.02.16    NACL-森脇    04/01/16  プログラム更新ボタン追加
      *  01.02.17    NACL-多々納  04/03/19  曜日設定追加
      *  01.02.18    NACL-多々納  05/05/17  病床数フラグ追加
      *  01.02.19    NACL-多々納  05/10/14  データ収集作成フラグ、
      *                                     データ収集提出方法区分追加
      *  02.07.00    NACL-竹田　  06/02/16  リダイレクタ不整表示(仮)
      *  02.07.01    NACL-伊藤　  06/02/22  MONFUNC 対応
      *  02.07.02    NACL-多々納  06/02/24  主科設定情報追加
      *  03.01.00    NACL-太田    06/07/14  ＤＢ不整合チェック追加
      *  03.03.00    NACL-多々納  06/10/24  排他制御初期設定修正
      *  03.04.01    NACL-森脇    06/12/13  ユーザーＨＴＭＬ表示
      *  03.04.02    NACL-多々納  07/01/16  患者番号構成年切替え月追加
      *  04.03.00    NACL-太田    08/08/25  マスタ更新処理状況表示
      *  04.03.00    NACL-太田    08/09/04  従での医療事務遷移をプロテクト
      *  04.04.00    NACL-多々納  09/01/26  ＳＰＡ設定共通対応
      *  04.06.01    NACL-伊藤    11/01/28  リアルタイム送信
      *  04.06.02    NACL-伊藤    11/05/19  感染症参加送信
      *  04.06.00    NACL-多々納  12/03/26  平成２４年４月改正対応
      *  04.07.00    NACL-森脇    12/07/04  ユーザーＩＤ表示
      *  04.08.00    NACL-伊藤    14/09/24  プログラム更新を別LDへ移動
      *  05.00.00    NACL-伊藤    16/09/30  DB構造バージョンチェック追加
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    スパ領域
           COPY    "M00COMMON-SPA".
      *   
           COPY    "ENUM-VALUE".
      *
      *    Ｍ００スパ領域
           COPY    "M00SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-DBKANRI         PIC 9(01).
           03  FLG-MSTKANRI        PIC 9(01).
           03  FLG-SYSBASE         PIC 9(01).
           03  FLG-ACCESSKEY       PIC 9(01).
      *
           03  FLG-INIT            PIC 9(01).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
           03  IDX-PNT             PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  IDX                   PIC 9(03).
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-WSYSYMDH          PIC X(09).
      *
           03  WRK-WSYSYMD.
               05  WRK-WSGO         PIC X(01).
               05  WRK-WSYY         PIC 9(02).
               05  WRK-WSMM         PIC 9(02).
               05  WRK-WSDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *
           03  WRK-GAI-JIKANKBN    PIC X(01).
           03  WRK-SYSYMD-YOBI     PIC X(01).
      *ver.4.7
           03  WRK-BGCOLORS        PIC X(20).
      *
           03  WRK-GNO             PIC X(02).
      *
           03  WRK-OPID            PIC X(16).
      *
           03  WRK-ERRMSG          PIC X(40).
           03  WRK-PRGNAME         PIC X(08).
           03  WRK-DBVERSION       PIC X(64).
           03  WRK-SYSBASE-CNT     PIC 9(04).
           03  WRK-JOBID           PIC 9(07).
           03  WRK-SHELLID         PIC X(08).
           03  WRK-MNTTITLE        PIC X(20).
           03  WRK-MNTYMDW         PIC X(09).
           03  WRK-MNTMSG-STYLE    PIC X(20).
           03  WRK-MNTMSG          PIC X(70).
           03  WRK-MSG             PIC X(70).
           03  WRK-MOTOPG          PIC X(08).
           03  WRK-MOTOPG2         PIC X(08).
           03  WRK-MIDMSG          PIC X(80).
           03  WRK-ENV-OFFLINE     PIC X(03).
      *
      *H24.4
       01  WRK-HZN-AREA.
           03  WRK-SPA-ERRCD           PIC X(04).
           03  FLG-CHIIKKBN-ARI        PIC 9(01).
      *
       01  SHELL-AREA.
           05  SHELL-NAME          PIC X(17) VALUE
                                   "infectiondata1.sh".
           05  SHELL-NAME1         PIC X(17) VALUE
                                   "infectionagree.sh".
           05  SHELL-NAME2         PIC X(15)  VALUE
                                   "set_contract.sh".
      *
       01  CONST-AREA.
           03  CONST-MNTJOBID      PIC 9(07)  VALUE    1.
           03  CONST-MNTSHELLID    PIC X(08)  VALUE    "MSTMNT".
           03  CONST-MLIJOBID      PIC 9(07)  VALUE    1.
           03  CONST-MLISHELLID    PIC X(08)  VALUE    "MSTMLI".
      *    感染症報告  
           03  CONST-INFJOBID      PIC 9(07)  VALUE    1.
           03  CONST-INFSHELLID    PIC X(08)  VALUE    "INFECTIO".
      *    感染症参加送信  
           03  CONST-INFAJOBID     PIC 9(07)  VALUE    1.
           03  CONST-INFASHELLID   PIC X(08)  VALUE    "INFAGREE".
      *    商用版契約情報  
           03  CONST-CONTRJOBID    PIC 9(07)  VALUE    1.
           03  CONST-CONTRSHELLID  PIC X(08)  VALUE    "CONTRACT".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    システム管理マスタ領域
      *    職員情報
           COPY  "CPSK1010.INC".
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *---- (01.00.01) LINE ADD START --------------------------------
      *    自動算定情報
           COPY  "CPSK1007.INC".
      *---- (01.00.01) LINE ADD END   --------------------------------
      *---- (01.00.02) LINE ADD START --------------------------------
      *    患者番号構成管理情報
           COPY  "CPSK1009.INC".
      *---- (01.00.02) LINE ADD END   --------------------------------
      *---- (01.02.08) LINE ADD START --------------------------------
      *    排他管理情報
           COPY  "CPSK9800.INC".
      *---- (01.02.08) LINE ADD END   --------------------------------
      *    主科設定情報
           COPY  "CPSK2009.INC".
      *----(03.04.01)--ADD-START---------------------------------------
      *    広告情報
           COPY  "CPSK1003.INC".
      *----(03.04.01)--ADD-END-----------------------------------------
      *
           COPY    "CPSK1006.INC".
      *
      *    システム基本
       01  SYSBASE-REC.
           COPY    "CPSYSBASE.INC".
      *
      *    ＤＢ管理
       01  DBKANRI-REC.
           COPY    "CPDBKANRI.INC".
      *    マスタ管理
       01  MSTKANRI-REC.
           COPY    "CPMSTKANRI.INC".
      *    ジョブ管理
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システムカタログ
           COPY    "CPDBCTG.INC".
      *
      *    アクセスキー
           COPY    "CPACCESSKEY.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    システムバージョン取得
           COPY    "CPORCSVERSION.INC".
      *    ファイルデイレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "CPORCSCOLCHK.INC".
           COPY    "CPORCSINFCHK.INC".
      *    COMMON-SPA 初期編集
           COPY    "CPORCSCOMMON.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *     
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    MCPAREA.
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "M00.INC".
           COPY    "M01.INC".
           COPY    "M01N.INC".
           COPY    "M02.INC". 
           COPY    "M95.INC".
           COPY    "MERR.INC".
           COPY    "MID1.INC".
           COPY    "MVER.INC".
           COPY    "M98.INC".
           COPY    "MUID.INC".
           COPY    "M99.INC".
           COPY    "MID2.INC".
           COPY    "MDAS.INC".
           COPY    "MLOG.INC".
           COPY    "MINF.INC".
           COPY    "MPRT.INC".
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
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-M00KYOUTU
           MOVE    SPA-SYS-1010-REC    TO  SYS-1010-REC
           MOVE    SPA-FREE        TO  SPA-M00
      *H24.4
           MOVE    SPA-ERRCD       TO  WRK-SPA-ERRCD
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
           MOVE    SYS-1010-REC    TO  SPA-SYS-1010-REC
           MOVE    SPA-M00KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-M00         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "MERR"
      *H24.4
      *        確認メッセージ表示後、業務へ
               IF      WRK-SPA-ERRCD           =   "9999"
                   MOVE    "M01"               TO  SPA-SAKIPG
                   MOVE    SPACE               TO  SPA-MOTOPG2
                   MOVE    "M00"               TO  SPA-MOTOPG
                   MOVE     "CHANGE"           TO  MCP-PUTTYPE
                   MOVE     SPA-SAKIPG         TO  MCP-WINDOW
      *
                   PERFORM 900-PUT-WINDOW
      *
                   MOVE    1                   TO  FLG-END
                   GO      TO    100-INIT-EXT
               END-IF
      *!!!
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 5101-ERRSET-SEC
                   MOVE    SPA-ERRMSG      TO  SPA-GMN-LBLMSG
                   IF      SPA-ERRCD       =   "1019"
                       MOVE    SPA-ERRMSG      TO  SPA-GMN-LBLMSG2
                       MOVE    "jma-setup 処理を実施してください。"
                                               TO  SPA-GMN-LBLMSG
                   END-IF
                   IF      SPA-ERRCD       =   "1020"
                       MOVE    SPA-ERRMSG      TO  SPA-GMN-LBLMSG2
                       MOVE    "プログラム更新処理を実施してください。"
                                               TO  SPA-GMN-LBLMSG
                   END-IF
      *            １回のみの警告表示
                   IF      SPA-ERRCD       =   "K001"
                       MOVE    1           TO  SPA-ERR-FLG
                   END-IF
               END-IF
      *
      *        感染症報告処理起動
               IF      SPA-NAI-REDL   NOT  =   1
                   IF      WRK-MOTOPG          =   SPACE
                                               OR  "M95"
                       PERFORM 800-INFECTION-SEC
                       PERFORM 800-INFECTION-AGREE-SEC
                   END-IF
                   IF      WRK-MOTOPG2         =   SPACE
                       IF    ( SPA-MW              =   SPA-PANDA ) AND
                             ( SYS-1010-KANRIAUTHFLG   =   "T" )   AND
                             ( WRK-ENV-OFFLINE     NOT =   "yes" )
                           PERFORM 820-SET-CONTRACT-SEC
                       END-IF
                   END-IF
               END-IF
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               PERFORM 510-GSRAUTH-SEC
      *
      *        IF      SPA-ERRCD       NOT =   SPACE
      *            PERFORM 510-ERRSET-SEC
      *            GO  TO  100-INIT-EXT
      *        END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "M00    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           INITIALIZE              SPA-M00-AREA
      *---- (01.00.04) LINE ADD START --------------------------------
      *    日付セット画面より
      *****IF      SPA-MOTOPG          =   "M95"  OR
      *    IF      SPA-MOTOPG          =   "MVER"
      *        MOVE    1               TO  SPA-GMN-CUR
      *        GO      TO      300-SCREEN-EXT
      *    END-IF
      *---- (01.00.04) LINE ADD END   --------------------------------
      *
           IF      LNK-KYOUTU      NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "MID1"
      *2012.10 サーベイ簡易設定改修
      *        MDASからの復帰はMID1からの復帰になりすます
               WHEN    "MDAS"
                   PERFORM 3001-MID1-SYORI-SEC
      ****             GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       3001-MID1-SYORI-SEC              SECTION.
      *
           EVALUATE    SPA-MIDCD
               WHEN    "9102"
                   IF      SPA-MID1-FLG        =   "OK"
      *                システム管理登録処理へ
                       PERFORM 810-INFECTION-003-SEC
                       PERFORM 800-INFECTION-AGREE-EX-SEC
                   END-IF
                   MOVE    1               TO  SPA-GMN-CUR
                   MOVE    SPACE           TO  SPA-MID1-FLG
                   MOVE    SPACE           TO  SPA-MIDCD
               WHEN    "9901"
                   IF      SPA-MID1-FLG        =   "OK"
                       PERFORM 820-SET-CONTRACT-SEC
                   END-IF
                   MOVE    SPACE           TO  SPA-MID1-FLG
           END-EVALUATE
           .
      *
       3001-MID1-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           DISPLAY "310-SPASET SPA-MOTOPG:" SPA-MOTOPG "."
      *
           ACCEPT  WRK-ENV-OFFLINE FROM ENVIRONMENT    "ENV_OFFLINE"
      *
           MOVE    SPACE               TO  WRK-MOTOPG
           MOVE    SPACE               TO  WRK-MOTOPG2
           IF      SPA-MOTOPG          =   "M01"  OR  "M01N"
                                       OR  "M95"  OR  "MVER"
                                       OR  "KT01" OR  "M97"
                                       OR  "M98"  OR  "XG04"
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
           END-IF
           IF     (SPA-MOTOPG(1:1)    >=   "A")  AND
                  (SPA-MOTOPG(1:1)    <=   "Z")
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG2
           END-IF
      *    初期クリア
           IF      SPA-MOTOPG(1:1)     =   "M"
                                       OR  "K"
               CONTINUE
           ELSE
               MOVE    ZERO                TO  SPA-ERR-FLG
           END-IF
      *
      *---- (01.00.04) LINE ADD START --------------------------------
      *    Ｍ０１より戻ってきた時、システム日付のセットをしない
           IF      SPA-MOTOPG          =   "M01"
                                       OR  "M01N"
                                       OR  "M95"
                                       OR  "MVER"
                                       OR  "KT01"
                                       OR  "MLOG"
                                       OR  "M98"
      *H25.9
                                       OR  "M97"
                                       OR  "MDAS"
      *H29.10
                                       OR  "MJOB"
                                       OR  "M80"
                                       OR  "XG04"
                                       OR
                 ((SPA-MOTOPG          =   "MID1") AND
                  (SPA-MIDCD           =   "9901"))
               MOVE    SPA-SYSYMDWH        TO  WRK-WSYSYMDH
               MOVE    SPA-SYSYMD          TO  WRK-SYSYMD
               MOVE    SPA-SYSYMDW         TO  WRK-WYMD
               MOVE    SPA-GAI-JIKANKBN    TO  WRK-GAI-JIKANKBN
               MOVE    SPA-SYSYMD-YOBI     TO  WRK-SYSYMD-YOBI
               MOVE    SPA-OPID            TO  WRK-OPID
      *ver.4.7
               MOVE    SPA-M00-BGCOLORS    TO  WRK-BGCOLORS
               INITIALIZE                  SPA-AREA
               MOVE    WRK-WSYSYMDH        TO  SPA-SYSYMDWH
               MOVE    WRK-SYSYMD          TO  SPA-SYSYMD
               MOVE    WRK-WYMD            TO  SPA-SYSYMDW
               MOVE    WRK-GAI-JIKANKBN    TO  SPA-GAI-JIKANKBN
               MOVE    WRK-SYSYMD-YOBI     TO  SPA-SYSYMD-YOBI
               MOVE    WRK-OPID            TO  SPA-OPID
      *ver.4.7
               MOVE    WRK-BGCOLORS        TO  SPA-M00-BGCOLORS 
               MOVE    ZERO                TO  FLG-INIT
           ELSE
               MOVE    1                   TO  FLG-INIT
      *---- (01.00.04) LINE ADD END   --------------------------------
      *
           INITIALIZE              SPA-AREA
           MOVE    MCP-USER            TO  SPA-OPID
      *    システム日付セット
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM              TO  WRK-SYSMM
           MOVE    SYS-DD              TO  WRK-SYSDD
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "11"                TO  LNK-DAY1-IRAI
           MOVE    WRK-SYSYMD          TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
           IF      STS-DAY-RC1         =   ZERO 
               MOVE    LNK-DAY1-YMD (2:7)  TO  SPA-SYSYMDW
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYSYMD          TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               IF      STS-DAY-RC1         =   ZERO 
                   MOVE    LNK-DAY2-EDTYMD1    TO  SPA-SYSYMDWH
                   MOVE    WRK-SYSYMD          TO  SPA-SYSYMD
               END-IF
           END-IF
           IF      STS-DAY-RC1     NOT =   ZERO 
      *H30.7
               INITIALIZE                  ORCSCOMMONAREA
               MOVE    "1003"              TO  SPA-ERRCD
               GO  TO  310-SPASET-EXT
           END-IF
      *    曜日指定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY3-AREA
           MOVE    "31"                TO  LNK-DAY3-IRAI
           MOVE    WRK-SYSYMD          TO  LNK-DAY3-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY3-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY3-YOBI       TO  SPA-SYSYMD-YOBI
           END-IF
      *
      *---- (01.00.04) LINE ADD START --------------------------------
           END-IF
      *
      *H30.7
           INITIALIZE                  ORCSCOMMONAREA
      *
      *---- (01.00.04) LINE ADD END   --------------------------------
      *    医療機関識別番号セット SPA-HOSPNUM
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  310-SPASET-EXT
           END-IF
      *!!
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
      *    年度切替メッセージ
           MOVE    ORCSCOMMON-LABLMSG  TO  SPA-GMN-LBLMSG2
      *ver.4.7
      *****IF      FLG-INIT            =   1
      *        環境設定の初期表示で背景色を設定する
               PERFORM 3109-M95INIT-SEC
      *****END-IF
      *ver.4.7
      *
           MOVE    '1'                 TO  SPA-NOCHK-HOSPNUM
           MOVE    "tbl_dbcatalog"     TO  MCP-TABLE
           MOVE    "redl"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_dbcatalog"     TO  MCP-TABLE
               MOVE    "redl"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    1           TO  SPA-NAI-REDL
                   MOVE    "0"         TO  SYS-1010-GSRAUTH(1:2)
                                           SYS-1010-GSRAUTH(4:)
               END-IF
           END-IF
      *
           MOVE    "tbl_dbcatalog"     TO  MCP-TABLE
           MOVE    "redl"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  SPA-NOCHK-HOSPNUM
      *
      *!!
      *以下 ＳＰＡセットは削除
      *!!
      *    医療機関ＩＤ編集
      *    MOVE    SPACE               TO  SYS-1001-REC
      *    INITIALIZE                  SYS-1001-REC
      *    MOVE    "1001"              TO  SYS-1001-KANRICD
      *    MOVE    "*"                 TO  SYS-1001-KBNCD
      *    MOVE    SPA-SYSYMD          TO  SYS-1001-STYUKYMD
      *    MOVE    SPA-SYSYMD          TO  SYS-1001-EDYUKYMD
      *    MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
      *    MOVE    SYS-1001-REC        TO  MCPDATA-REC
      *    MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *    MOVE    "key10"             TO  MCP-PATHNAME
      *    PERFORM 910-DBSELECT-SEC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "tbl_syskanri"  TO  MCP-TABLE
      *        MOVE    "key10"         TO  MCP-PATHNAME
      *        PERFORM 900-SYSKANRI-READ-SEC
      *        IF      FLG-SYSKANRI        =   ZERO
      *            MOVE    MCPDATA-REC         TO  SYS-1001-REC
      *            MOVE    SYS-1001-HOSPID     TO  SPA-HOSPID
      *            MOVE    SYS-1001-HOSPSBT    TO  SPA-HOSPSBT
      *            MOVE    SYS-1001-ROUPAYKBN  TO  SPA-ROUPAYKBN
      *************MOVE    SYS-1001-SYOHOKBN   TO  SPA-SYOHOKBN
      ************ MOVE    SYS-1001-SKYPRTFLG  TO  SPA-SKYPRTFLG
      *            病床数
      *            MOVE    SYS-1001-BEDSU      TO  SPA-BEDSU
      *            病床数（一般）
      *            IF     (SYS-1001-BEDSUIPN(1:4)  =   SPACE )  OR
      *                   (SYS-1001-BEDSUIPN(1:4)  NOT NUMERIC)
      *                MOVE    ZERO                TO  SPA-BEDSUIPN
      *            ELSE
      *                MOVE    SYS-1001-BEDSUIPN   TO  SPA-BEDSUIPN
      *            END-IF
      *
      *            データ作成フラグ
      *            IF     (SYS-1001-DATACREATEFLG  =   SPACE )  OR
      *                   (SYS-1001-DATACREATEFLG  NOT NUMERIC)
      *                MOVE    ZERO            TO  SPA-DATACREATEFLG
      *            ELSE
      *                MOVE    SYS-1001-DATACREATEFLG
      *                                        TO  SPA-DATACREATEFLG
      *            END-IF
      *            データ提出方法区分
      *            IF     (SYS-1001-DATATEISYUTUKBN  =   SPACE )  OR
      *                   (SYS-1001-DATATEISYUTUKBN  NOT NUMERIC)
      *                MOVE    ZERO            TO  SPA-DATATEISYUTUKBN
      *            ELSE
      *                MOVE    SYS-1001-DATATEISYUTUKBN
      *                                        TO  SPA-DATATEISYUTUKBN
      *            END-IF
      *
      *            県番号
      *            MOVE    SYS-1001-PREFNUM    TO  SPA-PREFNUM
      *           有床区分
      *            MOVE    1                   TO  SPA-BEDKBN
      *
*******@ CLINIC_BED_SET_TO_ZERO @
      *        ELSE
      *            MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *            MOVE    "key10"             TO  MCP-PATHNAME
      *            PERFORM 990-DBCLOSE-SEC
      *            MOVE    "1002"              TO  SPA-ERRCD
      *            GO  TO  310-SPASET-EXT
      *        END-IF
      *    ELSE
      *        MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *        MOVE    "key10"             TO  MCP-PATHNAME
      *        PERFORM 990-DBCLOSE-SEC
      *        MOVE    "1002"              TO  SPA-ERRCD
      *        GO  TO  310-SPASET-EXT
      *    END-IF
      *    MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *    MOVE    "key10"             TO  MCP-PATHNAME
      *    PERFORM 990-DBCLOSE-SEC
      *    病床数フラグ
      *    IF     (SPA-BEDKBN          =   ZERO )  OR
      *           (SPA-BEDSU           =   ZERO )
      *        MOVE    ZERO            TO  SPA-BEDFLG
      *    ELSE
      *        MOVE    1               TO  SPA-BEDFLG
      *    END-IF
      *---- (01.00.03) LINE DEL START --------------------------------
      *---- (01.00.01) LINE ADD START --------------------------------
      *
      *    自動算定情報編集
      *     INITIALIZE                  SYS-1007-REC
      *     MOVE    "1007"              TO  SYS-1007-KANRICD
      *     MOVE    "*"                 TO  SYS-1007-KBNCD
      *     MOVE    SPA-SYSYMD          TO  ORC-DBYMD
      *     MOVE    SYS-1007-REC        TO  MCPDATA-REC
      *     MOVE    "SYSKANRI-KEY"      TO  WRK-PATH
      *     PERFORM 800-SYSKANRI-READ-SEC
      *     IF      FLG-SYSKANRI        =   ZERO
      *         MOVE    SYSKANRI-REC            TO  SYS-1007-REC
      *         MOVE    SYS-1007-SANTEI (1:1)   TO  SPA-CHOKI
      **** ELSE
      ****     MOVE    "1004"              TO  SPA-ERRCD
      ****     GO  TO  310-SPASET-EXT
      *     END-IF
      *---- (01.00.01) LINE ADD END   --------------------------------
      *---- (01.00.03) LINE DEL END   --------------------------------
      *---- (01.00.02) LINE ADD START --------------------------------
      *
      *    患者番号構成管理情報編集
      *    MOVE    SPACE               TO  SYS-1009-REC
      *    INITIALIZE                  SYS-1009-REC
      *    MOVE    "1009"              TO  SYS-1009-KANRICD
      *    MOVE    "*"                 TO  SYS-1009-KBNCD
      *    MOVE    SPA-SYSYMD          TO  SYS-1009-STYUKYMD
      *    MOVE    SPA-SYSYMD          TO  SYS-1009-EDYUKYMD
      *    MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
      *    MOVE    SYS-1009-REC        TO  MCPDATA-REC
      *    MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *    MOVE    "key10"             TO  MCP-PATHNAME
      *    PERFORM 910-DBSELECT-SEC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "tbl_syskanri"  TO  MCP-TABLE
      *        MOVE    "key10"         TO  MCP-PATHNAME
      *        PERFORM 900-SYSKANRI-READ-SEC
      *        IF      FLG-SYSKANRI        =   ZERO
      *            MOVE    MCPDATA-REC     TO  SYS-1009-REC
      *            MOVE    SYS-1009-PTNUMKSIKBN
      *                                TO  SPA-1009-PTNUMKSIKBN
      *            MOVE    SYS-1009-JIYKSIKBN
      *                                TO  SPA-1009-JIYKSIKBN
      *            MOVE    SYS-1009-JIYKSIKETA
      *                                TO  SPA-1009-JIYKSIKETA
      *            MOVE    SYS-1009-HJNKSIKBN
      *                                TO  SPA-1009-HJNKSIKBN
      *            MOVE    SYS-1009-HJNKSINENKBN
      *                                TO  SPA-1009-HJNKSINENKBN
      *            MOVE    SYS-1009-HJNKSIRENNUMKBN
      *                                TO  SPA-1009-HJNKSIRENNUMKBN
      *            MOVE    SYS-1009-HJNKSIRENNUMKETA
      *                                TO  SPA-1009-HJNKSIRENNUMKETA
      *拡張構成区分
      *            MOVE    SYS-1009-KKCKSIKBN
      *                                TO  SPA-1009-KKCKSIKBN
      *            MOVE    SYS-1009-KKCKSIMAEKETA
      *                                TO  SPA-1009-KKCKSIMAEKETA
      *            MOVE    SYS-1009-KKCKSIRENNUMKETA
      *                                TO  SPA-1009-KKCKSIRENNUMKETA
      *            MOVE    SYS-1009-KKCKSIATOKETA
      *                                TO  SPA-1009-KKCKSIATOKETA
      *        ELSE
      *            MOVE    "tbl_syskanri"  TO  MCP-TABLE
      *            MOVE    "key10"         TO  MCP-PATHNAME
      *            PERFORM 990-DBCLOSE-SEC
      *            MOVE    "1005"          TO  SPA-ERRCD
      *            GO  TO  310-SPASET-EXT
      *        END-IF
      *    ELSE
      *        MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *        MOVE    "key10"             TO  MCP-PATHNAME
      *        PERFORM 990-DBCLOSE-SEC
      *        MOVE    "1005"              TO  SPA-ERRCD
      *        GO  TO  310-SPASET-EXT
      *    END-IF
      *    MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *    MOVE    "key10"             TO  MCP-PATHNAME
      *    PERFORM 990-DBCLOSE-SEC
      *
      *    患者番号構成の年切替えチェック
      *    IF      SYS-1009-NENTUKI        =   SPACE
      *        MOVE    "01"                TO  SYS-1009-NENTUKI
      *    END-IF
      *    IF     (SYS-1009-PTNUMKSIKBN    =   "2" )  AND
      *           (SYS-1009-HJNKSIKBN      =   "1"
      *                                    OR  "2" )  AND
      *           (SYS-1009-NEN        NOT =   SPA-SYSYMD (1:4)) AND
      *           (SYS-1009-NENTUKI        <=  SPA-SYSYMD (5:2))
      *        STRING  "【患者番号構成の年切替え月になりました。"
      *                "システム管理情報を更新して下さい。】"
      *                                    DELIMITED  BY  SIZE
      *                                    INTO    SPA-GMN-LBLMSG2
      *        END-STRING
      *    ELSE
      *        MOVE    SPACE               TO  SPA-GMN-LBLMSG2
      *    END-IF
      *    DISPLAY "310-SPASET-SEC SPA-1009-PTNUMKSIKBN = "
      *                                SPA-1009-PTNUMKSIKBN
      *---- (01.00.02) LINE ADD END   --------------------------------
      *---- (01.02.08) LINE ADD START --------------------------------
      *    排他管理情報編集
      *    MOVE    SPACE               TO  SYS-9800-REC
      *    INITIALIZE                  SYS-9800-REC
      *    MOVE    "9800"              TO  SYS-9800-KANRICD
      *    MOVE    "*"                 TO  SYS-9800-KBNCD
      *    MOVE    SPA-SYSYMD          TO  SYS-9800-STYUKYMD
      *    MOVE    SPA-SYSYMD          TO  SYS-9800-EDYUKYMD
      *    MOVE    SPA-HOSPNUM         TO  SYS-9800-HOSPNUM
      *    MOVE    SYS-9800-REC        TO  MCPDATA-REC
      *    MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *    MOVE    "key10"             TO  MCP-PATHNAME
      *    PERFORM 910-DBSELECT-SEC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "tbl_syskanri"  TO  MCP-TABLE
      *        MOVE    "key10"         TO  MCP-PATHNAME
      *        PERFORM 900-SYSKANRI-READ-SEC
      *        IF      FLG-SYSKANRI        =   ZERO
      *            MOVE    MCPDATA-REC     TO  SYS-9800-REC
      *            MOVE    SYS-9800-HAITA  TO  SPA-HAITAFLG
      *        ELSE
      *            MOVE    1               TO  SPA-HAITAFLG      
      *        END-IF
      *    ELSE
      *        MOVE    1               TO  SPA-HAITAFLG      
      *    END-IF
      *    MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *    MOVE    "key10"             TO  MCP-PATHNAME
      *    PERFORM 990-DBCLOSE-SEC
      *---- (01.02.08) LINE ADD END   --------------------------------
      *
      *    端末ＩＤ
      *    MOVE    MCP-TERM            TO  SPA-TERMID
      **** PERFORM VARYING IDX FROM    32  BY  -1
      *    PERFORM VARYING IDX FROM    64  BY  -1
      *            UNTIL   IDX <       1
      *        IF      SPA-TERMID (IDX:1)  NOT =   SPACE
      *            MOVE    1                   TO  IDX 
      *        ELSE  
      *            MOVE    "Z"                 TO  SPA-TERMID (IDX:1)
      *        END-IF
      *    END-PERFORM 
      *    DISPLAY "310-SPASET-SEC SPA-TERMID = " SPA-TERMID
      *
      *    オペレータＩＤ
      *    MOVE    MCP-USER            TO  SPA-OPID
      *    DISPLAY "310-SPASET-SEC SPA-OPID = " SPA-OPID
      *
      *    職員情報
      *    MOVE    SPACE               TO  SYS-1010-REC
      *    INITIALIZE                  SYS-1010-REC
      *    MOVE    "1010"              TO  SYS-1010-KANRICD
      *    STRING  SPA-OPID            DELIMITED  BY  SPACE
      *            "%"                 DELIMITED  BY  SIZE
      *                                INTO    SYS-1010-TBL
      *    END-STRING
      *    MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
      *    MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
      *    MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *    MOVE    SYS-1010-REC        TO  MCPDATA-REC
      *    MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *    MOVE    "key3"              TO  MCP-PATHNAME
      *    PERFORM 910-DBSELECT-SEC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "tbl_syskanri"  TO  MCP-TABLE
      *        MOVE    "key3"          TO  MCP-PATHNAME
      *        PERFORM 900-SYSKANRI-READ-SEC
      *        MOVE    ZERO                TO  FLG-OK
      *        PERFORM UNTIL    FLG-SYSKANRI   =   1
      *            MOVE    MCPDATA-REC         TO  SYS-1010-REC
      *            IF      SPA-OPID            =   SYS-1010-USERID
      *                MOVE    1               TO  FLG-OK
      *                MOVE    1               TO  FLG-SYSKANRI
      *            ELSE
      *               MOVE    "tbl_syskanri"  TO  MCP-TABLE
      *               MOVE    "key3"          TO  MCP-PATHNAME
      *               PERFORM 900-SYSKANRI-READ-SEC
      *           END-IF
      *        END-PERFORM
      *        IF      FLG-OK              =   1
      *            MOVE    MCPDATA-REC         TO  SYS-1010-REC
      *
      *2001.9.27 強制プロテクト（ＰＧ未作成）
      *----(01.02.16)--UPD-START---
      *         MOVE    "0"             TO  SYS-1010-GSRAUTH(3:1)
      *            MOVE    "0"          TO  SYS-1010-GSRAUTH(4:1)
      *                                    SYS-1010-GSRAUTH(18:1)
      *                                    SYS-1010-GSRAUTH(26:1)
      *                                    SYS-1010-GSRAUTH(32:1)
      *                                    SYS-1010-GSRAUTH(41:1)
      *                                    SYS-1010-GSRAUTH(46:1)
      *                                    SYS-1010-GSRAUTH(47:1)
      *            IF ( MCP-DBSTATUS    =   "REDL" )
      *                MOVE    "0"      TO SYS-1010-GSRAUTH(1:1)
      *            END-IF
      *----(01.02.16)--UPD-END-----
      *2001.9.27 強制プロテクト（ＰＧ未作成）END
      *********MOVE    SYS-1010-REC        TO  SPA-WORK
      *---- (01.02.10) LINE ADD START --------------------------------
      *        職員コード設定
      *            MOVE    SYS-1010-KBNCD(1:5)     TO  SPA-STAFFCD
      *---- (01.02.10) LINE ADD END   --------------------------------
      *        ELSE
      *            MOVE    "1001"          TO  SPA-ERRCD
      *        END-IF
      *    ELSE
      *        MOVE    "1001"              TO  SPA-ERRCD
      *    END-IF
      *    MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *    MOVE    "key3"              TO  MCP-PATHNAME
      *    PERFORM 990-DBCLOSE-SEC
      *    スーパーバイザー？
      *    IF    ( SPA-SUPERVISOR          =   "F" )
      *    AND   ( SYS-1010-KANRIAUTHFLG   =   "T" )
      *        MOVE    ZERO                TO  FLG-SYSBASE
      *        MOVE    ZERO                TO  WRK-SYSBASE-CNT
      *        INITIALIZE                      SYSBASE-REC
      *        MOVE    SYSBASE-REC         TO  MCPDATA-REC
      *        MOVE    "tbl_sysbase"       TO  MCP-TABLE 
      *        MOVE    "all"               TO  MCP-PATHNAME 
      *        PERFORM 910-DBSELECT-SEC
      *        IF      MCP-RC              =   ZERO
      *            PERFORM UNTIL   FLG-SYSBASE =   1
      *                MOVE    "tbl_sysbase"   TO  MCP-TABLE 
      *                MOVE    "all"           TO  MCP-PATHNAME 
      *                PERFORM 910-DBFETCH-SEC
      *                IF      MCP-RC          =   ZERO
      *                    ADD     1           TO  WRK-SYSBASE-CNT
      *                ELSE
      *                    MOVE    1           TO  FLG-SYSBASE
      *                END-IF
      *            END-PERFORM
      *        END-IF
      *        MOVE    "tbl_sysbase"       TO  MCP-TABLE 
      *        MOVE    "all"               TO  MCP-PATHNAME 
      *        PERFORM 990-DBCLOSE-SEC
      *        IF      WRK-SYSBASE-CNT     =   1
      *            MOVE    "T"             TO  SPA-SUPERVISOR
      *        END-IF
      *    END-IF
      *
      *    主科設定情報
      *    INITIALIZE                      SPA-SYUKA-INFO
      *    MOVE    SPACE               TO  SYS-2009-REC
      *    INITIALIZE                  SYS-2009-REC
      *    MOVE    "2009"              TO  SYS-2009-KANRICD
      *    MOVE    "*"                 TO  SYS-2009-KBNCD
      *    MOVE    SPA-SYSYMD          TO  SYS-2009-STYUKYMD
      *    MOVE    SPA-SYSYMD          TO  SYS-2009-EDYUKYMD
      *    MOVE    SPA-HOSPNUM         TO  SYS-2009-HOSPNUM
      *    MOVE    SYS-2009-REC        TO  MCPDATA-REC
      *    MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *    MOVE    "key10"             TO  MCP-PATHNAME
      *    PERFORM 910-DBSELECT-SEC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "tbl_syskanri"  TO  MCP-TABLE
      *        MOVE    "key10"         TO  MCP-PATHNAME
      *        PERFORM 900-SYSKANRI-READ-SEC
      *        IF      FLG-SYSKANRI        =   ZERO
      *            MOVE    MCPDATA-REC         TO  SYS-2009-REC
      *            MOVE    SYS-2009-SYUKA-KBN  TO  SPA-SYUKA-KBN
      *            MOVE    SYS-2009-SYUKA-NYUIN
      *                                        TO  SPA-SYUKA-NYUIN
      *            MOVE    SYS-2009-SYUKA-GAIRAI
      *                                        TO  SPA-SYUKA-GAIRAI
      *            MOVE    SYS-2009-SYUKA-MODE TO  SPA-SYUKA-MODE
      *            MOVE    SYS-2009-SYUKA-SELECT
      *                                        TO  SPA-SYUKA-SELECT
      *        ELSE
      *            MOVE    "0"                 TO  SPA-SYUKA-KBN
      *        END-IF
      *    ELSE
      *        MOVE    "0"                 TO  SPA-SYUKA-KBN
      *    END-IF
      *    MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *    MOVE    "key10"             TO  MCP-PATHNAME
      *!!  PERFORM 990-DBCLOSE-SEC
      *
      *
           MOVE    "black"             TO  SPA-GMN-MNTMSG-STYLE
           MOVE    "black"             TO  SPA-GMN-MLIMSG-STYLE
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    CONST-MNTJOBID      TO  WRK-JOBID
               MOVE    CONST-MNTSHELLID    TO  WRK-SHELLID
               PERFORM 3101-MNTUPD-EDIT-SEC
               MOVE    WRK-MNTYMDW         TO  SPA-GMN-MNTYMD
               MOVE    WRK-MNTMSG-STYLE    TO  SPA-GMN-MNTMSG-STYLE
               MOVE    WRK-MNTMSG          TO  SPA-GMN-MNTMSG
      *
               MOVE    CONST-MLIJOBID      TO  WRK-JOBID
               MOVE    CONST-MLISHELLID    TO  WRK-SHELLID
               PERFORM 3101-MNTUPD-EDIT-SEC
               MOVE    WRK-MNTYMDW         TO  SPA-GMN-MLIYMD
               MOVE    WRK-MNTMSG-STYLE    TO  SPA-GMN-MLIMSG-STYLE
               MOVE    WRK-MNTMSG          TO  SPA-GMN-MLIMSG
           END-IF
      *
      *    点数マスタ更新判定(2006/7/27分更新）
           PERFORM 3101-MSTKANRI-SET-SEC
      *
      *    定点調査 チェック
           PERFORM 3102-TEITEN-CHK-SEC
      *
      *     リダイレクタ不整チェック追加
           IF  MCP-DBSTATUS                =   "DISC"
               MOVE    "1006"              TO  SPA-ERRCD
           END-IF
           IF  MCP-DBSTATUS                =   "WAIT"
               MOVE    "1007"              TO  SPA-ERRCD
           END-IF
           IF  MCP-DBSTATUS                =   "RERR"
               MOVE    "1008"              TO  SPA-ERRCD
           END-IF
      *    IF  MCP-DBSTATUS                =   "NORE"
      *        MOVE    "1009"              TO  SPA-ERRCD
      *    END-IF
      *
           MOVE    ZERO                TO  FLG-DBKANRI
           INITIALIZE                      DBKANRI-REC
           MOVE    "ORCADB00"          TO  DBKANRI-KANRICD
           MOVE    DBKANRI-REC         TO  MCPDATA-REC
           MOVE    "tbl_dbkanri"       TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_dbkanri"       TO  MCP-TABLE 
               MOVE    "key"               TO  MCP-PATHNAME 
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  DBKANRI-REC
               ELSE
                   MOVE    1           TO  FLG-DBKANRI
               END-IF
           ELSE
                   MOVE    1           TO  FLG-DBKANRI
           END-IF
      *
           MOVE    "tbl_dbkanri"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 990-DBCLOSE-SEC
      *
           IF    ( FLG-DBKANRI         =   ZERO )
               MOVE    SPACE           TO  WRK-DBVERSION
               MOVE    FUNCTION TRIM (DBKANRI-VERSION)
                                       TO  WRK-DBVERSION
               IF    ( DBKANRI-DBSVERSION1 NOT =   DBKANRI-DBSVERSION2 )
                   MOVE    "1010"      TO  SPA-ERRCD
               ELSE
      *
                   CALL    "ORCSVERSION"       USING   SVERSION-AREA
                   IF    ( SVERSION-VERSION    NOT =   SPACE )
                       IF    ( WRK-DBVERSION
                                       NOT =   SVERSION-VERSION)
                           MOVE    "1012"      TO  SPA-ERRCD
                       END-IF
                   END-IF
                   IF    ( SPA-ERRCD                   =   SPACE ) AND
                         ( SVERSION-DBVERSION-1    NOT =   SPACE )
                       PERFORM 3101-DBVERSION-CHK-SEC
                   END-IF
               END-IF
           ELSE
                   MOVE    "1011"      TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        マスタデータ移行処理チェックの結果取得
      *        正常:SPA-ERRCD=SPACE 異常:SPA-ERRCD="1022"
               CALL    "ORCSMSTCONVCHECK"  USING   SPA-AREA
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        プラグイン起動時設定チェックの結果取得
      *        正常:SPA-ERRCD=SPACE 異常:SPA-ERRCD="1021"
               CALL    "ORCSPLUGINCHECK"   USING   SPA-AREA
           END-IF
      *
      *    感染症
           PERFORM 810-INFECTION-001-SEC
      *
           IF      SPA-MW              =   SPA-PANDA
               PERFORM 900-ACCESSKEY-READ-SEC
               IF      FLG-ACCESSKEY       =   ZERO
                   MOVE    ACCESS-KEY-KADO-LEVEL
                                           TO  SPA-NAI-KADO-LEVEL
                   MOVE    ACCESS-KEY-CONTRACT-COND
                                           TO  SPA-NAI-CONTRACT-COND
                   MOVE    ACCESS-KEY-CONTRACT-YMD
                                           TO  SPA-NAI-CONTRACT-YMD
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  SPA-MID1-FLG
           MOVE    SPACE           TO  SPA-MIDCD
           .
       310-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    データベース整合性判定処理
      *****************************************************************
       3101-DBVERSION-CHK-SEC      SECTION.
      *
           IF    ( DBKANRI-DBSVERSION1 =   SVERSION-DBVERSION-2 )
               DISPLAY "DBVERSION-CHK P1"
               GO  TO  3101-DBVERSION-CHK-EXT
           END-IF
      *
           IF   (( DBKANRI-DBSVERSION1 =   SVERSION-DBVERSION-1 )  OR
                 ( DBKANRI-DBSVERSION1 =   SVERSION-DBVERSION-3 )) AND
                 ( SVERSION-DBVERSION-2    =   SPACE )
               DISPLAY "DBVERSION-CHK P2"
               GO  TO  3101-DBVERSION-CHK-EXT
           END-IF
      *
           IF    ( DBKANRI-DBSVERSION1 <   SVERSION-DBVERSION-1 )
               MOVE    "1019"              TO  SPA-ERRCD
               MOVE    "0"                 TO  SYS-1010-GSRAUTH
               DISPLAY "DBVERSION-CHK P3"
               GO  TO  3101-DBVERSION-CHK-EXT
           END-IF
      *
           MOVE    "1020"              TO  SPA-ERRCD
           MOVE    "0"                 TO  SYS-1010-GSRAUTH(1:2)
                                           SYS-1010-GSRAUTH(4:)
               DISPLAY "DBVERSION-CHK P4"
           .
       3101-DBVERSION-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ更新判定処理
      *****************************************************************
       3101-MSTKANRI-SET-SEC               SECTION.
      *
           INITIALIZE                      MSTKANRI-REC
           MOVE    SPA-HOSPNUM         TO  MSTKANRI-HOSPNUM
           MOVE    MSTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_mstkanri"      TO  MCP-TABLE 
           MOVE    "all"               TO  MCP-PATHNAME 
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_mstkanri"  TO  MCP-TABLE 
               MOVE    "all"           TO  MCP-PATHNAME 
               PERFORM 900-MSTKANRI-READ-SEC
           ELSE
               INITIALIZE                  MSTKANRI-REC
               MOVE    1               TO  FLG-MSTKANRI
           END-IF
           MOVE    ZERO                TO  SPA-DBUPDKBN
           PERFORM UNTIL   FLG-MSTKANRI    =   1
               IF      MSTKANRI-KANRICD    =   "ORCADB00"
                   IF      MSTKANRI-DBRVERSION1 >=
                                           "R-020800-2-20060727-1"
                       MOVE    1               TO  SPA-DBUPDKBN
                   END-IF
                   MOVE    1                   TO  FLG-MSTKANRI
               END-IF
               IF      FLG-MSTKANRI        =   ZERO
                   MOVE    "tbl_mstkanri"  TO  MCP-TABLE 
                   MOVE    "all"           TO  MCP-PATHNAME 
                   PERFORM 900-MSTKANRI-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_mstkanri"      TO  MCP-TABLE 
           MOVE    "all"               TO  MCP-PATHNAME 
           PERFORM 990-DBCLOSE-SEC
           .
       3101-MSTKANRI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    マスタ更新情報編集処理
      *****************************************************************
       3101-MNTUPD-EDIT-SEC            SECTION.
      *
           MOVE    SPACE           TO  WRK-MNTYMDW
                                       WRK-MNTMSG-STYLE
                                       WRK-MNTMSG
      *
      *    ＪＯＢ管理 
           INITIALIZE                  ORCSJOBKANRIAREA
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-JOBID       TO  JOB-JOBID
           MOVE    WRK-SHELLID     TO  JOB-SHELLID
      *
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           IF    ( SJOBKANRI-RETURN    =   ZERO )
      *
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    JOB-STARTYMD        TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY2-AREA
               IF    ( STS-DAY-RC1         =   ZERO )
                   MOVE    LNK-DAY2-EDTYMD1
                                           TO  WRK-MNTYMDW
               END-IF
      *
               IF    ( JOB-ENDTIME NOT =   SPACE )
                AND  ( JOB-ENDYMD  NOT =   SPACE )
                   EVALUATE    JOB-ERRCD
                   WHEN    "00"
                   WHEN    "0000"
                       IF    ( JOB-UPDCNT      >   ZERO )
                           MOVE    "マスタ更新結果に問題があります。"
                                           TO  WRK-MNTMSG
                           MOVE    "blue"  TO  WRK-MNTMSG-STYLE
                       ELSE
                           MOVE    "マスタ更新は正常に終了しました。"
                                           TO  WRK-MNTMSG
                           MOVE    "blue"  TO  WRK-MNTMSG-STYLE
                       END-IF
                   WHEN    "01"
                   WHEN    "8001"
                       MOVE    "更新データはありませんでした。"
                                           TO  WRK-MNTMSG
                       MOVE    "blue"      TO  WRK-MNTMSG-STYLE
                   WHEN    "98"
                   WHEN    "8002"
                       MOVE    "サーバへ接続できませんでした。"
                                           TO  WRK-MNTMSG
                       MOVE    "blue"      TO  WRK-MNTMSG-STYLE
                   WHEN    "8888"
                       MOVE    "マスタ更新は中止されました"
                                           TO  WRK-MNTMSG
                       MOVE    "blue"      TO  WRK-MNTMSG-STYLE
                   WHEN    OTHER
                       MOVE    "マスタ更新は異常終了しました。"
                                           TO  WRK-MNTMSG
                       MOVE    "red"       TO  WRK-MNTMSG-STYLE
                   END-EVALUATE
                   IF    ( JOB-UPDCNT      >   ZERO )
                       MOVE    WRK-MNTMSG  TO  WRK-MSG
                       MOVE    SPACE       TO  WRK-MNTMSG
                       STRING  WRK-MSG         DELIMITED   BY  SPACE
                               "【確認リストが作成されました】"
                                               DELIMITED   BY  SIZE
                       INTO    WRK-MNTMSG
                       END-STRING
                   END-IF
               ELSE
                   MOVE    "マスタ更新中です。"
                                           TO  WRK-MNTMSG
                   MOVE    "blue"          TO  WRK-MNTMSG-STYLE
               END-IF
      *
           END-IF
      *
           .
       3101-MNTUPD-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    定点調査研究事業 収集データ・作成送信チェック処理
      *****************************************************************
       3102-TEITEN-CHK-SEC             SECTION.
      *
           INITIALIZE                  ORCSCOLCHKAREA
           CALL    "ORCSCOLCHK"        USING
                                           ORCSCOLCHKAREA
                                           SPA-AREA
      *
           IF      COLCHK-RC           =   20
                   MOVE    "1013"      TO  SPA-ERRCD
	   END-IF
           IF      COLCHK-RC           =   30
                   MOVE    "1014"      TO  SPA-ERRCD
	   END-IF
           .
       3102-TEITEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        終了
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        感染症設定メッセージ
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 810-INFECTION-002-SEC
      *        バッチログ
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 450-MJOB-SEC
      *        システム日付設定
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-M95-SEC
      *        情報連携
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 440-MIS-SEC
      *        情報画面
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 470-MVER-SEC
      *        JOBLOG画面
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 480-M98-SEC
      *!!!!!!!
      *外来まとめテスト
      *        外来まとめテスト
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 490-KT01-SEC
      *        LOGIN情報画面
               WHEN    "CLICKED"       ALSO    "B10"
                   IF    ( SPA-MW    = SPA-PANDA )
                       PERFORM 470-MLOG-SEC
                   ELSE
                       IF    ( SYS-1010-KANRIAUTHFLG   =   "T" )
                           PERFORM 470-GINBEE-PLUGIN-SEC
                       END-IF
                   END-IF
      *!!!!!!!
               WHEN    "CLICKED"       ALSO    "B12S"
                   IF    ( SPA-MW              =   SPA-PANDA ) AND
                         ( SYS-1010-KANRIAUTHFLG   =   "T" )   AND
                         ( WRK-ENV-OFFLINE     NOT =   "yes" )
                       MOVE    "9901"              TO  SPA-MIDCD
                   END-IF
      *    各業務移動へ
               WHEN    "CLICKED"       ALSO    ANY
                   PERFORM 400-GYOMU-SEL-SEC
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
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 4101-KEY-ENT-SEC
           END-EVALUATE
      *
          .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力　処理
      *****************************************************************
       4101-KEY-ENT-SEC            SECTION.
      *
           MOVE    M00-SELNUM      TO  SPA-GMN-GSEL
      *
           IF      SPA-GMN-GSEL    NOT =   ZERO
               MOVE    SPA-GMN-GSEL        TO  WRK-GNO
               PERFORM 420-GYOMU-START-SEC
           END-IF
      *
           .
       4101-KEY-ENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力　処理
      *****************************************************************
       400-GYOMU-SEL-SEC            SECTION.
      *
           MOVE   MCP-WIDGET(2:2)      TO  WRK-GNO
      *
           PERFORM 420-GYOMU-START-SEC
           .
       400-GYOMU-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務起動　処理
      *****************************************************************
       420-GYOMU-START-SEC         SECTION.
      *
      **** MOVE    SPA-WORK            TO  SYS-1010-REC
      *
      *
          MOVE    SPACE               TO  SPA-SAKIPG
          EVALUATE    WRK-GNO
      *        医事業務
               WHEN    "01"
                   IF      SYS-1010-GSRAUTH(1:1)    =  "1"
                       MOVE    "M01"           TO  SPA-SAKIPG
                   ELSE
                       MOVE    "0002"          TO  SPA-ERRCD
                   END-IF 
      *----(01.02.16)--UPD-START---
      *        プログラム更新処理
               WHEN    "03"
                 IF  ( SPA-MW              NOT =   SPA-PANDA )
                       MOVE    "0001"          TO  SPA-ERRCD
                 ELSE
                   IF    ( SPA-SUPERVISOR          =   "T" )
                   AND   ( SYS-1010-KANRIAUTHFLG   =   "T" )
                       AND     SYS-1010-GSRAUTH(3:1)    =  "1"
                       MOVE    "M97"           TO  SPA-SAKIPG
                   ELSE
                       MOVE    "0002"          TO  SPA-ERRCD
                   END-IF 
                 END-IF 
      *----(01.02.16)--UPD-END-----
      *        点数改正処理
      *        WHEN    "04"
      *            IF      SYS-1010-GSRAUTH(4:1)    =  "1"
      *                MOVE    "???"           TO  SPA-SAKIPG
      *            ELSE
      *                MOVE    "0002"          TO  SPA-ERRCD
      *            END-IF
               WHEN    OTHER
                   MOVE    "0001"          TO  SPA-ERRCD
           END-EVALUATE.
      *
      *
      *H24.4
      *    H24.4.2まで施設基準の更新確認メッセージ表示
           IF      SPA-SAKIPG          =   "M01"
                ACCEPT  SYS-YMD         FROM    DATE
                COMPUTE WRK-SYSYY   =   SYS-YY      +   2000
                MOVE    SYS-MM          TO  WRK-SYSMM
                MOVE    SYS-DD          TO  WRK-SYSDD
                IF      WRK-SYSYMD      <=  "20120402"
                    PERFORM 420-SYS1006-CHK-SEC
                END-IF
            END-IF
      *H24.4
      *
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-SAKIPG      NOT =  SPACE
                   MOVE    SPACE               TO  SPA-MOTOPG2
                   MOVE    "M00"               TO  SPA-MOTOPG
                   EVALUATE    SPA-SAKIPG
                       WHEN    "M01"
                           CONTINUE
                       WHEN    OTHER
                           INITIALIZE                  LINKAREA
                           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
                   END-EVALUATE
                   MOVE    SPACE               TO  SPA-MOTOPG2
                   MOVE    "M00"               TO  SPA-MOTOPG
                   MOVE     "CHANGE"           TO  MCP-PUTTYPE
                   MOVE     SPA-SAKIPG         TO  MCP-WINDOW
      *
                   PERFORM 900-PUT-WINDOW
      *
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
      *
       420-GYOMU-START-EXT.
           EXIT.
      *
      *H24.4
      *****************************************************************
      *    施設基準の更新確認
      *****************************************************************
       420-SYS1006-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-CHIIKKBN-ARI
      *
           MOVE    SPACE               TO  SYS-1006-REC
           INITIALIZE                      SYS-1006-REC
           MOVE    "1006"              TO  SYS-1006-KANRICD
           MOVE    "2"                 TO  SYS-1006-KBNCD
           MOVE    "20120331"          TO  SYS-1006-STYUKYMD
           MOVE    "20120331"          TO  SYS-1006-EDYUKYMD
      *    システム管理検索（施設基準）
           MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1006-REC
               INITIALIZE                  SYS-1006-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
      *        地域医療貢献加算(754 - 500)
               IF      SYS-1006-SSTKIJUN (254) =   1
                   MOVE    1               TO  FLG-CHIIKKBN-ARI
               END-IF
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  SYS-1006-REC
           INITIALIZE                      SYS-1006-REC
           MOVE    "1006"              TO  SYS-1006-KANRICD
           MOVE    "20120401"          TO  SYS-1006-STYUKYMD
           MOVE    "20120401"          TO  SYS-1006-EDYUKYMD
      *    システム管理検索（施設基準）
           MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1006-REC
               INITIALIZE                  SYS-1006-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM UNTIL   FLG-SYSKANRI    =   1
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
               IF      SYS-1006-KBNCD      =   "2"
      *            地域医療貢献加算(754 - 500)
                   IF      SYS-1006-SSTKIJUN (254) =   1
                       MOVE    1               TO  FLG-CHIIKKBN-ARI
                   END-IF
               END-IF
               IF      SYS-1006-KBNCD      =   "7"
      *            時間外対応加算(3001)(3155)(3002)
                   IF     (SYS-1006-SSTKIJUN (001) =   1  )
                      OR  (SYS-1006-SSTKIJUN (155) =   1  )
                      OR  (SYS-1006-SSTKIJUN (002) =   1  )
                       MOVE    ZERO            TO  FLG-CHIIKKBN-ARI
                   END-IF
               END-IF
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-CHIIKKBN-ARI    =   1
               MOVE    "9999"          TO  SPA-ERRCD
           END-IF
           .
       420-SYS1006-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    データファイル出力指示業務
      *****************************************************************
       440-MIS-SEC                 SECTION.
      *
           MOVE    "M00"               TO  SPA-MOTOPG
           MOVE    "XG04"              TO  SPA-SAKIPG
           MOVE    SPACE               TO  SPA-MOTOPG2
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *    戻り時、画面を元へ戻す
           MOVE    SPA-M00             TO  LNK-FREE
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XG04"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       440-MIS-EXT.
           EXIT.
      *
      *****************************************************************
      *    バッチログ画面
      *****************************************************************
       450-MJOB-SEC                SECTION.
      *
           MOVE    "M00"               TO  SPA-MOTOPG
           MOVE    "MJOB"              TO  SPA-SAKIPG
           MOVE    SPACE               TO  SPA-MOTOPG2
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *    戻り時、画面を元へ戻す
           MOVE    SPA-M00             TO  LNK-FREE
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "MJOB   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       450-MJOB-EXT.
           EXIT.
      *
      *H24.4
      *
      *****************************************************************
      *    システム日付変更画面へ1
      *****************************************************************
       460-M95-SEC                    SECTION.
      *
           MOVE    SPA-M00KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGM95"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-M00KYOUTU
      *
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "M00"                TO  SPA-MOTOPG
           MOVE    "M95"                TO  SPA-SAKIPG
           MOVE    "M00"               TO  SPA-M95-MOTOPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "M95"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       460-M95-EXT.
           EXIT.
      *
      *****************************************************************
      *   環境設定の初期表示
      *****************************************************************
       3109-M95INIT-SEC                    SECTION.
      *
           MOVE    SPA-M00KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGM95"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-M00KYOUTU
      *
      *ver.4.7
           MOVE    SPA-M95-BGCOLORS    TO  SPA-M00-BGCOLORS
      *
           .
       3109-M95INIT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ヴァージョン情報表示画面へ
      *****************************************************************
       470-MVER-SEC                    SECTION.
      *
      *     MOVE    SPACE                TO  MVER
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "M00"                TO  SPA-MOTOPG
           MOVE    "MVER"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "MVER"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END 
      *
           .
       470-MVER-EXT.
           EXIT.
      *
      *****************************************************************
      *    LOGIN情報表示画面へ
      *****************************************************************
       470-MLOG-SEC                    SECTION.
      *
           INITIALIZE                      MLOG
      *
           MOVE    SPA-M00KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGMLOG"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-M00KYOUTU
      *
           MOVE    "M00"                TO  SPA-MOTOPG
           MOVE    "MLOG"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "MLOG"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END 
      *
           .
       470-MLOG-EXT.
           EXIT.
      *
      *****************************************************************
      *    GINBEE環境でプラグイン設定画面へ
      *****************************************************************
       470-GINBEE-PLUGIN-SEC       SECTION.
      *
           MOVE    "M00"               TO  SPA-MOTOPG
           MOVE    "M80"               TO  SPA-SAKIPG
           MOVE    SPACE               TO  SPA-MOTOPG2
      *
      *del ok? INITIALIZE  SPA-KYOTUKEY
           INITIALIZE  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
      *del ok? MOVE    SPA-M00             TO  LNK-FREE
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       470-GINBEE-PLUGIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    JOBLOG表示画面へ
      *****************************************************************
       480-M98-SEC                     SECTION.
      *
           IF     (SPA-DATACREATEFLG   =    1)  OR
                  (SPA-DATAINFECTIONKBN    =    1)  OR
                  (SPA-DATAINFECTIONKBN    =    2)
      *       MOVE    SPACE            TO  MVER
              MOVE    "B12"            TO  MCP-WIDGET
      *
              MOVE    "M00"            TO  SPA-MOTOPG
              MOVE    "M98"            TO  SPA-SAKIPG
      *
              MOVE    "CHANGE"         TO  MCP-PUTTYPE
              MOVE    "M98"            TO  MCP-WINDOW
      *
              PERFORM 900-PUT-WINDOW
      *
              MOVE    1                    TO  FLG-END 
           END-IF
      *
           .
       480-M98-EXT.
           EXIT.
      *
      *!!!!!!!
      *****************************************************************
      *    外来まとめテスト　処理
      *****************************************************************
       490-KT01-SEC                   SECTION.
      *
           IF      SYS-1010-GSRAUTH(5:1)    NOT =  "1"
               MOVE    "0002"          TO  SPA-ERRCD
               GO  TO  490-KT01-EXT
           END-IF
      *!!
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M01"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
      *!!
           MOVE    "KT01"              TO  SPA-SAKIPG
           MOVE    "M00"               TO  SPA-MOTOPG
           INITIALIZE                  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE     "CHANGE"           TO  MCP-PUTTYPE
           MOVE     SPA-SAKIPG         TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       490-KT01-EXT.
           EXIT.
      *!!!!!!!
      *
      *****************************************************************
      *    終了　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *---- (01.00.06) LINE MOD START --------------------------------
      ***  MOVE    "L01"               TO  SPA-SAKIPG
           MOVE    SPACE               TO  SPA-SAKIPG
      *---- (01.00.06) LINE MOD END   --------------------------------
           MOVE    "M00"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
      *---- (01.00.06) LINE MOD START --------------------------------
      ***  MOVE    "L01"               TO  MCP-WINDOW
           MOVE    SPACE               TO  MCP-WINDOW
      *---- (01.00.06) LINE MOD END   --------------------------------
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           PERFORM 510-GSRAUTH-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-MIDCD       NOT =   SPACE
               PERFORM 520-KIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "M00    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE                  M00
      *
      *----(04.07.00) LINE UPD START ----------------------------------
      **** MOVE    SYS-1001-HOSPNAME   TO  M00-TITLE
      *     MOVE    ORCSCOMMON-HOSPNAME TO  M00-TITLE
           MOVE    SPACE               TO  M00-TITLE
      *    R01.11
      **** STRING  ORCSCOMMON-HOSPNAME     DELIMITED BY SPACE
           STRING  SPA-HOSPNAME            DELIMITED BY SPACE
                   "  ["                   DELIMITED BY SIZE
                   SPA-OPID                DELIMITED BY SPACE
                   "]"                     DELIMITED BY SIZE
                                     INTO  M00-TITLE
           END-STRING
      *----(04.07.00) LINE UPD END   ----------------------------------
           MOVE    SPA-GMN-GSEL        TO  M00-SELNUM
           MOVE    SPA-GMN-LBLMSG      TO  M00-LBLMSG
           MOVE    SPA-GMN-LBLMSG2     TO  M00-LBLMSG2
           MOVE    SPACE               TO  M00-HOSPID
      *
           IF      SPA-MW              =   SPA-PANDA
               PERFORM 5001-GMNHEN-SEC
           END-IF
      *
           IF    ( SPA-GMN-MNTYMD      NOT =   SPACE )
            OR   ( SPA-GMN-MNTMSG      NOT =   SPACE )
               MOVE    "標準提供マスタ"    TO  M00-MNTTITLE
               STRING  "最終更新日 "       DELIMITED   BY  SIZE
                       SPA-GMN-MNTYMD      DELIMITED   BY  SIZE
               INTO    M00-MNTYMD
               END-STRING
               MOVE    SPA-GMN-MNTMSG      TO  M00-MNTMSG
               MOVE    SPA-GMN-MNTMSG-STYLE
                                           TO  M00-MNTMSG-STYLE
           END-IF
      *
           IF    ( SPA-GMN-MLIYMD      NOT =   SPACE )
            OR   ( SPA-GMN-MLIMSG      NOT =   SPACE )
               MOVE    "ライセンスマスタ"  TO  M00-MLITITLE
               STRING  "最終更新日 "       DELIMITED   BY  SIZE
                       SPA-GMN-MLIYMD      DELIMITED   BY  SIZE
               INTO    M00-MLIYMD
               END-STRING
               MOVE    SPA-GMN-MLIMSG      TO  M00-MLIMSG
               MOVE    SPA-GMN-MLIMSG-STYLE
                                           TO  M00-MLIMSG-STYLE
           END-IF
      *
           IF    ( M00-LBLMSG          =   SPACE )
               MOVE    SPACE           TO  M00-LBLMSG-STYLE
           ELSE
               MOVE    "red"           TO  M00-LBLMSG-STYLE
           END-IF
           IF    ( M00-LBLMSG2         =   SPACE )
               MOVE    SPACE           TO  M00-LBLMSG2-STYLE
           ELSE
               MOVE    "red"           TO  M00-LBLMSG2-STYLE
           END-IF
      *
           IF    ( SPA-NAI-REDL        =   1 )
               MOVE    "blue"          TO  M00-LBLMSG3-STYLE
               MOVE    "従サーバに接続しています。"
                                       TO  M00-LBLMSG3
           END-IF
      *
           IF    ( MCP-DBSTATUS        =   "SYNC" )
               MOVE    "blue"          TO  M00-LBLMSG3-STYLE
               MOVE    "同期処理中です。"
                                       TO  M00-LBLMSG3
           END-IF
      *
           IF    ( SPA-DATACREATEFLG   =    1 )  OR
                 ( SPA-DATAINFECTIONKBN    =    1 )  OR
                 ( SPA-DATAINFECTIONKBN    =    2 )
               MOVE    "定点調査"      TO  M00-B11-LABEL
           END-IF
      *
      *ver.4.7
           MOVE    SPA-M00-BGCOLORS    TO  M00-BGCOLORS
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       5001-GMNHEN-SEC              SECTION.
      *
           MOVE    SPA-HOSPID          TO  M00-HOSPID
           EVALUATE    SPA-NAI-CONTRACT-COND
               WHEN    1
                   IF      SPA-NAI-KADO-LEVEL  =   5
                       MOVE    "教育版登録状態：手続き処理中"
                                               TO  M00-CONTRACTMSG
                   ELSE
                       MOVE    "商用版登録状態：手続き処理中"
                                               TO  M00-CONTRACTMSG
                   END-IF
                   MOVE    "red"               TO  M00-CONTRACTMSG-STYLE
               WHEN    2
                   IF      SPA-NAI-CONTRACT-YMD    NOT =   SPACE
                       INITIALIZE  STS-AREA-DAY
                       INITIALIZE  LNK-DAY2-AREA
                       MOVE    "21"                    TO  LNK-DAY2-IRAI
                       MOVE    SPA-NAI-CONTRACT-YMD    TO  LNK-DAY2-YMD
                       CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY2-AREA
                       IF    ( STS-DAY-RC1         =   ZERO )
                           IF      SPA-NAI-KADO-LEVEL  =   5
                               STRING  "教育版登録日 " DELIMITED BY SIZE
                                       LNK-DAY2-EDTYMD1
                                                       DELIMITED BY SIZE
                                   INTO    M00-CONTRACTMSG
                               END-STRING
                           ELSE
                               STRING  "商用版登録日 " DELIMITED BY SIZE
                                       LNK-DAY2-EDTYMD1
                                                       DELIMITED BY SIZE
                                   INTO    M00-CONTRACTMSG
                               END-STRING
                           END-IF
                       END-IF
                       MOVE    "red"               TO
                                                   M00-CONTRACTMSG-STYLE
                   END-IF
               WHEN    9
                   IF      SPA-NAI-KADO-LEVEL  =   5
                       MOVE    "教育版登録状態：契約停止中"
                                               TO  M00-CONTRACTMSG
                   ELSE
                       MOVE    "商用版登録状態：契約停止中"
                                               TO  M00-CONTRACTMSG
                   END-IF
                   MOVE    "red"               TO  M00-CONTRACTMSG-STYLE
               WHEN    OTHER
                   IF      SPA-NAI-KADO-LEVEL  =   5
                       MOVE    "教育版登録情報はありませんでした。"
                                               TO  M00-CONTRACTMSG
                   ELSE
                       MOVE    "商用版登録情報はありませんでした。"
                                               TO  M00-CONTRACTMSG
                   END-IF
                   MOVE    "blue"              TO  M00-CONTRACTMSG-STYLE
           END-EVALUATE
           .
       5001-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務権限非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
      *****MOVE    SPA-WORK            TO  SYS-1010-REC
      *
      *    PERFORM VARYING IDX FROM    1   BY  1
      *            UNTIL   IDX >       3
      *        MOVE    WIDGET-INSENSITIVE      TO  M00-STATE-O (IDX)
      *    END-PERFORM           
           MOVE    WIDGET-INSENSITIVE      TO  M00-G01-STATE
           MOVE    WIDGET-INSENSITIVE      TO  M00-G03-STATE
      *
           MOVE    WIDGET-INSENSITIVE      TO  M00-B04-STATE
           MOVE    WIDGET-INSENSITIVE      TO  M00-B05-STATE
           MOVE    WIDGET-INSENSITIVE      TO  M00-B06-STATE
           MOVE    WIDGET-INSENSITIVE      TO  M00-B09-STATE
           MOVE    WIDGET-INSENSITIVE      TO  M00-B10-STATE
           MOVE    WIDGET-INSENSITIVE      TO  M00-B11-STATE
      *
           MOVE    "T"                     TO  M00-G03-VISIBLE
      *
           IF      SYS-1010-GSRAUTH(1:1)   =   "1"
               MOVE    WIDGET-NORMAL       TO  M00-G01-STATE
               MOVE    WIDGET-NORMAL       TO  M00-B04-STATE
               MOVE    WIDGET-NORMAL       TO  M00-B06-STATE
               MOVE    WIDGET-NORMAL       TO  M00-B09-STATE
               MOVE    WIDGET-NORMAL       TO  M00-B10-STATE
               MOVE    WIDGET-NORMAL       TO  M00-B11-STATE
           END-IF
      *
      *----(01.02.16)--UPD-START---
           IF    ( SPA-SUPERVISOR          =   "T" )
           AND   ( SYS-1010-KANRIAUTHFLG   =   "T" )
               AND     SYS-1010-GSRAUTH(3:1)    =  "1"
               MOVE    WIDGET-NORMAL       TO  M00-G03-STATE
           END-IF
      *----(01.02.16)--UPD-END-----
      *
           IF    ( SPA-MW             NOT =   SPA-PANDA )
               MOVE    "F"                TO  M00-G03-VISIBLE
           END-IF
      *
           IF      SYS-1010-GSRAUTH(5:1)    =  "1"
               MOVE    "（まとめ）"       TO  M00-B09-LABEL
           ELSE
               MOVE    SPACE              TO  M00-B09-LABEL
           END-IF
      *
           IF    ( SPA-MW                 =   SPA-PANDA )
               MOVE    "ユーザ管理"       TO  M00-B10-LABEL
           ELSE
               MOVE    "プラグイン"       TO  M00-B10-LABEL
           END-IF
      *
           IF    ( SPA-NAI-REDL           =   1 )
               MOVE    WIDGET-INSENSITIVE TO  M00-G01-STATE
      *
               IF    ( M00-B09-LABEL  NOT =   SPACE )
                   MOVE    WIDGET-INSENSITIVE TO  M00-B09-STATE
               END-IF
      *
                   MOVE    WIDGET-INSENSITIVE TO  M00-B10-STATE
      *
               IF    ( M00-B11-LABEL  NOT =   SPACE )
                   MOVE    WIDGET-INSENSITIVE TO  M00-B11-STATE
               END-IF
           END-IF
      *
           IF    ( SPA-SUPERVISOR          =   "T" )   OR
                 ( SYS-1010-KANRIAUTHFLG   =   "T" )
               MOVE    WIDGET-NORMAL       TO  M00-B05-STATE
               MOVE    "バッチログ"        TO  M00-B05-LABEL
           END-IF
      *
      *2001.8.7 強制プロテクト（ＰＧ未作成）
      ***     MOVE    WIDGET-INSENSITIVE      TO  M00-G03-STATE
      ***     MOVE    WIDGET-INSENSITIVE      TO  M00-G04-STATE
      *
      *----(03.04.01)--ADD-START---------------------------------------
      *    広告情報編集
           MOVE    SPACE               TO  SYS-1003-REC
           INITIALIZE                  SYS-1003-REC
           MOVE    "1003"              TO  SYS-1003-KANRICD
           MOVE    "*"                 TO  SYS-1003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1003-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1003-HOSPNUM
           MOVE    SYS-1003-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1003-REC  
               END-IF  
           ELSE
               MOVE    1               TO  FLG-SYSKANRI 
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYS-1003-USERURL    TO  M00-HTML1     
           ELSE
               MOVE    SPACE               TO  M00-HTML1     
           END-IF
      *----(03.04.01)--ADD-END-----------------------------------------
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE  "SELNUM"    TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           PERFORM 5101-ERRSET-SEC
      *
           MOVE    SPA-ERRCD            TO  SPA-GMN-ERRCD  
      *
           MOVE    SPACE                TO  MERR
           MOVE    SPA-ERRCD            TO  MERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  MERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "M00"                TO  SPA-MOTOPG
           MOVE    "MERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "MERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       5101-ERRSET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラー"        TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "この業務は選択できません"
                                               TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "職員情報が取得できませんでした。処理を終"
                                               TO  SPA-ERRMSG (1:40)
                   MOVE    "了してください。"
                                               TO  SPA-ERRMSG (41:)
               WHEN    "1002"
                   MOVE    "医療機関情報が取得できませんでした。処理"
                                               TO  SPA-ERRMSG (1:40)
                   MOVE    "を終了してください。"
                                               TO  SPA-ERRMSG (41:)
               WHEN    "1003"
                   MOVE    "システム日付が取得できませんでした。処理"
                                               TO  SPA-ERRMSG (1:40)
                   MOVE    "を終了してください。"
                                               TO  SPA-ERRMSG (41:)
      *---- (01.00.01) LINE ADD START --------------------------------
               WHEN    "1004"
                   MOVE    "自動算定情報が取得できませんでした。処理"
                                               TO  SPA-ERRMSG (1:40)
                   MOVE    "を終了してください。"
                                               TO  SPA-ERRMSG (41:)
      *---- (01.00.01) LINE ADD END   --------------------------------
      *---- (01.00.02) LINE ADD START --------------------------------
               WHEN    "1005"
                   MOVE    "患者番号構成情報が取得できませんでした。"
                                               TO  SPA-ERRMSG (1:40)
                   MOVE    "処理を終了してください。"
                                               TO  SPA-ERRMSG (41:)
      *---- (01.00.02) LINE ADD END   --------------------------------
               WHEN    "1006"
                   MOVE    "主従のデータベースに不整合が発生していま"
                                               TO  SPA-ERRMSG (1:40)
                   MOVE    "す。"
                                               TO  SPA-ERRMSG (41:)
               WHEN    "1007"
                   MOVE    "リダイレクタ接続待ちです。"
                                               TO  SPA-ERRMSG (1:40)
               WHEN    "1008"
                   MOVE    "リダイレクタ接続に失敗しました。"
                                               TO  SPA-ERRMSG (1:40)
      *        WHEN    "1009"
      *            MOVE    "リダイレクタ未起動です。"
      *                                        TO  SPA-ERRMSG (1:40)
               WHEN    "1010"
                   MOVE
                      "ＤＢの構造バージョンに不整合が発生しています！！"
                                               TO  SPA-ERRMSG
               WHEN    "1011"
                   MOVE
                      "ＤＢの構造バージョンの取得に失敗しました。"
                                               TO  SPA-ERRMSG
               WHEN    "1012"
                   MOVE
                           "システムバージョンとＤＢの構造に不整合が"
                                               TO  SPA-ERRMSG (1:40)
                   MOVE    "発生しています！！"
                                               TO  SPA-ERRMSG (41:)
               WHEN    "1013"
                   MOVE    "定点調査研究事業（データ収集）用のファイ"
                                               TO  SPA-ERRMSG (1:40)
                   MOVE    "ル作成処理中です。"
                                               TO  SPA-ERRMSG (41:)
               WHEN    "1014"
                   MOVE    "定点調査研究事業（データ収集）用のファイ"
                                               TO  SPA-ERRMSG (1:40)
                   MOVE    "ルが未送信です。"
                                               TO  SPA-ERRMSG (41:)
               WHEN    "1015"
                   STRING  "グループ医療機関が不整合です。"
                           "処理を終了して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1016"
                   MOVE    "更新処理ができませんでした。"
                                               TO  SPA-ERRMSG
               WHEN    "1017"
                   STRING  "ＯＲＣＡサーベイランス調査"
                           "へは参加中です。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1018"
                   STRING  "あなたは管理者ではありませんので参加"
                           "する場合は管理者まで連絡してください。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1019"
               WHEN    "1020"
                   STRING  "プログラムのＤＢ構造バージョンと"
                           "ＤＢ管理テーブルの構造バージョンが"
                           "不整合です！！"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1021"
                   STRING  "プラグイン設定に矛盾があります。"
                           "管理者まで連絡してください。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1022"
                   STRING  "マスタデータ移行処理が未実施です。"
                           "管理者まで連絡してください。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1023"
                   STRING  "医療機関識別番号２以上のデータには"
                           "接続できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K001"
                   STRING  "警告！！"
                           "この端末は使用できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
      *H24.4
               WHEN    "9999"
                   STRING  "施設基準コードの"
                           "地域医療貢献加算が時間外対応加算２へ"
                           "変更されていません。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
           END-EVALUATE
	   .
      *
       5101-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示処理
      *****************************************************************
       520-KIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-MIDCD
               WHEN    "9102"
                   STRING  "ＯＲＣＡサーベイランス調査"
                           "へのデータ提供（参加）を"
                           "開始しますか？"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-MIDMSG
                   END-STRING
               WHEN    "9901"
                   IF      SPA-NAI-KADO-LEVEL  =   5
                       STRING  "教育版契約情報を照会します。"
                               "よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-MIDMSG
                       END-STRING
                   ELSE
                       STRING  "商用版契約情報を照会します。"
                               "よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-MIDMSG
                       END-STRING
                   END-IF
               WHEN    OTHER
                   MOVE    SPA-MIDCD       TO  WRK-MIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  MID1
           INITIALIZE                      MID1
           MOVE    SPA-MIDCD           TO  MID1-ID1CODE
           MOVE    WRK-MIDMSG          TO  MID1-ID1MSG
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "M00"               TO  SPA-MOTOPG
           MOVE    "MID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "MID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-KIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    感染症報告処理
      *****************************************************************
       800-INFECTION-SEC               SECTION.
      *
           INITIALIZE                  ORCSINFCHKAREA
           CALL    "ORCSINFCHK"        USING
                                           ORCSINFCHKAREA
                                           SPA-AREA
      *
           IF    ( INFCHK-RC       NOT =   ZERO )  AND
                 ( INFCHK-RC       NOT =   90   )
               GO  TO    800-INFECTION-EXT
	   END-IF
      *    
      *    ジョブ開始セット処理
           MOVE    "DEL"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CONST-INFJOBID      TO  JOB-JOBID
           MOVE    CONST-INFSHELLID    TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA 
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           MOVE    "JBS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CONST-INFJOBID      TO  JOB-JOBID
           MOVE    CONST-INFSHELLID    TO  JOB-SHELLID
           MOVE    SPA-TERMID          TO  JOB-TERMID
           CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA 
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    SHELL-NAME          TO  SHELLTBL-NAME
           MOVE    SPA-TERMID          TO  SHELLTBL-ARG1
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG2
           MOVE    SPA-HOSPID          TO  SHELLTBL-ARG3
           MOVE    CONST-INFJOBID      TO  SHELLTBL-ARG4
           MOVE    CONST-INFSHELLID    TO  SHELLTBL-ARG5
           MOVE    SPA-BEDFLG          TO  SHELLTBL-ARG6
      *
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "daily"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       800-INFECTION-EXT.
           EXIT.
      *
      *****************************************************************
      *    感染症参加送信処理
      *****************************************************************
       800-INFECTION-AGREE-SEC         SECTION.
      *
           INITIALIZE                  ORCSINFCHKAREA
           CALL    "ORCSINFSTATE"      USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSINFCHKAREA
      *
      **   DISPLAY "INFCHK-RC = " INFCHK-RC
           IF    ( INFCHK-RC       NOT =   ZERO )  AND
                 ( INFCHK-RC       NOT =   90   )
               GO  TO    800-INFECTION-AGREE-EXT
	   END-IF
      *
           PERFORM 800-INFECTION-AGREE-EX-SEC
           .
       800-INFECTION-AGREE-EXT.
           EXIT.
      *    
      *****************************************************************
      *    感染症参加送信処理
      *****************************************************************
       800-INFECTION-AGREE-EX-SEC      SECTION.
      *
      *    ジョブ開始セット処理
           MOVE    "DEL"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CONST-INFAJOBID     TO  JOB-JOBID
           MOVE    CONST-INFASHELLID   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA 
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           MOVE    "JBS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CONST-INFAJOBID     TO  JOB-JOBID
           MOVE    CONST-INFASHELLID   TO  JOB-SHELLID
           MOVE    SPA-TERMID          TO  JOB-TERMID
           CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA 
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    SHELL-NAME1         TO  SHELLTBL-NAME
           MOVE    SPA-TERMID          TO  SHELLTBL-ARG1
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG2
           MOVE    SPA-HOSPID          TO  SHELLTBL-ARG3
           MOVE    CONST-INFAJOBID     TO  SHELLTBL-ARG4
           MOVE    CONST-INFASHELLID   TO  SHELLTBL-ARG5
      *
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "daily"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       800-INFECTION-AGREE-EX-EXT.
           EXIT.
      *
      *****************************************************************
      *    感染症参加促進処理
      *****************************************************************
       810-INFECTION-001-SEC       SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-DATAINFECTIONKBN    =   ZERO)  AND
                  (ORCSCOMMON-LABLMSG      =   SPACE)
               STRING  "日レセから自動送信による地域マップを"
                       "日本医師会ホームページにて公開中です。"
                       "ご協力ください。"
                                           DELIMITED  BY  SIZE
                                           INTO    SPA-GMN-LBLMSG2
               END-STRING
               STRING  "Ｆ４キー（調査協力ボタン）で参加"
                       "登録をお願いします。"
                                           DELIMITED  BY  SIZE
                                           INTO    SPA-GMN-LBLMSG
               END-STRING
           END-IF
           .
       810-INFECTION-001-EXT.
           EXIT.
      *
      *****************************************************************
      *    感染症参加促進処理
      *****************************************************************
       810-INFECTION-002-SEC       SECTION.
      *
      *****IF     (SPA-SUPERVISOR          =   "T" )  AND
           IF    ((SPA-DATAINFECTIONKBN    =   ZERO)  OR
                  (SPA-DATAINFECTIONKBN    =   9   ))
               IF     (SYS-1010-KANRIAUTHFLG   =   "T" )
      *2012.10     MOVE    "9102"              TO  SPA-MIDCD
                   MOVE    "M00"               TO  SPA-MOTOPG
                   MOVE    "MDAS"              TO  SPA-SAKIPG
                   MOVE    "NEW"               TO  MCP-PUTTYPE
                   MOVE    "MDAS"              TO  MCP-WINDOW
                   PERFORM 900-PUT-WINDOW
                   MOVE    1                   TO  FLG-END
               ELSE
                   MOVE    "1018"              TO  SPA-ERRCD
               END-IF 
           ELSE
               MOVE    "1017"              TO  SPA-ERRCD
           END-IF 
           .
       810-INFECTION-002-EXT.
           EXIT.
      *
      *****************************************************************
      *    感染症参加促進処理
      *****************************************************************
       810-INFECTION-003-SEC       SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      * 
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1001-REC
                   MOVE    "1"         TO  SYS-1001-DATAINFECTIONKBN
                   MOVE    SYS-1001-REC        TO  SYSKANRI-REC
      * 
                   MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
                   MOVE    SYS-KANRICD         TO  SYSUP-KANRICD
                   MOVE    SYS-KBNCD           TO  SYSUP-KBNCD
                   MOVE    SYS-STYUKYMD        TO  SYSUP-STYUKYMD
                   MOVE    SYS-EDYUKYMD        TO  SYSUP-EDYUKYMD
      *
                   MOVE    SPA-OPID            TO  SYS-OPID
                   MOVE    SMCNDATE-YMD        TO  SYS-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
      *
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "upd1"              TO  MCP-PATHNAME
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                   MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                   CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   MOVE    1                   TO  SPA-DATAINFECTIONKBN
               ELSE
                   MOVE    "1016"                  TO  SPA-ERRCD
               END-IF 
           ELSE
               MOVE    "1016"                  TO  SPA-ERRCD
           END-IF      
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       810-INFECTION-003-EXT.
           EXIT.
      *****************************************************************
      *    商用版契約情報設定処理
      *****************************************************************
       820-SET-CONTRACT-SEC        SECTION.
      *
           IF      ACCESS-KEY-1        =   SPACE
               GO  TO  820-SET-CONTRACT-EXT
           END-IF
      *
           IF      SPA-MIDCD       NOT =   "9901"
      *        31日以上経過
               IF      ACCESS-KEY-INQUIRE-YMD
                                       NOT =   SPACE
                   MOVE    SPACE               TO  STS-AREA-DAY
                   INITIALIZE  STS-AREA-DAY
                   INITIALIZE  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    ACCESS-KEY-INQUIRE-YMD
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE    31                  TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
      **           DISPLAY "SPA-SYSYMD [ " SPA-SYSYMD " ]"
      **           DISPLAY "after 31   [ " LNK-DAY6-KEISAN " ]"
                   IF      SPA-SYSYMD          <=  LNK-DAY6-KEISAN
      **               DISPLAY "CONTRACT NOT 31 SKIP"
                       GO  TO  820-SET-CONTRACT-EXT
                   END-IF
               END-IF
           END-IF
      *
           INITIALIZE  ORCSJOBKANRIAREA
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE  JOBKANRI-REC
           MOVE    CONST-CONTRJOBID    TO  JOB-JOBID
           MOVE    CONST-CONTRSHELLID  TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           IF      SJOBKANRI-RETURN    =   ZERO
               IF    ( JOB-ENDTIME         =   SPACE ) OR
                     ( JOB-ENDYMD          =   SPACE )
                   GO  TO  820-SET-CONTRACT-EXT
               END-IF
               MOVE    "DEL"               TO  SJOBKANRI-MODE
               INITIALIZE  JOBKANRI-REC
               MOVE    CONST-CONTRJOBID    TO  JOB-JOBID
               MOVE    CONST-CONTRSHELLID  TO  JOB-SHELLID
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
               CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
                                               JOBKANRI-REC
                                               SPA-AREA
           END-IF
           MOVE    "JBS"               TO  SJOBKANRI-MODE
           INITIALIZE  JOBKANRI-REC
           MOVE    CONST-CONTRJOBID    TO  JOB-JOBID
           MOVE    CONST-CONTRSHELLID  TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    SHELL-NAME2         TO  SHELLTBL-NAME
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-HOSPNUM
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG1
           MOVE    SPA-HOSPID          TO  SHELLTBL-ARG2
           MOVE    ACCESS-KEY-1        TO  SHELLTBL-ARG3
           MOVE    CONST-CONTRJOBID    TO  SHELLTBL-ARG4
           MOVE    CONST-CONTRSHELLID  TO  SHELLTBL-ARG5
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       820-SET-CONTRACT-EXT.
           EXIT.
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ管理読込み処理
      *****************************************************************
       900-DBKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  DBKANRI-REC
               MOVE    ZERO            TO  FLG-DBKANRI
           ELSE
               INITIALIZE                  DBKANRI-REC
               MOVE    1               TO  FLG-DBKANRI
           END-IF
      *
           .
       900-DBKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    マスタ管理読込み処理
      *****************************************************************
       900-MSTKANRI-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  MSTKANRI-REC
               MOVE    ZERO            TO  FLG-MSTKANRI
           ELSE
               INITIALIZE                  MSTKANRI-REC
               MOVE    1               TO  FLG-MSTKANRI
           END-IF
           .
       900-MSTKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    アクセスキー読込み処理
      *****************************************************************
       900-ACCESSKEY-READ-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-ACCESSKEY
           INITIALIZE  ACCESS-KEY-REC
           MOVE    SPA-HOSPNUM         TO  ACCESS-KEY-HOSPNUM
           MOVE    ACCESS-KEY-REC      TO  MCPDATA-REC
           MOVE    "tbl_access_key"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_access_key"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBFETCH"           TO  MCP-FUNC
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  ACCESS-KEY-REC
               ELSE
                   MOVE    1                   TO  FLG-ACCESSKEY
                   INITIALIZE  ACCESS-KEY-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-ACCESSKEY
               INITIALIZE  ACCESS-KEY-REC
           END-IF
           MOVE    "tbl_access_key"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME 
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-ACCESSKEY-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルFETCH処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE    "PUTWINDOW"         TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
