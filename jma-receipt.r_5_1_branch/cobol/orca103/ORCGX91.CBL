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
       PROGRAM-ID.             ORCGX91.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : チェックマスタ情報設定
      *  コンポーネント名  : チェックマスタ出力設定画面（Ｘ９１）
      *  管理者            : 
      *  04/02/27    NACL-太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-森脇    04/07/29  直前ジョブ管理チェック追加
      *  02.00.06    NACL-森脇    06/03/20  プレビュー表示
      *  03.04.01    NACL-森脇    07/02/21  チェック区分追加
      *  03.05.00    NACL-多々納  07/05/08  グループ診療対応
      *  04.08.01    NACL-森脇    16/11/29  ＣＳＶ出力対応
      *  05.00.00    NACL-森脇    17/11/29  クライアント印刷対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域 
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //CHKMERR//.
      *
      *    画面活性非活性
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    点数マスタ設定用共通パラメタ
           COPY    "X01COMMON-SPA".
      *
      *    第一画面ＳＰＡエリア
           COPY    "X01SCR-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-X91.
           03  SPA-X91-AREA.
               05  SPA-GMN-CUR             PIC 9(03).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-CHK-VAL.
                       09  SPA-GMN-CHKYKZBYO       PIC X(01).
                       09  SPA-GMN-CHKSRYBYO       PIC X(01).
                       09  SPA-GMN-CHKYKZHEI       PIC X(01).
                       09  SPA-GMN-CHKSRYHEI       PIC X(01).
                       09  SPA-GMN-CHKYKZKNKBYO    PIC X(01).
                       09  SPA-GMN-CHKSRYSNT       PIC X(01).
                       09  SPA-GMN-CHKBYOYKZ       PIC X(01).
                       09  SPA-GMN-CHKBYOSRY       PIC X(01).
                       09  SPA-GMN-CHKSRYHEIA      PIC X(01).
                       09  SPA-GMN-CHKSRYHEIB      PIC X(01).
                       09  SPA-GMN-CHKTEKBYOYKZ    PIC X(01).
                       09  SPA-GMN-CHKTEKBYOSRY    PIC X(01).
                   07  SPA-GMN-CHK-R   REDEFINES   SPA-GMN-CHK-VAL.
                       09  SPA-GMN-CHKKBN      PIC X(01)   OCCURS  12.
                   07  SPA-GMN-KJNYMD          PIC X(09).
                   07  SPA-GMN-CHKSRYRANGE     PIC X(01).
                   07  SPA-GMN-STSRYCD         PIC X(09).
                   07  SPA-GMN-STSRYNAME       PIC X(100).
                   07  SPA-GMN-EDSRYCD         PIC X(09).
                   07  SPA-GMN-SORT-G.
                       09  SPA-GMN-SORT            PIC X(01).
                       09  SPA-GMN-SORTFL          PIC X(01).
                       09  SPA-GMN-SORTMEI         PIC X(28).
                   07  SPA-GMN-CHKMORE-G.
                       09  SPA-GMN-CHKMOREYKZBYO   PIC X(01).
                       09  SPA-GMN-CHKMOREKENSA    PIC X(01).
                       09  SPA-GMN-CHKMOREBUI      PIC X(01).
                   07  SPA-GMN-MOREKIKAN-G.
                       09  SPA-GMN-MOREKIKAN       PIC X(01).
                       09  SPA-GMN-MOREKIKANFL     PIC X(01).
                       09  SPA-GMN-MOREKIKANMEI    PIC X(38).
                   07  SPA-GMN-MORESORT-G.
                       09  SPA-GMN-MORESORT        PIC X(01).
                       09  SPA-GMN-MORESORTFL      PIC X(01).
                       09  SPA-GMN-MORESORTMEI     PIC X(28).
                   07  SPA-GMN-CHKKAISU            PIC X(01).
               05  SPA-GMN-CMB-AREA.
                   07  SPA-GMN-SORTLST-G.
                       09  SPA-GMN-SORT-MAX         PIC 9(05).
                       09  SPA-GMN-SORTLST         OCCURS  5.
                           11  SPA-GMN-SORTL       PIC X(01).
                           11  SPA-GMN-SORTFLL     PIC X(01).
                           11  SPA-GMN-SORTMEIL    PIC X(28).
                   07  SPA-GMN-MOREKIKANLST-G.
                       09  SPA-GMN-MOREKIKAN-MAX       PIC 9(05).
                       09  SPA-GMN-MOREKIKANLST        OCCURS  5.
                           11  SPA-GMN-MOREKIKANL      PIC X(01).
                           11  SPA-GMN-MOREKIKANFLL    PIC X(01).
                           11  SPA-GMN-MOREKIKANMEIL   PIC X(38).
                   07  SPA-GMN-MORESORTLST-G.
                       09  SPA-GMN-MORESORT-MAX        PIC 9(05).
                       09  SPA-GMN-MORESORTLST         OCCURS  5.
                           11  SPA-GMN-MORESORTL       PIC X(01).
                           11  SPA-GMN-MORESORTFLL     PIC X(01).
                           11  SPA-GMN-MORESORTMEIL    PIC X(28).
               05  SPA-NAI-AREA.
                   07  SPA-NAI-KJNYMD                  PIC X(08).
                   07  SPA-NAI-SRYCD-EXSIT-FLG         PIC 9(01).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-KANJI               PIC 9(01).
           03  FLG-KENSAKU             PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-CHKMERR                     PIC X(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDX-CMB                 PIC 9(05).
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *      
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
           03  SYS-TIME                PIC X(06).
      *
       01  WRK-AREA.
           03  WRK-SYMD                PIC X(10).
           03  WRK-EDTYMD1             PIC X(10).
           03  WRK-MCP-WIDGET          PIC X(64).
           03  WRK-CMB-CD                  PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM         PIC X(200).
           03  WRK-MOTOPG              PIC X(08).
           03  WRK-PRTKANRI-RENNUM     PIC 9(04).
           03  WRK-SHORIERR            PIC X(200).
           03  WRK-XIDMSG              PIC X(70).
           03  WRK-PAGE                PIC ZZZZZZZZZ9.
      *
           03  WRK-STPAGE              PIC 9(05).
           03  WRK-EDPAGE              PIC 9(05).
           03  WRK-HENTIME.
               05  WRK-HH              PIC X(02).
               05  FILLER              PIC X(01)   VALUE   ".".
               05  WRK-MM              PIC X(02).
               05  FILLER              PIC X(01)   VALUE   ".".
               05  WRK-SS              PIC X(02).
           03  WRK-CHKKBN              PIC X(01).
           03  WRK-CHKMORE             PIC X(01).
           03  WRK-MORERANGE-G.
               05  WRK-MOREKENSA       PIC X(01).
               05  WRK-MOREBUI         PIC X(01).
           03  WRK-SRTKBN              PIC X(01).
           03  WRK-INPUTCD             PIC X(09).
           03  WRK-CD-MCNT             PIC 9(02).
           03  WRK-STR-IN              PIC X(500).
           03  WRK-STR-OUT             PIC X(500).
           03  WRK-STR-LEN             PIC 9(8).
           03  WRK-KIKAN               PIC 9(05).
           03  WRK-CHKMERR         PIC X(200).
           03  WRK-PRT-PAGE        PIC 9(10).
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".
      *    プレビュー用情報
           COPY    "CPXC01LNK.INC".
      *
      *    SHELL用
           COPY    "CPSHELLTBL.INC".
      *
       01  CONST-AREA.
           03  CONST-CHKKBN-MAX        PIC 9(02)   VALUE   12.
           03  CONST-CHKKBN-VAL.
               05                      PIC X(01)   VALUE   "1".
               05                      PIC X(01)   VALUE   "2".
               05                      PIC X(01)   VALUE   "4".
               05                      PIC X(01)   VALUE   "5".
               05                      PIC X(01)   VALUE   "6".
               05                      PIC X(01)   VALUE   "7".
               05                      PIC X(01)   VALUE   "8".
               05                      PIC X(01)   VALUE   "9".
               05                      PIC X(01)   VALUE   "A".
               05                      PIC X(01)   VALUE   "B".
               05                      PIC X(01)   VALUE   "Y".
               05                      PIC X(01)   VALUE   "Z".
           03  CONST-CHKKBN-R       REDEFINES   CONST-CHKKBN-VAL.
               05  CONST-CHKKBN     PIC X(01)   OCCURS  12.
      *    システム管理ＤＢ（３００３）のキー値  
           03  CONST-3003-KBNCD     PIC X(08)   VALUE   "103".
      *    印刷管理ＤＢのキー値  
           03  CONST-PRTKANRI-TBL-KEY
                                       PIC X(08)   VALUE   "CHKMST".
           03  CONST-PRTKANRI-TBL-GROUP
                                       PIC X(14)   VALUE   SPACE.
      *    ジョブ管理ＤＢのキー値  
           03  CONST-JOB-SHELLID    PIC X(08)   VALUE   "CHKMST".
           03  CONST-JOB-JOBID      PIC 9(07)   VALUE   ZERO.
      *
           COPY    "CPSHELLTBL.INC"    REPLACING  //SHELLTBL//
                                       BY         //PRMTBL//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1031.INC".
      *
           COPY    "CPSK3003.INC".
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
      *    チェック
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *
      *    チェック
       01  CHKSND-REC.
           COPY    "CPCHKSND.INC".
      *
      *    チェック
       01  CHKTRD-REC.
           COPY    "CPCHKTRD.INC".
      *
      *----(03.04.01)--ADD-START---
      *    チェック
       01  CHK005-REC.
           COPY    "CPCHK005.INC".
      *----(03.04.01)--ADD-END-----
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    一時チェックマスタ編集サブ
           COPY    "CPORCSS013.INC".
           COPY    "CPORCSS013.INC"    REPLACING    //SS013-//
                                       BY           //TSS013-//.
      *
      *----(05.00.00)--ADD-START---
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *----(05.00.00)--ADD-END-----
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "X01.INC".
           COPY    "X02.INC".
           COPY    "X03.INC".
           COPY    "X04.INC".
           COPY    "X05.INC".
           COPY    "X06.INC".
           COPY    "X07.INC".
           COPY    "X08.INC".
           COPY    "X91.INC".
           COPY    "X97.INC".
           COPY    "X98.INC".
           COPY    "X99.INC".
           COPY    "X100.INC".
           COPY    "XERR.INC".
           COPY    "XID1.INC".
           COPY    "XID2.INC".
      *
       PROCEDURE                                 DIVISION    USING
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-WORK        TO  SPA-X01KYOUTU
           MOVE    SPA-FREE        TO  SPA-X01FREE
           MOVE    SPA-X01SONOTA   TO  SPA-X91
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN     "PUTG"         ALSO    "CLICKED"
               PERFORM 200-GMNSENI
      *    入力
           WHEN      OTHER
               PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-X01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-X91         TO  SPA-X01SONOTA
           MOVE    SPA-X01FREE     TO  SPA-FREE
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
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "XERR" )
               MOVE    SPACE       TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END     =   1 )
                   GO  TO  100-INIT-EXT
               END-IF
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
      *
      *
      ***  MOVE    "NEW"           TO  MCP-PUTTYPE.
           MOVE    SPACE           TO  MCP-PUTTYPE.
           MOVE    "X91"           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
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
      *----(02.00.06)--UPD-START---
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *         MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
      *        元の画面へ戻す
               IF    (  SPA-MOTOPG     =   "XA01" OR "XB01" OR "XC01" )
      *----(04.08.01)--ADD-START---
                   OR ( SPA-MOTOPG     =   "XG01" )
      *----(04.08.01)--ADD-END-----
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-X91
                   MOVE    SPA-WORK        TO  SPA-X01KYOUTU
                   MOVE    SPACE           TO  SPA-XID1-FLG
                   GO  TO  300-SCREEN-EXT
               END-IF
      *
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
      *
      *         MOVE    SPACE               TO  SPA-XID1-FLG
      *         MOVE    SPACE               TO  SPA-XIDCD
           END-IF
      *----(02.00.06)--UPD-END-----
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "X97"
      *----(02.00.06)--ADD-START-----
               PERFORM 340-X97-SET-SEC
      *----(02.00.06)--ADD-END-------
               GO  TO      300-SCREEN-EXT
           WHEN    "X98"
               PERFORM 320-X98SPASET-SEC
               GO  TO      300-SCREEN-EXT
      *    作表処理
           WHEN    "XID1"
               PERFORM 330-XID1-SET-SEC
               GO  TO      300-SCREEN-EXT
           WHEN    "XID2"
               PERFORM 350-XID2-SET-SEC
               GO  TO      300-SCREEN-EXT
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
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-X91
      *
           PERFORM 3101-CMB-SYOKI-SEC
      *
           MOVE    SPA-SYSYMD      TO  SPA-NAI-KJNYMD
           MOVE    SPA-NAI-KJNYMD  TO  WRK-SYMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-EDTYMD1     TO  SPA-GMN-KJNYMD
      *
           MOVE    ALL "F"         TO  SPA-GMN-CHK-VAL
                                       SPA-GMN-CHKMORE-G
                                       SPA-GMN-CHKKAISU
      *
           MOVE    "F"             TO  SPA-GMN-CHKSRYRANGE
      *
           MOVE    SPA-GMN-SORTLST (1)
                                   TO  SPA-GMN-SORT-G
      *
           MOVE    SPA-GMN-MOREKIKANLST (1)
                                   TO  SPA-GMN-MOREKIKAN-G
      *
           MOVE    SPA-GMN-MORESORTLST (1)
                                   TO  SPA-GMN-MORESORT-G
      *
           MOVE    1               TO  SPA-GMN-CUR 
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コンボボックス初期処理
      *****************************************************************
       3101-CMB-SYOKI-SEC              SECTION.
      *
           INITIALIZE  SPA-GMN-CMB-AREA
      *
           PERFORM 3101-SORTLST-SET-SEC
      *
           PERFORM 3101-MOREKIKANLST-SET-SEC
      *
           PERFORM 3101-MORESORTLST-SET-SEC
      *
           .
       3101-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    出力順コンボボックス初期処理
      *****************************************************************
       3101-SORTLST-SET-SEC            SECTION.
      *
           MOVE    "1"                 TO  SPA-GMN-SORTL      (1)
           MOVE    "診療行為コード順"  TO  SPA-GMN-SORTMEIL   (1)
      *
           MOVE    "2"                 TO  SPA-GMN-SORTL      (2)
           MOVE    "カナ名称順"        TO  SPA-GMN-SORTMEIL   (2)
      *
           MOVE    "3"                 TO  SPA-GMN-SORTL      (3)
           MOVE    "区分番号順"        TO  SPA-GMN-SORTMEIL   (3)
      *
           MOVE    3                   TO  SPA-GMN-SORT-MAX
      *
           .
       3101-SORTLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    登録漏れ確認リスト期間コンボボックス初期処理
      *****************************************************************
       3101-MOREKIKANLST-SET-SEC       SECTION.
      *
           MOVE    "1"                 TO  SPA-GMN-MOREKIKANL      (1)
           MOVE    "なし"              TO  SPA-GMN-MOREKIKANMEIL   (1)
      *
           MOVE    "2"                 TO  SPA-GMN-MOREKIKANL      (2)
           MOVE    "２ヶ月分"
                                       TO  SPA-GMN-MOREKIKANMEIL   (2)
      *
           MOVE    "3"                 TO  SPA-GMN-MOREKIKANL      (3)
           MOVE    "６ヶ月分"
                                       TO  SPA-GMN-MOREKIKANMEIL   (3)
      *
           MOVE    3                   TO  SPA-GMN-MOREKIKAN-MAX
      *
           .
       3101-MOREKIKANLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    登録漏れ確認リスト並び順コンボボックス初期処理
      *****************************************************************
       3101-MORESORTLST-SET-SEC         SECTION.
      *
           MOVE    "1"                 TO  SPA-GMN-MORESORTL      (1)
           MOVE    "診療行為コード順"  TO  SPA-GMN-MORESORTMEIL   (1)
      *
           MOVE    "2"                 TO  SPA-GMN-MORESORTL      (2)
           MOVE    "カナ名称順"        TO  SPA-GMN-MORESORTMEIL   (2)
      *
           MOVE    "3"                 TO  SPA-GMN-MORESORTL      (3)
           MOVE    "区分番号順"        TO  SPA-GMN-MORESORTMEIL   (3)
      *
           MOVE    3                   TO  SPA-GMN-MORESORT-MAX
      *
           .
       3101-MORESORTLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コードセット処理
      *****************************************************************
       320-X98SPASET-SEC          SECTION.
      *
           IF    ( SPA-X98-SRYCD (1)   NOT =   SPACE )
               MOVE    SPA-X98-SRYCD (1)
                                       TO  SPA-GMN-STSRYCD
               PERFORM 4101-STSRYCD-CHK-SEC
               MOVE    7               TO  SPA-GMN-CUR
           ELSE
               MOVE    6               TO  SPA-GMN-CUR
           END-IF
      *
           .
       320-X98SPASET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面ＯＫ処理
      *****************************************************************
       330-XID1-SET-SEC            SECTION.
      *
           IF    ( SPA-XID1-FLG    =   "OK" )
               EVALUATE    SPA-XIDCD
               WHEN    "6666"
                   MOVE    "JOIN"  TO  MCP-PUTTYPE
                   PERFORM 210-BACK-SYORI-SEC
      *----(01.00.01)--UPD-START---
               WHEN    OTHER
      *            ジョブ管理チェック
                   PERFORM 4501-JOBKANRI-CHK-SEC
                   IF      SPA-ERRCD       =   SPACE
                       PERFORM 490-PRINT-SEC
                   END-IF
      *----(01.00.01)--UPD-END-----
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-X97-FLG
           MOVE    SPACE           TO  SPA-XID1-FLG
           MOVE    SPACE           TO  SPA-XIDCD
      *
           .
       330-XID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＺＩＤ２）ＯＫ処理
      *****************************************************************
       350-XID2-SET-SEC            SECTION.
      *
      *----(02.00.06)--UPD-START---
           IF    ( SPA-XID1-FLG    NOT =   SPACE )
      *----(01.00.01)--UPD-START---
      *        ジョブ管理チェック
               PERFORM 4501-JOBKANRI-CHK-SEC
               IF      SPA-ERRCD       =   SPACE
                   PERFORM 490-PRINT-SEC
               END-IF
      *----(01.00.01)--UPD-END-----
           ELSE
               MOVE    SPACE           TO  SPA-XID1-FLG
           END-IF
      *
           MOVE    SPACE           TO  SPA-X97-FLG
           MOVE    SPACE           TO  SPA-XIDCD
      *----(02.00.06)--UPD-END-----
      *
           .
       350-XID2-SET-EXT.
           EXIT.
      *
      *----(02.00.06)--ADD-START---
      *****************************************************************
      *    処理表示画面戻り処理
      *****************************************************************
       340-X97-SET-SEC             SECTION.
      *
           IF    ( SPA-X97-FLG     =   "OK" )
               IF    ( SPA-XID1-FLG    =   "PV" )
                   PERFORM 4501-NEXT-GMN-SEC
               ELSE
                   MOVE    SPA-X91-TBL-GROUP
                                   TO  SPA-PRT-GROUP
                   PERFORM 450-REPRINT-SEC
               END-IF
           END-IF
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           MOVE    SPACE           TO  SPA-X97-FLG
           MOVE    SPACE           TO  SPA-XID1-FLG
           MOVE    SPACE           TO  SPA-XIDCD
      *
           .
       340-X97-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビュー処理
      *****************************************************************
       4501-NEXT-GMN-SEC            SECTION.
      *
      *    画面移行用のパラメタ変更
           MOVE    "X91"               TO  SPA-MOTOPG
           MOVE    "XC01"              TO  SPA-SAKIPG
      *
           MOVE    0                   TO  WRK-STPAGE
           MOVE    0                   TO  WRK-EDPAGE
      *
           INITIALIZE                  LNK-XC01-AREA
      *
           MOVE    CONST-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
           MOVE    ZERO                TO  LNK-XC01-RENNUM
           MOVE    SPA-X91-TBL-GROUP   TO  LNK-XC01-TBL-GROUP
           MOVE    ZERO                TO  LNK-XC01-SHORI-RENNUM
           MOVE    WRK-STPAGE          TO  LNK-XC01-ST-PAGE
           MOVE    WRK-EDPAGE          TO  LNK-XC01-ED-PAGE
           MOVE    "X91"               TO  LNK-XC01-MOTOPG
           MOVE    SPACE               TO  LNK-XC01-MODE
      *
           MOVE    LNK-XC01-AREA       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-X91             TO  LNK-SCRSPA
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   "XC01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4501-NEXT-GMN-EXT.
           EXIT.
      *----(02.00.06)--ADD-END-----
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"     TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 310-SPASET-SEC
      *    再印刷
           WHEN    "CLICKED"       ALSO    "B05"
               PERFORM 4101-GMN-SPA-SET-SEC
               MOVE    SPACE    TO  SPA-PRT-GROUP
               PERFORM 450-REPRINT-SEC
      *----(04.08.01)--ADD-START---
      *    CSV出力
           WHEN    "CLICKED"       ALSO    "B10"
               PERFORM 4101-GMN-SPA-SET-SEC
               PERFORM 490-PUTCSV-SEC
      *----(04.08.01)--ADD-END-----
      *    情報削除
           WHEN    "CLICKED"       ALSO    "B11"
               PERFORM 4101-GMN-SPA-SET-SEC
               PERFORM 490-JYOUHOU-DEL-SEC
      *    印刷
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 490-PRINT-MAE-SEC
      *    処理確認
           WHEN    "CLICKED"       ALSO    "B13"
               PERFORM 4101-GMN-SPA-SET-SEC
               PERFORM 490-KAKUNIN-SEC
      *
      *    薬剤と病名チェックボタン
           WHEN    "CLICKED"       ALSO    "CHKYKZBYO"
               MOVE    X91-CHKYKZBYO   TO  SPA-GMN-CHKYKZBYO
               PERFORM 4101-GMN-SPA-SET-SEC
               PERFORM 41011-CHKMST-CHK-SEC
      *
      *    診療行為と病名チェックボタン
           WHEN    "CLICKED"       ALSO    "CHKSRYBYO"
               MOVE    X91-CHKSRYBYO   TO  SPA-GMN-CHKSRYBYO
               PERFORM 4101-GMN-SPA-SET-SEC
               PERFORM 41011-CHKMST-CHK-SEC
      *
      *    薬剤と併用禁忌チェックボタン
           WHEN    "CLICKED"       ALSO    "CHKYKZHEI"
               MOVE    X91-CHKYKZHEI   TO  SPA-GMN-CHKYKZHEI
               PERFORM 4101-GMN-SPA-SET-SEC
               PERFORM 41011-CHKMST-CHK-SEC
      *
      *    診療行為の併用算定チェックボタン
           WHEN    "CLICKED"       ALSO    "CHKSRYHEI"
               MOVE    X91-CHKSRYHEI   TO  SPA-GMN-CHKSRYHEI
               PERFORM 4101-GMN-SPA-SET-SEC
               PERFORM 41011-CHKMST-CHK-SEC
      *    投与禁忌薬剤と病名
           WHEN    "CLICKED"       ALSO    "CHKYKZKNKBYO"
               MOVE    X91-CHKYKZKNKBYO    TO  SPA-GMN-CHKYKZKNKBYO
               PERFORM 4101-GMN-SPA-SET-SEC
               PERFORM 41011-CHKMST-CHK-SEC
      *    診療行為どおし
           WHEN    "CLICKED"       ALSO    "CHKSRYSNT"
               MOVE    X91-CHKSRYSNT       TO  SPA-GMN-CHKSRYSNT
               PERFORM 4101-GMN-SPA-SET-SEC
               PERFORM 41011-CHKMST-CHK-SEC
      *----(03.04.01)--ADD-START---
      *    病名と薬剤
           WHEN    "CLICKED"       ALSO    "CHKBYOYKZ"
               MOVE    X91-CHKBYOYKZ      TO  SPA-GMN-CHKBYOYKZ
               PERFORM 4101-GMN-SPA-SET-SEC
               PERFORM 41011-CHKMST-CHK-SEC
      *    病名と診療行為
           WHEN    "CLICKED"       ALSO    "CHKBYOSRY"
               MOVE    X91-CHKBYOSRY      TO  SPA-GMN-CHKBYOSRY
               PERFORM 4101-GMN-SPA-SET-SEC
               PERFORM 41011-CHKMST-CHK-SEC
      *
      *----(03.04.01)--ADD-END-----
      *    診療行為の併用算定（同日）
           WHEN    "CLICKED"       ALSO    "CHKSRYHEIA"
               MOVE    X91-CHKSRYHEIA     TO  SPA-GMN-CHKSRYHEIA
               PERFORM 4101-GMN-SPA-SET-SEC
               PERFORM 41011-CHKMST-CHK-SEC
      *    診療行為の併用算定（同日）
           WHEN    "CLICKED"       ALSO    "CHKSRYHEIB"
               MOVE    X91-CHKSRYHEIB     TO  SPA-GMN-CHKSRYHEIB
               PERFORM 4101-GMN-SPA-SET-SEC
               PERFORM 41011-CHKMST-CHK-SEC
      *    適応病名一覧表（薬剤）
           WHEN    "CLICKED"       ALSO    "CHKTEKBYOYKZ"
               MOVE    X91-CHKTEKBYOYKZ   TO  SPA-GMN-CHKTEKBYOYKZ
               PERFORM 4101-GMN-SPA-SET-SEC
           WHEN    "CLICKED"       ALSO    "CHKTEKBYOSRY"
               MOVE    X91-CHKTEKBYOSRY   TO  SPA-GMN-CHKTEKBYOSRY
               PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    診療行為範囲指定チェックボタン
           WHEN    "CLICKED"       ALSO    "CHKSRYRANGE"
               PERFORM 4101-CHKSRYRANGE-CHK-SEC
      *
      *    算定回数チェック一覧
           WHEN    "CLICKED"       ALSO    "CHKKAISU"
               PERFORM 4101-GMN-SPA-SET-SEC
               MOVE    X91-CHKKAISU    TO  SPA-GMN-CHKKAISU
               MOVE    ZERO            TO  SPA-GMN-CUR
      *
      *    登録漏れ確認リスト−薬剤と病名
           WHEN    "CLICKED"       ALSO    "CHKMOREYKZBYO"
               PERFORM 4101-GMN-SPA-SET-SEC
               MOVE    X91-CHKMOREYKZBYO
                                       TO  SPA-GMN-CHKMOREYKZBYO
               MOVE    ZERO            TO  SPA-GMN-CUR
      *    登録漏れ確認リスト−診療行為と病名（検査）
           WHEN    "CLICKED"       ALSO    "CHKMOREKENSA"
               PERFORM 4101-GMN-SPA-SET-SEC
               MOVE    X91-CHKMOREKENSA TO SPA-GMN-CHKMOREKENSA
               MOVE    ZERO            TO  SPA-GMN-CUR
      *    登録漏れ確認リスト−診療行為と病名（部位）
           WHEN    "CLICKED"       ALSO    "CHKMOREBUI"
               PERFORM 4101-GMN-SPA-SET-SEC
               MOVE    X91-CHKMOREBUI  TO  SPA-GMN-CHKMOREBUI
               MOVE    ZERO            TO  SPA-GMN-CUR
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
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    基準日
           WHEN    "ACTIVATE"      ALSO    "KJNYMD"
               PERFORM 4100-KJNYMD-CHK-SEC
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 410-INPUT-CHK-SEC
               END-IF
           WHEN    OTHER
      *
               MOVE    ZERO            TO  SPA-GMN-CUR
      *
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
      *
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    X91-STSRYCD     TO  SPA-GMN-STSRYCD
           MOVE    X91-EDSRYCD     TO  SPA-GMN-EDSRYCD
           MOVE    X91-SORT        TO  SPA-GMN-SORT-G
           MOVE    X91-MOREKIKAN   TO  SPA-GMN-MOREKIKAN-G
           MOVE    X91-MORESORT    TO  SPA-GMN-MORESORT-G
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
            AND  ( FLG-END         =   ZERO )
      *        開始診療行為コードチェック
               PERFORM 4101-STSRYCD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
            AND  ( FLG-END         =   ZERO )
      *        終了診療行為コードチェック
               PERFORM 4101-EDSRYCD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
            AND  ( FLG-END         =   ZERO )
      *        一覧リスト並び順チェック
               PERFORM 4101-SORT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
            AND  ( FLG-END         =   ZERO )
      *        登録漏れ確認リスト
               PERFORM 4101-MOREKIKAN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
            AND  ( FLG-END         =   ZERO )
      *        登録漏れ確認リスト並び順チェック
               PERFORM 4101-MORESORT-CHK-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    基準日
      *****************************************************************
       4100-KJNYMD-CHK-SEC             SECTION.
      *
           IF    ( X91-KJNYMD  =   SPACE )
               MOVE    SPA-SYSYMD  TO  X91-KJNYMD
           END-IF
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
           MOVE    X91-KJNYMD      TO  WRK-SYMD
           PERFORM 800-HIZUKE-CHK-SEC
           IF  ( WRK-EDTYMD1       =   SPACE )
               MOVE    "0003"      TO  SPA-ERRCD
               MOVE    101         TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-EDTYMD1 TO  SPA-GMN-KJNYMD
               MOVE    WRK-SYMD    TO  SPA-NAI-KJNYMD
           END-IF
      *
           .
      *
       4100-KJNYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始診療行為コードチェック
      *****************************************************************
       4101-STSRYCD-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-KENSAKU
           MOVE    ZERO                TO  FLG-KANJI
      *
           MOVE    ZERO                TO  WRK-CD-MCNT
           MOVE    SPACE               TO  WRK-INPUTCD
      *
           MOVE    SPACE               TO  SPA-GMN-STSRYNAME
      *
           MOVE    ZERO                TO  SPA-NAI-SRYCD-EXSIT-FLG
      *
           IF    ( SPA-GMN-STSRYCD     =   SPACE )
               GO  TO  4101-STSRYCD-CHK-EXT
           END-IF
      *
      *    "/"が入力された場合は検索画面へ
           IF    ( SPA-GMN-STSRYCD(1:1)    =   "/" )
               PERFORM 490-KENSAKU-SEC
               GO  TO  4101-STSRYCD-CHK-EXT
           END-IF
      *
      *    全角チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-STSRYCD     TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-STSRYCD
           IF    ( KANACHK-RC      NOT =   ZERO )
      *        全角半角混在エラー
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    6               TO  SPA-GMN-CUR
               GO  TO   4101-STSRYCD-CHK-EXT
           ELSE
               MOVE    KANACHK-MAX     TO  WRK-CD-MCNT
      *        全角
               IF    ( KANACHK-SYUBETU =   2 )
                   MOVE    1           TO  FLG-KANJI
                   MOVE    SPACE       TO  SPA-GMN-STSRYCD(9:)
               ELSE
                   MOVE    ZERO        TO  FLG-KANJI
               END-IF
           END-IF
      *
      *    ９桁コード入力以外は検索画面へ
           IF    ( WRK-CD-MCNT         =   9  )
            AND  ( FLG-KANJI           =   ZERO )
            AND  ( SPA-GMN-STSRYCD         NUMERIC  )
                CONTINUE
           ELSE
               MOVE    SPA-GMN-STSRYCD TO  WRK-INPUTCD
               PERFORM 490-KENSAKU-SEC
               GO  TO  4101-STSRYCD-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-GMN-STSRYCD     TO  TNS-SRYCD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
      *
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           PERFORM UNTIL ( FLG-TENSU   NOT =  ZERO )
                    OR  (( SPA-NAI-KJNYMD  >=  TNS-YUKOSTYMD )
                     AND ( SPA-NAI-KJNYMD  <=  TNS-YUKOEDYMD ))
      *
               MOVE    ZERO                TO  FLG-TENSU
      *
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
      *
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                    MOVE    1              TO  FLG-TENSU
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSE-SEC
      *
           IF     (TNS-SRYCD(1:1)      =   "1") AND
                  (TNS-ROUTEKKBN       =   2  )
                MOVE    SPACE       TO  SPA-GMN-STSRYNAME
                STRING  "［老］"        DELIMITED   BY  SIZE
                        TNS-NAME        DELIMITED   BY  SIZE
                INTO    SPA-GMN-STSRYNAME
                END-STRING
           ELSE
                MOVE    TNS-NAME    TO  SPA-GMN-STSRYNAME
           END-IF
      *
      *    チェックマスタチェック
           PERFORM 41011-CHKMST-CHK-SEC
      *
           .
       4101-STSRYCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタチェック
      *****************************************************************
       41011-CHKMST-CHK-SEC            SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           MOVE    ZERO            TO  SPA-NAI-SRYCD-EXSIT-FLG
      *
           IF    ( SPA-GMN-STSRYNAME   =   SPACE )
               GO  TO  41011-CHKMST-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-CHKKBN-MAX )
                    OR   ( SPA-NAI-SRYCD-EXSIT-FLG
                                   =   1 )
      *
               IF    ( SPA-GMN-CHKKBN (IDX0)   =   "T" )
                   EVALUATE    CONST-CHKKBN (IDX0)
                   WHEN    "1"
                   WHEN    "2"
                   WHEN    "6"
                       INITIALIZE                  CHKSND-REC
                       MOVE    CONST-CHKKBN (IDX0)
                                               TO  CHKSND-CHKKBN
                       MOVE    SPA-GMN-STSRYCD TO  CHKSND-SRYCD
                       MOVE    SPA-NAI-KJNYMD  TO  CHKSND-YUKOSTYMD
                                                   CHKSND-YUKOEDYMD
                       MOVE    ZERO            TO  CHKSND-CDKBN
                       MOVE    SPA-HOSPNUM     TO  CHKSND-HOSPNUM
                       MOVE    CHKSND-REC      TO  MCPDATA-REC
                       MOVE    "tbl_chksnd"    TO  MCP-TABLE
                       MOVE    "key30"         TO  MCP-PATHNAME
                       PERFORM 910-DBSELECT-SEC
                       IF    ( MCP-RC          =   ZERO )
                           MOVE    1       TO  SPA-NAI-SRYCD-EXSIT-FLG
                       END-IF
                       MOVE    "tbl_chksnd"             TO  MCP-TABLE
                       MOVE    "key30"                  TO  MCP-PATHNAME
                       PERFORM 910-DBCLOSE-SEC
                   WHEN    "7"
                       INITIALIZE                  CHKTRD-REC
                       MOVE    CONST-CHKKBN (IDX0)
                                               TO  CHKTRD-CHKKBN
                       MOVE    SPA-GMN-STSRYCD TO  CHKTRD-SRYCD
                       MOVE    SPA-NAI-KJNYMD  TO  CHKTRD-YUKOSTYMD
                                                   CHKTRD-YUKOEDYMD
                       MOVE    SPA-HOSPNUM     TO  CHKTRD-HOSPNUM
                       MOVE    CHKTRD-REC      TO  MCPDATA-REC
                       MOVE    "tbl_chktrd"            TO  MCP-TABLE
                       MOVE    "key2"                  TO  MCP-PATHNAME
                       PERFORM 910-DBSELECT-SEC
                       IF    ( MCP-RC          =   ZERO )
                           MOVE    1       TO  SPA-NAI-SRYCD-EXSIT-FLG
                       END-IF
                       MOVE    "tbl_chktrd"            TO  MCP-TABLE
                       MOVE    "key2"                  TO  MCP-PATHNAME
                       PERFORM 910-DBCLOSE-SEC
                   WHEN    OTHER
                       INITIALIZE                  CHK-REC
                       MOVE    CONST-CHKKBN (IDX0)
                                               TO  CHK-CHKKBN
                       MOVE    SPA-GMN-STSRYCD TO  CHK-SRYCD
                       MOVE    SPA-NAI-KJNYMD  TO  CHK-YUKOSTYMD
                                                   CHK-YUKOEDYMD
                       MOVE    SPA-HOSPNUM     TO  CHK-HOSPNUM
                       MOVE    CHK-REC         TO  MCPDATA-REC
                       MOVE    "tbl_chk"           TO  MCP-TABLE
                       MOVE    "key2"              TO  MCP-PATHNAME
                       PERFORM 910-DBSELECT-SEC
                       IF    ( MCP-RC          =   ZERO )
                           MOVE    1       TO  SPA-NAI-SRYCD-EXSIT-FLG
                       END-IF
                       MOVE    "tbl_chk"           TO  MCP-TABLE
                       MOVE    "key2"              TO  MCP-PATHNAME
                       PERFORM 910-DBCLOSE-SEC
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
           .
       41011-CHKMST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了診療行為コードチェック
      *****************************************************************
       4101-EDSRYCD-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-EDSRYCD     =   SPACE )
               GO  TO  4101-EDSRYCD-CHK-EXT
           END-IF
      *
      *    全角チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-EDSRYCD     TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-EDSRYCD
           IF    ( KANACHK-RC      NOT =   ZERO )
      *        全角半角混在エラー
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    7               TO  SPA-GMN-CUR
               GO  TO   4101-EDSRYCD-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-EDSRYCD (9:)    =   SPACE )
               IF    ( SPA-GMN-EDSRYCD (1:3)   =   "999" )
                   MOVE    "999999999" TO  SPA-GMN-EDSRYCD
               END-IF
           END-IF
      *
           .
       4101-EDSRYCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一覧リスト並び順チェック
      *****************************************************************
       4101-SORT-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-SORT    TO  WRK-CMB-CD
           PERFORM 4200-SORTLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SORTLST (1)
                                   TO  SPA-GMN-SORT-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-SORT-G
           END-IF
      *
           .
       4101-SORT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    登録漏れ確認リスト
      *****************************************************************
       4101-MOREKIKAN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-MOREKIKAN   TO  WRK-CMB-CD
           PERFORM 4200-MOREKIKANLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-MOREKIKANLST (1)
                                   TO  SPA-GMN-MOREKIKAN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-MOREKIKAN-G
           END-IF
      *
           .
       4101-MOREKIKAN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    登録漏れ確認リスト並び順チェック
      *****************************************************************
       4101-MORESORT-CHK-SEC           SECTION.
      *
           MOVE    SPA-GMN-MORESORT    TO  WRK-CMB-CD
           PERFORM 4200-MORESORTLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-MORESORTLST (1)
                                   TO  SPA-GMN-MORESORT-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-MORESORT-G
           END-IF
      *
           .
       4101-MORESORT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       4101-KANREN-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-CHK-VAL     = ALL "F" )
            AND  ( SPA-GMN-CHKMORE-G   = ALL "F" )
            AND  ( SPA-GMN-CHKKAISU    = "F" )
               MOVE    "0100"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
               GO  TO  4101-KANREN-CHK-EXT
           END-IF
      *
      *    診療行為コードで絞り込むが選択されている場合のチェック
           IF    ( SPA-GMN-CHKSRYRANGE =   "T" )
      *
               IF    ( SPA-GMN-STSRYCD NOT =   SPACE )
      *            開始診療行為コードの桁数をチェック
                   MOVE    SPA-GMN-STSRYCD TO  WRK-STR-IN
                   PERFORM 800-LENGTH-CHK-SEC
                   IF    ( WRK-STR-LEN     NOT =   9 )
                       MOVE    "0101"      TO  SPA-ERRCD
                       MOVE    6           TO  SPA-GMN-CUR
                       GO  TO  4101-KANREN-CHK-EXT
                   END-IF
               ELSE
                       MOVE    "0102"      TO  SPA-ERRCD
                       MOVE    6           TO  SPA-GMN-CUR
                       GO  TO  4101-KANREN-CHK-EXT
               END-IF
      *
      *        終了診療行為コードの桁数をチェック
               IF    ( SPA-GMN-EDSRYCD NOT =   SPACE )
                   MOVE    SPA-GMN-EDSRYCD TO  WRK-STR-IN
                   PERFORM 800-LENGTH-CHK-SEC
                   IF    ( WRK-STR-LEN     NOT =   9 )
                       MOVE    "0103"      TO  SPA-ERRCD
                       MOVE    7           TO  SPA-GMN-CUR
                       GO  TO  4101-KANREN-CHK-EXT
                   END-IF
      *
                   IF    ( SPA-GMN-STSRYCD <=  SPA-GMN-EDSRYCD )
                       CONTINUE
                   ELSE
                       MOVE    "0104"      TO  SPA-ERRCD
                       MOVE    6           TO  SPA-GMN-CUR
                       GO  TO  4101-KANREN-CHK-EXT
                   END-IF
               ELSE
                   IF    ( SPA-GMN-STSRYNAME   =   SPACE )
                       MOVE    "0105"      TO  SPA-ERRCD
                       MOVE    6           TO  SPA-GMN-CUR
                       GO  TO  4101-KANREN-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           .
       4101-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為範囲指定チェックボタンチェック処理
      *****************************************************************
       4101-CHKSRYRANGE-CHK-SEC            SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    X91-CHKSRYRANGE     TO  SPA-GMN-CHKSRYRANGE
      *
           IF    ( SPA-GMN-CHKSRYRANGE =   "F" )
               INITIALIZE              SPA-GMN-STSRYCD
                                       SPA-GMN-STSRYNAME
                                       SPA-GMN-EDSRYCD
           END-IF
      *
           MOVE    6                   TO  SPA-GMN-CUR
      *
           .
       4101-CHKSRYRANGE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    出力順コンボチェック処理
      *****************************************************************
       4200-SORTLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SORT-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SORTL (IDX-CMB) =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SORTLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SORTLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    登録漏れ確認リスト期間コンボチェック処理
      *****************************************************************
       4200-MOREKIKANLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-MOREKIKAN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-MOREKIKANL (IDX-CMB) =   WRK-CMB-CD )
                   MOVE    SPA-GMN-MOREKIKANLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-MOREKIKANLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    登録漏れ確認リスト並び順コンボチェック処理
      *****************************************************************
       4200-MORESORTLST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-MORESORT-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-MORESORTL (IDX-CMB) =   WRK-CMB-CD )
                   MOVE    SPA-GMN-MORESORTLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-MORESORTLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    CONST-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-3003-REC 
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0 >      SYS-3003-JOBID )
      *
      *            ジョブ管理チェック処理
                   MOVE    "CHK"       TO  SJOBKANRI-MODE
                   INITIALIZE              JOBKANRI-REC
                   MOVE    IDX0        TO  JOB-JOBID
                   MOVE    CONST-JOB-SHELLID
                                       TO  JOB-SHELLID
                   MOVE    SPA-HOSPNUM TO  JOB-HOSPNUM
                   PERFORM 800-ORCSJOB-SEC
                   IF    ( SJOBKANRI-RETURN
                                           =   ZERO )
                       IF    ( JOB-ENDYMD      =   SPACE )
      *********        AND     JOB-TERMID      =   SPA-TERMID
                           MOVE    "6666"  TO  SPA-XIDCD
                           GO  TO  210-BACK-EXT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           PERFORM 210-BACK-SYORI-SEC
      *
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
           MOVE    "X01"               TO  SPA-SAKIPG
           MOVE    "X91"               TO  SPA-MOTOPG
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
      *----(04.08.01)--ADD-START---
      *****************************************************************
      *    ＣＳＶ出力画面へ
      *****************************************************************
       490-PUTCSV-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-X91             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "X91"               TO  SPA-MOTOPG
           MOVE    "XG01"              TO  SPA-SAKIPG
      *
           MOVE    CONST-JOB-SHELLID   TO  SPA-JOB-SHELLID
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XG01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-PUTCSV-EXT.
           EXIT.
      *----(04.08.01)--ADD-END-----
      *****************************************************************
      *    再印刷画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-X91             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "X91"               TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
      *
           MOVE    CONST-JOB-SHELLID
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
      *    検索画面へ
      *****************************************************************
       490-KENSAKU-SEC                 SECTION.
      *
      *    共通ＳＰＡ編集
           INITIALIZE                  SPA-X98-AREA
           MOVE    "2"                 TO  SPA-X98-TYPE
      *
           INITIALIZE                  SPA-X98GMN-AREA
           IF    ( WRK-INPUTCD         NOT =   SPACE )
               MOVE    WRK-INPUTCD     TO  SPA-X98-INPUTCD
               MOVE    "1"             TO  SPA-X98-TYPE
               IF    ( FLG-KANJI       =   1 )
                   MOVE    "J"         TO  SPA-X98-CDSYUBETU
               ELSE
                   IF    ( WRK-INPUTCD(1:1)    NUMERIC )
                       EVALUATE    WRK-CD-MCNT
                       WHEN    3
                       WHEN    6
                           MOVE    "6"     TO  SPA-X98-CDSYUBETU
                       WHEN    4
                           MOVE    "4"     TO  SPA-X98-CDSYUBETU
                       WHEN    5
                           MOVE    "5"     TO  SPA-X98-CDSYUBETU
                       END-EVALUATE
                   ELSE
                       MOVE    "A"         TO  SPA-X98-CDSYUBETU
                   END-IF
               END-IF
               MOVE    "2"                 TO  SPA-X98-GMN-KBN
      *        画面編集
               PERFORM 41011-X98-SET-SEC
           ELSE
      *        画面編集
               MOVE    "2"                 TO  SPA-X98-GMN-KBN
               PERFORM 41011-X98-SET-SEC
           END-IF
      *
           MOVE    "X91"               TO  SPA-MOTOPG
           MOVE    "X98"               TO  SPA-SAKIPG
           MOVE    "X91"               TO  SPA-X98-MOTOPG
      *
           IF    ( SPA-X98-GMN-MAX     =   ZERO )
               MOVE    "INPUT"         TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM1"       TO  MCP-WIDGET
           END-IF
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE
           MOVE   "X98    "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索画面編集処理
      *****************************************************************
       41011-X98-SET-SEC               SECTION.
      *
           MOVE    SPA-NAI-KJNYMD  TO  SPA-X01-KJNYMD
           MOVE    SPACE           TO  SPA-X01-CHKKBN
      *
           MOVE    SPA-X01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-X91         TO  SPA-X01SONOTA
           MOVE    SPA-X01FREE     TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGX98"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-X01KYOUTU 
           MOVE    SPA-FREE        TO  SPA-X01FREE
           MOVE    SPA-X01SONOTA   TO  SPA-X91
      *
           .
       41011-X98-SET-EXT.
           EXIT.
      *****************************************************************
      *    再印刷画面へ
      *****************************************************************
       450-REPRINT-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-X91             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "X91"               TO  SPA-MOTOPG
           MOVE    "XA01"              TO  SPA-SAKIPG
      *
           MOVE    CONST-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
      *          
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XA01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       450-REPRINT-EXT.
           EXIT.
      *
      *----(01.00.01)--ADD-START---
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       4501-JOBKANRI-CHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    CONST-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-3003-REC
               PERFORM VARYING IDX0 FROM    1   BY  1
                       UNTIL ( IDX0 >       SYS-3003-JOBID )
      *
      *            ジョブ管理チェック処理
                   MOVE    "CHK"       TO  SJOBKANRI-MODE
                   INITIALIZE              JOBKANRI-REC
                   MOVE    IDX0        TO  JOB-JOBID
                   MOVE    CONST-JOB-SHELLID
                                       TO  JOB-SHELLID
                   MOVE    SPA-HOSPNUM TO  JOB-HOSPNUM
                   PERFORM 800-ORCSJOB-SEC
                   IF    ( SJOBKANRI-RETURN
                                           =   ZERO )
                       IF    ( JOB-ENDYMD  =   SPACE )
                           MOVE    "7777"          TO  SPA-ERRCD
                           MOVE    SPACE           TO  WRK-SHORIERR
                           STRING  "処理中です【"  DELIMITED  BY  SIZE
                                   JOB-SHELLMSG    DELIMITED  BY  SPACE
                                   "】"            DELIMITED  BY  SIZE
                                                   INTO  WRK-SHORIERR
                           END-STRING
                           MOVE    SYS-3003-JOBID  TO  IDX0
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       4501-JOBKANRI-CHK--EXT.
           EXIT.
      *
      *----(01.00.01)--ADD-END-----
      *****************************************************************
      *    印刷前確認処理
      *****************************************************************
       490-PRINT-MAE-SEC                   SECTION.
      *
      *    入力チェック
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  490-PRINT-MAE-EXT
           END-IF
      *
      *    関連チェック
           PERFORM 4101-KANREN-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  490-PRINT-MAE-EXT
           END-IF
      *
      *----(01.00.01)--UPD-START---
      *    ジョブ管理チェック
           PERFORM 4501-JOBKANRI-CHK-SEC
           IF   (  SPA-ERRCD        NOT =   SPACE )
               GO  TO  490-PRINT-MAE-EXT
           END-IF
      *----(01.00.01)--UPD-END-----
      *
           MOVE    "1001"              TO  SPA-XIDCD
      *
           .
       490-PRINT-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       490-PRINT-SEC                       SECTION.
      * 
      *----(02.00.06)--ADD-START---
           INITIALIZE                      SPA-X91-TBL-GROUP
                                           SPA-X91-JOB-JOBID
      *----(02.00.06)--ADD-END-----
      *    ＪＯＢＩＤ（３００４）削除処理
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    CONST-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-3003-REC
               MOVE    SYS-3003-REC    TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF    ( MCP-RC          =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    "1005"      TO  SPA-ERRCD
                   GO  TO  490-PRINT-EXT
               END-IF
           END-IF
      * 
      *    ジョブ管理削除処理
           MOVE    "ALL"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    CONST-JOB-SHELLID
                                       TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM TO  JOB-HOSPNUM
           PERFORM 800-ORCSJOB-SEC
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               CONTINUE
           ELSE
               MOVE    "1004"          TO  SPA-ERRCD
               GO  TO  490-PRINT-EXT
           END-IF
      *
      *    印刷処理
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM              TO  WRK-SYSMM
           MOVE    SYS-DD              TO  WRK-SYSDD
           ACCEPT  SYS-TIME            FROM    TIME
      *
           MOVE    ZERO                TO  CONST-JOB-JOBID
                                           WRK-PRTKANRI-RENNUM
           MOVE    SPACE               TO  CONST-PRTKANRI-TBL-GROUP
           MOVE    WRK-SYSYMD          TO
                                       CONST-PRTKANRI-TBL-GROUP (1:8)
           MOVE    SYS-TIME            TO
                                       CONST-PRTKANRI-TBL-GROUP (9:)
      *
           MOVE    SPA-HOSPNUM         TO  CHKMERR-HOSPNUM
           MOVE    "OR103ERR"          TO  CHKMERR-FILE-ID
           MOVE    SPA-TERMID          TO  CHKMERR-TERMID
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
           IF    ( ORCSPRTNM-RC    NOT =   ZERO )
               INITIALIZE                  SYS-1031-REC
           END-IF 
      *
           MOVE    SPA-GMN-SORT        TO  WRK-SRTKBN
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-CHKKBN-MAX )
      *
               IF    ( SPA-GMN-CHKKBN (IDX0)   =   "T" )
                   MOVE    CONST-CHKKBN (IDX0)
                                       TO  WRK-CHKKBN
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-MORESORT    TO  WRK-SRTKBN
      *
           MOVE    ZERO                TO  WRK-KIKAN
           EVALUATE    SPA-GMN-MOREKIKAN
           WHEN    "2"
               MOVE    2               TO  WRK-KIKAN
           WHEN    "3"
               MOVE    6               TO  WRK-KIKAN
           END-EVALUATE
      *
           IF    ( SPA-GMN-CHKMOREYKZBYO   =   "T" )
               MOVE    "1"             TO  WRK-CHKMORE
               MOVE    SPACE           TO  WRK-MORERANGE-G
               PERFORM 4902-PRINT-SYORI-SEC
           END-IF
      *
           IF    ( SPA-GMN-CHKMOREKENSA    =   "T" )
            OR   ( SPA-GMN-CHKMOREBUI      =   "T" )
               MOVE    "2"             TO  WRK-CHKMORE
               IF    ( SPA-GMN-CHKMOREKENSA    =   "T" )
                   MOVE    "1"         TO  WRK-MOREKENSA
               ELSE
                   MOVE    "0"         TO  WRK-MOREKENSA
               END-IF
               IF    ( SPA-GMN-CHKMOREBUI      =   "T" )
                   MOVE    "1"         TO  WRK-MOREBUI
               ELSE
                   MOVE    "0"         TO  WRK-MOREBUI
               END-IF
               PERFORM 4902-PRINT-SYORI-SEC
           END-IF
      *
           IF    ( SPA-GMN-CHKKAISU        =   "T" )
               PERFORM 4904-PRINT-SYORI-SEC
           END-IF
      *
      *    印刷処理
           IF      SPA-XID1-FLG        =   "OK"  OR "PV"
               PERFORM 4903-PRINT-SYORI-SEC
           END-IF
      *
      *    ＪＯＢＩＤ更新処理
           PERFORM 45012-JOBID-UPD-SEC
      *
           PERFORM 490-KAKUNIN-SEC
      *
           .
       490-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       4901-PRINT-SYORI-SEC            SECTION.
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *
      *    ファイルデイレクトリ位置指定
           MOVE    SPACE           TO  LNK-PRTKANRI-AREA
           INITIALIZE                  LNK-PRTKANRI-AREA
           ADD     1               TO  WRK-PRTKANRI-RENNUM
           MOVE    WRK-PRTKANRI-RENNUM
                                   TO  LNK-PRTKANRI-RENNUM
           MOVE    1               TO  LNK-PRTKANRI-SHORI-RENNUM
           MOVE    CONST-PRTKANRI-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY 
           MOVE    CONST-PRTKANRI-TBL-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    "000000"        TO  LNK-PRTKANRI-SRYYM
           MOVE    SPA-SYSYMD      TO  LNK-PRTKANRI-SKYYMD
           MOVE    "chkmst.sh"     TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
           MOVE    SYS-1031-PRTNM (6)
                                   TO  LNK-PRTKANRI-PRTNM
      *
           MOVE    SPACE           TO  SHELLTBL 
           INITIALIZE              ORCSMKPASSAREA
           MOVE    "chkmst.sh"     TO  SHELLTBL-NAME
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
           MOVE    CONST-JOB-JOBID
                                   TO  SHELLTBL-ARG12
           MOVE    CONST-JOB-SHELLID
                                   TO  SHELLTBL-ARG13
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG14
           MOVE    WRK-CHKKBN      TO  SHELLTBL-ARG15
           IF    ( WRK-CHKKBN      =   "X" OR "Y" )
               MOVE    "3"         TO  SHELLTBL-ARG16
           ELSE
               MOVE    WRK-SRTKBN  TO  SHELLTBL-ARG16
           END-IF
           MOVE    SPA-GMN-STSRYCD TO  SHELLTBL-ARG17
           MOVE    SPA-GMN-EDSRYCD TO  SHELLTBL-ARG18
           MOVE    CHKMERR         TO  SHELLTBL-ARG19
           MOVE    SPA-NAI-KJNYMD  TO  SHELLTBL-ARG20
      *
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "allways"       TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ERROR MCP-RC=" MCP-RC 
           END-IF
           .
      *
       4901-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       4902-PRINT-SYORI-SEC            SECTION.
      *
           MOVE    SPACE           TO  PRMTBL
      *
           MOVE    "ORCBX02"       TO  PRMTBL-ARG1
      *
           MOVE    WRK-CHKMORE     TO  PRMTBL-ARG17
           MOVE    WRK-KIKAN       TO  PRMTBL-ARG18
           MOVE    WRK-MORERANGE-G TO  PRMTBL-ARG19
           MOVE    WRK-SRTKBN      TO  PRMTBL-ARG20
           MOVE    SPA-SYSYMD      TO  PRMTBL-ARG21
      *
           PERFORM 49021-PRINT-SYORI-SEC
      *
           .
      *
       4902-PRINT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       49021-PRINT-SYORI-SEC            SECTION.
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *
      *    ファイルデイレクトリ位置指定
           MOVE    SPACE           TO  LNK-PRTKANRI-AREA
           INITIALIZE                  LNK-PRTKANRI-AREA
           ADD     1               TO  WRK-PRTKANRI-RENNUM
           MOVE    WRK-PRTKANRI-RENNUM
                                   TO  LNK-PRTKANRI-RENNUM
           MOVE    CONST-PRTKANRI-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY 
           MOVE    CONST-PRTKANRI-TBL-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    "000000"        TO  LNK-PRTKANRI-SRYYM
           MOVE    SPA-SYSYMD      TO  LNK-PRTKANRI-SKYYMD
           MOVE    "chkmst2.sh"    TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
           MOVE    SYS-1031-PRTNM (6)
                                   TO  LNK-PRTKANRI-PRTNM
      *
           MOVE    SPACE           TO  SHELLTBL
           MOVE    "chkmst2.sh"    TO  SHELLTBL-NAME
           MOVE    PRMTBL-ARG1     TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG3
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG6
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG10
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG11
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SHELLTBL-ARG12
      *
           MOVE    CONST-JOB-JOBID
                                   TO  SHELLTBL-ARG13
           MOVE    CONST-JOB-SHELLID
                                   TO  SHELLTBL-ARG14
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG15
           MOVE    CHKMERR         TO  SHELLTBL-ARG16
           MOVE    PRMTBL-ARG17    TO  SHELLTBL-ARG17
           MOVE    PRMTBL-ARG18    TO  SHELLTBL-ARG18
           MOVE    PRMTBL-ARG19    TO  SHELLTBL-ARG19
           MOVE    PRMTBL-ARG20    TO  SHELLTBL-ARG20
           MOVE    PRMTBL-ARG21    TO  SHELLTBL-ARG21
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "allways"       TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ERROR MCP-RC=" MCP-RC 
           END-IF
      *
           .
      *
       49021-PRINT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       4903-PRINT-SYORI-SEC            SECTION.
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *
           MOVE    CONST-PRTKANRI-TBL-GROUP
                                       TO  SPA-X91-TBL-GROUP
           MOVE    CONST-JOB-JOBID
                                       TO  SPA-X91-JOB-JOBID
      *
           MOVE    0                   TO  WRK-STPAGE
           MOVE    99999               TO  WRK-EDPAGE
      *
           MOVE    SPACE               TO  SHELLTBL
      *
      *    ファイルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    "print_prv.sh"      TO  SHELLTBL-NAME
           MOVE    SPACE               TO  LNK-PRTKANRI-AREA
           INITIALIZE                      LNK-PRTKANRI-AREA
           MOVE    CONST-PRTKANRI-TBL-KEY
                                       TO  LNK-PRTKANRI-TBL-KEY
           MOVE    CONST-PRTKANRI-TBL-GROUP
                                       TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    ZERO                TO  LNK-PRTKANRI-RENNUM
           MOVE    ZERO                TO  LNK-PRTKANRI-SHORI-RENNUM
           MOVE    SPA-SYSYMD(1:6)     TO  LNK-PRTKANRI-SRYYM
           MOVE    SPA-SYSYMD          TO  LNK-PRTKANRI-SKYYMD
           MOVE    "print_prv.sh"      TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO                TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID          TO  LNK-PRTKANRI-TERMID
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
           MOVE    CHKMERR             TO  SHELLTBL-ARG12
           MOVE    CONST-JOB-JOBID  TO  SHELLTBL-ARG13
           MOVE    CONST-JOB-SHELLID
                                       TO  SHELLTBL-ARG14
           MOVE    WRK-STPAGE          TO  SHELLTBL-ARG15
           MOVE    WRK-EDPAGE          TO  SHELLTBL-ARG16
           EVALUATE    SPA-XID1-FLG
           WHEN    "PV"
               MOVE    "3"             TO  SHELLTBL-ARG17
      *     WHEN    "NO"
      *         MOVE    "3"             TO  SHELLTBL-ARG17
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
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG18
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
           MOVE    "SHELL"             TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       4903-PRINT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       4904-PRINT-SYORI-SEC            SECTION.
      *
           MOVE    SPACE           TO  PRMTBL
      *
           MOVE    "ORCBX03"       TO  PRMTBL-ARG1
      *
           MOVE    SPA-NAI-KJNYMD  TO  PRMTBL-ARG17
      *
           PERFORM 49021-PRINT-SYORI-SEC
      *
           .
      *
       4904-PRINT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ開始セット処理
      *****************************************************************
       45011-JOBKANRI-HENSYU-SEC       SECTION.
      *
           ADD     1               TO  CONST-JOB-JOBID
      *       
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CONST-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    CONST-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM TO  JOB-HOSPNUM
           PERFORM 800-ORCSJOB-SEC
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CONST-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    CONST-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
           MOVE    SPA-HOSPNUM TO  JOB-HOSPNUM
           PERFORM 800-ORCSJOB-SEC
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
           MOVE    CONST-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
      *       
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    CONST-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    ZERO                TO  SYS-3003-STYUKYMD
           MOVE    ALL "9"             TO  SYS-3003-EDYUKYMD
           MOVE    CONST-PRTKANRI-TBL-KEY
                                       TO  SYS-3003-TBL-KEY
           MOVE    CONST-PRTKANRI-TBL-GROUP
                                       TO  SYS-3003-TBL-GROUP
           MOVE    CONST-JOB-JOBID  TO  SYS-3003-JOBID
      *
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "DBUPDATE"      TO  MCP-FUNC
           ELSE
               MOVE    "DBINSERT"      TO  MCP-FUNC
           END-IF
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          NOT =   ZERO )
               DISPLAY "SYSKANRI 3003  ERR KEY = ["
                                  MCP-RC  " " SYS-3003-KEY  "]"
           END-IF
      *
           .
       45012-JOBID-UPD-EXT.
           EXIT.
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
           MOVE    SPACE               TO  X97
           INITIALIZE                      X97
           MOVE    ZERO                TO  X97-COUNT
      *----(02.00.06)--ADD-START---
           MOVE    WIDGET-INSENSITIVE  TO  X97-B12-STATE
           MOVE    ZERO                TO  WRK-PRT-PAGE
      *----(02.00.06)--ADD-END-----
      *
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    CONST-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-3003-REC 
               PERFORM VARYING IDX0 FROM   1   BY  1
                       UNTIL ( IDX0 >      SYS-3003-JOBID )
      *
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX0            TO  JOB-JOBID
                   MOVE    CONST-JOB-SHELLID
                                           TO  JOB-SHELLID
                   MOVE    SPA-HOSPNUM TO  JOB-HOSPNUM
                   PERFORM 800-ORCSJOB-SEC
      *
                   IF    ( SJOBKANRI-RETURN    =   ZERO )
      *
                       MOVE    JOB-JOBID       TO  X97-RENNUM   (IDX0)
                       MOVE    JOB-SHELLMSG    TO  X97-SHORINM  (IDX0)
                       MOVE    JOB-STARTTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-STARTTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-STARTTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  X97-STARTTIME(IDX0)
                       MOVE    JOB-ENDTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-ENDTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-ENDTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  X97-ENDTIME  (IDX0)
                       MOVE    JOB-UPDCNT      TO  WRK-PAGE
                       MOVE    WRK-PAGE        TO  X97-PAGE     (IDX0)
                       MOVE    JOB-YOBI        TO  X97-ERRMSG   (IDX0)
                       IF    ( JOB-ENDYMD      =   SPACE )
                           MOVE    "処理中です"    TO  X97-MSG
                       ELSE
                           IF    ( JOB-ERRCD   =   SPACE )
                               CONTINUE
                           ELSE
                               MOVE    SPACE   TO  X97-MSG
                               STRING
                                   JOB-YOBI        DELIMITED  BY  SPACE
                                   "【"            DELIMITED  BY  SIZE
                                   JOB-SHELLMSG    DELIMITED  BY  SPACE
                                   "】"            DELIMITED  BY  SIZE
                               INTO    X97-MSG
                               END-STRING
                           END-IF
                       END-IF
                       MOVE    IDX0        TO  X97-COUNT
                   END-IF
               END-PERFORM
           END-IF
      *
      *----(02.00.06)--UPDA-START---
           MOVE    "B11"               TO  MCP-WIDGET
           IF    ( X97-MSG             =   SPACE )
               MOVE    "B01"           TO  MCP-WIDGET
               IF    ( X97-COUNT       >   ZERO )
                   MOVE    "処理は正常に終了しました"
                                       TO  X97-MSG
               ELSE
                   MOVE    "該当する処理はありません"
                                       TO  X97-MSG
               END-IF
           END-IF
      *
           EVALUATE    SPA-XID1-FLG
           WHEN    "PV"
               MOVE    "F12:プレビュー"    TO  X97-B12-LABEL
               IF    ( WRK-PRT-PAGE    >   ZERO )
                       MOVE    WIDGET-NORMAL    TO  X97-B12-STATE
                       MOVE    "B12"            TO  MCP-WIDGET
               END-IF
           WHEN    "OK"
               MOVE    "F12:印刷"               TO  X97-B12-LABEL
               IF    ( WRK-PRT-PAGE    >   ZERO )
                       MOVE    WIDGET-NORMAL    TO  X97-B12-STATE
                       MOVE    "B12"            TO  MCP-WIDGET
               END-IF
           END-EVALUATE
      *----(02.00.06)--UPD-END-----
      *
           MOVE    "X91"               TO  SPA-MOTOPG
           MOVE    "X97"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "X97"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-XIDCD       NOT =   SPACE )
               EVALUATE    SPA-XIDCD
               WHEN    "1001"
                   PERFORM 540-XID2SET-SEC
                   GO  TO  500-SET-SCREEN-EXT
               WHEN    OTHER
                   PERFORM 520-XIDSET-SEC
                   GO  TO  500-SET-SCREEN-EXT
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "X91    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE                      X91
      *
           MOVE    SPA-GMN-KJNYMD      TO  X91-KJNYMD
           MOVE    SPA-GMN-CHKYKZBYO   TO  X91-CHKYKZBYO
           MOVE    SPA-GMN-CHKSRYBYO   TO  X91-CHKSRYBYO
           MOVE    SPA-GMN-CHKYKZHEI   TO  X91-CHKYKZHEI
           MOVE    SPA-GMN-CHKSRYHEI   TO  X91-CHKSRYHEI
           MOVE    SPA-GMN-CHKYKZKNKBYO    TO  X91-CHKYKZKNKBYO
           MOVE    SPA-GMN-CHKSRYSNT   TO  X91-CHKSRYSNT
      *----(03.04.01)--ADD-START---
           MOVE    SPA-GMN-CHKBYOYKZ   TO  X91-CHKBYOYKZ
           MOVE    SPA-GMN-CHKBYOSRY   TO  X91-CHKBYOSRY
      *----(03.04.01)--ADD-END-----
           MOVE    SPA-GMN-CHKSRYHEIA  TO  X91-CHKSRYHEIA
           MOVE    SPA-GMN-CHKSRYHEIB  TO  X91-CHKSRYHEIB
           MOVE    SPA-GMN-CHKTEKBYOYKZ TO  X91-CHKTEKBYOYKZ
           MOVE    SPA-GMN-CHKTEKBYOSRY TO  X91-CHKTEKBYOSRY
           MOVE    SPA-GMN-CHKSRYRANGE TO  X91-CHKSRYRANGE
           MOVE    SPA-GMN-STSRYCD     TO  X91-STSRYCD
           MOVE    SPA-GMN-STSRYNAME   TO  X91-STSRYNAME
           MOVE    SPA-GMN-EDSRYCD     TO  X91-EDSRYCD
           MOVE    SPA-GMN-CHKKAISU    TO  X91-CHKKAISU
           MOVE    SPA-GMN-CHKMOREYKZBYO TO X91-CHKMOREYKZBYO
           MOVE    SPA-GMN-CHKMOREKENSA TO X91-CHKMOREKENSA
           MOVE    SPA-GMN-CHKMOREBUI  TO X91-CHKMOREBUI
      *
      *
           MOVE    SPA-GMN-SORT-G TO  X91-SORT
           MOVE    SPA-GMN-SORT-MAX TO X91-SORT-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SORT-MAX )
      *
               MOVE    SPA-GMN-SORTLST (IDX-CMB)
                                       TO  X91-SORT-ITEM (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-MOREKIKAN-G TO  X91-MOREKIKAN
           MOVE    SPA-GMN-MOREKIKAN-MAX TO X91-MOREKIKAN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-MOREKIKAN-MAX )
      *
               MOVE    SPA-GMN-MOREKIKANLST (IDX-CMB)
                                       TO  X91-MOREKIKAN-ITEM (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-MORESORT-G TO  X91-MORESORT
           MOVE    SPA-GMN-MORESORT-MAX TO X91-MORESORT-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-MORESORT-MAX )
      *
               MOVE    SPA-GMN-MORESORTLST (IDX-CMB)
                                       TO  X91-MORESORT-ITEM (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    "black"             TO  X91-STSRYNAME-STYLE
           IF    ( SPA-NAI-SRYCD-EXSIT-FLG
                                       =   ZERO )
               MOVE    "blue"          TO  X91-STSRYNAME-STYLE
           END-IF
      *
           IF    ( SPA-GMN-CHKSRYRANGE =   "T" )
               MOVE    WIDGET-NORMAL   TO  X91-STSRYCD-STATE
                                           X91-STSRYNAME-STATE
                                           X91-EDSRYCD-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE
                                       TO  X91-STSRYCD-STATE
                                           X91-STSRYNAME-STATE
                                           X91-EDSRYCD-STATE
           END-IF
      *
      *    カーソル位置
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル位置処理
      *****************************************************************
       5001-MAPCUR-SEC                 SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
             AND ( SPA-GMN-CUR         =   ZERO  )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 50012-CUR-SKIP-SEC
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    01
               MOVE    "CHKYKZBYO"     TO  MCP-WIDGET
           WHEN    02
               MOVE    "CHKSRYBYO"     TO  MCP-WIDGET
           WHEN    03
               MOVE    "CHKYKZHEI"     TO  MCP-WIDGET
           WHEN    04
               MOVE    "CHKSRYHEI"     TO  MCP-WIDGET
           WHEN    05
               MOVE    "CHKSRYRANGE"   TO  MCP-WIDGET
           WHEN    06
               MOVE    "STSRYCD"       TO  MCP-WIDGET
           WHEN    07
               MOVE    "EDSRYCD"       TO  MCP-WIDGET
           WHEN    08
               MOVE    "SORT"          TO  MCP-WIDGET
           WHEN    11
               MOVE    "CHKYKZKNKBYO"  TO  MCP-WIDGET
      *----(03.04.01)--ADD-START---
           WHEN    12
               MOVE    "CHKSRYSNT"     TO  MCP-WIDGET
           WHEN    13
               MOVE    "CHKBYOYKZ"     TO  MCP-WIDGET
           WHEN    14
               MOVE    "CHKBYOSRY"     TO  MCP-WIDGET
      *----(03.04.01)--ADD-END-----
           WHEN    15
               MOVE    "MOREKIKAN"     TO  MCP-WIDGET
           WHEN    16
               MOVE    "CHKMOREYKZBYO" TO  MCP-WIDGET
           WHEN    17
               MOVE    "CHKMOREKENSA"  TO  MCP-WIDGET
           WHEN    18
               MOVE    "CHKMOREBUI"    TO  MCP-WIDGET
           WHEN    19
               MOVE    "MORESORT"      TO  MCP-WIDGET
           WHEN    20
               MOVE    "CHKSRYHEIA"    TO  MCP-WIDGET
           WHEN    21
               MOVE    "CHKSRYHEIB"    TO  MCP-WIDGET
           WHEN    22
               MOVE    "CHKKAISU"      TO  MCP-WIDGET
           WHEN    23
               MOVE    "CHKTEKBYO"     TO  MCP-WIDGET
           WHEN    101
               MOVE    "KJNYMD"        TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"           TO  MCP-WIDGET
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
           WHEN    "CHKYKZBYO"
               MOVE    02              TO  SPA-GMN-CUR
           WHEN    "CHKSRYBYO"
               MOVE    03              TO  SPA-GMN-CUR
           WHEN    "CHKYKZHEI"
               MOVE    04              TO  SPA-GMN-CUR
      *----(03.04.01)--UPD-START---
           WHEN    "CHKSRYHEI"
               MOVE    11              TO  SPA-GMN-CUR
           WHEN    "CHKYKZKNKBYO"
               MOVE    12              TO  SPA-GMN-CUR
           WHEN    "CHKSRYSNT"
               MOVE    20              TO  SPA-GMN-CUR
           WHEN    "CHKSRYHEIA"
               MOVE    21              TO  SPA-GMN-CUR
           WHEN    "CHKSRYHEIB"
               MOVE    05              TO  SPA-GMN-CUR
           WHEN    "CHKSRYRANGE"
               MOVE    06              TO  SPA-GMN-CUR
           WHEN    "STSRYCD"
               MOVE    07              TO  SPA-GMN-CUR
           WHEN    "EDSRYCD"
               MOVE    08              TO  SPA-GMN-CUR
           WHEN    "SORT"
               MOVE    13              TO  SPA-GMN-CUR
           WHEN    "CHKBYOYKZ"
               MOVE    14              TO  SPA-GMN-CUR
           WHEN    "CHKBYOSRY"
               MOVE    23              TO  SPA-GMN-CUR
           WHEN    "CHKTEKBYO"
               MOVE    22              TO  SPA-GMN-CUR
           WHEN    "CHKKAISU"
               MOVE    15              TO  SPA-GMN-CUR
      *----(03.04.01)--UPD-END-----
           WHEN    "MOREKIKAN"
               MOVE    16              TO  SPA-GMN-CUR
           WHEN    "CHKMOREYKZBYO"
               MOVE    17              TO  SPA-GMN-CUR
           WHEN    "CHKMOREKENSA"
               MOVE    18              TO  SPA-GMN-CUR
           WHEN    "CHKMOREBUI"
               MOVE    19              TO  SPA-GMN-CUR
           WHEN    "KJNYMD"
               MOVE    1               TO  SPA-GMN-CUR
           WHEN    "MORESORT"
               MOVE    212             TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       50012-CUR-SKIP-SEC              SECTION.
      *
           IF    ( SPA-GMN-CUR         =   06 )
               IF   ( X91-STSRYCD-STATE    =   WIDGET-INSENSITIVE )
                   MOVE    08           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       50012-CUR-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC                  SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "開始診療行為コード入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "終了診療行為コード入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "基準日に正しい日付を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0100"
               MOVE    "印刷するチェック項目を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "0101"
               MOVE    "開始診療行為コードを９桁で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0102"
               MOVE    "開始診療行為コードが未入力です"
                                       TO  SPA-ERRMSG
           WHEN    "0103"
               MOVE    "終了診療行為コードを９桁で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0104"
               MOVE    "診療行為コードの範囲指定に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0105"
               MOVE    "点数マスタに診療行為コードが登録されていません"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "ジョブ管理ＤＢが削除できませんでした"
                                       TO  SPA-ERRMSG
           WHEN    "1005"
               MOVE
                   "システム管理ＤＢ（３００３）削除できませんでした"
                                       TO  SPA-ERRMSG
           WHEN    "7777"
               MOVE    WRK-SHORIERR    TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  XERR
           MOVE    SPA-ERRCD           TO  XERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  XERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "X91"               TO  SPA-MOTOPG
           MOVE    "XERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-XIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-XIDMSG
      *
           EVALUATE    SPA-XIDCD
           WHEN    "6666"
               MOVE    "印刷処理中です"
                                       TO  WRK-XIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  XID1
           INITIALIZE                      XID1
           MOVE    SPA-XIDCD           TO  XID1-ID1CODE
           MOVE    "戻る"              TO  XID1-B01
           MOVE    "ＯＫ"              TO  XID1-B12
      *
           MOVE    WRK-XIDMSG          TO  XID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "X91"               TO  SPA-MOTOPG
           MOVE    "XID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-XIDSET-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       540-XID2SET-SEC                 SECTION.
      *
           MOVE    SPACE               TO  WRK-XIDMSG
      *
           EVALUATE    SPA-XIDCD
           WHEN    "1001"
               MOVE    "設定内容に従い帳票を印刷します"
                                       TO  WRK-XIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  XID2
           INITIALIZE                      XID2
           MOVE    SPA-XIDCD           TO  XID2-ID1CODE
           MOVE    WRK-XIDMSG          TO  XID2-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "X91"               TO  SPA-MOTOPG
           MOVE    "XID2"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XID2"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       540-XID2SET-EXT.
           EXIT.
      **************************************************************
      *    日付チェック処理
      **************************************************************
       800-HIZUKE-CHK-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-EDTYMD1
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-SYMD            TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-EDTYMD1
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           END-IF
      *
           .
       800-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理制御サブ呼び出し
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           .
       800-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    文字列長チェック（先頭から半角空白があらわれるまで）
      *****************************************************************
       800-LENGTH-CHK-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-STR-OUT
           MOVE    1               TO  WRK-STR-LEN
      *
           STRING  WRK-STR-IN      DELIMITED   BY  SPACE
           INTO    WRK-STR-OUT
           WITH    POINTER         WRK-STR-LEN
           END-STRING
      *
           COMPUTE WRK-STR-LEN     =   WRK-STR-LEN     -   1
      *
           .
       800-LENGTH-CHK-EXT.
           EXIT.
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC              =   ZERO )
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF    ( MCP-RC          =   ZERO )
                   MOVE    ZERO        TO  FLG-SYSKANRI
               ELSE
                   MOVE    1           TO  FLG-SYSKANRI
               END-IF
           ELSE
                   MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
