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
       PROGRAM-ID.             ORCGR97.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : レセプト作成-主科まとめ入力
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/02/20    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *
      *  03.02.01    NACL-藤原    06/02/20  国保主科対応
      *
      *  03.04.01    NACL-藤原    06/12/27  主科設定情報（入院）が２以外
      *                                     で科が設定できない場合の対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.01.01    NACL-藤原    07/01/21  月途中の負担者番号変更対応
      *                                     （生保の公費単独のみ） 
      *
      *  04.02.01    NACL-藤原    08/03/04  平成２０年４月改正対応
      *  04.02.02    NACL-藤原    08/03/25  広域連合追加
      *
      *  04.03.01    NACL-藤原    08/08/22  公費４種までの併用の記録対応
      *
      *  04.05.01    NACL-藤原    10/01/13  治験対応
      *  04.05.02    NACL-藤原    11/11/29  医保以外を対象外とする
      *
      *  04.06.01    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *
      *  04.07.01    NACL-藤原    15/06/19  診療科表示件数の変更
      *
      *  04.08.01    NACL-藤原    16/08/08  災害該当分（非減免対象と減免対象）
      *                                     レセプトの分割対応
      *  04.08.02    NACL-藤原    18/03/28  平成３０年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    レセプト作成指示共通パラメタ
           COPY    "R01COMMON-SPA".
      *
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
       01  SPA-R97.
           03  SPA-R97-AREA.
               05  SPA-GMN-MAX                     PIC 9(04).
               05  SPA-GMN-PAGE                    PIC 9(03).
               05  SPA-GMN-CUR                     PIC 9(02).
               05  SPA-GMN-NEXT                    PIC 9(01).
               05  SPA-GMN-AREA.
                   07  SPA-GMN-SRYYM               PIC X(07).
      *                         
                   07  SPA-GMN-NYUGAIKBNLST        OCCURS  2.
                       09  SPA-GMN-NYUGAIKBNL      PIC X(01).
                       09  SPA-GMN-F1L             PIC X(01).
                       09  SPA-GMN-NYUGAIKBNMEIL   PIC X(06).    
                   07  SPA-GMN-NYUGAIKBN-MAX       PIC 9(02).
      *             
                   07  SPA-GMN-NYUGAIKBN-G.
                       09  SPA-GMN-NYUGAIKBN       PIC X(01).
                       09  SPA-GMN-F1              PIC X(01).
                       09  SPA-GMN-NYUGAIKBNMEI    PIC X(06).
      *
                   07  SPA-GMN-HTEISYUTUSAKILST-G.                         
                       09  SPA-GMN-HTEISYUTUSAKILST
                                                   OCCURS  4.
                           11  SPA-GMN-HTEISYUTUSAKIL
                                                   PIC X(01).
                           11  SPA-GMN-F1L         PIC X(01).
                           11  SPA-GMN-HTEISYUTUSAKIMEIL
                                                   PIC X(08).    
                   07  SPA-GMN-HTEISYUTUSAKI-MAX   PIC 9(02).
      *             
                   07  SPA-GMN-HTEISYUTUSAKI-G.
                       09  SPA-GMN-HTEISYUTUSAKI   PIC X(01).
                       09  SPA-GMN-F1              PIC X(01).
                       09  SPA-GMN-HTEISYUTUSAKIMEI
                                                   PIC X(08).
      *                         
                   07  SPA-GMN-HYOUJIKBNLST        OCCURS  2.
                       09  SPA-GMN-HYOUJIKBNL      PIC X(01).
                       09  SPA-GMN-F1L             PIC X(01).
                       09  SPA-GMN-HYOUJIKBNMEIL   PIC X(08).    
                   07  SPA-GMN-HYOUJIKBN-MAX       PIC 9(02).
      *             
                   07  SPA-GMN-HYOUJIKBN-G.
                       09  SPA-GMN-HYOUJIKBN       PIC X(01).
                       09  SPA-GMN-F1              PIC X(01).
                       09  SPA-GMN-HYOUJIKBNMEI    PIC X(08).
      *
                   07  SPA-GMN-SYUKALST            OCCURS  100.
                       09  SPA-GMN-SYUKAL          PIC X(02).
                       09  SPA-GMN-C02-F1L         PIC X(01).
                       09  SPA-GMN-SYUKAMEIL       PIC X(20).
                   07  SPA-GMN-SYUKA-MAX           PIC 9(03).
      *
                   07  SPA-GMN-SYUKA-TABLE.
                       09  SPA-GMN-SYUKA-TBL       OCCURS  500.
                           11  SPA-GMN-TRENNUM     PIC 9(05).
                           11  SPA-GMN-TTSTPTNUMKBN
                                                   PIC X(02).
                           11  SPA-GMN-TPTNUM      PIC X(20).
                           11  SPA-GMN-TNAME       PIC X(30).
                           11  SPA-GMN-TTEISYUTUSAKI
                                                   PIC X(08).
                           11  SPA-GMN-TRECESYUBETU
                                                   PIC X(10).
                           11  SPA-GMN-THKNJANUM   PIC X(08).
                           11  SPA-GMN-THOJOKBN    PIC X(20).
                           11  SPA-GMN-TRECEIPT-KBN
                                                   PIC X(04).
                           11  SPA-GMN-TDISASTER   PIC X(02).
                           11  SPA-GMN-TSYUKA-AUTO PIC X(06).
                           11  SPA-GMN-TSYUKA-INPUT
                                                   PIC X(06).
                       09  SPA-GMN-SYUKA-TBL-MAX   PIC 9(03).
      *
                   07  SPA-GMN-PTNUM-TABLE.
                       09  SPA-GMN-PTNUM-TBL       OCCURS   100.
                           11  SPA-GMN-PTNUM-TSRYKA
                                                   PIC X(02).
                           11  SPA-GMN-PTNUM-TSRYKAMEI
                                                   PIC X(12).
                           11  SPA-GMN-PTNUM-TTENSU
                                                   PIC S9(10).
                       09  SPA-GMN-PTNUM-TBL-MAX   PIC 9(03).
      *                                             
                   07  SPA-GMN-KOUMOKU.
                       09  SPA-GMN-SELNUM          PIC 9(05).
                       09  SPA-GMN-PTNUM           PIC X(20).
                       09  SPA-GMN-NAME            PIC X(30).
                       09  SPA-GMN-TEISYUTUSAKI    PIC X(08).
                       09  SPA-GMN-RECESYUBETU     PIC X(20).
                       09  SPA-GMN-HKNJANUM        PIC X(08).
                       09  SPA-GMN-HKNJAMEI        PIC X(20).
                       09  SPA-GMN-HOJOKBN         PIC X(20).
                       09  SPA-GMN-RECEIPT-KBN     PIC X(04).
                       09  SPA-GMN-SYUKA-G.
                           11  SPA-GMN-SYUKA       PIC X(02).
                           11  SPA-GMN-F1          PIC X(01).
                           11  SPA-GMN-SYUKAMEI    PIC X(20).
                       09  SPA-GMN-SRYKA-AUTO-G.
                           11  SPA-GMN-SRYKA-AUTO  PIC X(02).
                           11  SPA-GMN-F1          PIC X(01).
                           11  SPA-GMN-SRYKAMEI-AUTO
                                                   PIC X(12).
                       09  SPA-GMN-SRYKA-INPUT-G.
                           11  SPA-GMN-SRYKA-INPUT PIC X(02).
                           11  SPA-GMN-F1          PIC X(01).
                           11  SPA-GMN-SRYKAMEI-INPUT
                                                   PIC X(12).
                       09  SPA-GMN-SRYKA-INPUT1-G.
                           11  SPA-GMN-SRYKA-INPUT1
                                                   PIC X(02).
                           11  SPA-GMN-F1          PIC X(01).
                           11  SPA-GMN-SRYKAMEI-INPUT1
                                                   PIC X(12).
      *
               05  SPA-NAI-AREA.
                   07  SPA-NAI-SYUKALST            OCCURS  100.
                       09  SPA-NAI-SYUKAMEIL       PIC X(12).
      *
                   07  SPA-NAI-SRYYM               PIC 9(06).
                   07  SPA-NAI-SRYYMH              PIC X(16).
                   07  SPA-NAI-NYUGAIKBN           PIC X(06).
      *
                   07  SPA-NAI-SRYYM-OLD           PIC 9(06).
                   07  SPA-NAI-NYUGAIKBN-OLD       PIC X(01).
                   07  SPA-NAI-HTEISYUTUSAKI-OLD   PIC X(01).
                   07  SPA-NAI-HYOUJIKBN-OLD       PIC X(01).
      *
                   07  SPA-NAI-KOUMOKU. 
                       09  SPA-NAI-PTID            PIC 9(10).
                       09  SPA-NAI-HOJOKBN         PIC X(01).
                       09  SPA-NAI-KOHID           PIC 9(10).
                       09  SPA-NAI-RECEIPT-KBN     PIC X(10).
                       09  SPA-NAI-HKNJANUM        PIC X(08).
                       09  SPA-NAI-TEISYUTUSAKI    PIC 9(01).
                       09  SPA-NAI-RECESYUBETU     PIC X(04).
                       09  SPA-NAI-SELNUM          PIC 9(05).
                       09  SPA-NAI-SELNUM1         PIC 9(05).
      *
                   07  SPA-NAI-SYUKA-TABLE.
                       09  SPA-NAI-SYUKA-TBL       OCCURS  500.
                           11  SPA-NAI-TPTID       PIC 9(10).
                           11  SPA-NAI-TPTNUM      PIC X(20).
                           11  SPA-NAI-TNAME       PIC X(30).
                           11  SPA-NAI-TTEISYUTUSAKI
                                                   PIC 9(01).
                           11  SPA-NAI-THOJOKBN    PIC X(01).
                           11  SPA-NAI-TKOHID      PIC 9(10).
                           11  SPA-NAI-TRECESYUBETU
                                                   PIC X(04).
                           11  SPA-NAI-THKNJANUM   PIC X(08).
                           11  SPA-NAI-TRECEIPT-KBN
                                                   PIC X(10).
      *
                   07  SPA-NAI-PAGE-TABLE.
                       09  SPA-NAI-PAGE-TBL        OCCURS  100.
                           11  SPA-NAI-PAGE-TPTNUM PIC X(20).
      *        入力項目名
               05  SPA-MCP-WIDGET                  PIC X(64).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYUKA           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
      *     
           03  FLG-SYUKA-ROW       PIC 9(01).
           03  FLG-PTNUM-ROW       PIC 9(01).
           03  FLG-DSP             PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
      *
           03  WRK-PTID            PIC 9(10).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(05).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDXY                PIC 9(04).
           03  IDXZ                PIC 9(04).
      *
           03  IDX1                PIC 9(04). 
           03  IDX2                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-SRYKA-HENSYU-AREA.
               05  WRK-SRYKA       PIC X(02).
               05  WRK-SRYKAMEI    PIC X(20).
               05  WRK-SRYKAMEITAN PIC X(12).
           03  SRYKA-IDX           PIC 9(04).     
      *
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-GMN-CUR         PIC 9(02).
      *
           03  WRK-BANGO           PIC S9(05).
           03  WRK-NUM             PIC ZZZZ9.
           03  WRK-TENSUZ          PIC ---------9.
           03  WRK-SELNUM          PIC 9(03).
           03  WRK-LINE            PIC 9(04).
           03  WRK-PAGE            PIC 9(05).
           03  WRK-RENNUM          PIC 9(04).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG1        PIC X(09).
           03  WRK-HENYMDG3        PIC X(22).
      *
           03  WRK-SYUKA-ROW       PIC S9(9)   BINARY.
           03  WRK-PTNUM-ROW       PIC S9(9)   BINARY.
      *
           03  WRK-RECEERR         PIC X(200).
           03  WRK-RIDMSG          PIC X(70).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
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
      *    主科（退避用）
       01  WRK-SYUKA-REC.
           COPY    "CPSYUKA.INC"   REPLACING  //SYUKA-//
                                   BY         //WRK-SYUKA-//.
       01  WRK-FLG-SYUKA           PIC 9(01). 
      *
       01  WRK-ORCSSYUACCAREA.
           COPY    "CPORCSSYUACC.INC"
                                   REPLACING  //LNK-SYUACC//
                                   BY         //WRK-LNK-SYUACC//.
      *                             
       01  WRK-SHELL-NAME.
           05  FILLER             PIC X(16) VALUE
                                             "scripts/monthly/".
           05  WRK-SHELL-SHELL    PIC X(20).         
      *
       01  WRK-CONS-AREA.
      *    
           03  WRK-CONS-LINE-SYUKA-MAX
                                   PIC 9(03)   VALUE   500.
           03  WRK-CONS-LINE-PTNUM-MAX
                                   PIC 9(03)   VALUE   100.
      *
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "RECEPT1".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   1.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSK1005.INC".
      *
      *    レセプト印刷情報（主科設定）
           COPY  "CPSK2009.INC".
      *
      *    主科
       01  SYUKA-REC.
           COPY    "CPSYUKA.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険者
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
       01  ORCSSYUACCAREA.
           COPY    "CPORCSSYUACC.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *     
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "CPSHELLTBL.INC".
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
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-R97
           MOVE    SPA-WORK        TO  SPA-R01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"         ALSO   "CLICKED"
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
           MOVE    SPA-R01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-R97         TO  SPA-FREE
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
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "R97    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
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
      *        元の画面へ戻す
               IF      SPA-MOTOPG      =   "XB01"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-R97
                   MOVE    SPA-WORK        TO  SPA-R01KYOUTU
                   GO  TO  300-SCREEN-EXT
               END-IF
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "RID1"
                   PERFORM 330-RID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "R99"
      *            検索処理
                   MOVE    1                   TO  SPA-GMN-PAGE
                   INITIALIZE                      SPA-NAI-PAGE-TABLE 
                   PERFORM 3101-SYUKA-HENSYU-SEC     
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
           INITIALIZE              SPA-R97-AREA
      *
      *    対象診療年月
           IF      SPA-SYSYMD(7:2)     >   "10"
               MOVE    SPA-SYSYMD(1:6)     TO  SPA-NAI-SRYYM
           ELSE
               INITIALIZE                     STS-AREA-DAY
               INITIALIZE                     LNK-DAY6-AREA
               MOVE  "61"                     TO  LNK-DAY6-IRAI
               MOVE  SPA-SYSYMD               TO  LNK-DAY6-KIJUN
               MOVE  "2"                      TO  LNK-DAY6-ZOGENPTN
               MOVE  -1                       TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"                USING   STS-AREA-DAY
                                              LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN(1:6)   TO  SPA-NAI-SRYYM
           END-IF
           MOVE    SPA-NAI-SRYYM       TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2) 
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG1(1:6)   TO  SPA-GMN-SRYYM
           MOVE    WRK-HENYMDG3(1:16)  TO  SPA-NAI-SRYYMH
      *
      *    入外区分
           IF      SPA-BEDSU       >   ZERO
               MOVE    "1"             TO  SPA-GMN-NYUGAIKBNL       (1)
               MOVE    "入院"          TO  SPA-GMN-NYUGAIKBNMEIL    (1)
               MOVE    "2"             TO  SPA-GMN-NYUGAIKBNL       (2)
               MOVE    "入院外"        TO  SPA-GMN-NYUGAIKBNMEIL    (2)
               MOVE    2               TO  SPA-GMN-NYUGAIKBN-MAX
           ELSE
               MOVE    "2"             TO  SPA-GMN-NYUGAIKBNL       (1)
               MOVE    "入院外"        TO  SPA-GMN-NYUGAIKBNMEIL    (1)
               MOVE    1               TO  SPA-GMN-NYUGAIKBN-MAX
           END-IF
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-NYUGAIKBN-MAX
               IF      SPA-R01-NYUGAIKBN 
                               =   SPA-GMN-NYUGAIKBNL (IDX)
                   MOVE    SPA-GMN-NYUGAIKBNLST (IDX)
                                       TO  SPA-GMN-NYUGAIKBN-G
                   MOVE    SPA-GMN-NYUGAIKBN-MAX
                                       TO  IDX
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-NYUGAIKBNMEI
                                       TO  SPA-NAI-NYUGAIKBN
      *
           MOVE    "1"                 TO  SPA-GMN-HYOUJIKBNL       (1)
           MOVE    "全体"              TO  SPA-GMN-HYOUJIKBNMEIL    (1)
           MOVE    "2"                 TO  SPA-GMN-HYOUJIKBNL       (2)
           MOVE    "未入力分"          TO  SPA-GMN-HYOUJIKBNMEIL    (2)
           MOVE    2                   TO  SPA-GMN-HYOUJIKBN-MAX
           MOVE    SPA-GMN-HYOUJIKBNLST (1)
                                       TO  SPA-GMN-HYOUJIKBN-G
      *
      *    診療科
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-NAI-SRYYM       TO  SYS-1005-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-1005-STYUKYMD (7:2)
           MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF 
      *
           MOVE    ZERO                TO  IDX
           PERFORM         UNTIL     ( IDX            >=  99  )
                           OR        ( FLG-SYSKANRI   =   1   )
               ADD     1                     TO  IDX
               MOVE    MCPDATA-REC           TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD (1:2)  TO
                                             SPA-GMN-SYUKAL   (IDX)
               MOVE    SYS-1005-SRYKANAME1   TO
                                             SPA-GMN-SYUKAMEIL(IDX)
               MOVE    SYS-1005-SRYKANAME2   TO
                                             SPA-NAI-SYUKAMEIL(IDX)
      *
               MOVE    IDX                   TO  SPA-GMN-SYUKA-MAX
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
      *    提出先
           MOVE    SPACE               TO  SYS-2009-REC
           INITIALIZE                      SYS-2009-REC
           MOVE    "2009"              TO  SYS-2009-KANRICD
           MOVE    "*"                 TO  SYS-2009-KBNCD
           MOVE    SPA-NAI-SRYYM       TO  SYS-2009-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2009-STYUKYMD (7:2)
           MOVE    SYS-2009-STYUKYMD   TO  SYS-2009-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-2009-HOSPNUM
           MOVE    SYS-2009-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2009-REC
           ELSE
               INITIALIZE                      SYS-2009-REC
           END-IF
      *
           IF    ( SYS-2009-SYUKA-KBN  =   "1"   )
           AND   ( SYS-2009-SYUKA-SYAHO
                                       =   SPACE )
               MOVE    "1"                 TO  SYS-2009-SYUKA-SYAHO
           END-IF      
           IF    ( SYS-2009-SYUKA-KBN  =   "1"   )
           AND   ( SYS-2009-SYUKA-KOKUHO
                                       =   SPACE )
               MOVE    "0"                 TO  SYS-2009-SYUKA-KOKUHO
           END-IF
           IF    ( SYS-2009-SYUKA-KBN  =   "1"   )
           AND   ( SYS-2009-SYUKA-KOUIKI
                                       =   SPACE )
               MOVE    "0"                 TO  SYS-2009-SYUKA-KOUIKI
           END-IF
      *
           IF    ( SYS-2009-SYUKA-SYAHO    =   "0" )
           AND   ( SYS-2009-SYUKA-KOKUHO   =   "0" )
           AND   ( SYS-2009-SYUKA-KOUIKI   =   "0" )
               CONTINUE
           ELSE
               IF    ( SYS-2009-SYUKA-SYAHO    NOT =   "0" )
               AND   ( SYS-2009-SYUKA-KOKUHO   NOT =   "0" )
               AND   ( SYS-2009-SYUKA-KOUIKI   NOT =   "0" )
                   ADD     1       TO  SPA-GMN-HTEISYUTUSAKI-MAX
                   MOVE    "1"     TO  SPA-GMN-HTEISYUTUSAKIL
                                           (SPA-GMN-HTEISYUTUSAKI-MAX)
                   MOVE    "全体"  TO  SPA-GMN-HTEISYUTUSAKIMEIL
                                           (SPA-GMN-HTEISYUTUSAKI-MAX)
               END-IF
               IF      SYS-2009-SYUKA-SYAHO    NOT =   "0"
                   ADD     1       TO  SPA-GMN-HTEISYUTUSAKI-MAX
                   MOVE    "2"     TO  SPA-GMN-HTEISYUTUSAKIL
                                           (SPA-GMN-HTEISYUTUSAKI-MAX)
                   MOVE    "社保"  TO  SPA-GMN-HTEISYUTUSAKIMEIL
                                           (SPA-GMN-HTEISYUTUSAKI-MAX)
               END-IF
               IF      SYS-2009-SYUKA-KOKUHO   NOT =   "0"
                   ADD     1       TO  SPA-GMN-HTEISYUTUSAKI-MAX
                   MOVE    "3"     TO  SPA-GMN-HTEISYUTUSAKIL
                                           (SPA-GMN-HTEISYUTUSAKI-MAX)
                   MOVE    "国保"  TO  SPA-GMN-HTEISYUTUSAKIMEIL
                                           (SPA-GMN-HTEISYUTUSAKI-MAX)
               END-IF
               IF    ( SYS-2009-SYUKA-KOUIKI   NOT =   "0"    )
                   ADD     1       TO  SPA-GMN-HTEISYUTUSAKI-MAX
                   MOVE    "4"     TO  SPA-GMN-HTEISYUTUSAKIL
                                           (SPA-GMN-HTEISYUTUSAKI-MAX)
                   MOVE    "広域"  TO  SPA-GMN-HTEISYUTUSAKIMEIL
                                           (SPA-GMN-HTEISYUTUSAKI-MAX)
               END-IF
           END-IF
           IF      SPA-GMN-HTEISYUTUSAKI-MAX
                                   >   ZERO    
               MOVE    SPA-GMN-HTEISYUTUSAKILST (1)
                                       TO  SPA-GMN-HTEISYUTUSAKI-G
           END-IF
      *
           MOVE    5                   TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    該当診療月の主科情報編集処理
      *****************************************************************
       3101-SYUKA-HENSYU-SEC         SECTION.
      *
           INITIALIZE                  WRK-ORCSSYUACCAREA
      *
      *****MOVE    1               TO  WRK-PAGE
      *****MOVE    ZERO            TO  WRK-RENNUM
      *****                            WRK-LINE
           MOVE    SPA-GMN-PAGE    TO  WRK-PAGE
           COMPUTE WRK-RENNUM  = ( WRK-PAGE -   1 )    *
                                   WRK-CONS-LINE-SYUKA-MAX   
           MOVE    ZERO            TO  WRK-LINE
      *
           INITIALIZE                  SPA-GMN-SYUKA-TABLE
                                       SPA-GMN-PTNUM-TABLE
                                       SPA-NAI-SYUKA-TABLE
                                       SPA-GMN-KOUMOKU
                                       SPA-NAI-KOUMOKU
      *
      *    患者情報画面編集処理
           INITIALIZE                  SYUKA-REC
grpsys     MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
           MOVE    SPA-NAI-SRYYM   TO  SYUKA-SRYYM
           MOVE    SPA-GMN-NYUGAIKBN  
                                   TO  SYUKA-NYUGAIKBN
           EVALUATE    SPA-GMN-HTEISYUTUSAKI
               WHEN    "1"
                   MOVE    "%"             TO  SYUKA-TEISYUTUSAKI
               WHEN    "2"
                   MOVE    "1"             TO  SYUKA-TEISYUTUSAKI
               WHEN    "3"
                   MOVE    "2"             TO  SYUKA-TEISYUTUSAKI
               WHEN    "4"
                   MOVE    "6"             TO  SYUKA-TEISYUTUSAKI
           END-EVALUATE
           MOVE    SPA-NAI-PAGE-TPTNUM (SPA-GMN-PAGE)
                                   TO  SYUKA-PTNUM
           MOVE    SYUKA-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syuka"     TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syuka"     TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 910-SYUKA-KEY10-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-SYUKA
           END-IF
      *
           PERFORM UNTIL   WRK-PAGE    >   SPA-GMN-PAGE
                   OR      FLG-SYUKA   =   1
               MOVE    ZERO            TO  IDW
               MOVE    WRK-ORCSSYUACCAREA
                                       TO  ORCSSYUACCAREA
               MOVE    SYUKA-PTID      TO  WRK-PTID
               MOVE    ZERO            TO  FLG-DSP
      *
               PERFORM UNTIL   FLG-SYUKA   =   1
                       OR      SYUKA-PTID
                                       NOT =   WRK-PTID
      *
                   IF      SPA-GMN-HYOUJIKBN   =   2
                       IF      SYUKA-SRYKA-INPUT   =   SPACE
                           MOVE    1               TO  FLG-DSP
                       END-IF
                   ELSE
                        MOVE    1                  TO  FLG-DSP
                   END-IF
      *
                   ADD   1                 TO  IDW
                   MOVE  SYUKA-REC         TO  LNK-SYUACC-IO2-OCC(IDW)
      *
                   MOVE    "tbl_syuka"     TO  MCP-TABLE
                   MOVE    "key10"         TO  MCP-PATHNAME
                   PERFORM 910-SYUKA-KEY10-NEXT-SEC
               END-PERFORM
      *
               IF      FLG-DSP      =   1
                   MOVE   SYUKA-REC     TO  WRK-SYUKA-REC
                   MOVE    FLG-SYUKA    TO  WRK-FLG-SYUKA
      *
      *            手入力による主科情報の取得
                   INITIALIZE                  SYUKA-REC
grpsys             MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
                   MOVE    SPA-NAI-SRYYM   TO  SYUKA-SRYYM
                   MOVE    WRK-PTID        TO  SYUKA-PTID
                   MOVE    SYUKA-REC       TO  MCPDATA-REC
                   MOVE    "tbl_syuka"     TO  WRK-MCP-TABLE
                   MOVE    "key4"          TO  WRK-MCP-PATHNAME
                   PERFORM 900-SYUKA-READ-SEC
                   IF      FLG-SYUKA       =   ZERO
                       ADD     1               TO  IDW
                       MOVE    SYUKA-REC       TO
                                               LNK-SYUACC-IO2-OCC (IDW)
                   END-IF
                   MOVE    WRK-SYUKA-REC   TO  SYUKA-REC
                   MOVE    WRK-FLG-SYUKA   TO  FLG-SYUKA
      *
                   PERFORM 31011-SYUKA-GMN-HENSYU-SEC
               END-IF 
           END-PERFORM
      *
           MOVE    "tbl_syuka"     TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    次ページの有無
           IF    ( WRK-PAGE            >   SPA-GMN-PAGE )
      *****AND   ( FLG-SYUKA           =   ZERO         )
               INITIALIZE                      ORCSPTIDAREA
grpsys         MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
               MOVE    WRK-PTID            TO  SPTID-PTID
               CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                   SPA-AREA
               MOVE    SPTID-PTNUM         TO  SPA-NAI-PAGE-TPTNUM
                                                           (WRK-PAGE)
               MOVE    1                   TO  SPA-GMN-NEXT
           ELSE
               MOVE    ZERO                TO  SPA-GMN-NEXT
           END-IF
      *
           MOVE    1                   TO  FLG-SYUKA-ROW
      *
      *    該当のレセプトがないとき
           IF      SPA-GMN-SYUKA-TBL-MAX   =   ZERO
               MOVE    98              TO  SPA-GMN-CUR
           ELSE         
               MOVE    1               TO  SPA-GMN-CUR
           END-IF    
      *
           .
       3101-SYUKA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科情報画面編集処理
      *****************************************************************
       31011-SYUKA-GMN-HENSYU-SEC         SECTION.
      *
           MOVE    "07"            TO  LNK-SYUACC-I-KBN
           MOVE    SPA-NAI-SRYYM   TO  LNK-SYUACC-I-SRYYM
           MOVE    SPA-GMN-NYUGAIKBN
                                   TO  SPA-NYUGAIKBN
           MOVE    WRK-PTID        TO  SPA-PTID
           CALL    "ORCSSYUKAMAIN" USING
                                       SPA-AREA
                                       ORCSSYUACCAREA
           IF      LNK-SYUACC-RC   NOT = ZERO
                   GO  TO  31011-SYUKA-GMN-HENSYU-EXT
           END-IF       
           IF      SPA-GMN-HYOUJIKBN   =   2
               IF  ( LNK-SYUACC-IO-SYUKA-AUTO (1)
                                       NOT =  SPACE )
               OR  ( LNK-SYUACC-IO-H-SRYKA
                                       NOT =  SPACE )
                   GO  TO  31011-SYUKA-GMN-HENSYU-EXT
               END-IF
           END-IF
      *
      *    連番号と表示行の仮計算
           MOVE    WRK-LINE        TO  IDXX
           MOVE    WRK-RENNUM      TO  IDXY
           MOVE    ZERO            TO  IDXZ
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       100
                   OR      LNK-SYUACC-IO-RECESYUBETU (IDX1)    = SPACE
               ADD     1               TO  IDXX
                                           IDXY
                                           IDXZ
           END-PERFORM
      *
      *    該当頁に同一患者の情報を表示できるか
           IF      IDXX            >   WRK-CONS-LINE-SYUKA-MAX
               ADD     1               TO  WRK-PAGE
               MOVE    ZERO            TO  WRK-LINE
           END-IF
           IF      WRK-PAGE        <   SPA-GMN-PAGE
               ADD     IDXZ            TO  WRK-LINE
                                           WRK-RENNUM
           END-IF
      *
      *    該当頁に表示するデータか
           IF      WRK-PAGE    NOT =   SPA-GMN-PAGE
               GO  TO  31011-SYUKA-GMN-HENSYU-EXT
           END-IF
      *
      *    主科情報表示
           INITIALIZE                  ORCSPTIDAREA
grpsys     MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
           MOVE    WRK-PTID        TO  SPTID-PTID
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-AREA
      *
grpsys     MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    WRK-PTID        TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       100
                   OR      LNK-SYUACC-IO-RECESYUBETU (IDX1)    = SPACE
               ADD     1               TO  WRK-RENNUM
               ADD     1               TO  WRK-LINE
      *
               MOVE    WRK-RENNUM      TO  SPA-GMN-TRENNUM    (WRK-LINE)
               IF      PTINF-TSTPTNUMKBN   =   "0"
                   CONTINUE
               ELSE
                   MOVE    "◎"        TO  SPA-GMN-TTSTPTNUMKBN
                                                              (WRK-LINE)
               END-IF
               MOVE    WRK-PTID        TO  SPA-NAI-TPTID      (WRK-LINE)
               MOVE    SPTID-PTNUM     TO  SPA-NAI-TPTNUM     (WRK-LINE)
               MOVE    PTINF-NAME      TO  SPA-NAI-TNAME      (WRK-LINE)
               IF      IDX1            =   1
                   MOVE    SPTID-PTNUM     TO  SPA-GMN-TPTNUM (WRK-LINE)
                   MOVE    PTINF-NAME      TO  SPA-GMN-TNAME  (WRK-LINE)
               END-IF
      *
               MOVE    LNK-SYUACC-IO-TEISYUTUSAKI(IDX1)
                                       TO  SPA-NAI-TTEISYUTUSAKI
                                                              (WRK-LINE)
               EVALUATE    LNK-SYUACC-IO-TEISYUTUSAKI(IDX1)
                   WHEN    "1"
                       MOVE    "社保"  TO  SPA-GMN-TTEISYUTUSAKI
                                                              (WRK-LINE)
                   WHEN    "2"
                       MOVE    "国保"  TO  SPA-GMN-TTEISYUTUSAKI
                                                              (WRK-LINE)
                   WHEN    "6"
                       MOVE    "広域"  TO  SPA-GMN-TTEISYUTUSAKI
                                                              (WRK-LINE)
               END-EVALUATE
               MOVE    LNK-SYUACC-IO-RECESYUBETU(IDX1)
                                       TO  SPA-NAI-TRECESYUBETU
                                                              (WRK-LINE)
               EVALUATE    LNK-SYUACC-IO-RECESYUBETU(IDX1)
                   WHEN    "0000"
                       MOVE    "不明分"    TO  SPA-GMN-TRECESYUBETU
                                                              (WRK-LINE)
                   WHEN    "6666"
                       MOVE    "特別療養費"
                                           TO  SPA-GMN-TRECESYUBETU
                                                              (WRK-LINE)
                   WHEN    OTHER
                       MOVE    LNK-SYUACC-IO-RECESYUBETU(IDX1)
                                       TO  SPA-GMN-TRECESYUBETU
                                                              (WRK-LINE)
                       EVALUATE    LNK-SYUACC-IO-RECESYUBETU(IDX1)(3:1)
                           WHEN    "4"
                           WHEN    "5"
                               MOVE    "3"             TO
                                   SPA-GMN-TRECESYUBETU (WRK-LINE)(3:1)
                       END-EVALUATE
               END-EVALUATE
               MOVE    LNK-SYUACC-IO-HKNJANUM   (IDX1)
                                       TO  SPA-NAI-THKNJANUM  (WRK-LINE)
               IF      LNK-SYUACC-IO-HKNNUM (IDX1) =   SPACE
      *            精神入院か生保単独複数のとき
                   CONTINUE
               ELSE
                   MOVE    LNK-SYUACC-IO-HKNJANUM   (IDX1)
                                       TO  SPA-GMN-THKNJANUM  (WRK-LINE)
               END-IF 
               EVALUATE    LNK-SYUACC-IO-HKNNUM (IDX1) 
                   WHEN    "002"
                   WHEN    "031"  THRU  "034"
                       EVALUATE    LNK-SYUACC-IO-HOJOKBN    (IDX1)
                           WHEN    "1"
                           WHEN    "A"
                           WHEN    "D"
                           WHEN    "G"
                               MOVE    "職務上" 
                                       TO  SPA-GMN-THOJOKBN   (WRK-LINE)
                           WHEN    "2"
                           WHEN    "B"
                           WHEN    "E"
                           WHEN    "H"
                               MOVE    "下船後３ヶ月以内"
                                       TO  SPA-GMN-THOJOKBN   (WRK-LINE)
                           WHEN    "3"
                           WHEN    "C"
                           WHEN    "F"
                           WHEN    "I"
                               MOVE    "通勤災害"
                                       TO  SPA-GMN-THOJOKBN   (WRK-LINE)
                   END-EVALUATE
               END-EVALUATE
               MOVE    LNK-SYUACC-IO-HOJOKBN    (IDX1)
                                       TO  SPA-NAI-THOJOKBN   (WRK-LINE)
               MOVE    LNK-SYUACC-IO-KOHID      (IDX1)
                                       TO  SPA-NAI-TKOHID     (WRK-LINE)
               MOVE    LNK-SYUACC-IO-RECEIPT-KBN(IDX1)
                                       TO  SPA-NAI-TRECEIPT-KBN
                                                              (WRK-LINE)
               EVALUATE    LNK-SYUACC-IO-RECEIPT-KBN(IDX1) (1:1)
                   WHEN    "1"
                       MOVE    "老併"  TO  SPA-GMN-TRECEIPT-KBN
                                                              (WRK-LINE)
                   WHEN    "2"
                       MOVE    "老健"  TO  SPA-GMN-TRECEIPT-KBN
                                                              (WRK-LINE)
                   WHEN    "3"
                       MOVE    "医併"  TO  SPA-GMN-TRECEIPT-KBN
                                                              (WRK-LINE)
                   WHEN    "4"
                       MOVE    "医療"  TO  SPA-GMN-TRECEIPT-KBN
                                                              (WRK-LINE)
               END-EVALUATE
               EVALUATE    LNK-SYUACC-IO-RECEIPT-KBN(IDX1) (5:1)
                   WHEN    "1"
                       MOVE    "災"    TO  SPA-GMN-TDISASTER  (WRK-LINE)
               END-EVALUATE
               MOVE    LNK-SYUACC-IO-SYUKA-AUTO (IDX1)
                                       TO  SPA-GMN-TSYUKA-AUTO(WRK-LINE)
               IF      LNK-SYUACC-IO-H-SRYKA
                                   NOT =   SPACE
                   MOVE    LNK-SYUACC-IO-H-SRYKA
                                       TO  SPA-GMN-TSYUKA-INPUT
                                                              (WRK-LINE)
               ELSE    
                   MOVE    LNK-SYUACC-IO-SYUKA-INPUT(IDX1)
                                       TO  SPA-GMN-TSYUKA-INPUT
                                                              (WRK-LINE)
               END-IF
           END-PERFORM
      *
           MOVE    WRK-LINE            TO  SPA-GMN-SYUKA-TBL-MAX
      *
           .
       31011-SYUKA-GMN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面ＯＫ処理
      *****************************************************************
       330-RID1-SET-SEC            SECTION.
      *
           IF      SPA-RID1-FLG    =   "OK"
               EVALUATE    SPA-RIDCD
                   WHEN    "1001"
                       PERFORM 4104-KOUSIN-SEC
                   WHEN    "1002"
                       PERFORM 450-KOUSHIN-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-RID1-FLG
           MOVE    SPACE           TO  SPA-RIDCD      
           .
       330-RID1-SET-EXT.
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
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 420-CLEAR-SEC
      *        前月分 
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 450-BACK-MONTH-SEC
      *        前頁 
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-BACK-PAGE-SEC
      *        次頁 
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 470-NEXT-PAGE-SEC
      *        次月分 
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 480-NEXT-MONTH-SEC
      *        検索 
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 490-KENSAKU-SEC
      *        情報削除
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-JYOUHOU-DEL-SEC
      *        一括取得
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-TOUROKU-MAE-SEC
      *    処理確認
               WHEN    "CLICKED"       ALSO    "B13"
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
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 41020-SELNUM-CHK-SEC
      *
               WHEN    "ACTIVATE"      ALSO    "PTNUM"
                   PERFORM 41021-PTNUM-CHK-SEC
      *
               WHEN    "ACTIVATE"      ALSO    "SYUKA"
                   PERFORM 41022-SYUKA-CHK-SEC
      *        主科一覧
               WHEN    ANY             ALSO    "SYUKALST"
                   PERFORM 400-SYUKALST-CHK-SEC
      *        患者レセプト一覧
               WHEN    ANY             ALSO    "PTNUMLST"
                   PERFORM 400-PTNUMLST-CHK-SEC
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
      *
           IF      SPA-ERRCD       NOT =   SPACE   AND "0103"
               INITIALIZE                  SPA-GMN-SYUKA-TABLE
                                           SPA-GMN-PTNUM-TABLE
                                           SPA-NAI-SYUKA-TABLE
                                           SPA-GMN-KOUMOKU
                                           SPA-NAI-KOUMOKU
           ELSE
               IF    ( SPA-NAI-SRYYM
                                   NOT =   SPA-NAI-SRYYM-OLD         )
               OR    ( SPA-GMN-HYOUJIKBN
                                   NOT =   SPA-NAI-HYOUJIKBN-OLD     )
               OR    ( SPA-GMN-NYUGAIKBN
                                   NOT =   SPA-NAI-NYUGAIKBN-OLD     )
               OR    ( SPA-GMN-HTEISYUTUSAKI
                                   NOT =   SPA-NAI-HTEISYUTUSAKI-OLD )
                   INITIALIZE                  SPA-GMN-SYUKA-TABLE
                                               SPA-GMN-PTNUM-TABLE
                                               SPA-NAI-SYUKA-TABLE
                                               SPA-GMN-KOUMOKU
                                               SPA-NAI-KOUMOKU
               END-IF
           END-IF
           .
       400-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *   入力チェック
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "SRYYM"
               WHEN    "ACTIVATE"      ALSO    "NYUGAIKBN"
               WHEN    "ACTIVATE"      ALSO    "HTEISYUTUSAKI"
               WHEN    "ACTIVATE"      ALSO    "HYOUJIKBN"
                   IF      SPA-NAI-PTID        >   ZERO
                       MOVE    "0103"              TO  SPA-ERRCD
                       MOVE    3                   TO  SPA-GMN-CUR 
                       GO  TO  410-INPUT-CHK-EXT
                   END-IF
           END-EVALUATE
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
           MOVE    SPA-NAI-SRYYM       TO  SPA-NAI-SRYYM-OLD
           MOVE    SPA-GMN-NYUGAIKBN   TO  SPA-NAI-NYUGAIKBN-OLD
           MOVE    SPA-GMN-HTEISYUTUSAKI
                                       TO  SPA-NAI-HTEISYUTUSAKI-OLD
           MOVE    SPA-GMN-HYOUJIKBN   TO  SPA-NAI-HYOUJIKBN-OLD
      *
           MOVE    R97-SRYYM           TO  SPA-GMN-SRYYM
           MOVE    R97-NYUGAIKBN       TO  SPA-GMN-NYUGAIKBN-G
           MOVE    R97-HTEISYUTUSAKI   TO  SPA-GMN-HTEISYUTUSAKI-G
           MOVE    R97-HYOUJIKBN       TO  SPA-GMN-HYOUJIKBN-G
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
           PERFORM 41023-SRYYM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41024-NYUGAIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41026-HTEISYUTUSAKI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41025-HYOUJIKBN-CHK-SEC
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       41020-SELNUM-CHK-SEC          SECTION.
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           INITIALIZE                  SPA-GMN-KOUMOKU
                                       SPA-NAI-KOUMOKU
                                       SPA-GMN-PTNUM-TABLE
      *
           MOVE    R97-SELNUM       TO  SPA-GMN-SELNUM
      *
      *    選択番号チェック
           IF      SPA-GMN-SELNUM      =   ZERO
      *****    MOVE    "0002"                  TO  SPA-ERRCD
               MOVE    2                       TO  SPA-GMN-CUR 
           ELSE
               COMPUTE WRK-BANGO   =   SPA-GMN-SELNUM
                                   - ( SPA-GMN-PAGE    -   1 )
                                   *   WRK-CONS-LINE-SYUKA-MAX
      *
               IF    ( WRK-BANGO       >   SPA-GMN-SYUKA-TBL-MAX )
               OR    ( WRK-BANGO       <   1                     )
                   MOVE    ZERO            TO  SPA-GMN-SELNUM
                   MOVE    "0002"          TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-BANGO       TO  SPA-NAI-SELNUM
      *
                   MOVE    SPA-NAI-TPTNUM        (SPA-NAI-SELNUM)
                                           TO  SPA-GMN-PTNUM
                   MOVE    SPA-NAI-TPTID         (SPA-NAI-SELNUM)
                                           TO  SPA-NAI-PTID
                   MOVE    SPA-NAI-TNAME         (SPA-NAI-SELNUM)
                                           TO  SPA-GMN-NAME
                   MOVE    SPA-NAI-TRECESYUBETU  (SPA-NAI-SELNUM)
                                           TO  SPA-NAI-RECESYUBETU
                   MOVE    SPA-GMN-THKNJANUM     (SPA-NAI-SELNUM)
                                           TO  SPA-GMN-HKNJANUM
                   MOVE    SPA-NAI-THKNJANUM     (SPA-NAI-SELNUM)
                                           TO  SPA-NAI-HKNJANUM
                   MOVE    SPA-GMN-THOJOKBN      (SPA-NAI-SELNUM)
                                        TO  SPA-GMN-HOJOKBN
                   MOVE    SPA-NAI-THOJOKBN      (SPA-NAI-SELNUM)
                                        TO  SPA-NAI-HOJOKBN
                   MOVE    SPA-NAI-TKOHID        (SPA-NAI-SELNUM)
                                        TO  SPA-NAI-KOHID
                   MOVE    SPA-GMN-TRECEIPT-KBN  (SPA-NAI-SELNUM)
                                        TO  SPA-GMN-RECEIPT-KBN
                   MOVE    SPA-NAI-TRECEIPT-KBN  (SPA-NAI-SELNUM)
                                        TO  SPA-NAI-RECEIPT-KBN
                   MOVE    SPA-NAI-TTEISYUTUSAKI  (SPA-NAI-SELNUM)
                                           TO  SPA-NAI-TEISYUTUSAKI
                   PERFORM 4001-SELNUM-HENSYU-SEC
      *
                   MOVE    SPA-NAI-SELNUM  TO  R97-SYUKALST-ROW
               END-IF
           END-IF
      *
           .
       41020-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号編集処理
      *****************************************************************
       4001-SELNUM-HENSYU-SEC          SECTION.
      *
           INITIALIZE                  HKNJAINF-REC
grpsys     MOVE    SPA-HOSPNUM     TO  HKNJA-HOSPNUM
           MOVE    SPA-GMN-HKNJANUM
                                   TO  HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC    TO  MCPDATA-REC
           PERFORM 900-HKNJAINF-INV-SEC
           IF      FLG-HKNJAINF    =   ZERO
               MOVE    HKNJA-HKNJANAME TO  SPA-GMN-HKNJAMEI
           END-IF
      * 
           EVALUATE    SPA-NAI-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    "社保"          TO  SPA-GMN-TEISYUTUSAKI
               WHEN    "2"
                   MOVE    "国保"          TO  SPA-GMN-TEISYUTUSAKI
               WHEN    "6"
                   MOVE    "広域"          TO  SPA-GMN-TEISYUTUSAKI
           END-EVALUATE
      *
           EVALUATE    SPA-NAI-RECESYUBETU
               WHEN    "0000"
                   MOVE    "不明分"        TO  SPA-GMN-RECESYUBETU
               WHEN    "6666"
                   MOVE    "特別療養費"    TO  SPA-GMN-RECESYUBETU
               WHEN    OTHER
                   MOVE    SPA-NAI-RECESYUBETU
                                           TO  SPA-GMN-RECESYUBETU
                   EVALUATE    SPA-NAI-RECESYUBETU (3:1)
                       WHEN    "4"
                       WHEN    "5"
                           MOVE    "3"     TO  SPA-GMN-RECESYUBETU (3:1)
                   END-EVALUATE
           END-EVALUATE
      *
           INITIALIZE                  SPA-NAI-SELNUM1
                                       SPA-GMN-PTNUM-TABLE
      *
           INITIALIZE                  ORCSSYUACCAREA
           MOVE    "06"            TO  LNK-SYUACC-I-KBN
           MOVE    SPA-NAI-SRYYM   TO  LNK-SYUACC-I-SRYYM
           MOVE    SPA-GMN-NYUGAIKBN
                                   TO  SPA-NYUGAIKBN
           MOVE    SPA-NAI-PTID    TO  SPA-PTID
           MOVE    SPA-NAI-RECESYUBETU
                                   TO  LNK-SYUACC-IO-RECESYUBETU (1)
           MOVE    SPA-NAI-HKNJANUM
                                   TO  LNK-SYUACC-IO-HKNJANUM    (1)
           MOVE    SPA-NAI-HOJOKBN TO  LNK-SYUACC-IO-HOJOKBN     (1)
           MOVE    SPA-NAI-TEISYUTUSAKI
                                   TO  LNK-SYUACC-IO-TEISYUTUSAKI(1)
           MOVE    SPA-NAI-RECEIPT-KBN
                                   TO  LNK-SYUACC-IO-RECEIPT-KBN (1)
           MOVE    SPA-NAI-KOHID   TO  LNK-SYUACC-IO-KOHID       (1)
           CALL    "ORCSSYUKAMAIN" USING
                                       SPA-AREA
                                       ORCSSYUACCAREA
      *
           MOVE    LNK-SYUACC-IO-H-SRYKA
                                   TO  SPA-GMN-SRYKA-INPUT
           MOVE    SPA-GMN-SRYKA-INPUT
                                   TO  WRK-SRYKA
           PERFORM 9001-SRYKA-HENSYU-SEC
           MOVE    WRK-SRYKAMEI    TO  SPA-GMN-SRYKAMEI-INPUT
      *
           MOVE    LNK-SYUACC-IO-SYUKA-AUTO  (1)
                                   TO  SPA-GMN-SRYKA-AUTO
           MOVE    SPA-GMN-SRYKA-AUTO
                                   TO  WRK-SRYKA
           PERFORM 9001-SRYKA-HENSYU-SEC
           MOVE    WRK-SRYKAMEI    TO  SPA-GMN-SRYKAMEI-AUTO
      *
           MOVE    LNK-SYUACC-IO-SYUKA-INPUT (1)
                                   TO  SPA-GMN-SRYKA-INPUT1
           MOVE    SPA-GMN-SRYKA-INPUT1
                                   TO  WRK-SRYKA
           PERFORM 9001-SRYKA-HENSYU-SEC
           MOVE    WRK-SRYKAMEI    TO  SPA-GMN-SRYKAMEI-INPUT1
      *
           IF      LNK-SYUACC-IO-H-SRYKA
                               NOT =   SPACE
               MOVE    LNK-SYUACC-IO-H-SRYKA
                                       TO  SPA-GMN-SYUKA
           ELSE
               MOVE    LNK-SYUACC-IO-SYUKA-INPUT (1)
                                       TO  SPA-GMN-SYUKA
           END-IF
           MOVE    SPA-GMN-SYUKA   TO  WRK-SRYKA
           PERFORM 9001-SRYKA-HENSYU-SEC
           MOVE    WRK-SRYKAMEI    TO  SPA-GMN-SYUKAMEI
      *
           IF    ( SPA-GMN-HOJOKBN     =   SPACE )
           AND   ( SPA-NAI-HOJOKBN NOT =   SPACE )      
               EVALUATE    LNK-SYUACC-IO-HKNNUM (1)
                   WHEN    "002"
                   WHEN    "031"  THRU  "034"
                       EVALUATE    SPA-NAI-HOJOKBN
                           WHEN    "1"
                           WHEN    "A"
                           WHEN    "D"
                           WHEN    "G"
                               MOVE    "職務上"    TO  SPA-GMN-HOJOKBN
                           WHEN    "2"
                           WHEN    "B"
                           WHEN    "E"
                           WHEN    "H"
                               MOVE    "下船後３ヶ月以内"
                                                   TO  SPA-GMN-HOJOKBN
                           WHEN    "3"
                           WHEN    "C"
                           WHEN    "F"
                           WHEN    "I"
                               MOVE    "通勤災害"  TO  SPA-GMN-HOJOKBN
                   END-EVALUATE
               END-EVALUATE
           END-IF
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       50
                   OR      LNK-SYUACC-IO-SRYKA (1 IDY)
                                   =  SPACE
               MOVE   LNK-SYUACC-IO-SRYKA (1 IDY)
                                       TO  SPA-GMN-PTNUM-TSRYKA(IDY)
               PERFORM VARYING IDZ FROM    1   BY  1
                       UNTIL   IDZ >       SPA-GMN-SYUKA-MAX
                   IF      SPA-GMN-PTNUM-TSRYKA (IDY)
                                           =   SPA-GMN-SYUKAL (IDZ)
                       MOVE    SPA-GMN-SYUKAMEIL (IDZ)
                                               TO
                                           SPA-GMN-PTNUM-TSRYKAMEI(IDY)
                       MOVE    SPA-GMN-SYUKA-MAX
                                               TO  IDZ
                   END-IF
               END-PERFORM
      *
               EVALUATE    SPA-GMN-NYUGAIKBN
                   WHEN    "1"
                       EVALUATE    SPA-SYUKA-NYUIN
                           WHEN    "1"
                               MOVE    LNK-SYUACC-IO-NISSU  (1 IDY)
                                           TO  SPA-GMN-PTNUM-TTENSU(IDY)
                           WHEN    "2"
                               MOVE    LNK-SYUACC-IO-TENSU  (1 IDY)
                                           TO  SPA-GMN-PTNUM-TTENSU(IDY)
                           WHEN    "3"
                               MOVE    LNK-SYUACC-IO-EDDAY  (1 IDY)
                                           TO  SPA-GMN-PTNUM-TTENSU(IDY)
                       END-EVALUATE
                   WHEN    "2"
                       EVALUATE    SPA-SYUKA-GAIRAI
                           WHEN    "1"
                               MOVE    LNK-SYUACC-IO-KAISU  (1 IDY)
                                           TO  SPA-GMN-PTNUM-TTENSU(IDY)
                           WHEN    "2"
                               MOVE    LNK-SYUACC-IO-TENSU  (1 IDY)
                                           TO  SPA-GMN-PTNUM-TTENSU(IDY)
                       END-EVALUATE
               END-EVALUATE
               MOVE    IDY                 TO  SPA-GMN-PTNUM-TBL-MAX  
           END-PERFORM
      *
           MOVE    3                   TO  SPA-GMN-CUR    
           
           .    
       4001-SELNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科一覧選択処理
      *****************************************************************
       400-SYUKALST-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-SYUKA-TBL-MAX
               IF      R97-SYUKALST-SELECT (IDX)  =   "T"
                   IF      IDX                 =   SPA-NAI-SELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX                 TO  WRK-SELNUM
                       MOVE    SPA-GMN-SYUKA-TBL-MAX
                                                   TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM           >   ZERO
               MOVE    WRK-SELNUM           TO  SPA-NAI-SELNUM
           END-IF
      *
           INITIALIZE                  SPA-GMN-KOUMOKU
      *
           MOVE    SPA-GMN-TRENNUM (SPA-NAI-SELNUM)
                                   TO  SPA-GMN-SELNUM
      *
           MOVE    SPA-NAI-TPTNUM        (SPA-NAI-SELNUM)
                                   TO  SPA-GMN-PTNUM
           MOVE    SPA-NAI-TPTID         (SPA-NAI-SELNUM)
                                   TO  SPA-NAI-PTID
           MOVE    SPA-NAI-TNAME         (SPA-NAI-SELNUM)
                                   TO  SPA-GMN-NAME
           MOVE    SPA-NAI-TRECESYUBETU  (SPA-NAI-SELNUM)
                                   TO  SPA-NAI-RECESYUBETU
           MOVE    SPA-GMN-THKNJANUM     (SPA-NAI-SELNUM)
                                   TO  SPA-GMN-HKNJANUM
           MOVE    SPA-NAI-THKNJANUM     (SPA-NAI-SELNUM)
                                   TO  SPA-NAI-HKNJANUM
           MOVE    SPA-GMN-THOJOKBN      (SPA-NAI-SELNUM)
                                   TO  SPA-GMN-HOJOKBN
           MOVE    SPA-NAI-THOJOKBN      (SPA-NAI-SELNUM)
                                   TO  SPA-NAI-HOJOKBN
           MOVE    SPA-NAI-TKOHID        (SPA-NAI-SELNUM)
                                   TO  SPA-NAI-KOHID
           MOVE    SPA-GMN-TRECEIPT-KBN  (SPA-NAI-SELNUM)
                                   TO  SPA-GMN-RECEIPT-KBN
           MOVE    SPA-NAI-TRECEIPT-KBN  (SPA-NAI-SELNUM)
                                    TO  SPA-NAI-RECEIPT-KBN
           MOVE    SPA-NAI-TTEISYUTUSAKI  (SPA-NAI-SELNUM)
                                   TO  SPA-NAI-TEISYUTUSAKI
           PERFORM 4001-SELNUM-HENSYU-SEC
           .
       400-SYUKALST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者レセプト一覧選択処理
      *****************************************************************
       400-PTNUMLST-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-PTNUM-TBL-MAX
               IF      R97-PTNUM-SELECT (IDX)  =   "T"
                   IF      IDX                 =   SPA-NAI-SELNUM1
                       CONTINUE
                   ELSE
                       MOVE    IDX                 TO  WRK-SELNUM
                       MOVE    SPA-GMN-PTNUM-TBL-MAX
                                                   TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM           >   ZERO
               MOVE    WRK-SELNUM           TO  SPA-NAI-SELNUM1
           END-IF
      *
           MOVE    SPA-GMN-PTNUM-TSRYKA (SPA-NAI-SELNUM1)
                                       TO  SPA-GMN-SYUKA
                                           WRK-SRYKA
           PERFORM 9001-SRYKA-HENSYU-SEC
           MOVE    WRK-SRYKAMEI    TO  SPA-GMN-SYUKAMEI
      *
           MOVE    3                   TO  SPA-GMN-CUR     
           .
       400-PTNUMLST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       41021-PTNUM-CHK-SEC            SECTION.
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           IF      SPA-NAI-SELNUM      >   ZERO
               IF      SPA-GMN-PTNUM   NOT =   R97-PTNUM
                   MOVE   "0004"               TO  SPA-ERRCD
               ELSE
                   MOVE    3                   TO  SPA-GMN-CUR
               END-IF
               GO  TO  41021-PTNUM-CHK-EXT
           END-IF
      *
           INITIALIZE                      SPA-GMN-KOUMOKU
                                           SPA-NAI-KOUMOKU
                                           SPA-GMN-PTNUM-TABLE
      *
           MOVE    R97-PTNUM           TO  SPA-GMN-PTNUM
      *
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    "0003"              TO  SPA-ERRCD
           ELSE
               INITIALIZE                      ORCSPTIDAREA
grpsys         MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
               MOVE    SPA-GMN-PTNUM       TO  SPTID-PTNUM
               CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                   SPA-AREA
               IF      SPTID-RC        NOT =   ZERO
                   MOVE    "0003"              TO  SPA-ERRCD
               ELSE
                   MOVE    SPTID-PTNUM         TO  SPA-GMN-PTNUM
                   MOVE    SPTID-PTID          TO  SPA-NAI-PTID
      *
      *            患者マスター存在チェック
grpsys             MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
                   MOVE    SPA-NAI-PTID        TO  PTINF-PTID
                   PERFORM 900-PTINF-READ-SEC
                   IF      FLG-PTINF       NOT =   ZERO
                       MOVE    "0001"          TO  SPA-ERRCD
                   ELSE
                       MOVE    PTINF-NAME      TO  SPA-GMN-NAME
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               INITIALIZE                  SYUKA-REC
grpsys         MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
               MOVE    SPA-NAI-SRYYM   TO  SYUKA-SRYYM
               MOVE    SPA-GMN-NYUGAIKBN  
                                       TO  SYUKA-NYUGAIKBN
               MOVE    "%"             TO  SYUKA-TEISYUTUSAKI
               MOVE    SPA-NAI-PTID    TO  SYUKA-PTID
               MOVE    SYUKA-REC       TO  MCPDATA-REC
               MOVE    "DBSELECT"      TO  MCP-FUNC
               MOVE    "tbl_syuka"     TO  MCP-TABLE
               MOVE    "key8"          TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    "tbl_syuka"     TO  MCP-TABLE
                   MOVE    "key8"          TO  MCP-PATHNAME
                   PERFORM 910-SYUKA-NEXT-SEC
               ELSE
                   MOVE    1               TO  FLG-SYUKA
               END-IF
               IF      FLG-SYUKA       =   ZERO
      *            該当診療月の主科情報編集処理
                   PERFORM 410211-PTNUM-SYUKA-HENSYU-SEC     
               ELSE
                   MOVE    "0005"          TO  SPA-ERRCD
               END-IF
      *
               MOVE    "tbl_syuka"     TO  MCP-TABLE
               MOVE    "key8"          TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF    
           .
       41021-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    該当患者番号の診療月の主科情報編集処理
      *****************************************************************
       410211-PTNUM-SYUKA-HENSYU-SEC   SECTION.
      *
      *    条件の変更
           MOVE    SPA-GMN-HTEISYUTUSAKILST (1)
                                   TO  SPA-GMN-HTEISYUTUSAKI-G
           MOVE    SPA-GMN-HYOUJIKBNLST (1)
                                   TO  SPA-GMN-HYOUJIKBN-G
      *
           INITIALIZE                  WRK-ORCSSYUACCAREA
      *
           MOVE    1               TO  SPA-GMN-PAGE
                                       WRK-PAGE 
           MOVE    ZERO            TO  WRK-RENNUM
                                       WRK-LINE
      *
           MOVE    SPA-NAI-PTID    TO  WRK-PTID
           INITIALIZE                  SPA-GMN-SYUKA-TABLE
                                       SPA-GMN-PTNUM-TABLE
                                       SPA-NAI-SYUKA-TABLE
                                       SPA-GMN-KOUMOKU
                                       SPA-NAI-KOUMOKU
      *
      *    患者情報画面編集処理
      *
           MOVE    ZERO            TO  IDW
           MOVE    WRK-ORCSSYUACCAREA
                                   TO  ORCSSYUACCAREA
           PERFORM UNTIL   FLG-SYUKA   =   1
               ADD   1                 TO  IDW
               MOVE  SYUKA-REC         TO  LNK-SYUACC-IO2-OCC (IDW)
      *
               MOVE    "tbl_syuka"     TO  MCP-TABLE
               MOVE    "key8"          TO  MCP-PATHNAME
               PERFORM 910-SYUKA-NEXT-SEC
           END-PERFORM
      *
      *    手入力による主科情報の取得
           INITIALIZE                  SYUKA-REC
grpsys     MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
           MOVE    SPA-NAI-SRYYM   TO  SYUKA-SRYYM
           MOVE    WRK-PTID        TO  SYUKA-PTID
           MOVE    SYUKA-REC       TO  MCPDATA-REC
           MOVE    "tbl_syuka"     TO  WRK-MCP-TABLE
           MOVE    "key4"          TO  WRK-MCP-PATHNAME
           PERFORM 900-SYUKA-READ-SEC
           IF      FLG-SYUKA       =   ZERO
               ADD     1               TO  IDW
               MOVE    SYUKA-REC       TO  LNK-SYUACC-IO2-OCC (IDW)
           END-IF
      *
           PERFORM 31011-SYUKA-GMN-HENSYU-SEC
      *    次ページの有無
           MOVE    ZERO            TO  SPA-GMN-NEXT
      *
           MOVE    1               TO  SPA-GMN-SELNUM
                                       SPA-NAI-SELNUM
           MOVE    SPA-NAI-TPTNUM        (SPA-NAI-SELNUM)
                                   TO  SPA-GMN-PTNUM
           MOVE    SPA-NAI-TPTID         (SPA-NAI-SELNUM)
                                   TO  SPA-NAI-PTID
           MOVE    SPA-NAI-TNAME         (SPA-NAI-SELNUM)
                                   TO  SPA-GMN-NAME
           MOVE    SPA-NAI-TRECESYUBETU  (SPA-NAI-SELNUM)
                                   TO  SPA-NAI-RECESYUBETU
           MOVE    SPA-GMN-THKNJANUM     (SPA-NAI-SELNUM)
                                   TO  SPA-GMN-HKNJANUM
           MOVE    SPA-NAI-THKNJANUM     (SPA-NAI-SELNUM)
                                   TO  SPA-NAI-HKNJANUM
           MOVE    SPA-GMN-THOJOKBN      (SPA-NAI-SELNUM)
                                   TO  SPA-GMN-HOJOKBN
           MOVE    SPA-NAI-THOJOKBN      (SPA-NAI-SELNUM)
                                   TO  SPA-NAI-HOJOKBN
           MOVE    SPA-NAI-TKOHID        (SPA-NAI-SELNUM)
                                   TO  SPA-NAI-KOHID
           MOVE    SPA-GMN-TRECEIPT-KBN  (SPA-NAI-SELNUM)
                                   TO  SPA-GMN-RECEIPT-KBN
           MOVE    SPA-NAI-TRECEIPT-KBN  (SPA-NAI-SELNUM)
                                   TO  SPA-NAI-RECEIPT-KBN
           MOVE    SPA-NAI-TTEISYUTUSAKI  (SPA-NAI-SELNUM)
                                   TO  SPA-NAI-TEISYUTUSAKI
           PERFORM 4001-SELNUM-HENSYU-SEC
      *
           MOVE    SPA-NAI-SELNUM  TO  R97-SYUKALST-ROW
           MOVE    3               TO  SPA-GMN-CUR
      *
           .
       410211-PTNUM-SYUKA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科入力処理
      *****************************************************************
       41022-SYUKA-CHK-SEC                SECTION.
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *
            MOVE    R97-SYUKA          TO  SPA-GMN-SYUKA
      *
           IF      SPA-GMN-SYUKA       =   SPACE   
               MOVE    SPACE               TO  SPA-GMN-SYUKA-G
           ELSE
               MOVE    SPA-GMN-SYUKA       TO  WRK-SRYKA
               PERFORM 9001-SRYKA-HENSYU-SEC
               MOVE    WRK-SRYKAMEI    TO  SPA-GMN-SYUKAMEI
               IF      SPA-GMN-SYUKAMEI    =   SPACE
                   MOVE    "0006"          TO  SPA-ERRCD
               END-IF
           END-IF
      *    
           IF      SPA-ERRCD           =   SPACE
      *        更新時の必須入力チェック
               PERFORM 410221-KANREN-CHK-SEC
               IF    ( SPA-ERRCD           =   SPACE )
               AND   ( SPA-RIDCD           =   SPACE )
                   PERFORM 4104-KOUSIN-SEC
               END-IF
           END-IF
           .
       41022-SYUKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       410221-KANREN-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-SYUKAMEI   =   SPACE )
           AND   ( SPA-GMN-SRYKA-AUTO =   SPACE )
               MOVE    "0102"             TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
               GO      TO      410221-KANREN-CHK-EXT
           END-IF
      *
           IF      SPA-NAI-PTID        =   ZERO
               MOVE    "0100"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      410221-KANREN-CHK-EXT
           END-IF
      *
      *    システム科がないときは一覧のチェックはしない
           IF    ( SPA-GMN-PTNUM-TBL-MAX         )
                                       =   ZERO
           AND   ( SPA-GMN-SRYKA-AUTO
                                   NOT =   SPACE )
               MOVE    "0101"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      410221-KANREN-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-SYUKA   NOT =   SPACE    
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PTNUM-TBL-MAX
                   IF      SPA-GMN-SYUKA   =   SPA-GMN-PTNUM-TSRYKA(IDY)
                       MOVE    1                       TO  FLG-CHK
                       MOVE    SPA-GMN-PTNUM-TBL-MAX   TO  IDY
                   END-IF
               END-PERFORM
               IF      FLG-CHK         =   ZERO
                   MOVE    "1001"          TO  SPA-RIDCD
               END-IF
           END-IF    
           .
      *
       410221-KANREN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月入力処理
      *****************************************************************
       41023-SRYYM-CHK-SEC           SECTION.
      *
           MOVE    ZERO               TO  SPA-NAI-SRYYM
           MOVE    SPACE              TO  SPA-NAI-SRYYMH
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    "1"                 TO  SGDAY-INTYPE
           MOVE    SPA-GMN-SRYYM       TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN-SRYYM
               MOVE    SGDAY-SDAY (1:6)    TO  SPA-NAI-SRYYM
               MOVE    SGDAY-SDAY          TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG3(1:16)  TO  SPA-NAI-SRYYMH
           ELSE
               MOVE    "0007"          TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    4               TO  SPA-GMN-CUR
           END-IF
           .
       41023-SRYYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分  入力処理
      *****************************************************************
       41024-NYUGAIKBN-CHK-SEC                SECTION.
      *
           MOVE    SPACE                TO  SPA-NAI-NYUGAIKBN
      *
           PERFORM VARYING IDY FROM   1   BY  1
                   UNTIL   IDY >      SPA-GMN-NYUGAIKBN-MAX
               IF      SPA-GMN-NYUGAIKBN 
                               =   SPA-GMN-NYUGAIKBNL (IDY)
                   MOVE    SPA-GMN-NYUGAIKBNLST (IDY)
                                      TO  SPA-GMN-NYUGAIKBN-G
                   MOVE    SPA-GMN-NYUGAIKBN-MAX
                                      TO  IDY
               END-IF
           END-PERFORM
           IF      SPA-GMN-NYUGAIKBNMEI
                                       =   SPACE
               MOVE    "0008"              TO  SPA-ERRCD
           ELSE
               MOVE    SPA-GMN-NYUGAIKBNMEI 
                                           TO  SPA-NAI-NYUGAIKBN
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    5               TO  SPA-GMN-CUR
           END-IF
      *             
           .
       41024-NYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    表示条件入力処理
      *****************************************************************
       41025-HYOUJIKBN-CHK-SEC                SECTION.
      *
           PERFORM VARYING IDY FROM   1   BY  1
                   UNTIL   IDY >      SPA-GMN-HYOUJIKBN-MAX
               IF      SPA-GMN-HYOUJIKBN 
                               =   SPA-GMN-HYOUJIKBNL (IDY)
                   MOVE    SPA-GMN-HYOUJIKBNLST (IDY)
                                      TO  SPA-GMN-HYOUJIKBN-G
                   MOVE    SPA-GMN-HYOUJIKBN-MAX
                                      TO  IDY
               END-IF
           END-PERFORM
           IF      SPA-GMN-HYOUJIKBNMEI
                                       =   SPACE
               MOVE    "0009"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    6               TO  SPA-GMN-CUR
           END-IF
      *             
           .
       41025-HYOUJIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    提出先入力処理
      *****************************************************************
       41026-HTEISYUTUSAKI-CHK-SEC                SECTION.
      *
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-HTEISYUTUSAKI-MAX
               IF      SPA-GMN-HTEISYUTUSAKI 
                               =   SPA-GMN-HTEISYUTUSAKIL (IDY)
                   MOVE    SPA-GMN-HTEISYUTUSAKILST (IDY)
                                       TO  SPA-GMN-HTEISYUTUSAKI-G
                   MOVE    SPA-GMN-HTEISYUTUSAKI-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      SPA-GMN-HTEISYUTUSAKIMEI
                                       =   SPACE
               MOVE    "0010"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    7               TO  SPA-GMN-CUR
           END-IF
      *             
           .
       41026-HTEISYUTUSAKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括取得処理前チェック処理
      *****************************************************************
       4101-KAKUTEI-CHK-SEC        SECTION.
      *
      *
           .
       4101-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       4104-KOUSIN-SEC             SECTION.
      *
           INITIALIZE                  SYUKA-REC
grpsys     MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
           MOVE    SPA-PTID        TO  SYUKA-PTID
           MOVE    LNK-SYUACC-I-SRYYM
                                   TO  SYUKA-SRYYM
           MOVE    SPA-GMN-NYUGAIKBN
                                   TO  SYUKA-NYUGAIKBN
           MOVE    SPA-NAI-RECESYUBETU
                                   TO  SYUKA-RECESYUBETU
           MOVE    SPA-NAI-HKNJANUM
                                   TO  SYUKA-HKNJANUM
           MOVE    SPA-NAI-HOJOKBN TO  SYUKA-HOJOKBN 
           MOVE    SPA-NAI-KOHID   TO  SYUKA-KOHID 
           MOVE    SPA-NAI-RECEIPT-KBN
                                   TO  SYUKA-RECEIPT-KBN 
           MOVE    SPA-NAI-TEISYUTUSAKI
                                   TO  SYUKA-TEISYUTUSAKI
           MOVE    SPA-GMN-SYUKA   TO  SYUKA-SRYKA-INPUT 
      *
           MOVE    SMCNDATE-YMD        TO  SYUKA-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYUKA-UPHMS
      *
           MOVE    SYUKA-REC       TO  MCPDATA-REC
      *  
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           MOVE    "tbl_syuka"     TO  MCP-TABLE
           MOVE    "upd2"          TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC      NOT =   ZERO
               DISPLAY "SYUKA KEY=" 
                                   MCP-FUNC " " SYUKA-KEY
               MOVE    "1001"          TO  SPA-ERRCD
           ELSE
      *
      *        手入力による主科情報のクリア
               INITIALIZE                  SYUKA-REC
grpsys         MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
               MOVE    SPA-PTID        TO  SYUKA-PTID
               MOVE    LNK-SYUACC-I-SRYYM
                                       TO  SYUKA-SRYYM
               MOVE    SYUKA-REC       TO  MCPDATA-REC
               MOVE    "tbl_syuka"     TO  WRK-MCP-TABLE
               MOVE    "key4"          TO  WRK-MCP-PATHNAME
               PERFORM 900-SYUKA-READ-SEC
               IF      FLG-SYUKA       =   ZERO
                   MOVE    SPACE           TO  SYUKA-SRYKA-INPUT
                   MOVE    SYUKA-REC       TO  MCPDATA-REC
                   MOVE    "DBUPDATE"      TO  MCP-FUNC
                   MOVE    "tbl_syuka"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC      NOT =   ZERO
                       DISPLAY "SYUKA KEY=" 
                                       MCP-FUNC " " SYUKA-KEY
                       MOVE    "1001"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        該当患者の再表示
               IF      SPA-NAI-SELNUM  >   ZERO
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       SPA-GMN-SYUKA-TBL-MAX
                       IF      SPA-NAI-PTID
                                           =   SPA-NAI-TPTID (IDX)
                           INITIALIZE                  ORCSSYUACCAREA
                           MOVE    "06"            TO  LNK-SYUACC-I-KBN
                           MOVE    SPA-NAI-SRYYM   TO
                                           LNK-SYUACC-I-SRYYM
                           MOVE    SPA-GMN-NYUGAIKBN
                                                   TO  SPA-NYUGAIKBN
                           MOVE    SPA-NAI-PTID    TO  SPA-PTID
                           MOVE    SPA-NAI-TRECESYUBETU (IDX)
                                                   TO
                                           LNK-SYUACC-IO-RECESYUBETU (1)
                           MOVE    SPA-NAI-THKNJANUM (IDX)
                                                   TO
                                           LNK-SYUACC-IO-HKNJANUM    (1)
                           MOVE    SPA-NAI-THOJOKBN (IDX)
                                                   TO
                                           LNK-SYUACC-IO-HOJOKBN     (1)
                           MOVE    SPA-NAI-TTEISYUTUSAKI (IDX)
                                                   TO
                                           LNK-SYUACC-IO-TEISYUTUSAKI(1)
                           MOVE    SPA-NAI-TKOHID   (IDX)
                                                   TO
                                           LNK-SYUACC-IO-KOHID       (1)
                           MOVE    SPA-NAI-TRECEIPT-KBN  (IDX)
                                                   TO
                                           LNK-SYUACC-IO-RECEIPT-KBN (1)
                           CALL    "ORCSSYUKAMAIN" USING
                                                       SPA-AREA
                                                       ORCSSYUACCAREA
                           MOVE    LNK-SYUACC-IO-SYUKA-AUTO (1)
                                           TO  SPA-GMN-TSYUKA-AUTO (IDX)
                           IF      LNK-SYUACC-IO-H-SRYKA
                                           NOT =   SPACE
                               MOVE    LNK-SYUACC-IO-H-SRYKA
                                           TO  SPA-GMN-TSYUKA-INPUT(IDX)
                           ELSE    
                               MOVE    LNK-SYUACC-IO-SYUKA-INPUT(1)
                                           TO  SPA-GMN-TSYUKA-INPUT(IDX)
                           END-IF
                       END-IF
                   END-PERFORM        
               END-IF
      *
               INITIALIZE              SPA-GMN-PTNUM-TABLE
                                       SPA-GMN-KOUMOKU
                                       SPA-NAI-KOUMOKU 
               MOVE    1               TO  SPA-GMN-CUR    
           END-IF
           .
      *
       4104-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       420-CLEAR-SEC                SECTION.
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           INITIALIZE                  SPA-GMN-KOUMOKU
                                       SPA-NAI-KOUMOKU
                                       SPA-NAI-SELNUM
                                       SPA-NAI-SELNUM1
                                       SPA-GMN-PTNUM-TABLE
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    前月分処理
      *****************************************************************
       450-BACK-MONTH-SEC               SECTION.
      *
           IF      SPA-NAI-PTID        >   ZERO
               MOVE    "0103"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    SPA-NAI-SRYYM       TO  LNK-DAY6-KIJUN (1:6)
               MOVE    "01"                TO  LNK-DAY6-KIJUN (7:2)  
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    -1                  TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN(1:6)
                                           TO  SPA-NAI-SRYYM
      *
               MOVE    SPA-NAI-SRYYM       TO  WRK-SYMD (1:6)
               MOVE    "01"                TO  WRK-SYMD (7:2) 
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG1(1:6)   TO  SPA-GMN-SRYYM
               MOVE    WRK-HENYMDG3(1:16)  TO  SPA-NAI-SRYYMH
      *
               MOVE    1                   TO  SPA-GMN-PAGE
      *     
               PERFORM 3101-SYUKA-HENSYU-SEC
           END-IF
           .
      *
       450-BACK-MONTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       460-BACK-PAGE-SEC               SECTION.
      *
           IF      SPA-GMN-PAGE    =   1
               GO      TO      460-BACK-PAGE-EXT
           END-IF
      *
           IF      SPA-NAI-PTID        >   ZERO
               MOVE    "0103"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE    -   1
               PERFORM 3101-SYUKA-HENSYU-SEC
           END-IF    
           .
      *
       460-BACK-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁処理
      *****************************************************************
       470-NEXT-PAGE-SEC               SECTION.
      *
           IF      SPA-NAI-PTID        >   ZERO
               MOVE    "0103"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-GMN-NEXT        =   ZERO
               GO      TO      470-NEXT-PAGE-EXT
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE    +   1
               PERFORM 3101-SYUKA-HENSYU-SEC
           END-IF    
           .
      *
       470-NEXT-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    次月分処理
      *****************************************************************
       480-NEXT-MONTH-SEC               SECTION.
      *
           IF      SPA-NAI-PTID        >   ZERO
               MOVE    "0103"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *         次月診療年月         
               INITIALIZE                     STS-AREA-DAY
               INITIALIZE                     LNK-DAY6-AREA
               MOVE    "61"                   TO  LNK-DAY6-IRAI
               MOVE    SPA-NAI-SRYYM          TO  LNK-DAY6-KIJUN (1:6)
               MOVE    "01"                   TO  LNK-DAY6-KIJUN (7:2)  
               MOVE    "2"                    TO  LNK-DAY6-ZOGENPTN
               MOVE    1                      TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"              USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN(1:6)   TO  SPA-NAI-SRYYM
      *
               MOVE    SPA-NAI-SRYYM       TO  WRK-SYMD (1:6)
               MOVE    "01"                TO  WRK-SYMD (7:2) 
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG1(1:6)   TO  SPA-GMN-SRYYM
               MOVE    WRK-HENYMDG3(1:16)  TO  SPA-NAI-SRYYMH
      *
               MOVE    1                   TO  SPA-GMN-PAGE
      *     
               PERFORM 3101-SYUKA-HENSYU-SEC
           END-IF
           .
      *
       480-NEXT-MONTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索処理
      *****************************************************************
       490-KENSAKU-SEC             SECTION.
      *
           IF      SPA-NAI-PTID        >   ZERO
               MOVE    "0103"              TO  SPA-ERRCD
               GO  TO  490-KENSAKU-EXT
           END-IF
      *
           MOVE    1               TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  490-KENSAKU-EXT
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           INITIALIZE                      SPA-NAI-PAGE-TABLE
      *
      *    該当診療月の主科情報編集処理
           PERFORM 3101-SYUKA-HENSYU-SEC     
          .
      *
       490-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-R97             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "R97"               TO  SPA-MOTOPG
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
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *    
           MOVE    SPACE               TO  SPA-R01-GMNKBN
      *
           MOVE    SPA-R01KYOUTU       TO  SPA-WORK       
      *
           MOVE    "R03"               TO  SPA-SAKIPG
           MOVE    "R97"               TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括取得前処理
      *****************************************************************
       450-TOUROKU-MAE-SEC                SECTION.
      *
      *    入力チェック
           MOVE    1               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  450-TOUROKU-MAE-EXT
           END-IF
      *
      *    ジョブ管理チェック処理
           PERFORM 4501-JOBKANRI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  450-TOUROKU-MAE-EXT
           END-IF    
      *
           MOVE    "1002"              TO  SPA-RIDCD
           .
       450-TOUROKU-MAE-EXT.
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
                   STRING  "処理中です【"    DELIMITED  BY  SIZE
                           JOB-SHELLMSG      DELIMITED  BY  SPACE
                           "】"              DELIMITED  BY  SIZE      
                                             INTO  WRK-RECEERR
                   END-STRING                          
               END-IF
           END-IF    
           .
       4501-JOBKANRI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       450-KOUSHIN-SEC                 SECTION.
      *
      *    フラグ初期化
           MOVE    ZERO            TO  SPA-R01-JOBCNT       
                                       SPA-R01-STOPFLG    
           MOVE    SPACE           TO  SPA-R99-CANCEL
      *
      *    ジョブ開始セット
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
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           MOVE    SPACE           TO  SHELLTBL
      *    ファイルデイレクトリ位置指定
           INITIALIZE              ORCSMKPASSAREA
           MOVE    "recept50.sh"
                                   TO  WRK-SHELL-SHELL
           MOVE    WRK-SHELL-NAME  TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"    USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
      *             
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG1
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG2
grpsys     MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG3
           MOVE    SPA-NAI-SRYYM   TO  SHELLTBL-ARG4
           MOVE    SPA-GMN-NYUGAIKBN
                                   TO  SHELLTBL-ARG5
           MOVE    SPA-PREFNUM     TO  SHELLTBL-ARG6
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "shell"         TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *    情報確認画面を表示                                                   
           PERFORM 490-KAKUNIN-SEC
           .
       450-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *                                     
      *    画面移行用のパラメタ
           PERFORM 45011-PARA-HENSYU-SEC
      *
           MOVE    SPACE               TO  R99
           INITIALIZE                      R99
           MOVE    ZERO                TO  R99-COUNT
                                           R99-PROGRESSBAR-VALUE
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
                           JOB-STEPCNT  /   1     *   100
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "処理中です"    TO  R99-MSG
               ELSE
                   IF      JOB-ERRCD       =   SPACE
                       CONTINUE
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
           MOVE    "B01"                TO  MCP-WIDGET
           IF      R99-MSG              =   SPACE
               IF      R99-COUNT            >   ZERO    
                   MOVE    "処理は正常に終了しました"
                                                TO  R99-MSG
                   MOVE   100                   TO
                                                R99-PROGRESSBAR-VALUE
               ELSE
                   MOVE    "該当する処理はありません"
                                                TO  R99-MSG
               END-IF                                 
           ELSE
               MOVE    "B11"                TO  MCP-WIDGET
           END-IF
      *
           MOVE    "R97"                TO  SPA-MOTOPG
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
      *    画面移行用のパラメタ変更処理
      *****************************************************************
       45011-PARA-HENSYU-SEC            SECTION.       
      *    
           MOVE    "R97"               TO  SPA-R01-GMNKBN
      *
           MOVE    SPA-R01KYOUTU       TO  SPA-WORK       
      *
           .
       45011-PARA-HENSYU-EXT.
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
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG1
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG1
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG3
               
           END-IF
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "R97    "           TO  MCP-WINDOW.
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
           IF      FLG-SYUKA-ROW       =   1
               MOVE    ZERO                TO  WRK-SYUKA-ROW
           ELSE
               MOVE    R97-SYUKALST-ROW    TO  WRK-SYUKA-ROW
           END-IF
           IF      FLG-PTNUM-ROW       =   1
               MOVE    ZERO                TO  WRK-PTNUM-ROW
           ELSE
               MOVE    R97-PTNUM-ROW       TO  WRK-PTNUM-ROW
           END-IF
      * 
           MOVE    SPACE               TO  R97
      *
           INITIALIZE                      R97
           MOVE    SPA-GMN-SRYYM       TO  R97-SRYYM
      *
           MOVE    SPA-GMN-NYUGAIKBN-G TO  R97-NYUGAIKBN
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   SPA-GMN-NYUGAIKBN-MAX
               MOVE    SPA-GMN-NYUGAIKBNLST (IDX)
                                           TO  R97-NYUGAIKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-NYUGAIKBN-MAX
                                       TO  R97-NYUGAIKBN-COUNT
      *
           MOVE    SPA-GMN-HYOUJIKBN-G TO  R97-HYOUJIKBN
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   SPA-GMN-HYOUJIKBN-MAX
               MOVE    SPA-GMN-HYOUJIKBNLST (IDX)
                                           TO  R97-HYOUJIKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-HYOUJIKBN-MAX
                                       TO  R97-HYOUJIKBN-COUNT
      *
           MOVE    SPA-GMN-HTEISYUTUSAKI-G     TO  R97-HTEISYUTUSAKI
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-HTEISYUTUSAKI-MAX
               MOVE    SPA-GMN-HTEISYUTUSAKILST (IDX)
                                       TO  R97-HTEISYUTUSAKI-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-HTEISYUTUSAKI-MAX
                                       TO  R97-HTEISYUTUSAKI-COUNT
      *          
           IF      SPA-GMN-SYUKA-MAX   =   ZERO
               MOVE    "該当患者なし"      TO  R97-SYUKA-TNAME  (1)
               MOVE    1                   TO  R97-SYUKALST-COUNT
               MOVE    ZERO                TO  R97-SYUKALST-ROW
           ELSE
             PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-SYUKA-TBL-MAX
               MOVE    SPA-GMN-TRENNUM     (IDX)
                                       TO  WRK-NUM
               MOVE    WRK-NUM         TO  R97-SYUKA-TNUM (IDX)
               MOVE    SPA-GMN-TTSTPTNUMKBN(IDX)
                                       TO  R97-SYUKA-TTSTPTNUMKBN(IDX) 
               MOVE    SPA-GMN-TPTNUM      (IDX)
                                       TO  R97-SYUKA-TPTNUM      (IDX)
               MOVE    SPA-GMN-TNAME       (IDX)
                                       TO  R97-SYUKA-TNAME       (IDX)
               MOVE    SPA-GMN-TTEISYUTUSAKI (IDX)
                                       TO  R97-SYUKA-TTEISYUTUSAKI (IDX)
               MOVE    SPA-GMN-TRECESYUBETU(IDX)
                                       TO  R97-SYUKA-TRECESYUBETU(IDX)
               MOVE    SPA-GMN-THKNJANUM   (IDX)
                                       TO  R97-SYUKA-THKNJANUM   (IDX)
               MOVE    SPA-GMN-THOJOKBN    (IDX)
                                       TO  R97-SYUKA-THOJOKBN    (IDX)
               MOVE    SPA-GMN-TRECEIPT-KBN(IDX)
                                       TO  R97-SYUKA-TRECEIPT-KBN(IDX)
               MOVE    SPA-GMN-TDISASTER   (IDX)
                                       TO  R97-SYUKA-TDISASTER   (IDX)
               MOVE    SPA-GMN-TSYUKA-INPUT(IDX)
                                       TO  R97-SYUKA-TSYUKA-INPUT(IDX)
               MOVE    SPA-GMN-TSYUKA-AUTO (IDX)
                                       TO  R97-SYUKA-TSYUKA-AUTO (IDX)
      *         
               IF      IDX             =   SPA-NAI-SELNUM
                   MOVE    "T"             TO  R97-SYUKALST-SELECT (IDX)
               ELSE
                   MOVE    "F"             TO  R97-SYUKALST-SELECT (IDX)
               END-IF
             END-PERFORM
             MOVE    SPA-GMN-SYUKA-TBL-MAX
                                         TO  R97-SYUKALST-COUNT
             MOVE    ZERO                TO  R97-SYUKALST-ROW
             IF      WRK-SYUKA-ROW       >   ZERO
                 MOVE    WRK-SYUKA-ROW       TO  R97-SYUKALST-ROW
             END-IF
           END-IF
      *
           EVALUATE    SPA-GMN-NYUGAIKBN
               WHEN    "1"
                   EVALUATE    SPA-SYUKA-NYUIN
                       WHEN    "1"
                           MOVE    "日数"      TO  R97-PTNUM-HYODAI
                       WHEN    "2"
                           MOVE    "点数"      TO  R97-PTNUM-HYODAI
                       WHEN    "3"
                           MOVE    "最終"      TO  R97-PTNUM-HYODAI
                   END-EVALUATE
               WHEN    "2"
                   EVALUATE    SPA-SYUKA-GAIRAI
                       WHEN    "1"
                           MOVE    "回数"      TO  R97-PTNUM-HYODAI
                       WHEN    "2"
                           MOVE    "点数"      TO  R97-PTNUM-HYODAI
                   END-EVALUATE
           END-EVALUATE
      *          
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-PTNUM-TBL-MAX
               MOVE    SPA-GMN-PTNUM-TSRYKA      (IDX)
                                           TO  R97-PTNUM-TSRYKA (IDX)
               MOVE    SPA-GMN-PTNUM-TSRYKAMEI   (IDX)
                                           TO  R97-PTNUM-TSRYKA(IDX)(4:)
               MOVE    SPA-GMN-PTNUM-TTENSU      (IDX)
                                           TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ          TO  R97-PTNUM-TTENSU (IDX)
      *         
               IF      IDX             =   SPA-NAI-SELNUM1
                   MOVE    "T"             TO  R97-PTNUM-SELECT (IDX)
               ELSE
                   MOVE    "F"             TO  R97-PTNUM-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-PTNUM-TBL-MAX
                                       TO  R97-PTNUM-COUNT
           MOVE    ZERO                TO  R97-PTNUM-ROW
           IF      WRK-PTNUM-ROW       >   ZERO
               MOVE    WRK-PTNUM-ROW       TO  R97-PTNUM-ROW
           END-IF
      *
           MOVE    SPA-GMN-SELNUM      TO  R97-SELNUM
           MOVE    SPA-GMN-PTNUM       TO  R97-PTNUM
           MOVE    SPA-GMN-NAME        TO  R97-NAME
           MOVE    SPA-GMN-TEISYUTUSAKI
                                       TO  R97-TEISYUTUSAKI
           MOVE    SPA-GMN-RECESYUBETU TO  R97-RECESYUBETU
           MOVE    SPA-GMN-HKNJANUM    TO  R97-HKNJANUM
           MOVE    SPA-GMN-HKNJAMEI    TO  R97-HKNJAMEI
           MOVE    SPA-GMN-HOJOKBN     TO  R97-HOJOKBN
      *****MOVE    SPA-GMN-RECEIPT-KBN TO  R97-RECEIPT-KBN
           MOVE    SPA-GMN-SRYKA-INPUT-G
                                       TO  R97-SYUKA-INPUT
           MOVE    SPA-GMN-SRYKA-INPUT1-G
                                       TO  R97-SYUKA-INPUT1
           MOVE    SPA-GMN-SRYKA-AUTO-G
                                       TO  R97-SYUKA-AUTO 
           MOVE    SPA-GMN-SYUKA-G     TO  R97-SYUKA
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   SPA-GMN-SYUKA-MAX
               MOVE    SPA-GMN-SYUKALST (IDX)  TO  R97-SYUKA-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-SYUKA-MAX   TO  R97-SYUKA-COUNT
      *
           IF      SPA-GMN-PAGE        <=  1     
               MOVE    WIDGET-INSENSITIVE  TO  R97-B06-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  R97-B06-STATE
           END-IF    
           IF      SPA-GMN-NEXT        =   ZERO
               MOVE    WIDGET-INSENSITIVE  TO  R97-B07-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  R97-B07-STATE
           END-IF
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
           IF    ( SPA-ERRCD       =   SPACE )
           AND   ( SPA-GMN-CUR     =   ZERO  )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE  "SELNUM"      TO  MCP-WIDGET
               WHEN    2
                   MOVE  "PTNUM"       TO  MCP-WIDGET
               WHEN    3
                   MOVE  "SYUKA"       TO  MCP-WIDGET
               WHEN    4
                   MOVE  "SRYYM"       TO  MCP-WIDGET
               WHEN    5
                   MOVE  "NYUGAIKBN"   TO  MCP-WIDGET
               WHEN    6
                   MOVE  "HYOUJIKBN"   TO  MCP-WIDGET
               WHEN    7
                   MOVE  "HTEISYUTUSAKI"
                                       TO  MCP-WIDGET
               WHEN    91 
                   MOVE  "B09"         TO  MCP-WIDGET   
               WHEN    98 
                   MOVE  "B01"         TO  MCP-WIDGET   
               WHEN    99
                   MOVE  "B12"         TO  MCP-WIDGET   
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SELNUM"
                   MOVE    2           TO  SPA-GMN-CUR
               WHEN    "PTNUM"
                   MOVE    3           TO  SPA-GMN-CUR
               WHEN    "SYUKA"
                   MOVE    1           TO  SPA-GMN-CUR
               WHEN    "SRYYM"
                   MOVE    5           TO  SPA-GMN-CUR
               WHEN    "NYUGAIKBN"
                   MOVE    7           TO  SPA-GMN-CUR
               WHEN    "HTEISYUTUSAKI"
                   MOVE    6           TO  SPA-GMN-CUR
               WHEN    "HYOUJIKBN"
                   MOVE    91          TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "患者番号入力エラー"        TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "選択番号入力エラー"        TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "該当の患者番号はありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "他の患者番号を選択中です"  TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "該当診療年月のレセプトがありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "主科入力エラー"            TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "診療年月入力エラー"        TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "入外区分入力エラー"        TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "表示条件入力エラー"        TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "提出先入力エラー"          TO  SPA-ERRMSG
      *
               WHEN    "0100"
                   MOVE    "患者番号が未入力です"      TO  SPA-ERRMSG
               WHEN    "0101"
                   MOVE    "該当診療年月のレセプトがありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0102"
                   MOVE    "主科が未設定となります"    TO  SPA-ERRMSG
               WHEN    "0103"
                   MOVE    "患者番号選択中です"        TO  SPA-ERRMSG
      *
      *
               WHEN    "1001"
                   MOVE    "主科マスタが更新できませんでした"
                                                       TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "主科マスタが削除できませんでした"
                                                       TO  SPA-ERRMSG
               WHEN    "7777"
                   MOVE    WRK-RECEERR                 TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  RERR
           MOVE    SPA-ERRCD            TO  RERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  RERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "R97"                TO  SPA-MOTOPG
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
      *    エラーメッセージ表示理
      *****************************************************************
       520-RIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-RIDMSG
      *
           EVALUATE    SPA-RIDCD
               WHEN    "1001"
                   MOVE
              "選択された主科は、診療がありません。更新しますか"
                                               TO  WRK-RIDMSG
               WHEN    "1002"
                   STRING   SPA-NAI-SRYYMH     DELIMITED   BY  SIZE
                            "　"               DELIMITED   BY  SIZE
                            SPA-NAI-NYUGAIKBN  DELIMITED   BY  SPACE
                            "分一括取得処理を行います"
                                               DELIMITED   BY  SIZE  
                                               INTO    WRK-RIDMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE                TO  RID1
           INITIALIZE                       RID1
           MOVE    SPA-RIDCD            TO  RID1-ID1CODE
           MOVE    WRK-RIDMSG           TO  RID1-ID1MSG
           EVALUATE    SPA-RIDCD
               WHEN    "1002"
                   MOVE    "戻る"       TO  RID1-B01-LABEL
               WHEN    "1001"
                   MOVE    "ＮＯ"       TO  RID1-B01-LABEL
           END-EVALUATE        
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "R97"                TO  SPA-MOTOPG
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
      *    診療科編集処理
      *****************************************************************
       9001-SRYKA-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE              TO  WRK-SRYKAMEI
                                          WRK-SRYKAMEITAN
      *
           IF      WRK-SRYKA          =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING SRYKA-IDX  FROM    1   BY  1
                       UNTIL   SRYKA-IDX  >       SPA-GMN-SYUKA-MAX
                   IF      WRK-SRYKA      =   SPA-GMN-SYUKAL (SRYKA-IDX)
                       MOVE    SPA-GMN-SYUKAMEIL (SRYKA-IDX)
                                               TO  WRK-SRYKAMEI
                       MOVE    SPA-NAI-SYUKAMEIL (SRYKA-IDX)
                                               TO  WRK-SRYKAMEITAN
                       MOVE    SPA-GMN-SYUKA-MAX
                                               TO  SRYKA-IDX
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       9001-SRYKA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                      PTINF-REC
               END-IF
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
      *    主科公費情報読込
      *****************************************************************
       900-SYUKA-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
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
      *    主科公費情報読込
      *****************************************************************
       910-SYUKA-NEXT-SEC         SECTION.
      * 
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC               =   ZERO
               MOVE    ZERO                 TO  FLG-SYUKA
               MOVE    MCPDATA-REC          TO  SYUKA-REC
           ELSE
               MOVE    1                    TO  FLG-SYUKA
           END-IF
      *
           .
       910-SYUKA-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科公費情報読込
      *****************************************************************
       910-SYUKA-KEY10-NEXT-SEC    SECTION.
      * 
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC               =   ZERO
               MOVE    ZERO                 TO  FLG-SYUKA
               MOVE    MCPDATA-REC          TO  SYUKA-REC
               IF       SYUKA-TEISYUTUSAKI  =   "1" OR  "2" OR  "6"
                   CONTINUE
               ELSE
                   GO  TO  910-SYUKA-KEY10-NEXT-SEC
               END-IF
           ELSE
               MOVE    1                    TO  FLG-SYUKA
           END-IF
      *
           .
       910-SYUKA-KEY10-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
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
      *    ＰＵＴ処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
