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
       PROGRAM-ID.             ORCHC03V03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 外来請求書兼領収書　印刷
      *                      自費１０項目対応（ＨＣＭ０３Ｖ０３）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/07/08    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  06/05/15  MONFUNC 対応
      *  01.00.02    NACL-太田    06/08/22  パラメタテーブルより編集データ
      *                                     を参照する区分を追加
      *                                     月まとめ請求書発行機能追加
      *  03.03.00    NACL-多々納  06/10/02  返金対応追加
      *  03.04.01    NACL-森脇    07/01/17  医療機関名称、住所セット変更
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  03.05.00    NACL-多々納  07/05/22  再印刷処理追加
      *  03.05.00    NACL-太田    07/08/03  月別発行実日数記載追加
      *  03.05.01    NACL-多々納  07/08/06  訂正時合計対応
      *  04.01.00    NACL-太田    07/10/22  公務災害対応
      *  04.02.00    NACL-太田    08/03/06  改正対応
      *  04.03.00    NACL-太田    08/09/17  改正対応
      *  04.04.00    NACL-森脇    09/01/14  プログラムオプション対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
@@@@@@*    アサイン名を変更
      *    SELECT  PRT-FILE        ASSIGN  HC01PARA
           SELECT  PRT-FILE        ASSIGN  ASGNPARA
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
       01  PRT-REC                 PIC X(10000).
      *
       WORKING-STORAGE             SECTION.
      *
       01  WKPRT-REC               PIC X(10000).
      *
           COPY    "HCM03V03B.INC" REPLACING   //HCM03V03B//
                                   BY          //HC03//.
      *
           COPY    "HCM03V02B.INC" REPLACING   //HCM03V02B//
                                   BY          //HC03V02//.
      *
           COPY    "HCM03B.INC"    REPLACING   //HCM03B//
                                   BY          //HC03V01//.
      *
           COPY    "CPFMSEIKYU.INC".
      *
      *     印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
@@@@@@*    ファイル名編集用のコピー句を追加
           COPY    "CPASGNPARA.INC".
      *    シェル用領域
      **** COPY    "CPCOMMONSHELL.INC".
      *
      *    シェル用領域
      *    COPY    "CPSHELLTBL.INC".
      *****COPY    "ORCA-DBPATH".
      *
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
      *
           03  FLG-SP              PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-HIZUKECHK       PIC 9(01).
           03  FLG-JOIN            PIC 9(01).
           03  FLG-EDIT            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDXA                PIC 9(05).
           03  IDXD                PIC 9(05).
           03  IDX-ACT             PIC 9(02).
           03  IDX-REN             PIC 9(04).
           03  IDX-JYU             PIC 9(04).
           03  IDX-JIHI            PIC 9(04).
      *
           03  IDX-SYU             PIC 9(04).
           03  IDH3                PIC 9(04).
      *
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
           03  IDXP                PIC 9(05).
           03  IDXM                PIC 9(02).
           03  IDK                 PIC 9(04).
           03  IDX2-1              PIC 9(01).
           03  IDX-MEMO            PIC 9(04).
           03  IDXC                PIC 9(04).
           03  IDX-CM              PIC 9(04).
           03  IDX-API             PIC 9(05).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-DATE.
               05  WRK-DATE-YY         PIC 9(04).
               05  WRK-DATE-FL1        PIC X(01).
               05  WRK-DATE-MM         PIC 9(02).
               05  WRK-DATE-FL2        PIC X(01).
               05  WRK-DATE-DD         PIC 9(02).
           03  WRK-YMD             PIC X(10).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WYY         PIC X(03).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-HKNCOMBIMEI     PIC X(40).
      *
           03  WRK-SKYMONEY        PIC S9(11).
           03  WRK-MISYUMONEY      PIC S9(11).
           03  WRK-MISYU           PIC S9(11).
           03  WRK-GOKTEN          PIC S9(11).
           03  WRK-SKYMONEY-TAX-SAI    PIC S9(11).
      *
           03  WRK-DENPNUM             PIC 9(07).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-HKNCOMBI            PIC X(04).
      *
           03  WRK-WARI            PIC 9(02)V9.
      *****03  WRK-Z6              PIC ZZZZZZ.
      *****03  WRK-Z62             PIC ZZZ9.9.
           03  WRK-HKNFTN          PIC ZZ9.
           03  WRK-HKNFTN2         PIC Z9.9.
      *
           03  WRK-ZZZ             PIC ZZZ.
           03  WRK-NO              PIC Z(7).
           03  WRK-Z7-G.
               05  WRK-FIL             PIC X(09).
               05  WRK-Z7              PIC Z,ZZZ,ZZZ.
           03  WRK-Z72-G.
               05  WRK-FIL2            PIC X(18).
           03  WRK-Z72                 PIC S9(07).
           03  WRK-Z72-H               PIC Z,ZZZ,ZZZ.
           03  WRK-Z72-HS              REDEFINES   WRK-Z72-H
                                       PIC -,---,---.
           03  WRK-Z72-H9              PIC Z,ZZZ,ZZ9.
           03  WRK-MZ                  PIC --------.
           03  WRK-M9                  PIC -------9.
      *    自費集計
           03  WRK-JIHI-REC.
               05  WRK-JIHI            PIC S9(07)   OCCURS 10.
           03  WRK-JIHI-TOTAL          PIC S9(08).
           03  WRK-JIHIMEI-REC.
               05  WRK-JIHIMEI         PIC X(20)   OCCURS 10.
      *
      *    複数科まとめ集計
           03  WRK-HKNTEN-KIN              PIC 9(07).
           03  WRK-HKNTEN-KEI              PIC 9(07)V99.
           03  WRK-NYUKIN                  PIC S9(07).
      *
           03  WRK-NYUKIN-TOTAL            PIC S9(07).
           03  WRK-ZENMISYU                PIC S9(07).
      *
           03  WRK-SRYKA-MEI               PIC X(30).
           03  WRK-PAGE                    PIC 9(03).
           03  WRK-MEMOID-G.
               05  WRK-MEMOID              PIC X(04).
               05  WRK-MEMOIDNO            PIC 9(02).
      *
           03  WRK-PARA-NAME               PIC X(25).
           03  WRK-PARA                    PIC X(100).
      *
      *    文字変換
           03  WRK-ZENKAKU-G.
               05  WRK-ZENKAKU         PIC X(02)   OCCURS   10.
           03  WRK-MOJI                PIC X(10).
           03  WRK-MOJISU              PIC 9(02).
           03  WRK-LBLSRYYMD           PIC X(10).
           03  WRK-SRYYMDG             PIC X(50).
           03  WRK-STSRYYMDG           PIC X(22).
           03  WRK-EDSRYYMDG           PIC X(22).
           03  WRK-JNISSU              PIC ZZ9.
      *
      *----(03.04.01)--ADD-START---
           03  WRK-HOSPNAME            PIC X(100).
           03  WRK-HOSPNAME-R          REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1       PIC X(50).
               05  WRK-HOSPNAME2       PIC X(50).
           03  WRK-HOSPADRS            PIC X(100).
           03  WRK-HOSPADRS-R          REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1       PIC X(50).
               05  WRK-HOSPADRS2       PIC X(50).
           03  WRK-HOSPTEL             PIC X(15).
      *----(03.04.01)--ADD-END-----
           03  WRK-FORMID              PIC X(30).
           03  WRK-FILEMEI.
               05  WRK-FILEMEI-YMD     PIC X(08).
               05  WRK-FILEMEI-HMS     PIC X(06).
               05  WRK-FILEMEI-CD      PIC X(06).
           03  WRK-HOSPPOST            PIC X(07).
           03  WRK-API-HOSPNAME-G.
               05  WRK-API-HOSPNAME    PIC X(120)
                                       OCCURS 3.
           03  WRK-API-HOSPADRS-G.
               05  WRK-API-HOSPADRS    PIC X(120)
                                       OCCURS 3.
           03  WRK-SRYKIKANKBN         PIC X(01).
           03  WRK-COMMNT.
               05  WRK-COM-STR         PIC X(09).
               05  WRK-COM-FIL         PIC X(01).
               05  WRK-COM-END         PIC X(09).
           03  WRK-API-GRPNUM          PIC 9(03).
           03  WRK-API-GRPNUM-HEAD     PIC 9(03).
           03  WRK-H-NAME.
               05  WRK-H-NAME1             PIC X(74).
               05  WRK-H-NAME2             PIC X(74).
           03  WRK-KINGAKU-X.
               05  WRK-KINGAKU-Z1      PIC -(8)9.
      *
       01  TAPI-AREA.
           03  TAPI-MEISAI-G.
               05  TAPI-HKNTEN-KBN     PIC X(03)
                                       OCCURS 16.
           03  TAPI-CHOSEI1            PIC X(08).
           03  TAPI-CHOSEI2            PIC X(08).
      *
      *    プログラムオプションパラメタ
       01  TPARA-AREA.
           03  TPARA-SRYKA              PIC X(01).
           03  TPARA-HKNMEI             PIC X(01).
           03  TPARA-FTNKBN             PIC X(01).
           03  TPARA-HAKKOUBIKBN        PIC X(01).
           03  TPARA-TEISEIKBN          PIC X(01).
           03  TPARA-KANANAME           PIC X(01).
           03  TPARA-MISYUKBN1          PIC X(01).
           03  TPARA-MISYUKBN2          PIC X(01).
           03  TPARA-YUKOSTYMD          PIC X(08).
           03  TPARA-YUKOEDYMD          PIC X(08).
           03  TPARA-MEMO               PIC X(80) OCCURS 10.
           03  TPARA-CHUI-YUKOSTYMD     PIC X(08).
           03  TPARA-CHUI-YUKOEDYMD     PIC X(08).
           03  TPARA-CHUI               PIC X(100).
           03  TPARA-API-DATA           PIC X(1).
           03  TPARA-API-CUSTOM-ID      PIC X(20).
      *    以下V5用
           03  TPARA-GOKEITANI          PIC X(1).
           03  TPARA-SYOKEI             PIC X(1).
           03  TPARA-RCHUI              PIC X(100).
           03  TPARA-MCHUI              PIC X(100).
           03  TPARA-PARA-COM-HYOJI-G.
               05  TPARA-COM-HYOJI      PIC X(20)  OCCURS  5.
           03  TPARA-JIHIKBN            PIC X(01).
      *
       01  TBL-COM-HYOJI-AREA.
           03  TBL-COM-HYOJI-OCC       OCCURS   5.
               05  TBL-COM-STR             PIC X(09).
               05  TBL-COM-END             PIC X(09).
      *
       01  CONST-AREA.
           03  CONST-H200401           PIC X(08)   VALUE "20080401".
           03  CONST-H201001           PIC X(08)   VALUE "20081001".
           03  CONST-MEMO-MAX          PIC 9(05)   VALUE   10.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報−基本情報 
           COPY    "CPSK1001.INC".
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSK1013.INC".
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *----(03.04.01)--ADD-START---
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *----(03.04.01)--ADD-END-----
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    保存用収納ワーク
       01  WRK-SYUNOU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING
                                     //SYU-// BY //WRK-SYU-//.
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
           COPY    "CPSYUPARA01.INC".
           COPY    "CPSYUPARA03.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSGDAY.INC".
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *    請求書収納パラメタ
           COPY    "CPORCHC03SUB.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
@@@@@@*    サブプログラムのコピーを追加
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    API対応
           COPY    "CPORCRPMAIN.INC".
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    明細書作成サブ
           COPY    "CPORCHC04SUB.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    請求書パラメタ
           COPY    "CPORCHC03.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHC03AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  TBL-COM-HYOJI-AREA
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    各ＤＢ読込
           PERFORM 100-SYORI-SEC
      *
           IF    ( ORCHC03-PARAFLG     =   1 )
                   PERFORM 203-PARA-SYUNOU-SEC
           ELSE
               IF      ORCHC03-HAKHOUFLG   =   ZERO
      *            対象の伝票番号分のみ
                   PERFORM 201-ONLY-SYUNOU-SEC
               ELSE
      *            発行区分による
                   PERFORM 202-KAKHOU-SYUNOU-SEC
               END-IF
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    帳票印刷開始処理
      *****************************************************************
       400-SYOKI-PRINT-SEC             SECTION.
      *
           ADD     1                   TO  WRK-PAGE
      *
           IF    ( TPARA-API-DATA      NOT =   "1" )
      *        ファイルＯＰＥＮ
               MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
               MOVE    "HC03"              TO  HC01PARA-FORM-ID
               MOVE    SPA-TERMID          TO  HC01PARA-TERMID
               MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
      *        変更★★★★★★
               MOVE    WRK-PAGE            TO  HC01PARA-CNT
      *
@@@@@@*        サブプログラムより一時ファイル名の編集を行う。
@@@@@@*        返却値をASGNPARAに設定する。
               INITIALIZE                      SGETTEMP-AREA
               MOVE    HC01PARA            TO  SGETTEMP-BASENAME
               CALL    "ORCSGETTEMP"       USING
                                           SGETTEMP-AREA
               MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *
               OPEN    OUTPUT              PRT-FILE
           END-IF
      *
           .
       400-SYOKI-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       400-PRINT-SEC                   SECTION.
      *
           IF    ( TPARA-API-DATA      NOT =   "1" )
               PERFORM 4000-PRINT-SEC
           ELSE
               PERFORM 4000-API-PRINT-SEC
           END-IF
      *
           .
       400-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       4000-PRINT-SEC                  SECTION.
      *
            MOVE     WKPRT-REC         TO  PRT-REC
      *
      *　　印刷用データ出力
              WRITE  PRT-REC
      *
      ******MOVE   "HC01.sh"           TO  SHELL-ID
      *     MOVE   "HC03.dia"          TO  SHELL-DIA
      *     MOVE   "HC03.def"          TO  SHELL-DEF
      *     MOVE   HC01PARA            TO  SHELL-DAT
      ***** MOVE   SYS-1031-PRTNM(3)   TO  SHELL-PRTNM 
            CLOSE                       PRT-FILE
      *
            INITIALIZE                 ORCSMKPRTAREA
            MOVE   "HC01.sh"           TO  MKPRT-ID
      ***** MOVE   "HC03.dia"          TO  MKPRT-DIA
      ***** MOVE   "HC03.def"          TO  MKPRT-DEF
            MOVE   WRK-FORMID          TO  MKPRT-DIA
            MOVE   SPACE               TO  MKPRT-DEF
@@@@@@*     アサイン名の編集項目を変更
            MOVE   ASGNPARA            TO  MKPRT-DAT
            MOVE   SYS-1031-PRTNM(3)   TO  MKPRT-PRTNM
            IF    ( ORCHC03-PARAFLG     =   1 )
             AND  ( ORCHC03-PARA-GYOUMUID   =   "SU1" )
                MOVE   "請求書兼領収書（修正）"
                                       TO  MKPRT-INFO
            ELSE
                MOVE   "請求書兼領収書"
                                       TO  MKPRT-INFO
            END-IF
            CALL   "ORCSMKPRT1"        USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
      *   印刷用スクリプト処理
      *★   ETC-EXEC SHELL   SHELL-AREA 10
      *    MOVE    SHELL-NAME          TO  SHELLTBL-NAME
      *    MOVE    SHELL-ARG1          TO  SHELLTBL-ARG1
      *    MOVE    SHELL-ARG2          TO  SHELLTBL-ARG2
      *    MOVE    SHELL-ARG3          TO  SHELLTBL-ARG3
      *    MOVE    SHELL-ARG4          TO  SHELLTBL-ARG4
      *    MOVE    SHELL-ARG5          TO  SHELLTBL-ARG5
      *    MOVE    SHELLTBL            TO  MCPDATA-REC
      *    MOVE    "SHELL"             TO  MCP-FUNC
      *    MOVE    PATH-SHELL-SHELL    TO  MCP-PATH
      *    CALL    "MCPSUB"            USING
      *            MCPAREA
      **********   MCPDATA-REC
      *
           .
       4000-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       4000-API-PRINT-SEC              SECTION.
      *
      *    カスタムＩＤ
           MOVE    TPARA-API-CUSTOM-ID  TO  FMSEIKYU-CUSTOM-ID
      *
           INITIALIZE                      RPMAIN-AREA
           MOVE    "seikyusho"         TO  RPMAIN-ID
           MOVE    TPARA-API-CUSTOM-ID  TO  RPMAIN-CUSTOM-ID
           MOVE    "請求書兼領収書"    TO  RPMAIN-TITLE
           MOVE    "fm_invoice_receipt"
                                       TO  MCP-TABLE
           MOVE    FMSEIKYU-DATA       TO  RPMAIN-DATA
           CALL    "ORCRPMAIN"         USING
                                       MCPAREA
                                       RPMAIN-AREA
                                       SPA-AREA
      *
           .
       4000-API-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理 (印刷ＤＢに出力）
      *****************************************************************
       401-PRINT-SEC                SECTION.
      *
           MOVE    ORCHC03-PRTAREA     TO  ORCSPRTAREA
      *
           MOVE    WRK-FORMID          TO  SPRT-PRTID
           MOVE    WKPRT-REC           TO  SPRT-PRTDATA
      *-----------------------------------------------------------------
           MOVE    "1"                 TO  SPRT-SITEKBN
      *    カスタマイズＰＧの場合"2"をセットすること。
      *    MOVE    "2"                 TO  SPRT-SITEKBN
      *-----------------------------------------------------------------
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF    ( SPRT-RETURN         =   ZERO )
               CONTINUE
           ELSE
               DISPLAY "ORCSPRT ERR: PTID = " SPRT-PTID
           END-IF
      *
           .
       401-PRINT-EXT.
           EXIT.
      *****************************************************************
      *   対象の伝票番号分のみ処理
      *****************************************************************
       201-ONLY-SYUNOU-SEC                SECTION.
      *
      *    収納マスタ検索
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    ORCHC03-DENPNUM     TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    診療科名編集
           MOVE    SPACE                TO  WRK-SRYKA-MEI
           MOVE    SYU-SRYKA            TO  WRK-SRYKA
           PERFORM 1000-SRYKAMEI-HEN-SEC
      *    保険組合せ
           PERFORM 1001-HKNCOMBI-HEN-SEC
      *
           MOVE    ORCHC03-DENPNUM     TO  WRK-DENPNUM
           MOVE    ORCHC03-SEIKYU      TO  WRK-SKYMONEY
           MOVE    ORCHC03-NYUKIN      TO  WRK-NYUKIN
           MOVE    ORCHC03-SEIKYU-TAX-SAI  TO  WRK-SKYMONEY-TAX-SAI
      **** MOVE    ORCHC03-ZENMISYU    TO  WRK-MISYUMONEY
      *---(03.03.00) LINE ADD  START -----------------------------------
           MOVE    ORCHC03-ZENMISYU-2  TO  WRK-MISYUMONEY
      *    前回請求額
           IF     (ORCHC03-ZEN-HENKIN  >   ZERO ) OR
                  (ORCHC03-KON-HENKIN (1)  >   ZERO )
      *        今回返金額
               COMPUTE WRK-MISYUMONEY      =   WRK-MISYUMONEY
                                           -   ORCHC03-ZEN-HENKIN
                                           -   ORCHC03-KON-HENKIN
                                                             (1)
           ELSE
      *        前回までの超過額
               COMPUTE WRK-MISYUMONEY      =   WRK-MISYUMONEY
                                           -   ORCHC03-KANYUKIN
               IF      WRK-MISYUMONEY      <   ZERO
                   MOVE    ZERO            TO  WRK-MISYUMONEY
               END-IF
           END-IF
      *
           PERFORM 2011-MISYU-OPTION-CHK-SEC
      *
      *    返金対応
           IF      (ORCHC03-HAKKOFLG   =   ZERO )  AND
                   (ORCHC03-NYUKIN     =   ZERO )  AND
                   (ORCHC03-ZEN-HENKIN >   ZERO )  AND
                   (WRK-MISYUMONEY     <   ZERO )  AND
                   ((WRK-MISYUMONEY * -1 )
                                       >   WRK-SKYMONEY)
      *        返金額が今回請求額より多い時、マイナス入金とする
               COMPUTE WRK-NYUKIN          =   WRK-SKYMONEY
                                           +   WRK-MISYUMONEY
           END-IF
      *---(03.03.00) LINE ADD  END   -----------------------------------
      *---(03.05.01) LINE ADD  START -----------------------------------
      *    訂正時に合計金額表示
           IF     (ORCHC03-TEIKIN-KBN  =   1   )  AND
                  (ORCHC03-HAKKOFLG    =   2   )
               MOVE    SYU-SKYMONEY        TO  WRK-SKYMONEY
               MOVE    SYU-NYUKIN-TOTAL    TO  WRK-NYUKIN
               MOVE    SYU-SKYMONEY-TAX-SAI
                                           TO  WRK-SKYMONEY-TAX-SAI
           END-IF
      *---(03.05.01) LINE ADD  END   -----------------------------------
      *
           INITIALIZE                      SYUPARA03-AREA
           MOVE    1                   TO  SYUPARA03-GRP-DENPNUM-MAX
           MOVE    SYU-DENPNUM         TO  SYUPARA03-GRP-DENPNUM (1)
      *
      *    帳票印刷開始処理
           PERFORM 400-SYOKI-PRINT-SEC
      *
      *    帳票編集処理
           PERFORM 300-HC03-HEN-SEC
      *
      *    帳票印刷処理
           PERFORM 400-PRINT-SEC
      *
           .
       201-ONLY-SYUNOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプションによる前回未収額印字判定
      *****************************************************************
       2011-MISYU-OPTION-CHK-SEC       SECTION.
      *
           IF    ( ORCHC03-PARAFLG     =   ZERO )
               IF    ( ORCHC03-MOTOID      =   ZERO )
                  AND( TPARA-MISYUKBN1     =   ZERO )
                   IF    ( ORCHC03-MATOME-NYUHENKIN-KBN
                                       =   SPACE )
                       MOVE    ZERO    TO  WRK-MISYUMONEY
                   END-IF
               END-IF
      *
               IF    ( ORCHC03-MOTOID      =   3 )
                  AND( TPARA-MISYUKBN2     =   ZERO )
                   IF    ( ORCHC03-MATOME-NYUHENKIN-KBN
                                       =   SPACE )
                       MOVE    ZERO    TO  WRK-MISYUMONEY
                   END-IF
               END-IF
           END-IF
      *
           .
       2011-MISYU-OPTION-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       100-SYORI-SEC                SECTION.
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
      *    システム管理検索
           MOVE    SPA-SRYYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1001-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1001-REC
           ELSE
               INITIALIZE                  SYS-1001-REC
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1002-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1002-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1002-REC
           ELSE
               INITIALIZE                  SYS-1002-REC
           END-IF
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
      *
      *    労災医療機関情報編集
           MOVE    SPACE               TO  SYS-4001-REC
           INITIALIZE                  SYS-4001-REC
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-4001-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-4001-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-4001-HOSPNUM
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-4001-REC
           ELSE
               INITIALIZE                  SYS-4001-REC
           END-IF
      *
      *    自費名称編集
           PERFORM 1002-JIHIMSG-HEN-SEC
      *
      *----(03.04.01)--ADD-START---
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
                                           WRK-API-HOSPNAME (1)
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
                                           WRK-API-HOSPADRS (1)
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
           MOVE    SYS-1002-POST       TO  WRK-HOSPPOST
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *----(03.04.01)--ADD-END-----
      *
           PERFORM 900-PTINF-READ-SEC
      *
           PERFORM 100-OPTION-SELECT-SEC
      *
           PERFORM 800-MCNDATE-SEC
      *
           .
       100-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    自費名称編集処理
      *****************************************************************
       1002-JIHIMSG-HEN-SEC             SECTION.
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    SPA-SRYYMD          TO  SYS-1013-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1013-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               INITIALIZE                  SYS-1013-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           INITIALIZE                  WRK-JIHIMEI-REC
           PERFORM
               UNTIL       FLG-SYSKANRI    =   1
               MOVE    MCPDATA-REC         TO  SYS-1013-REC
               IF      SPA-NYUGAIKBN       =   SYS-1013-KBNCD(1:1)
                   EVALUATE    SYS-1013-KBNCD(2:1)
                       WHEN    "1"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (1)
                       WHEN    "2"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (2)
                       WHEN    "3"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (3)
                       WHEN    "4"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (4)
                       WHEN    "5"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (5)
                       WHEN    "5"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (5)
                       WHEN    "6"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (6)
                       WHEN    "7"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (7)
                       WHEN    "8"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (8)
                       WHEN    "9"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (9)
                       WHEN    "0"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                   (10)
                   END-EVALUATE
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       1002-JIHIMSG-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       1001-HKNCOMBI-HEN-SEC             SECTION.
      *
      *    保険組合せマスター検索
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
           PERFORM 910-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   1
                DISPLAY "*** HKNCOMBI  ERR:"   COMB-KEY "."
           END-IF
      *
           PERFORM 910-PTHKNINF-READ-SEC
      *
           IF    ( COMB-HKNNUM         =   "971" OR "973" )
      *
      *        患者労災保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
               MOVE    SPA-PTID            TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               PERFORM 910-PTRSIINF-READ-SEC
           ELSE
               INITIALIZE                      PTRSIINF-REC
           END-IF
      *
           PERFORM 10011-HKNCOMBIMEI-HEN-SEC
      *
           .
       1001-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       10011-HKNCOMBIMEI-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
      *
           IF    ( COMB-HKNNUM         =   "971" )
            AND  ( PTRSI-HKNKBN        =    "5" )
               MOVE    PTHKN-KIGO      TO  WRK-HKNCOMBIMEI
           ELSE
      *
               IF    ( SYU-SRYYMD      >=      CONST-H201001 )
                AND  ( COMB-HKNNUM      =      "001" )
                   INSPECT COMB-SYU-TANSEIDONAME
                           REPLACING    ALL    "政管"   BY  "協会"
               END-IF
      *
               STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                       "　"                    DELIMITED  BY  SIZE
                       COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       "　"                    DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       "　"                    DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       "　"                    DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                             INTO              WRK-HKNCOMBIMEI
               END-STRING
           END-IF
      *
           .
       10011-HKNCOMBIMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科名編集処理
      *****************************************************************
       1000-SRYKAMEI-HEN-SEC                SECTION.
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
               IF      WRK-SRYKA-MEI       =   SPACE
                   MOVE    SYS-1005-SRYKANAME2
                                               TO  WRK-SRYKA-MEI
               ELSE
                   MOVE    ZERO                TO  FLG-JOIN
                   IF    ( TPARA-API-DATA      =   "1" )
                       IF    ( WRK-SRYKA-MEI(29:)  =   SPACE )
                           MOVE    1           TO  FLG-JOIN
                       END-IF
                   ELSE
                       IF    ( WRK-SRYKA-MEI(19:)  =   SPACE )
                           MOVE    1           TO  FLG-JOIN
                       END-IF
                   END-IF
      *
                   IF    ( FLG-JOIN            =   1 )
                       STRING  WRK-SRYKA-MEI   DELIMITED  BY  SPACE
                               "、"            DELIMITED  BY  SIZE
                               SYS-1005-SRYKANAME2
                                               DELIMITED  BY  SPACE
                               INTO                WRK-SRYKA-MEI
                       END-STRING
                   END-IF
               END-IF
           ELSE
               INITIALIZE                  SYS-1005-REC
           END-IF
      *
           .
       1000-SRYKAMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           INITIALIZE                  TPARA-AREA
           MOVE    "1"             TO  TPARA-SRYKA
           MOVE    "1"             TO  TPARA-HKNMEI
           MOVE    "2"             TO  TPARA-FTNKBN
           MOVE    "0"             TO  TPARA-HAKKOUBIKBN
           MOVE    "1"             TO  TPARA-TEISEIKBN
           MOVE    "0"             TO  TPARA-KANANAME
           MOVE    "1"             TO  TPARA-MISYUKBN1
           MOVE    "1"             TO  TPARA-MISYUKBN2
           MOVE    "00000000"      TO  TPARA-YUKOSTYMD
           MOVE    "99999999"      TO  TPARA-YUKOEDYMD
           MOVE    "00000000"      TO  TPARA-CHUI-YUKOSTYMD
           MOVE    "99999999"      TO  TPARA-CHUI-YUKOEDYMD
           MOVE    "0"             TO  TPARA-API-DATA
           MOVE    "0"             TO  TPARA-GOKEITANI
           MOVE    "1"             TO  TPARA-SYOKEI
           MOVE    "0"             TO  TPARA-JIHIKBN
      *
      *    診療科名
           MOVE    "SRYKA"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA        =   "0" OR "1" )
               MOVE    WRK-PARA    TO  TPARA-SRYKA
           END-IF
      *
      *    保険名称
           MOVE    "HKNMEI"        TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA        =   "0" OR "1" )
               MOVE    WRK-PARA    TO  TPARA-HKNMEI
           END-IF
      *
      *    負担割合
           MOVE    "FTNKBN"        TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA        =   "0" OR "1" OR "2" )
               MOVE    WRK-PARA    TO  TPARA-FTNKBN
           END-IF
      *
      *    伝票発行日区分
           MOVE    "HAKKOUBIKBN"   TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA        =   "0" OR "1" )
               MOVE    WRK-PARA    TO  TPARA-HAKKOUBIKBN
           END-IF
      *
      *    （訂正）表示
           MOVE    "TEISEIKBN"     TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA        =   "0" OR "1" )
               MOVE    WRK-PARA    TO  TPARA-TEISEIKBN
           END-IF
      *
      *    カナ氏名
           MOVE    "KANANAME"      TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA        =   "0" OR "1" )
               MOVE    WRK-PARA    TO  TPARA-KANANAME
           END-IF
      *
      *    前回未収額（診療行為業務）
           MOVE    "MISYUKBN1"     TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA        =   "0" OR "1" )
               MOVE    WRK-PARA    TO  TPARA-MISYUKBN1
           END-IF
      *
      *    前回未収額（収納業務）
           MOVE    "MISYUKBN2"     TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA        =   "0" OR "1" )
               MOVE    WRK-PARA    TO  TPARA-MISYUKBN2
           END-IF
      *
      *    有効開始日
           MOVE    "YUKOSTYMD"     TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA        TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           IF    ( FLG-HIZUKECHK   =   ZERO )
               MOVE    WRK-SYMD    TO  TPARA-YUKOSTYMD
           END-IF
      *
      *    有効終了日
           MOVE    "YUKOEDYMD"     TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA        TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           IF    ( FLG-HIZUKECHK   =   ZERO )
               MOVE    WRK-SYMD    TO  TPARA-YUKOEDYMD
           END-IF
      *
           PERFORM VARYING IDX-MEMO    FROM    1   BY  1
                   UNTIL ( IDX-MEMO    >   CONST-MEMO-MAX )
      *
      *        メモ欄
               MOVE    "MEMO"          TO  WRK-MEMOID
               MOVE    IDX-MEMO        TO  WRK-MEMOIDNO
               MOVE    WRK-MEMOID-G    TO  WRK-PARA-NAME
               PERFORM 1001-OPTION-CALL-SEC
               MOVE    WRK-PARA        TO  TPARA-MEMO (IDX-MEMO)
      *
           END-PERFORM
      *
      *    注意書き有効開始日
           MOVE    "CHUI_YUKOSTYMD" TO WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA        TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           IF    ( FLG-HIZUKECHK   =   ZERO )
               MOVE    WRK-SYMD    TO  TPARA-CHUI-YUKOSTYMD
           END-IF
      *
      *    注意書き有効終了日
           MOVE    "CHUI_YUKOEDYMD" TO WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA        TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           IF    ( FLG-HIZUKECHK   =   ZERO )
               MOVE    WRK-SYMD    TO  TPARA-CHUI-YUKOEDYMD
           END-IF
      *
      *    注意書き
           MOVE    "CHUI"          TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA        TO  TPARA-CHUI
      *
      *
      *    API_DATA
           IF    ( ORCHC03-SIZE    =   "V1" OR "V2" )
            OR   ( ORCHC03-PRTKBN  =   "1" )
               CONTINUE
           ELSE
               MOVE    "API_DATA"      TO  WRK-PARA-NAME
               PERFORM 1001-OPTION-CALL-SEC
               IF    ( WRK-PARA        =   "1" )
                   MOVE    WRK-PARA    TO  TPARA-API-DATA
               END-IF
           END-IF
      *
           IF     ( SPA-API-FLG    NOT =   ZERO )
               IF    ( SPA-PRTCONF     =   "2" OR "3" )
                   MOVE    ZERO    TO  TPARA-API-DATA
               ELSE
                   MOVE    "1"     TO  TPARA-API-DATA
               END-IF
           END-IF
      *
      *    API_CUSTOM_ID
           MOVE    "API_CUSTOM_ID" TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA        TO  TPARA-API-CUSTOM-ID
      *
      *    以下V5用
      *    合計単位
           MOVE    "GOKEITANI"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA            =   "0" OR "1" )
               MOVE    WRK-PARA        TO  TPARA-GOKEITANI
           END-IF
      *    小計
           MOVE    "SYOKEI"            TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA            =   "0" OR "1" )
               MOVE    WRK-PARA        TO  TPARA-SYOKEI
           END-IF
      *    注意書き
           MOVE    "RCHUI"             TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA            TO  TPARA-RCHUI
      *    注意書き
           MOVE    "MCHUI"             TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA            TO  TPARA-MCHUI
           MOVE    "COM_HYOUJI_1"      TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA            TO  TPARA-COM-HYOJI (1)
           MOVE    "COM_HYOUJI_2"      TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA            TO  TPARA-COM-HYOJI (2)
           MOVE    "COM_HYOUJI_3"      TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA            TO  TPARA-COM-HYOJI (3)
           MOVE    "COM_HYOUJI_4"      TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA            TO  TPARA-COM-HYOJI (4)
           MOVE    "COM_HYOUJI_5"      TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA            TO  TPARA-COM-HYOJI (5)
           MOVE    "JIHIKBN"           TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA            TO  TPARA-JIHIKBN
      *    コメント範囲編集
           PERFORM 1001-COMHYOJI-HEN-SEC
      *
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプションパラメタ取得処理
      *****************************************************************
       1001-OPTION-CALL-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-PARA
      *
           INITIALIZE                  ORCSPRGOPTIONAREA
           EVALUATE    ORCHC03-SIZE
           WHEN    "V1"
               MOVE    "ORCHC03"       TO  ORCSPRGOPTION-PRGID
           WHEN    "V2"
               MOVE    "ORCHC03V02"    TO  ORCSPRGOPTION-PRGID
           WHEN    "A5"
               MOVE    "ORCHC03V03A5"  TO  ORCSPRGOPTION-PRGID
           WHEN    "V5"
               MOVE    "ORCHC03V05"    TO  ORCSPRGOPTION-PRGID
           WHEN    OTHER
               MOVE    "ORCHC03V03"    TO  ORCSPRGOPTION-PRGID
           END-EVALUATE
           MOVE    WRK-PARA-NAME       TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
      *
           EVALUATE    WRK-PARA-NAME
           WHEN    "MEMO01"
           WHEN    "MEMO02"
           WHEN    "MEMO03"
           WHEN    "MEMO04"
           WHEN    "MEMO05"
           WHEN    "MEMO06"
           WHEN    "MEMO07"
           WHEN    "MEMO08"
           WHEN    "MEMO09"
           WHEN    "MEMO10"
               MOVE    ORCSPRGOPTION-OT-PARA (1:80)
                                       TO  KANACHK-MAE-INPUT
           WHEN    "CHUI"
           WHEN    "RCHUI"
           WHEN    "MCHUI"
               MOVE    ORCSPRGOPTION-OT-PARA (1:100)
                                       TO  KANACHK-MAE-INPUT
           WHEN    "YUKOSTYMD"
           WHEN    "YUKOEDYMD"
           WHEN    "CHUI_YUKOSTYMD"
           WHEN    "CHUI_YUKOEDYMD"
               MOVE    ORCSPRGOPTION-OT-PARA(1:10)
                                       TO  KANACHK-MAE-INPUT
           WHEN    OTHER
               MOVE    ORCSPRGOPTION-OT-PARA
                                       TO  KANACHK-MAE-INPUT
           END-EVALUATE
      *
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-PARA
      *
           .
       1001-OPTION-CALL-EXT.
           EXIT.
      *****************************************************************
      *    対象コメントコード編集処理
      *****************************************************************
       1001-COMHYOJI-HEN-SEC                SECTION.
      *
           MOVE    SPACE               TO  TBL-COM-HYOJI-AREA
           MOVE    ZERO                TO  IDX
           PERFORM VARYING    IDXC     FROM    1   BY  1
                   UNTIL      IDXC     >   5
               MOVE    TPARA-COM-HYOJI (IDXC)   TO  WRK-COMMNT
               PERFORM 10011-COMHYOJI-HENSYU-SEC
           END-PERFORM
           .
       1001-COMHYOJI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    対象コメントコード編集処理
      *****************************************************************
       10011-COMHYOJI-HENSYU-SEC                SECTION.
      *
           IF     (WRK-COM-STR(1:1)    =   "8"  )
               OR (WRK-COM-STR(1:3)    =   "008")
               ADD     1                   TO  IDX
               MOVE    WRK-COM-STR         TO  TBL-COM-STR (IDX)
               IF     (WRK-COM-END(1:1)    =   "8"  )
                   OR (WRK-COM-END(1:3)    =   "008")
                   MOVE    WRK-COM-END         TO  TBL-COM-END (IDX)
               ELSE
                   MOVE    WRK-COM-STR         TO  TBL-COM-END (IDX)
               END-IF
               IF      TBL-COM-STR (IDX)   >   TBL-COM-END (IDX)
                   MOVE    WRK-COM-STR         TO  TBL-COM-END (IDX)
               END-IF
           END-IF
           .
       10011-COMHYOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       300-HC03-HEN-SEC                SECTION.
      *
           INITIALIZE                  HC03
           INITIALIZE                  WKPRT-REC
           INITIALIZE                  WRK-JNISSU
           INITIALIZE                  TAPI-AREA
      *
      *    伝票番号
           MOVE    WRK-DENPNUM         TO  WRK-NO  
           MOVE    WRK-NO              TO  HC03-NO
      *    診療科
           IF    ( TPARA-SRYKA         =   "1" )
               MOVE    WRK-SRYKA-MEI(1:20) TO  HC03-MIDASI
           END-IF
      *    患者番号
           MOVE    SPA-PTNUM           TO  HC03-KNJNO
      *
      *    カナ名称
      *    名称
           IF    ( SYU-SRYYMD             >=   CONST-H200401 )
            AND  ( SPA-NAME (31:)      NOT =   SPACE )
      *
               MOVE    PTINF-KANANAME
                                       TO  HC03-KANANAME2
               MOVE    SPA-NAME        TO  HC03-KNJNAMW2
           ELSE
               MOVE    PTINF-KANANAME
                                       TO  HC03-KANANAME
               MOVE    SPA-NAME        TO  HC03-KNJNAMW
           END-IF
      *
           IF    ( TPARA-KANANAME      =   "1" )
               CONTINUE
           ELSE
               MOVE    SPACE           TO  HC03-KANANAME
                                           HC03-KANANAME2
           END-IF
      *
      *    保険種別
           MOVE    WRK-HKNCOMBIMEI TO  HC03-HOKEN
      *    負担割合
           IF    ( SYU-SRYYMD         >=   CONST-H200401 )
               EVALUATE    TPARA-FTNKBN
               WHEN    "1"
                       MOVE    "％"                TO  HC03-LBLFTNRATE
                       MOVE    SYU-PTFTNRATE       TO  WRK-HKNFTN
                       MOVE    WRK-HKNFTN          TO  HC03-FUTAN
               WHEN    "2"
                   MOVE    "割"            TO  HC03-LBLFTNRATE
                   COMPUTE WRK-WARI        =   SYU-PTFTNRATE   /   10
                   IF      SYU-PTFTNRATE(3:1)  =   ZERO
                       MOVE    WRK-WARI            TO  WRK-HKNFTN
                       MOVE    WRK-HKNFTN          TO  HC03-FUTAN
                   ELSE
                       MOVE    WRK-WARI            TO  WRK-HKNFTN2
                       MOVE    WRK-HKNFTN2         TO  HC03-FUTAN
                   END-IF
               END-EVALUATE
           ELSE
                   COMPUTE WRK-WARI        =   SYU-PTFTNRATE   /   10
                   IF      SYU-PTFTNRATE(3:1)
                                           =   ZERO
                       MOVE    WRK-WARI    TO  WRK-HKNFTN
                       MOVE    WRK-HKNFTN  TO  HC03-FUTAN
                   ELSE
                       MOVE    WRK-WARI    TO  WRK-HKNFTN2
                       MOVE    WRK-HKNFTN2 TO  HC03-FUTAN
                   END-IF
           END-IF
           IF    ( SYU-HKNCOMBINUM =   SPACE OR    ZERO )
                   MOVE    SPACE       TO  HC03-FUTAN
           END-IF
           IF    ( TPARA-HKNMEI        =   "0" )
               MOVE    SPACE           TO  HC03-HOKEN
           END-IF
           IF    ( TPARA-FTNKBN        =   "0" )
               MOVE    SPACE           TO  HC03-FUTAN
                                           HC03-LBLFTNRATE
           END-IF
      *
      *    診療日
           PERFORM 3101-SRYYMD-HEN-SEC
           MOVE    WRK-LBLSRYYMD       TO  HC03-LBLSRYYMD
           MOVE    WRK-SRYYMDG         TO  HC03-SRYYMD
      *    発行日
           IF    ( ORCHC03-MOTOID      =    1 OR 3 OR 4 )
            AND  ( TPARA-HAKKOUBIKBN   =   "1" )
               MOVE    SPA-SYSYMD      TO  WRK-SYMD
           ELSE
               MOVE    SYU-DENPPRTYMD  TO  WRK-SYMD
           END-IF
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HC03-HAKYMD
      *
      *----(03.04.01)--UPD-START---
      *    医療機関住所
      *    MOVE    SYS-1002-ADRS       TO  HC03-IRYOADRS
      *    医療機関名
      *    MOVE    SYS-1001-HOSPNAME   TO  HC03-IRYOMEI
      *    医療機関ＴＥＬ
      **** MOVE    SYS-1002-TEL        TO  HC03-IRYOTEL
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  HC03-IRYOADRS
           INSPECT HC03-IRYOADRS       REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  HC03-IRYOMEI
           INSPECT HC03-IRYOMEI        REPLACING ALL "  " BY "　"
      *    医療機関ＴＥＬ
           MOVE    WRK-HOSPTEL         TO  HC03-IRYOTEL
      *----(03.04.01)--UPD-END-----
      *
      *    自費名称編集
           PERFORM 3102-JIHIMSG-HEN-SEC
      *
           MOVE    SPACE               TO  WRK-Z7-G
           MOVE    SPACE               TO  WRK-Z72-G
      *    診療科
           MOVE    SYU-HKNTEN (01)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO1
      *    指導科
           MOVE    SYU-HKNTEN (02)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO2
      *    在宅科
           MOVE    SYU-HKNTEN (03)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO3
      *    投薬科
           MOVE    SYU-HKNTEN (04)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO4
      *    注射科
           MOVE    SYU-HKNTEN (05)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO5
      *    処置科
           MOVE    SYU-HKNTEN (06)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO6
      *    手術科
           MOVE    SYU-HKNTEN (07)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO7
      *    麻酔
           MOVE    SYU-HKNTEN (08)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO8
      *    検査科
           MOVE    SYU-HKNTEN (09)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO9
      *    Ｘ線科
           MOVE    SYU-HKNTEN (10)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G          TO  HC03-HKNRYO10
      *    リハビリ
           MOVE    SYU-HKNTEN (11)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO11
      *    精神科専門
           MOVE    SYU-HKNTEN (12)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO12
      *    放射線治療
           MOVE    SYU-HKNTEN (13)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO13
      *    病理診断
           MOVE    SYU-HKNTEN (14)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO14
      *    入院料
           MOVE    SYU-HKNTEN (15)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO15
      *    療養担当手当て
           MOVE    SYU-HKNTEN (16)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO16
      *    見出し
           PERFORM 3001-MEISAI-HEN-SEC
      *    合計点数
           COMPUTE WRK-GOKTEN          =   SYU-TOTAL-HKNTEN
           MOVE    WRK-GOKTEN          TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-GOKTEN
      *
           IF    ( TPARA-API-DATA      NOT =   "1" )
      *        労災・自賠で患者請求が無い場合、点数表示なし
               IF    ( COMB-HKNNUM             =    "971"
                  AND  PTRSI-HKNKBN        NOT =    "5"
                  AND  SYS-4001-RSIIRYOKBN     =    "1" )
                OR   ( COMB-HKNNUM             =    "971"
                  AND PTRSI-HKNKBN             =    "5"
                  AND SYU-PTFTNRATE            =    ZERO )
                OR   ( COMB-HKNNUM             =    "973"
                   AND SYU-PTFTNRATE           =    ZERO )
      *
                   MOVE    SPACE               TO  HC03-HKNRYO1
                   MOVE    SPACE               TO  HC03-HKNRYO2
                   MOVE    SPACE               TO  HC03-HKNRYO3
                   MOVE    SPACE               TO  HC03-HKNRYO4
                   MOVE    SPACE               TO  HC03-HKNRYO5
                   MOVE    SPACE               TO  HC03-HKNRYO6
                   MOVE    SPACE               TO  HC03-HKNRYO7
                   MOVE    SPACE               TO  HC03-HKNRYO8
                   MOVE    SPACE               TO  HC03-HKNRYO9
                   MOVE    SPACE               TO  HC03-HKNRYO10
                   MOVE    SPACE               TO  HC03-HKNRYO11
                   MOVE    SPACE               TO  HC03-HKNRYO12
                   MOVE    SPACE               TO  HC03-HKNRYO13
                   MOVE    SPACE               TO  HC03-HKNRYO14
                   MOVE    SPACE               TO  HC03-HKNRYO15
                   MOVE    SPACE               TO  HC03-HKNRYO16
                  MOVE    ALL "*"              TO  HC03-GOKTEN
               END-IF
           END-IF
      *
      *    保険適用分
           MOVE    SYU-HKNTEKMONEY     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HOKENBUN
      *
      *    保険適用外編集
           PERFORM     VARYING   IDX-JIHI FROM  1  BY  1
                       UNTIL     IDX-JIHI  >  16
               MOVE   ZERO      TO    WRK-Z72
               COMPUTE   WRK-Z72   =   SYU-TGMONEY(IDX-JIHI)  +
                                       SYU-TGMONEY-TAX(IDX-JIHI)
               PERFORM   800-HENKAN-SEC
               MOVE    WRK-Z72-G   TO  HC03-JIHIBUN(IDX-JIHI)
           END-PERFORM
      *
      *    保険適用外消費税（再掲）
           MOVE    SYU-TGMONEY-TAX-SAI     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G               TO  HC03-HOKENZEI
      *
           IF    ( COMB-HKNNUM             =    "971"
                                           OR   "973" )
            OR   ( COMB-KOHHKNNUM (1)      =    "970"
              AND  SYU-PTFTNRATE       NOT =     ZERO )
      *        労災・自賠の時、点数表示なし
               IF    ( COMB-HKNNUM             =    "971"
                   AND PTRSI-HKNKBN        NOT =    "5"
                   AND SYS-4001-RSIIRYOKBN     =    "1" )
                OR   ( COMB-HKNNUM             =    "971"
                   AND PTRSI-HKNKBN            =    "5"
                   AND SYU-PTFTNRATE           =    ZERO )
                OR   ( COMB-HKNNUM             =    "973"
                   AND SYU-PTFTNRATE           =    ZERO )
      *
                   MOVE    SPACE               TO  HC03-HOKENBUN
      *
               ELSE
      *
                   COMPUTE WRK-GOKTEN      =    SYU-HKNTEKMONEY
                                           +    SYU-RSISHOSHIN-MONEY
                                           +    SYU-RSISAISHIN-MONEY
                                           +    SYU-RSISDO-MONEY
                                           +    SYU-RSIETC-MONEY
                                           +    SYU-KOH-FTN
                   MOVE    WRK-GOKTEN      TO  WRK-Z72
                   PERFORM 800-HENKAN-SEC
                   MOVE    WRK-Z72-G           TO  HC03-HOKENBUN
               END-IF
           END-IF
      *
      *    労災・自賠の診察料等の記載
           IF   (SYU-RSI-TOTAL     NOT =   ZERO) AND
                (SYS-4001-RSIPRTKBN    =   "2")
               EVALUATE    COMB-HKNNUM
                   WHEN    "971"
                       MOVE  "（内労災診察等）"  TO HC03-RSIMIDASI
                   WHEN    "973"
                       MOVE  "（内自賠診察等）"  TO HC03-RSIMIDASI
                   WHEN    OTHER
                       IF    ( COMB-KOHHKNNUM (1)   =   "970" )
                           MOVE  "（内自賠文書等）"
                                                 TO HC03-RSIMIDASI
                       END-IF
               END-EVALUATE
               MOVE    SYU-RSI-TOTAL          TO  WRK-Z72
               PERFORM 800-HENKAN-SEC
               MOVE    WRK-Z72-G              TO  HC03-RSIBUN
               MOVE    "円"                   TO  HC03-RSIEN
           ELSE
               MOVE    SPACE                  TO  HC03-RSIMIDASI
               MOVE    SPACE                  TO  HC03-RSIBUN
               MOVE    SPACE                  TO  HC03-RSIEN
           END-IF
      *
      *    保険適用外分
           MOVE    ZERO                TO  WRK-GOKTEN
           COMPUTE WRK-GOKTEN          =   SYU-TOTAL-TGMONEY
                                       +   SYU-TOTAL-TGMONEY-TAX
           MOVE    WRK-GOKTEN          TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HOKENGAI
      *    自費１から１０
           MOVE    ZERO                TO  WRK-JIHI-REC
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
               COMPUTE WRK-JIHI (IDX)      =   SYU-JIHI-TAXNASI (IDX)
                                           +   SYU-JIHI-TAXARI  (IDX)
               MOVE    WRK-JIHI (IDX)  TO  WRK-Z72
               PERFORM 800-HENKAN-SEC
               MOVE    WRK-Z72-G       TO  HC03-JIHI(IDX)
           END-PERFORM
      *
      *    自費計
           COMPUTE WRK-JIHI-TOTAL      =   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
      *    自費消費税
           MOVE    SYU-JIHI-TAX        TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-JIHI(12)
      *    自費合計
           IF      SYU-SRYYMD          <=  "20070331"
             COMPUTE WRK-GOKTEN        =   WRK-JIHI-TOTAL
                                       +   SYU-JIHI-TAX
           ELSE
             MOVE    WRK-JIHI-TOTAL    TO  WRK-GOKTEN
           END-IF
           MOVE    WRK-GOKTEN          TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-JIHI(11)
      *    薬剤一部負担
           MOVE    SYU-YKZ-FTN         TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-YAKUZAI
      *    老人一部負担金
           MOVE    SYU-RJN-FTN         TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-ROUJIN
      *    公費一部負担金
           MOVE    SYU-KOH-FTN         TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-KOUHI
      *
      *    調整金
           MOVE    SYU-CHOSEI          TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-CHOSEI
      *
           MOVE    SYU-CHOSEI1         TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  TAPI-CHOSEI1
      *
           MOVE    SYU-CHOSEI2         TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  TAPI-CHOSEI2
      *
      *    今回請求額
           MOVE    WRK-SKYMONEY        TO  WRK-Z72
           PERFORM 810-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-KONSEIKYU
      *
      *    今回請求額消費税（再掲）
           MOVE    WRK-SKYMONEY-TAX-SAI    TO  WRK-Z72
           PERFORM 810-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-SEIKYUZEI
      *
      *    今回入金額
           MOVE    WRK-NYUKIN          TO  WRK-Z72
           PERFORM 810-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-KONNYUKIN
      *    前回未収額計算
      *!!! PERFORM 3101-MISYU-HEN-SEC
      *****MOVE    ORCHC03-ZENMISYU    TO  WRK-MISYUMONEY
           MOVE    WRK-MISYUMONEY      TO  WRK-Z72
           PERFORM 810-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-ZENMISYU
      *    合計請求額
           COMPUTE WRK-GOKTEN          =   WRK-SKYMONEY
                                       +   WRK-MISYUMONEY
           MOVE    WRK-GOKTEN          TO  WRK-Z72
           PERFORM 810-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-GOUKEIGAK
      *
      *    発行区分
           EVALUATE    ORCHC03-HAKKOFLG
      *        WHEN    1
      *            MOVE    "（再）"        TO  HC03-MIDASHI
               WHEN    2
                   IF    ( TPARA-TEISEIKBN =   "1" )
                       MOVE    "（訂正）"  TO  HC03-MIDASHI
                   END-IF
           END-EVALUATE
      *
           IF    ( SMCNDATE-YMD        >=  TPARA-YUKOSTYMD )
            AND  ( SMCNDATE-YMD        <=  TPARA-YUKOEDYMD )
               PERFORM VARYING IDX-MEMO    FROM    1   BY  1
                       UNTIL ( IDX-MEMO    >   CONST-MEMO-MAX )
      *
                   MOVE    TPARA-MEMO (IDX-MEMO)
                                           TO  HC03-MEMO (IDX-MEMO)
      *
               END-PERFORM
           END-IF
      *
           IF    ( SMCNDATE-YMD        >=  TPARA-CHUI-YUKOSTYMD )
            AND  ( SMCNDATE-YMD        <=  TPARA-CHUI-YUKOEDYMD )
               MOVE    TPARA-CHUI          TO  HC03-CHUI
           END-IF
      *
           IF    ( ORCHC03-HAKKOFLG        =   4 )
               PERFORM 3001-TUKIBETU-HEN-SEC
           END-IF
      *
           IF    ( TPARA-API-DATA      =   "1" )
               PERFORM 2002-APIRES-HEN-SEC
           ELSE
               IF    ( SYU-SRYYMD          >=  CONST-H200401 )
                   EVALUATE    ORCHC03-SIZE
                   WHEN    "V1"
                       MOVE    "HCM03BN.red"       TO  WRK-FORMID
                       PERFORM 2002-HC03V01-HEN-SEC
                       MOVE    HC03V01             TO  WKPRT-REC
                   WHEN    "V2"
                       MOVE    "HCM03V02BN.red"    TO  WRK-FORMID
                       PERFORM 2002-HC03V02-HEN-SEC
                       MOVE    HC03V02             TO  WKPRT-REC
                   WHEN    "A5"
                       MOVE    "HCM03V03A5BN.red"  TO  WRK-FORMID
                       MOVE    HC03                TO  WKPRT-REC
                   WHEN    OTHER
                       MOVE    "HCM03V03BN.red"    TO  WRK-FORMID
                       MOVE    HC03                TO  WKPRT-REC
                   END-EVALUATE
               ELSE
                   EVALUATE    ORCHC03-SIZE
                   WHEN    "V1"
                       MOVE    "HCM03BO.red"       TO  WRK-FORMID
                       PERFORM 2002-HC03V01-HEN-SEC
                       MOVE    HC03V01             TO  WKPRT-REC
                   WHEN    "V2"
                       MOVE    "HCM03V02BO.red"    TO  WRK-FORMID
                       PERFORM 2002-HC03V02-HEN-SEC
                       MOVE    HC03V02             TO  WKPRT-REC
                   WHEN    "A5"
                       MOVE    "HCM03V03A5BO.red"  TO  WRK-FORMID
                       MOVE    HC03                TO  WKPRT-REC
                   WHEN    OTHER
                       MOVE    "HCM03V03BO.red"    TO  WRK-FORMID
                       MOVE    HC03                TO  WKPRT-REC
                   END-EVALUATE
               END-IF
           END-IF
      *
      *
           .
       300-HC03-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   明細見出し編集処理
      *****************************************************************
       3001-MEISAI-HEN-SEC                SECTION.
      *
           MOVE    "初 ・ 再 診 料"
                                       TO  HC03-MEISAI (1)
           MOVE    "A00"               TO  TAPI-HKNTEN-KBN (1)
      *
           MOVE    "医 学 管 理 等"
                                       TO  HC03-MEISAI (2)
           MOVE    "B00"               TO  TAPI-HKNTEN-KBN (2)
      *
           MOVE    "在　宅　医　療"
                                       TO  HC03-MEISAI (3)
           MOVE    "C00"               TO  TAPI-HKNTEN-KBN (3)
      *
           MOVE    "投　　　　　薬"
                                       TO  HC03-MEISAI (4)
           MOVE    "F00"               TO  TAPI-HKNTEN-KBN (4)
      *
           MOVE    "注　　　　　射"
                                       TO  HC03-MEISAI (5)
           MOVE    "G00"               TO  TAPI-HKNTEN-KBN (5)
      *
           MOVE    "処　　　　　置"
                                       TO  HC03-MEISAI (6)
           MOVE    "J00"               TO  TAPI-HKNTEN-KBN (6)
      *
           MOVE    "手　　　　　術"
                                       TO  HC03-MEISAI (7)
           MOVE    "K00"               TO  TAPI-HKNTEN-KBN (7)
      *
           MOVE    "麻　　　　　酔"
                                       TO  HC03-MEISAI (8)
           MOVE    "L00"               TO  TAPI-HKNTEN-KBN (8)
      *
           MOVE    "検　　　　　査"
                                       TO  HC03-MEISAI (9)
           MOVE    "D00"               TO  TAPI-HKNTEN-KBN (9)
      *
           MOVE    "画　像　診　断"
                                       TO  HC03-MEISAI (10)
           MOVE    "E00"               TO  TAPI-HKNTEN-KBN (10)
      *
           MOVE    "リハビリテーション"
                                       TO  HC03-MEISAI (11)
           MOVE    "H00"               TO  TAPI-HKNTEN-KBN (11)
      *
           MOVE    "精神科専門療法"
                                       TO  HC03-MEISAI (12)
           MOVE    "I00"               TO  TAPI-HKNTEN-KBN (12)
      *
           MOVE    "放 射 線 治 療"
                                       TO  HC03-MEISAI (13)
           MOVE    "M00"               TO  TAPI-HKNTEN-KBN (13)
      *
           MOVE    "病　理　診　断"
                                       TO  HC03-MEISAI (14)
           MOVE    "N00"               TO  TAPI-HKNTEN-KBN (14)
      *
           MOVE    "入　院　料　等"
                                       TO  HC03-MEISAI (15)
           MOVE    "A10"               TO  TAPI-HKNTEN-KBN (15)
      *
           MOVE    "療養担当手当"
                                       TO  HC03-MEISAI (16)
           MOVE    "001"               TO  TAPI-HKNTEN-KBN (16)
      *
      *    明細削除
      *    療養担当手当て
           IF     (SYU-HKNTEN (16)     =   ZERO ) AND
                  (SYU-TGMONEY (16)    =   ZERO ) AND
                  (SYU-TGMONEY-TAX(16) =   ZERO )
               MOVE    SPACE           TO  HC03-MEISAI (16)
                                           TAPI-HKNTEN-KBN (16)
           END-IF
      *
           .
       3001-MEISAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理（API）
      *****************************************************************
       2002-APIRES-HEN-SEC                 SECTION.
      *
           INITIALIZE                      FMSEIKYU-REC
           MOVE    "seikyusho"         TO  FMSEIKYU-FORM-ID
           MOVE    TPARA-API-CUSTOM-ID TO  FMSEIKYU-CUSTOM-ID
           MOVE    SYS-1031-PRTNM(3)   TO  FMSEIKYU-PRINTER
           MOVE    ORCHC03-MOTOID      TO  FMSEIKYU-MOTOID
           MOVE    HC03-KNJNO          TO  FMSEIKYU-PTNUM
           IF    ( TPARA-KANANAME      =   "1" )
               MOVE    PTINF-KANANAME  TO  FMSEIKYU-KANANAME
           END-IF
           MOVE    SPA-NAME            TO  FMSEIKYU-NAME
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   3 )
               MOVE    WRK-API-HOSPNAME (IDXA)
                                       TO  FMSEIKYU-HOSP-NAME (IDXA)
           END-PERFORM
           MOVE    WRK-HOSPPOST        TO  FMSEIKYU-HOSP-POST
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   3 )
               MOVE    WRK-API-HOSPADRS (IDXA)
                                       TO  FMSEIKYU-HOSP-ADRS (IDXA)
           END-PERFORM
           MOVE    WRK-HOSPTEL         TO  FMSEIKYU-HOSP-TEL
           IF    ( ORCHC03-MOTOID      =   2 OR 5 )
               MOVE    WRK-JNISSU      TO  FMSEIKYU-JNISSU
           END-IF
           IF    ( TPARA-SRYKA         =   "1" )
               MOVE    WRK-SRYKA-MEI   TO  FMSEIKYU-SRYKANAME
           END-IF
      *
           
           MOVE    WRK-SRYKIKANKBN     TO  FMSEIKYU-SRYKIKANKBN
      *
           IF    ( WRK-SRYKIKANKBN     =   "1" )
               MOVE    WRK-STSRYYMDG   TO  FMSEIKYU-STSRYYMD
               MOVE    WRK-EDSRYYMDG   TO  FMSEIKYU-EDSRYYMD
           ELSE
               MOVE    WRK-STSRYYMDG   TO  FMSEIKYU-SRYYM
           END-IF
      *
           MOVE    HC03-HAKYMD         TO  FMSEIKYU-DENPPRTYMD
           MOVE    HC03-HOKEN          TO  FMSEIKYU-HOKENNAME
           MOVE    HC03-FUTAN          TO  FMSEIKYU-FUTAN
           MOVE    HC03-LBLFTNRATE     TO  FMSEIKYU-FUTANMEI
           MOVE    HC03-NO             TO  FMSEIKYU-DENPNUM
           IF    ( HC03-MIDASHI    NOT =   SPACE )
               MOVE    "1"             TO  FMSEIKYU-TEISEI-KBN
           END-IF
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA        >   16 )
      *
               MOVE    TAPI-HKNTEN-KBN (IDXA)
                                       TO  FMSEIKYU-HKNTEN-KBN (IDXA)
               MOVE    HC03-MEISAI (IDXA)
                                       TO  FMSEIKYU-HKNTEN-NAME (IDXA)
               MOVE    HC03-JIHIBUN (IDXA)
                                       TO  FMSEIKYU-TGMONEY (IDXA)
      *
           END-PERFORM
      *
           MOVE    HC03-HKNRYO1        TO  FMSEIKYU-HKNTEN (1)
           MOVE    HC03-HKNRYO2        TO  FMSEIKYU-HKNTEN (2)
           MOVE    HC03-HKNRYO3        TO  FMSEIKYU-HKNTEN (3)
           MOVE    HC03-HKNRYO4        TO  FMSEIKYU-HKNTEN (4)
           MOVE    HC03-HKNRYO5        TO  FMSEIKYU-HKNTEN (5)
           MOVE    HC03-HKNRYO6        TO  FMSEIKYU-HKNTEN (6)
           MOVE    HC03-HKNRYO7        TO  FMSEIKYU-HKNTEN (7)
           MOVE    HC03-HKNRYO8        TO  FMSEIKYU-HKNTEN (8)
           MOVE    HC03-HKNRYO9        TO  FMSEIKYU-HKNTEN (9)
           MOVE    HC03-HKNRYO10       TO  FMSEIKYU-HKNTEN (10)
           MOVE    HC03-HKNRYO11       TO  FMSEIKYU-HKNTEN (11)
           MOVE    HC03-HKNRYO12       TO  FMSEIKYU-HKNTEN (12)
           MOVE    HC03-HKNRYO13       TO  FMSEIKYU-HKNTEN (13)
           MOVE    HC03-HKNRYO14       TO  FMSEIKYU-HKNTEN (14)
           MOVE    HC03-HKNRYO15       TO  FMSEIKYU-HKNTEN (15)
           MOVE    HC03-HKNRYO16       TO  FMSEIKYU-HKNTEN (16)
           MOVE    HC03-GOKTEN         TO  FMSEIKYU-TOTAL-TEN
           MOVE    HC03-HOKENBUN       TO  FMSEIKYU-HKNTEKMONEY
           MOVE    HC03-RSIMIDASI      TO  FMSEIKYU-LBL-RSI-TOTAL
           MOVE    HC03-RSIBUN         TO  FMSEIKYU-RSI-TOTAL
           MOVE    HC03-KOUHI          TO  FMSEIKYU-KOH-FTN
           MOVE    HC03-HOKENGAI       TO  FMSEIKYU-TOTAL-TGMONEY
           MOVE    HC03-HOKENZEI       TO  FMSEIKYU-TOTAL-TGMONEY-TAX
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   10 )
      *
               MOVE   WRK-JIHIMEI (IDXA)
                                       TO  FMSEIKYU-JIHI-NAME (IDXA)
               MOVE   HC03-JIHI   (IDXA)
                                       TO  FMSEIKYU-JIHI (IDXA)
      *
           END-PERFORM
           MOVE   HC03-JIHI(11)        TO  FMSEIKYU-JIHI-TOTAL
           MOVE   HC03-JIHI(12)        TO  FMSEIKYU-JIHI-TOTAL-TAX
      *
           MOVE   TAPI-CHOSEI1         TO  FMSEIKYU-CHOSEI1
           MOVE   TAPI-CHOSEI2         TO  FMSEIKYU-CHOSEI2
           MOVE   HC03-KONSEIKYU       TO  FMSEIKYU-SKYMONEY
           MOVE   HC03-SEIKYUZEI       TO  FMSEIKYU-SKYMONEY-TAX
           MOVE   HC03-ZENMISYU        TO  FMSEIKYU-SKYMONEY-LAST
           MOVE   HC03-GOUKEIGAK       TO  FMSEIKYU-SKYMONEY-TOTAL
           MOVE   HC03-KONNYUKIN       TO  FMSEIKYU-NYUKIN
           PERFORM VARYING IDXA FROM    1   BY  1
                   UNTIL ( IDXA >   10 )
               MOVE  HC03-MEMO (IDXA)
                                       TO  FMSEIKYU-MEMO (IDXA)
           END-PERFORM
      *
           IF    ( ORCHC03-SIZE    NOT =   "V5" )
               MOVE    HC03-CHUI       TO  FMSEIKYU-SKY-CAUTION
           ELSE
      *
               MOVE    ZERO            TO  WRK-API-GRPNUM
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   SYUPARA03-GRP-DENPNUM-MAX )
      *
                   INITIALIZE                  ORCHC04SUBAREA
                   MOVE    SYUPARA03-GRP-DENPNUM (IDXD)
                                       TO  ORCHC04SUB-DENPNUM
                   MOVE    TPARA-GOKEITANI
                                       TO  ORCHC04SUB-GOKEITANI
                   MOVE    TPARA-SYOKEI
                                       TO  ORCHC04SUB-PARAKBN
                   IF    ( TBL-COM-STR (1) NOT =   SPACE )
                       MOVE    "1"     TO  ORCHC04SUB-PARACOMKBN
                   END-IF
                   IF     ( TPARA-JIHIKBN  =   "1" )
                       MOVE    "1"     TO  ORCHC04SUB-PARAJIHIKBN
                   END-IF
                   MOVE    "1"         TO  ORCHC04SUB-APIKBN
      *
                   CALL    "ORCHC04SUB"    USING
                                           SPA-AREA
                                           ORCHC04SUBAREA
      *
                   IF    ( ORCHC04SUB-TBL-MAX  >   ZERO )
      *
                       PERFORM 30050-COMMNT-CHK-SEC
      *
                       IF    ( IDX-API     <   2000 )
                           IF    ( SYUPARA03-GRP-DENPNUM-MAX   >   1 )
                            OR   ( WRK-SRYKIKANKBN             =   "2" )
                               PERFORM 20021-EDIT-HEAD-SEC
                           END-IF
                       END-IF
                       IF    ( IDX-API     <   2000 )
                           PERFORM 20021-EDIT-MEISAI-SEC
                       END-IF
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       2002-APIRES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    コメント判定追加処理
      *****************************************************************
       30050-COMMNT-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-CM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   ORCHC04SUB-TBL-MAX
      *
               MOVE    1                   TO  FLG-OK
      *
               IF     (ORCHC04SUB-SRYCD (IDX)(1:1) =   "8" )
                  OR  (ORCHC04SUB-SRYCD (IDX)(1:3) =   "008")
                   MOVE    ZERO                TO  FLG-OK
                   PERFORM VARYING     IDXC    FROM    1   BY  1
                           UNTIL      (IDXC    >   5 )
                                OR    (TBL-COM-STR (IDXC)  =   SPACE)
                                OR    (FLG-OK      =   1   )
                       IF    (ORCHC04SUB-SRYCD (IDX)
                                           >=  TBL-COM-STR (IDXC))
                        AND  (ORCHC04SUB-SRYCD (IDX)
                                           <=  TBL-COM-END (IDXC))
                           MOVE    1           TO  FLG-OK
                       END-IF
                   END-PERFORM
                   IF      FLG-OK              =   ZERO
      *                対象外
                       IF     (ORCHC04SUB-H-ZAIHEN(IDX)
                                                   NOT =   "＊")
                         AND  (ORCHC04SUB-H-TENSU  (IDX-CM)
                                                       =   SPACE )
                         AND  (ORCHC04SUB-H-KINGAKU(IDX-CM)
                                                       =   SPACE )
      *                    剤の最後
                           MOVE    ORCHC04SUB-H-KAISU (IDX)
                                               TO  ORCHC04SUB-H-KAISU
                                                             (IDX-CM)
                           MOVE    ORCHC04SUB-H-TENSU (IDX)
                                               TO  ORCHC04SUB-H-TENSU
                                                             (IDX-CM)
                           MOVE    ORCHC04SUB-H-KINGAKU (IDX)
                                               TO  ORCHC04SUB-H-KINGAKU
                                                             (IDX-CM)
                           MOVE    ORCHC04SUB-H-ENKBN (IDX)
                                               TO  ORCHC04SUB-H-ENKBN
                                                             (IDX-CM)
                           MOVE    ORCHC04SUB-ZAIEND (IDX)
                                               TO  ORCHC04SUB-ZAIEND
                                                             (IDX-CM)
                       END-IF
                       IF     (ORCHC04SUB-H-ZAIHEN(IDX)
                                                       =   "＊")
                       AND    (ORCHC04SUB-ZAIEND(IDX)  =   SPACE)
      *                    剤の先頭
                           MOVE    ORCHC04SUB-H-SRYKBN (IDX)
                                               TO  ORCHC04SUB-H-SRYKBN
                                                             (IDX + 1)
                           MOVE    ORCHC04SUB-H-ZAIHEN (IDX)
                                               TO  ORCHC04SUB-H-ZAIHEN
                                                             (IDX + 1)
                       END-IF
                       MOVE    "999999999"     TO ORCHC04SUB-SRYCD (IDX)
                   END-IF
               END-IF
      *
               IF      FLG-OK              =   1
                   MOVE    IDX             TO  IDX-CM
               END-IF
           END-PERFORM
           .
       30050-COMMNT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    明細書行見出し編集処理
      *****************************************************************
       20021-EDIT-HEAD-SEC             SECTION.
      *
           COMPUTE IDX-API =   IDX-API +   1
      *
           MOVE    "1"                 TO  FMSEIKYU-GRPHEAD(IDX-API)
      *
           COMPUTE WRK-API-GRPNUM-HEAD
               =   WRK-API-GRPNUM  +   1
           MOVE    WRK-API-GRPNUM-HEAD TO  FMSEIKYU-GRPNUM (IDX-API)
      *
           MOVE    1                   TO  IDXP
      *
           IF    ( WRK-SRYKIKANKBN     =   "2" )
            OR   ( WRK-STSRYYMDG   NOT =   WRK-EDSRYYMDG )
               STRING  "〔"            DELIMITED BY SIZE
                       ORCHC04SUB-SRYYMDMEI(17:6)
                                       DELIMITED BY SIZE
                       "〕"            DELIMITED BY SIZE
               INTO    FMSEIKYU-SRYNAME-G(IDX-API)
               WITH    POINTER    IDXP
               END-STRING
           END-IF
      *
           IF    ( TPARA-SRYKA             =   "1" )
            AND  ( ORCHC04SUB-SRYKAMEI NOT =   SPACE )
               STRING  "≪"            DELIMITED  BY  SIZE
                       ORCHC04SUB-SRYKAMEI
                                       DELIMITED  BY  SPACE
                       "≫"            DELIMITED  BY  SIZE
               INTO    FMSEIKYU-SRYNAME-G(IDX-API)
               WITH    POINTER    IDXP
               END-STRING
           END-IF
      *
           IF    ( TPARA-HKNMEI                =   "1" )
            AND  ( ORCHC04SUB-HKNCOMBIMEI  NOT =   SPACE )
               STRING  "【"            DELIMITED  BY  SIZE
                       ORCHC04SUB-HKNCOMBIMEI
                                       DELIMITED  BY  SPACE
                       "】"            DELIMITED  BY  SPACE
               INTO    FMSEIKYU-SRYNAME-G(IDX-API)
               WITH    POINTER    IDXP
               END-STRING
           END-IF
      *
           .
       20021-EDIT-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    明細書剤部分編集処理
      *****************************************************************
       20021-EDIT-MEISAI-SEC           SECTION.
      *
           ADD     1                   TO  WRK-API-GRPNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   ORCHC04SUB-TBL-MAX )
                        OR    (IDX-API     >=   2000 )
             IF      ORCHC04SUB-SRYCD (IDX)    NOT =   "999999999"
               ADD     1                   TO  IDX-API
               MOVE    WRK-API-GRPNUM      TO  FMSEIKYU-GRPNUM(IDX-API)
      *        名称＋数量編集
               IF     (ORCHC04SUB-SURYO  (IDX) =   ZERO  )
                 OR   (ORCHC04SUB-SRYKBN (IDX) =   "95"
                                               OR  "96"  )
      *            自費の数量記載なし
                   MOVE    ORCHC04SUB-NAME (IDX)   TO  WRK-H-NAME
               ELSE
                   MOVE    SPACE                   TO  WRK-H-NAME
                   STRING  ORCHC04SUB-NAME (IDX)
                           "　　"
                           ORCHC04SUB-H-ZEN-SURYO (IDX)
                           DELIMITED  BY  SPACE
                                           INTO   WRK-H-NAME
                   END-STRING
               END-IF
      *        小計
               IF      ORCHC04SUB-SRYKBN (IDX)     =   "XX"
                   MOVE    "小計"          TO  FMSEIKYU-SRYNAME
                                                     (IDX-API)
               ELSE
      *            名称
                   MOVE    WRK-H-NAME      TO  FMSEIKYU-SRYNAME
                                                     (IDX-API)
               END-IF
               MOVE    ORCHC04SUB-H-SRYKBN(IDX)
                                           TO  FMSEIKYU-SRYKBN
                                                     (IDX-API)
      *        剤開始（＊）
               MOVE    ORCHC04SUB-H-ZAIHEN (IDX)
                                           TO  FMSEIKYU-ZAIHEN
                                                     (IDX-API)
      *        点数（単価）
               MOVE    ORCHC04SUB-H-TENSU (IDX)
                                           TO  FMSEIKYU-TENSU
                                                     (IDX-API)
      *        合計
               IF    ( ORCHC04SUB-H-KINGAKU(IDX)
                                       NOT =   SPACE )
                   MOVE    FUNCTION NUMVAL(ORCHC04SUB-H-KINGAKU(IDX))
                                           TO  WRK-KINGAKU-Z1
                   MOVE    WRK-KINGAKU-X   TO  FMSEIKYU-TOTAL
                                                     (IDX-API)
               END-IF
      *        円
               IF    ( TPARA-GOKEITANI     =   "1" )
                   MOVE    ORCHC04SUB-H-ENKBN (IDX)
                                           TO  FMSEIKYU-TENSU-MARK
                                                     (IDX-API)
               END-IF
      *        回数
               MOVE    ORCHC04SUB-H-KAISU (IDX)
                                           TO  FMSEIKYU-KAISU
                                                      (IDX-API)
      *        剤終了
               MOVE    ORCHC04SUB-ZAIEND (IDX)
                                          TO  FMSEIKYU-ZAIEND(IDX-API)
             END-IF
      *
           END-PERFORM
      *
           IF    ( TPARA-GOKEITANI     =   "1" )
      *        点数表示
               MOVE    "0"                TO  FMSEIKYU-YEN-FLG
           ELSE
      *        単価表示
               MOVE    "1"                TO  FMSEIKYU-YEN-FLG
           END-IF
      *
           IF    ( SMCNDATE-YMD        >=  TPARA-CHUI-YUKOSTYMD )
            AND  ( SMCNDATE-YMD        <=  TPARA-CHUI-YUKOEDYMD )
               MOVE    TPARA-RCHUI        TO  FMSEIKYU-SKY-CAUTION
               MOVE    TPARA-MCHUI        TO  FMSEIKYU-SRY-CAUTION
           END-IF
      *
           .
       20021-EDIT-MEISAI-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集処理（HC03V01）
      *****************************************************************
       2002-HC03V01-HEN-SEC            SECTION.
      *
           INITIALIZE                  HC03V01
      *
           MOVE  HC03-NO           TO  HC03V01-NO
           MOVE  HC03-LBLSRYYMD    TO  HC03V01-LBLSRYYMD
           MOVE  HC03-SRYYMD       TO  HC03V01-SRYYMD
           MOVE  HC03-KANANAME     TO  HC03V01-KANANAME
           MOVE  HC03-KNJNAMW      TO  HC03V01-KNJNAMW
           MOVE  HC03-KANANAME2    TO  HC03V01-KANANAME2
           MOVE  HC03-KNJNAMW2     TO  HC03V01-KNJNAMW2
           MOVE  HC03-KNJNO        TO  HC03V01-KNJNO
           MOVE  HC03-MIDASI       TO  HC03V01-MIDASI
           MOVE  HC03-HAKYMD       TO  HC03V01-HAKYMD
           MOVE  HC03-HOKEN        TO  HC03V01-HOKEN
           MOVE  HC03-FUTAN        TO  HC03V01-FUTAN
           MOVE  HC03-TUKISRYKA    TO  HC03V01-TUKISRYKA
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL ( IDX >   16 )
               MOVE  HC03-MEISAI (IDX)
                                   TO  HC03V01-MEISAI (IDX)
           END-PERFORM
           MOVE  HC03-HKNRYO1      TO  HC03V01-HKNRYO1
           MOVE  HC03-HKNRYO2      TO  HC03V01-HKNRYO2
           MOVE  HC03-HKNRYO3      TO  HC03V01-HKNRYO3
           MOVE  HC03-HKNRYO4      TO  HC03V01-HKNRYO4
           MOVE  HC03-HKNRYO5      TO  HC03V01-HKNRYO5
           MOVE  HC03-HKNRYO6      TO  HC03V01-HKNRYO6
           MOVE  HC03-HKNRYO7      TO  HC03V01-HKNRYO7
           MOVE  HC03-HKNRYO8      TO  HC03V01-HKNRYO8
           MOVE  HC03-HKNRYO9      TO  HC03V01-HKNRYO9
           MOVE  HC03-HKNRYO10     TO  HC03V01-HKNRYO10
           MOVE  HC03-HKNRYO11     TO  HC03V01-HKNRYO11
           MOVE  HC03-HKNRYO12     TO  HC03V01-HKNRYO12
           MOVE  HC03-HKNRYO13     TO  HC03V01-HKNRYO13
           MOVE  HC03-HKNRYO14     TO  HC03V01-HKNRYO14
           MOVE  HC03-HKNRYO15     TO  HC03V01-HKNRYO15
           MOVE  HC03-HKNRYO16     TO  HC03V01-HKNRYO16
           MOVE  HC03-GOKTEN       TO  HC03V01-GOKTEN
           MOVE  HC03-HOKENBUN     TO  HC03V01-HOKENBUN
           MOVE  HC03-HOKENGAI     TO  HC03V01-HOKENGAI
           MOVE  HC03-JIHIMSG1     TO  HC03V01-JIHIMSG1
           MOVE  HC03-JIHIMSG2     TO  HC03V01-JIHIMSG2
           MOVE  HC03-JIHIMSG3     TO  HC03V01-JIHIMSG3
           MOVE  HC03-JIHIMSG4     TO  HC03V01-JIHIMSG4
           MOVE  HC03-JIHIMSG5     TO  HC03V01-JIHIMSG5
           MOVE  HC03-JIHI (1)     TO  HC03V01-JIHI1
           MOVE  HC03-JIHI (2)     TO  HC03V01-JIHI2
           MOVE  HC03-JIHI (3)     TO  HC03V01-JIHI3
           MOVE  HC03-JIHI (4)     TO  HC03V01-JIHI4
           MOVE  HC03-JIHI (5)     TO  HC03V01-JIHI5
           MOVE  WRK-JIHI-TOTAL    TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G       TO  HC03V01-JIHI6
           MOVE  HC03-JIHI (12)    TO  HC03V01-JIHI7
           MOVE  HC03-JIHI (11)    TO  HC03V01-JIHI8
           MOVE  HC03-YAKUZAI      TO  HC03V01-YAKUZAI
           MOVE  HC03-ROUJIN       TO  HC03V01-ROUJIN
           MOVE  HC03-KOUHI        TO  HC03V01-KOUHI
           MOVE  HC03-CHOSEI       TO  HC03V01-CHOSEI
           MOVE  HC03-KONSEIKYU    TO  HC03V01-KONSEIKYU
           MOVE  HC03-ZENMISYU     TO  HC03V01-ZENMISYU
           MOVE  HC03-GOUKEIGAK    TO  HC03V01-GOUKEIGAK
           MOVE  HC03-KONNYUKIN    TO  HC03V01-KONNYUKIN
           MOVE  HC03-IRYOADRS     TO  HC03V01-IRYOADRS
           MOVE  HC03-IRYOMEI      TO  HC03V01-IRYOMEI
           MOVE  HC03-IRYOTEL      TO  HC03V01-IRYOTEL
           MOVE  HC03-MIDASHI      TO  HC03V01-MIDASHI
           MOVE  HC03-RSIMIDASI    TO  HC03V01-RSIMIDASI
           MOVE  HC03-RSIBUN       TO  HC03V01-RSIBUN
           MOVE  HC03-RSIEN        TO  HC03V01-RSIEN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL ( IDX >   10 )
               MOVE  HC03-MEMO (IDX)
                                   TO  HC03V01-MEMO (IDX)
           END-PERFORM
           MOVE  HC03-LBLFTNRATE   TO  HC03V01-LBLFTNRATE
           MOVE  HC03-CHUI         TO  HC03V01-CHUI
      *
           .
       2002-HC03V01-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集処理（HC03V02）
      *****************************************************************
       2002-HC03V02-HEN-SEC            SECTION.
      *
           INITIALIZE                  HC03V02
      *
           MOVE  HC03-NO           TO  HC03V02-NO
           MOVE  HC03-LBLSRYYMD    TO  HC03V02-LBLSRYYMD
           MOVE  HC03-SRYYMD       TO  HC03V02-SRYYMD
           MOVE  HC03-KANANAME     TO  HC03V02-KANANAME
           MOVE  HC03-KNJNAMW      TO  HC03V02-KNJNAMW
           MOVE  HC03-KANANAME2    TO  HC03V02-KANANAME2
           MOVE  HC03-KNJNAMW2     TO  HC03V02-KNJNAMW2
           MOVE  HC03-KNJNO        TO  HC03V02-KNJNO
           MOVE  HC03-MIDASI       TO  HC03V02-MIDASI
           MOVE  HC03-HAKYMD       TO  HC03V02-HAKYMD
           MOVE  HC03-HOKEN        TO  HC03V02-HOKEN
           MOVE  HC03-FUTAN        TO  HC03V02-FUTAN
           MOVE  HC03-TUKISRYKA    TO  HC03V02-TUKISRYKA
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL ( IDX >   16 )
               MOVE  HC03-MEISAI (IDX)
                                   TO  HC03V02-MEISAI (IDX)
           END-PERFORM
           MOVE  HC03-HKNRYO1      TO  HC03V02-HKNRYO1
           MOVE  HC03-HKNRYO2      TO  HC03V02-HKNRYO2
           MOVE  HC03-HKNRYO3      TO  HC03V02-HKNRYO3
           MOVE  HC03-HKNRYO4      TO  HC03V02-HKNRYO4
           MOVE  HC03-HKNRYO5      TO  HC03V02-HKNRYO5
           MOVE  HC03-HKNRYO6      TO  HC03V02-HKNRYO6
           MOVE  HC03-HKNRYO7      TO  HC03V02-HKNRYO7
           MOVE  HC03-HKNRYO8      TO  HC03V02-HKNRYO8
           MOVE  HC03-HKNRYO9      TO  HC03V02-HKNRYO9
           MOVE  HC03-HKNRYO10     TO  HC03V02-HKNRYO10
           MOVE  HC03-HKNRYO11     TO  HC03V02-HKNRYO11
           MOVE  HC03-HKNRYO12     TO  HC03V02-HKNRYO12
           MOVE  HC03-HKNRYO13     TO  HC03V02-HKNRYO13
           MOVE  HC03-HKNRYO14     TO  HC03V02-HKNRYO14
           MOVE  HC03-HKNRYO15     TO  HC03V02-HKNRYO15
           MOVE  HC03-HKNRYO16     TO  HC03V02-HKNRYO16
           MOVE  HC03-GOKTEN       TO  HC03V02-GOKTEN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL ( IDX >   16 )
               MOVE  HC03-JIHIBUN (IDX)
                                   TO  HC03V02-JIHIBUN (IDX)
           END-PERFORM

           MOVE  HC03-HOKENBUN     TO  HC03V02-HOKENBUN
           MOVE  HC03-HOKENGAI     TO  HC03V02-HOKENGAI
           MOVE  HC03-HOKENZEI     TO  HC03V02-HOKENZEI
           MOVE  HC03-JIHIMSG1     TO  HC03V02-JIHIMSG1
           MOVE  HC03-JIHIMSG2     TO  HC03V02-JIHIMSG2
           MOVE  HC03-JIHIMSG3     TO  HC03V02-JIHIMSG3
           MOVE  HC03-JIHIMSG4     TO  HC03V02-JIHIMSG4
           MOVE  HC03-JIHIMSG5     TO  HC03V02-JIHIMSG5
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL ( IDX >   5 )
               MOVE  HC03-JIHI (IDX)
                                   TO  HC03V02-JIHI (IDX)
           END-PERFORM
           MOVE  HC03-JIHI (11)    TO  HC03V02-JIHI (6)
           MOVE  HC03-JIHI (12)    TO  HC03V02-JIHI (7)
           MOVE  HC03-YAKUZAI      TO  HC03V02-YAKUZAI
           MOVE  HC03-ROUJIN       TO  HC03V02-ROUJIN
           MOVE  HC03-KOUHI        TO  HC03V02-KOUHI
           MOVE  HC03-CHOSEI       TO  HC03V02-CHOSEI
           MOVE  HC03-KONSEIKYU    TO  HC03V02-KONSEIKYU
           MOVE  HC03-SEIKYUZEI    TO  HC03V02-SEIKYUZEI
           MOVE  HC03-ZENMISYU     TO  HC03V02-ZENMISYU
           MOVE  HC03-GOUKEIGAK    TO  HC03V02-GOUKEIGAK
           MOVE  HC03-KONNYUKIN    TO  HC03V02-KONNYUKIN
           MOVE  HC03-IRYOADRS     TO  HC03V02-IRYOADRS
           MOVE  HC03-IRYOMEI      TO  HC03V02-IRYOMEI
           MOVE  HC03-IRYOTEL      TO  HC03V02-IRYOTEL
           MOVE  HC03-MIDASHI      TO  HC03V02-MIDASHI
           MOVE  HC03-RSIMIDASI    TO  HC03V02-RSIMIDASI
           MOVE  HC03-RSIBUN       TO  HC03V02-RSIBUN
           MOVE  HC03-RSIEN        TO  HC03V02-RSIEN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL ( IDX >   10 )
               MOVE  HC03-MEMO (IDX)
                                   TO  HC03V02-MEMO (IDX)
           END-PERFORM
           MOVE  HC03-LBLFTNRATE   TO  HC03V02-LBLFTNRATE
           MOVE  HC03-CHUI         TO  HC03V02-CHUI
      *
           .
       2002-HC03V02-HEN-EXT.
           EXIT.
      *****************************************************************
      *    月別請求書兼領収書編集処理
      *****************************************************************
       3001-TUKIBETU-HEN-SEC           SECTION.
      *
            MOVE    1                  TO  IDXP
      *
            MOVE    SYUPARA03-JNISSU   TO  WRK-JNISSU
            STRING  "実日数"           DELIMITED   BY  SIZE
                    WRK-JNISSU         DELIMITED   BY  SIZE
                    "日"               DELIMITED   BY  SIZE
            INTO    HC03-TUKISRYKA
            WITH    POINTER    IDXP
            END-STRING
      *
            IF    ( TPARA-SRYKA        =   "1" )
                STRING  "（診療科："       DELIMITED   BY  SIZE
                        SYUPARA03-SRYKAMEI DELIMITED   BY  SPACE
                        "）"               DELIMITED   BY  SIZE
                INTO    HC03-TUKISRYKA
                WITH    POINTER    IDXP
                END-STRING
            END-IF
      *
           .
       3001-TUKIBETU-HEN-EXT.
           EXIT.
      *****************************************************************
      *   複数科・保険入力の印刷処理
      *****************************************************************
       202-KAKHOU-SYUNOU-SEC                SECTION.
      *
           INITIALIZE                  ORCHC03SUBAREA
      *    収納マスタまとめ計算
           CALL    "ORCHC03SUB"        USING
                                       SPA-AREA
                                       ORCHC03AREA
                                       ORCHC03SUBAREA
      *    入金合計
           MOVE    ZERO                TO  WRK-NYUKIN-TOTAL
           PERFORM VARYING IDX-SYU     FROM    1   BY  1
                   UNTIL   IDX-SYU     >   ORCHC03SUB-MAX
               ADD     ORCHC03SUB-NYUKIN-TOTAL(IDX-SYU)
                                           TO  WRK-NYUKIN-TOTAL
           END-PERFORM
      *    入金合計から前回未収額の入金額を求める
           IF      ORCHC03-NYUKIN      >   WRK-NYUKIN-TOTAL
               COMPUTE WRK-ZENMISYU    =   ORCHC03-NYUKIN
                                       -   WRK-NYUKIN-TOTAL
           ELSE
               MOVE    ZERO            TO  WRK-ZENMISYU
           END-IF
      *
           PERFORM VARYING IDX-SYU     FROM    1   BY  1
                   UNTIL   IDX-SYU     >   ORCHC03SUB-MAX
               MOVE    ORCHC03SUB-SYUNOU-REC(IDX-SYU)
                                           TO  SYUNOU-REC
      *        診療科名編集
               MOVE    SPACE               TO  WRK-SRYKA-MEI
               IF      ORCHC03SUB-DOUJI-SRYKA-G (IDX-SYU)
                                           =   SPACE
                   MOVE    SYU-SRYKA           TO  WRK-SRYKA
                   PERFORM 1000-SRYKAMEI-HEN-SEC
               ELSE
                   PERFORM 2021-GRP-SRYKAMEI-HEN-SEC
               END-IF
               INITIALIZE                      HKNCOMBI-REC
                                               PTRSIINF-REC
      *        保険組合せ
               IF      SYU-HKNCOMBINUM     =   ZERO  OR  SPACE
                   MOVE    "複数保険使用"      TO  WRK-HKNCOMBIMEI
                   MOVE    ZERO                TO  SYU-PTFTNRATE
               ELSE
                   PERFORM 1001-HKNCOMBI-HEN-SEC
               END-IF
      *
               MOVE    SYU-DENPNUM         TO  WRK-DENPNUM
               MOVE    ORCHC03SUB-SKYMONEY (IDX-SYU)
                                           TO  WRK-SKYMONEY
               MOVE    ORCHC03SUB-NYUKIN-TOTAL (IDX-SYU)
                                           TO  WRK-NYUKIN
               MOVE    ORCHC03SUB-SKYMONEY-TAX-SAI (IDX-SYU)
                                           TO  WRK-SKYMONEY-TAX-SAI
      *
      *        １枚目に前回未収額を表示する
               IF      IDX-SYU             =   1
      *---(03.03.00) LINE ADD  START -----------------------------------
                  AND  (ORCHC03-HAKKOFLG   =   ZERO )
      ***********  MOVE    ORCHC03-ZENMISYU    TO  WRK-MISYUMONEY
                   MOVE    ORCHC03-ZENMISYU-2  TO  WRK-MISYUMONEY
      *---(03.03.00) LINE ADD  END   -----------------------------------
                   COMPUTE WRK-NYUKIN          =   WRK-NYUKIN
                                               +   WRK-ZENMISYU
               ELSE
                   MOVE    ZERO                TO  WRK-MISYUMONEY
               END-IF
      *---(03.03.00) LINE ADD  START -----------------------------------
               IF      IDX-SYU             =   1
                   IF     (ORCHC03-ZEN-HENKIN  >   ZERO )  OR
                          (ORCHC03SUB-HENKIN (IDX-SYU) >  ZERO)
                       COMPUTE WRK-MISYUMONEY  =   WRK-MISYUMONEY
                                               -   ORCHC03-ZEN-HENKIN
                                               -   ORCHC03SUB-HENKIN
                                                         (IDX-SYU)
                   ELSE
                       COMPUTE WRK-MISYUMONEY  =   WRK-MISYUMONEY
                                               -   ORCHC03-KANYUKIN
                       IF      WRK-MISYUMONEY      <   ZERO
                           MOVE    ZERO            TO  WRK-MISYUMONEY
                       END-IF
                   END-IF
      *
                   PERFORM 2011-MISYU-OPTION-CHK-SEC
      *
      *            返金対応
                   IF      (ORCHC03-HAKKOFLG   =   ZERO )  AND
                           (WRK-NYUKIN         =   ZERO )  AND
                           (ORCHC03-ZEN-HENKIN >   ZERO )  AND
                           (WRK-MISYUMONEY     <   ZERO )  AND
                           ((WRK-MISYUMONEY * -1)
                                                >   WRK-SKYMONEY)
      *            返金額が今回請求額より多い時、マイナス入金とする
                       COMPUTE WRK-NYUKIN      =   WRK-SKYMONEY
                                               +   WRK-MISYUMONEY
                   END-IF
               ELSE
                   COMPUTE WRK-MISYUMONEY  =   WRK-MISYUMONEY
                                           -   ORCHC03SUB-HENKIN
                                                         (IDX-SYU)
               END-IF
      *---(03.03.00) LINE ADD  END   -----------------------------------
      *
               INITIALIZE                  SYUPARA03-AREA
               MOVE    1               TO  SYUPARA03-GRP-DENPNUM-MAX
               MOVE    SYU-DENPNUM     TO  SYUPARA03-GRP-DENPNUM (1)
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >    15   )
                        OR   ( ORCHC03SUB-DOUJI-DENPNUM (IDX-SYU IDXD)
                                           =   ZERO )
      *
                   COMPUTE SYUPARA03-GRP-DENPNUM-MAX
                       =   IDXD    +   1
      *
                   MOVE    ORCHC03SUB-DOUJI-DENPNUM (IDX-SYU IDXD)
                                   TO  SYUPARA03-GRP-DENPNUM (IDXD + 1)
      *
               END-PERFORM
      *
      *        帳票印刷開始処理
               PERFORM 400-SYOKI-PRINT-SEC
      *
      *        帳票編集処理
               PERFORM 300-HC03-HEN-SEC
      *
      *        帳票印刷処理
               PERFORM 400-PRINT-SEC
           END-PERFORM
      *
           .
       202-KAKHOU-SYUNOU-EXT.
           EXIT.
      *
      *****************************************************************
      *   パラメタテーブルからの印刷処理
      *****************************************************************
       203-PARA-SYUNOU-SEC                 SECTION.
      *
           PERFORM 900-PARA-KEY4-SEL-SEC
           MOVE    PARA-DATA-REC       TO  SYUPARA01-AREA
      *
           PERFORM 900-PARA-KEY5-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PARA    NOT =   ZERO )
      *
               MOVE    PARA-DATA-REC       TO  SYUPARA03-AREA
               MOVE    SYUPARA03-SYU-DATA  TO  SYUNOU-REC
      *
               MOVE    SYUPARA03-DENPNUM   TO  WRK-DENPNUM
               MOVE    SYUPARA03-SEIKYU    TO  WRK-SKYMONEY
               MOVE    SYUPARA03-SEIKYU-TAX-SAI
                                           TO  WRK-SKYMONEY-TAX-SAI
               MOVE    SYUPARA03-NYUKIN    TO  WRK-NYUKIN
               MOVE    SYUPARA03-MISYU     TO  WRK-MISYUMONEY
               IF    ( ORCHC03-MOTOID      =   1 OR 2 OR 3 OR 4 )
                  AND( SYUPARA01-UPDFLG    =   ZERO )
                  AND( TPARA-MISYUKBN2     =   ZERO )
                   IF    ( ORCHC03-MATOME-NYUHENKIN-KBN
                                           =   SPACE )
                       MOVE    ZERO        TO  WRK-MISYUMONEY
                   END-IF
               END-IF
      *
               IF    ( TPARA-API-DATA      =   "1" )
                       MOVE    SYUPARA03-SRYKAMEI2
                                           TO  WRK-SRYKA-MEI
               ELSE
                   IF    ( ORCHC03-HAKKOFLG    =   4 )
                       MOVE    SPACE       TO  WRK-SRYKA-MEI
                   ELSE
                       MOVE    SYUPARA03-SRYKAMEI
                                           TO  WRK-SRYKA-MEI
                   END-IF
               END-IF
      *
               INITIALIZE                      HKNCOMBI-REC
                                               PTRSIINF-REC
               IF    ( SYUPARA03-HKNCOMBI-MAX  =   1 )
                   PERFORM 1001-HKNCOMBI-HEN-SEC
               END-IF
      *
               MOVE    SYUPARA03-HKNCOMBIMEI
                                           TO  WRK-HKNCOMBIMEI
      *
               IF    ( ORCHC03-PRTKBN      =   "1" )
      *
      *            帳票編集処理
                   PERFORM 300-HC03-HEN-SEC
      *
      *            帳票印刷処理
                   PERFORM 401-PRINT-SEC
               ELSE
      *
      *            帳票印刷開始処理
                   PERFORM 400-SYOKI-PRINT-SEC
      *
      *            帳票編集処理
                   PERFORM 300-HC03-HEN-SEC
      *
      *            帳票印刷処理
                   PERFORM 400-PRINT-SEC
               END-IF
      *
               PERFORM 900-PARA-KEY5-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       203-PARA-SYUNOU-EXT.
           EXIT.
      *****************************************************************
      *    複数診療科編集処理
      *****************************************************************
       2021-GRP-SRYKAMEI-HEN-SEC             SECTION.
      *
           PERFORM VARYING     IDH3    FROM    1   BY  1
                   UNTIL       IDH3    >   10
               IF      ORCHC03SUB-DOUJI-SRYKA (IDX-SYU IDH3)
                                       NOT =   SPACE
                   MOVE    ORCHC03SUB-DOUJI-SRYKA(IDX-SYU IDH3)
                                           TO  WRK-SRYKA
                   PERFORM 1000-SRYKAMEI-HEN-SEC
               END-IF
           END-PERFORM
           .
       2021-GRP-SRYKAMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角全角変換処理
      *****************************************************************
       800-HENKAN-SEC             SECTION.
      *
           IF    ( TPARA-API-DATA      =   "1" )
               MOVE    WRK-Z72         TO  WRK-MZ
               MOVE    WRK-MZ          TO  WRK-Z72-G
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI
               IF      WRK-Z72         <   ZERO
                   MOVE    WRK-Z72     TO  WRK-Z72-HS
                   MOVE    WRK-Z72-HS  TO  KANACHK-MAE-INPUT
               ELSE
                   MOVE    WRK-Z72     TO  WRK-Z72-H
                   MOVE    WRK-Z72-H   TO  KANACHK-MAE-INPUT
               END-IF
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
                   MOVE    KANACHK-OUT-INPUT(1:18)    TO  WRK-Z72-G
               END-IF
           END-IF
           .
       800-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角全角変換処理（ゼロ表示用）
      *****************************************************************
       810-HENKAN-SEC             SECTION.
      *
           IF    ( TPARA-API-DATA      =   "1" )
               MOVE    WRK-Z72         TO  WRK-M9
               MOVE    WRK-M9          TO  WRK-Z72-G
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               IF      WRK-Z72             <   ZERO
                   MOVE    WRK-Z72             TO  WRK-Z72-HS
                   MOVE    WRK-Z72-HS          TO  KANACHK-MAE-INPUT
               ELSE
                   MOVE    WRK-Z72             TO  WRK-Z72-H9
                   MOVE    WRK-Z72-H9          TO  KANACHK-MAE-INPUT
               END-IF
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
                   MOVE    KANACHK-OUT-INPUT(1:18)    TO  WRK-Z72-G
               END-IF
           END-IF
           .
       810-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    未収金額編集処理
      *****************************************************************
      *3101-MISYU-HEN-SEC             SECTION.
      *
      *    MOVE    ZERO                TO  WRK-MISYUMONEY
      *    MOVE    SYU-DENPNUM         TO  WRK-DENPNUM
      *    収納テーブル読み込み
      *    INITIALIZE                  SYUNOU-REC
      *    MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
      *    MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
      *    MOVE    SPA-PTID            TO  SYU-PTID
      *
      *    MOVE    SYUNOU-REC          TO  MCPDATA-REC
      *    MOVE    "DBSELECT"          TO  MCP-FUNC
      *    MOVE    "SYUNOU-KEY2"       TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "SYUNOU-KEY2"       TO  ORC-DBPATH
      *        PERFORM 980-SYUNOU-READ-SEC
      *    ELSE
      *        INITIALIZE                  SYUNOU-REC
      *        MOVE    1               TO  FLG-SYUNOU
      *    END-IF
      *    MOVE    ZERO                TO  WRK-MISYUMONEY
      *    PERFORM
      *        UNTIL     FLG-SYUNOU    =   1
      *        IF      ORCHC03-FUKU-DENPNUM    =   ZERO
      *            IF      SYU-DENPNUM     NOT =   WRK-DENPNUM
      *                COMPUTE WRK-MISYU       =   SYU-SKYMONEY
      *                                        -   SYU-NYUKIN-TOTAL
      *                ADD     WRK-MISYU           TO  WRK-MISYUMONEY
      *            END-IF
      *        ELSE
      *            IF      SYU-FUKU-DENPNUM    =   ORCHC03-FUKU-DENPNUM
      *                CONTINUE
      *            ELSE
      *                COMPUTE WRK-MISYU       =   SYU-SKYMONEY
      *                                        -   SYU-NYUKIN-TOTAL
      *                ADD     WRK-MISYU           TO  WRK-MISYUMONEY
      *            END-IF
      *        END-IF
      *
      *        MOVE    "SYUNOU-KEY2"       TO  ORC-DBPATH
      *        PERFORM 980-SYUNOU-READ-SEC
      *    END-PERFORM
      *
      *    MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *    MOVE    "SYUNOU-KEY2"       TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *    .
      *3101-MISYU-HEN-EXT.
      *****EXIT.
      *
      *****************************************************************
      *    自費名称編集処理
      *****************************************************************
       3102-JIHIMSG-HEN-SEC             SECTION.
      *
           MOVE    WRK-JIHIMEI(1)      TO  HC03-JIHIMSG1
           MOVE    WRK-JIHIMEI(2)      TO  HC03-JIHIMSG2
           MOVE    WRK-JIHIMEI(3)      TO  HC03-JIHIMSG3
           MOVE    WRK-JIHIMEI(4)      TO  HC03-JIHIMSG4
           MOVE    WRK-JIHIMEI(5)      TO  HC03-JIHIMSG5
           MOVE    WRK-JIHIMEI(6)      TO  HC03-JIHIMSG6
           MOVE    WRK-JIHIMEI(7)      TO  HC03-JIHIMSG7
           MOVE    WRK-JIHIMEI(8)      TO  HC03-JIHIMSG8
           MOVE    WRK-JIHIMEI(9)      TO  HC03-JIHIMSG9
           MOVE    WRK-JIHIMEI(10)     TO  HC03-JIHIMSG10
      *
           .
       3102-JIHIMSG-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療日編集処理
      *****************************************************************
       3101-SRYYMD-HEN-SEC         SECTION.
      *
           MOVE    SPACE                   TO  WRK-SRYYMDG
                                               WRK-LBLSRYYMD
                                               WRK-STSRYYMDG
                                               WRK-EDSRYYMDG
      *
           MOVE    "1"                     TO  WRK-SRYKIKANKBN
      *
           IF    ( ORCHC03-HAKKOFLG    =   4 )
               IF    ( SYUPARA03-KIKANKBN  =   "1" )
                   IF    ( SYUPARA03-STSRYYMD
                                       NOT =   SYUPARA03-EDSRYYMD )
                       MOVE    "診療日"    TO  WRK-LBLSRYYMD
                       PERFORM 31012-SRYYMD-HEN-SEC
                   ELSE
                       MOVE    "診療日"    TO  WRK-LBLSRYYMD
                       MOVE    SYU-SRYYMD  TO  WRK-SYMD
                       PERFORM 31012-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG TO  WRK-SRYYMDG
                                               WRK-STSRYYMDG
                   END-IF
               ELSE
                   MOVE    "2"             TO  WRK-SRYKIKANKBN
                   MOVE    "診療年月"      TO  WRK-LBLSRYYMD
                   MOVE    SYU-SRYYMD      TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   IF    ( TPARA-API-DATA  =   "1" )
                       MOVE    WRK-HENYMDG  (1:7)
                                           TO  WRK-STSRYYMDG
                   ELSE
                       MOVE    WRK-HENYMDG  (1:16)
                                           TO  WRK-SRYYMDG
                   END-IF
               END-IF
           ELSE
               IF    ( ORCHC03-PARAFLG         =   1 )
                AND  ( SYUPARA03-STSRYYMD  NOT =   SYUPARA03-EDSRYYMD )
                   MOVE    "診療日"        TO  WRK-LBLSRYYMD
                   PERFORM 31012-SRYYMD-HEN-SEC
               ELSE
                   MOVE    "診療日"        TO  WRK-LBLSRYYMD
                   MOVE    SYU-SRYYMD      TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG     TO  WRK-SRYYMDG
                                               WRK-STSRYYMDG
               END-IF
           END-IF
      *
           .
       3101-SRYYMD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療日編集処理
      *****************************************************************
       31012-SRYYMD-HEN-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYYMDG
      *
           MOVE    SYUPARA03-STSRYYMD  TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-STSRYYMDG
      *
           MOVE    SYUPARA03-EDSRYYMD  TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-EDSRYYMDG
      *    MOVE    WRK-HENYMDG (17:)   TO  WRK-EDSRYYMDG
      *
           STRING  WRK-STSRYYMDG       DELIMITED   BY  SIZE
                   "〜"                DELIMITED   BY  SIZE
                   WRK-EDSRYYMDG       DELIMITED   BY  SIZE
           INTO    WRK-SRYYMDG
           END-STRING
      *
           .
       31012-SRYYMD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC             SECTION.
      *
           IF    ( TPARA-API-DATA      =   "1" )
               PERFORM 800-DATE-SEC
           ELSE
               PERFORM 800-SEIWA-HEN-SEC
           END-IF
      *
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       800-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-HIZUKECHK
      *
           IF    ( WRK-YMD         =   "00000000" 
                                   OR  "99999999" )
               MOVE    WRK-YMD     TO  WRK-HENYMDG
                                       WRK-SYMD
           ELSE
               INITIALIZE                  ORCSGDAYAREA
               MOVE    WRK-YMD         TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
               IF  ( SGDAY-RC          =   ZERO )
                   MOVE    SGDAY-OTDAY TO  WRK-HENYMDG
                   MOVE    SGDAY-SDAY  TO  WRK-SYMD
               ELSE
                   MOVE    1           TO  FLG-HIZUKECHK
               END-IF
           END-IF
      *
           .
       800-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD        NOT  =   SPACE )
               INITIALIZE                  WRK-DATE
               MOVE    WRK-SYY             TO  WRK-DATE-YY
               MOVE    WRK-SMM             TO  WRK-DATE-MM
               MOVE    WRK-SDD             TO  WRK-DATE-DD
               MOVE    "-"                 TO  WRK-DATE-FL1
               MOVE    "-"                 TO  WRK-DATE-FL2
               IF    ( WRK-SYMD            =   "99999999" )
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-IF
           END-IF
      *
           MOVE    WRK-DATE            TO  WRK-HENYMDG
      *
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *
      *----(03.04.01)--ADD-START---
      *****************************************************************
      *    医療機関編集情報読み込み
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE           TO  SYS-1900-REC
           INITIALIZE                  SYS-1900-REC
           MOVE    "1900"          TO  SYS-1900-KANRICD
           MOVE    "*"             TO  SYS-1900-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1900-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1900-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(10) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
               END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
           .
      *
       900-1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報読み込み
      *****************************************************************
       900-1901-READ-SEC           SECTION.
      *
      *    医療機関名称編集情報読み込み
           MOVE    SPACE           TO  SYS-1901-REC
           INITIALIZE                  SYS-1901-REC
           MOVE    "1901"          TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(10)
                                   TO  SYS-1901-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1901-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-HOSPNAME(1:50)
               MOVE    SYS-1901-HOSPNAME2  TO  WRK-HOSPNAME(51:50)
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-API-HOSPNAME (1)
               MOVE    SYS-1901-HOSPNAME2  TO  WRK-API-HOSPNAME (2)
               MOVE    SYS-1901-HOSPNAME3  TO  WRK-API-HOSPNAME (3)
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
           .
      *
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
      *    医療機関住所編集情報読み込み
           MOVE    SPACE           TO  SYS-1902-REC
           INITIALIZE                  SYS-1902-REC
           MOVE    "1902"          TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(10)
                                   TO  SYS-1902-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1902-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-HOSPADRS(1:50)
               MOVE    SYS-1902-ADRS2  TO  WRK-HOSPADRS(51:50)
               MOVE    SYS-1902-TEL    TO  WRK-HOSPTEL
               MOVE    SYS-1902-ADRS1  TO  WRK-API-HOSPADRS (1)
               MOVE    SYS-1902-ADRS2  TO  WRK-API-HOSPADRS (2)
               MOVE    SYS-1902-ADRS3  TO  WRK-API-HOSPADRS (3)
               MOVE    SYS-1902-POST   TO  WRK-HOSPPOST
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
           .
      *
       900-1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ主キー読み込み処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-KEY10-EXT.
           EXIT.
      *----(03.04.01)--ADD-END-----
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   INITIALIZE                  PTINF-REC
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブル検索
      *****************************************************************
       900-PARA-KEY5-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           INITIALIZE                      PARA-REC
           MOVE    ORCHC03-PARA-GYOUMUID
                                       TO  PARA-GYOUMUID
           MOVE    ORCHC03-PARA-TERMID
                                       TO  PARA-TERMID
           MOVE    ORCHC03-PARA-FILEMEI
                                       TO  PARA-FILEMEI
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-PARA-KEY5-FET-SEC
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           .
       900-PARA-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブルFETCH
      *****************************************************************
       900-PARA-KEY5-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PARA-REC
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           .
       900-PARA-KEY5-FET-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブル検索処理
      *****************************************************************
       900-PARA-KEY4-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    ORCHC03-PARA-GYOUMUID
                                       TO  PARA-GYOUMUID
           MOVE    ORCHC03-PARA-TERMID TO  PARA-TERMID
           MOVE    ORCHC03-PARA-FILEMEI TO WRK-FILEMEI
           MOVE    "00101"             TO  WRK-FILEMEI-CD
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
           MOVE    1                   TO  PARA-EDANUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_para"      TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  PARA-REC
               ELSE
                   MOVE    1           TO  FLG-PARA
                   INITIALIZE              PARA-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
      *
       900-PARA-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           IF    ( SYU-HKNTEKKBN       =   "1" )
               MOVE    SYU-KOH-FTN     TO  SYU-HKNTEKMONEY
               MOVE    ZERO            TO  SYU-KOH-FTN
           END-IF
      *
           .
       980-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
     *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       910-PTRSIINF-READ-SEC         SECTION.
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTRSIINF-REC
                   MOVE    ZERO            TO  FLG-PTRSIINF
               ELSE
                   MOVE    1               TO  FLG-PTRSIINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
     *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-PTRSIINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報読み込み
      *****************************************************************
       910-PTHKNINF-READ-SEC       SECTION.
      *
           MOVE    SPA-HOSPNUM     TO  PTHKN-HOSPNUM
           MOVE    SPA-PTID        TO  PTHKN-PTID
           MOVE    COMB-HKNID      TO  PTHKN-HKNID
           MOVE    PTHKNINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTHKNINF-REC
                   MOVE    ZERO            TO  FLG-PTHKNINF
               ELSE
                   MOVE    1               TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
     *
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *
