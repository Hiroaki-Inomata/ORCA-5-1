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
       PROGRAM-ID.             A21000H01A4.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : カルテ（診療録）印刷
      *  管理者            :
      *  作成日付    作業者        記述
      *  03/03/31    @@@@−有村　  新規作成(ORCHC01 V1.0.6カスタマイズ)
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者       日付      内容
      *  01.00.01    @@@@-門間    11/01/07  コメント履歴削除
      *                                     open-cobol1.0対応
      *  01.00.02    @@@@-田中    11/01/24  4.6対応
      *  01.00.03    @@@@-田中    11/03/10  補足病名コメント対応
      *  01.00.04    @@@@-門間    12/06/19  保険者名称が12文字を超えた場合、
      *                                     2行表示（24文字）するように修正
      *  01.00.05    @@@@-萩野    13/01/09  カルテ発行(病名無)を選択した場合、
      *                                     病名を印字しないように修正
      *  01.00.06    @@@@-門間    14/01/16  第三者行為対応
      *  02.00.01    @@@@-門間    14/10/31  ver4.8.0対応（一時ディレクトリ変更）
      *  02.00.02    @@@@-門間    19/04/17  新元号対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    国保印刷用データ
           SELECT  PRT-FILE     ASSIGN  ASGNPARA
                                FILE    STATUS  IS  STS-PARA.
      *
      *    健保印刷用データ
           SELECT  PRT-FILE2    ASSIGN  ASGNPARA2
                                FILE    STATUS  IS  STS-PARA2.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    国保印刷用データ
       FD  PRT-FILE.
           COPY    "A21000H01KA4.INC".
      *
      *    健保印刷用データ
       FD  PRT-FILE2.
           COPY    "A21000H01SA4.INC".
      *
       WORKING-STORAGE             SECTION.
      *    国保印刷用データ 名称領域
           COPY    "CPCOMMONPRT.INC".
      *
      *    一時デイレクトリ領域
           COPY    "CPASGNPARA.INC".
      *
      *    健保印刷用データ 名称領域
           COPY    "CPCOMMONPRT.INC"   REPLACING  //HC01PARA//
                                       BY         //HC01PARA2//.
      *
      *    一時デイレクトリ領域
           COPY    "CPASGNPARA.INC"    REPLACING  //ASGNPARA//
                                       BY         //ASGNPARA2//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-PARA2           PIC X(02).
       01  STSX-AREA.
           03  STSX-PARA           PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ZOKUSHI         PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-INNO            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *    システム領域
      *01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
           03  IDXP                PIC 9(02).
           03  IDY                 PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDX2-1              PIC 9(01).
           03  IDX-H               PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  WRK-WSYSYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-PAGE            PIC 9(03).
           03  WRK-ADRS            PIC X(200).
           03  WRK-NENREI-X.
               05  WRK-NENREI      PIC 9(03).
           03  WRK-HKNJA           PIC X(08).
           03  WRK-HKNJA-G         PIC X(16).
           03  WRK-BYOMEI          PIC X(100).
      *
      *    文字変換
           03  WRK-ZENKAKU-G.
               05  WRK-ZENKAKU     PIC X(02)  OCCURS 10.
           03  WRK-MOJI            PIC X(10).
           03  WRK-MOJISU          PIC 9(02).
           03  WRK-MAE-INPUT       PIC X(200).
           03  WRK-OUT-INPUT       PIC X(200).
           03  WRK-HEN-KIGONUM     PIC X(80).
           03  WRK-KIGONUM         PIC X(80).
           03  WRK-NUM             PIC X(80).
           03  WRK-WARI            PIC 9(02)V9.
           03  WRK-Z6              PIC ZZZZ.
           03  WRK-Z62             PIC Z9.9.
           03  WRK-Z3              PIC ZZ9.
           03  WRK-Z9V9            PIC Z9.9.
           03  WRK-Z9              PIC Z9.
           03  WRK-HKNCOMBI        PIC X(40).
           03  WRK-HPTNUM          PIC X(08).
           03  WRK-FTNSNO          PIC X(16).
           03  WRK-JKYSNO          PIC X(20).
           03  WRK-TEKSTYMD        PIC X(22).
           03  WRK-TEKEDYMD        PIC X(22).
           03  WRK-STRYMD.
               05  WRK-STRGO       PIC X(04).
               05  WRK-STRYY       PIC X(04).
               05  WRK-STR1        PIC X(02).
               05  WRK-STRMM       PIC X(04).
               05  WRK-STR2        PIC X(02).
               05  WRK-STRDD       PIC X(04).
               05  WRK-STR3        PIC X(02).
           03  WRK-ENDYMD.
               05  WRK-ENDGO       PIC X(04).
               05  WRK-ENDYY       PIC X(04).
               05  WRK-END1        PIC X(02).
               05  WRK-ENDMM       PIC X(04).
               05  WRK-END2        PIC X(02).
               05  WRK-ENDDD       PIC X(04).
               05  WRK-END3        PIC X(02).
           03  WRK-STRYMDW.
               05  WRK-STRGOW      PIC X(01).
               05  WRK-STRYYW      PIC X(02).
               05  WRK-STR1W       PIC X(01).
               05  WRK-STRMMW      PIC X(02).
               05  WRK-STR2W       PIC X(01).
               05  WRK-STRDDW      PIC X(02).
           03  WRK-ENDYMDW.
               05  WRK-ENDGOW      PIC X(01).
               05  WRK-ENDYYW      PIC X(02).
               05  WRK-END1W       PIC X(01).
               05  WRK-ENDMMW      PIC X(02).
               05  WRK-END2W       PIC X(01).
               05  WRK-ENDDDW      PIC X(02).
           03  WRK-ERRMSG          PIC X(40).
           03  WRK-MSG             PIC X(200).
      *
           03  WRK-BIKO            PIC X(40).
      *
           03  WRK-COMB-HKNNUM         PIC X(03).
           03  WRK-COMB-KOHID          PIC 9(10).
      *
      *    保険組合せテーブル
       01  TBL-HKNCOMBI-AREA.
           03  TBL-HKNCOMBI-OCC        OCCURS   20.
               05  TBL-HKNCOMBINUM     PIC X(04).
               05  TBL-HKNNUM          PIC X(03).
               05  TBL-KOHID           PIC 9(10).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC X(02).
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC X(02).
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC X(02).
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
           03  WRK-SPA-SYSYYYY.
               05  WRK-SPA-SYSYY   PIC  9(04).
           03  WRK-POSTG.
               05  WRK-POST1       PIC  X(03).
               05  FILLER          PIC  X(01)  VALUE  "-".
               05  WRK-POST2       PIC  X(04).
      *
      *    日本語数字
       01  TBL-SUJI-AREA.
           03  FILLER              PIC X(20)
                                   VALUE "１２３４５６７８９０".
       01  TBL-SUJI-R              REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI            PIC X(02)   OCCURS   10.
      *
       01  DUMMY PIC X(03).
      *
      *    プログラムオプションパラメタ
       01  PARA-AREA.
           03  PARA-HKNCOMBIKBN        PIC X(01).
           03  PARA-DAISANKBN          PIC X(01).
           03  PARA-FORMKBN            PIC X(01).
      *
           03  WRK-PARA-NAME           PIC X(25).
           03  WRK-PARA                PIC X(50).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *    患者病名マスタ
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    患者登録機能制御情報
           COPY    "CPSK1017.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
           COPY  "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      *    一括用
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    保険者号マスタ
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *    保険年齢パラメタ
           COPY    "CPORCSAGECHK.INC".
      *
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "COMMON-SPA".
      *
      *    カルテ出力パラメタ
           COPY    "CPORCHC01.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHC01AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM  100-INIT-SEC
      *
           PERFORM  200-MAIN-SEC  UNTIL  FLG-END  =  1
      *
           PERFORM  300-END-SEC
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
           INITIALIZE                  H01KA4
           INITIALIZE                  H01SA4
           MOVE    ZERO                TO  WRK-PAGE
      *
           ACCEPT  SYS-TIME            FROM  TIME
      *
      *    ファイルＯＰＥＮ
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    SPA-HOSPNUM         TO  HC01PARA2-HOSPNUM
           MOVE    "HC01"              TO  HC01PARA-FORM-ID
           MOVE    "H1A4"              TO  HC01PARA2-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
                                           HC01PARA2-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
                                           HC01PARA2-SYOHMS
           ADD     1                   TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC01PARA-CNT
                                           HC01PARA2-CNT
      *
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAMES(1)
           MOVE    HC01PARA2           TO  SGETTEMP-BASENAMES(2)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES(1)   TO  ASGNPARA
           MOVE    SGETTEMP-FULLNAMES(2)   TO  ASGNPARA2
      *
           OPEN    OUTPUT               PRT-FILE
                                        PRT-FILE2
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC                   SECTION.
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                SYS-1031-REC
           INITIALIZE               ORCSPRTNMAREA
           MOVE    "2"              TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD       TO  ORCSPRTNM-SRYYMD
           MOVE    "2"              TO  ORCSPRTNM-NYUGAIKBN
      *
           CALL    "ORCSPRTNM"      USING
                                    ORCSPRTNMAREA
                                    SPA-AREA
                                    SYS-1031-REC
      *
           IF   ORCSPRTNM-RC  NOT =  ZERO
             MOVE    1              TO  FLG-END
           END-IF
      *
      *    患者登録機能制御情報
           MOVE    SPACE            TO  SYS-1017-REC
           INITIALIZE               SYS-1017-REC
           MOVE    "1017"           TO  SYS-1017-KANRICD
           MOVE    "*"              TO  SYS-1017-KBNCD
           MOVE    SPA-SYSYMD       TO  SYS-1017-STYUKYMD
           MOVE    SPA-SYSYMD       TO  SYS-1017-EDYUKYMD
           MOVE    SPA-HOSPNUM      TO  SYS-1017-HOSPNUM
           MOVE    SYS-1017-REC     TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI  =  ZERO
             MOVE    MCPDATA-REC    TO  SYS-1017-REC
             IF   ORCHC01-SYORIKBN  =  "3"
               MOVE  ORCHC01-BYOMEIFLG    TO  SYS-1017-KARUTE-BYOMEI
             END-IF
           ELSE
             INITIALIZE             SYS-1017-REC
           END-IF
      *    医療機関情報
           MOVE    SPACE            TO   SYS-1001-REC
           INITIALIZE               SYS-1001-REC
           MOVE    "1001"           TO   SYS-1001-KANRICD
           MOVE    "*"              TO   SYS-1001-KBNCD
           MOVE    SPA-SYSYMD       TO   SYS-1001-EDYUKYMD
           MOVE    SPA-SYSYMD       TO   SYS-1001-STYUKYMD
           MOVE    SPA-HOSPNUM      TO   SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC     TO   MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI  =  ZERO
             MOVE    MCPDATA-REC    TO   SYS-1001-REC
           ELSE
             INITIALIZE             SYS-1001-REC
           END-IF
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           INITIALIZE                  PARA-AREA
      *
      *    保険適応病名区分
           MOVE    "HKNCOMBIKBN"       TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-HKNCOMBIKBN
      *
      *    第三者編集区分
           MOVE    "DAISANKBN"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-DAISANKBN
      *
      *    様式設定区分
           MOVE    "FORMKBN"           TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-FORMKBN
      *
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションパラメタ取得処理
      *****************************************************************
       1001-OPTION-CALL-SEC             SECTION.
      *
           INITIALIZE                  ORCSPRGOPTIONAREA
           MOVE    "A21000H01A4"       TO  ORCSPRGOPTION-PRGID
           MOVE    WRK-PARA-NAME       TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
           MOVE    SPACE               TO  WRK-PARA
           IF      ORCSPRGOPTION-OT-TYPE   =   1
      *        半角で１文字目を値とする
               MOVE    ORCSPRGOPTION-OT-PARA(1:1)  TO  WRK-PARA(1:1)
           END-IF
           .
       1001-OPTION-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    印刷項目編集
            PERFORM 2001-SYORI-SEC
      *    印刷用データ出力
      *
            WRITE  H01KA4
            WRITE  H01SA4
      *
            CLOSE                       PRT-FILE
                                        PRT-FILE2
      *
           IF ORCHC01-SYORIKBN   NOT = "3"
      *
      *     国保・健保印刷判別
             EVALUATE   TRUE
               WHEN  PTHKN-HKNNUM  =  "060"  OR  "067"  OR  "039"
                                  OR  "040"  OR  "068"  OR  "069"
                 INITIALIZE                  ORCSMKPRTAREA
                 MOVE   "HC01.sh"            TO  MKPRT-ID
                 MOVE   "A21000H01KA4.red"   TO  MKPRT-DIA
                 IF  PARA-FORMKBN  =  "1"
                   MOVE   "A21000H01KA4V02.red"  TO  MKPRT-DIA
                 END-IF
                 MOVE   SPACE                TO  MKPRT-DEF
                 MOVE   ASGNPARA             TO  MKPRT-DAT
                 MOVE   SYS-1031-PRTNM(1)    TO  MKPRT-PRTNM
                 CALL   "ORCSMKPRTSITE"      USING
                                             ORCSMKPRTAREA
                                             SPA-AREA
               WHEN   OTHER
                 INITIALIZE                  ORCSMKPRTAREA
                 MOVE   "HC01.sh"            TO  MKPRT-ID
                 MOVE   "A21000H01SA4.red"   TO  MKPRT-DIA
                 IF  PARA-FORMKBN  =  "1"
                   MOVE   "A21000H01SA4V02.red"  TO  MKPRT-DIA
                 END-IF
                 MOVE   SPACE                TO  MKPRT-DEF
                 MOVE   ASGNPARA2            TO  MKPRT-DAT
                 MOVE   SYS-1031-PRTNM(1)    TO  MKPRT-PRTNM
                 CALL   "ORCSMKPRTSITE"      USING
                                             ORCSMKPRTAREA
                                             SPA-AREA
             END-EVALUATE
      *
           ELSE
      *
      *     国保・健保印刷判別
             EVALUATE   TRUE
               WHEN  PTHKN-HKNNUM  =  "060"  OR  "067"  OR  "039"
                                  OR  "040"  OR  "068"  OR  "069"
      *
                 MOVE    ORCHC01-PRTAREA   TO  ORCSPRTAREA
      *
                 MOVE    H01KA4       TO  SPRT-PRTDATA
                 MOVE    "2"          TO  SPRT-SITEKBN
                 MOVE    "A21000H01KA4.red"    TO  SPRT-PRTID
                 IF  PARA-FORMKBN  =  "1"
                   MOVE    "A21000H01KA4V02.red"  TO  SPRT-PRTID
                 END-IF
                 MOVE    "診療録（カルテ１号紙）（外来）一括"
                                      TO  SPRT-TITLE
                 CALL    "ORCSPRT"    USING
                                      ORCSPRTAREA
                                      SPA-AREA
                 IF  SPRT-RETURN  =  ZERO
                   ADD     1          TO  ORCHC01-CNT
                 ELSE
                   MOVE    9          TO  ORCHC01-RC
                 END-IF
               WHEN   OTHER
      *
                 MOVE    ORCHC01-PRTAREA   TO  ORCSPRTAREA
      *
                 MOVE    H01SA4       TO  SPRT-PRTDATA
                 MOVE    "2"          TO  SPRT-SITEKBN
                 MOVE    "A21000H01SA4.red"  TO  SPRT-PRTID
                 IF  PARA-FORMKBN  =  "1"
                   MOVE    "A21000H01SA4V02.red"  TO  SPRT-PRTID
                 END-IF
                 MOVE    "診療録（カルテ１号紙）（外来）一括"
                                      TO  SPRT-TITLE
                 CALL    "ORCSPRT"    USING
                                      ORCSPRTAREA
                                      SPA-AREA
                 IF  SPRT-RETURN  =  ZERO
                   ADD     1          TO  ORCHC01-CNT
                 ELSE
                   MOVE    9          TO  ORCHC01-RC
                 END-IF
             END-EVALUATE
           END-IF
      *
           IF  ORCHC01-SYORIKBN   NOT = "4"
      *        続紙あり
               IF  FLG-ZOKUSHI  =  1
                   AND  FLG-PTBYOMEI  =  ZERO
                 PERFORM 2002-ZOKUSHI-SEC
                 CLOSE                    PRT-FILE
                                          PRT-FILE2
               END-IF
      *
               MOVE    "DBCLOSECURSOR"    TO  MCP-FUNC
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           END-IF
      *
           MOVE    1                  TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       2001-SYORI-SEC                SECTION.
      *
      *    患者マスター検索
           INITIALIZE                PTINF-REC
           MOVE    SPA-HOSPNUM       TO  PTINF-HOSPNUM
           MOVE    SPA-PTID          TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           IF   FLG-PTINF  =  1
             GO      TO      2001-SYORI-EXT
           END-IF
      *
      *    保険組合せマスター検索
           INITIALIZE                HKNCOMBI-REC
           MOVE    SPA-HOSPNUM       TO  COMB-HOSPNUM
           MOVE    SPA-PTID          TO  COMB-PTID
           MOVE    SPA-HKNCOMBINUM   TO  COMB-HKNCOMBINUM
           PERFORM 910-HKNCOMBI-READ-SEC
           IF   FLG-HKNCOMBI  =  1
             INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
      *    患者保険情報検索
           INITIALIZE                PTHKNINF-REC
           MOVE    SPA-HOSPNUM       TO  PTHKN-HOSPNUM
           MOVE    SPA-PTID          TO  PTHKN-PTID
      *    主保険−ＩＤ
           MOVE    COMB-HKNID        TO  PTHKN-HKNID
           PERFORM 920-PTHKNINF-READ-SEC
           IF      FLG-PTHKNINF  NOT =   ZERO
             INITIALIZE              PTHKNINF-REC
           END-IF
      *
      *    保険者番号変換処理
           MOVE    SPACE             TO  WRK-HKNJA
           MOVE    9                 TO  IDY
           MOVE    ZERO              TO  FLG-OK
           PERFORM  VARYING  IDX  FROM  8  BY  -1
                    UNTIL  IDX  <  1
             IF   PTHKN-HKNJANUM(IDX:1)   NOT =   SPACE
               MOVE    1             TO  FLG-OK
             END-IF
             IF    FLG-OK  =  1
               COMPUTE IDY  =  IDY  -  1
               MOVE    PTHKN-HKNJANUM(IDX:1)   TO  WRK-HKNJA (IDY:1)
             END-IF
           END-PERFORM
           MOVE    16                  TO  WRK-MOJISU
           MOVE    WRK-HKNJA           TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT(1:16) TO  WRK-HKNJA-G
      *
      *    保険番号マスター検索
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    COMB-HKNNUM         TO  HKN-HKNNUM
           MOVE    COMB-TEKSTYMD       TO  HKN-TEKSTYMD
           PERFORM 930-HKNNUM-READ-SEC
           IF      FLG-HKNNUM  NOT =   ZERO
               INITIALIZE              HKNNUM-REC
           END-IF
      *
      *    保険者番号　検索チェック
           INITIALIZE                  HKNJAINF-REC
           MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
           MOVE    PTHKN-HKNJA-G       TO  HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =   ZERO
             MOVE    "tbl_hknjainf"    TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             PERFORM  900-HKNJAINF-READ-SEC
           ELSE
             INITIALIZE                HKNJAINF-REC
             MOVE    1                 TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
      *    帳票編集処理
           PERFORM  310-HC01-HEN-SEC
      *
      *    患者病名編集
           IF  ORCHC01-SYORIKBN   NOT = "4"
               PERFORM  311-HC01-HEN-SEC
           END-IF
      *
           .
       2001-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       310-HC01-HEN-SEC                SECTION.
      *
           INITIALIZE  H01KA4
           INITIALIZE  H01SA4
           INITIALIZE  H01SA4-KANANAME
      *
      *    患者マスターより
           MOVE    SPA-PTNUM         TO  H01KA4-PTNUM
                                         H01SA4-PTNUM
           IF  PTINF-NAME(21:)  =  SPACE
               MOVE    PTINF-NAME    TO  H01KA4-NAME
                                         H01SA4-NAME
           ELSE
               MOVE    PTINF-NAME    TO  H01KA4-NAME2
                                         H01SA4-NAME2
           END-IF
           MOVE    PTINF-KANANAME    TO  H01KA4-KANANAME
                                         H01SA4-KANANAME
      *
           EVALUATE    PTINF-SEX
             WHEN  "1"
               MOVE    "○"          TO  H01KA4-OTOKO
                                         H01SA4-OTOKO
             WHEN  "2"
               MOVE    "○"          TO  H01SA4-ONNA
               IF   PTHKN-HKNNUM  =  "039"  OR  "040"
                 MOVE    "○"        TO  H01KA4-ONNA2
               ELSE
                 MOVE    "○"        TO  H01KA4-ONNA
               END-IF
           END-EVALUATE
      *
           MOVE    PTINF-BIRTHDAY    TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
      *
           IF  PARA-FORMKBN  =  "1"
      *      201905以降の様式
             MOVE    WRK-HENYMDG(1:4)  TO  H01KA4-TANJYOG
                                           H01SA4-TANJYOG
           ELSE
      *      201904以前の様式
             EVALUATE  LNK-DAY2-EDTYMD1-G
               WHEN  "M"
                 MOVE    "○"          TO  H01KA4-MEIJI
                                           H01SA4-MEIJI
               WHEN  "T"
                 MOVE    "○"          TO  H01KA4-TAISYOU
                                           H01SA4-TAISYOU
               WHEN  "S"
                 MOVE    "○"          TO  H01KA4-SYOUWA
                                           H01SA4-SYOUWA
               WHEN  "H"
                 MOVE    "○"          TO  H01KA4-HEISEI
                                           H01SA4-HEISEI
             END-EVALUATE
           END-IF
      *
           MOVE    WRK-HYY           TO  H01KA4-TANJYOY
                                         H01SA4-TANJYOY
           MOVE    WRK-HMM           TO  H01KA4-TANJYOM
                                         H01SA4-TANJYOM
           MOVE    WRK-HDD           TO  H01KA4-TANJYOD
                                         H01SA4-TANJYOD
      *    年齢
           MOVE    SPA-SYSYMD(1:4)   TO  WRK-SPA-SYSYYYY
           COMPUTE  WRK-NENREI  =  WRK-SPA-SYSYY
                                -  WRK-SYY
           IF   SPA-SYSYMD(5:2)  <  WRK-SYMD(5:2)
      *    現在日付が誕生月に達していない場合
             COMPUTE  WRK-NENREI  =  WRK-NENREI  -  1
           ELSE
      *    現在日付と誕生月が一致して誕生日が１日以降の場合
             IF   SPA-SYSYMD(5:2)   =  WRK-SYMD(5:2)
                 AND  WRK-SYMD(7:2)  >  01
               COMPUTE  WRK-NENREI  =  WRK-NENREI  -  1
             END-IF
           END-IF
           MOVE    WRK-NENREI         TO  WRK-Z3
      *
      *    自宅−住所
           MOVE    SPACE              TO  WRK-ADRS
           STRING  PTINF-HOME-ADRS    DELIMITED   BY  SPACE
                   PTINF-HOME-BANTI   DELIMITED   BY  SPACE
                   INTO                   WRK-ADRS
           END-STRING
           MOVE    WRK-ADRS           TO  H01KA4-ADRS
                                          H01SA4-ADRS
      *    自宅郵便番号
           MOVE    PTINF-HOME-POST(1:3)   TO  WRK-POST1
           MOVE    PTINF-HOME-POST(4:4)   TO  WRK-POST2
           MOVE    WRK-POSTG          TO  H01KA4-ADRSPOST
                                          H01SA4-ADRSPOST
      *
      *    自宅−昼電話番号
           MOVE    PTINF-HOME-TEL1    TO  H01KA4-ADRSTEL
                                          H01SA4-ADRSTEL
      *    続柄
           MOVE    PTINF-ZOKUGARA     TO  H01KA4-TUZUKI
                                          H01SA4-TUZUKI
           IF   PTHKN-HKNNUM  =  "039"  OR  "040"
             MOVE     SPACE           TO  H01KA4-TUZUKI
           END-IF
      *    職業
           MOVE    PTINF-JOB          TO  H01KA4-WORK
                                          H01SA4-WORK
      *
      *事業所
      *    名称
           MOVE    PTINF-OFFICE-NAME  TO  H01SA4-KINNAME
      *    勤務先−住所,番地方書
           MOVE    SPACE              TO  WRK-ADRS
           STRING  PTINF-OFFICE-ADRS   DELIMITED   BY  SPACE
                   PTINF-OFFICE-BANTI  DELIMITED   BY  SPACE
                                       INTO        WRK-ADRS
           END-STRING
           MOVE    WRK-ADRS           TO  H01SA4-KINADRS
      *    電話番号
           MOVE    PTINF-OFFICE-TEL   TO  H01SA4-KINADRSTEL
           IF  COMB-HKNNUM  =  "975"
             MOVE    COMB-SYU-TANSEIDONAME   TO  WRK-HKNJA-G
           END-IF
      *
      *    保険者情報
      *    保険者番号
           MOVE    WRK-HKNJA-G        TO  H01KA4-HPTNUM
                                          H01SA4-HPTNUM
      *
      *    記号・番号
      *    全角変換
           MOVE    PTHKN-KIGO         TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT(1:KANACHK-MAX)   TO  WRK-KIGONUM
           MOVE    PTHKN-NUM          TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT(1:KANACHK-MAX)   TO  WRK-NUM
      *
           IF   ( WRK-KIGONUM  =  SPACE )
               AND  ( WRK-NUM  =  SPACE )
             MOVE    SPACE            TO  WRK-HEN-KIGONUM
           ELSE
             STRING  WRK-KIGONUM      DELIMITED   BY  SPACE
                     "・"             DELIMITED   BY  SIZE
                     WRK-NUM          DELIMITED   BY  SPACE
                 INTO                 WRK-HEN-KIGONUM
             END-STRING
           END-IF
      *
      *    後期高齢者は番号のみ
           MOVE    WRK-HEN-KIGONUM    TO  H01KA4-KIGONUM
                                          H01SA4-KIGONUM
           IF  (COMB-HKNNUM  =  "039" )
               AND (WRK-KIGONUM  =  SPACE )
             MOVE    WRK-NUM          TO  H01KA4-KIGONUM
                                          H01SA4-KIGONUM
           END-IF
      *
      *    記号・番号が１６文字以上は別行へ
           IF   WRK-HEN-KIGONUM(33:)  NOT =  SPACE
             MOVE    SPACE            TO  H01KA4-KIGONUM
                                          H01SA4-KIGONUM
             MOVE    WRK-KIGONUM      TO  H01KA4-KIGO
                                          H01SA4-KIGO
             MOVE    WRK-NUM          TO  H01KA4-NUM
                                          H01SA4-NUM
           END-IF
      *
      *    被保険者名・世帯主に変更
           MOVE    PTINF-SETAINUSI    TO  H01KA4-HIHKNJNAME
           MOVE    PTHKN-HIHKNJANAME  TO  H01SA4-HIHKNJNAME
      *
      *    適用開始年月日（資格取得日へ）
           IF   PTHKN-SKKGETYMD  =  SPACE
             CONTINUE
           ELSE
             MOVE    PTHKN-SKKGETYMD  TO  WRK-SYMD
             PERFORM  31012-SEIWA-HEN-SEC
      *
             IF  PARA-FORMKBN  =  "1"
      *        201905以降の様式
               MOVE    WRK-HENYMDG(1:4)  TO  H01KA4-SIKAKUG
                                             H01SA4-SIKAKUG
             ELSE
      *        201904以前の様式
               EVALUATE  LNK-DAY2-EDTYMD1-G
               WHEN  "S"
                 MOVE    "○"           TO  H01KA4-SKKSYOWA
                                            H01SA4-SKKSYOWA
               WHEN  "H"
                 MOVE    "○"           TO  H01KA4-SKKHEISEI
                                            H01SA4-SKKHEISEI
               END-EVALUATE
             END-IF
      *
             MOVE    WRK-HYY          TO  H01KA4-SIKAKUY
                                          H01SA4-SIKAKUY
             MOVE    WRK-HMM          TO  H01KA4-SIKAKUM
                                          H01SA4-SIKAKUM
             MOVE    WRK-HDD          TO  H01KA4-SIKAKUD
                                          H01SA4-SIKAKUD
           END-IF
      *    適用終了年月日
           IF   PTHKN-TEKEDYMD  =  SPACE  OR  ALL "9"
             CONTINUE
           ELSE
             MOVE    PTHKN-TEKEDYMD   TO  WRK-SYMD
             PERFORM 31012-SEIWA-HEN-SEC
      *
             IF  PARA-FORMKBN  =  "1"
      *        201905以降の様式
               MOVE    WRK-HENYMDG(1:4)  TO  H01KA4-YUKOG
                                             H01SA4-YUKOG
             END-IF
      *
             MOVE    WRK-HYY          TO  H01KA4-YUKOY
                                          H01SA4-YUKOY
             MOVE    WRK-HMM          TO  H01KA4-YUKOM
                                          H01SA4-YUKOM
             MOVE    WRK-HDD          TO  H01KA4-YUKOD
                                          H01SA4-YUKOD
           END-IF
      *
      *    保険者情報（国保、後期）
           MOVE    HKNJA-HKNJANAME    TO  H01KA4-HKNJANAME
      *
      *    保険者情報（社保）
           IF  HKNJA-HKNJANAME(25:)  =  SPACE
             MOVE    HKNJA-HKNJANAME    TO  H01SA4-HKNNAME
           ELSE
             MOVE    HKNJA-HKNJANAME    TO  H01SA4-HKNNAME2
           END-IF
      *
           STRING  HKNJA-ADRS         DELIMITED  BY  SPACE
                   HKNJA-BANTI        DELIMITED  BY  SPACE
                                      INTO  H01SA4-HKNADRS
           END-STRING
           IF   PTHKN-HKNNUM  =  "039"  OR  "040"
             STRING   HKNJA-ADRS      DELIMITED  BY  SPACE
                      HKNJA-BANTI     DELIMITED  BY  SPACE
                                      INTO  H01KA4-HKNJAADR
             END-STRING
           END-IF
           MOVE    HKNJA-TEL          TO  H01SA4-HKNADRSTEL
      *
      *    保険算定年齢チェック等
           INITIALIZE                 ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM        TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID           TO  AGECHK-PTID
           MOVE    SPA-SRYYMD         TO  AGECHK-SRYYMD
           MOVE    COMB-HKNCOMBINUM   TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"       USING
                                      ORCSAGECHKAREA
                                      SPA-AREA
           IF   SPA-SRYYMD  <  "20080401"
             IF   WRK-NENREI  <  3   AND  COMB-GAI-KFTNRATE = 30
               MOVE    2              TO  WRK-Z9
               MOVE    WRK-Z9         TO  H01KA4-FTNRITU
                                           H01SA4-FTNRITU
             ELSE
               COMPUTE  WRK-WARI  =  COMB-GAI-KFTNRATE  /  10
               IF   WRK-WARI(3:1)  =   ZERO
                 MOVE    WRK-WARI     TO  WRK-Z9
                 MOVE    WRK-Z9       TO  H01KA4-FTNRITU
                                          H01SA4-FTNRITU
               ELSE
                 MOVE    WRK-WARI     TO  WRK-Z9V9
                 MOVE    WRK-Z9V9     TO  H01KA4-FTNRITU
                                          H01SA4-FTNRITU
               END-IF
             END-IF
           ELSE
             IF  AGECHK-SYUGAKUKBN  =  1
                 AND  COMB-GAI-KFTNRATE  =  30
               MOVE   2               TO  WRK-Z9
               MOVE   WRK-Z9          TO  H01KA4-FTNRITU
                                          H01SA4-FTNRITU
             ELSE
               COMPUTE  WRK-WARI  =   COMB-GAI-KFTNRATE  /  10
               IF   WRK-WARI(3:1) =   ZERO
                 MOVE    WRK-WARI     TO  WRK-Z9
                 MOVE    WRK-Z9       TO  H01KA4-FTNRITU
                                          H01SA4-FTNRITU
               ELSE
                 MOVE    WRK-WARI     TO  WRK-Z9V9
                 MOVE    WRK-Z9V9     TO  H01KA4-FTNRITU
                                          H01SA4-FTNRITU
               END-IF
             END-IF
           END-IF
      *
           IF   PTHKN-HKNNUM = SPACE
             MOVE    SPACE        TO  H01KA4-FTNRITU
                                      H01SA4-FTNRITU
           END-IF
      *
      *    保険区分 主保険−短縮制度名
           MOVE    SPACE          TO  WRK-HKNCOMBI
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
                                           INTO       WRK-HKNCOMBI
           END-STRING
      *
      *    公費情報
           PERFORM  VARYING  IDY FROM  1  BY  1
                    UNTIL  IDY  >  2
             INITIALIZE                  PTKOHINF-REC
             MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
             MOVE    SPA-PTID            TO  PTKOH-PTID
             MOVE    COMB-KOHID(IDY)     TO  PTKOH-KOHID
             PERFORM 940-PTKOHINF-READ-SEC
             IF   FLG-PTKOHINF  =  ZERO
               IF   (PTKOH-KOHNUM  =  "141"
                   OR   PTKOH-KOHNUM  =  "143"
                   OR   PTKOH-KOHNUM  =  "144"
                   OR   PTKOH-KOHNUM  =  "145"
                   OR   PTKOH-KOHNUM  =  "146"
                   OR   PTKOH-KOHNUM  =  "147")
                   AND  SYS-1001-PREFNUM  =  "21"
      *    受給者番号
                 MOVE    PTKOH-JKYSNUM  TO  H01KA4-FK-JKYSNO
                                            H01SA4-FK-JKYSNO
      *    福祉医療費名称
                 EVALUATE  PTKOH-KOHNUM
                   WHEN  "141"
                   WHEN  "147"
                     MOVE    "老人"     TO  H01KA4-FK-NAME
                                            H01SA4-FK-NAME
                   WHEN  "145"
                     MOVE    "乳児"     TO  H01KA4-FK-NAME
                                            H01SA4-FK-NAME
                   WHEN  "144"
                     MOVE    "重老"     TO  H01KA4-FK-NAME
                                            H01SA4-FK-NAME
                   WHEN  "146"
                     MOVE    "重度"     TO  H01KA4-FK-NAME
                                            H01SA4-FK-NAME
                   WHEN  "143"
                     MOVE    "母子"     TO  H01KA4-FK-NAME
                                            H01SA4-FK-NAME
                 END-EVALUATE
               ELSE
                 PERFORM 3102-PTKOHINF-HEN-SEC
                 MOVE    WRK-FTNSNO     TO  H01KA4-FTNSNO(IDY)
                                            H01SA4-FTNSNO(IDY)
                 IF   WRK-JKYSNO(15:)  =  SPACE
                   MOVE    WRK-JKYSNO   TO  H01KA4-JKYSNO(IDY)
                                            H01SA4-JKYSNO(IDY)
                 ELSE
                   MOVE    WRK-JKYSNO   TO  H01KA4-JKYSNON(IDY)
                                            H01SA4-JKYSNON(IDY)
                 END-IF
               END-IF
             ELSE
               MOVE    "平成　　年　　月　　日"   TO  WRK-TEKSTYMD
                                                      WRK-TEKEDYMD
             END-IF
           END-PERFORM
      *
      *    労災の時、保険者番号・記号の変更
           IF   COMB-HKNNUM  =  "971"  OR  "973"
      *    患者保険情報検索
             INITIALIZE                      PTRSIINF-REC
             MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
             MOVE    SPA-PTID            TO  PTRSI-PTID
             MOVE    COMB-HKNID          TO  PTRSI-HKNID
             MOVE    PTRSIINF-REC        TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    "tbl_ptrsiinf"    TO  MCP-TABLE
               MOVE    "key"             TO  MCP-PATHNAME
               PERFORM 993-PTRSIINF-NEXT-SEC
             ELSE
               INITIALIZE                    PTRSIINF-REC
               MOVE    1                 TO  FLG-PTRSIINF
             END-IF
             MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
             MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF   FLG-PTRSIINF  =  ZERO
      *    保険者番号へ記号（労災区分名）
               MOVE    WRK-HEN-KIGONUM   TO  H01KA4-HPTNUM
                                             H01SA4-HPTNUM
      *    記号へ患者交付番号
               MOVE    PTRSI-KOFUNUM     TO  WRK-MAE-INPUT
               PERFORM  2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT(1:KANACHK-MAX)  TO  H01KA4-KIGONUM
                                                         H01SA4-KIGONUM
      *
             END-IF
           END-IF
           .
       310-HC01-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者病名編集処理
      *****************************************************************
       311-HC01-HEN-SEC        SECTION.
      *
      *    保険組合せテーブル編集
           INITIALIZE                  TBL-HKNCOMBI-AREA
           IF      SPA-HKNCOMBINUM     =   ZERO
             MOVE    SPACE               TO  WRK-COMB-HKNNUM
           ELSE
             MOVE    COMB-HKNNUM         TO  WRK-COMB-HKNNUM
      *      第三者
             IF      COMB-KOH1HKNNUM     =   "970"
               MOVE    COMB-KOH1HKNNUM TO  WRK-COMB-HKNNUM
               MOVE    COMB-KOH1ID     TO  WRK-COMB-KOHID
             END-IF
           END-IF
           IF     (PARA-HKNCOMBIKBN    =   "1"  OR  "2" )
             PERFORM 2009-HKNCOMBI-HEN-SEC
           END-IF
      *
      *    患者病名マスタより編集
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  FLG-ZOKUSHI
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-PTID            TO  PTBYO-PTID
           MOVE    SPA-SRYKA           TO  PTBYO-SRYKA
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "tbl_ptbyomei"    TO  MCP-TABLE
             MOVE    "key2"            TO  MCP-PATHNAME
             PERFORM 950-PTBYOMEI-READ-SEC
           ELSE
             MOVE    1                 TO  FLG-PTBYOMEI
           END-IF
           PERFORM  UNTIL  ( FLG-PTBYOMEI  NOT =  ZERO )
                    OR  ( FLG-ZOKUSHI  NOT =  ZERO )
      *
             MOVE    ZERO                TO  FLG-INNO
             IF  ( SYS-1017-KARUTE-BYOMEI   =   "1"   )  AND
                 ( PTBYO-TENKIYMD       NOT =   SPACE )
               MOVE    1                   TO  FLG-INNO
             END-IF
      *      入院分は対象外
             IF  ( PTBYO-NYUGAIKBN          =   "1" )
               MOVE    1                   TO  FLG-INNO
             END-IF
      *      保険適応病名判定
             IF  ( FLG-INNO                 =   ZERO  )  AND
                 ( PARA-HKNCOMBIKBN         =   "1"  OR  "2" )
                 PERFORM 20024-HKNCOMBI-CHK-SEC
             END-IF
      *
      *    継続病名のみの時、転帰日のあるものを対象外とする
             IF      FLG-INNO        =   ZERO
               IF     SYS-1017-KARUTE-BYOMEI  =  "2"
                 CONTINUE
               ELSE
      *    病名編集処理
                 PERFORM 3103-BYOMEI-SET-SEC
      *    カルテ病名がある時はカルテ病名
                 IF   PTBYO-CHARTBYOMEI  =  SPACE
                   MOVE    SPACE          TO  WRK-BYOMEI
                   IF   PTBYO-HOSOKU-COMMENT  =  SPACE
                     MOVE    PTBYO-BYOMEI     TO  H01KA4-BYOMEI(IDX)
                                                  H01SA4-BYOMEI(IDX)
                   ELSE
                     STRING  PTBYO-BYOMEI          DELIMITED  BY  SPACE
                             "（"                  DELIMITED  BY  SIZE
                             PTBYO-HOSOKU-COMMENT  DELIMITED  BY  SPACE
                             "）"                  DELIMITED  BY  SIZE
                                                   INTO   WRK-BYOMEI
                     END-STRING
                     MOVE    WRK-BYOMEI   TO  H01KA4-BYOMEI(IDX)
                                              H01SA4-BYOMEI(IDX)
                   END-IF
                 ELSE
                   MOVE    PTBYO-CHARTBYOMEI   TO  H01KA4-BYOMEI(IDX)
                                                   H01SA4-BYOMEI(IDX)
                 END-IF
                 MOVE    WRK-STRYMDW(1:3)  TO  H01KA4-STRYMDY(IDX)
                                               H01SA4-STRYMDY(IDX)
                 MOVE    WRK-STRMMW      TO  H01KA4-STRYMDM(IDX)
                                             H01SA4-STRYMDM(IDX)
                 MOVE    WRK-STRDDW      TO  H01KA4-STRYMDD(IDX)
                                             H01SA4-STRYMDD(IDX)
                 MOVE    WRK-ENDYMDW(1:3)  TO  H01KA4-ENDYMDY(IDX)
                                               H01SA4-ENDYMDY(IDX)
                 MOVE    WRK-ENDMMW      TO  H01KA4-ENDYMDM(IDX)
                                             H01SA4-ENDYMDM(IDX)
                 MOVE    WRK-ENDDDW      TO  H01KA4-ENDYMDD(IDX)
                                               H01SA4-ENDYMDD(IDX)
                 EVALUATE  IDX2
                 WHEN  1
                   MOVE    "治癒"        TO  H01KA4-TENKI(IDX)
                                             H01SA4-TENKI(IDX)
                 WHEN  2
                   MOVE    "死亡"        TO  H01KA4-TENKI(IDX)
                                             H01SA4-TENKI(IDX)
                 WHEN  3
                   MOVE    "中止"        TO  H01KA4-TENKI(IDX)
                                             H01SA4-TENKI(IDX)
                 END-EVALUATE
               END-IF
             END-IF
      *
             MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
             MOVE    "key2"              TO  MCP-PATHNAME
             PERFORM  950-PTBYOMEI-READ-SEC
      *
             IF   IDX  >=  17
      *    続紙無し
               MOVE    1          TO  FLG-ZOKUSHI
             END-IF
           END-PERFORM
      *
           .
       311-HC01-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険適応病名判定処理
      *****************************************************************
       20024-HKNCOMBI-CHK-SEC          SECTION.
      *
           EVALUATE    PARA-HKNCOMBIKBN
           WHEN    "1"
      *        労災・自賠責とその他保険
               IF      WRK-COMB-HKNNUM         =   "971"
                                               OR  "973"
                                               OR  "970"
      *            保険組合せ一致のみ
                   IF      SPA-HKNCOMBINUM     =   PTBYO-HKNCOMBI
                       CONTINUE
                   ELSE
                       MOVE    1                   TO  FLG-INNO
                   END-IF
      *            第三者行為は、公費ＩＤ一致
                   IF      WRK-COMB-HKNNUM     =   "970"
                       MOVE    1                   TO  FLG-INNO
                       IF      PTBYO-KOHID         =   WRK-COMB-KOHID
                           MOVE    ZERO                TO  FLG-INNO
                       END-IF
                   END-IF
               ELSE
      *            労災・自賠責は対象外
                   PERFORM VARYING IDX-H   FROM    1   BY  1
                           UNTIL   (IDX-H          >   20  )  OR
                                   (TBL-HKNCOMBINUM (IDX-H)  =   SPACE)
                       IF      PTBYO-HKNCOMBI      =   TBL-HKNCOMBINUM
                                                               (IDX-H)
                           IF      TBL-HKNNUM (IDX-H)  =   "971"
                                                       OR  "973"
                                                       OR  "970"
                               MOVE    1                   TO  FLG-INNO
                           END-IF
                           MOVE    20                  TO  IDX-H
                       END-IF
                   END-PERFORM
      *            第三者行為は対象外
                   IF      PTBYO-KOHID     NOT =   ZERO
                       MOVE    1                   TO  FLG-INNO
                   END-IF
               END-IF
           WHEN    "2"
      *        保険組合せ一致のみ
               IF      SPA-HKNCOMBINUM     =   PTBYO-HKNCOMBI
                   CONTINUE
               ELSE
                   MOVE    1                   TO  FLG-INNO
               END-IF
      *        第三者行為は、公費ＩＤ一致
               IF      WRK-COMB-HKNNUM     =   "970"
                   MOVE    1                   TO  FLG-INNO
                   IF      PTBYO-KOHID         =   WRK-COMB-KOHID
                       MOVE    ZERO                TO  FLG-INNO
                   END-IF
               END-IF
           END-EVALUATE
           .
       20024-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費変換処理
      *****************************************************************
       3102-PTKOHINF-HEN-SEC        SECTION.
      *
           MOVE    SPACE         TO  WRK-TEKSTYMD
           MOVE    SPACE         TO  WRK-TEKEDYMD
           MOVE    SPACE         TO  WRK-FTNSNO
           MOVE    SPACE         TO  WRK-JKYSNO
      *    保険者情報
      *    全角変換
           MOVE    PTKOH-FTNJANUM      TO  WRK-MAE-INPUT
           MOVE    16                  TO  WRK-MOJISU
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT(1:16) TO  WRK-FTNSNO
      *    受給者番号１
      *    全角変換
           MOVE    PTKOH-JKYSNUM       TO  WRK-MAE-INPUT
           MOVE    14                  TO  WRK-MOJISU
           PERFORM 2009-HENKAN-SEC
           MOVE    SPACE               TO  WRK-JKYSNO
           MOVE    WRK-OUT-INPUT(1:KANACHK-MAX)
                                       TO  WRK-JKYSNO
      *
           IF  COMB-KOH1HKNNUM  =  "970"
      *        第三者行為
               IF  PARA-DAISANKBN  =  "1"
      *          負担者番号へ
                 MOVE    COMB-KOH1-TANSEIDONAME
                                             TO  WRK-FTNSNO
                 MOVE    SPACE               TO  WRK-JKYSNO
                 IF      PTKOH-JKYSNUM   NOT =   SPACE
                     STRING  "（"
                             PTKOH-JKYSNUM
                             "）"            DELIMITED  BY  SPACE
                                         INTO    WRK-JKYSNO
                     END-STRING
                 END-IF
               ELSE
      *          備考へ
                 MOVE    SPACE               TO  WRK-FTNSNO
                 MOVE    SPACE               TO  WRK-JKYSNO
                 MOVE    SPACE               TO  WRK-BIKO
                 IF      PTKOH-JKYSNUM       =   SPACE
                     MOVE    COMB-KOH1-TANSEIDONAME
                                             TO  WRK-BIKO
                 ELSE
                     STRING  COMB-KOH1-TANSEIDONAME
                             "（"
                             PTKOH-JKYSNUM
                             "）"            DELIMITED  BY  SPACE
                                             INTO    WRK-BIKO
                     END-STRING
                 END-IF
                 MOVE    WRK-BIKO     TO  H01KA4-BIKO
                                          H01SA4-BIKO
               END-IF
           END-IF
      *
      *    適用開始年月日
           MOVE    PTKOH-TEKSTYMD      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-TEKSTYMD
      *    適用終了年月日
           IF   PTKOH-TEKEDYMD  =  SPACE  OR  ALL "9"
             MOVE    "平成　　年　　月　　日"   TO  WRK-TEKEDYMD
           ELSE
             MOVE    PTKOH-TEKEDYMD    TO  WRK-SYMD
             PERFORM 31012-SEIWA-HEN-SEC
             MOVE    WRK-HENYMDG       TO  WRK-TEKEDYMD
           END-IF
      *
           .
       3102-PTKOHINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せテーブル編集処理
      *****************************************************************
       2009-HKNCOMBI-HEN-SEC        SECTION.
      *
           INITIALIZE                      TBL-HKNCOMBI-AREA
           MOVE    ZERO                TO  IDX-H
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    SPA-SYSYMD          TO  COMB-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  COMB-TEKEDYMD
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key6"               TO  MCP-PATHNAME
               PERFORM 909-HKNCOMBI-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
           PERFORM UNTIL   (FLG-HKNCOMBI   =   1  )
                       OR  (IDX-H          >=  20 )
               IF      COMB-DELKBN         =   SPACE
                   ADD     1                   TO  IDX-H
                   MOVE    COMB-HKNCOMBINUM    TO  TBL-HKNCOMBINUM
                                                               (IDX-H)
                   MOVE    COMB-HKNNUM         TO  TBL-HKNNUM
                                                               (IDX-H)
      *ver.4.7
      *            第三者
                   IF      COMB-KOH1HKNNUM     =   "970"
                       MOVE    COMB-KOH1HKNNUM TO  TBL-HKNNUM (IDX-H)
                       MOVE    COMB-KOH1ID     TO  TBL-KOHID  (IDX-H)
                   END-IF
               END-IF
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 909-HKNCOMBI-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       2009-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    続紙印刷処理
      *****************************************************************
       2002-ZOKUSHI-SEC          SECTION.
      *
      *    ファイルＯＰＥＮ
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    SPA-HOSPNUM         TO  HC01PARA2-HOSPNUM
           MOVE    "HC01"              TO  HC01PARA-FORM-ID
           MOVE    "H1A4"              TO  HC01PARA2-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
                                           HC01PARA2-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
                                           HC01PARA2-SYOHMS
           ADD     1                   TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC01PARA-CNT
                                           HC01PARA2-CNT
      *
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAMES(1)
           MOVE    HC01PARA2           TO  SGETTEMP-BASENAMES(2)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES(1)   TO  ASGNPARA
           MOVE    SGETTEMP-FULLNAMES(2)   TO  ASGNPARA2
      *
           OPEN    OUTPUT              PRT-FILE
                                       PRT-FILE2
      *
           INITIALIZE  H01KA4
           INITIALIZE  H01SA4
           MOVE    ZERO                TO  IDX
           PERFORM 310-HC01-HEN-SEC
      *
      *    印刷項目編集
           PERFORM  UNTIL  FLG-PTBYOMEI  NOT =  ZERO
      *
             IF   IDX  >=  17
               PERFORM 20021-HC011-PRINT-SEC
               INITIALIZE             H01KA4
               INITIALIZE             H01SA4
               MOVE    ZERO           TO  IDX
               PERFORM  310-HC01-HEN-SEC
             END-IF
      *
             MOVE    ZERO                TO  FLG-INNO
             IF  ( SYS-1017-KARUTE-BYOMEI   =   "1"   )  AND
                 ( PTBYO-TENKIYMD       NOT =   SPACE )
               MOVE    1                   TO  FLG-INNO
             END-IF
      *      入院分は対象外
             IF  ( PTBYO-NYUGAIKBN          =   "1" )
               MOVE    1                   TO  FLG-INNO
             END-IF
      *      保険適応病名判定
             IF  ( FLG-INNO                 =   ZERO  )  AND
                 ( PARA-HKNCOMBIKBN         =   "1"  OR  "2" )
                 PERFORM 20024-HKNCOMBI-CHK-SEC
             END-IF
      *
      *    継続病名のみの時、転帰日のあるものを対象外とする
             IF      FLG-INNO        =   ZERO
               IF     SYS-1017-KARUTE-BYOMEI   =  "2"
                 CONTINUE
               ELSE
      *    病名編集処理
                 PERFORM 3103-BYOMEI-SET-SEC
      *    カルテ病名がある時はカルテ病名
                 IF   PTBYO-CHARTBYOMEI  =  SPACE
                   MOVE    SPACE          TO  WRK-BYOMEI
                   IF   PTBYO-HOSOKU-COMMENT  =  SPACE
                     MOVE    PTBYO-BYOMEI     TO  H01KA4-BYOMEI(IDX)
                                                  H01SA4-BYOMEI(IDX)
                   ELSE
                     STRING  PTBYO-BYOMEI          DELIMITED  BY  SPACE
                             "（"                  DELIMITED  BY  SIZE
                             PTBYO-HOSOKU-COMMENT  DELIMITED  BY  SPACE
                             "）"                  DELIMITED  BY  SIZE
                                                   INTO   WRK-BYOMEI
                     END-STRING
                     MOVE    WRK-BYOMEI  TO  H01KA4-BYOMEI(IDX)
                                             H01SA4-BYOMEI(IDX)
                   END-IF
                 ELSE
                   MOVE    PTBYO-CHARTBYOMEI   TO  H01KA4-BYOMEI(IDX)
                                                   H01SA4-BYOMEI(IDX)
                 END-IF
                 MOVE    WRK-STRYMDW(1:3)  TO  H01KA4-STRYMDY(IDX)
                                               H01SA4-STRYMDY(IDX)
                 MOVE    WRK-STRMMW      TO  H01KA4-STRYMDM(IDX)
                                             H01SA4-STRYMDM(IDX)
                 MOVE    WRK-STRDDW      TO  H01KA4-STRYMDD(IDX)
                                             H01SA4-STRYMDD(IDX)
                 MOVE    WRK-ENDYMDW(1:3)  TO  H01KA4-ENDYMDY(IDX)
                                               H01SA4-ENDYMDY(IDX)
                 MOVE    WRK-ENDMMW      TO  H01KA4-ENDYMDM(IDX)
                                             H01SA4-ENDYMDM(IDX)
                 MOVE    WRK-ENDDDW      TO  H01KA4-ENDYMDD(IDX)
                                             H01SA4-ENDYMDD(IDX)
      *
                 EVALUATE IDX2
                 WHEN    1
                   MOVE    "治癒"        TO  H01KA4-TENKI(IDX)
                                             H01SA4-TENKI(IDX)
                 WHEN    2
                   MOVE    "死亡"        TO  H01KA4-TENKI(IDX)
                                             H01SA4-TENKI(IDX)
                 WHEN    3
                   MOVE    "中止"        TO  H01KA4-TENKI(IDX)
                                             H01SA4-TENKI(IDX)
                 END-EVALUATE
               END-IF
             END-IF
      *
             MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
             MOVE    "key2"              TO  MCP-PATHNAME
             PERFORM  950-PTBYOMEI-READ-SEC
           END-PERFORM
      *
           IF   IDX  >  ZERO
             PERFORM 20021-HC011-PRINT-SEC
           END-IF
      *
           .
       2002-ZOKUSHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    続紙印刷　処理
      *****************************************************************
       20021-HC011-PRINT-SEC                SECTION.
      *
      *    印刷用データ出力
            WRITE  H01KA4
            WRITE  H01SA4
      *
            CLOSE                       PRT-FILE
                                        PRT-FILE2
      *
           IF ORCHC01-SYORIKBN   NOT = "3"
      *
      *     国保・健保印刷判別
             EVALUATE   TRUE
             WHEN  PTHKN-HKNNUM   =  "060"  OR  "067"  OR  "039"
                                 OR  "040"  OR  "068"  OR  "069"
               INITIALIZE                  ORCSMKPRTAREA
               MOVE    "HC01.sh"           TO  MKPRT-ID
               MOVE    "A21000H01KA4.red"  TO  MKPRT-DIA
               IF  PARA-FORMKBN  =  "1"
                 MOVE    "A21000H01KA4V02.red"  TO  MKPRT-DIA
               END-IF
               MOVE    SPACE               TO  MKPRT-DEF
               MOVE    ASGNPARA            TO  MKPRT-DAT
               MOVE    SYS-1031-PRTNM(1)   TO  MKPRT-PRTNM
               CALL    "ORCSMKPRTSITE"     USING
                                           ORCSMKPRTAREA
                                           SPA-AREA
             WHEN   OTHER
               INITIALIZE                 ORCSMKPRTAREA
               MOVE   "HC01.sh"           TO  MKPRT-ID
               MOVE   "A21000H01SA4.red"  TO  MKPRT-DIA
               IF  PARA-FORMKBN  =  "1"
                 MOVE   "A21000H01SA4V02.red"  TO  MKPRT-DIA
               END-IF
               MOVE   SPACE                       TO  MKPRT-DEF
               MOVE   ASGNPARA2            TO  MKPRT-DAT
               MOVE   SYS-1031-PRTNM(1)   TO  MKPRT-PRTNM
               CALL   "ORCSMKPRTSITE"      USING
                                           ORCSMKPRTAREA
                                           SPA-AREA
             END-EVALUATE
      *
      *    一括印刷
           ELSE
      *
      *    国保・健保印刷判別
             EVALUATE  TRUE
               WHEN  PTHKN-HKNNUM   =  "060"  OR  "067"  OR  "039"
                                   OR  "040"  OR  "068"  OR  "069"
      *
                 MOVE    ORCHC01-PRTAREA     TO  ORCSPRTAREA
      *
                 MOVE    H01KA4              TO  SPRT-PRTDATA
                 MOVE    "2"                 TO  SPRT-SITEKBN
                 MOVE    "A21000H01KA4.red"  TO  SPRT-PRTID
                 IF  PARA-FORMKBN  =  "1"
                   MOVE    "A21000H01KA4V02.red"  TO  SPRT-PRTID
                 END-IF
                 MOVE    "診療録（カルテ１号紙）（外来）一括"
                                             TO  SPRT-TITLE
                 CALL    "ORCSPRT"           USING
                                             ORCSPRTAREA
                                             SPA-AREA
                 IF   SPRT-RETURN  =  ZERO
                   ADD     1                 TO  ORCHC01-CNT
                 ELSE
                   MOVE    9                 TO  ORCHC01-RC
                 END-IF
               WHEN   OTHER
      *
               MOVE    ORCHC01-PRTAREA       TO  ORCSPRTAREA
      *
               MOVE    H01SA4                TO  SPRT-PRTDATA
               MOVE    "2"                   TO  SPRT-SITEKBN
               MOVE    "A21000H01SA4.red"    TO  SPRT-PRTID
               IF  PARA-FORMKBN  =  "1"
                 MOVE    "A21000H01SA4V02.red"  TO  SPRT-PRTID
               END-IF
               MOVE    "診療録（カルテ１号紙）（外来）一括"
                                             TO  SPRT-TITLE
               CALL    "ORCSPRT"           USING
                                           ORCSPRTAREA
                                           SPA-AREA
               IF   SPRT-RETURN  =  ZERO
                 ADD     1             TO  ORCHC01-CNT
               ELSE
                 MOVE    9             TO  ORCHC01-RC
               END-IF
             END-EVALUATE
           END-IF
      *
      *    ファイルＯＰＥＮ
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    SPA-HOSPNUM         TO  HC01PARA2-HOSPNUM
           MOVE    "HC01"              TO  HC01PARA-FORM-ID
           MOVE    "H1A4"              TO  HC01PARA2-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
                                           HC01PARA2-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
                                           HC01PARA2-SYOHMS
           ADD     1                   TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC01PARA-CNT
                                           HC01PARA2-CNT
      *
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAMES(1)
           MOVE    HC01PARA2           TO  SGETTEMP-BASENAMES(2)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES(1)   TO  ASGNPARA
           MOVE    SGETTEMP-FULLNAMES(2)   TO  ASGNPARA2
      *
           OPEN    OUTPUT              PRT-FILE
                                       PRT-FILE2
      *
           .
       20021-HC011-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *        病名編集処理
      *****************************************************************
       3103-BYOMEI-SET-SEC        SECTION.
      *
           ADD     1                   TO  IDX
      *    開始年月日（診療年月日）
           MOVE    PTBYO-SRYYMD        TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMD          TO  WRK-STRYMDW
      *
      *    終了年月日（転帰年月日）
           MOVE    SPACE               TO  WRK-ENDYMD
           MOVE    SPACE               TO  WRK-ENDYMDW
           IF  PTBYO-TENKIYMD  NOT =   SPACE
             MOVE    PTBYO-TENKIYMD    TO  WRK-SYMD
             PERFORM 31012-SEIWA-HEN-SEC
             MOVE    WRK-HENYMD        TO  WRK-ENDYMDW
           END-IF
      *
           EVALUATE  PTBYO-TENKIKBN
             WHEN  "1"
               MOVE    1               TO  IDX2
             WHEN  "2"
               MOVE    2               TO  IDX2
             WHEN  "3"
               MOVE    3               TO  IDX2
             WHEN  OTHER
               MOVE    ZERO            TO  IDX2
           END-EVALUATE
      *
           .
       3103-BYOMEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       2009-HENKAN-SEC                 SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF   KANACHK-RC  =  ZERO
             MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                       TO  WRK-OUT-INPUT
           ELSE
             MOVE    WRK-MAE-INPUT     TO  WRK-OUT-INPUT
           END-IF
      *
           .
       2009-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_ptinf"       TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
             ELSE
               MOVE    1               TO  FLG-PTINF
             END-IF
           ELSE
             MOVE    1                 TO  FLG-PTINF
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_hkncombi"    TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
             ELSE
               MOVE    1               TO  FLG-HKNCOMBI
             END-IF
           ELSE
             MOVE    1                 TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       909-HKNCOMBI-NEXT-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           .
       909-HKNCOMBI-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       920-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_pthkninf"    TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
             ELSE
               MOVE    1               TO  FLG-PTHKNINF
             END-IF
           ELSE
             MOVE    1                 TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       920-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスター読み込み
      *****************************************************************
       930-HKNNUM-READ-SEC         SECTION.
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_hknnum"      TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
             ELSE
               MOVE    1               TO  FLG-HKNNUM
             END-IF
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       930-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費マスター読み込み
      *****************************************************************
       940-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_ptkohinf"    TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
             ELSE
               MOVE    1               TO  FLG-PTKOHINF
             END-IF
           ELSE
             MOVE    1                 TO  FLG-PTKOHINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       940-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスター読み込み
      *****************************************************************
       950-PTBYOMEI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    MCPDATA-REC       TO  PTBYOMEI-REC
             MOVE    ZERO              TO  FLG-PTBYOMEI
           ELSE
             MOVE    1                 TO  FLG-PTBYOMEI
           END-IF
      *
           .
       950-PTBYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =   ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_syskanri"    TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
             ELSE
               MOVE    1               TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1                 TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号マスタ次検索処理
      *****************************************************************
       900-HKNJAINF-READ-SEC              SECTION.
      *
           MOVE    "DBFETCH"         TO  MCP-FUNC
           CALL    "ORCDBMAIN"       USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    MCPDATA-REC     TO  HKNJAINF-REC
             MOVE    ZERO            TO  FLG-HKNJAINF
           ELSE
             INITIALIZE              HKNJAINF-REC
             MOVE    1               TO  FLG-HKNJAINF
           END-IF
           .
       900-HKNJAINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           MOVE    "DBFETCH"         TO  MCP-FUNC
           CALL    "ORCDBMAIN"       USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    MCPDATA-REC     TO  PTRSIINF-REC
             MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
             INITIALIZE                  PTRSIINF-REC
             MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           .
       993-PTRSIINF-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.
