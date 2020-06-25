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
       PROGRAM-ID.             ORCGP024.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 所得者情報（Ｐ０２４）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-太田      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-太田     01/05/10  日付変換後ゼロ表示変更
      *  01.00.02    MCC-多々納   01/06/13  登録後、診療行為遷移を追加
      *  01.00.04    MCC-太田     01/05/10  日付変換後ゼロ表示変更
      *  01.00.05    MCC-多々納   01/07/02  日付入力チェックサブ追加
      *  02.00.00    NACL-多々納  02/07/30  インターフェース変更
      *  02.00.01    NACL-多々納  03/02/13  食事標準負担減額日追加
      *  02.00.02    NACL-多々納  03/03/24  公費負担額追加
      *  02.00.03    NACL-多々納  03/10/14  公費負担額初期編集追加
      *  02.00.04    NACL-多々納  05/02/10  特定の受給者番号表示追加
      *  02.00.05    NACL-多々納  05/11/11  MONFUNC 対応 他
      *  02.00.06    NACL-多々納  06/03/10  精神通院、名称変更対応
      *  03.02.00    NACL-多々納  06/09/07  老齢福祉年金受給証追加
      *  03.05.00    NACL-多々納  07/05/XX  グループ診療対応
      *  03.05.01    NACL-多々納  07/07/20  認定情報追加対応
      *  04.05.00    NACL-多々納  09/07/XX  患者公費負担額機能追加
      *                                     公費負担額未入力警告対応
      *  04.07.00    NACL-多々納  11/11/07  地方公費負担額変更対応
      *  04.07.00    NACL-多々納  14/06/04  公費負担額上限件数対応
      *  04.07.00    NACL-多々納  14/12/XX  難病等対応
      *  04.08.00    NACL-多々納  16/05/31  原爆一般対応
      *  04.08.00    NACL-多々納  16/11/15  HAORI他対応
      *  04.08.00    NACL-多々納  17/05/XX  境界層該当対応
      *  05.00.00    NACL-多々納  18/11/XX  092 肝がん治療対応
      ****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *    患者画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PARA            PIC 9(01).
           03  FLG-END             PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-IN-TNKSELNUM    PIC 9(01).
           03  FLG-IN-TSYSELNUM    PIC 9(01).
      *
           03  FLG-PTKOHFTNETC     PIC 9(01).
           03  FLG-ENDYMD          PIC 9(01).
           03  FLG-CHKNO           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
     *
           03  IDP-1               PIC 9(04).
           03  IDP-2               PIC 9(04).
      *
           03  IDF                 PIC 9(04).
           03  IDX-F               PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-RENNUMZ         PIC ZZZZ.
           03  WRK-NAIBU-IDX-AREA.
               05  WRK-G-IDX       PIC 9(04).
               05  WRK-N-IDX       PIC 9(04).
      *        1:所得テーブル /2:年金テーブル
               05  WRK-W-TBL       PIC 9(01).
           03  WRK-TSY-SMAX        PIC 9(04).
           03  WRK-TNK-SMAX        PIC 9(04).
           03  WRK-TSY-MAX         PIC 9(04).
           03  WRK-TNK-MAX         PIC 9(04).
      *----(01.00.01) LINE ADD START --------------------------------
           03  WRK-WWYMD           PIC X(10).
      *----(01.00.01) LINE ADD END   --------------------------------
           03  WRK-SELNUM          PIC 9(04).
      *    入力項目名
           03  WRK-MCP-WIDGET          PIC X(64).
      *
           03  WRK-Z7                  PIC Z,ZZZ,ZZZ.
           03  WRK-Z8                  PIC ZZ,ZZZ,ZZZ.
      *
           03  WRK-IDX                 PIC 9(04).
           03  WRK-KOHID               PIC 9(10).
           03  WRK-PARA-EDANUM         PIC 9(03).
           03  WRK-FTN1-IDX            PIC 9(04).
      *
           03  WRK-FILEMEI             PIC X(20).
      *
      *    認定範囲区分編集処理
           03  WRK-HKNTEKKBN-MEI       PIC X(12).
           03  WRK-HKNTEKKBN           PIC X(01).
           03  IDX-TEK                 PIC 9(04).
      *    他一部負担入力画面へ
           03  WRK-TGAIJGNGAK-G.
               05  WRK-TGAIJGNGAK      PIC 9(07)   OCCURS   10.
           03  WRK-TNYUJGNGAK          PIC 9(07).
           03  IDK                     PIC 9(03).
           03  IDK2                    PIC 9(03).
           03  WRK-MSG01               PIC X(50).
           03  WRK-MSG                 PIC X(50).
           03  WRK-HKINGAK             PIC X(09).
      *
           03  WRK-YMD.
               05  WRK-YY              PIC 9(04).
               05  WRK-MM              PIC 9(02).
               05  WRK-DD              PIC 9(02).
           03  WRK-FTNSTYMD.
               05  WRK-FTNSTYY         PIC 9(04).
               05  WRK-FTNSTMM         PIC 9(02).
               05  WRK-FTNSTDD         PIC 9(02).
           03  WRK-FTNEDYMD.
               05  WRK-FTNEDYY         PIC 9(04).
               05  WRK-FTNEDMM         PIC 9(02).
               05  WRK-FTNEDDD         PIC 9(02).
      *H29.6
           03  WRK-KYOUKAI             PIC X(01).
           03  WRK-KYOUKAI-MEI         PIC X(20).
      *
      *    認定範囲一覧表示
       01  TBL-HKNTEKKBN-AREA.
           03  FILLER              PIC X(14)   VALUE   "0 全対象".
           03  FILLER              PIC X(14)   VALUE   "1 地対象外".
           03  FILLER              PIC X(14)   VALUE   "2 地のみ対象".
           03  FILLER              PIC X(14)   VALUE   "3 食のみ対象".
           03  FILLER              PIC X(14)   VALUE   "4 地＋食対象".
       01  TBL-HKNTEKKBN-AREAR     REDEFINES   TBL-HKNTEKKBN-AREA.
           03  TBL-HKNTEKKBN-G     PIC X(14)   OCCURS   5.
      *
       01  WRK-HENSYU-AREA.
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
      *公費件数
       01  TBL-MAX-KOH             PIC 9(03)   VALUE   60.
      *
      *公費一部負担件数
       01  MAX-KOHFTN              PIC 9(03)   VALUE   300.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
       01  HZN-PARA-REC.
           COPY    "CPPARA.INC"
                              REPLACING  //PARA-// BY //HZN-PARA-//.
      *
      *   低所得者履歴
      *
       01  LNK-TSY-AREA.
           02  LNK-TSY-REC           OCCURS  20.
           COPY    "CPTSYRRK.INC"
                            REPLACING  //TSYRRK-// BY //LNK-TSY-//.
      *
      *   年金履歴
       01  LNK-TNK-AREA.
           02  LNK-TNK-REC           OCCURS  20.
           COPY    "CPTNKRRK.INC"
                              REPLACING  //TNKRRK-// BY //LNK-TNK-//.
      *
      *    公費負担保存
       01  LNK-PTKOHFTN-AREA.
           02  LNK-PTKOHFTN-REC           OCCURS  200.
           COPY    "CPPTKOHFTN.INC"
                       REPLACING  //PTKOHFTN-// BY //LNK-PTKOHFTN-//.
      *    公費負担
       01  PTKOHFTN-REC.
           COPY    "CPPTKOHFTN.INC".
      *
      *    公費負担更新保存
       01  HZN-PTKOHFTN-AREA.
           03  HZN-PTKOHFTN-OCC           OCCURS  60.
               05  HZN-PTKOHFTN-OCC2          OCCURS  200.
                   07  HZN-PTKOHFTN-REC       PIC X(200).
      *    患者公費負担
       01  PTKOHFTNETC-REC.
           COPY    "CPPTKOHFTNETC.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   数字変換領域
           COPY    "CPORCSNUM.INC".
      *----(01.00.04) LINE ADD START ----------------------------------
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *----(01.00.04) LINE ADD END   ----------------------------------
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
      *
       01  SCRAREA.
           COPY    "ORCA12SCRAREA.INC".
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
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  SPA-FLG-END
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
           IF      SPA-FLG-END         =   ZERO
               PERFORM 5001-MAPCUR-SEC
      *********PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
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
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
      *    初期画面編集
           PERFORM 300-SCREEN-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    パラメタファイル検索処理
           PERFORM 320-PARAFILE-KENSAKU-SEC
      *
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
      *    初期画面編集
           PERFORM 310-SPASET-SEC
      *
           MOVE    1       TO  SPA-GMN24-CUR
      *
           .
       3OO-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタファイル検索処理
      *****************************************************************
       320-PARAFILE-KENSAKU-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-TNK-MAX
           MOVE    ZERO                TO  SPA-TSY-MAX
           INITIALIZE                  LNK-TSY-AREA
           INITIALIZE                  LNK-TNK-AREA
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "TNKRRK"            TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           PERFORM
               UNTIL   FLG-PARA            =   1
               IF      PARA-EDANUM         <=  20
                   MOVE    PARA-DATA-REC   TO  LNK-TNK-REC
                                                         (PARA-EDANUM)
                   MOVE    PARA-EDANUM     TO  SPA-TNK-MAX
               END-IF
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "TSYRRK"            TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           PERFORM
               UNTIL   FLG-PARA            =   1
               IF      PARA-EDANUM         <=  20
                   MOVE    PARA-DATA-REC   TO  LNK-TSY-REC
                                                         (PARA-EDANUM)
                   MOVE    PARA-EDANUM     TO  SPA-TSY-MAX
               END-IF
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       320-PARAFILE-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
      *    低所得者編集
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   20 )
                   OR        ( IDX     >   SPA-TSY-MAX )
      *
               MOVE    LNK-TSY-NINSTYMD (IDX)
                                         TO  SPA-GMN24-TSYDPSTYMD (IDX)
               MOVE    LNK-TSY-NINSTYMD (IDX)  TO  WRK-SYMD
               PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
               MOVE    WRK-HENYMD        TO  SPA-GMN24-TSYDPSTYMDW(IDX)
      *
               MOVE    LNK-TSY-NINEDYMD (IDX)
                                         TO  SPA-GMN24-TSYDPEDYMD (IDX)
               MOVE    LNK-TSY-NINEDYMD (IDX)  TO  WRK-SYMD
               IF  WRK-SYMD              =   ALL "9"
                   MOVE    ALL "9"  TO  SPA-GMN24-TSYDPEDYMDW(IDX)(1:7)
               ELSE
                   PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
                   MOVE    WRK-HENYMD    TO  SPA-GMN24-TSYDPEDYMDW(IDX)
               END-IF
      *
               MOVE    LNK-TSY-SKJGNSTYMD (IDX)
                                         TO  SPA-GMN24-TSYLSKJGNSTYMD
                                                              (IDX)
               MOVE    LNK-TSY-SKJGNSTYMD (IDX)  TO  WRK-SYMD
               IF      WRK-SYMD            =   SPACE
                   MOVE    SPACE       TO  SPA-GMN24-TSYLSKJGNSTYMDW
                                                           (IDX)(1:7)
               ELSE
                   PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
                   MOVE    WRK-HENYMD    TO  SPA-GMN24-TSYLSKJGNSTYMDW
                                                                (IDX)
               END-IF
      *        標準負担額減額開始日
               MOVE    LNK-TSY-SKJNINSTYMD (IDX)
                                          TO  SPA-NAI24-TSYLNINYMD
                                                                  (IDX)
               MOVE    LNK-TSY-SKJNINSTYMD (IDX) TO  WRK-SYMD
               IF      WRK-SYMD            =   SPACE
                   MOVE    SPACE           TO  SPA-GMN24-TSYLNINYMD
                                                           (IDX)(1:7)
               ELSE
                   PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
                   MOVE    WRK-HENYMD      TO  SPA-GMN24-TSYLNINYMD
                                                                (IDX)
               END-IF
      *        認定範囲区分
               IF      LNK-TSY-HKNTEKKBN (IDX) =   SPACE
                   MOVE    "0"             TO  LNK-TSY-HKNTEKKBN (IDX)
               END-IF
               MOVE    LNK-TSY-HKNTEKKBN (IDX) TO  SPA-NAI24-TSYLHKNKBN
                                                                (IDX)
               MOVE    SPA-NAI24-TSYLHKNKBN(IDX)   TO  WRK-HKNTEKKBN
               PERFORM 41002-HKNTEKKBN-HEN-SEC
               MOVE    WRK-HKNTEKKBN-MEI       TO  SPA-GMN24-TSYLHKNKBN
                                                                (IDX)
      *H29.6
      *        境界層該当区分
               IF      LNK-TSY-KYOUKAISOUKBN (IDX) =   SPACE
                   MOVE    "0"         TO  LNK-TSY-KYOUKAISOUKBN (IDX)
               END-IF
               MOVE    LNK-TSY-KYOUKAISOUKBN (IDX)
                                       TO  SPA-NAI24-TSYLKYOUKAI(IDX)
               MOVE    LNK-TSY-KYOUKAISOUKBN (IDX) TO  WRK-KYOUKAI
               PERFORM 41003-KYOUKAI-HEN-SEC
               MOVE    WRK-KYOUKAI-MEI TO  SPA-GMN24-TSYLKYOUKAI(IDX)
           END-PERFORM
      *
      *    年金編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   20 )
                   OR        ( IDX     >   SPA-TNK-MAX )
      *
               MOVE    LNK-TNK-STYMD (IDX)
                                       TO  SPA-GMN24-TNKDPSTYMD (IDX)
               MOVE    LNK-TNK-STYMD (IDX) TO  WRK-SYMD
               PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
               MOVE    WRK-HENYMD      TO  SPA-GMN24-TNKDPSTYMDW(IDX)
      *
               MOVE    LNK-TNK-EDYMD (IDX)
                                       TO  SPA-GMN24-TNKDPEDYMD (IDX)
               MOVE    LNK-TNK-EDYMD (IDX) TO  WRK-SYMD
               IF  WRK-SYMD            =    ALL "9"
                   MOVE    ALL "9" TO  SPA-GMN24-TNKDPEDYMDW(IDX)(1:7)
               ELSE
                   PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
                   MOVE    WRK-HENYMD  TO  SPA-GMN24-TNKDPEDYMDW(IDX)
               END-IF
      *        高齢者福祉
               MOVE    LNK-TNK-RRI-FUKUSHI-KBN(IDX)
                                       TO  SPA-GMN24-TFUKUSHI-KBN(IDX)
               PERFORM VARYING     IDF     FROM    1   BY  1
                       UNTIL       IDF     >   SPA-GMN24-FUKUSHI-MAX
                   IF      SPA-GMN24-TFUKUSHI-KBN(IDX)
                                        =   SPA-GMN24-FUKUSHI-LST(IDF)
                                                              (1:1)
                       MOVE    SPA-GMN24-FUKUSHI-LST(IDF)(3:)
                                        TO  SPA-GMN24-TFUKUSHI-MEI(IDX)
                   END-IF
               END-PERFORM
      *        標準負担額減額開始日
               MOVE    LNK-TNK-SKJNINSTYMD (IDX)
                                          TO  SPA-NAI24-TNKLNINYMD
                                                                  (IDX)
               MOVE    LNK-TNK-SKJNINSTYMD (IDX) TO  WRK-SYMD
               IF      WRK-SYMD            =   SPACE
                   MOVE    SPACE           TO  SPA-GMN24-TNKLNINYMD
                                                           (IDX)(1:7)
               ELSE
                   PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
                   MOVE    WRK-HENYMD      TO  SPA-GMN24-TNKLNINYMD
                                                                (IDX)
               END-IF
      *        認定範囲区分
               IF      LNK-TNK-HKNTEKKBN (IDX) =   SPACE
                   MOVE    "0"             TO  LNK-TNK-HKNTEKKBN (IDX)
               END-IF
               MOVE    LNK-TNK-HKNTEKKBN (IDX) TO  SPA-NAI24-TNKLHKNKBN
                                                                (IDX)
               MOVE    SPA-NAI24-TNKLHKNKBN(IDX)   TO  WRK-HKNTEKKBN
               PERFORM 41002-HKNTEKKBN-HEN-SEC
               MOVE    WRK-HKNTEKKBN-MEI       TO  SPA-GMN24-TNKLHKNKBN
                                                                (IDX)
      *H29.6
      *        境界層該当区分
               IF      LNK-TNK-KYOUKAISOUKBN (IDX) =   SPACE
                   MOVE    "0"         TO  LNK-TNK-KYOUKAISOUKBN (IDX)
               END-IF
               MOVE    LNK-TNK-KYOUKAISOUKBN (IDX)
                                       TO  SPA-NAI24-TNKLKYOUKAI(IDX)
               MOVE    LNK-TNK-KYOUKAISOUKBN (IDX) TO  WRK-KYOUKAI
               PERFORM 41003-KYOUKAI-HEN-SEC
               MOVE    WRK-KYOUKAI-MEI TO  SPA-GMN24-TNKLKYOUKAI(IDX)
           END-PERFORM
      *
      *    前回の選択番号は反映させない
           MOVE    ZERO                TO  SPA-MAE24-FTNSELNUM
           INITIALIZE                  SPA-GMN24-FTN-AREA
           INITIALIZE                  SPA-GMN24-FTN-AREA2
      *
      *    公費負担の最初を選択とする
           IF      (SPA-GMN24-FTNSELNUM    =   ZERO )  AND
                   (SPA-MAE24-FTNSELNUM    =   ZERO )  AND
                   (SPA-FTN1-MAX           >   ZERO )
      *
               PERFORM 3102-KOHFTN-HEN-SEC
           END-IF
           IF      (SPA-GMN24-FTNSELNUM    =   ZERO )  AND
                   (SPA-MAE24-FTNSELNUM    =   ZERO )
               INITIALIZE                  SPA-GMN24-KOHFTNTBL2
               MOVE    1                   TO  SPA-NAI24-INIT2
           END-IF
      *    見出し変更
      *    PERFORM 3101-KOH-MEISAI-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    認定範囲区分編集処理
      *****************************************************************
       3101-KOH-MEISAI-SEC              SECTION.
      *
      *    特定の受給者番号編集
           MOVE    SPACE               TO  SPA-GMN24-JKYSNUM
      *H28.11
           MOVE    ZERO                TO  SPA-NAI24-FTNSYORI
      *
           IF      SPA-GMN24-FTNSELNUM >   ZERO
               IF      (SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
                                           =   "051" 
     *****                                 OR  "052"
                                           OR  "015"
                                           OR  "016"
                                           OR  "091"
                                           OR  "021"
      *                   肝炎治療　追加
                                           OR  "038"
      *                   生活保護　追加
                                           OR  "012"    )
      *               北海道の地方公費
                 OR   ((SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
                                           =   "183" ) AND
                       (SPA-PREFNUM        =    01   )) 
      *H26.12
      *            小児特定　Ｈ２６．１２まで
      *          OR   ((SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
      *                                    =   "052" ) AND
      *                (SPA-NAI24-TKOHSTYMD (SPA-NAI24-FTNSELNUM)
      *                                    <=  "20141231" ))
      *R02.4       小児特定　Ｈ２６．１２までを廃止
                 OR   ( SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
                                           =   "052"     )
      *H27.1         難病
                 OR   ( SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
                                           =   "054"     )
      *H30.11        肝がん治療
                 OR   ( SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
                                           =   "092"     )
      *
                   MOVE    "受給者番号："      TO  SPA-GMN24-JKYSNUM
                   MOVE    SPA-NAI24-TJKYSNUM (SPA-NAI24-FTNSELNUM)
                                               TO  SPA-GMN24-JKYSNUM
                                                          (13:)
               END-IF
           END-IF
      *    見出し名称
           IF     (SPA-NAI24-FTNSELNUM >   ZERO   ) 
               IF     (SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
                                           =   "021"
                                           OR  "015"
      ******************************       OR  "281"
                                           OR  "016"     )  AND
                      (SPA-NAI24-TKOHSTYMD (SPA-NAI24-FTNSELNUM)
                                           >=   "20060401"  )
                   MOVE    "入外上限額"         TO  SPA-GMN24-FTNMSG1
                   MOVE    "他一部負担累計"     TO  SPA-GMN24-FTNMSG2
      *H28.11      他一部負担有
                   MOVE    1                    TO  SPA-NAI24-FTNSYORI
               ELSE
                   MOVE    "外来上限額"         TO  SPA-GMN24-FTNMSG1
                   MOVE    "入院上限額"         TO  SPA-GMN24-FTNMSG2
               END-IF
      *
               IF     (SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
                                           =   "960"   )  AND
                      (SPA-NAI24-TKOHSTYMD (SPA-NAI24-FTNSELNUM)
                                           >=   "20061001"  )
                   MOVE    "外来減額（％）"     TO  SPA-GMN24-FTNMSG1
                   MOVE    "入院減額（％）"     TO  SPA-GMN24-FTNMSG2
      *H28.11      ％減額
                   MOVE    2                    TO  SPA-NAI24-FTNSYORI
               END-IF
               IF     (SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
                                           =   "961"   )  AND
                      (SPA-NAI24-TKOHSTYMD (SPA-NAI24-FTNSELNUM)
                                           >=   "20061001"  )
                   MOVE    "外来減額（円）"     TO  SPA-GMN24-FTNMSG1
                   MOVE    "入院減額（円）"     TO  SPA-GMN24-FTNMSG2
               END-IF
      *
               IF     (SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
                                           =   "079"
                                           OR  "024"   )  AND
                      (SPA-NAI24-TKOHSTYMD (SPA-NAI24-FTNSELNUM)
                                           >=   "20061001"  )
                   MOVE    "医療部分上限額"     TO  SPA-GMN24-FTNMSG1
                   MOVE    "食事療養上限額"     TO  SPA-GMN24-FTNMSG2
               END-IF
      *        肝炎治療名称
               IF     (SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
                                           =   "038"   )  AND
                      (SPA-NAI24-TKOHSTYMD (SPA-NAI24-FTNSELNUM)
                                           >=   "20061001"  )
                   MOVE    "入外上限額"         TO  SPA-GMN24-FTNMSG1
                   MOVE    "他一部負担累計"     TO  SPA-GMN24-FTNMSG2
      *H28.11      他一部負担有
                   MOVE    1                    TO  SPA-NAI24-FTNSYORI
               END-IF
      *H30.11
      *        肝がん治療名称
               IF     (SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
                                           =   "092"   )  AND
                      (SPA-NAI24-TKOHSTYMD (SPA-NAI24-FTNSELNUM)
                                           >=   "20181201"  )
                   MOVE    "外来上限額"         TO  SPA-GMN24-FTNMSG1
                   MOVE    "入院上限額"         TO  SPA-GMN24-FTNMSG2
      *??          MOVE    1                    TO  SPA-NAI24-FTNSYORI
               END-IF
      *
      *H26.12
      *        難病・小児特定(H27.1から）名称
               IF    ((SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
                                           =   "052"
                                           OR  "054"       )  AND
                      (SPA-NAI24-TKOHSTYMD (SPA-NAI24-FTNSELNUM)
                                           >=   "20150101"  ))
                   MOVE    "入外上限額"         TO  SPA-GMN24-FTNMSG1
                   MOVE    "他一部負担累計"     TO  SPA-GMN24-FTNMSG2
      *H28.11      他一部負担有
                   MOVE    1                    TO  SPA-NAI24-FTNSYORI
               END-IF
      *H28.6
      *        原爆一般
               IF     (SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
                                           =   "019"   )  AND
                      (SPA-NAI24-TKOHSTYMD (SPA-NAI24-FTNSELNUM)
                                           >=   "20160401"  )
                   MOVE    "外来負担（％）"     TO  SPA-GMN24-FTNMSG1
                   MOVE    "入院負担（％）"     TO  SPA-GMN24-FTNMSG2
      *H28.11      ％減額
                   MOVE    2                    TO  SPA-NAI24-FTNSYORI
               END-IF
      *ver.4.7
      *        地方公費の設定
               EVALUATE    SPA-NAI24-TJYOGENKBN (SPA-NAI24-FTNSELNUM)
               WHEN    "1"
                   MOVE    "外来上限額"         TO  SPA-GMN24-FTNMSG1
                   MOVE    "入院上限額"         TO  SPA-GMN24-FTNMSG2
               WHEN    "2"
                   MOVE    "入外上限額"         TO  SPA-GMN24-FTNMSG1
                   MOVE    "他一部負担累計"     TO  SPA-GMN24-FTNMSG2
      *H28.11      他一部負担有
                   MOVE    1                    TO  SPA-NAI24-FTNSYORI
               END-EVALUATE
           END-IF
      *
           .
       3101-KOH-MEISAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    認定範囲区分編集処理
      *****************************************************************
       41002-HKNTEKKBN-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNTEKKBN-MEI
           PERFORM VARYING     IDX-TEK     FROM    1   BY  1
                   UNTIL       IDX-TEK     >   5
               IF      WRK-HKNTEKKBN       =   TBL-HKNTEKKBN-G(IDX-TEK)
                                                              (1:1)
                   MOVE    TBL-HKNTEKKBN-G (IDX-TEK)
                                           TO  WRK-HKNTEKKBN-MEI
               END-IF
           END-PERFORM
           .
       41002-HKNTEKKBN-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    境界層該当区分編集処理
      *****************************************************************
       41003-KYOUKAI-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-KYOUKAI-MEI
           EVALUATE    WRK-KYOUKAI
               WHEN    "0"
                   MOVE    "境界層非該当"  TO  WRK-KYOUKAI-MEI
               WHEN    "1"
                   MOVE    "境界層該当"    TO  WRK-KYOUKAI-MEI
           END-EVALUATE
           .
       41003-KYOUKAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担初期編集処理
      *****************************************************************
       3102-KOHFTN-HEN-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-FTN1-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL      IDX     >   SPA-FTN1-MAX
               IF      SPA-NAI24-FTNDELKBN (IDX)   =   SPACE
                   MOVE    IDX             TO  WRK-FTN1-IDX
                   MOVE    IDX             TO  SPA-NAI24-FTNSELNUM
                   MOVE    SPA-FTN1-MAX    TO  IDX
               END-IF
           END-PERFORM
           IF      WRK-FTN1-IDX        =   ZERO
               GO      TO                  3102-KOHFTN-HEN-EXT
           END-IF
           MOVE    1                   TO  SPA-GMN24-FTNSELNUM
           MOVE    SPA-GMN24-FTNSELNUM TO  SPA-MAE24-FTNSELNUM
      *
           INITIALIZE                  SPA-GMN24-KOHFTNTBL2
           MOVE    1                   TO  SPA-NAI24-INIT2
      *    見出し変更
           PERFORM 3101-KOH-MEISAI-SEC
      *    対象のデータをパラメタファイルより編集する
           PERFORM 31022-KOHFTN-SPAHENSYU-SEC
      *
           IF      SPA-FTN2-MAX        =   ZERO
               MOVE    ZERO                TO  SPA-GMN24-FTNSELNUM
               MOVE    ZERO                TO  SPA-NAI24-FTNSELNUM
               MOVE    ZERO                TO  SPA-MAE24-FTNSELNUM
           END-IF
      *
           .
       3102-KOHFTN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費負担編集定処理
      *****************************************************************
       31022-KOHFTN-SPAHENSYU-SEC          SECTION.
      *
      *    対象のデータをパラメタファイルより編集する
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTKOHFTN"          TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  WRK-PARA-EDANUM
           MOVE    SPACE               TO  LNK-PTKOHFTN-AREA
           PERFORM
               UNTIL   FLG-PARA            =   1
               MOVE    PARA-DATA-REC           TO  PTKOHFTN-REC
      *        公費ＩＤの同じものを対象とする
               IF      PTKOHFTN-KOHID      =   SPA-NAI24-KOHID
                                                        (WRK-FTN1-IDX)
                   ADD     1                   TO  IDX
                   IF      IDX                 <=  MAX-KOHFTN
                       MOVE    PARA-DATA-REC   TO  LNK-PTKOHFTN-REC
                                                             (IDX)
                       MOVE    IDX             TO  SPA-FTN2-MAX
                   END-IF
               END-IF
      *
               MOVE    PARA-EDANUM         TO  WRK-PARA-EDANUM
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    画面テーブルへ編集する
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-FTN2-MAX
               MOVE    LNK-PTKOHFTN-REC (IDX)  TO  PTKOHFTN-REC
      *
               MOVE    PTKOHFTN-RENNUM     TO  SPA-NAI24-TRENNUM  (IDX)
               MOVE    PTKOHFTN-TEKSTYMD   TO  SPA-NAI24-TFTNSTYMD(IDX)
               MOVE    PTKOHFTN-TEKEDYMD   TO  SPA-NAI24-TFTNEDYMD (IDX)
               MOVE    PTKOHFTN-GAIJGNGAK  TO  SPA-GMN24-TGAIJGNGAK(IDX)
               MOVE    PTKOHFTN-NYUJGNGAK  TO  SPA-GMN24-TNYUJGNGAK(IDX)
      *
               MOVE    PTKOHFTN-TEKSTYMD   TO  WRK-SYMD
               PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-GMN24-TFTNSTYMD(IDX)
               MOVE    PTKOHFTN-TEKEDYMD   TO  WRK-SYMD
               PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-GMN24-TFTNEDYMD(IDX)
      *
               MOVE    PTKOHFTN-TERMID(1:1)    TO  SPA-NAI24-TFTNETCKBN
                                                                  (IDX)
               IF     (PTKOHFTN-TERMID(1:1)    =   SPACE )
      *H28.11 設定区分判定へ
      *            IF     (SPA-NAI24-KOHNUM (WRK-FTN1-IDX)
      *                                        =   "015"
      *                                        OR  "016"
      *                                        OR  "021"
      *                                        OR  "038"    )
      *H26.12
      *                難病・小児特定（H26.1から）
      *              OR  ((SPA-NAI24-KOHNUM  (WRK-FTN1-IDX)
      *                                        =   "052"
      *                                        OR  "054"   )  AND
      *                   (SPA-NAI24-TKOHSTYMD (WRK-FTN1-IDX)
      *                                        >=  "20150101" ))
      ***            OR  ((SPA-NAI24-KOHNUM (WRK-FTN1-IDX)
      *                                        =   "281" ) AND
      ***                 (SPA-PREFNUM         =    07   ))
      *              地方公費の対象
      *              OR   (SPA-NAI24-TJYOGENKBN (WRK-FTN1-IDX)
      *********                                =   "2"    )
      *H28.11
      *            他一部負担あり
                   IF      SPA-NAI24-FTNSYORI  =   1
                       PERFORM 31021-PTKOHFTNETC-HEN-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       31022-KOHFTN-SPAHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費負担有無判定処理
      *****************************************************************
       31021-PTKOHFTNETC-HEN-SEC          SECTION.
      *
           INITIALIZE                      PTKOHFTNETC-REC
           MOVE    SPA-HOSPNUM         TO  PTKOHFTNETC-HOSPNUM
           MOVE    PTKOHFTN-PTID       TO  PTKOHFTNETC-PTID
           MOVE    PTKOHFTN-KOHID      TO  PTKOHFTNETC-KOHID
           STRING  PTKOHFTN-TEKSTYMD(1:6)
                   "%"                  DELIMITED  BY  SIZE
                                       INTO  PTKOHFTNETC-SRYYMD
           END-STRING
      *
           MOVE    PTKOHFTNETC-REC     TO  MCPDATA-REC
           MOVE    "tbl_ptkohftnetc"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohftnetc"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-PTKOHFTNETC-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHFTNETC
           END-IF
           IF      FLG-PTKOHFTNETC     =   ZERO
               MOVE   "1"                  TO  SPA-NAI24-TFTNETCKBN
                                                                  (IDX)
           ELSE
               MOVE   "0"                  TO  SPA-NAI24-TFTNETCKBN
                                                                  (IDX)
           END-IF
           MOVE    "tbl_ptkohftnetc"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       31021-PTKOHFTNETC-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
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
      *    カーソル指定のない時、入力した項目の次ぎへ
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN24-CUR       =   ZERO )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN24-CUR
               WHEN    01
                   MOVE    "P024_TSRSELNUM"     TO  MCP-WIDGET
               WHEN    02
                   MOVE    "P024_TSYSTYMD"      TO  MCP-WIDGET
               WHEN    03
                   MOVE    "P024_TSYEDYMD"      TO  MCP-WIDGET
               WHEN    04
                   MOVE    "P024_TSYSKJGNSTYMD" TO  MCP-WIDGET
               WHEN    05
                   MOVE    "P024_TSYTEKK"      TO  MCP-WIDGET
               WHEN    06
                   MOVE    "P024_TSYNINSTYMD"   TO  MCP-WIDGET
      *        境界層該当
               WHEN    07
                   MOVE    "P024_TSYKYOUKAI"    TO  MCP-WIDGET
               WHEN    08
                   MOVE    "BT1"                TO  MCP-WIDGET
               WHEN    09
                   MOVE    "BT2"                TO  MCP-WIDGET
      *        所得１（年金）
               WHEN    10
                   MOVE    "P024_TNKSELNUM"     TO  MCP-WIDGET
               WHEN    11
                   MOVE    "P024_TNKSTYMD"      TO  MCP-WIDGET
               WHEN    12
                   MOVE    "P024_TNKEDYMD"      TO  MCP-WIDGET
               WHEN    13
                   MOVE    "P024_FUKUSHI"       TO  MCP-WIDGET
               WHEN    14
                   MOVE    "P024_TNKTEKK"      TO  MCP-WIDGET
               WHEN    15
                   MOVE    "P024_TNKNINSTYMD"   TO  MCP-WIDGET
               WHEN    16
                   MOVE    "P024_TNKKYOUKAI"    TO  MCP-WIDGET
               WHEN    18
                   MOVE    "BN1"                TO  MCP-WIDGET
               WHEN    19
                   MOVE    "BN2"                TO  MCP-WIDGET
      *        公費負担
               WHEN    30
                   MOVE    "P024_FTNSELNUM"     TO  MCP-WIDGET
               WHEN    31
                   MOVE    "P024_FTNSELNUM2"    TO  MCP-WIDGET
               WHEN    32
                   MOVE    "P024_FTNTEKSTYMD"   TO  MCP-WIDGET
               WHEN    33
                   MOVE    "P024_FTNTEKEDYMD"   TO  MCP-WIDGET
               WHEN    34
                   MOVE    "P024_GAIJGNGAK"     TO  MCP-WIDGET
               WHEN    35
                   MOVE    "P024_NYUJGNGAK"     TO  MCP-WIDGET
               WHEN    36
                   MOVE    "BF1"                TO  MCP-WIDGET
               WHEN    37
                   MOVE    "BF4"                TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC          SECTION.
      *
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "P024_TSYLIST"
                   MOVE    2           TO  SPA-GMN24-CUR
               WHEN    "P024_TSRSELNUM"
                   MOVE    2           TO  SPA-GMN24-CUR
               WHEN    "P024_TSYSTYMD"
                   MOVE    3           TO  SPA-GMN24-CUR
               WHEN    "P024_TSYEDYMD"
      *D           MOVE    4           TO  SPA-GMN24-CUR
                   MOVE    5           TO  SPA-GMN24-CUR
               WHEN    "P024_TSYSKJGNSTYMD"
      ***          MOVE    8            TO  SPA-GMN24-CUR
                   MOVE    7            TO  SPA-GMN24-CUR
               WHEN    "P024_TSYTEKK"
                   MOVE    6           TO  SPA-GMN24-CUR
               WHEN    "P024_TSYNINSTYMD"
                   MOVE    4           TO  SPA-GMN24-CUR
               WHEN    "P024_TSYKYOUKAI"
                   MOVE    8           TO  SPA-GMN24-CUR
      *
               WHEN    "P024_TNKLIST"
                   MOVE    11          TO  SPA-GMN24-CUR
               WHEN    "P024_TNKSELNUM"
                   MOVE    11          TO  SPA-GMN24-CUR
               WHEN    "P024_TNKSTYMD"
                   MOVE    12          TO  SPA-GMN24-CUR
               WHEN    "P024_TNKEDYMD"
      *D           MOVE    13          TO  SPA-GMN24-CUR
                   MOVE    14          TO  SPA-GMN24-CUR
               WHEN    "P024_FUKUSHI"
      *D           MOVE    18          TO  SPA-GMN24-CUR
                   MOVE    16          TO  SPA-GMN24-CUR
               WHEN    "P024_TNKTEKK"
                   MOVE    15          TO  SPA-GMN24-CUR
               WHEN    "P024_TNKNINSTYMD"
                   MOVE    13          TO  SPA-GMN24-CUR
               WHEN    "P024_TNKKYOUKAI"
                   MOVE    18          TO  SPA-GMN24-CUR
      *        公費負担
               WHEN    "P024_FTNSELNUM"
                   MOVE    31          TO  SPA-GMN24-CUR
               WHEN    "P024_FTNSELNUM2"
                   MOVE    32          TO  SPA-GMN24-CUR
               WHEN    "P024_FTNTEKSTYMD"
                   MOVE    33          TO  SPA-GMN24-CUR
               WHEN    "P024_FTNTEKEDYMD"
                   MOVE    34          TO  SPA-GMN24-CUR
      *H30.11
      *            肝がん治療は入力なし
                   IF     (SPA-NAI24-FTNSELNUM >   ZERO  )
                     AND  (SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
                                               =   "092" )
                       MOVE    35          TO  SPA-GMN24-CUR
                   END-IF
               WHEN    "P024_GAIJGNGAK"
                   IF      SPA-NAI24-FTNETCKBN =   SPACE
                                               OR  "0"
                       MOVE    35          TO  SPA-GMN24-CUR
                   ELSE
                       MOVE    36          TO  SPA-GMN24-CUR
                   END-IF
               WHEN    "P024_NYUJGNGAK"
                   MOVE    36          TO  SPA-GMN24-CUR
      *
           END-EVALUATE
      *
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    登録処理
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    低所得更新
               WHEN    "CLICKED"       ALSO    "BT1"
                   PERFORM 411-TSKRRK-KOUSIN-SEC
      *    低所得削除
               WHEN    "CLICKED"       ALSO    "BT2"
                   PERFORM 412-TSKRREK-DELETE-SEC
      *    年金更新
               WHEN    "CLICKED"       ALSO    "BN1"
                   PERFORM 413-TNKRRK-KOUSIN-SEC
      *    年金削除
               WHEN    "CLICKED"       ALSO    "BN2"
                   PERFORM 414-TNKRRK-DELETE-SEC
      *    公費負担登録
               WHEN    "CLICKED"       ALSO    "BF1"
                   PERFORM 415-KOHFTN-KOUSIN-SEC
      *    公費負担削除
               WHEN    "CLICKED"       ALSO    "BF2"
                   PERFORM 416-KOHFTN-DELETE-SEC
      *    次月複写
               WHEN    "CLICKED"       ALSO    "BF3"
                   PERFORM 417-KOHFTN-COPY-SEC
      *    他一部負担入力
               WHEN    "CLICKED"       ALSO    "BF4"
                   PERFORM 418-KOHFTN-P02K-SEC
      *H28.11
      *HAORI対応
               WHEN    "CLICKED"       ALSO    "APIV3"
                   PERFORM 2009-API-SYORI-SEC
      *
      *     以外はエラー
               WHEN    "CLICKED"       ALSO     ANY
                   MOVE    "0110"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-GMN24-CUR
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得者／更新
      *****************************************************************
       411-TSKRRK-KOUSIN-SEC           SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF  SPA-ERRCD           NOT =   SPACE
               GO      TO              411-TSKRRK-KOUSIN-EXT
           END-IF
      *
      *
           IF      SPA-GMN24-TSYSELNUM =   ZERO  OR  SPACE
               COMPUTE SPA-NAI24-TSYSELNUM =   SPA-TSY-MAX   +   1
               ADD     1               TO  SPA-TSY-MAX
               IF      SPA-TSY-MAX         >   20
                   MOVE   "0008"       TO  SPA-ERRCD
      *----(01.00.01) LINE ADD START --------------------------------
                   MOVE    1           TO  SPA-GMN24-CUR
      *----(01.00.01) LINE ADD END   --------------------------------
                   COMPUTE SPA-TSY-MAX =   SPA-TSY-MAX   -   1
                   GO     TO           411-TSKRRK-KOUSIN-EXT
               END-IF
               MOVE    SPA-TSY-MAX     TO  WRK-N-IDX
           END-IF
           MOVE    WRK-N-IDX           TO  IDX
           MOVE    SPA-GMN24-TSYSTYMDW TO  SPA-GMN24-TSYDPSTYMDW(IDX)
           MOVE    SPA-GMN24-TSYEDYMDW TO  SPA-GMN24-TSYDPEDYMDW(IDX)
           MOVE    SPA-GMN24-TSYSTYMD  TO  SPA-GMN24-TSYDPSTYMD (IDX)
           MOVE    SPA-GMN24-TSYEDYMD  TO  SPA-GMN24-TSYDPEDYMD (IDX)
           MOVE    SPA-GMN24-TSYSKJGNSTYMDW
                                       TO  SPA-GMN24-TSYLSKJGNSTYMDW
                                                                (IDX)
           MOVE    SPA-GMN24-TSYSKJGNSTYMD
                                       TO  SPA-GMN24-TSYLSKJGNSTYMD
                                                                (IDX)
      *
           MOVE    SPA-GMN24-TSYNINSTYMD
                                       TO  SPA-GMN24-TSYLNINYMD (IDX)
           MOVE    SPA-NAI24-TSYNINSTYMD
                                       TO  SPA-NAI24-TSYLNINYMD (IDX)
           MOVE    SPA-GMN24-TSYTEKKBN
                                       TO  SPA-NAI24-TSYLHKNKBN (IDX)
           MOVE    SPA-GMN24-TSYTEKKBN TO  WRK-HKNTEKKBN
           PERFORM 41002-HKNTEKKBN-HEN-SEC
           MOVE    WRK-HKNTEKKBN-MEI   TO  SPA-GMN24-TSYLHKNKBN (IDX)
      *H29.6
      *    境界層該当区分
           MOVE    SPA-GMN24-TSYKYOUKAI
                                       TO  SPA-NAI24-TSYLKYOUKAI(IDX)
           MOVE    SPA-GMN24-TSYKYOUKAI    TO  WRK-KYOUKAI
           PERFORM 41003-KYOUKAI-HEN-SEC
           MOVE    WRK-KYOUKAI-MEI     TO  SPA-GMN24-TSYLKYOUKAI(IDX)
      *
           INITIALIZE                  SPA-GMN24-TSYS-AREA
      *
           MOVE    1                   TO  SPA-GMN24-CUR
      *
           .
       411-TSKRRK-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得者／削除
      *****************************************************************
       412-TSKRREK-DELETE-SEC          SECTION.
      *
           IF   SPA-NAI24-TSYSELNUM    =   ZERO  OR  SPACE
                GO      TO             412-TSKRREK-DELETE-EXT
           END-IF
      *
           INITIALIZE                  WRK-NAIBU-IDX-AREA
           MOVE    SPA-NAI24-TSYSELNUM TO  WRK-G-IDX
           MOVE    1                   TO  WRK-W-TBL
           PERFORM 4501-NAIBU-IDX-GET-SEC
           IF  WRK-N-IDX               =   ZERO
               MOVE    "2401"          TO  SPA-ERRCD
      *----(01.00.01) LINE ADD START --------------------------------
               MOVE    1               TO  SPA-GMN24-CUR
      *----(01.00.01) LINE ADD END   --------------------------------
               GO      TO              412-TSKRREK-DELETE-EXT
           END-IF
           MOVE    "1"                 TO  SPA-GMN24-TSYDELKBN
                                                     (WRK-N-IDX)
      *
           INITIALIZE                  SPA-GMN24-TSYS-AREA
      *
           MOVE    2                   TO  SPA-GMN24-CUR
      *
           .
       412-TSKRREK-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    年金／更新
      *****************************************************************
       413-TNKRRK-KOUSIN-SEC           SECTION.
      *
           MOVE    2                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF  SPA-ERRCD           NOT =   SPACE
               GO      TO              413-TNKRRK-KOUSIN-EXT
           END-IF
      *
      *
           IF      SPA-GMN24-TNKSELNUM =   ZERO  OR  SPACE
               COMPUTE SPA-NAI24-TNKSELNUM =   SPA-TNK-MAX   +   1
               ADD     1               TO  SPA-TNK-MAX
               IF  SPA-TNK-MAX         >   20
                   MOVE   "0008"       TO  SPA-ERRCD
                   MOVE    10          TO  SPA-GMN24-CUR
                   COMPUTE SPA-TNK-MAX =   SPA-TNK-MAX   -   1
                   GO     TO           413-TNKRRK-KOUSIN-EXT
               END-IF
               MOVE    SPA-TNK-MAX     TO  WRK-N-IDX
           END-IF
           MOVE    WRK-N-IDX           TO  IDX
           MOVE    SPA-GMN24-TNKSTYMDW TO  SPA-GMN24-TNKDPSTYMDW(IDX)
           MOVE    SPA-GMN24-TNKEDYMDW TO  SPA-GMN24-TNKDPEDYMDW(IDX)
           MOVE    SPA-GMN24-TNKSTYMD  TO  SPA-GMN24-TNKDPSTYMD (IDX)
           MOVE    SPA-GMN24-TNKEDYMD  TO  SPA-GMN24-TNKDPEDYMD (IDX)
      *
           MOVE    SPA-GMN24-FUKUSHI-KBN   TO  SPA-GMN24-TFUKUSHI-KBN
                                                                (IDX)
           MOVE    SPA-GMN24-FUKUSHI-MEI   TO  SPA-GMN24-TFUKUSHI-MEI
                                                                (IDX)
      *
           MOVE    SPA-GMN24-TNKNINSTYMD
                                       TO  SPA-GMN24-TNKLNINYMD (IDX)
           MOVE    SPA-NAI24-TNKNINSTYMD
                                       TO  SPA-NAI24-TNKLNINYMD (IDX)
           MOVE    SPA-GMN24-TNKTEKKBN
                                       TO  SPA-NAI24-TNKLHKNKBN (IDX)
           MOVE    SPA-GMN24-TNKTEKKBN TO  WRK-HKNTEKKBN
           PERFORM 41002-HKNTEKKBN-HEN-SEC
           MOVE    WRK-HKNTEKKBN-MEI   TO  SPA-GMN24-TNKLHKNKBN (IDX)
      *H29.6
      *    境界層該当区分
           MOVE    SPA-GMN24-TNKKYOUKAI
                                       TO  SPA-NAI24-TNKLKYOUKAI(IDX)
           MOVE    SPA-GMN24-TNKKYOUKAI    TO  WRK-KYOUKAI
           PERFORM 41003-KYOUKAI-HEN-SEC
           MOVE    WRK-KYOUKAI-MEI TO  SPA-GMN24-TNKLKYOUKAI(IDX)
      *
      *
           INITIALIZE                  SPA-GMN24-TNK-AREA
      *
           MOVE    10                  TO  SPA-GMN24-CUR
           .
       413-TNKRRK-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    年金／削除
      *****************************************************************
       414-TNKRRK-DELETE-SEC           SECTION.
      *
           IF   SPA-NAI24-TNKSELNUM    =   ZERO  OR  SPACE
                GO      TO             414-TNKRRK-DELETE-EXT
           END-IF
      *
           INITIALIZE                  WRK-NAIBU-IDX-AREA
           MOVE    SPA-NAI24-TNKSELNUM TO  WRK-G-IDX
           MOVE    2                   TO  WRK-W-TBL
           PERFORM 4501-NAIBU-IDX-GET-SEC
           IF  WRK-N-IDX               =   ZERO
               MOVE    "2406"          TO  SPA-ERRCD
               MOVE    10              TO  SPA-GMN24-CUR
               GO      TO              414-TNKRRK-DELETE-EXT
           END-IF
           MOVE    "1"                 TO  SPA-GMN24-TNKDELKBN
                                                     (WRK-N-IDX)
      *
           INITIALIZE                  SPA-GMN24-TNK-AREA
      *
           MOVE    7                   TO  SPA-GMN24-CUR
           .
       414-TNKRRK-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担更新処理
      *****************************************************************
       415-KOHFTN-KOUSIN-SEC           SECTION.
      *
           MOVE    3                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO              415-KOHFTN-KOUSIN-EXT
           END-IF
      *
           IF      SPA-NAI24-FTNSELNUM2    =   ZERO
      *        追加
               IF      SPA-FTN2-MAX            >=  MAX-KOHFTN
      **********   MOVE    "2443"              TO  SPA-ERRCD
                   MOVE    "2444"              TO  SPA-ERRCD
                   GO      TO              415-KOHFTN-KOUSIN-EXT
               END-IF
               ADD     1                       TO  SPA-FTN2-MAX
               MOVE    SPA-FTN2-MAX            TO  IDX
      *
               MOVE    "0"                 TO  SPA-NAI24-TFTNETCKBN(IDX)
      *
               MOVE    2                   TO  SPA-NAI24-INIT2
      *
           ELSE
               MOVE    SPA-NAI24-FTNSELNUM2    TO  IDX
           END-IF
           MOVE    SPA-GMN24-FTNSTYMD  TO  SPA-GMN24-TFTNSTYMD (IDX)
           MOVE    SPA-GMN24-FTNEDYMD  TO  SPA-GMN24-TFTNEDYMD (IDX)
           MOVE    SPA-NAI24-GAIJGNGAK TO  SPA-GMN24-TGAIJGNGAK (IDX)
           MOVE    SPA-NAI24-NYUJGNGAK TO  SPA-GMN24-TNYUJGNGAK (IDX)
           MOVE    SPA-NAI24-FTNSTYMD  TO  SPA-NAI24-TFTNSTYMD (IDX)
           MOVE    SPA-NAI24-FTNEDYMD  TO  SPA-NAI24-TFTNEDYMD (IDX)
      *
           INITIALIZE                  SPA-GMN24-FTN-AREA2
      *
           MOVE    31                  TO  SPA-GMN24-CUR
           .
       415-KOHFTN-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担／削除
      *****************************************************************
       416-KOHFTN-DELETE-SEC          SECTION.
      *
           IF   SPA-NAI24-FTNSELNUM2   =   ZERO
                GO      TO             416-KOHFTN-DELETE-EXT
           END-IF
      *
           MOVE    "1"                 TO  SPA-NAI24-FTNDELKBN2
                                                (SPA-NAI24-FTNSELNUM2)
      *    他一部負担額削除
           IF      SPA-NAI24-TFTNETCKBN (SPA-NAI24-FTNSELNUM2)
                                       =   "1"
                                       OR  "2"
               PERFORM 4161-KOHFTNETC-DELCHK-SEC
           END-IF
      *
           INITIALIZE                  SPA-GMN24-FTN-AREA2
      *
           MOVE    31                  TO  SPA-GMN24-CUR
      *
           .
       416-KOHFTN-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    他一部負担額削除処理
      *****************************************************************
       4161-KOHFTNETC-DELCHK-SEC             SECTION.
      *
           MOVE    SPA-NAI24-TFTNSTYMD (SPA-NAI24-FTNSELNUM2)
                                       TO  WRK-YMD
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  IDK2
           PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL       IDK     >   SPA-FTN2-MAX
               IF      SPA-NAI24-FTNSELNUM2    =   IDK
                   CONTINUE
               ELSE
                   IF     (SPA-NAI24-FTNDELKBN2 (IDK)  =   SPACE ) AND
                          (SPA-NAI24-TFTNSTYMD (IDK)(1:6)
                                               =   WRK-YMD (1:6))
                       MOVE    IDK             TO  IDK2
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
      *        他一部負担履歴削除
               PERFORM 41611-KOHFTNETC-DEL-SEC
           ELSE
      *        他一部負担累計編集
               IF      SPA-GMN24-TNYUJGNGAK (SPA-NAI24-FTNSELNUM2)
                                           NOT =   ZERO
                   MOVE    SPA-GMN24-TNYUJGNGAK (SPA-NAI24-FTNSELNUM2)
                                           TO  SPA-GMN24-TNYUJGNGAK
                                                              (IDK2)
               END-IF
           END-IF
           .
       4161-KOHFTNETC-DELCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担額次月複写処理
      *****************************************************************
       41611-KOHFTNETC-DEL-SEC             SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTKOHFTNETC"       TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           PERFORM
               UNTIL   FLG-PARA            =   1
               MOVE    PARA-DATA-REC       TO  PTKOHFTNETC-REC
               IF     (PTKOHFTNETC-KOHID   =   SPA-NAI24-KOHID
                                                (SPA-NAI24-FTNSELNUM))
               AND    (PTKOHFTNETC-SRYYMD(1:6)
                                           =   WRK-YMD(1:6))
      *            削除
                   MOVE    PARA-REC            TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_para"          TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                                   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               END-IF
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    更新対象編集
           PERFORM VARYING     IDX3    FROM    1   BY   1
                   UNTIL      (IDX3    >   MAX-KOHFTN  )
               IF      SPA-NAI24-KOHFTN-ID (IDX3)   =   ZERO
                    MOVE    SPA-NAI24-KOHID(SPA-NAI24-FTNSELNUM)
                                           TO  SPA-NAI24-KOHFTN-ID
                                                              (IDX3)
                    MOVE    SPA-NAI24-TFTNSTYMD (SPA-NAI24-FTNSELNUM2)
                                                            (1:6)
                                           TO  SPA-NAI24-KOHFTN-YM
                                                              (IDX3)
               END-IF
               IF     (SPA-NAI24-KOHID(SPA-NAI24-FTNSELNUM)
                                           =   SPA-NAI24-KOHFTN-ID
                                                                (IDX3))
               AND    (SPA-NAI24-TFTNSTYMD (SPA-NAI24-FTNSELNUM2)(1:6)
                                           =   SPA-NAI24-KOHFTN-YM
                                                                 (IDX3))
                   MOVE    MAX-KOHFTN      TO  IDX3
               END-IF
           END-PERFORM
           .
        41611-KOHFTNETC-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担額次月複写処理
      *****************************************************************
       417-KOHFTN-COPY-SEC             SECTION.
      *
      *    選択解除
           INITIALIZE                  SPA-GMN24-FTN-AREA2
      *    対象の公費であること
           IF      (SPA-NAI24-FTNSELNUM >   ZERO   )
      *
      *H28.11 設定区分判定へ
      *        IF     (SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
      *                                    =   "015"
      *                                    OR  "016"
      *                                    OR  "021"
      *                                    OR  "038"    )
      *H26.12
      *            難病・小児特定（H26.1から）
      *          OR  ((SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
      *                                    =   "052"
      *                                    OR  "054"   )  AND
      *               (SPA-NAI24-TKOHSTYMD (SPA-NAI24-FTNSELNUM)
      *                                    >=  "20150101" ))
      ***        OR  ((SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
      *                                    =   "281" ) AND
      ***             (SPA-PREFNUM         =    07   ))
      *          地方公費の対象
      *          OR   (SPA-NAI24-TJYOGENKBN (SPA-NAI24-FTNSELNUM)
      *                                    =   "2"    )
      *H28.11
      *        他一部負担あり
               IF      SPA-NAI24-FTNSYORI  =   1
      *H30.11
      *            肝がん治療
                 OR   (SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
                                           =   "092"   )
                   CONTINUE
               ELSE
                   MOVE    "2440"              TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    "2440"              TO  SPA-ERRCD
           END-IF
      *    複写元判定
           IF      (SPA-ERRCD          =   SPACE  )  AND
                   (SPA-FTN2-MAX       =   ZERO   )
               MOVE    "2441"              TO  SPA-ERRCD
           END-IF
      *    終了日判定
           IF      SPA-ERRCD           =   SPACE
               MOVE    ZERO                TO  IDX-F
               MOVE    SPACE               TO  WRK-FTNEDYMD
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   SPA-FTN2-MAX
      *
                   IF      SPA-NAI24-TFTNEDYMD (IDX)   >   WRK-FTNEDYMD
                    AND   (SPA-NAI24-FTNDELKBN2(IDX)  =   SPACE )
                       MOVE    IDX                 TO  IDX-F
                       MOVE    SPA-NAI24-TFTNEDYMD (IDX)
                                                   TO  WRK-FTNEDYMD
                   END-IF
               END-PERFORM
           END-IF
      *    公費終了年月＝終了日判定
           IF      (SPA-ERRCD          =   SPACE  )  AND
                   (SPA-NAI24-TFTNEDYMD(IDX-F)
                                   NOT =   "99999999" ) AND
                   (SPA-NAI24-TFTNEDYMD(IDX-F)(1:6)
                                       >=  SPA-NAI24-TKOHEDYMD
                                               (SPA-NAI24-FTNSELNUM)
                                               (1:6)  )
               MOVE    "2443"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-FTN2-MAX        >=  MAX-KOHFTN
      ****     MOVE    "2443"              TO  SPA-ERRCD
               MOVE    "2444"              TO  SPA-ERRCD
           END-IF
      *    公費負担額複写
           IF      (SPA-ERRCD          =   SPACE  ) 
               PERFORM 4171-KOHFTN-COPY-SYORI-SEC
           ELSE
               MOVE    30                  TO  SPA-GMN24-CUR
           END-IF
           .
       417-KOHFTN-COPY-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担額複写処理
      *****************************************************************
       4171-KOHFTN-COPY-SYORI-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-ENDYMD
           IF      SPA-NAI24-TFTNEDYMD (IDX-F) =   "99999999"
      *        開始年月日の月末を終了日とする
               MOVE    SPACE               TO  STS-AREA-DAY
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE    SPA-NAI24-TFTNSTYMD (IDX-F)(1:6)
                                           TO  LNK-DAY7-YM
               CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY7-AREA
               MOVE    LNK-DAY7-KOYOMI     TO  SPA-NAI24-TFTNEDYMD
                                                             (IDX-F)
               MOVE    1                   TO  FLG-ENDYMD
      *        公費の終了日
               IF      SPA-NAI24-TFTNEDYMD (IDX-F)
                                           >   SPA-NAI24-TKOHEDYMD
                                               (SPA-NAI24-FTNSELNUM)
                   MOVE    SPA-NAI24-TKOHEDYMD(SPA-NAI24-FTNSELNUM)
                                           TO  SPA-NAI24-TFTNEDYMD
                                                             (IDX-F)
                   MOVE    ZERO            TO  FLG-ENDYMD
               END-IF
               MOVE    SPA-NAI24-TFTNEDYMD  (IDX-F)
                                           TO  WRK-SYMD
               PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-GMN24-TFTNEDYMD
                                                            (IDX-F)
           END-IF
      *    公費の終了日
           IF      SPA-NAI24-TFTNEDYMD (IDX-F)
                                           >=  SPA-NAI24-TKOHEDYMD
                                               (SPA-NAI24-FTNSELNUM)
               GO      TO      4171-KOHFTN-COPY-SYORI-EXT
           END-IF
      *    最終月の翌月
           MOVE    SPA-NAI24-TFTNEDYMD (IDX-F)
                                           TO  WRK-YMD
           IF      WRK-MM              =   12
               COMPUTE WRK-FTNSTYY         =   WRK-YY   +   1
               MOVE    01                  TO  WRK-FTNSTMM
           ELSE
               MOVE    WRK-YY              TO  WRK-FTNSTYY
               COMPUTE WRK-FTNSTMM         =   WRK-MM   +   1
           END-IF
           MOVE    01                  TO  WRK-FTNSTDD
      *    月末日
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-FTNSTYMD(1:6)   TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-FTNEDYMD
           IF      WRK-FTNEDYMD        >   SPA-NAI24-TKOHEDYMD
                                               (SPA-NAI24-FTNSELNUM)
               MOVE    SPA-NAI24-TKOHEDYMD (SPA-NAI24-FTNSELNUM)
                                           TO  WRK-FTNEDYMD
              MOVE    ZERO                 TO  FLG-ENDYMD
           END-IF
           IF      FLG-ENDYMD          =   1
               MOVE    "99999999"          TO  WRK-FTNEDYMD
           END-IF
      *
      *    追加
           ADD     1                       TO  SPA-FTN2-MAX
           MOVE    SPA-FTN2-MAX            TO  IDX
           MOVE    SPA-GMN24-TGAIJGNGAK(IDX-F)
                                       TO  SPA-GMN24-TGAIJGNGAK (IDX)
           MOVE    ZERO                TO  SPA-GMN24-TNYUJGNGAK (IDX)
      *H30.11
      *    肝がん治療は両方複写
           IF     (SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
                                           =   "092"   )
               MOVE    SPA-GMN24-TGAIJGNGAK(IDX-F)
                                       TO  SPA-GMN24-TGAIJGNGAK (IDX)
               MOVE    SPA-GMN24-TNYUJGNGAK(IDX-F)
                                       TO  SPA-GMN24-TNYUJGNGAK (IDX)
           END-IF
      *
           MOVE    WRK-FTNSTYMD        TO  SPA-NAI24-TFTNSTYMD (IDX)
           MOVE    WRK-FTNEDYMD        TO  SPA-NAI24-TFTNEDYMD (IDX)
           MOVE    "0"                 TO  SPA-NAI24-TFTNETCKBN (IDX)
      *
           MOVE    WRK-FTNSTYMD        TO  WRK-SYMD
           PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-GMN24-TFTNSTYMD(IDX)
           MOVE    WRK-FTNEDYMD        TO  WRK-SYMD
           PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-GMN24-TFTNEDYMD(IDX)
      *
           MOVE    2                   TO  SPA-NAI24-INIT2
      *
           .
       4171-KOHFTN-COPY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    他一部負担入力処理
      *****************************************************************
       418-KOHFTN-P02K-SEC             SECTION.
      *
      *    対象の公費であること
           IF      (SPA-NAI24-FTNSELNUM >   ZERO   )
      *H28.11 設定区分判定へ
      *        IF     (SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
      *                                    =   "015"
      *                                    OR  "016"
      *                                    OR  "021"
      *                                    OR  "038"    )
      *H26.12
      *            難病・小児特定（H26.1から）
      *          OR  ((SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
      *                                    =   "052"
      *                                    OR  "054"   )  AND
      *               (SPA-NAI24-TKOHSTYMD (SPA-NAI24-FTNSELNUM)
      *                                    >=  "20150101" ))
      *
      **         OR  ((SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
      *                                    =   "281" ) AND
      **              (SPA-PREFNUM         =    07   ))
      *            地方公費の対象分
      *          OR   (SPA-NAI24-TJYOGENKBN (SPA-NAI24-FTNSELNUM)
      *****                                =   "2"    )
      *H28.11
      *        他一部負担あり
               IF      SPA-NAI24-FTNSYORI  =   1
                   CONTINUE
               ELSE
                   MOVE    "2445"              TO  SPA-ERRCD
                   MOVE    30                  TO  SPA-GMN24-CUR
               END-IF
           ELSE
               MOVE    "2445"              TO  SPA-ERRCD
               MOVE    30                  TO  SPA-GMN24-CUR
           END-IF
      *    上限履歴の選択
           IF     (SPA-ERRCD               =   SPACE  )  AND
                  (SPA-NAI24-FTNSELNUM2    =   ZERO   )
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   SPA-FTN2-MAX
                   IF     (SPA-NAI24-TFTNSTYMD (IDX)(1:6)
                                               =   SPA-SYSYMD(1:6))
                     AND  (SPA-NAI24-FTNDELKBN2 (IDX)  =   SPACE )
                       MOVE    IDX             TO   SPA-NAI24-FTNSELNUM2
                       MOVE    SPA-FTN2-MAX    TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *
           IF     (SPA-ERRCD               =   SPACE  )  AND
                  (SPA-NAI24-FTNSELNUM2    =   ZERO   )
               MOVE    "2446"              TO  SPA-ERRCD
               MOVE    31                  TO  SPA-GMN24-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4181-P02K-SYORI-SEC
           END-IF
           .
       418-KOHFTN-P02K-EXT.
           EXIT.
      *
      *****************************************************************
      *    他一部負担金入力画面処理
      *****************************************************************
       4181-P02K-SYORI-SEC              SECTION.
      *
           INITIALIZE                      SPA-P02K-AREA
      *
           MOVE    SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
                                       TO  SPA-P02K-KOHNUM
           MOVE    SPA-NAI24-KOHID  (SPA-NAI24-FTNSELNUM)
                                       TO  SPA-P02K-KOHID
           MOVE    SPA-NAI24-TFTNSTYMD (SPA-NAI24-FTNSELNUM2)
                                       TO  SPA-P02K-NAI-SRYYMD
           MOVE    SPA-NAI24-TFTNEDYMD (SPA-NAI24-FTNSELNUM2)
                                       TO  SPA-P02K-NAI-ENDYMD
      *    開始年月末日
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SPA-P02K-NAI-SRYYMD(1:6)
                                       TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI(7:2)
                                       TO  SPA-P02K-NAIMATU
      *    同じ年月はまとめる
           MOVE    SPACE               TO  SPA-P02K-FTNRUIKBN
           MOVE    ZERO                TO  IDK2
           MOVE    ZERO                TO  WRK-TGAIJGNGAK-G
           MOVE    ZERO                TO  SPA-P02K-TNYUJGNGAK
           PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL       IDK     >   SPA-FTN2-MAX
               IF      SPA-NAI24-FTNSELNUM2    =   IDK
                   ADD     SPA-GMN24-TGAIJGNGAK (IDK)
                                               TO  SPA-P02K-GAIJGNGAKKEI
                   ADD     1                   TO  IDK2
                   MOVE    SPA-GMN24-TGAIJGNGAK (IDK)
                                               TO  WRK-TGAIJGNGAK (IDK2)
      *            負担累計
                   ADD     SPA-GMN24-TNYUJGNGAK (IDK)
                                               TO  SPA-P02K-TNYUJGNGAK
                   IF      SPA-NAI24-TFTNETCKBN (IDK)
                                           NOT =   "0"
                       MOVE    "1"             TO  SPA-P02K-FTNRUIKBN
                   END-IF
               ELSE
                   IF     (SPA-NAI24-TFTNSTYMD (IDK)(1:6)
                                           =   SPA-P02K-NAI-SRYYMD(1:6))
                     AND  (SPA-NAI24-FTNDELKBN2 (IDK)  =   SPACE )
                       IF      SPA-NAI24-TFTNSTYMD (IDK)(7:2)
                                           <   SPA-P02K-NAI-SRYYMD(7:2)
                           MOVE    SPA-NAI24-TFTNSTYMD (IDK)
                                               TO  SPA-P02K-NAI-SRYYMD
                       END-IF
                       IF      SPA-NAI24-TFTNEDYMD (IDK)
                                           >   SPA-P02K-NAI-ENDYMD
                           MOVE    SPA-NAI24-TFTNEDYMD (IDK)
                                               TO  SPA-P02K-NAI-ENDYMD
                       END-IF
      *                上限額編集
                       ADD     SPA-GMN24-TGAIJGNGAK (IDK)
                                               TO  SPA-P02K-GAIJGNGAKKEI
                       ADD     1               TO  IDK2
                       MOVE    SPA-GMN24-TGAIJGNGAK (IDK)
                                               TO  WRK-TGAIJGNGAK (IDK2)
      *                負担累計
                       ADD     SPA-GMN24-TNYUJGNGAK (IDK)
                                               TO  SPA-P02K-TNYUJGNGAK
                       IF      SPA-NAI24-TFTNETCKBN (IDK)
                                               NOT =   "0"
                           MOVE    "1"         TO  SPA-P02K-FTNRUIKBN
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *    開始年月＝終了年月
           IF      SPA-P02K-NAI-ENDYMD(1:6)
                                   NOT =   SPA-P02K-NAI-SRYYMD (1:6)
               MOVE    LNK-DAY7-KOYOMI     TO  SPA-P02K-NAI-ENDYMD
           END-IF
      *    和暦変換
           MOVE    SPA-P02K-NAI-SRYYMD TO  WRK-SYMD
           PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-P02K-GMN-SRYYMD
           MOVE    SPA-P02K-NAI-ENDYMD TO  WRK-SYMD
           PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-P02K-GMN-ENDYMD
      *    上限額表示
           MOVE    SPACE               TO  WRK-MSG
           MOVE    SPACE               TO  WRK-MSG01
           IF      IDK2                >   1
               PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL      (IDK     >   10 )
                          OR  (WRK-TGAIJGNGAK (IDK)    =   ZERO)
                   MOVE    WRK-TGAIJGNGAK (IDK)    TO  WRK-Z7
                   PERFORM 41812-KINGAK-HEN-SEC
                   MOVE    SPACE               TO  WRK-MSG01
                   IF      WRK-MSG             =   SPACE
                       STRING "("
                          WRK-HKINGAK      DELIMITED  BY  SPACE
                                           INTO      WRK-MSG01
                       END-STRING
                   ELSE
                       STRING WRK-MSG          DELIMITED  BY  SPACE
                          "+"
                          WRK-HKINGAK      DELIMITED  BY  SPACE
                                           INTO      WRK-MSG01
                       END-STRING
                   END-IF
                   MOVE    WRK-MSG01           TO  WRK-MSG
               END-PERFORM
               MOVE    1                   TO  SPA-P02K-FUKUFLG
           ELSE
               MOVE    ZERO                TO  SPA-P02K-FUKUFLG
           END-IF
           IF      WRK-MSG             =   SPACE
               MOVE    SPACE               TO  SPA-P02K-MSG06
           ELSE
               STRING  WRK-MSG
                       ")"                 DELIMITED  BY  SPACE
                                           INTO    SPA-P02K-MSG06
               END-STRING
           END-IF
      *    他一部負担累計入力
           MOVE    SPACE               TO  SPA-P02K-MSG07
           IF      SPA-P02K-FTNRUIKBN  =   SPACE
               IF      SPA-P02K-TNYUJGNGAK     >   ZERO
                   MOVE    SPA-P02K-TNYUJGNGAK     TO  WRK-Z8
                   STRING    "他一部負担累計："
                             WRK-Z8        DELIMITED  BY  SIZE
                                           INTO    SPA-P02K-MSG07
                   END-STRING
                   MOVE    "9"             TO  SPA-P02K-FTNRUIKBN
               END-IF
           ELSE
               MOVE    ZERO            TO  SPA-P02K-TNYUJGNGAK
           END-IF
      *
           MOVE    SPACE               TO  SPA-MOTOPG
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP02K"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
           MOVE    "NYUGAIKBN"         TO  MCP-WIDGET
           IF      SPA-P02K-FTNRUIKBN  =   "9"
               MOVE    "B01"               TO  MCP-WIDGET
           END-IF
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
      *
           MOVE    "P02K"              TO  SPA-SAKIPG
           MOVE    "P02"               TO  SPA-MOTOPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P02K"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           MOVE    1                   TO  SPA-FLG-END
      *
           .
       4181-P02K-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量編集処理
      *****************************************************************
       41812-KINGAK-HEN-SEC           SECTION.
      *
           MOVE    SPACE              TO  WRK-HKINGAK
           PERFORM VARYING     IDK2    FROM    1   BY  1
                   UNTIL       IDK2    >   7
               IF      WRK-Z7 (IDK2:1)     NOT =   SPACE
                   MOVE    WRK-Z7 (IDK2:)      TO  WRK-HKINGAK
                   MOVE    7                   TO  IDK2
               END-IF
           END-PERFORM
           .
       41812-KINGAK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       490-TOUROKU-SEC             SECTION.
      *
      *    所得・年金レコード数を一時退避
           MOVE    SPA-TSY-MAX     TO  WRK-TSY-SMAX
           MOVE    SPA-TNK-MAX     TO  WRK-TNK-SMAX
      *    パラメタファイル再検索処理
           INITIALIZE                  LNK-TNK-AREA
           INITIALIZE                  LNK-TSY-AREA
           PERFORM 320-PARAFILE-KENSAKU-SEC
      *    初期表示時の所得・年金レコード数を保存
           MOVE    SPA-TSY-MAX     TO  WRK-TSY-MAX
           MOVE    SPA-TNK-MAX     TO  WRK-TNK-MAX
           MOVE    WRK-TSY-SMAX    TO  SPA-TSY-MAX
           MOVE    WRK-TNK-SMAX    TO  SPA-TNK-MAX
      *
      *    画面内容をパラメタ出力処理
           PERFORM 500-SPA-PARA-TSYRRK-SEC
           PERFORM 500-SPA-PARA-TNKRRK-SEC
      *    公費負担
           MOVE    SPACE               TO  HZN-PTKOHFTN-AREA
           INITIALIZE                  HZN-PTKOHFTN-AREA
           IF      SPA-GMN24-FTNSELNUM NOT =   ZERO
               PERFORM 410131-KOHFTN-PARA-SEC
      *        患者公費負担額
               PERFORM 410132-KOHFTNETC-PARA-SEC
           END-IF
      *
      *    公費負担の登録チェック
           IF      SPA-TAGU-FLG        =   ZERO
               IF      SPA-NAI-KOHRRKKBN-FLG   =   "1"
                   IF      SPA-P024-KOHKEIFLG  =   ZERO
                       PERFORM 410132-KOHFTN-CHK-SEC
                   END-IF
               END-IF
           END-IF
           .
       490-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    低所得者画面の内容をパラメタへ保存
      *****************************************************************
       500-SPA-PARA-TSYRRK-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  SPA-NAI-TSYRRK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX         >   20          )
                   OR        ( IDX         >   SPA-TSY-MAX )
      *********MOVE    SPACE               TO  LNK-TSY-KEY-O   (IDX)
      *        初期表示時の所得レコード数以内の場合
               IF      IDX                 <=  WRK-TSY-MAX
      *            削除データの場合
                   IF  SPA-GMN24-TSYDELKBN(IDX)   =   "1"
                       MOVE    ALL "9"     TO  LNK-TSY-UPYMD      (IDX)
                   ELSE
      **************   変更前キー転送
      *                MOVE    LNK-TSY-HOSPNUM (IDX)
      *                                    TO  LNK-TSY-HOSPNUM-O   (IDX)
      *                MOVE    LNK-TSY-PTID   (IDX)
      *                                    TO  LNK-TSY-PTID-O     (IDX)
      *                MOVE    LNK-TSY-NINSTYMD (IDX)
      **************                       TO  LNK-TSY-NINSTYMD-O (IDX)
      *                認定期間変更時、変更年月日・更新年月日転送
                       IF  ( SPA-GMN24-TSYDPSTYMD(IDX)
                                   NOT =   LNK-TSY-NINSTYMD(IDX)) OR
                           ( SPA-GMN24-TSYDPEDYMD(IDX)
                                   NOT =   LNK-TSY-NINEDYMD(IDX)) OR
                           ( SPA-GMN24-TSYLSKJGNSTYMD(IDX)
                                   NOT =   LNK-TSY-SKJGNSTYMD(IDX))
                       OR  ( SPA-NAI24-TSYLNINYMD(IDX)
                                   NOT =   LNK-TSY-SKJNINSTYMD(IDX))
                       OR  ( SPA-NAI24-TSYLHKNKBN(IDX)
                                   NOT =   LNK-TSY-HKNTEKKBN(IDX))
                       OR  ( SPA-NAI24-TSYLKYOUKAI(IDX)
                                   NOT =   LNK-TSY-KYOUKAISOUKBN(IDX))
                           MOVE    SPA-SYSYMD  TO  LNK-TSY-UPYMD  (IDX)
                           MOVE    SPA-GMN24-TSYDPSTYMD (IDX)
                                           TO  LNK-TSY-NINSTYMD   (IDX)
                           MOVE    SPA-GMN24-TSYDPEDYMD (IDX)
                                           TO  LNK-TSY-NINEDYMD   (IDX)
                           MOVE    SPA-GMN24-TSYLSKJGNSTYMD (IDX)
                                           TO  LNK-TSY-SKJGNSTYMD (IDX)
      *                    食事標準負担額減額開始日
                           MOVE    SPA-NAI24-TSYLNINYMD (IDX)
                                           TO  LNK-TSY-SKJNINSTYMD (IDX)
      *                    保険適用区分
                           MOVE    SPA-NAI24-TSYLHKNKBN (IDX)
                                           TO  LNK-TSY-HKNTEKKBN   (IDX)
      *                    境界層該当区分
                           MOVE    SPA-NAI24-TSYLKYOUKAI (IDX)
                                           TO  LNK-TSY-KYOUKAISOUKBN
                                                                  (IDX)
                       END-IF
                   END-IF
                   MOVE    IDX             TO  IDY
                   PERFORM 510-PARA-TSYRRK-OUT-SEC
               ELSE
      *        初期表示時の所得レコード数を超える場合
                   IF  SPA-GMN24-TSYDELKBN (IDX)   =   SPACE
                       ADD     1               TO  IDY
      *
                       INITIALIZE                  LNK-TSY-REC (IDY)
                       MOVE    SPA-HOSPNUM     TO  LNK-TSY-HOSPNUM (IDY)
                       MOVE    SPA-GMN-PTID    TO  LNK-TSY-PTID (IDY)
                       MOVE    SPA-GMN24-TSYDPSTYMD (IDX)
                                           TO  LNK-TSY-NINSTYMD   (IDY)
                       MOVE    SPA-GMN24-TSYDPEDYMD (IDX)
                                           TO  LNK-TSY-NINEDYMD   (IDY)
                       MOVE    SPA-GMN24-TSYLSKJGNSTYMD (IDX)
                                           TO  LNK-TSY-SKJGNSTYMD
                                                                 (IDY)
                       MOVE    SPA-TERMID      TO  LNK-TSY-TERMID (IDY)
                       MOVE    SPA-SYSYMD      TO  LNK-TSY-CREYMD (IDY)
      *                食事標準負担額減額開始日
                       MOVE    SPA-NAI24-TSYLNINYMD (IDX)
                                               TO  LNK-TSY-SKJNINSTYMD
                                                                  (IDY)
      *                保険適用区分
                       MOVE    SPA-NAI24-TSYLHKNKBN (IDX)
                                               TO  LNK-TSY-HKNTEKKBN
                                                                  (IDY)
      *                境界層該当区分
                       MOVE    SPA-NAI24-TSYLKYOUKAI (IDX)
                                               TO  LNK-TSY-KYOUKAISOUKBN
                                                                  (IDY)
                       PERFORM 510-PARA-TSYRRK-OUT-SEC
                   END-IF
               END-IF
               IF      SPA-GMN24-TSYDELKBN (IDX)   =   SPACE
                   IF     (SPA-GMN24-TSYDPSTYMD (IDX)  <=  SPA-SYSYMD)
                     AND  (SPA-GMN24-TSYDPEDYMD (IDX)  >=  SPA-SYSYMD)
                       MOVE    1               TO  SPA-NAI-TSYRRK
                   END-IF
               END-IF
           END-PERFORM
           .
      *
       500-SPA-PARA-TSYRRK-EXT.
           EXIT.
           .
      *****************************************************************
      *    低所得者画面の内容をパラメタへ保存
      *****************************************************************
       510-PARA-TSYRRK-OUT-SEC         SECTION.
      *
      *    パラメタ登録
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "TSYRRK"            TO  PARA-FILEMEI
           MOVE    IDY                 TO  PARA-EDANUM
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-PARA            =   1
               INITIALIZE                      PARA-REC
               MOVE    "P02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "TSYRRK"            TO  PARA-FILEMEI
               MOVE    IDY                 TO  PARA-EDANUM
      *
               MOVE    LNK-TSY-REC(IDY)    TO  PARA-DATA-REC
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA INSERT ERR:"  MCP-RC
                           ",KEY:" PARA-KEY(1:42)
               END-IF
           ELSE
               MOVE    LNK-TSY-REC(IDY)    TO  PARA-DATA-REC
      *
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA UPDATE ERR:"  MCP-RC
                           ",KEY:" PARA-KEY(1:42)
               END-IF
           END-IF
      *
           .
       510-PARA-TSYRRK-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    年金画面の内容をパラメタへ保存
      *****************************************************************
       500-SPA-PARA-TNKRRK-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  SPA-NAI-TNKRRK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX         >   20          )
                   OR        ( IDX         >   SPA-TNK-MAX )
      *********MOVE    SPACE               TO  LNK-TNK-KEY-O(IDX)
      *        初期表示時の所得レコード数以内の場合
               IF  IDX                     <=  WRK-TNK-MAX
      *            削除データの場合
                   IF  SPA-GMN24-TNKDELKBN(IDX)   =   "1"
                       MOVE    ALL "9"     TO  LNK-TNK-UPYMD   (IDX)
                   ELSE
      ************     変更前キー転送
      *                MOVE    LNK-TNK-HOSPNUM (IDX)
      *                                    TO  LNK-TNK-HOSPNUM-O(IDX)
      *                MOVE    LNK-TNK-PTID (IDX)
      *                                    TO  LNK-TNK-PTID-O  (IDX)
      *                MOVE    LNK-TNK-STYMD (IDX)
      ************                         TO  LNK-TNK-STYMD-O (IDX)
      *                認定期間変更時、変更年月日・更新年月日転送
                       IF  (SPA-GMN24-TNKDPSTYMD(IDX)
                                   NOT =   LNK-TNK-STYMD(IDX))  OR
                           (SPA-GMN24-TNKDPEDYMD(IDX)
                                   NOT =   LNK-TNK-EDYMD(IDX))
                       OR  ( SPA-NAI24-TNKLNINYMD(IDX)
                                   NOT =   LNK-TNK-SKJNINSTYMD(IDX))
                       OR  ( SPA-NAI24-TNKLHKNKBN(IDX)
                                   NOT =   LNK-TNK-HKNTEKKBN(IDX))
                       OR  ( SPA-NAI24-TNKLKYOUKAI(IDX)
                                   NOT =   LNK-TNK-KYOUKAISOUKBN(IDX))
                         MOVE    SPA-SYSYMD  TO  LNK-TNK-UPYMD (IDX)
                         MOVE    SPA-GMN24-TNKDPSTYMD (IDX)
                                           TO  LNK-TNK-STYMD   (IDX)
                         MOVE    SPA-GMN24-TNKDPEDYMD (IDX)
                                           TO  LNK-TNK-EDYMD   (IDX)
      *
                         MOVE    SPA-NAI24-TNKLNINYMD (IDX)
                                           TO  LNK-TNK-SKJNINSTYMD(IDX)
                         MOVE    SPA-NAI24-TNKLHKNKBN (IDX)
                                           TO  LNK-TNK-HKNTEKKBN  (IDX)
      *                  境界層該当区分
                         MOVE    SPA-NAI24-TNKLKYOUKAI (IDX)
                                           TO  LNK-TNK-KYOUKAISOUKBN
                                                                (IDX)
      *
                       END-IF
      *                高齢者福祉
                       IF  (SPA-GMN24-TFUKUSHI-KBN(IDX)
                                   NOT =   LNK-TNK-RRI-FUKUSHI-KBN(IDX))
                           MOVE    SPA-GMN24-TFUKUSHI-KBN(IDX)
                                       TO  LNK-TNK-RRI-FUKUSHI-KBN(IDX)
                           MOVE    SPA-SYSYMD  TO  LNK-TNK-UPYMD (IDX)
                       END-IF
                   END-IF
                   MOVE    IDX             TO  IDY
                   PERFORM 510-PARA-TNKRRK-OUT-SEC
               ELSE
      *        初期表示時の所得レコード数を超える場合
                   IF  SPA-GMN24-TNKDELKBN (IDX)   =   SPACE
                       ADD     1               TO  IDY
      *
                       INITIALIZE                  LNK-TNK-REC (IDY)
                       MOVE    SPA-HOSPNUM     TO  LNK-TNK-HOSPNUM (IDY)
                       MOVE    SPA-GMN-PTID    TO  LNK-TNK-PTID   (IDY)
                       MOVE    SPA-GMN24-TNKDPSTYMD (IDX)
                                               TO  LNK-TNK-STYMD  (IDY)
                       MOVE    SPA-GMN24-TNKDPEDYMD (IDX)
                                               TO  LNK-TNK-EDYMD  (IDY)
                       MOVE    SPA-GMN24-TFUKUSHI-KBN(IDX)
                                           TO  LNK-TNK-RRI-FUKUSHI-KBN
                                                                  (IDY)
                       MOVE    SPA-TERMID      TO  LNK-TNK-TERMID (IDY)
                       MOVE    SPA-SYSYMD      TO  LNK-TNK-CREYMD (IDY)
      *                食事標準負担額減額開始日
                       MOVE    SPA-NAI24-TNKLNINYMD (IDX)
                                               TO  LNK-TNK-SKJNINSTYMD
                                                                  (IDY)
      *                保険適用区分
                       MOVE    SPA-NAI24-TNKLHKNKBN (IDX)
                                               TO  LNK-TNK-HKNTEKKBN
                                                                  (IDY)
      *                境界層該当区分
                       MOVE    SPA-NAI24-TNKLKYOUKAI (IDX)
                                           TO  LNK-TNK-KYOUKAISOUKBN
                                                                (IDY)
                       PERFORM 510-PARA-TNKRRK-OUT-SEC
                   END-IF
               END-IF
               IF      SPA-GMN24-TNKDELKBN (IDX)   =   SPACE
                   IF      (SPA-GMN24-TNKDPSTYMD (IDX) <=  SPA-SYSYMD)
                      AND  (SPA-GMN24-TNKDPEDYMD (IDX) >=  SPA-SYSYMD)
                       MOVE    1                   TO  SPA-NAI-TNKRRK
                   END-IF
               END-IF
           END-PERFORM
           .
      *
       500-SPA-PARA-TNKRRK-EXT.
           EXIT.
           .
      *****************************************************************
      *    低所得者画面の内容をパラメタへ保存
      *****************************************************************
       510-PARA-TNKRRK-OUT-SEC         SECTION.
      *
      *    パラメタ登録
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "TNKRRK"            TO  PARA-FILEMEI
           MOVE    IDY                 TO  PARA-EDANUM
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-PARA            =   1
               INITIALIZE                      PARA-REC
               MOVE    "P02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "TNKRRK"            TO  PARA-FILEMEI
               MOVE    IDY                 TO  PARA-EDANUM
      *
               MOVE    LNK-TNK-REC(IDY)    TO  PARA-DATA-REC
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA INSERT ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY(1:42)
               END-IF
           ELSE
               MOVE    LNK-TNK-REC(IDY)    TO  PARA-DATA-REC
      *
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA UPDATE ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY(1:42)
               END-IF
           END-IF
      *
      *
           .
       510-PARA-TNKRRK-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担の登録チェック処理
      *****************************************************************
       410132-KOHFTN-CHK-SEC          SECTION.
      *
      *    選択がない時、公費負担テーブル編集
           IF     (SPA-FTN1-MAX        >   ZERO )  AND
                  (SPA-GMN24-FTNSELNUM =   ZERO )
               PERFORM 4101321-KOHFTN-TBL-SEC
           END-IF
      * 
           MOVE    1                   TO  SPA-P024-KOHKEIFLG
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   SPA-FTN1-MAX  )
                         OR   (SPA-ERRCD   NOT =   SPACE )
             MOVE    ZERO              TO  FLG-CHKNO
      *      上限額が必須でないもの(結３５、生保、精神入院、残留邦人）
             IF     (SPA-NAI24-KOHNUM (IDX2)   =   "011"
                                               OR  "012"
                                               OR  "020"
                                               OR  "025" )
      *H28.6
                OR  (SPA-NAI24-KOHNUM (IDX2)   =   "019" )
      *H30.11   肝がん治療
                OR  (SPA-NAI24-KOHNUM (IDX2)   =   "092" )
               MOVE    1                   TO  FLG-CHKNO
             END-IF
      *H27.7
             IF     (SPA-NAI24-KOHNUM    (IDX2)   =   "964"     )
               AND  (SPA-NAI24-TKOHSTYMD (IDX2)   >=  "20150101")
               EVALUATE    SPA-NAI24-TJKYSNUM (IDX2)(1:2)
                   WHEN    "ア"
                   WHEN    "イ"
                   WHEN    "ウ"
                   WHEN    "エ"
                   WHEN    "オ"
                       MOVE    1                   TO  FLG-CHKNO
               END-EVALUATE
             END-IF
      *
             IF      FLG-CHKNO         =   ZERO
      *        当日有効な公費のみ判定
               IF     (SPA-NAI24-TKOHSTYMD (IDX2)  <=  SPA-SYSYMD )
                 AND  (SPA-NAI24-TKOHEDYMD (IDX2)  >=  SPA-SYSYMD )
                 AND  (SPA-NAI24-FTNDELKBN (IDX2)  =   SPACE      )
                   MOVE    ZERO                TO  FLG-OK
                   PERFORM VARYING IDX         FROM    1   BY  1
                       UNTIL  (IDX         >   TBL-MAX-KOH  )  OR
                              (HZN-PTKOHFTN-REC (IDX 1)
                                           =   SPACE  )  OR
                              (FLG-OK      =   1      )
                       MOVE    HZN-PTKOHFTN-REC (IDX 1)
                                               TO  PTKOHFTN-REC
                       IF      PTKOHFTN-KOHID  =   SPA-NAI24-KOHID(IDX2)
                           MOVE    1                   TO  FLG-OK
                      END-IF
                   END-PERFORM
                   IF      FLG-OK              =   ZERO
                       MOVE    "K240"          TO  SPA-ERRCD
                       MOVE    30              TO  SPA-GMN24-CUR
                   END-IF
               END-IF
             END-IF
           END-PERFORM
      *
           .
       410132-KOHFTN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担テーブル編集処理
      *****************************************************************
       4101321-KOHFTN-TBL-SEC          SECTION.
      *
      *    公費負担更新保存
           MOVE    SPACE               TO  HZN-PTKOHFTN-AREA
           INITIALIZE                  HZN-PTKOHFTN-AREA
           MOVE    ZERO                TO  IDP-1
           MOVE    ZERO                TO  IDP-2
      *
      *    対象のデータをパラメタファイルより編集する
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTKOHFTN"          TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  WRK-PARA-EDANUM
           MOVE    ZERO                TO  WRK-KOHID
           MOVE    ZERO                TO  WRK-IDX
           PERFORM
               UNTIL  (FLG-PARA            =   1  )  OR
                      (SPA-ERRCD       NOT =   SPACE )
               MOVE    PARA-DATA-REC           TO  PTKOHFTN-REC
      *        公費ＩＤがテーブルにあること（ないものは削除とする）
               MOVE    ZERO            TO  FLG-OK
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2    >   SPA-FTN1-MAX
                   IF      PTKOHFTN-KOHID      =   SPA-NAI24-KOHID(IDX2)
                       IF      SPA-NAI24-FTNDELKBN (IDX2)  =   SPACE
                           MOVE    1               TO  FLG-OK
                       ELSE
                           MOVE    ZERO            TO  FLG-OK
                       END-IF
                       MOVE    SPA-FTN1-MAX        TO  IDX2
                   END-IF
               END-PERFORM
      *
               IF      FLG-OK              =   1
                   PERFORM 4101313-KOHFTN-TBL-HEN-SEC
               END-IF
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4101321-KOHFTN-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    低所得　選択番号選択
               WHEN    ANY             ALSO    "P024_TSYLIST"
                   PERFORM 41012-TSYLIST-SEC
      *    低年金　選択番号選択
               WHEN    ANY             ALSO    "P024_TNKLIST"
                   PERFORM 41012-TNKLIST-SEC
      *    公費負担額番号選択
               WHEN    "ACTIVATE"      ALSO    "P024_FTNSELNUM"
                   PERFORM 41013-FTNSELNUM-SEC
                   GO      TO      200-ENTRY-EXT
      *    公費負担額番号選択
               WHEN    "ACTIVATE"      ALSO    "P024_FTNSELNUM2"
                   PERFORM 41014-FTNSELNUM2-SEC
                   GO      TO      200-ENTRY-EXT
      *    公費負担額番号選択
               WHEN    ANY             ALSO    "P024_PTKOHFTN"
                   PERFORM 41015-KOHFTNLIST-SEC
                   GO      TO      200-ENTRY-EXT
      *    公費負担額番号選択
               WHEN    ANY             ALSO    "P024_PTKOHFTN2"
                   PERFORM 41016-KOHFTNLIST2-SEC
                   GO      TO      200-ENTRY-EXT
           END-EVALUATE
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN24-CUR
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得者選択
      *****************************************************************
       41012-TSYLIST-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX       >   SPA-TSY-MAX
               IF      P02-P024-TSYLIST-SELECT (IDX)  =   "T"
                   IF      IDX               =   SPA-NAI24-TSYSELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX               TO  WRK-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-NAI24-TSYSELNUM
           END-IF
      *
           IF      SPA-NAI24-TSYSELNUM NOT =   ZERO
               MOVE    SPA-NAI24-TSYSELNUM TO   WRK-RENNUMZ
               MOVE    WRK-RENNUMZ         TO   P02-P024-TSRSELNUM
               MOVE    2                   TO  SPA-GMN24-CUR
           END-IF
      *
           .
       41012-TSYLIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    年金／選択番号選択
      *****************************************************************
       41012-TNKLIST-SEC           SECTION.
      *
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX       >   SPA-TNK-MAX
               IF      P02-P024-TNKLIST-SELECT (IDX)  =   "T"
                   IF      IDX               =   SPA-NAI24-TNKSELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX               TO  WRK-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-NAI24-TNKSELNUM
           END-IF
      *
           IF      SPA-NAI24-TNKSELNUM   NOT =   ZERO
               MOVE     SPA-NAI24-TNKSELNUM    TO   WRK-RENNUMZ
               MOVE     WRK-RENNUMZ        TO  P02-P024-TNKSELNUM
               MOVE    11                  TO  SPA-GMN24-CUR
           END-IF
      *
           .
       41012-TNKLIST-EXT.
           EXIT.
      *****************************************************************
      *    公費負担／公費選択番号選択
      *****************************************************************
       41015-KOHFTNLIST-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX       >   SPA-FTN1-MAX
               IF      P02-P024-HFTNLST-SELECT (IDX)  =   "T"
                   IF      IDX           =   SPA-GMN24-FTNSELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX               TO  WRK-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-GMN24-FTNSELNUM
           END-IF
      *
           IF      SPA-GMN24-FTNSELNUM     NOT =   ZERO
               MOVE    SPA-GMN24-FTNSELNUM     TO  P02-P024-FTNSELNUM
               PERFORM 41013-FTNSELNUM-SEC
           END-IF
      *
           .
       41015-KOHFTNLIST-EXT.
           EXIT.
      *****************************************************************
      *    公費負担／公費選択番号選択
      *****************************************************************
       41013-FTNSELNUM-SEC           SECTION.
      *
           MOVE    P02-P024-FTNSELNUM  TO  SPA-GMN24-FTNSELNUM
      *    変更がある時、現在の更新分をパラメタへ
           IF     (SPA-GMN24-FTNSELNUM NOT =   SPA-MAE24-FTNSELNUM )
      *      OR  ((SPA-MAE24-FTNSELNUM NOT =   ZERO    )  AND
      *           (SPA-GMN24-FTNSELNUM     =   SPA-MAE24-FTNSELNUM))
      *        前回が未選択の時、更新なし
      *        IF     (SPA-MAE24-FTNSELNUM     =   ZERO
      *                                        OR  SPACE )
      *            CONTINUE
      *        ELSE
               IF      SPA-NAI24-FTNSELNUM NOT =   ZERO
                   PERFORM 410131-KOHFTN-PARA-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       GO      TO      41013-FTNSELNUM-EXT
                   END-IF
               END-IF
           END-IF
      *
           INITIALIZE                  SPA-GMN24-KOHFTNTBL2
           INITIALIZE                  SPA-GMN24-FTN-AREA2
           MOVE    1                   TO  SPA-NAI24-INIT2
           MOVE    SPACE               TO  SPA-GMN24-JKYSNUM
           MOVE    SPACE               TO  SPA-GMN24-FTNMSG1
           MOVE    SPACE               TO  SPA-GMN24-FTNMSG2
           MOVE    ZERO                TO  SPA-NAI24-FTNSYORI
      *
           MOVE    ZERO                TO  WRK-FTN1-IDX
           IF      SPA-GMN24-FTNSELNUM =   ZERO
               MOVE    ZERO            TO  SPA-NAI24-FTNSELNUM
           ELSE
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   SPA-FTN1-MAX
                   IF      SPA-GMN24-FTNSELNUM =   SPA-GMN24-TGMNNUM
                                                                (IDX)
                       MOVE    IDX             TO  WRK-FTN1-IDX
                       MOVE    IDX             TO  SPA-NAI24-FTNSELNUM
                       MOVE    SPA-FTN1-MAX    TO  IDX
                   END-IF
               END-PERFORM
           END-IF
           IF      WRK-FTN1-IDX        =   ZERO
               IF      SPA-GMN24-FTNSELNUM     =   ZERO
                   MOVE    SPA-GMN24-FTNSELNUM TO  SPA-MAE24-FTNSELNUM
               ELSE
                   MOVE    "2430"              TO  SPA-ERRCD
               END-IF
               MOVE    ZERO                TO  SPA-NAI24-FTNSELNUM
               MOVE    30                  TO  SPA-GMN24-CUR
               GO      TO                  41013-FTNSELNUM-EXT
           END-IF
           MOVE    SPA-GMN24-FTNSELNUM TO  SPA-MAE24-FTNSELNUM
      *
      *    見出し変更
           PERFORM 3101-KOH-MEISAI-SEC
      *
      *    対象のデータをパラメタファイルより編集する
           PERFORM 31022-KOHFTN-SPAHENSYU-SEC
      *
      *    １件もないとき、公費の内容を初期表示
           IF      SPA-FTN2-MAX        =   ZERO
               MOVE    SPA-GMN24-TKOHSTYMD (WRK-FTN1-IDX)
                                          TO  SPA-GMN24-FTNSTYMD
               MOVE    SPA-NAI24-TKOHSTYMD (WRK-FTN1-IDX)
                                          TO  SPA-NAI24-FTNSTYMD
               MOVE    SPA-GMN24-TKOHEDYMD (WRK-FTN1-IDX)
                                          TO  SPA-GMN24-FTNEDYMD
               MOVE    SPA-NAI24-TKOHEDYMD (WRK-FTN1-IDX)
                                          TO  SPA-NAI24-FTNEDYMD
               MOVE    ZERO               TO  SPA-NAI24-GAIJGNGAK
               MOVE    ZERO               TO  SPA-NAI24-NYUJGNGAK
               MOVE    SPACE              TO  SPA-GMN24-GAIJGNGAK
               MOVE    SPACE              TO  SPA-GMN24-NYUJGNGAK
               MOVE    32                 TO  SPA-GMN24-CUR
           ELSE
               MOVE    31                 TO  SPA-GMN24-CUR
           END-IF
           .
       41013-FTNSELNUM-EXT.
           EXIT.
      *****************************************************************
      *    公費負担パラメタ変更処理
      *****************************************************************
       410131-KOHFTN-PARA-SEC           SECTION.
      *
      *    公費負担更新保存
           MOVE    SPACE               TO  HZN-PTKOHFTN-AREA
           INITIALIZE                  HZN-PTKOHFTN-AREA
           MOVE    ZERO                TO  IDP-1
           MOVE    ZERO                TO  IDP-2
      *
      *    対象のデータをパラメタファイルより編集する
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTKOHFTN"          TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  WRK-PARA-EDANUM
           MOVE    ZERO                TO  WRK-KOHID
           MOVE    ZERO                TO  WRK-IDX
           PERFORM
               UNTIL  (FLG-PARA            =   1  )  OR
                      (SPA-ERRCD       NOT =   SPACE )
               MOVE    PARA-DATA-REC           TO  PTKOHFTN-REC
      *        公費ＩＤがテーブルにあること（ないものは削除とする）
               MOVE    ZERO            TO  FLG-OK
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2    >   SPA-FTN1-MAX
                   IF      PTKOHFTN-KOHID      =   SPA-NAI24-KOHID(IDX2)
                       IF      SPA-NAI24-FTNDELKBN (IDX2)  =   SPACE
                           MOVE    1               TO  FLG-OK
                       ELSE
                           MOVE    ZERO            TO  FLG-OK
                       END-IF
                       MOVE    SPA-FTN1-MAX        TO  IDX2
                   END-IF
               END-PERFORM
      *
               IF      FLG-OK              =   1
      *            今回選択分以外
                   IF     (SPA-NAI24-FTNSELNUM >   ZERO   )  AND
                          (PTKOHFTN-KOHID      =   SPA-NAI24-KOHID
                                                 (SPA-NAI24-FTNSELNUM))
                       CONTINUE
                   ELSE
                       PERFORM 4101313-KOHFTN-TBL-HEN-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    現在の更新分をパラメタへ
           IF      SPA-FTN2-MAX        =   ZERO
      *        パラメタ更新
               PERFORM 410132-KOHFTN-PARA-INS-SEC
               GO      TO      410131-KOHFTN-PARA-EXT
           END-IF
      *
           ADD     1                   TO  IDP-1
           MOVE    ZERO                TO  IDP-2
           IF      IDP-1               >   TBL-MAX-KOH
               DISPLAY "GP024 KOHFTN PARA MAX  OVER"
               MOVE    "2499"          TO  SPA-ERRCD
           END-IF
      *
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL  (IDX         >   SPA-FTN2-MAX )  OR
                          (SPA-ERRCD   NOT =   SPACE    )
      *
               IF      SPA-NAI24-FTNDELKBN2 (IDX)  =   SPACE
                   PERFORM 410131-KOHFTN-TBL-INS-SEC
               END-IF
           END-PERFORM
      *
      *    パラメタ追加
           PERFORM 410132-KOHFTN-PARA-INS-SEC
           .
       410131-KOHFTN-PARA-EXT.
           EXIT.
      *****************************************************************
      *    公費負担テーブル追加処理
      *****************************************************************
       4101313-KOHFTN-TBL-HEN-SEC           SECTION.
      *
      *    公費ＩＤ毎にテーブル保存
           IF      PTKOHFTN-KOHID  NOT =   WRK-KOHID
               ADD     1                   TO  IDP-1
               IF      IDP-1               >   TBL-MAX-KOH
                   MOVE    TBL-MAX-KOH         TO  IDP-1
                   MOVE    "2499"              TO  SPA-ERRCD
               END-IF
               MOVE    PTKOHFTN-KOHID      TO  WRK-KOHID
               MOVE    ZERO                TO  IDP-2
           END-IF
           ADD     1                   TO  IDP-2
           IF      IDP-2               <=  MAX-KOHFTN
               MOVE    PARA-DATA-REC   TO  HZN-PTKOHFTN-REC
                                                        (IDP-1 IDP-2)
           END-IF
           .
       4101313-KOHFTN-TBL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    公費負担テーブル追加処理
      *****************************************************************
       410131-KOHFTN-TBL-INS-SEC           SECTION.
      *
      *    公費負担編集
           INITIALIZE                      PTKOHFTN-REC
           MOVE    SPA-HOSPNUM         TO  PTKOHFTN-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTKOHFTN-PTID
           MOVE    SPA-NAI24-KOHID  (SPA-NAI24-FTNSELNUM)
                                       TO  PTKOHFTN-KOHID
      *    連番を再番
           ADD     1                   TO  IDP-2
           MOVE    IDP-2               TO  PTKOHFTN-RENNUM
      *    適用開始年月日
           MOVE    SPA-NAI24-TFTNSTYMD(IDX)
                                       TO  PTKOHFTN-TEKSTYMD
           MOVE    SPA-NAI24-TFTNEDYMD(IDX)
                                       TO  PTKOHFTN-TEKEDYMD
           MOVE    SPA-NAI24-KOHNUM  (SPA-NAI24-FTNSELNUM)
                                       TO  PTKOHFTN-KOHNUM
           MOVE    SPA-NAI24-PAYKBN  (SPA-NAI24-FTNSELNUM)
                                       TO  PTKOHFTN-PAYKBN
           MOVE    SPA-GMN24-TGAIJGNGAK(IDX)
                                       TO  PTKOHFTN-GAIJGNGAK
           MOVE    SPA-GMN24-TNYUJGNGAK(IDX)
                                       TO  PTKOHFTN-NYUJGNGAK
      *
           MOVE    SPA-NAI24-TFTNETCKBN (IDX)
                                       TO  PTKOHFTN-TERMID(1:1)
      *
           MOVE    PTKOHFTN-REC        TO  HZN-PTKOHFTN-REC
                                                        (IDP-1 IDP-2)
           .
       410131-KOHFTN-TBL-INS-EXT.
           EXIT.
      *****************************************************************
      *    公費負担パラメタ追加処理
      *****************************************************************
       410132-KOHFTN-PARA-INS-SEC           SECTION.
      *
      *    削除する
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTKOHFTN"          TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    ZERO                TO  WRK-PARA-EDANUM
      *
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL   IDX         >   IDP-1
               PERFORM VARYING IDY         FROM    1   BY  1
                       UNTIL  (IDY         >   MAX-KOHFTN )  OR
                              (HZN-PTKOHFTN-REC(IDX IDY) =   SPACE)
                   ADD     1                   TO  WRK-PARA-EDANUM
                   MOVE    SPACE               TO  PARA-REC
                   INITIALIZE                      PARA-REC
                   MOVE    "P02"               TO  PARA-GYOUMUID
                   MOVE    SPA-TERMID          TO  PARA-TERMID
                   MOVE    "PTKOHFTN"          TO  PARA-FILEMEI
                   MOVE    WRK-PARA-EDANUM     TO  PARA-EDANUM
                   MOVE    HZN-PTKOHFTN-REC(IDX IDY)
                                               TO  PARA-DATA-REC
                   PERFORM 400-PARA-INSERT-SEC
      *
               END-PERFORM
           END-PERFORM
      *
           .
       410132-KOHFTN-PARA-INS-EXT.
           EXIT.
      *****************************************************************
      *    患者公費負担額パラメタ変更処理
      *****************************************************************
       410132-KOHFTNETC-PARA-SEC           SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTKOHFTNETC"       TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           PERFORM
               UNTIL   FLG-PARA            =   1
               MOVE    PARA-DATA-REC       TO  PTKOHFTNETC-REC
      *        公費ＩＤがテーブルにあること（ないものは削除とする）
               MOVE    ZERO            TO  FLG-OK
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2    >   SPA-FTN1-MAX
                   IF      PTKOHFTNETC-KOHID   =   SPA-NAI24-KOHID(IDX2)
                       IF      SPA-NAI24-FTNDELKBN (IDX2)  =   SPACE
                           MOVE    1               TO  FLG-OK
                       ELSE
                           MOVE    ZERO            TO  FLG-OK
                       END-IF
                       MOVE    SPA-FTN1-MAX        TO  IDX2
                   END-IF
               END-PERFORM
      *
               IF      FLG-OK              =   ZERO
      *            削除
                   MOVE    PARA-REC            TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_para"          TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                                   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               END-IF
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   SPA-FTN1-MAX
               IF     (SPA-NAI24-KOHNUM (IDX2)
                                           =   "015"
                                           OR  "016"
                                           OR  "021"
                                           OR  "038"    )
      *H26.12
      *          難病・小児特定（H26.1から）
                 OR  ((SPA-NAI24-KOHNUM  (IDX2)
                                           =   "052"
                                           OR  "054"   )  AND
                      (SPA-NAI24-TKOHSTYMD (IDX2)
                                           >=  "20150101" ))
      ***        OR  ((SPA-NAI24-KOHNUM (IDX2)
      *                                    =   "281" ) AND
      **              (SPA-PREFNUM         =    07   ))
      *          地方公費の対象
                 OR   (SPA-NAI24-TJYOGENKBN (IDX2)
                                           =   "2"    )
      *            今回削除分
                   IF      SPA-NAI24-FTNDELKBN (IDX2)  =   "1" 
      *                更新対象編集
                       PERFORM VARYING     IDX3    FROM    1   BY   1
                               UNTIL      (IDX3    >   MAX-KOHFTN  )
                           IF      SPA-NAI24-KOHFTN-ID (IDX3)  =   ZERO
                               MOVE    SPA-NAI24-KOHID(IDX2)
                                               TO  SPA-NAI24-KOHFTN-ID
                                                                (IDX3)
                           END-IF
                           IF     (SPA-NAI24-KOHID(IDX2)
                                                =   SPA-NAI24-KOHFTN-ID
                                                                (IDX3))
                               MOVE    SPACE    TO  SPA-NAI24-KOHFTN-YM
                                                                (IDX3)
                               MOVE    MAX-KOHFTN      TO  IDX3
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       410132-KOHFTNETC-PARA-EXT.
           EXIT.
      *****************************************************************
      *    公費負担／公費選択番号選択２
      *****************************************************************
       41016-KOHFTNLIST2-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX       >   SPA-FTN2-MAX
               IF      P02-P024-HFTNLST2-SELECT (IDX)  =   "T"
                   IF      IDX           =   SPA-GMN24-FTNSELNUM2
                       CONTINUE
                   ELSE
                       MOVE    IDX               TO  WRK-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-GMN24-FTNSELNUM2
           END-IF
      *
           IF      SPA-GMN24-FTNSELNUM2    NOT =   ZERO
               MOVE    SPA-GMN24-FTNSELNUM2    TO  P02-P024-FTNSELNUM2
               PERFORM 41014-FTNSELNUM2-SEC
               MOVE    31                  TO  SPA-GMN24-CUR
           END-IF
      *
           .
       41016-KOHFTNLIST2-EXT.
           EXIT.
      *****************************************************************
      *    公費負担／公費選択番号選択
      *****************************************************************
       41014-FTNSELNUM2-SEC           SECTION.
      *
           IF      P02-P024-FTNSELNUM2     =   SPA-GMN24-FTNSELNUM2
               IF      SPA-GMN24-FTNSELNUM2    =   ZERO
                   GO     TO      41014-FTNSELNUM2-EXT
               END-IF
           END-IF
      *
           MOVE    P02-P024-FTNSELNUM2     TO  SPA-GMN24-FTNSELNUM2
      *
           MOVE    ZERO                TO  WRK-FTN1-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-FTN2-MAX
               IF      SPA-GMN24-FTNSELNUM2    =   SPA-GMN24-TGMNNUM2
                                                                (IDX)
                   MOVE    IDX             TO  WRK-FTN1-IDX
                   MOVE    IDX             TO  SPA-NAI24-FTNSELNUM2
                   MOVE    SPA-FTN2-MAX    TO  IDX
               END-IF
           END-PERFORM
           IF      WRK-FTN1-IDX        =   ZERO
               IF      SPA-GMN24-FTNSELNUM2    NOT =   ZERO
                   MOVE    "2431"              TO  SPA-ERRCD
               END-IF
               MOVE    31                  TO  SPA-GMN24-CUR
               INITIALIZE                  SPA-GMN24-FTN-AREA2
               MOVE    P02-P024-FTNSELNUM2     TO  SPA-GMN24-FTNSELNUM2
           ELSE
      *
      *    対象のデータを編集する
           MOVE    WRK-FTN1-IDX        TO  IDX
           MOVE    SPA-NAI24-TFTNSTYMD(IDX)    TO  SPA-NAI24-FTNSTYMD
           MOVE    SPA-NAI24-TFTNEDYMD(IDX)    TO  SPA-NAI24-FTNEDYMD
           MOVE    SPA-GMN24-TGAIJGNGAK(IDX)   TO  SPA-NAI24-GAIJGNGAK
           MOVE    SPA-GMN24-TNYUJGNGAK(IDX)   TO  SPA-NAI24-NYUJGNGAK
           MOVE    SPA-NAI24-TFTNETCKBN(IDX)   TO  SPA-NAI24-FTNETCKBN
      *
           MOVE    SPA-GMN24-TFTNSTYMD(IDX)    TO  SPA-GMN24-FTNSTYMD
           MOVE    SPA-GMN24-TFTNEDYMD(IDX)    TO  SPA-GMN24-FTNEDYMD
           MOVE    SPA-NAI24-GAIJGNGAK         TO  WRK-Z7
           MOVE    WRK-Z7                      TO  SPA-GMN24-GAIJGNGAK
           MOVE    SPA-NAI24-NYUJGNGAK         TO  WRK-Z7
           MOVE    WRK-Z7                      TO  SPA-GMN24-NYUJGNGAK
      *
           MOVE    32                  TO  SPA-GMN24-CUR
      *
           END-IF
           .
       41014-FTNSELNUM2-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
      *    登録時、必須入力チェック
           IF      FLG-TOUROKU     NOT =   ZERO
               PERFORM 4109-TOUROKU-CHK-SEC
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
      *    低所得　選択番号
           IF      P02-P024-TSRSELNUM  =   SPA-MAE24-TSYSELNUM
               MOVE    ZERO                TO  FLG-IN-TSYSELNUM
           ELSE
               MOVE    1                   TO  FLG-IN-TSYSELNUM
           END-IF
           MOVE    P02-P024-TSRSELNUM  TO  SPA-GMN24-TSYSELNUM
           MOVE    P02-P024-TSYSTYMD   TO  SPA-GMN24-TSYSTYMDW
           MOVE    P02-P024-TSYEDYMD   TO  SPA-GMN24-TSYEDYMDW
           MOVE    P02-P024-TSYSKJGNSTYMD  TO  SPA-GMN24-TSYSKJGNSTYMDW
           MOVE    P02-P024-TSYNINSTYMD    TO  SPA-GMN24-TSYNINSTYMD
           MOVE    P02-P024-TSYTEKK        TO  SPA-GMN24-TSYTEKKBN-G
           MOVE    P02-P024-TSYKYOUKAI     TO  SPA-GMN24-TSYKYOUKAI-G
      *
      *    低年金　選択番号
           IF      P02-P024-TNKSELNUM  =   SPA-MAE24-TNKSELNUM
               MOVE    ZERO                TO  FLG-IN-TNKSELNUM
           ELSE
               MOVE    1                   TO  FLG-IN-TNKSELNUM
           END-IF
           MOVE    P02-P024-TNKSELNUM  TO  SPA-GMN24-TNKSELNUM
           MOVE    P02-P024-TNKSTYMD   TO  SPA-GMN24-TNKSTYMDW
           MOVE    P02-P024-TNKEDYMD   TO  SPA-GMN24-TNKEDYMDW
           MOVE    P02-P024-FUKUSHI    TO  SPA-GMN24-FUKUSHI-G
           MOVE    P02-P024-TNKNINSTYMD    TO  SPA-GMN24-TNKNINSTYMD
           MOVE    P02-P024-TNKTEKK    TO  SPA-GMN24-TNKTEKKBN-G
           MOVE    P02-P024-TNKKYOUKAI TO  SPA-GMN24-TNKKYOUKAI-G
      *
      *    公費負担金　選択番号
      *****MOVE    P02-P024-FTNSELNUM2 TO  SPA-GMN24-FTNSELNUM2
           MOVE    P02-P024-FTNTEKSTYMD    TO  SPA-GMN24-FTNSTYMD
           MOVE    P02-P024-FTNTEKEDYMD    TO  SPA-GMN24-FTNEDYMD
           MOVE    P02-P024-GAIJGNGAK  TO  SPA-GMN24-GAIJGNGAK
           MOVE    P02-P024-NYUJGNGAK  TO  SPA-GMN24-NYUJGNGAK
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    低所得　選択番号
           PERFORM 41012-TSYSELNUM-SEC
      *    低所得開始年月日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-TSYSTYMD-SEC
           END-IF
      *    低所得終了年月日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-TSYEDYMD-SEC
           END-IF
      *    低所得認定範囲
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-TSYKEKKBN-SEC
           END-IF
      *    低所得減額開始日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-TSYNINYMD-SEC
           END-IF
      *    食事標準負担減額開始日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-TSYSKJGNSTYMD-SEC
           END-IF
      *H29.6
      *    境界層該当
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-TSYKYOUKAI-SEC
           END-IF
      *
      *    低年金　選択番号
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-NENINSELNUM-SEC
           END-IF
      *    低年金開始年月日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-TNKSTYMD-SEC
           END-IF
      *    低年金終了年月日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-TNKEDYMD-SEC
           END-IF
      *    低年金認定範囲
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-TNKKEKKBN-SEC
           END-IF
      *    低年金減額開始日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-TNKNINYMD-SEC
           END-IF
      *    低年金高齢福祉年金
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-FUKUSHI-SEC
           END-IF
      *H29.6
      *    境界層該当
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-TNKKYOUKAI-SEC
           END-IF
      *
      *    公費負担 開始日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41031-FTNSTYMD-SEC
           END-IF
      *    公費負担 終了日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41031-FTNEDYMD-SEC
           END-IF
      *    公費負担 外来上限額
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41031-GAIJGNGAK-SEC
           END-IF
      *    公費負担 入院上限額
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41032-NYUJGNGAK-SEC
           END-IF
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得者／選択番号 
      *****************************************************************
       41012-TSYSELNUM-SEC           SECTION.
      *
           IF      SPA-GMN24-TSYSELNUM =   SPACE
               MOVE    SPA-GMN24-TSYSELNUM TO  SPA-MAE24-TSYSELNUM
               MOVE    ZERO                TO  SPA-NAI24-TSYSELNUM
               MOVE    SPA-GMN24-TSYSELNUM     TO  SPA-MAE24-TSYSELNUM
               GO      TO               41012-TSYSELNUM-EXT
           END-IF
      *
           INITIALIZE                       ORCSNUMAREA
           MOVE    SPA-GMN24-TSYSELNUM  TO  SNUM-INX
           CALL    "ORCSNUM"                USING
                                            ORCSNUMAREA
           IF     (SNUM-RC          NOT =   ZERO  )   OR
                  (SNUM-MINKBN      NOT =   SPACE )   OR
                  (SNUM-SYOKBN      NOT =   SPACE )
               MOVE    "2401"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN24-CUR
               GO      TO                  41012-TSYSELNUM-EXT
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-NAI24-TSYSELNUM
               MOVE    SNUM-OUTNUM         TO  WRK-RENNUMZ
               MOVE    WRK-RENNUMZ         TO  SPA-GMN24-TSYSELNUM
           END-IF
      *
      **** MOVE    SPA-GMN24-TSYSELNUM     TO  SPA-MAE24-TSYSELNUM
           IF      FLG-IN-TSYSELNUM    =   ZERO
               GO      TO      41012-TSYSELNUM-EXT
           END-IF
      *
      *    削除データを除く内部インデックスを取得
           INITIALIZE                   WRK-NAIBU-IDX-AREA
           MOVE    SPA-NAI24-TSYSELNUM  TO  WRK-G-IDX
           MOVE    1                    TO  WRK-W-TBL
           PERFORM 4501-NAIBU-IDX-GET-SEC
           IF      WRK-N-IDX            =   ZERO
               MOVE    "2401"           TO  SPA-ERRCD
               MOVE    SPACE            TO  SPA-GMN24-TSYSTYMDW
               MOVE    SPACE            TO  SPA-GMN24-TSYSTYMD
               MOVE    SPACE            TO  SPA-GMN24-TSYEDYMDW
               MOVE    SPACE            TO  SPA-GMN24-TSYEDYMD
               MOVE    SPACE            TO  SPA-GMN24-TSYSKJGNSTYMDW
               MOVE    SPACE            TO  SPA-GMN24-TSYSKJGNSTYMD
               MOVE    SPACE            TO  SPA-GMN24-TSYNINSTYMD
               MOVE    SPACE            TO  SPA-GMN24-TSYTEKKBN-G
               MOVE    SPACE            TO  SPA-GMN24-TSYKYOUKAI-G
           ELSE
               MOVE    SPA-GMN24-TSYSELNUM     TO  SPA-MAE24-TSYSELNUM
      *
               MOVE  SPA-GMN24-TSYDPSTYMDW (WRK-N-IDX)
                                        TO  SPA-GMN24-TSYSTYMDW
               MOVE  SPA-GMN24-TSYDPEDYMDW (WRK-N-IDX)
                                        TO  SPA-GMN24-TSYEDYMDW
               MOVE  SPA-GMN24-TSYDPSTYMD  (WRK-N-IDX)
                                        TO  SPA-GMN24-TSYSTYMD
               MOVE  SPA-GMN24-TSYDPEDYMD  (WRK-N-IDX)
                                        TO  SPA-GMN24-TSYEDYMD
               MOVE  SPA-GMN24-TSYLSKJGNSTYMDW (WRK-N-IDX)
                                        TO  SPA-GMN24-TSYSKJGNSTYMDW
               MOVE  SPA-GMN24-TSYLSKJGNSTYMD (WRK-N-IDX)
                                        TO  SPA-GMN24-TSYSKJGNSTYMD
               MOVE  SPA-GMN24-TSYLNINYMD    (WRK-N-IDX)
                                        TO  SPA-GMN24-TSYNINSTYMD
               MOVE    SPACE            TO  SPA-GMN24-TSYTEKKBN-G
               MOVE    SPA-NAI24-TSYLHKNKBN (WRK-N-IDX)
                                        TO  SPA-GMN24-TSYTEKKBN
      *        境界層該当
               MOVE    SPACE            TO  SPA-GMN24-TSYKYOUKAI-G
               MOVE    SPA-NAI24-TSYLKYOUKAI (WRK-N-IDX)
                                        TO  SPA-GMN24-TSYKYOUKAI
               IF      SPA-GMN24-TSYKYOUKAI =  SPACE
                   MOVE    ZERO         TO  SPA-GMN24-TSYKYOUKAI
               END-IF
               PERFORM 41012-TSYKYOUKAI-SEC
           END-IF
           .
       41012-TSYSELNUM-EXT.
           EXIT.
      *****************************************************************
      *    低所得者／開始年月日 4121120--->H12.11.20
      *****************************************************************
       41012-TSYSTYMD-SEC           SECTION.
      *
           IF      SPA-GMN24-TSYSTYMDW =   SPACE
               MOVE    SPACE               TO  SPA-GMN24-TSYSTYMDW
               MOVE    SPACE               TO  SPA-GMN24-TSYSTYMD
               GO  TO                   41012-TSYSTYMD-EXT
           END-IF
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN24-TSYSTYMDW     TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN24-TSYSTYMDW
               MOVE    SGDAY-SDAY          TO  SPA-GMN24-TSYSTYMD
           ELSE
               MOVE    "2402"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN24-CUR
           END-IF
           .
       41012-TSYSTYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得者／終了年月日 4121120--->H12.11.20
      *****************************************************************
       41012-TSYEDYMD-SEC           SECTION.
      *
           IF    ( SPA-GMN24-TSYEDYMDW     =   SPACE )  AND
                 ( SPA-GMN24-TSYSTYMDW     =   SPACE )
               MOVE    SPACE               TO  SPA-GMN24-TSYEDYMDW
               MOVE    SPACE               TO  SPA-GMN24-TSYEDYMD
               GO  TO                     41012-TSYEDYMD-EXT
           END-IF
      *----(01.00.04) LINE ADD START --------------------------------
           IF     ((SPA-GMN24-TSYEDYMDW(1:7)   =   ALL "9" ) AND
                   (SPA-GMN24-TSYEDYMDW(8:)    =   SPACE   ))  OR
                  ( SPA-GMN24-TSYEDYMDW        =   SPACE    )
               MOVE    SPACE               TO  SPA-GMN24-TSYEDYMDW(1:7)
               MOVE    ALL "9"             TO  SPA-GMN24-TSYEDYMDW(1:7)
               MOVE    ALL "9"             TO  SPA-GMN24-TSYEDYMD
               GO  TO                      41012-TSYEDYMD-EXT
           END-IF
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN24-TSYEDYMDW     TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN24-TSYEDYMDW
               MOVE    SGDAY-SDAY          TO  SPA-GMN24-TSYEDYMD
           ELSE
               MOVE    "2403"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN24-CUR
               GO  TO                   41012-TSYEDYMD-EXT
           END-IF
      *
      *H30.5  同日対応
           IF  SPA-GMN24-TSYSTYMD       <=  SPA-GMN24-TSYEDYMD
               CONTINUE
           ELSE
               MOVE    "2404"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN24-CUR
           END-IF
      *
           .
       41012-TSYEDYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得者／認定範囲
      *****************************************************************
       41012-TSYKEKKBN-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDF     FROM    1   BY  1
                   UNTIL       IDF     >   SPA-GMN24-TSYTEKKBN-MAX
               IF      SPA-GMN24-TSYTEKKBN
                                       =   SPA-GMN24-TSYTEKKBN-LST(IDF)
                                                              (1:1)
                   MOVE    SPA-GMN24-TSYTEKKBN-LST(IDF)
                                            TO  SPA-GMN24-TSYTEKKBN-G
                   MOVE    1                TO  FLG-OK
                   MOVE    SPA-GMN24-TSYTEKKBN-MAX   TO  IDF
               END-IF
           END-PERFORM
      *
           IF      SPA-GMN24-TSYTEKKBN =   SPACE
               CONTINUE
           ELSE
               IF      FLG-OK              =   ZERO
                   MOVE    "2417"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN24-CUR
               END-IF
           END-IF
      *
           .
       41012-TSYKEKKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得者／減額開始日
      *****************************************************************
       41012-TSYNINYMD-SEC           SECTION.
      *
           IF      SPA-GMN24-TSYNINSTYMD   =   SPACE
               MOVE    SPACE               TO  SPA-NAI24-TSYNINSTYMD
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN24-TSYNINSTYMD   TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN24-TSYNINSTYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI24-TSYNINSTYMD
               ELSE
                   MOVE    "2418"              TO  SPA-ERRCD
                   MOVE    6                   TO  SPA-GMN24-CUR
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD               =   SPACE  )  AND
                  (SPA-GMN24-TSYNINSTYMD   NOT =   SPACE )
               IF     (SPA-GMN24-TSYSTYMD  <=  SPA-NAI24-TSYNINSTYMD)
                 AND  (SPA-GMN24-TSYEDYMD  >=  SPA-NAI24-TSYNINSTYMD)
                   CONTINUE
               ELSE
                   MOVE    "2419"              TO  SPA-ERRCD
                   MOVE    6                   TO  SPA-GMN24-CUR
               END-IF
           END-IF
      *
           .
       41012-TSYNINYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得者／長期入院食事減額開始日
      *****************************************************************
       41012-TSYSKJGNSTYMD-SEC           SECTION.
      *
           IF      SPA-GMN24-TSYSKJGNSTYMDW    =   SPACE
               MOVE    SPACE               TO  SPA-GMN24-TSYSKJGNSTYMD
           ELSE
      *
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN24-TSYSKJGNSTYMDW
                                           TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY     TO  SPA-GMN24-TSYSKJGNSTYMDW
                   MOVE    SGDAY-SDAY      TO  SPA-GMN24-TSYSKJGNSTYMD
               ELSE
                   MOVE    "2413"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-GMN24-CUR
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-GMN24-TSYSKJGNSTYMD NOT =   SPACE )
               IF     (SPA-GMN24-TSYSTYMD  <=  SPA-GMN24-TSYSKJGNSTYMD)
                 AND  (SPA-GMN24-TSYEDYMD  >=  SPA-GMN24-TSYSKJGNSTYMD)
                   CONTINUE
               ELSE
                   MOVE    "2414"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-GMN24-CUR
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-GMN24-TSYSKJGNSTYMD NOT =   SPACE )
      *        長期入院該当日＜減額開始日はエラー
               IF     (SPA-NAI24-TSYNINSTYMD   =   SPACE )  OR
                      (SPA-GMN24-TSYSKJGNSTYMD <  SPA-NAI24-TSYNINSTYMD)
                   MOVE    "2423"          TO  SPA-ERRCD
                   MOVE     4              TO  SPA-GMN24-CUR
               END-IF
           END-IF
      *
           .
       41012-TSYSKJGNSTYMD-EXT.
           EXIT.
      *
      *H29.6
      *****************************************************************
      *    低所得者／境界層該当
      *****************************************************************
       41012-TSYKYOUKAI-SEC           SECTION.
      *
      *    認定日から認定範囲の入力で初期表示
           IF     (SPA-GMN24-TSYEDYMDW    NOT =   SPACE )
             AND  (SPA-GMN24-TSYSTYMDW    NOT =   SPACE )
             AND  (SPA-GMN24-TSYTEKKBN    NOT =   SPACE )
               IF      SPA-GMN24-TSYKYOUKAI   =   SPACE
                   MOVE    SPA-GMN24-TSYKYOUKAI-LST(1)
                                            TO  SPA-GMN24-TSYKYOUKAI-G
               END-IF
           END-IF
      *
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDF     FROM    1   BY  1
                   UNTIL      (IDF     >   SPA-GMN24-TSYKYOUKAI-MAX)
                           OR (FLG-OK  =   1    )
               IF      SPA-GMN24-TSYKYOUKAI
                                       =   SPA-GMN24-TSYKYOUKAI-LST(IDF)
                                                              (1:1)
                   MOVE    SPA-GMN24-TSYKYOUKAI-LST(IDF)
                                            TO  SPA-GMN24-TSYKYOUKAI-G
                   MOVE    1                TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      SPA-GMN24-TSYKYOUKAI    =   SPACE
               CONTINUE
           ELSE
               IF      FLG-OK              =   ZERO
                   MOVE    "2424"              TO  SPA-ERRCD
                   MOVE    7                   TO  SPA-GMN24-CUR
               END-IF
           END-IF
      *
           .
       41012-TSYKYOUKAI-EXT.
           EXIT.
      *****************************************************************
      *    年金／選択番号 
      *****************************************************************
       41012-NENINSELNUM-SEC           SECTION.
      *
           IF  SPA-GMN24-TNKSELNUM     =   SPACE
               MOVE    SPA-GMN24-TNKSELNUM TO  SPA-MAE24-TNKSELNUM
               MOVE    ZERO                TO  SPA-NAI24-TNKSELNUM
               MOVE    SPA-GMN24-TNKSELNUM TO  SPA-MAE24-TNKSELNUM
               GO      TO               41012-NENINSELNUM-EXT
           END-IF
      *
           INITIALIZE                       ORCSNUMAREA
           MOVE    SPA-GMN24-TNKSELNUM  TO  SNUM-INX
           CALL    "ORCSNUM"                USING
                                            ORCSNUMAREA
           IF     (SNUM-RC          NOT =   ZERO  )   OR
                  (SNUM-MINKBN      NOT =   SPACE )   OR
                  (SNUM-SYOKBN      NOT =   SPACE )
               MOVE    "2406"           TO  SPA-ERRCD
               MOVE    10               TO  SPA-GMN24-CUR
               GO      TO               41012-NENINSELNUM-EXT
           ELSE
               MOVE    SNUM-OUTNUM      TO  SPA-NAI24-TNKSELNUM
               MOVE    SNUM-OUTNUM      TO  WRK-RENNUMZ
               MOVE    WRK-RENNUMZ      TO  SPA-GMN24-TNKSELNUM
           END-IF
      *
      **** MOVE    SPA-GMN24-TNKSELNUM TO  SPA-MAE24-TNKSELNUM
           IF      FLG-IN-TNKSELNUM    =   ZERO
               GO      TO               41012-NENINSELNUM-EXT
           END-IF
      *
      *    削除データを除く内部インデックスを取得
           INITIALIZE                   WRK-NAIBU-IDX-AREA
           MOVE    SPA-NAI24-TNKSELNUM  TO  WRK-G-IDX
           MOVE    2                    TO  WRK-W-TBL
           PERFORM 4501-NAIBU-IDX-GET-SEC
           IF  WRK-N-IDX                =   ZERO
               MOVE    "2406"           TO  SPA-ERRCD
               MOVE    10               TO  SPA-GMN24-CUR
               MOVE    SPACE            TO  SPA-GMN24-TNKSTYMDW
               MOVE    SPACE            TO  SPA-GMN24-TNKEDYMDW
               MOVE    SPACE            TO  SPA-GMN24-TNKSTYMD
               MOVE    SPACE            TO  SPA-GMN24-TNKEDYMD
               MOVE    SPACE            TO  SPA-GMN24-FUKUSHI-G
               MOVE    SPACE            TO  SPA-GMN24-TNKNINSTYMD
               MOVE    SPACE            TO  SPA-GMN24-TNKTEKKBN-G
               MOVE    SPACE            TO  SPA-GMN24-TNKKYOUKAI-G
           ELSE
               MOVE    SPA-GMN24-TNKSELNUM TO  SPA-MAE24-TNKSELNUM
               MOVE  SPA-GMN24-TNKDPSTYMDW (WRK-N-IDX)
                                        TO  SPA-GMN24-TNKSTYMDW
               MOVE  SPA-GMN24-TNKDPEDYMDW (WRK-N-IDX)
                                        TO  SPA-GMN24-TNKEDYMDW
               MOVE  SPA-GMN24-TNKDPSTYMD  (WRK-N-IDX)
                                        TO  SPA-GMN24-TNKSTYMD
               MOVE  SPA-GMN24-TNKDPEDYMD  (WRK-N-IDX)
                                        TO  SPA-GMN24-TNKEDYMD
               MOVE  SPA-GMN24-TFUKUSHI-KBN  (WRK-N-IDX)
                                        TO  SPA-GMN24-FUKUSHI-KBN
               MOVE  SPA-GMN24-TFUKUSHI-MEI  (WRK-N-IDX)
                                        TO  SPA-GMN24-FUKUSHI-MEI
               MOVE  SPA-GMN24-TNKLNINYMD    (WRK-N-IDX)
                                        TO  SPA-GMN24-TNKNINSTYMD
               MOVE    SPACE            TO  SPA-GMN24-TNKTEKKBN-G
               MOVE  SPA-NAI24-TNKLHKNKBN (WRK-N-IDX)
                                        TO  SPA-GMN24-TNKTEKKBN
      *        境界層該当
               MOVE    SPACE            TO  SPA-GMN24-TNKKYOUKAI-G
               MOVE    SPA-NAI24-TNKLKYOUKAI (WRK-N-IDX)
                                        TO  SPA-GMN24-TNKKYOUKAI
               IF      SPA-GMN24-TNKKYOUKAI =  SPACE
                   MOVE    ZERO         TO  SPA-GMN24-TNKKYOUKAI
               END-IF
               PERFORM 41012-TNKKYOUKAI-SEC
           END-IF
           .
       41012-NENINSELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    年金／開始年月日 4121120--->H12.11.20
      *****************************************************************
       41012-TNKSTYMD-SEC       SECTION.
      *
           IF      SPA-GMN24-TNKSTYMDW =   SPACE
               MOVE    SPACE               TO  SPA-GMN24-TNKSTYMDW
               MOVE    SPACE               TO  SPA-GMN24-TNKSTYMD
               GO  TO                   41012-TNKSTYMD-EXT
           END-IF
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN24-TNKSTYMDW     TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN24-TNKSTYMDW
               MOVE    SGDAY-SDAY          TO  SPA-GMN24-TNKSTYMD
           ELSE
               MOVE    11                  TO  SPA-GMN24-CUR
               MOVE    "2407"              TO  SPA-ERRCD
           END-IF
            .
       41012-TNKSTYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    年金／終了年月日 4121120--->H12.11.20
      *****************************************************************
       41012-TNKEDYMD-SEC           SECTION.
      *
           IF     (SPA-GMN24-TNKEDYMDW    =   SPACE )  AND
                  (SPA-GMN24-TNKSTYMDW    =   SPACE )
               MOVE    SPACE              TO  SPA-GMN24-TNKEDYMDW
               MOVE    SPACE              TO  SPA-GMN24-TNKEDYMD
               GO  TO                     41012-TNKEDYMD-EXT
           END-IF
      *----(01.00.04) LINE ADD START --------------------------------
           IF    ((SPA-GMN24-TNKEDYMDW(1:7)   =   ALL "9" )  AND
                  (SPA-GMN24-TNKEDYMDW(8:)    =   SPACE   ) )  OR
                 ( SPA-GMN24-TNKEDYMDW        =   SPACE     )
               MOVE    ALL "9"             TO  SPA-GMN24-TNKEDYMDW (1:7)
               MOVE    ALL "9"             TO  SPA-GMN24-TNKEDYMD
               GO  TO                      41012-TNKEDYMD-EXT
           END-IF
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN24-TNKEDYMDW     TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN24-TNKEDYMDW
               MOVE    SGDAY-SDAY          TO  SPA-GMN24-TNKEDYMD
           ELSE
               MOVE    12                  TO  SPA-GMN24-CUR
               MOVE    "2408"              TO  SPA-ERRCD
               GO  TO                   41012-TNKEDYMD-EXT
           END-IF
      *
      *H30.5  同日対応
           IF      SPA-GMN24-TNKSTYMD  <=  SPA-GMN24-TNKEDYMD
               CONTINUE
           ELSE
               MOVE    "2409"              TO  SPA-ERRCD
               MOVE    12                  TO  SPA-GMN24-CUR
           END-IF
           .
       41012-TNKEDYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    年金／老齢福祉年金受給者証チェック
      *****************************************************************
       41012-FUKUSHI-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDF     FROM    1   BY  1
                   UNTIL       IDF     >   SPA-GMN24-FUKUSHI-MAX
               IF      SPA-GMN24-FUKUSHI-KBN
                                       =   SPA-GMN24-FUKUSHI-LST(IDF)
                                                              (1:1)
                   MOVE    SPA-GMN24-FUKUSHI-LST(IDF)
                                        TO  SPA-GMN24-FUKUSHI-G
                   MOVE    1            TO  FLG-OK
                   MOVE    SPA-GMN24-FUKUSHI-MAX   TO  IDF
               END-IF
           END-PERFORM
      *
           IF      SPA-GMN24-FUKUSHI-KBN   NOT =   SPACE
               IF      FLG-OK              =   ZERO
                   MOVE    "2416"              TO  SPA-ERRCD
                   MOVE    13                  TO  SPA-GMN24-CUR
               END-IF
           END-IF
           .
       41012-FUKUSHI-EXT.
           EXIT.
      *****************************************************************
      *    年金／認定範囲
      *****************************************************************
       41012-TNKKEKKBN-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDF     FROM    1   BY  1
                   UNTIL       IDF     >   SPA-GMN24-TNKTEKKBN-MAX
               IF      SPA-GMN24-TNKTEKKBN
                                       =   SPA-GMN24-TNKTEKKBN-LST(IDF)
                                                              (1:1)
                   MOVE    SPA-GMN24-TNKTEKKBN-LST(IDF)
                                            TO  SPA-GMN24-TNKTEKKBN-G
                   MOVE    1                TO  FLG-OK
                   MOVE    SPA-GMN24-TNKTEKKBN-MAX   TO  IDF
               END-IF
           END-PERFORM
      *
           IF      SPA-GMN24-TNKTEKKBN =   SPACE
               CONTINUE
           ELSE
               IF      FLG-OK              =   ZERO
                   MOVE    "2420"              TO  SPA-ERRCD
                   MOVE    14                  TO  SPA-GMN24-CUR
               END-IF
           END-IF
      *
           .
       41012-TNKKEKKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    年金／減額開始日
      *****************************************************************
       41012-TNKNINYMD-SEC           SECTION.
      *
           IF      SPA-GMN24-TNKNINSTYMD   =   SPACE
               MOVE    SPACE               TO  SPA-NAI24-TNKNINSTYMD
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN24-TNKNINSTYMD   TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN24-TNKNINSTYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI24-TNKNINSTYMD
               ELSE
                   MOVE    "2421"              TO  SPA-ERRCD
                   MOVE    15                  TO  SPA-GMN24-CUR
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD               =   SPACE  )  AND
                  (SPA-GMN24-TNKNINSTYMD   NOT =   SPACE )
               IF     (SPA-GMN24-TNKSTYMD  <=  SPA-NAI24-TNKNINSTYMD)
                 AND  (SPA-GMN24-TNKEDYMD  >=  SPA-NAI24-TNKNINSTYMD)
                   CONTINUE
               ELSE
                   MOVE    "2422"              TO  SPA-ERRCD
                   MOVE    15                  TO  SPA-GMN24-CUR
               END-IF
           END-IF
      *
           .
       41012-TNKNINYMD-EXT.
           EXIT.
      *
      *H29.6
      *****************************************************************
      *    低所得者／境界層該当
      *****************************************************************
       41012-TNKKYOUKAI-SEC           SECTION.
      *
      *    認定日から認定範囲の入力で初期表示
           IF     (SPA-GMN24-TNKEDYMDW    NOT =   SPACE )
             AND  (SPA-GMN24-TNKSTYMDW    NOT =   SPACE )
             AND  (SPA-GMN24-TNKTEKKBN    NOT =   SPACE )
               IF      SPA-GMN24-TNKKYOUKAI   =   SPACE
                   MOVE    SPA-GMN24-TNKKYOUKAI-LST(1)
                                            TO  SPA-GMN24-TNKKYOUKAI-G
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDF     FROM    1   BY  1
                   UNTIL       IDF     >   SPA-GMN24-TNKKYOUKAI-MAX
               IF      SPA-GMN24-TNKKYOUKAI
                                       =   SPA-GMN24-TNKKYOUKAI-LST(IDF)
                                                              (1:1)
                   MOVE    SPA-GMN24-TNKKYOUKAI-LST(IDF)
                                            TO  SPA-GMN24-TNKKYOUKAI-G
                   MOVE    1                TO  FLG-OK
                   MOVE    SPA-GMN24-TNKKYOUKAI-MAX   TO  IDF
               END-IF
           END-PERFORM
      *
           IF      SPA-GMN24-TNKKYOUKAI    =   SPACE
               CONTINUE
           ELSE
               IF      FLG-OK              =   ZERO
                   MOVE    "2425"              TO  SPA-ERRCD
                   MOVE    16                  TO  SPA-GMN24-CUR
               END-IF
           END-IF
      *
           .
       41012-TNKKYOUKAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担／開始年月日
      *****************************************************************
       41031-FTNSTYMD-SEC           SECTION.
      *
           IF      SPA-GMN24-FTNSTYMD  =   SPACE
               MOVE    SPACE               TO  SPA-GMN24-FTNSTYMD
               MOVE    SPACE               TO  SPA-NAI24-FTNSTYMD
               GO      TO      41031-FTNSTYMD-EXT
           END-IF
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN24-FTNSTYMD  TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN24-FTNSTYMD
               MOVE    SGDAY-SDAY          TO  SPA-NAI24-FTNSTYMD
           ELSE
               MOVE    "2432"              TO  SPA-ERRCD
               MOVE    32                  TO  SPA-GMN24-CUR
           END-IF
      *    対象公費の期間であること
           IF     (SPA-NAI24-FTNSELNUM     =   ZERO     )
             AND  (SPA-ERRCD               =   SPACE    )
               MOVE    "2430"              TO  SPA-ERRCD
               MOVE    30                  TO  SPA-GMN24-CUR
               GO      TO      41031-FTNSTYMD-EXT
           END-IF
      *
           IF     (SPA-NAI24-TKOHSTYMD (SPA-NAI24-FTNSELNUM)
                                           >   SPA-NAI24-FTNSTYMD )
             AND  (SPA-ERRCD               =   SPACE    )
               MOVE    "2432"              TO  SPA-ERRCD
               MOVE    32                  TO  SPA-GMN24-CUR
           END-IF
           .
       41031-FTNSTYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担／終了年月
      *****************************************************************
       41031-FTNEDYMD-SEC           SECTION.
      *
           IF    ( SPA-GMN24-FTNSTYMD  =   SPACE )  AND
                 ( SPA-GMN24-FTNEDYMD  =   SPACE )
               MOVE    SPACE           TO  SPA-GMN24-FTNEDYMD
               MOVE    SPACE           TO  SPA-NAI24-FTNEDYMD
               GO      TO      41031-FTNEDYMD-EXT
           END-IF
      *
           IF    ( SPA-GMN24-FTNEDYMD  =   SPACE )  AND
                 ((FLG-TOUROKU     NOT =   ZERO  )  OR
                  (WRK-MCP-WIDGET      =   "P024_FTNTEKEDYMD"))
             AND  (SPA-NAI24-FTNSELNUM >   ZERO     )
      *        公費の終了日
               MOVE    SPA-GMN24-TKOHEDYMD (SPA-NAI24-FTNSELNUM)
                                           TO  SPA-GMN24-FTNEDYMD
               MOVE    SPA-NAI24-TKOHEDYMD (SPA-NAI24-FTNSELNUM)
                                           TO  SPA-NAI24-FTNEDYMD
           END-IF
      *
           IF      SPA-GMN24-FTNEDYMD  =   SPACE
               GO      TO      41031-FTNEDYMD-EXT
           END-IF
      *
           IF      SPA-GMN24-FTNEDYMD(1:7) =   ALL "9"
               MOVE    SPACE               TO  SPA-GMN24-FTNEDYMD
               MOVE    ALL "9"             TO  SPA-GMN24-FTNEDYMD(1:7)
               MOVE    ALL "9"             TO  SPA-NAI24-FTNEDYMD
               GO      TO      41031-FTNEDYMD-EXT
           END-IF
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN24-FTNEDYMD  TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN24-FTNEDYMD
               MOVE    SGDAY-SDAY          TO  SPA-NAI24-FTNEDYMD
           ELSE
               MOVE    "2433"              TO  SPA-ERRCD
               MOVE    33                  TO  SPA-GMN24-CUR
               GO      TO      41031-FTNEDYMD-EXT
           END-IF
      *    対象公費の期間であること
           IF     (SPA-NAI24-FTNSELNUM     =   ZERO     )
             AND  (SPA-ERRCD               =   SPACE    )
               MOVE    "2430"              TO  SPA-ERRCD
               MOVE    30                  TO  SPA-GMN24-CUR
               GO      TO      41031-FTNEDYMD-EXT
           END-IF
      *
           IF      SPA-NAI24-TKOHEDYMD (SPA-NAI24-FTNSELNUM)
                                           <   SPA-NAI24-FTNEDYMD
               MOVE    "2433"              TO  SPA-ERRCD
               MOVE    33                  TO  SPA-GMN24-CUR
               GO      TO      41031-FTNEDYMD-EXT
           END-IF
      *
           IF      SPA-GMN24-FTNSTYMD  <=  SPA-GMN24-FTNEDYMD
               CONTINUE
           ELSE
               MOVE    "2438"              TO  SPA-ERRCD
               MOVE    33                  TO  SPA-GMN24-CUR
           END-IF
      *
           .
       41031-FTNEDYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担 外来上限額
      *****************************************************************
       41031-GAIJGNGAK-SEC           SECTION.
      *
      *H30.11
      *    肝がん治療は入力なし
           IF      SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
                                       =   "092"
               MOVE    ZERO             TO  SPA-NAI24-GAIJGNGAK
               MOVE    ZERO             TO  WRK-Z7
               MOVE    WRK-Z7           TO  SPA-GMN24-GAIJGNGAK
               GO      TO      41031-GAIJGNGAK-EXT
           END-IF
      *
           INITIALIZE                       ORCSNUMAREA
           MOVE    SPA-GMN24-GAIJGNGAK TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC          NOT =   ZERO  )   OR
                  (SNUM-MINKBN      NOT =   SPACE )   OR
                  (SNUM-SYOKBN      NOT =   SPACE )   OR
                  (SNUM-SEISUU          >   7     )
               MOVE    "2435"           TO  SPA-ERRCD
               MOVE    34               TO  SPA-GMN24-CUR
           ELSE
               MOVE    SNUM-OUTNUM      TO  SPA-NAI24-GAIJGNGAK
               MOVE    SNUM-OUTNUM      TO  WRK-Z7
               MOVE    WRK-Z7           TO  SPA-GMN24-GAIJGNGAK
      *        減額割合の時、１００まで
               IF     (SPA-NAI24-FTNSELNUM >   ZERO  )
      *H28.11    AND  (SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
      *                                    =   "960"   )
                 AND  (SPA-NAI24-FTNSYORI  =   2       )
                   IF      SPA-NAI24-GAIJGNGAK  >=  ZERO
                                           AND  <=  100
                       CONTINUE
                   ELSE
                       MOVE    "2439"           TO  SPA-ERRCD
                       MOVE    34               TO  SPA-GMN24-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
       41031-GAIJGNGAK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担 入院上限額
      *****************************************************************
       41032-NYUJGNGAK-SEC           SECTION.
      *
           INITIALIZE                       ORCSNUMAREA
           MOVE    SPA-GMN24-NYUJGNGAK TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC          NOT =   ZERO  )   OR
                  (SNUM-MINKBN      NOT =   SPACE )   OR
                  (SNUM-SYOKBN      NOT =   SPACE )   OR
                  (SNUM-SEISUU          >   7     )
               MOVE    "2436"           TO  SPA-ERRCD
               MOVE    35               TO  SPA-GMN24-CUR
           ELSE
               MOVE    SNUM-OUTNUM      TO  SPA-NAI24-NYUJGNGAK
               MOVE    SNUM-OUTNUM      TO  WRK-Z7
               MOVE    WRK-Z7           TO  SPA-GMN24-NYUJGNGAK
      *        減額割合の時、１００まで
               IF     (SPA-NAI24-FTNSELNUM >   ZERO  )
      *H28.11    AND  (SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
      *                                    =   "960"   )
                 AND  (SPA-NAI24-FTNSYORI  =   2       )
                   IF      SPA-NAI24-NYUJGNGAK  >=  ZERO
                                           AND  <=  100
                       CONTINUE
                   ELSE
                       MOVE    "2439"           TO  SPA-ERRCD
                       MOVE    35               TO  SPA-GMN24-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
       41032-NYUJGNGAK-EXT.
           EXIT.
      *
      ****************************************************************
      **    登録前関連チェック処理
      ****************************************************************
       4109-TOUROKU-CHK-SEC             SECTION.
      *    所得履歴登録
           IF      FLG-TOUROKU         =   1
               PERFORM 41091-SYOTOKU-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      4109-TOUROKU-CHK-EXT
               END-IF
           END-IF
      *    老年福祉年金履歴登録
           IF      FLG-TOUROKU         =   2
               PERFORM 41092-RFUKUSI-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      4109-TOUROKU-CHK-EXT
               END-IF
           END-IF
      *    公費負担額登録
           IF      FLG-TOUROKU         =   3
               PERFORM 41093-KOHFTN-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      4109-TOUROKU-CHK-EXT
               END-IF
           END-IF
      *
           .
       4109-TOUROKU-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      **    所得履歴登録前チェック処理
      ****************************************************************
       41091-SYOTOKU-CHK-SEC             SECTION.
      *
           IF     (SPA-GMN24-TSYSTYMD  =   SPACE )  OR
                  (SPA-GMN24-TSYEDYMD  =   SPACE )
               MOVE    "2411"          TO  SPA-ERRCD
               MOVE     2              TO  SPA-GMN24-CUR
               GO      TO      41091-SYOTOKU-CHK-EXT
           END-IF
      *    長期入院該当日＜減額開始日はエラー
           IF     (SPA-GMN24-TSYSKJGNSTYMD  NOT =   SPACE ) AND
                 ((SPA-NAI24-TSYNINSTYMD    =   SPACE )  OR
                  (SPA-GMN24-TSYSKJGNSTYMD  <   SPA-NAI24-TSYNINSTYMD))
               MOVE    "2423"          TO  SPA-ERRCD
               MOVE     4              TO  SPA-GMN24-CUR
               GO      TO      41091-SYOTOKU-CHK-EXT
           END-IF
           IF      SPA-GMN24-TSYTEKKBN  =   SPACE
               MOVE    "2417"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN24-CUR
               GO      TO      41091-SYOTOKU-CHK-EXT
           END-IF
      *    境界層該当
           IF      SPA-GMN24-TSYKYOUKAI    =   SPACE
      *        初期表示を設定
               MOVE    "2424"              TO  SPA-ERRCD
               MOVE    7                   TO  SPA-GMN24-CUR
               GO      TO      41091-SYOTOKU-CHK-EXT
           END-IF
      *
           INITIALIZE                  WRK-NAIBU-IDX-AREA
           MOVE    SPA-NAI24-TSYSELNUM TO  WRK-G-IDX
           MOVE    1                   TO  WRK-W-TBL
           PERFORM 4501-NAIBU-IDX-GET-SEC
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-TSY-MAX
               IF  ( SPA-GMN24-TSYDELKBN(IDX)   =   SPACE )  AND
                   ( WRK-N-IDX              NOT =   IDX   )
      *        表示明細中に入力開始・終了日を含むデータが存在しないこと
                   IF  ( SPA-GMN24-TSYEDYMD
                                       <   SPA-GMN24-TSYDPSTYMD(IDX)) OR
                       ( SPA-GMN24-TSYSTYMD
                                       >   SPA-GMN24-TSYDPEDYMD(IDX))
                       CONTINUE
                   ELSE
                       MOVE   "2405"       TO  SPA-ERRCD
                       MOVE    2           TO  SPA-GMN24-CUR
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       41091-SYOTOKU-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      **    所得履歴登録前チェック処理
      ****************************************************************
       41092-RFUKUSI-CHK-SEC             SECTION.
      *
      *
           IF     (SPA-GMN24-TNKSTYMD  =   SPACE )  OR
                  (SPA-GMN24-TNKEDYMD  =   SPACE )
               MOVE    "2412"          TO  SPA-ERRCD
               MOVE    10              TO  SPA-GMN24-CUR
               GO      TO      41092-RFUKUSI-CHK-EXT
           END-IF
           IF      SPA-GMN24-TNKTEKKBN  =   SPACE
               MOVE    "2420"              TO  SPA-ERRCD
               MOVE    14                  TO  SPA-GMN24-CUR
               GO      TO      41092-RFUKUSI-CHK-EXT
           END-IF
      *
           IF      SPA-GMN24-FUKUSHI-KBN   =   SPACE
               MOVE    "2415"              TO  SPA-ERRCD
               MOVE    13                  TO  SPA-GMN24-CUR
               GO      TO      41092-RFUKUSI-CHK-EXT
           END-IF
      *    境界層該当
           IF      SPA-GMN24-TNKKYOUKAI    =   SPACE
               MOVE    "2425"              TO  SPA-ERRCD
               MOVE    16                  TO  SPA-GMN24-CUR
               GO      TO      41092-RFUKUSI-CHK-EXT
           END-IF
      *
      *
      *    表示明細中に同開始日が存在しないこと
           INITIALIZE                  WRK-NAIBU-IDX-AREA
           MOVE    SPA-NAI24-TNKSELNUM TO  WRK-G-IDX
           MOVE    2                   TO  WRK-W-TBL
           PERFORM 4501-NAIBU-IDX-GET-SEC
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-TNK-MAX
             IF  ( SPA-GMN24-TNKDELKBN(IDX)   =   SPACE )  AND
                 ( WRK-N-IDX              NOT =   IDX   )
      *        表示明細中に入力開始・終了日を含むデータが存在しないこと
               IF  ( SPA-GMN24-TNKEDYMD < SPA-GMN24-TNKDPSTYMD(IDX)) OR
                   ( SPA-GMN24-TNKSTYMD > SPA-GMN24-TNKDPEDYMD(IDX))
                   CONTINUE
               ELSE
                   MOVE   "2410"       TO  SPA-ERRCD
      *----(01.00.01) LINE ADD START --------------------------------
                   MOVE    7           TO  SPA-GMN24-CUR
      *----(01.00.01) LINE ADD END   --------------------------------
                   GO      TO          41092-RFUKUSI-CHK-EXT
               END-IF
             END-IF
           END-PERFORM
      *
           .
       41092-RFUKUSI-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      **    内部インデックス取得処理　（削除データ除く）
      ****************************************************************
       4501-NAIBU-IDX-GET-SEC             SECTION.
      **
      * / WRK-G-IDX...画面上インデックス
      *   WRK-N-IDX...削除データ除く内部的なインデックス
      *
           MOVE    ZERO                   TO  WRK-N-IDX
           MOVE    ZERO                   TO  IDY
           EVALUATE    WRK-W-TBL
      *    所得テーブルより取得
           WHEN    1
               PERFORM   VARYING   IDX    FROM    1   BY  1
                         UNTIL    (IDX            >   20         )
                         OR       (IDX            >   SPA-TSY-MAX)
                         OR       (WRK-N-IDX      >   ZERO       )
                 IF  SPA-GMN24-TSYDELKBN (IDX)    =   SPACE
                     ADD     1                    TO  IDY
                     IF      IDY                  =   WRK-G-IDX
                         MOVE    IDX              TO  WRK-N-IDX
                     END-IF
                 END-IF
               END-PERFORM
      *
      *    年金テーブルより取得
           WHEN    2
               PERFORM   VARYING   IDX    FROM    1   BY  1
                         UNTIL    (IDX            >   20         )
                         OR       (IDX            >   SPA-TNK-MAX)
                         OR       (WRK-N-IDX      >   ZERO       )
                 IF  SPA-GMN24-TNKDELKBN (IDX)    =   SPACE
                     ADD     1                    TO  IDY
                     IF      IDY                  =   WRK-G-IDX
                         MOVE    IDX              TO  WRK-N-IDX
                     END-IF
                 END-IF
               END-PERFORM
           END-EVALUATE
           .
       4501-NAIBU-IDX-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担更新前チェック処理
      *****************************************************************
       41093-KOHFTN-CHK-SEC             SECTION.
      *
      *    公費選択番号
           IF      SPA-NAI24-FTNSELNUM =   ZERO
               MOVE    "2430"              TO  SPA-ERRCD
               MOVE    30                  TO  SPA-GMN24-CUR
               GO      TO      41093-KOHFTN-CHK-EXT
           END-IF
      *
           IF     (SPA-NAI24-FTNSTYMD  =   SPACE )  OR
                  (SPA-NAI24-FTNEDYMD  =   SPACE )
               MOVE    "2437"          TO  SPA-ERRCD
               MOVE    32              TO  SPA-GMN24-CUR
               GO      TO      41093-KOHFTN-CHK-EXT
           END-IF
      *
      *    期間が重複しないこと
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL      (IDX     >   SPA-FTN2-MAX )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF     (SPA-NAI24-FTNSELNUM2    NOT =   IDX    )
                 AND  (SPA-NAI24-FTNDELKBN2(IDX)   =   SPACE )
                   IF     (SPA-NAI24-TFTNEDYMD (IDX)
                                           <   SPA-NAI24-FTNSTYMD )  OR
                          (SPA-NAI24-TFTNSTYMD (IDX)
                                           >   SPA-NAI24-FTNEDYMD )
                       CONTINUE
                   ELSE
                       MOVE    "2434"          TO  SPA-ERRCD
                       MOVE    32              TO  SPA-GMN24-CUR
                   END-IF
               END-IF
           END-PERFORM
      *    期間変更時の他一部負担履歴チェック
           IF      SPA-NAI24-FTNSELNUM2   >   ZERO
               IF     (SPA-NAI24-FTNSTYMD(1:6)
                                       =   SPA-NAI24-TFTNSTYMD
                                              (SPA-NAI24-FTNSELNUM2)
                                                        (1:6))
               OR     (SPA-NAI24-TFTNETCKBN (SPA-NAI24-FTNSELNUM2)
                                       =   "0"
                                       OR  SPACE             )
                   CONTINUE
               ELSE
                   MOVE    "2447"          TO  SPA-ERRCD
                   MOVE    37              TO  SPA-GMN24-CUR
               END-IF
           END-IF
           .
       41093-KOHFTN-CHK-EXT.
           EXIT.
      *
      *H28.11
      *HAORI対応
      *****************************************************************
      *    HAORI　対応処理
      *****************************************************************
       2009-API-SYORI-SEC             SECTION.
      *
           EVALUATE    SPA-MCP-WIDGET
           WHEN    "01"
      *    見出し編集のみ
      *        見出し変更
               PERFORM 3101-KOH-MEISAI-SEC
           WHEN    "02"
      *    公費ＩＤ選択
               INITIALIZE                  SPA-GMN24-KOHFTNTBL2
               MOVE    1                   TO  SPA-NAI24-INIT2
               MOVE    SPA-NAI24-FTNSELNUM TO  WRK-FTN1-IDX
      *        見出し変更
               PERFORM 3101-KOH-MEISAI-SEC
      *        対象のデータをパラメタファイルより編集する
               PERFORM 31022-KOHFTN-SPAHENSYU-SEC
           WHEN    "03"
      *    公費負担チェック
      *        公費負担 開始日
               PERFORM 41031-FTNSTYMD-SEC
      *        公費負担 終了日
               IF      SPA-ERRCD           =   SPACE
      *           公費負担初期編集処理
                  PERFORM 41031-FTNEDYMD-SEC
               END-IF
      *        公費負担 外来上限額
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 41031-GAIJGNGAK-SEC
               END-IF
      *        公費負担 入院上限額
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 41032-NYUJGNGAK-SEC
               END-IF
           WHEN    "04"
      *    他一部負担選択
               PERFORM 4181-P02K-SYORI-SEC
           END-EVALUATE
           .
       2009-API-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦→和暦FORMAT日付編集
      *****************************************************************
       410112-SE-WAFORMAT-YMDEDIT-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"
      ******** MOVE    WRK-SYMD            TO  WRK-HENYMD
               MOVE    SPACE               TO  WRK-HENYMD
               MOVE    ALL "9"             TO  WRK-HENYMD(1:7)
               GO      TO      410112-SE-WAFORMAT-YMDEDIT-EXT
           END-IF
      *
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"        USING  STS-AREA-DAY
                                           LNK-DAY2-AREA
           IF    STS-DAY-RC1            =   ZERO
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
           ELSE
               MOVE    SPACE               TO  WRK-HENYMD
           END-IF
           .
       410112-SE-WAFORMAT-YMDEDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦→和暦FORMAT日付編集
      *****************************************************************
       410113-WA-FOMAT-YMDEDIT-SEC     SECTION.
      *
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "22"                TO  LNK-DAY2-IRAI
           MOVE    WRK-WYMD            TO  LNK-DAY2-YMD (2:7)
           CALL    "ORCSDAY"    USING   STS-AREA-DAY  LNK-DAY2-AREA
           IF  STS-DAY-RC1             =   ZERO
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
           ELSE
               MOVE    SPACE               TO  WRK-HENYMD
           END-IF
           .
       410113-WA-FOMAT-YMDEDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦→西暦日付編集
      *****************************************************************
       410114-WA-SE-YMDEDIT-SEC        SECTION.
      *
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY1-AREA
           MOVE    "12"                TO  LNK-DAY1-IRAI
           MOVE    ZERO                TO  LNK-DAY1-YMD
           MOVE    WRK-WYMD            TO  LNK-DAY1-YMD (2:7)
           CALL    "ORCSDAY"    USING   STS-AREA-DAY  LNK-DAY1-AREA
           IF  STS-DAY-RC1             =   ZERO
               MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
           ELSE
               MOVE    SPACE               TO  WRK-SYMD
           END-IF
           .
       410114-WA-SE-YMDEDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタファイル作成処理
      *****************************************************************
       400-PARA-INSERT-SEC           SECTION.
      *
      *    パラメタ登録
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  HZN-PARA-REC
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    HZN-PARA-REC        TO  PARA-REC
           IF      FLG-PARA            =   1
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA INSERT ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY
                           ",EDANUM:" PARA-EDANUM
               END-IF
           ELSE
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA UPDATE ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY
                           ",EDANUM:" PARA-EDANUM
               END-IF
           END-IF
      *
           .
       400-PARA-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           MOVE    "tbl_para"         TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者公費負担読み込み処理
      *****************************************************************
       990-PTKOHFTNETC-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHFTNETC-REC
               MOVE    ZERO            TO  FLG-PTKOHFTNETC
           ELSE
               INITIALIZE                  PTKOHFTNETC-REC
               MOVE    1               TO  FLG-PTKOHFTNETC
           END-IF
      *
           .
       990-PTKOHFTNETC-READ-EXT.
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
      *    ＤＢクローズ処理
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
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *
