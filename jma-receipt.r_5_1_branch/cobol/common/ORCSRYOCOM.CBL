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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSRYOCOM.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 療養レセコメント取得サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/10/06    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-小豆沢  07/05/29  グループ診療対応
      *  03.05.00    NACL-小豆沢  07/08/20  ＡＤＬ点数の直接入力対応
      *  04.02.00    NACL-小豆沢  08/04/08  医療区分コメント改定対応
      *  05.00.00    NACL-小豆沢  17/08/23  特定入院料の算定要件外で
      *                                     療養病棟入院料算定日はコメ
      *                                     ント出力しない
      *平成３０年４月改定対応
      *  05.00.00    NACL-小豆沢  18/03/08  療養病棟入院料の判定変更
      *  05.00.00    NACL-小豆沢  18/04/11  医療区分コメント改定対応
      *令和２年４月改定対応
      *  05.00.00    NACL-小豆沢  20/04/01  療養病棟入院料の判定変更
      *  05.00.00    NACL-小豆沢  20/04/01  医療区分コメント改定対応
      *  05.00.00    NACL-小豆沢  20/04/08  医療区分コメント"820000243"
      *                                     コメントマスターが提供される
      *                                     までの暫定対応
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-RYOUYOCOM               PIC 9(01).
           03  FLG-RYOYO-NYUIN             PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-1015-ADLINPUTKBN        PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  IDX2                        PIC 9(04).
           03  SIDX1                       PIC 9(04).
           03  SIDX2                       PIC 9(04).
           03  SIDX3                       PIC 9(04).
           03  IDX-ST                      PIC 9(02).
           03  IDX-ED                      PIC 9(02).
      *
      *    コンスタント値
       01  CONSTANT-AREA.
      *    平成２２年３月までのコンスタント値
           03  CONS-RYOYO-NYUIN.
      *        療養病棟入院基本料Ａ
               05                          PIC X(09) VALUE "190121310".
      *        療養病棟入院基本料Ｂ
               05                          PIC X(09) VALUE "190121410".
      *        療養病棟入院基本料Ｃ
               05                          PIC X(09) VALUE "190121510".
      *        療養病棟入院基本料Ｄ
               05                          PIC X(09) VALUE "190121610".
      *        療養病棟入院基本料Ｅ
               05                          PIC X(09) VALUE "190121710".
      *        療養病棟入院基本料（特別入院基本料）
               05                          PIC X(09) VALUE "190121810".
      *        療養病棟入院基本料Ａ（生活療養）
               05                          PIC X(09) VALUE "190123710".
      *        療養病棟入院基本料Ｂ（生活療養）
               05                          PIC X(09) VALUE "190123810".
      *        療養病棟入院基本料Ｃ（生活療養）
               05                          PIC X(09) VALUE "190123910".
      *        療養病棟入院基本料Ｄ（生活療養）
               05                          PIC X(09) VALUE "190124010".
      *        療養病棟入院基本料Ｅ（生活療養）
               05                          PIC X(09) VALUE "190124110".
      *        療養病棟入院基本料（特別入院基本料）（生活療養）
               05                          PIC X(09) VALUE "190124210".
      *        有床診療所療養病床入院基本料Ａ
               05                          PIC X(09) VALUE "190122010".
      *        有床診療所療養病床入院基本料Ｂ
               05                          PIC X(09) VALUE "190122110".
      *        有床診療所療養病床入院基本料Ｃ
               05                          PIC X(09) VALUE "190122210".
      *        有床診療所療養病床入院基本料Ｄ
               05                          PIC X(09) VALUE "190122310".
      *        有床診療所療養病床入院基本料Ｅ
               05                          PIC X(09) VALUE "190122410".
      *        有床診療所療養病床入院基本料（特別入院基本料）
               05                          PIC X(09) VALUE "190122510".
      *        有床診療所療養病床入院基本料Ａ（生活療養）
               05                          PIC X(09) VALUE "190124310".
      *        有床診療所療養病床入院基本料Ｂ（生活療養）
               05                          PIC X(09) VALUE "190124410".
      *        有床診療所療養病床入院基本料Ｃ（生活療養）
               05                          PIC X(09) VALUE "190124510".
      *        有床診療所療養病床入院基本料Ｄ（生活療養）
               05                          PIC X(09) VALUE "190124610".
      *        有床診療所療養病床入院基本料Ｅ（生活療養）
               05                          PIC X(09) VALUE "190124710".
      *        有床診療所療養病床入院基本料（特別入院基本料）（生活療養）
               05                          PIC X(09) VALUE "190124810".
           03  CONS-RYOYO-NYUINR      REDEFINES    CONS-RYOYO-NYUIN.
               05  CONS-RYOYO-NYUIN-OCC    OCCURS  24.
                   07  CONS-RYOYO-SRYCD    PIC X(09).
      *
      *    平成２２年４月からのコンスタント値
           03  CONS-RYOYO-NYUIN-H22.
      *        療養病棟入院基本料１（入院基本料Ａ）
               05                          PIC X(09) VALUE "190121310".
      *        療養病棟入院基本料１（入院基本料Ｂ）
               05                          PIC X(09) VALUE "190121410".
      *        療養病棟入院基本料１（入院基本料Ｃ）
               05                          PIC X(09) VALUE "190121510".
      *        療養病棟入院基本料１（入院基本料Ｄ）
               05                          PIC X(09) VALUE "190121610".
      *        療養病棟入院基本料１（入院基本料Ｅ）
               05                          PIC X(09) VALUE "190121710".
      *        療養病棟入院基本料１（入院基本料Ｆ）
               05                          PIC X(09) VALUE "190131610".
      *        療養病棟入院基本料１（入院基本料Ｇ）
               05                          PIC X(09) VALUE "190131810".
      *        療養病棟入院基本料１（入院基本料Ｈ）
               05                          PIC X(09) VALUE "190132010".
      *        療養病棟入院基本料１（入院基本料Ｉ）
               05                          PIC X(09) VALUE "190132210".
      *        療養病棟入院基本料１（入院基本料Ａ）（生活療養）
               05                          PIC X(09) VALUE "190123710".
      *        療養病棟入院基本料１（入院基本料Ｂ）（生活療養）
               05                          PIC X(09) VALUE "190123810".
      *        療養病棟入院基本料１（入院基本料Ｃ）（生活療養）
               05                          PIC X(09) VALUE "190123910".
      *        療養病棟入院基本料１（入院基本料Ｄ）（生活療養）
               05                          PIC X(09) VALUE "190124010".
      *        療養病棟入院基本料１（入院基本料Ｅ）（生活療養）
               05                          PIC X(09) VALUE "190124110".
      *        療養病棟入院基本料１（入院基本料Ｆ）（生活療養）
               05                          PIC X(09) VALUE "190131710".
      *        療養病棟入院基本料１（入院基本料Ｇ）（生活療養）
               05                          PIC X(09) VALUE "190131910".
      *        療養病棟入院基本料１（入院基本料Ｈ）（生活療養）
               05                          PIC X(09) VALUE "190132110".
      *        療養病棟入院基本料１（入院基本料Ｉ）（生活療養）
               05                          PIC X(09) VALUE "190132310".
      *        療養病棟入院基本料２（入院基本料Ａ）
               05                          PIC X(09) VALUE "190132410".
      *        療養病棟入院基本料２（入院基本料Ｂ）
               05                          PIC X(09) VALUE "190132610".
      *        療養病棟入院基本料２（入院基本料Ｃ）
               05                          PIC X(09) VALUE "190132810".
      *        療養病棟入院基本料２（入院基本料Ｄ）
               05                          PIC X(09) VALUE "190133010".
      *        療養病棟入院基本料２（入院基本料Ｅ）
               05                          PIC X(09) VALUE "190133210".
      *        療養病棟入院基本料２（入院基本料Ｆ）
               05                          PIC X(09) VALUE "190133410".
      *        療養病棟入院基本料２（入院基本料Ｇ）
               05                          PIC X(09) VALUE "190133610".
      *        療養病棟入院基本料２（入院基本料Ｈ）
               05                          PIC X(09) VALUE "190133810".
      *        療養病棟入院基本料２（入院基本料Ｉ）
               05                          PIC X(09) VALUE "190134010".
      *        療養病棟入院基本料２（入院基本料Ａ）（生活療養）
               05                          PIC X(09) VALUE "190132510".
      *        療養病棟入院基本料２（入院基本料Ｂ）（生活療養）
               05                          PIC X(09) VALUE "190132710".
      *        療養病棟入院基本料２（入院基本料Ｃ）（生活療養）
               05                          PIC X(09) VALUE "190132910".
      *        療養病棟入院基本料２（入院基本料Ｄ）（生活療養）
               05                          PIC X(09) VALUE "190133110".
      *        療養病棟入院基本料２（入院基本料Ｅ）（生活療養）
               05                          PIC X(09) VALUE "190133310".
      *        療養病棟入院基本料２（入院基本料Ｆ）（生活療養）
               05                          PIC X(09) VALUE "190133510".
      *        療養病棟入院基本料２（入院基本料Ｇ）（生活療養）
               05                          PIC X(09) VALUE "190133710".
      *        療養病棟入院基本料２（入院基本料Ｈ）（生活療養）
               05                          PIC X(09) VALUE "190133910".
      *        療養病棟入院基本料２（入院基本料Ｉ）（生活療養）
               05                          PIC X(09) VALUE "190134110".
      *        療養病棟入院基本料（特別入院基本料）
               05                          PIC X(09) VALUE "190121810".
      *        療養病棟入院基本料（特別入院基本料）（生活療養）
               05                          PIC X(09) VALUE "190124210".
      *        有床診療所療養病床入院基本料Ａ
               05                          PIC X(09) VALUE "190122010".
      *        有床診療所療養病床入院基本料Ｂ
               05                          PIC X(09) VALUE "190122110".
      *        有床診療所療養病床入院基本料Ｃ
               05                          PIC X(09) VALUE "190122210".
      *        有床診療所療養病床入院基本料Ｄ
               05                          PIC X(09) VALUE "190122310".
      *        有床診療所療養病床入院基本料Ｅ
               05                          PIC X(09) VALUE "190122410".
      *        有床診療所療養病床入院基本料（特別入院基本料）
               05                          PIC X(09) VALUE "190122510".
      *        有床診療所療養病床入院基本料Ａ（生活療養）
               05                          PIC X(09) VALUE "190124310".
      *        有床診療所療養病床入院基本料Ｂ（生活療養）
               05                          PIC X(09) VALUE "190124410".
      *        有床診療所療養病床入院基本料Ｃ（生活療養）
               05                          PIC X(09) VALUE "190124510".
      *        有床診療所療養病床入院基本料Ｄ（生活療養）
               05                          PIC X(09) VALUE "190124610".
      *        有床診療所療養病床入院基本料Ｅ（生活療養）
               05                          PIC X(09) VALUE "190124710".
      *        有床診療所療養病床入院基本料（特別入院基本料）（生活療養）
               05                          PIC X(09) VALUE "190124810".
           03  CONS-RYOYO-NYUINR-H22  REDEFINES  CONS-RYOYO-NYUIN-H22.
               05  CONS-RYOYO-NYUIN-OCC-H22    OCCURS  50.
                   07  CONS-RYOYO-SRYCD-H22    PIC X(09).
      *
      *    平成２８年４月に新設されたＡＤＬ点数コメントのコンスタント値
           03  CONS-ADL-A.
      *        ＡＤＬａ　ベッド上の可動性　１点
               05                          PIC X(09) VALUE "820000199".
      *        ＡＤＬａ　ベッド上の可動性　２点
               05                          PIC X(09) VALUE "820000200".
      *        ＡＤＬａ　ベッド上の可動性　３点
               05                          PIC X(09) VALUE "820000201".
      *        ＡＤＬａ　ベッド上の可動性　４点
               05                          PIC X(09) VALUE "820000202".
      *        ＡＤＬａ　ベッド上の可動性　５点
               05                          PIC X(09) VALUE "820000203".
      *        ＡＤＬａ　ベッド上の可動性　６点
               05                          PIC X(09) VALUE "820000204".
      *        ＡＤＬａ　ベッド上の可動性　０点
               05                          PIC X(09) VALUE "820000198".
           03  CONS-ADL-AR      REDEFINES    CONS-ADL-A.
               05  CONS-ADL-A-OCC          OCCURS  7.
                   07  CONS-ADL-A-COMCD    PIC X(09).
      *
           03  CONS-ADL-B.
      *        ＡＤＬｂ　移乗　１点
               05                          PIC X(09) VALUE "820000206".
      *        ＡＤＬｂ　移乗　２点
               05                          PIC X(09) VALUE "820000207".
      *        ＡＤＬｂ　移乗　３点
               05                          PIC X(09) VALUE "820000208".
      *        ＡＤＬｂ　移乗　４点
               05                          PIC X(09) VALUE "820000209".
      *        ＡＤＬｂ　移乗　５点
               05                          PIC X(09) VALUE "820000210".
      *        ＡＤＬｂ　移乗　６点
               05                          PIC X(09) VALUE "820000211".
      *        ＡＤＬｂ　移乗　０点
               05                          PIC X(09) VALUE "820000205".
           03  CONS-ADL-BR      REDEFINES    CONS-ADL-B.
               05  CONS-ADL-B-OCC          OCCURS  7.
                   07  CONS-ADL-B-COMCD    PIC X(09).
      *
           03  CONS-ADL-C.
      *        ＡＤＬｃ　食事　１点
               05                          PIC X(09) VALUE "820000213".
      *        ＡＤＬｃ　食事　２点
               05                          PIC X(09) VALUE "820000214".
      *        ＡＤＬｃ　食事　３点
               05                          PIC X(09) VALUE "820000215".
      *        ＡＤＬｃ　食事　４点
               05                          PIC X(09) VALUE "820000216".
      *        ＡＤＬｃ　食事　５点
               05                          PIC X(09) VALUE "820000217".
      *        ＡＤＬｃ　食事　６点
               05                          PIC X(09) VALUE "820000218".
      *        ＡＤＬｃ　食事　０点
               05                          PIC X(09) VALUE "820000212".
           03  CONS-ADL-CR      REDEFINES    CONS-ADL-C.
               05  CONS-ADL-C-OCC          OCCURS  7.
                   07  CONS-ADL-C-COMCD    PIC X(09).
      *
           03  CONS-ADL-D.
      *        ＡＤＬｄ　トイレの使用　１点
               05                          PIC X(09) VALUE "820000220".
      *        ＡＤＬｄ　トイレの使用　２点
               05                          PIC X(09) VALUE "820000221".
      *        ＡＤＬｄ　トイレの使用　３点
               05                          PIC X(09) VALUE "820000222".
      *        ＡＤＬｄ　トイレの使用　４点
               05                          PIC X(09) VALUE "820000223".
      *        ＡＤＬｄ　トイレの使用　５点
               05                          PIC X(09) VALUE "820000224".
      *        ＡＤＬｄ　トイレの使用　６点
               05                          PIC X(09) VALUE "820000225".
      *        ＡＤＬｄ　トイレの使用　０点
               05                          PIC X(09) VALUE "820000219".
           03  CONS-ADL-DR      REDEFINES    CONS-ADL-D.
               05  CONS-ADL-D-OCC          OCCURS  7.
                   07  CONS-ADL-D-COMCD    PIC X(09).
      *
      *    一時領域
       01  WRK-AREA.
      *
      *    外泊退避エリア
           03  WRK-GAIHAKU-AREA.
             05  WRK-GAIHAKU       PIC X(01)   OCCURS   31.
      *
      *    特定入院料算定要件外退避エリア
           03  WRK-TOKUGAI-AREA.
             05  WRK-TOKUGAI       PIC X(01)   OCCURS   31.
      *
      *    診療コード計集計用
           03  WRK-SRYCD-X                 PIC X(09).
           03  WRK-SRYCD-9    REDEFINES    WRK-SRYCD-X
                                           PIC 9(09).
      *
      *    医療区分コメント並び替え用
           03  WRK-IDX                     PIC 9(04).
           03  WRK-RYOCOM-IRYO-SORT-COM    PIC X(04).
           03  WRK-RYOCOM-IRYO-SRYCD       PIC X(09).
           03  WRK-RYOCOM-IRYO-ZAINUM      PIC 9(08).
      *
      *    ＡＤＬ点数全角変換領域
       01  TBL-ADLTEN-AREA.
           03  FILLER              PIC X(04)  VALUE "０１".
           03  FILLER              PIC X(04)  VALUE "０２".
           03  FILLER              PIC X(04)  VALUE "０３".
           03  FILLER              PIC X(04)  VALUE "０４".
           03  FILLER              PIC X(04)  VALUE "０５".
           03  FILLER              PIC X(04)  VALUE "０６".
           03  FILLER              PIC X(04)  VALUE "０７".
           03  FILLER              PIC X(04)  VALUE "０８".
           03  FILLER              PIC X(04)  VALUE "０９".
           03  FILLER              PIC X(04)  VALUE "１０".
           03  FILLER              PIC X(04)  VALUE "１１".
           03  FILLER              PIC X(04)  VALUE "１２".
           03  FILLER              PIC X(04)  VALUE "１３".
           03  FILLER              PIC X(04)  VALUE "１４".
           03  FILLER              PIC X(04)  VALUE "１５".
           03  FILLER              PIC X(04)  VALUE "１６".
           03  FILLER              PIC X(04)  VALUE "１７".
           03  FILLER              PIC X(04)  VALUE "１８".
           03  FILLER              PIC X(04)  VALUE "１９".
           03  FILLER              PIC X(04)  VALUE "２０".
           03  FILLER              PIC X(04)  VALUE "２１".
           03  FILLER              PIC X(04)  VALUE "２２".
           03  FILLER              PIC X(04)  VALUE "２３".
           03  FILLER              PIC X(04)  VALUE "２４".
       01  TBL-ADLTEN-AREA-R       REDEFINES  TBL-ADLTEN-AREA.
           03  TBL-ADLTEN          PIC X(04)   OCCURS   24.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK5000.INC".
           COPY    "CPSK5015.INC".
      *
      *    入院診療会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為マスター
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    療養レセプトコメントマスタ−
       01  RYOUYOCOM-REC.
           COPY    "CPRYOCOM.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    療養レセコメント取得パラメタ
           COPY    "CPORCSRYOCOM.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSRYOCOM-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-RYOCOM-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-RYOCOM-RC
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-RYOCOM-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    主処理
           IF      LNK-RYOCOM-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    パラメータチェック
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    算定年月チェック
           IF    ( LNK-RYOCOM-SANTEIYM    =   SPACE )
               MOVE    1               TO  LNK-RYOCOM-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    算定年月チェック
           IF    ( LNK-RYOCOM-SANTEIYM    <   "200610" )
               MOVE    1               TO  LNK-RYOCOM-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    患者マスター検索
           INITIALIZE                      PTINF-REC
           MOVE    LNK-RYOCOM-HOSPNUM  TO  PTINF-HOSPNUM
           MOVE    LNK-RYOCOM-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    2               TO  LNK-RYOCOM-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *
      *    入院中のチェック（入院会計マスター検索）
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-RYOCOM-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-RYOCOM-PTID     TO  NACCT-PTID
           MOVE    LNK-RYOCOM-SANTEIYM TO  NACCT-SRYYM
           MOVE    "5"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           NOT =   ZERO
               MOVE    3               TO  LNK-RYOCOM-RC
           ELSE
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT          NOT =   ZERO
                    MOVE    3              TO  LNK-RYOCOM-RC
               END-IF
           END-IF
      *
      *    入院会計クローズ
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *    平成２２年４月以降は外泊時の療養コメントを返却しない
      *    （外泊情報の取得）
           INITIALIZE    WRK-GAIHAKU-AREA
           IF      LNK-RYOCOM-RC    =    ZERO
               IF    LNK-RYOCOM-SANTEIYM   >=  "201004"
                   MOVE    LNK-RYOCOM-HOSPNUM  TO  NACCT-HOSPNUM
                   MOVE    LNK-RYOCOM-PTID     TO  NACCT-PTID
                   MOVE    LNK-RYOCOM-SANTEIYM TO  NACCT-SRYYM
                   MOVE    "2"                 TO  NACCT-ZAISKBKBN
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key17"             TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-SELECT-SEC
                   IF      FLG-NYUINACCT       =   ZERO
                       MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                       MOVE    "key17"         TO  MCP-PATHNAME
                       PERFORM 910-NYUINACCT-READ-SEC
                       IF   FLG-NYUINACCT      =   ZERO
                            PERFORM   VARYING   IDX    FROM   1  BY  1
                              UNTIL   IDX       >      31
                              IF   NACCT-DAY(IDX)    =   1 OR 2 OR 3
                                 MOVE "1"  TO  WRK-GAIHAKU(IDX)
                              END-IF
                            END-PERFORM
                       END-IF
                   END-IF
      *            入院会計クローズ
                   MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
                   MOVE    "key17"                 TO  MCP-PATHNAME
                   PERFORM   910-DB-CLOSE-SEC
               END-IF
           END-IF
      *
      *    特定入院料の算定要件外で入院している日は療養コメントを
      *    返却しない（患者入院履歴から取得）
           INITIALIZE    WRK-TOKUGAI-AREA
           IF      LNK-RYOCOM-RC    =    ZERO
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    LNK-RYOCOM-HOSPNUM  TO  PTNYUINRRK-HOSPNUM
               MOVE    LNK-RYOCOM-PTID     TO  PTNYUINRRK-PTID
               MOVE    LNK-RYOCOM-SANTEIYM TO  PTNYUINRRK-TENNYUYMD(1:6)
               MOVE    "31"                TO  PTNYUINRRK-TENNYUYMD(7:2)
               MOVE    LNK-RYOCOM-SANTEIYM TO  PTNYUINRRK-TENSTUYMD(1:6)
               MOVE    "01"                TO  PTNYUINRRK-TENSTUYMD(7:2)
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key41"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-SELECT-SEC
               IF      FLG-PTNYUINRRK      =   ZERO
                   MOVE    "tbl_ptnyuinrrk" TO  MCP-TABLE
                   MOVE    "key41"         TO  MCP-PATHNAME
                   PERFORM 910-PTNYUINRRK-READ-SEC
                   PERFORM    UNTIL   FLG-PTNYUINRRK  =   1
                        IF    PTNYUINRRK-TENNYUYMD(1:6)  < 
                                                   LNK-RYOCOM-SANTEIYM
                            MOVE   1       TO  IDX-ST
                        END-IF
                        IF    PTNYUINRRK-TENNYUYMD(1:6)  = 
                                                   LNK-RYOCOM-SANTEIYM
                            MOVE   PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                        END-IF
                        IF    PTNYUINRRK-TENSTUYMD(1:6)  > 
                                                   LNK-RYOCOM-SANTEIYM
                            MOVE   31       TO  IDX-ED
                        END-IF
                        IF    PTNYUINRRK-TENSTUYMD(1:6)  = 
                                                   LNK-RYOCOM-SANTEIYM
                            MOVE   PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                        END-IF
      *                  DISPLAY "PTNYUINRRK-TENNYUYMD = "
      *                           PTNYUINRRK-TENNYUYMD
      *                  DISPLAY "PTNYUINRRK-TENSTUYMD = "
      *                           PTNYUINRRK-TENSTUYMD
      *                  DISPLAY "IDX-ST = " IDX-ST
      *                  DISPLAY "IDX-ED = " IDX-ED
                        PERFORM   VARYING   IDX    FROM   IDX-ST  BY  1
                          UNTIL   IDX       >      IDX-ED
                          IF   PTNYUINRRK-TOKU-SANTEIYOUKENKBN   = "1"
                             MOVE "1"  TO  WRK-TOKUGAI(IDX)
                          ELSE
                             MOVE  SPACE   TO  WRK-TOKUGAI(IDX)
                          END-IF
                        END-PERFORM
                     MOVE    "tbl_ptnyuinrrk" TO  MCP-TABLE
                     MOVE    "key41"          TO  MCP-PATHNAME
                     PERFORM 910-PTNYUINRRK-READ-SEC
                   END-PERFORM
               END-IF
      *        患者入院履歴クローズ
               MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
               MOVE    "key41"                 TO  MCP-PATHNAME
               PERFORM   910-DB-CLOSE-SEC
           END-IF
      *
      *    療養入院算定のチェック（入院会計マスター検索）
           IF      LNK-RYOCOM-RC    =    ZERO
               INITIALIZE                      NYUINACCT-REC
               MOVE    LNK-RYOCOM-HOSPNUM  TO  NACCT-HOSPNUM
               MOVE    LNK-RYOCOM-PTID     TO  NACCT-PTID
               MOVE    LNK-RYOCOM-SANTEIYM TO  NACCT-SRYYM
               MOVE    "1"                 TO  NACCT-ZAISKBKBN
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-SELECT-SEC
               IF      FLG-NYUINACCT           NOT =   ZERO
                   MOVE    4               TO  LNK-RYOCOM-RC
               ELSE
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key17"             TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
                   IF   FLG-NYUINACCT          NOT =   ZERO
                        MOVE    4              TO  LNK-RYOCOM-RC
                   ELSE
                        PERFORM   UNTIL  FLG-NYUINACCT    =   1
                            PERFORM   100-PRM-RYOYO-NYUINCHK-SEC
                            MOVE    "tbl_nyuinacct"   TO  MCP-TABLE
                            MOVE    "key17"           TO  MCP-PATHNAME
                            PERFORM 910-NYUINACCT-READ-SEC
                        END-PERFORM
                   END-IF
               END-IF
      *        入院会計クローズ
               MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
               MOVE    "key17"                 TO  MCP-PATHNAME
               PERFORM   910-DB-CLOSE-SEC
      *
               IF   FLG-RYOYO-NYUIN    =   ZERO
                   MOVE    4           TO  LNK-RYOCOM-RC
               END-IF
           END-IF
      *
      *    医療機関情報の取得（ＡＤＬ入力）
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-RYOCOM-SANTEIYM TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    LNK-RYOCOM-HOSPNUM  TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
                                  SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF    FLG-SYSKANRI   =    ZERO
                   IF   SYS-5000-ADLINPUTKBN   =   "0"
                       MOVE     5      TO  LNK-RYOCOM-RC
                   END-IF
               END-IF
           END-IF

      *    システム管理クローズ
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    療養病棟（病床）での入院チェック
      *****************************************************************
       100-PRM-RYOYO-NYUINCHK-SEC          SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYKA         TO  NSRY-SRYKA
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACT-SELECT-SEC
           IF      FLG-NYUINACT        =   ZERO
               MOVE    "tbl_nyuinact"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-NYUINACT-READ-SEC
      *        平成２２年３月までの入院チェック
               IF    LNK-RYOCOM-SANTEIYM   <=  "201003"
                 PERFORM   VARYING  IDX   FROM   1   BY  1
                   UNTIL  (IDX      >     24)
                   IF   NSRY-SRYCD(1)   =    CONS-RYOYO-SRYCD(IDX)
                       PERFORM   VARYING  IDX1   FROM   1  BY  1
                         UNTIL  (IDX1    >   31)
                           IF   NACCT-DAY(IDX1)    =   1
                               MOVE "1"  TO  LNK-RYOCOM-NYUINKBN(IDX1)
                               MOVE  1   TO  FLG-RYOYO-NYUIN
                           END-IF
                       END-PERFORM
                   END-IF
                 END-PERFORM
               END-IF
      *        平成２２年４月から平成２４年３月までの入院チェック
               IF   ( LNK-RYOCOM-SANTEIYM   >=  "201004" )    AND
                    ( LNK-RYOCOM-SANTEIYM   <=  "201203" )
                 PERFORM   VARYING  IDX   FROM   1   BY  1
                   UNTIL  (IDX      >     50)
                   IF   NSRY-SRYCD(1)   =    CONS-RYOYO-SRYCD-H22(IDX)
                       PERFORM   VARYING  IDX1   FROM   1  BY  1
                         UNTIL  (IDX1    >   31)
                           IF  (NACCT-DAY(IDX1)    =   1)   AND
                               (WRK-GAIHAKU(IDX1)  =   SPACE)
                               MOVE "1"  TO  LNK-RYOCOM-NYUINKBN(IDX1)
                               MOVE  1   TO  FLG-RYOYO-NYUIN
                           END-IF
                       END-PERFORM
                   END-IF
                 END-PERFORM
               END-IF
      *        平成２４年４月から平成２６年３月までの入院チェック
               IF   ( LNK-RYOCOM-SANTEIYM   >=  "201204" )   AND
                    ( LNK-RYOCOM-SANTEIYM   <=  "201403" )
                   INITIALIZE                       TNS-TENSU-REC
                   MOVE   LNK-RYOCOM-SANTEIYM  TO  TNS-YUKOSTYMD (1:6)
                   MOVE   "01"                 TO  TNS-YUKOSTYMD (7:2)
                   MOVE   TNS-YUKOSTYMD        TO  TNS-YUKOEDYMD
                   MOVE   LNK-RYOCOM-HOSPNUM   TO  TNS-HOSPNUM
                   MOVE   NSRY-SRYCD(1)        TO  TNS-SRYCD
                   MOVE   TNS-TENSU-REC        TO  MCPDATA-REC
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE  "tbl_tensu"       TO  MCP-TABLE
                       MOVE  "key"             TO  MCP-PATHNAME
                       PERFORM 910-DBFETCH-SEC
                       IF      MCP-RC          =   ZERO
                           MOVE  MCPDATA-REC   TO  TNS-TENSU-REC
      *                    入院基本料加算区分からの判定
                           EVALUATE   TNS-NYUKHNKSNKBN
      *                      療養病棟入院基本料（特別入院基本料）
                             WHEN   152
      *                      有床診療所療養病床入院基本料Ａ
                             WHEN   153
      *                      有床診療所療養病床入院基本料Ｂ
                             WHEN   154
      *                      有床診療所療養病床入院基本料Ｃ
                             WHEN   155
      *                      有床診療所療養病床入院基本料Ｄ
                             WHEN   156
      *                      有床診療所療養病床入院基本料Ｅ
                             WHEN   157
      *                      有床診療所療養病床入院基本料（特別入院基本料）
                             WHEN   158
      *                      療養病棟入院基本料１（入院基本料Ａ）
                             WHEN   173
      *                      療養病棟入院基本料１（入院基本料Ｂ）
                             WHEN   174
      *                      療養病棟入院基本料１（入院基本料Ｃ）
                             WHEN   175
      *                      療養病棟入院基本料１（入院基本料Ｄ）
                             WHEN   176
      *                      療養病棟入院基本料１（入院基本料Ｅ）
                             WHEN   177
      *                      療養病棟入院基本料１（入院基本料Ｆ）
                             WHEN   178
      *                      療養病棟入院基本料１（入院基本料Ｇ）
                             WHEN   179
      *                      療養病棟入院基本料１（入院基本料Ｈ）
                             WHEN   180
      *                      療養病棟入院基本料１（入院基本料Ｉ）
                             WHEN   181
      *                      療養病棟入院基本料２（入院基本料Ａ）
                             WHEN   182
      *                      療養病棟入院基本料２（入院基本料Ｂ）
                             WHEN   183
      *                      療養病棟入院基本料２（入院基本料Ｃ）
                             WHEN   184
      *                      療養病棟入院基本料２（入院基本料Ｄ）
                             WHEN   185
      *                      療養病棟入院基本料２（入院基本料Ｅ）
                             WHEN   186
      *                      療養病棟入院基本料２（入院基本料Ｆ）
                             WHEN   187
      *                      療養病棟入院基本料２（入院基本料Ｇ）
                             WHEN   188
      *                      療養病棟入院基本料２（入院基本料Ｈ）
                             WHEN   189
      *                      療養病棟入院基本料２（入院基本料Ｉ）
                             WHEN   190
      *                      （一般病棟入院患者）療養病棟入院基本料１
                             WHEN   196
                               PERFORM  VARYING   IDX1 FROM 1 BY 1
                                 UNTIL  ( IDX1     >   31 )
                                 IF  (NACCT-DAY(IDX1)    =   1)   AND
                                     (WRK-GAIHAKU(IDX1)  =   SPACE)
                                      MOVE "1"  TO  LNK-RYOCOM-NYUINKBN
                                                                (IDX1)
                                      MOVE  1     TO  FLG-RYOYO-NYUIN
                                 END-IF
                               END-PERFORM
                           END-EVALUATE
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"   TO  MCP-TABLE
                   MOVE    "key"         TO  MCP-PATHNAME
                   PERFORM  910-DB-CLOSE-SEC
               END-IF
      *        平成２６年４月から平成２８年３月までの入院チェック
               IF   ( LNK-RYOCOM-SANTEIYM   >=  "201404" )   AND
                    ( LNK-RYOCOM-SANTEIYM   <=  "201603" )
                   INITIALIZE                       TNS-TENSU-REC
                   MOVE   LNK-RYOCOM-SANTEIYM  TO  TNS-YUKOSTYMD (1:6)
                   MOVE   "01"                 TO  TNS-YUKOSTYMD (7:2)
                   MOVE   TNS-YUKOSTYMD        TO  TNS-YUKOEDYMD
                   MOVE   LNK-RYOCOM-HOSPNUM   TO  TNS-HOSPNUM
                   MOVE   NSRY-SRYCD(1)        TO  TNS-SRYCD
                   MOVE   TNS-TENSU-REC        TO  MCPDATA-REC
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE  "tbl_tensu"       TO  MCP-TABLE
                       MOVE  "key"             TO  MCP-PATHNAME
                       PERFORM 910-DBFETCH-SEC
                       IF      MCP-RC          =   ZERO
                           MOVE  MCPDATA-REC   TO  TNS-TENSU-REC
      *                    入院基本料加算区分からの判定
                           EVALUATE   TNS-NYUKHNKSNKBN
      *                      療養病棟入院基本料（特別入院基本料）
                             WHEN   152
      *                      有床診療所療養病床入院基本料Ａ
                             WHEN   153
      *                      有床診療所療養病床入院基本料Ｂ
                             WHEN   154
      *                      有床診療所療養病床入院基本料Ｃ
                             WHEN   155
      *                      有床診療所療養病床入院基本料Ｄ
                             WHEN   156
      *                      有床診療所療養病床入院基本料Ｅ
                             WHEN   157
      *                      有床診療所療養病床入院基本料（特別入院基本料）
                             WHEN   158
      *                      療養病棟入院基本料１（入院基本料Ａ）
                             WHEN   173
      *                      療養病棟入院基本料１（入院基本料Ｂ）
                             WHEN   174
      *                      療養病棟入院基本料１（入院基本料Ｃ）
                             WHEN   175
      *                      療養病棟入院基本料１（入院基本料Ｄ）
                             WHEN   176
      *                      療養病棟入院基本料１（入院基本料Ｅ）
                             WHEN   177
      *                      療養病棟入院基本料１（入院基本料Ｆ）
                             WHEN   178
      *                      療養病棟入院基本料１（入院基本料Ｇ）
                             WHEN   179
      *                      療養病棟入院基本料１（入院基本料Ｈ）
                             WHEN   180
      *                      療養病棟入院基本料１（入院基本料Ｉ）
                             WHEN   181
      *                      療養病棟入院基本料２（入院基本料Ａ）
                             WHEN   182
      *                      療養病棟入院基本料２（入院基本料Ｂ）
                             WHEN   183
      *                      療養病棟入院基本料２（入院基本料Ｃ）
                             WHEN   184
      *                      療養病棟入院基本料２（入院基本料Ｄ）
                             WHEN   185
      *                      療養病棟入院基本料２（入院基本料Ｅ）
                             WHEN   186
      *                      療養病棟入院基本料２（入院基本料Ｆ）
                             WHEN   187
      *                      療養病棟入院基本料２（入院基本料Ｇ）
                             WHEN   188
      *                      療養病棟入院基本料２（入院基本料Ｈ）
                             WHEN   189
      *                      療養病棟入院基本料２（入院基本料Ｉ）
                             WHEN   190
      *                      （一般病棟入院患者）療養病棟入院基本料１
                             WHEN   196
      *                      療養病棟入院基本料２（入院基本料Ａ）（月平均夜勤時間超過減算）
                             WHEN   199
      *                      療養病棟入院基本料２（入院基本料Ｂ）（月平均夜勤時間超過減算）
                             WHEN   004
      *                      療養病棟入院基本料２（入院基本料Ｃ）（月平均夜勤時間超過減算）
                             WHEN   005
      *                      療養病棟入院基本料２（入院基本料Ｄ）（月平均夜勤時間超過減算）
                             WHEN   006
      *                      療養病棟入院基本料２（入院基本料Ｅ）（月平均夜勤時間超過減算）
                             WHEN   007
      *                      療養病棟入院基本料２（入院基本料Ｆ）（月平均夜勤時間超過減算）
                             WHEN   008
      *                      療養病棟入院基本料２（入院基本料Ｇ）（月平均夜勤時間超過減算）
                             WHEN   010
      *                      療養病棟入院基本料２（入院基本料Ｈ）（月平均夜勤時間超過減算）
                             WHEN   011
      *                      療養病棟入院基本料２（入院基本料Ｉ）（月平均夜勤時間超過減算）
                             WHEN   012
                               PERFORM  VARYING   IDX1 FROM 1 BY 1
                                 UNTIL  ( IDX1     >   31 )
                                 IF  (NACCT-DAY(IDX1)    =   1)   AND
                                     (WRK-GAIHAKU(IDX1)  =   SPACE)
                                      MOVE "1"  TO  LNK-RYOCOM-NYUINKBN
                                                                (IDX1)
                                      MOVE  1     TO  FLG-RYOYO-NYUIN
                                 END-IF
                               END-PERFORM
                           END-EVALUATE
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"   TO  MCP-TABLE
                   MOVE    "key"         TO  MCP-PATHNAME
                   PERFORM  910-DB-CLOSE-SEC
               END-IF
      *        平成２８年４月からの入院チェック
               IF   ( LNK-RYOCOM-SANTEIYM   >=  "201604" )   AND
                    ( LNK-RYOCOM-SANTEIYM   <=  "201803" )
                   INITIALIZE                       TNS-TENSU-REC
                   MOVE   LNK-RYOCOM-SANTEIYM  TO  TNS-YUKOSTYMD (1:6)
                   MOVE   "01"                 TO  TNS-YUKOSTYMD (7:2)
                   MOVE   TNS-YUKOSTYMD        TO  TNS-YUKOEDYMD
                   MOVE   LNK-RYOCOM-HOSPNUM   TO  TNS-HOSPNUM
                   MOVE   NSRY-SRYCD(1)        TO  TNS-SRYCD
                   MOVE   TNS-TENSU-REC        TO  MCPDATA-REC
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE  "tbl_tensu"       TO  MCP-TABLE
                       MOVE  "key"             TO  MCP-PATHNAME
                       PERFORM 910-DBFETCH-SEC
                       IF      MCP-RC          =   ZERO
                           MOVE  MCPDATA-REC   TO  TNS-TENSU-REC
      *                    入院基本料加算区分からの判定
                           EVALUATE   TNS-NYUKHNKSNKBN
      *                      療養病棟入院基本料（特別入院基本料）
                             WHEN   152
      *                      有床診療所療養病床入院基本料Ａ
                             WHEN   153
      *                      有床診療所療養病床入院基本料Ｂ
                             WHEN   154
      *                      有床診療所療養病床入院基本料Ｃ
                             WHEN   155
      *                      有床診療所療養病床入院基本料Ｄ
                             WHEN   156
      *                      有床診療所療養病床入院基本料Ｅ
                             WHEN   157
      *                      有床診療所療養病床入院基本料（特別入院基本料）
                             WHEN   158
      *                      療養病棟入院基本料１（入院基本料Ａ）
                             WHEN   173
      *                      療養病棟入院基本料１（入院基本料Ｂ）
                             WHEN   174
      *                      療養病棟入院基本料１（入院基本料Ｃ）
                             WHEN   175
      *                      療養病棟入院基本料１（入院基本料Ｄ）
                             WHEN   176
      *                      療養病棟入院基本料１（入院基本料Ｅ）
                             WHEN   177
      *                      療養病棟入院基本料１（入院基本料Ｆ）
                             WHEN   178
      *                      療養病棟入院基本料１（入院基本料Ｇ）
                             WHEN   179
      *                      療養病棟入院基本料１（入院基本料Ｈ）
                             WHEN   180
      *                      療養病棟入院基本料１（入院基本料Ｉ）
                             WHEN   181
      *                      療養病棟入院基本料２（入院基本料Ａ）
                             WHEN   182
      *                      療養病棟入院基本料２（入院基本料Ｂ）
                             WHEN   183
      *                      療養病棟入院基本料２（入院基本料Ｃ）
                             WHEN   184
      *                      療養病棟入院基本料２（入院基本料Ｄ）
                             WHEN   185
      *                      療養病棟入院基本料２（入院基本料Ｅ）
                             WHEN   186
      *                      療養病棟入院基本料２（入院基本料Ｆ）
                             WHEN   187
      *                      療養病棟入院基本料２（入院基本料Ｇ）
                             WHEN   188
      *                      療養病棟入院基本料２（入院基本料Ｈ）
                             WHEN   189
      *                      療養病棟入院基本料２（入院基本料Ｉ）
                             WHEN   190
      *                      （一般病棟入院患者）療養病棟入院基本料１
                             WHEN   196
      *                      療養病棟入院基本料２（入院基本料Ａ）（月平均夜勤時間超過減算）
                             WHEN   199
      *                      療養病棟入院基本料２（入院基本料Ｂ）（月平均夜勤時間超過減算）
                             WHEN   004
      *                      療養病棟入院基本料２（入院基本料Ｃ）（月平均夜勤時間超過減算）
                             WHEN   005
      *                      療養病棟入院基本料２（入院基本料Ｄ）（月平均夜勤時間超過減算）
                             WHEN   006
      *                      療養病棟入院基本料２（入院基本料Ｅ）（月平均夜勤時間超過減算）
                             WHEN   007
      *                      療養病棟入院基本料２（入院基本料Ｆ）（月平均夜勤時間超過減算）
                             WHEN   008
      *                      療養病棟入院基本料２（入院基本料Ｇ）（月平均夜勤時間超過減算）
                             WHEN   010
      *                      療養病棟入院基本料２（入院基本料Ｈ）（月平均夜勤時間超過減算）
                             WHEN   011
      *                      療養病棟入院基本料２（入院基本料Ｉ）（月平均夜勤時間超過減算）
                             WHEN   012
      *                      療養病棟入院基本料２（入院基本料Ａ）（看護職員数等経過措置）
                             WHEN   048
      *                      療養病棟入院基本料２（入院基本料Ｂ）（看護職員数等経過措置）
                             WHEN   049
      *                      療養病棟入院基本料２（入院基本料Ｃ）（看護職員数等経過措置）
                             WHEN   051
      *                      療養病棟入院基本料２（入院基本料Ｄ）（看護職員数等経過措置）
                             WHEN   052
      *                      療養病棟入院基本料２（入院基本料Ｅ）（看護職員数等経過措置）
                             WHEN   053
      *                      療養病棟入院基本料２（入院基本料Ｆ）（看護職員数等経過措置）
                             WHEN   054
      *                      療養病棟入院基本料２（入院基本料Ｇ）（看護職員数等経過措置）
                             WHEN   057
      *                      療養病棟入院基本料２（入院基本料Ｈ）（看護職員数等経過措置）
                             WHEN   058
      *                      療養病棟入院基本料２（入院基本料Ｉ）（看護職員数等経過措置）
                             WHEN   059
      *                      療養病棟入院基本料２（入院基本料Ａ）（夜勤時間特別入院基本料）
                             WHEN   060
      *                      療養病棟入院基本料２（入院基本料Ｂ）（夜勤時間特別入院基本料）
                             WHEN   061
      *                      療養病棟入院基本料２（入院基本料Ｃ）（夜勤時間特別入院基本料）
                             WHEN   062
      *                      療養病棟入院基本料２（入院基本料Ｄ）（夜勤時間特別入院基本料）
                             WHEN   063
      *                      療養病棟入院基本料２（入院基本料Ｅ）（夜勤時間特別入院基本料）
                             WHEN   064
      *                      療養病棟入院基本料２（入院基本料Ｆ）（夜勤時間特別入院基本料）
                             WHEN   065
      *                      療養病棟入院基本料２（入院基本料Ｇ）（夜勤時間特別入院基本料）
                             WHEN   066
      *                      療養病棟入院基本料２（入院基本料Ｈ）（夜勤時間特別入院基本料）
                             WHEN   067
      *                      療養病棟入院基本料２（入院基本料Ｉ）（夜勤時間特別入院基本料）
                             WHEN   068
                               PERFORM  VARYING   IDX1 FROM 1 BY 1
                                 UNTIL  ( IDX1     >   31 )
                                 IF  (NACCT-DAY(IDX1)    =   1)   AND
                                     (WRK-GAIHAKU(IDX1)  =   SPACE) AND
                                     (WRK-TOKUGAI(IDX1)  =   SPACE)
                                      MOVE "1"  TO  LNK-RYOCOM-NYUINKBN
                                                                (IDX1)
                                      MOVE  1     TO  FLG-RYOYO-NYUIN
                                 END-IF
                               END-PERFORM
                           END-EVALUATE
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"   TO  MCP-TABLE
                   MOVE    "key"         TO  MCP-PATHNAME
                   PERFORM  910-DB-CLOSE-SEC
               END-IF
      *        平成３０年４月から令和２年３月までの入院チェック
               IF   ( LNK-RYOCOM-SANTEIYM   >=  "201804" )   AND
                    ( LNK-RYOCOM-SANTEIYM   <=  "202003" )
                   INITIALIZE                       TNS-TENSU-REC
                   MOVE   LNK-RYOCOM-SANTEIYM  TO  TNS-YUKOSTYMD (1:6)
                   MOVE   "01"                 TO  TNS-YUKOSTYMD (7:2)
                   MOVE   TNS-YUKOSTYMD        TO  TNS-YUKOEDYMD
                   MOVE   LNK-RYOCOM-HOSPNUM   TO  TNS-HOSPNUM
                   MOVE   NSRY-SRYCD(1)        TO  TNS-SRYCD
                   MOVE   TNS-TENSU-REC        TO  MCPDATA-REC
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE  "tbl_tensu"       TO  MCP-TABLE
                       MOVE  "key"             TO  MCP-PATHNAME
                       PERFORM 910-DBFETCH-SEC
                       IF      MCP-RC          =   ZERO
                           MOVE  MCPDATA-REC   TO  TNS-TENSU-REC
      *                    入院基本料加算区分からの判定
                           EVALUATE TNS-CDKBN-KBNNUM    ALSO
                                    TNS-CDKBN-KOUBAN
      *                      療養病棟入院基本料（特別入院基本料）
                             WHEN   101   ALSO   03
      *                      有床診療所療養病床入院基本料Ａ
                             WHEN   109   ALSO   01
      *                      有床診療所療養病床入院基本料Ｂ
                             WHEN   109   ALSO   02
      *                      有床診療所療養病床入院基本料Ｃ
                             WHEN   109   ALSO   03
      *                      有床診療所療養病床入院基本料Ｄ
                             WHEN   109   ALSO   04
      *                      有床診療所療養病床入院基本料Ｅ
                             WHEN   109   ALSO   05
      *                      有床診療所療養病床入院基本料（特別入院基本料）
                             WHEN   109   ALSO   06
      *                      療養病床以外・有床診療所療養病床入院
                             WHEN   108   ALSO   13
      *                      療養病棟入院料１
                             WHEN   101   ALSO   01
      *                      療養病棟入院料２
                             WHEN   101   ALSO   02
      *                      療養病棟入院料２（経過措置１）
                             WHEN   101   ALSO   10
      *                      療養病棟入院料２（経過措置２）
                             WHEN   101   ALSO   11
      *                      一般病棟・療養病棟入院基本料１
                             WHEN   100   ALSO   14
      *                      特定一般病棟・療養病棟入院基本料１
                             WHEN   317   ALSO   08
                               PERFORM  VARYING   IDX1 FROM 1 BY 1
                                 UNTIL  ( IDX1     >   31 )
                                 IF  (NACCT-DAY(IDX1)    =   1)   AND
                                     (WRK-GAIHAKU(IDX1)  =   SPACE) AND
                                     (WRK-TOKUGAI(IDX1)  =   SPACE)
                                      MOVE "1"  TO  LNK-RYOCOM-NYUINKBN
                                                                (IDX1)
                                      MOVE  1     TO  FLG-RYOYO-NYUIN
                                 END-IF
                               END-PERFORM
                           END-EVALUATE
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"   TO  MCP-TABLE
                   MOVE    "key"         TO  MCP-PATHNAME
                   PERFORM  910-DB-CLOSE-SEC
               END-IF
      *        令和２年４月からの入院チェック
               IF     LNK-RYOCOM-SANTEIYM   >=  "202004"
                   INITIALIZE                       TNS-TENSU-REC
                   MOVE   LNK-RYOCOM-SANTEIYM  TO  TNS-YUKOSTYMD (1:6)
                   MOVE   "01"                 TO  TNS-YUKOSTYMD (7:2)
                   MOVE   TNS-YUKOSTYMD        TO  TNS-YUKOEDYMD
                   MOVE   LNK-RYOCOM-HOSPNUM   TO  TNS-HOSPNUM
                   MOVE   NSRY-SRYCD(1)        TO  TNS-SRYCD
                   MOVE   TNS-TENSU-REC        TO  MCPDATA-REC
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE  "tbl_tensu"       TO  MCP-TABLE
                       MOVE  "key"             TO  MCP-PATHNAME
                       PERFORM 910-DBFETCH-SEC
                       IF      MCP-RC          =   ZERO
                           MOVE  MCPDATA-REC   TO  TNS-TENSU-REC
      *                    入院基本料加算区分からの判定
                           EVALUATE TNS-CDKBN-KBNNUM    ALSO
                                    TNS-CDKBN-KOUBAN
      *                      療養病棟入院料１
                             WHEN   101   ALSO   01
      *                      療養病棟入院料２
                             WHEN   101   ALSO   02
      *                      療養病棟入院基本料（特別入院基本料）
                             WHEN   101   ALSO   03
      *                      療養病棟入院料２（経過措置）
                             WHEN   101   ALSO   10
      *                      有床診療所療養病床入院基本料Ａ
                             WHEN   109   ALSO   01
      *                      有床診療所療養病床入院基本料Ｂ
                             WHEN   109   ALSO   02
      *                      有床診療所療養病床入院基本料Ｃ
                             WHEN   109   ALSO   03
      *                      有床診療所療養病床入院基本料Ｄ
                             WHEN   109   ALSO   04
      *                      有床診療所療養病床入院基本料Ｅ
                             WHEN   109   ALSO   05
      *                      有床診療所療養病床入院基本料（特別入院基本料）
                             WHEN   109   ALSO   06
      *                      療養病床以外・有床診療所療養病床入院
                             WHEN   108   ALSO   13
      *                      一般病棟・療養病棟入院基本料１
                             WHEN   100   ALSO   14
      *                      特定一般病棟・療養病棟入院基本料１
                             WHEN   317   ALSO   08
                               PERFORM  VARYING   IDX1 FROM 1 BY 1
                                 UNTIL  ( IDX1     >   31 )
                                 IF  (NACCT-DAY(IDX1)    =   1)   AND
                                     (WRK-GAIHAKU(IDX1)  =   SPACE) AND
                                     (WRK-TOKUGAI(IDX1)  =   SPACE)
                                      MOVE "1"  TO  LNK-RYOCOM-NYUINKBN
                                                                (IDX1)
                                      MOVE  1     TO  FLG-RYOYO-NYUIN
                                 END-IF
                               END-PERFORM
                           END-EVALUATE
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"   TO  MCP-TABLE
                   MOVE    "key"         TO  MCP-PATHNAME
                   PERFORM  910-DB-CLOSE-SEC
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DB-CLOSE-SEC
      *
           .
       100-PRM-RYOYO-NYUINCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主取得
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *    システム管理（５０１５）からＡＤＬ点数の入力方法を取得
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5015"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    LNK-RYOCOM-SANTEIYM TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    LNK-RYOCOM-HOSPNUM  TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
                                  SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM  910-SYSKANRI-READ-SEC
               IF    FLG-SYSKANRI   =    ZERO
                   IF   SYS-5015-ADLINPUTKBN   =   "1"  OR  SPACE
                       MOVE     1      TO  FLG-1015-ADLINPUTKBN
                   ELSE
                       MOVE     2      TO  FLG-1015-ADLINPUTKBN
                   END-IF
               ELSE
                   MOVE     1      TO  FLG-1015-ADLINPUTKBN
               END-IF
           END-IF
      *    システム管理クローズ
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *    平成２２年４月以降はＡＤＬ点数入力は区分別に入力
           IF     LNK-RYOCOM-SANTEIYM    >=    "201004"
               MOVE     1              TO  FLG-1015-ADLINPUTKBN
           END-IF
      *
      *    療養レセプトコメントマスタの情報取得
           INITIALIZE                      RYOUYOCOM-REC
           MOVE    LNK-RYOCOM-HOSPNUM  TO  RYOCOM-HOSPNUM
           MOVE    "1"                 TO  RYOCOM-NYUGAIKBN
           MOVE    LNK-RYOCOM-PTID     TO  RYOCOM-PTID
           MOVE    LNK-RYOCOM-SANTEIYM TO  RYOCOM-SRYYM
           MOVE    "tbl_ryocom"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-RYOUYOCOM-SELECT-SEC
           IF      FLG-RYOUYOCOM       NOT =   ZERO
      *         MOVE    5               TO  LNK-RYOCOM-RC
               CONTINUE
           ELSE
               MOVE    "tbl_ryocom"    TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-RYOUYOCOM-READ-SEC
               IF   FLG-RYOUYOCOM      NOT =   ZERO
      *              MOVE    5          TO  LNK-RYOCOM-RC
                    CONTINUE
               ELSE
                    PERFORM   UNTIL  FLG-RYOUYOCOM    =   1
                        PERFORM   210-RYOCOM-REC-SEC
                        MOVE    "tbl_ryocom"      TO  MCP-TABLE
                        MOVE    "key2"            TO  MCP-PATHNAME
                        PERFORM 910-RYOUYOCOM-READ-SEC
                    END-PERFORM
               END-IF
           END-IF
      *    療養レセプトコメントクローズ
           MOVE    "tbl_ryocom"            TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *
      *    平成２０年４月以降分の医療区分コメント並び替え
           IF    LNK-RYOCOM-SANTEIYM       >=   "200804"
                PERFORM   220-RYOCOM-SORT-SEC
           END-IF
      *
      *
      *    取得したＡＤＬ点数を全角変換（平成２２年３月までの記録）
           IF    LNK-RYOCOM-SANTEIYM       <=   "201003"
             PERFORM   VARYING   IDX   FROM  1  BY  1
               UNTIL   IDX    >   31
                 IF   LNK-RYOCOM-NYUINKBN(IDX)  =  "1"
                    MOVE   "840000103"   TO   LNK-RYOCOM-ADL-SRYCD(IDX)
                    ADD    1             TO   LNK-RYOCOM-MEISAISU(IDX)
                    IF   LNK-RYOCOM-ADL-TEN(IDX)   =  ZERO
                        MOVE  "００"     TO
                                         LNK-RYOCOM-ADL-TEN-MOJI(IDX)
                    ELSE
                        MOVE  LNK-RYOCOM-ADL-TEN(IDX)   TO  IDX1
                        MOVE  TBL-ADLTEN(IDX1)
                                     TO  LNK-RYOCOM-ADL-TEN-MOJI(IDX)
                    END-IF
                 END-IF
             END-PERFORM
           END-IF
      *
      *    取得したＡＤＬ点数を全角変換（平成２２年４月からの記録）
           IF    LNK-RYOCOM-SANTEIYM       >=   "201004"
             PERFORM   VARYING   IDX   FROM  1  BY  1
               UNTIL   IDX    >   31
                 IF   LNK-RYOCOM-NYUINKBN(IDX)  =  "1"
                    ADD   4              TO   LNK-RYOCOM-MEISAISU(IDX)
      *             ＡＤＬａ　ベッド上の可動性
                    MOVE    "840000126"  TO
                                       LNK-RYOCOM-ADL-SRYCD-H22(IDX 1)
                    IF   LNK-RYOCOM-ADL-TEN-H22(IDX 1)   =  ZERO
                        MOVE  "００"     TO
                                     LNK-RYOCOM-ADL-TEN-MOJI-H22(IDX 1)
                    ELSE
                        MOVE  LNK-RYOCOM-ADL-TEN-H22(IDX 1)   TO  IDX1
                        MOVE  TBL-ADLTEN(IDX1)  TO
                                     LNK-RYOCOM-ADL-TEN-MOJI-H22(IDX 1)
                    END-IF
      *             ＡＤＬｂ　移乗
                    MOVE    "840000127"  TO
                                       LNK-RYOCOM-ADL-SRYCD-H22(IDX 2)
                    IF   LNK-RYOCOM-ADL-TEN-H22(IDX 2)   =  ZERO
                        MOVE  "００"     TO
                                     LNK-RYOCOM-ADL-TEN-MOJI-H22(IDX 2)
                    ELSE
                        MOVE  LNK-RYOCOM-ADL-TEN-H22(IDX 2)   TO  IDX1
                        MOVE  TBL-ADLTEN(IDX1)  TO
                                     LNK-RYOCOM-ADL-TEN-MOJI-H22(IDX 2)
                    END-IF
      *             ＡＤＬｃ　食事
                    MOVE    "840000128"  TO
                                       LNK-RYOCOM-ADL-SRYCD-H22(IDX 3)
                    IF   LNK-RYOCOM-ADL-TEN-H22(IDX 3)   =  ZERO
                        MOVE  "００"     TO
                                     LNK-RYOCOM-ADL-TEN-MOJI-H22(IDX 3)
                    ELSE
                        MOVE  LNK-RYOCOM-ADL-TEN-H22(IDX 3)   TO  IDX1
                        MOVE  TBL-ADLTEN(IDX1)  TO
                                     LNK-RYOCOM-ADL-TEN-MOJI-H22(IDX 3)
                    END-IF
      *             ＡＤＬｄ　トイレの使用
                    MOVE    "840000129"  TO
                                       LNK-RYOCOM-ADL-SRYCD-H22(IDX 4)
                    IF   LNK-RYOCOM-ADL-TEN-H22(IDX 4)   =  ZERO
                        MOVE  "００"     TO
                                     LNK-RYOCOM-ADL-TEN-MOJI-H22(IDX 4)
                    ELSE
                        MOVE  LNK-RYOCOM-ADL-TEN-H22(IDX 4)   TO  IDX1
                        MOVE  TBL-ADLTEN(IDX1)  TO
                                     LNK-RYOCOM-ADL-TEN-MOJI-H22(IDX 4)
                    END-IF
                 END-IF
             END-PERFORM
           END-IF
      *
      *    診療コード計の取得
           PERFORM   VARYING   IDX   FROM  1  BY  1
             UNTIL   IDX    >   31
               IF   LNK-RYOCOM-NYUINKBN(IDX)  =  "1"
                  PERFORM   VARYING   IDX1   FROM   1  BY  1
                    UNTIL  (IDX1   >   30)    OR
                           (LNK-RYOCOM-IRYO-SRYCD(IDX IDX1) = SPACE)
                        MOVE   LNK-RYOCOM-IRYO-SRYCD(IDX IDX1)
                                           TO    WRK-SRYCD-X
      *                  ADD    WRK-SRYCD-9 TO
      *                              LNK-RYOCOM-SRYCDTOTAL(IDX)
                        COMPUTE  LNK-RYOCOM-SRYCDTOTAL(IDX)  =
                                 LNK-RYOCOM-SRYCDTOTAL(IDX)  +
                                (WRK-SRYCD-9  *  LNK-RYOCOM-IRYO-ZAINUM
                                                    (IDX IDX1))
                  END-PERFORM
                  IF    LNK-RYOCOM-SANTEIYM       <=   "201003"
                    MOVE   LNK-RYOCOM-ADL-SRYCD(IDX)  TO  WRK-SRYCD-X
                    ADD    WRK-SRYCD-9   TO  LNK-RYOCOM-SRYCDTOTAL(IDX)
                    COMPUTE   LNK-RYOCOM-SRYCDTOTAL2(IDX)   =
                              LNK-RYOCOM-SRYCDTOTAL(IDX)   +
                              LNK-RYOCOM-ADL-TEN(IDX)
                  ELSE
      **              DISPLAY "==============================="
      **              DISPLAY "IDX = " IDX
      **              DISPLAY "LNK-RYOCOM-ADL-TEN-H22(IDX 1) = "
      **                      LNK-RYOCOM-ADL-TEN-H22(IDX 1)
      **              DISPLAY "LNK-RYOCOM-ADL-TEN-H22(IDX 2) = "
      **                       LNK-RYOCOM-ADL-TEN-H22(IDX 2)
      **              DISPLAY "LNK-RYOCOM-ADL-TEN-H22(IDX 3) = "
      **                       LNK-RYOCOM-ADL-TEN-H22(IDX 3)
      **              DISPLAY "LNK-RYOCOM-ADL-TEN-H22(IDX 4) = "
      **                       LNK-RYOCOM-ADL-TEN-H22(IDX 4)
                    MOVE   LNK-RYOCOM-SRYCDTOTAL(IDX)
                                         TO  LNK-RYOCOM-SRYCDTOTAL2(IDX)
      *             ＡＤＬａ　ベッド上の可動性
                    MOVE   LNK-RYOCOM-ADL-SRYCD-H22(IDX 1)
                                         TO  WRK-SRYCD-X
                    ADD    WRK-SRYCD-9   TO  LNK-RYOCOM-SRYCDTOTAL(IDX)
                    ADD    WRK-SRYCD-9   TO  LNK-RYOCOM-SRYCDTOTAL2(IDX)
                    ADD    LNK-RYOCOM-ADL-TEN-H22(IDX 1)
                                         TO  LNK-RYOCOM-SRYCDTOTAL2(IDX)
      *             ＡＤＬｂ　移乗
                    MOVE   LNK-RYOCOM-ADL-SRYCD-H22(IDX 2)
                                         TO  WRK-SRYCD-X
                    ADD    WRK-SRYCD-9   TO  LNK-RYOCOM-SRYCDTOTAL(IDX)
                    ADD    WRK-SRYCD-9   TO  LNK-RYOCOM-SRYCDTOTAL2(IDX)
                    ADD    LNK-RYOCOM-ADL-TEN-H22(IDX 2)
                                         TO  LNK-RYOCOM-SRYCDTOTAL2(IDX)
      *             ＡＤＬｃ　食事
                    MOVE   LNK-RYOCOM-ADL-SRYCD-H22(IDX 3)
                                         TO  WRK-SRYCD-X
                    ADD    WRK-SRYCD-9   TO  LNK-RYOCOM-SRYCDTOTAL(IDX)
                    ADD    WRK-SRYCD-9   TO  LNK-RYOCOM-SRYCDTOTAL2(IDX)
                    ADD    LNK-RYOCOM-ADL-TEN-H22(IDX 3)
                                         TO  LNK-RYOCOM-SRYCDTOTAL2(IDX)
      *             ＡＤＬｄ　トイレの使用
                    MOVE   LNK-RYOCOM-ADL-SRYCD-H22(IDX 4)
                                         TO  WRK-SRYCD-X
                    ADD    WRK-SRYCD-9   TO  LNK-RYOCOM-SRYCDTOTAL(IDX)
                    ADD    WRK-SRYCD-9   TO  LNK-RYOCOM-SRYCDTOTAL2(IDX)
                    ADD    LNK-RYOCOM-ADL-TEN-H22(IDX 4)
                                         TO  LNK-RYOCOM-SRYCDTOTAL2(IDX)
                  END-IF
               END-IF
           END-PERFORM
      *
      **     PERFORM     VARYING   IDX1    FROM  1  BY  1
      **       UNTIL     IDX1   >  31
      **           DISPLAY "IDX1 = " IDX1
      **           DISPLAY "LNK-RYOCOM-SRYCDTOTAL(IDX1) = "
      **                    LNK-RYOCOM-SRYCDTOTAL(IDX1)
      **           DISPLAY "LNK-RYOCOM-SRYCDTOTAL2(IDX1) = "
      **                    LNK-RYOCOM-SRYCDTOTAL2(IDX1)
      **     END-PERFORM
      *
      *    ＡＤＬ点数のコード変換（平成２８年４月からの新設コード返却用）
           IF    LNK-RYOCOM-SANTEIYM       >=   "201604"
             PERFORM   VARYING   IDX   FROM  1  BY  1
               UNTIL   IDX    >   31
                 IF   LNK-RYOCOM-NYUINKBN(IDX)  =  "1"
      *             ＡＤＬａ　ベッド上の可動性
                    IF      LNK-RYOCOM-ADL-TEN-H22(IDX 1)   =  ZERO
                        MOVE  CONS-ADL-A-COMCD(7)
                                    TO  LNK-RYOCOM-ADL-SRYCD-H28(IDX 1)
                    ELSE
                        MOVE  LNK-RYOCOM-ADL-TEN-H22(IDX 1)   TO  IDX1
                        MOVE  CONS-ADL-A-COMCD(IDX1) 
                                    TO  LNK-RYOCOM-ADL-SRYCD-H28(IDX 1)
                    END-IF
      *             ＡＤＬｂ　移乗
                    IF      LNK-RYOCOM-ADL-TEN-H22(IDX 2)   =  ZERO
                        MOVE  CONS-ADL-B-COMCD(7)
                                    TO  LNK-RYOCOM-ADL-SRYCD-H28(IDX 2)
                    ELSE
                        MOVE  LNK-RYOCOM-ADL-TEN-H22(IDX 2)   TO  IDX1
                        MOVE  CONS-ADL-B-COMCD(IDX1) 
                                    TO  LNK-RYOCOM-ADL-SRYCD-H28(IDX 2)
                    END-IF
      *             ＡＤＬｃ　食事
                    IF      LNK-RYOCOM-ADL-TEN-H22(IDX 3)   =  ZERO
                        MOVE  CONS-ADL-C-COMCD(7)
                                    TO  LNK-RYOCOM-ADL-SRYCD-H28(IDX 3)
                    ELSE
                        MOVE  LNK-RYOCOM-ADL-TEN-H22(IDX 3)   TO  IDX1
                        MOVE  CONS-ADL-C-COMCD(IDX1) 
                                    TO  LNK-RYOCOM-ADL-SRYCD-H28(IDX 3)
                    END-IF
      *             ＡＤＬｄ　トイレの使用
                    IF      LNK-RYOCOM-ADL-TEN-H22(IDX 4)   =  ZERO
                        MOVE  CONS-ADL-D-COMCD(7)
                                    TO  LNK-RYOCOM-ADL-SRYCD-H28(IDX 4)
                    ELSE
                        MOVE  LNK-RYOCOM-ADL-TEN-H22(IDX 4)   TO  IDX1
                        MOVE  CONS-ADL-D-COMCD(IDX1) 
                                    TO  LNK-RYOCOM-ADL-SRYCD-H28(IDX 4)
                    END-IF
                 END-IF
             END-PERFORM
           END-IF
      *
      **     PERFORM     VARYING   IDX    FROM  1  BY  1
      **       UNTIL     IDX   >  31
      **           DISPLAY "-----------(H28 COMMENT-----------"
      **           DISPLAY "IDX = " IDX
      **           DISPLAY "LNK-RYOCOM-ADL-SRYCD-H28(IDX 1) = "
      **                    LNK-RYOCOM-ADL-SRYCD-H28(IDX 1)
      **           DISPLAY "LNK-RYOCOM-ADL-SRYCD-H28(IDX 2) = "
      **                    LNK-RYOCOM-ADL-SRYCD-H28(IDX 2)
      **           DISPLAY "LNK-RYOCOM-ADL-SRYCD-H28(IDX 3) = "
      **                    LNK-RYOCOM-ADL-SRYCD-H28(IDX 3)
      **           DISPLAY "LNK-RYOCOM-ADL-SRYCD-H28(IDX 4) = "
      **                    LNK-RYOCOM-ADL-SRYCD-H28(IDX 4)
      **     END-PERFORM
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    療養レセコメントの記録
      *****************************************************************
       210-RYOCOM-REC-SEC            SECTION.
      *
      *    ＡＤＬ点数集計
           IF     FLG-1015-ADLINPUTKBN    =    1
             IF   (RYOCOM-ZAINUM       >=  1000)     AND
                  (RYOCOM-ZAINUM       <=  1999)
               IF   LNK-RYOCOM-SANTEIYM    <=   "201003"
      *            平成２２年３月までの集計
                   IF   RYOCOM-MONTH       NOT =     ZERO
                       PERFORM   VARYING  IDX   FROM  1  BY  1
                         UNTIL   IDX      >   31
                           IF   LNK-RYOCOM-NYUINKBN(IDX)  =  "1"
                               ADD   RYOCOM-MONTH  TO
                                            LNK-RYOCOM-ADL-TEN(IDX)
                           END-IF
                       END-PERFORM
                   ELSE
                       PERFORM   VARYING  IDX   FROM  1  BY  1
                         UNTIL   IDX      >   31
                           IF   (LNK-RYOCOM-NYUINKBN(IDX)  =  "1")  AND
                                (RYOCOM-DAY(IDX)      NOT =   ZERO)
                                ADD  RYOCOM-DAY(IDX)   TO
                                            LNK-RYOCOM-ADL-TEN(IDX)
                           END-IF
                       END-PERFORM
                   END-IF
               ELSE
      *            平成２２年４月からの集計
                   IF   RYOCOM-MONTH       NOT =     ZERO
                       PERFORM   VARYING  IDX   FROM  1  BY  1
                         UNTIL   IDX      >   31
                           IF   LNK-RYOCOM-NYUINKBN(IDX)  =  "1"
                               EVALUATE   RYOCOM-ZAINUM
                                   WHEN   1001
      *                               ＡＤＬａ　ベッド上の可動性
                                      ADD   RYOCOM-MONTH  TO
                                          LNK-RYOCOM-ADL-TEN-H22(IDX 1)
                                   WHEN   1002
      *                               ＡＤＬｂ　移乗
                                      ADD   RYOCOM-MONTH  TO
                                          LNK-RYOCOM-ADL-TEN-H22(IDX 2)
                                   WHEN   1003
      *                               ＡＤＬｃ　食事
                                      ADD   RYOCOM-MONTH  TO
                                          LNK-RYOCOM-ADL-TEN-H22(IDX 3)
                                   WHEN   1004
      *                               ＡＤＬｄ　トイレの使用
                                      ADD   RYOCOM-MONTH  TO
                                          LNK-RYOCOM-ADL-TEN-H22(IDX 4)
                               END-EVALUATE
                           END-IF
                       END-PERFORM
                   ELSE
                       PERFORM   VARYING  IDX   FROM  1  BY  1
                         UNTIL   IDX      >   31
                           IF   (LNK-RYOCOM-NYUINKBN(IDX)  =  "1")  AND
                                (RYOCOM-DAY(IDX)      NOT =   ZERO)
                               EVALUATE   RYOCOM-ZAINUM
                                   WHEN   1001
      *                               ＡＤＬａ　ベッド上の可動性
                                       ADD  RYOCOM-DAY(IDX)   TO
                                          LNK-RYOCOM-ADL-TEN-H22(IDX 1)
                                   WHEN   1002
      *                               ＡＤＬｂ　移乗
                                       ADD  RYOCOM-DAY(IDX)   TO
                                          LNK-RYOCOM-ADL-TEN-H22(IDX 2)
                                   WHEN   1003
      *                               ＡＤＬｃ　食事
                                       ADD  RYOCOM-DAY(IDX)   TO
                                          LNK-RYOCOM-ADL-TEN-H22(IDX 3)
                                   WHEN   1004
      *                               ＡＤＬｄ　トイレの使用
                                       ADD  RYOCOM-DAY(IDX)   TO
                                          LNK-RYOCOM-ADL-TEN-H22(IDX 4)
                               END-EVALUATE
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
             END-IF
           ELSE
             IF   (RYOCOM-ZAINUM       >=  3000)     AND
                  (RYOCOM-ZAINUM       <=  3999)
               IF   RYOCOM-MONTH       NOT =     ZERO
                   PERFORM   VARYING  IDX   FROM  1  BY  1
                     UNTIL   IDX      >   31
                       IF   LNK-RYOCOM-NYUINKBN(IDX)  =  "1"
                           ADD   RYOCOM-MONTH  TO
                                        LNK-RYOCOM-ADL-TEN(IDX)
                       END-IF
                   END-PERFORM
               ELSE
                   PERFORM   VARYING  IDX   FROM  1  BY  1
                     UNTIL   IDX      >   31
                       IF   (LNK-RYOCOM-NYUINKBN(IDX)  =  "1")  AND
                            (RYOCOM-DAY(IDX)      NOT =   ZERO)
                           ADD  RYOCOM-DAY(IDX)   TO
                                        LNK-RYOCOM-ADL-TEN(IDX)
                       END-IF
                   END-PERFORM
               END-IF
             END-IF
           END-IF
      *
      *    医療区分集計
           IF     (RYOCOM-ZAINUM       >=  2000)     AND
                  (RYOCOM-ZAINUM       <=  2999)
             IF    (RYOCOM-SRYYM       >=  "201204")   AND
                   (RYOCOM-SRYCD       =   "820000158")
      *        「９１　褥瘡に対する治療を実施している状態」は
      *          改正により平成２４年４月以降は処理対象外とする
                 CONTINUE
             ELSE
               IF   RYOCOM-MONTH       NOT =     ZERO
                   PERFORM   VARYING  IDX   FROM  1  BY  1
                     UNTIL   IDX      >   31
                       IF   LNK-RYOCOM-NYUINKBN(IDX)  =  "1"
                           ADD    1   TO    LNK-RYOCOM-MEISAISU(IDX)
                           MOVE   LNK-RYOCOM-MEISAISU(IDX)  TO  IDX2
                           IF     LNK-RYOCOM-SANTEIYM  <=   "200803"
                               MOVE   RYOCOM-SRYCD  TO
                                       LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
                           ELSE
                               PERFORM   2101-RYOCOM-CHG-SEC
                           END-IF
                           MOVE   RYOCOM-ZAINUM  TO
                                       LNK-RYOCOM-IRYO-ZAINUM(IDX IDX2)
                       END-IF
                   END-PERFORM
               ELSE
                   PERFORM   VARYING  IDX   FROM  1  BY  1
                     UNTIL   IDX      >   31
                       IF   (LNK-RYOCOM-NYUINKBN(IDX)  =  "1")  AND
                            (RYOCOM-DAY(IDX)      NOT =   ZERO)
                           ADD    1   TO    LNK-RYOCOM-MEISAISU(IDX)
                           MOVE   LNK-RYOCOM-MEISAISU(IDX)  TO  IDX2
                           IF     LNK-RYOCOM-SANTEIYM  <=   "200803"
                               MOVE   RYOCOM-SRYCD  TO
                                       LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
                           ELSE
                               PERFORM   2101-RYOCOM-CHG-SEC
                           END-IF
                           MOVE   RYOCOM-ZAINUM  TO
                                       LNK-RYOCOM-IRYO-ZAINUM(IDX IDX2)
                       END-IF
                   END-PERFORM
               END-IF
             END-IF
           END-IF
      *
           .
       210-RYOCOM-REC-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療区分コメントの置換え（平成２０年４月改定より）
      *    （　　）内の数字は改定時点の最新の医療区分
      *****************************************************************
       2101-RYOCOM-CHG-SEC            SECTION.
      *
           EVALUATE   RYOCOM-SRYCD
      *      （１０）
             WHEN   "820000057"
                MOVE  "820000105"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （１２）
             WHEN   "820000058"
                MOVE  "820000107"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （１３）
             WHEN   "820000059"
                MOVE  "820000108"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （１）
             WHEN   "820000060"
                MOVE  "820000096"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （１４）
             WHEN   "820000061"
                MOVE  "820000109"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （１５）
             WHEN   "820000062"
                MOVE  "820000110"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （１６）
             WHEN   "820000063"
                MOVE  "820000111"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （１７）
             WHEN   "820000064"
      *         平成２８年３月まで 820000112
      *         平成２８年４月から 820000229
                IF  RYOCOM-SRYYM   <=   "201603"
                    MOVE  "820000112"
                                   TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
                ELSE
                    MOVE  "820000229"
                                   TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
                END-IF
      *      （１８）
             WHEN   "820000065"
                MOVE  "820000113"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （１１）
             WHEN   "820000066"
                MOVE  "820000106"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （１９）
             WHEN   "820000067"
      *         平成２８年３月まで 820000114
      *         平成２８年４月から 820000230
                IF  RYOCOM-SRYYM   <=   "201603"
                    MOVE  "820000114"
                                   TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
                ELSE
                    MOVE  "820000230"
                                   TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
                END-IF
      *      （２０）
             WHEN   "820000068"
                MOVE  "820000115"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （２１）
             WHEN   "820000069"
                MOVE  "820000116"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （２２）
             WHEN   "820000070"
                MOVE  "820000117"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （２３）
             WHEN   "820000071"
      *         平成２８年３月まで 820000118
      *         平成２８年４月から 820000231
                IF  RYOCOM-SRYYM   <=   "201603"
                    MOVE  "820000118"
                                   TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
                ELSE
                    MOVE  "820000231"
                                   TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
                END-IF
      *      （２４）
             WHEN   "820000072"
                MOVE  "820000119"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （２５）
             WHEN   "820000073"
                MOVE  "820000120"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （２９）
             WHEN   "820000074"
                MOVE  "820000124"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （３０）
             WHEN   "820000075"
                MOVE  "820000125"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （２）
             WHEN   "820000076"
                MOVE  "820000097"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （３）
             WHEN   "820000077"
                MOVE  "820000098"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （４）
             WHEN   "820000078"
                MOVE  "820000099"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （５）
             WHEN   "820000079"
                MOVE  "820000100"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （６）
             WHEN   "820000080"
                MOVE  "820000101"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （３１）
             WHEN   "820000081"
                MOVE  "820000126"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （３２）
             WHEN   "820000082"
                MOVE  "820000127"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （７）
             WHEN   "820000083"
                MOVE  "820000102"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （３３）
             WHEN   "820000084"
                MOVE  "820000128"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （３４）
             WHEN   "820000085"
                MOVE  "820000129"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （２６）
             WHEN   "820000086"
                MOVE  "820000121"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （８）
             WHEN   "820000087"
                MOVE  "820000103"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （３５）
             WHEN   "820000088"
                MOVE  "820000130"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （３６）
             WHEN   "820000089"
                MOVE  "820000131"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （９）
             WHEN   "820000090"
                MOVE  "820000104"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （３７）
             WHEN   "820000091"
                MOVE  "820000132"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （２７）
             WHEN   "820000092"
                MOVE  "820000122"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （２８）
             WHEN   "820000093"
                MOVE  "820000123"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （３８）  平成２８年４月から
             WHEN   "820000232"
                MOVE  "820000232"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （３９）  平成３０年４月から
             WHEN   "82xxxxxxx"
                MOVE  "820000241"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （８１）  平成３０年４月から
             WHEN   "201804001"
                MOVE  "820000233"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （８２）  平成３０年４月から
             WHEN   "201804002"
                MOVE  "820000234"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （８３）  平成３０年４月から
             WHEN   "201804003"
                MOVE  "820000235"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （８４）  平成３０年４月から
             WHEN   "201804004"
                MOVE  "820000236"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （８５）  平成３０年４月から
             WHEN   "201804005"
                MOVE  "820000237"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （８６）  平成３０年４月から
             WHEN   "201804006"
                MOVE  "820000238"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      （８７）  令和２年４月から
             WHEN   "829000001"
                MOVE  "820000243"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
      *      
      *      （９１）  平成２２年４月から平成２４年３月
             WHEN   "820000158"
                MOVE  "820000158"  TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
             WHEN   "820000159"
      *         平成２４年４月以降は "820000159" ==> "820000160"
      *        （９２）
                IF  RYOCOM-SRYYM   <=   "201203"
                    MOVE  "820000159"
                                   TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
                ELSE
      *        （９１）
                    MOVE  "820000160"
                                   TO  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
                END-IF
           END-EVALUATE
      *
           IF     LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)   NOT =    SPACE
               INITIALIZE                      TNS-TENSU-REC
               MOVE   LNK-RYOCOM-SANTEIYM  TO  TNS-YUKOSTYMD (1:6)
               MOVE   "01"                 TO  TNS-YUKOSTYMD (7:2)
               MOVE   TNS-YUKOSTYMD        TO  TNS-YUKOEDYMD
               MOVE   LNK-RYOCOM-HOSPNUM   TO  TNS-HOSPNUM
               MOVE   LNK-RYOCOM-IRYO-SRYCD(IDX IDX2)
                                           TO  TNS-SRYCD
               MOVE   TNS-TENSU-REC        TO  MCPDATA-REC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE  "tbl_tensu"       TO  MCP-TABLE
                   MOVE  "key"             TO  MCP-PATHNAME
                   PERFORM 910-DBFETCH-SEC
                   IF      MCP-RC          =   ZERO
                       MOVE  MCPDATA-REC   TO  TNS-TENSU-REC
                       MOVE  TNS-NAME(1:4) TO  LNK-RYOCOM-IRYO-SORT-COM
                                                            (IDX IDX2)
                   ELSE
                       IF  LNK-RYOCOM-IRYO-SRYCD(IDX IDX2) = "820000243"
                           MOVE   "８７" TO  LNK-RYOCOM-IRYO-SORT-COM
                                                            (IDX IDX2)
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_tensu"   TO  MCP-TABLE
               MOVE    "key"         TO  MCP-PATHNAME
               PERFORM  910-DB-CLOSE-SEC
           END-IF
           .
       2101-RYOCOM-CHG-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療区分コメントの並び替え（平成２０年４月改定より）
      *****************************************************************
       220-RYOCOM-SORT-SEC            SECTION.
      *
           PERFORM    VARYING    SIDX1    FROM   1   BY   1
             UNTIL    SIDX1       >       31
               PERFORM   VARYING   SIDX2    FROM   1   BY   1
                 UNTIL  (SIDX2     >     29)  OR
                        (LNK-RYOCOM-IRYO-SRYCD(SIDX1 SIDX2) = SPACE)
                   MOVE      ZERO      TO    WRK-IDX
                   COMPUTE   WRK-IDX   =     SIDX2    +   1
                   PERFORM   VARYING   SIDX3   FROM  WRK-IDX   BY   1
                     UNTIL (SIDX3    >   30)    OR
                           (LNK-RYOCOM-IRYO-SRYCD(SIDX1 SIDX3) = SPACE)
                         IF   LNK-RYOCOM-IRYO-SORT-COM(SIDX1 SIDX2)
                                >  LNK-RYOCOM-IRYO-SORT-COM(SIDX1 SIDX3)
      *                       SIDX2からワークへの退避
                              MOVE  LNK-RYOCOM-IRYO-SORT-COM
                                                         (SIDX1 SIDX2)
                                            TO  WRK-RYOCOM-IRYO-SORT-COM
                              MOVE  LNK-RYOCOM-IRYO-SRYCD(SIDX1 SIDX2)
                                            TO  WRK-RYOCOM-IRYO-SRYCD
                              MOVE  LNK-RYOCOM-IRYO-ZAINUM(SIDX1 SIDX2)
                                            TO  WRK-RYOCOM-IRYO-ZAINUM
      *                       SIDX3からSIDX2への移入
                              MOVE  LNK-RYOCOM-IRYO-SORT-COM
                                                      (SIDX1 SIDX3)
                                            TO
                                    LNK-RYOCOM-IRYO-SORT-COM
                                                      (SIDX1 SIDX2)
                              MOVE  LNK-RYOCOM-IRYO-SRYCD(SIDX1 SIDX3)
                                            TO
                                    LNK-RYOCOM-IRYO-SRYCD(SIDX1 SIDX2)
                              MOVE  LNK-RYOCOM-IRYO-ZAINUM(SIDX1 SIDX3)
                                            TO
                                    LNK-RYOCOM-IRYO-ZAINUM(SIDX1 SIDX2)
      *                       ワークからSIDX3への移入
                              MOVE  WRK-RYOCOM-IRYO-SORT-COM
                                            TO
                                    LNK-RYOCOM-IRYO-SORT-COM
                                                     (SIDX1 SIDX3)
                              MOVE  WRK-RYOCOM-IRYO-SRYCD
                                            TO
                                    LNK-RYOCOM-IRYO-SRYCD(SIDX1 SIDX3)
                              MOVE  WRK-RYOCOM-IRYO-ZAINUM
                                            TO
                                    LNK-RYOCOM-IRYO-ZAINUM(SIDX1 SIDX3)
                         END-IF
                   END-PERFORM
               END-PERFORM
           END-PERFORM
      *
           .
       220-RYOCOM-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"   USING MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL "ORCDBMAIN" USING MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE    1           TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター検索
      *****************************************************************
       910-PTNYUINRRK-SELECT-SEC        SECTION.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"   USING MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"   USING MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTNYUINRRK-REC
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                     PTNYUINRRK-REC
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター読込
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"   USING MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-5000-REC
                                               SYS-5015-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                      SYS-5000-REC
                                               SYS-5015-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター検索
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"   USING MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター読込
      *****************************************************************
       910-NYUINACCT-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"   USING MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  NYUINACCT-REC
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                     NYUINACCT-REC
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為マスター検索
      *****************************************************************
       910-NYUINACT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"   USING MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACT
           ELSE
               MOVE    1                  TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為マスター読込
      *****************************************************************
       910-NYUINACT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"   USING MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    療養レセプトコメントマスタ−検索
      *****************************************************************
       910-RYOUYOCOM-SELECT-SEC        SECTION.
      *
           MOVE    RYOUYOCOM-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"         TO  MCP-FUNC
           CALL    "ORCDBMAIN"  USING MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-RYOUYOCOM
           ELSE
               MOVE    1                  TO  FLG-RYOUYOCOM
           END-IF
      *
           .
       910-RYOUYOCOM-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    療養レセプトコメントマスタ−読込
      *****************************************************************
       910-RYOUYOCOM-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL "ORCDBMAIN"      USING MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  RYOUYOCOM-REC
               MOVE    ZERO               TO  FLG-RYOUYOCOM
           ELSE
               INITIALIZE                     RYOUYOCOM-REC
               MOVE    1                  TO  FLG-RYOUYOCOM
           END-IF
      *
           .
       910-RYOUYOCOM-READ-EXT.
           EXIT.
      *
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
      *
      *****************************************************************
      *    テーブル読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"    USING MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
