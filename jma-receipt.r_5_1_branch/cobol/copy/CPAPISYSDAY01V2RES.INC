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
      ******************************************************************
      *    ＡＰＩレスポンス患者登録・診療行為システム設定情報取得
      *                                 COPY (CPAPISYSDAY01V2RES.INC)
      ******************************************************************
        01  XML-SYSTEMDAILYRES.
            02  SYSDAYRES-CONTEXT            PIC S9(9)   BINARY.
            02  SYSDAYRES-OBJECT             PIC X(36).
            02  SYSDAYRES-MODE               PIC S9(9)   BINARY.
            02  SYSDAYRES-RECORDNAME         PIC X(128).
            02  SYSDAYRES-APPOINTRES.
      *       実施日
              03  SYSDAYRES-INFORMATION-DATE    PIC X(10).
      *       実施時間
              03  SYSDAYRES-INFORMATION-TIME    PIC X(8).
      *       返却コード
              03  SYSDAYRES-API-RESULT          PIC X(2).
      *       返却メッセージ
              03  SYSDAYRES-API-RESULT-MSG      PIC X(100).
      *
              03  SYSDAYRES-RESKEY              PIC X(100).
      *       基準日
              03  SYSDAYRES-BASE-DATE           PIC X(10).
      *       患者登録関係
              03  SYSDAYRES-PATIENT-INFOMATION.
      *           二重登録疑いチェック区分
                  05  SYSDAYRES-JYUFUKU-CHKKBN     PIC X(01).
                  05  SYSDAYRES-JYUFUKU-CHK-MEI    PIC X(40).
      *           郵便番号自動記載区分
                  05  SYSDAYRES-POSTKBN-FLG        PIC X(01).
                  05  SYSDAYRES-POSTKBN-MEI        PIC X(40).
      *           都道府県名記載区分
                  05  SYSDAYRES-KENMEIKBN-FLG      PIC X(01).
                  05  SYSDAYRES-KENMEIKBN-MEI      PIC X(40).
      *
      *       診療行為関係
              03  SYSDAYRES-MEDICAL-INFOMATION.
      *設定
      *SYS1001
                  05  SYSDAYRES-SET-INF.
      *           処方区分
                      07  SYSDAYRES-SYOHOKBN           PIC X(01).
                      07  SYSDAYRES-SYOHOKBN-MEI       PIC X(40).
      *           消費税端数設定
                      07  SYSDAYRES-TAX-HASU-KBN       PIC X(01).
                      07  SYSDAYRES-TAX-HASU-MEI       PIC X(40).
      *           自費コード端数設定
                      07  SYSDAYRES-JIHI-HASU-KBN      PIC X(01).
                      07  SYSDAYRES-JIHI-HASU-MEI      PIC X(40).
      *SYS1038
      *           皮下筋肉注射料変換選択
                      07  SYSDAYRES-CHG310FLG          PIC X(01).
                      07  SYSDAYRES-CHG310FLG-MEI      PIC X(40).
      *           投薬診療区分変換選択
                      07  SYSDAYRES-CHGSRYSYU-FLG      PIC X(01).
                      07  SYSDAYRES-CHGSRYSYU-MEI      PIC X(40).
      *           セット展開区分
                      07  SYSDAYRES-SETALLKBN          PIC X(01).
                      07  SYSDAYRES-SETALLKBN-MEI      PIC X(40).
      *
      *リクエスト＝０１
                  05  SYSDAYRES-SET01-INF.
      *SYS1008
      *           診察料自動発生
                      07  SYSDAYRES-INITAUTO-FLG       PIC X(01).
                      07  SYSDAYRES-INITAUTO-MEI       PIC X(40).
      *           育児栄養加算自動算定
                      07  SYSDAYRES-IKUJIEIYO-FLG      PIC X(01).
                      07  SYSDAYRES-IKUJIEIYO-MEI      PIC X(40).
      *           育児栄養加算自動算定科
                      07  SYSDAYRES-IKUJIEIYO-SRYKA    PIC X(02).
                      07  SYSDAYRES-IKUJIEIYO-KAMEI    PIC X(40).
      *           病名疾患区分からの自動発生
                      07  SYSDAYRES-BYOMEIAUTO-FLG     PIC X(01).
                      07  SYSDAYRES-BYOMEIAUTO-MEI     PIC X(40).
      *SYS1007
      *           療養担当手当
                      07  SYSDAYRES-RYOYOU-FLG         PIC X(01).
                      07  SYSDAYRES-RYOYOU-MEI         PIC X(40).
      *SYS1008
      *           外来管理加算チェック
                      07  SYSDAYRES-GAIKANRIKBN-CHK    PIC X(01).
                      07  SYSDAYRES-GAIKANRIKBN-MEI    PIC X(40).
      *SYS1008
      *           時間外加算（小児科特例）
                      07  SYSDAYRES-JIKANTOKU-NYU-FLG  PIC X(01).
                      07  SYSDAYRES-JIKANTOKU-NYU-MEI  PIC X(40).
      *
      *リクエスト＝００
      *           最終来院日からの期間
                      07  SYSDAYRES-LAST-KIKAN         PIC X(02).
      *           前回保険組合せ相違チェック
                      07  SYSDAYRES-ZENHKNCHK-FLG      PIC X(01).
                      07  SYSDAYRES-ZENHKNCHK-MEI      PIC X(40).
      *
      *リクエスト＝０２，０３
                  05  SYSDAYRES-SET02-INF.
      *SYS1008
      *           薬剤投与量チェック
                      07  SYSDAYRES-TOUYOCHK-FLG       PIC X(01).
                      07  SYSDAYRES-TOUYOCHK-MEI       PIC X(40).
      *           投薬３０日超チェック
                      07  SYSDAYRES-SHOHOU30CHK        PIC X(01).
                      07  SYSDAYRES-SHOHOU30CHK-MEI    PIC X(40).
      *
      *           薬剤情報提供料チェック
                      07  SYSDAYRES-YAKJYOCHK-FLG      PIC X(01).
                      07  SYSDAYRES-YAKJYOCHK-MEI      PIC X(40).
      *           手帳加算確認チェック
                      07  SYSDAYRES-TECHOKBN-FLG       PIC X(01).
                      07  SYSDAYRES-TECHOKBN-MEI       PIC X(40).
      *           時間外緊急院内チェック
                      07  SYSDAYRES-JGAIKEIKBN-FLG     PIC X(01).
                      07  SYSDAYRES-JGAIKEIKBN-MEI     PIC X(40).
      *
      *SYS1038
      *           精神療法２０未満加算区分
                      07  SYSDAYRES-SEI20KSNKBN        PIC X(01).
                      07  SYSDAYRES-SEI20KSNKBN-MEI    PIC X(40).
      *           心身療法２０未満加算区分
                      07  SYSDAYRES-SIN20KSNKBN        PIC X(01).
                      07  SYSDAYRES-SIN20KSNKBN-MEI    PIC X(40).
      *           残量廃棄区分
                      07  SYSDAYRES-HAIKIKBN           PIC X(01).
                      07  SYSDAYRES-HAIKIKBN-MEI       PIC X(40).
      *SYS1007
      *           調剤技術基本料
                      07  SYSDAYRES-CHOKI-FLG          PIC X(01).
                      07  SYSDAYRES-CHOKI-MEI          PIC X(40).
      *           薬剤情報提供料（一般）
                      07  SYSDAYRES-YAKJYO1-FLG        PIC X(01).
                      07  SYSDAYRES-YAKJYO1-MEI        PIC X(40).
      *           薬剤情報提供料（老人）
                      07  SYSDAYRES-YAKJYO2-FLG        PIC X(01).
                      07  SYSDAYRES-YAKJYO2-MEI        PIC X(40).
      *           特定薬剤治療管理料有無
                      07  SYSDAYRES-TOKUTEI-FLG        PIC X(01).
                      07  SYSDAYRES-TOKUTEI-MEI        PIC X(40).
      *
      *           訂正時の自動発生（外来）
                      07  SYSDAYRES-TEISEIAUTO-FLG     PIC X(01).
                      07  SYSDAYRES-TEISEIAUTO-MEI     PIC X(40).
      *           訂正時の自動発生（特定疾患処方管理）
                      07  SYSDAYRES-TEISEITOKU-FLG     PIC X(01).
                      07  SYSDAYRES-TEISEITOKU-MEI     PIC X(40).
      *           検体検査加算自動発生
                      07  SYSDAYRES-KENSAAUTO-FLG      PIC X(01).
                      07  SYSDAYRES-KENSAAUTO-MEI      PIC X(40).
      *           画像診断管理加算１（写真診断）
                      07  SYSDAYRES-GAZOUKSN1-FLG      PIC X(01).
                      07  SYSDAYRES-GAZOUKSN1-MEI      PIC X(40).
      *
      *リクエスト＝０４
                  05  SYSDAYRES-SET04-INF.
      *           入金の取扱い区分
                      07  SYSDAYRES-NYKNKNRKBN          PIC X(01).
                      07  SYSDAYRES-NYKNKNRKBN-MEI      PIC X(40).
      *           入金・返金額設定区分
                      07  SYSDAYRES-NYKHNKKBN           PIC X(01).
                      07  SYSDAYRES-NYKHNKKBN-MEI       PIC X(40).
      *           訂正の請求書金額
                      07  SYSDAYRES-TEIKINKBN           PIC X(01).
                      07  SYSDAYRES-TEIKINKBN-MEI       PIC X(40).
      *
