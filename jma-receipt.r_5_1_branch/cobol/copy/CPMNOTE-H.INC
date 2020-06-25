      * 01  MNOTE-H-REC.
            03  MNOTE-H-KEY.
                05  MNOTE-H-HOSPNUM
                                       PIC 9(2).
                05  MNOTE-H-TBL-KEY
                                       PIC X(8).
      *         連番
                05  MNOTE-H-RENNUM PIC 9(4).
      *         グループ
                05  MNOTE-H-TBL-GROUP
                                       PIC X(14).
      *         UUID
                05  MNOTE-H-TBL-UUID
                                       PIC X(36).
      *         処理内連番
                05  MNOTE-H-SHORI-RENNUM
                                       PIC 9(4).
      *    業務コード
           05  MNOTE-H-GYOUMU-CD    PIC 9(03).
      *    出力場所コード
           05  MNOTE-H-LOCATION-CD  PIC X(02).
      *    シェルＩＤ
           03  MNOTE-H-SHELLID     PIC X(20).
      *    診療年月
           03  MNOTE-H-SRYYM       PIC X(6).
      *    診療年月日
           03  MNOTE-H-SRYYMD      PIC X(8).
      *    件数
           03  MNOTE-H-CNT         PIC 9(5).
      *    入外区分
           03  MNOTE-H-NYUGAIKBN   PIC X(1).
      *    患者ＩＤ
           03  MNOTE-H-PTID        PIC 9(10).
      *    ドクターコード
           03  MNOTE-H-DRCD        PIC X(5).
      *    先格納場所
           03  MNOTE-H-TO-FOLDER   PIC X(200).
      *    先ファイル名
           03  MNOTE-H-TO-DATA     PIC X(200).
      *    コード区分（空白、１：ＥＵＣ ２：Ｓ−ｊｉｓ）
           03  MNOTE-H-CODE-TYPE   PIC X(01).
      *    出力フラグ
           03  MNOTE-H-PUTFLG      PIC X(1).
      *    処理タイトル
           03  MNOTE-H-TITLE       PIC X(100).
      *    データバージョン
           03  MNOTE-H-DATA-VERSION
                                   PIC X(1).
      *    オペレータＩＤ
           03  MNOTE-H-OPID        PIC X(16).
      *    作成日
           03  MNOTE-H-CREYMD      PIC X(8).
      *    更新年月日
           03  MNOTE-H-UPYMD       PIC  X(08).
      *    更新時間
           03  MNOTE-H-UPHMS       PIC  X(06).
