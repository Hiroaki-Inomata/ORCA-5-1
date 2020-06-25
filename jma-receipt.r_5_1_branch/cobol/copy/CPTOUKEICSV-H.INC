      * 01  TOUKEICSV-H-REC.
            03  TOUKEICSV-H-KEY.
                05  TOUKEICSV-H-HOSPNUM
                                       PIC 9(2).
                05  TOUKEICSV-H-TBL-KEY
                                       PIC X(8).
      *         連番
                05  TOUKEICSV-H-RENNUM PIC 9(4).
      *         グループ
                05  TOUKEICSV-H-TBL-GROUP
                                       PIC X(14).
      *         処理内連番
                05  TOUKEICSV-H-SHORI-RENNUM
                                       PIC 9(4).
      *    シェルＩＤ
           03  TOUKEICSV-H-SHELLID     PIC X(20).
      *    診療年月
           03  TOUKEICSV-H-SRYYM       PIC X(6).
      *    診療年月日
           03  TOUKEICSV-H-SRYYMD      PIC X(8).
      *    件数
           03  TOUKEICSV-H-CNT         PIC 9(8).
      *    入外区分
           03  TOUKEICSV-H-NYUGAIKBN   PIC X(1).
      *    先格納場所
           03  TOUKEICSV-H-TO-FOLDER   PIC X(200).
      *    先ファイル名
           03  TOUKEICSV-H-TO-DATA     PIC X(200).
      *    コード区分（空白、１：ＥＵＣ ２：Ｓ−ｊｉｓ）
           03  TOUKEICSV-H-CODE-TYPE   PIC X(01).
      *    出力フラグ
           03  TOUKEICSV-H-PUTFLG      PIC X(1).
      *    処理タイトル
           03  TOUKEICSV-H-TITLE       PIC X(100).
      *    作成日
           03  TOUKEICSV-H-CREYMD      PIC X(8).
      *    更新年月日
           03  TOUKEICSV-H-UPYMD       PIC  X(08).
      *    更新時間
           03  TOUKEICSV-H-UPHMS       PIC  X(06).
