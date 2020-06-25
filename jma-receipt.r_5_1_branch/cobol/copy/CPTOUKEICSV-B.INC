      *01  TOUKEICSV-B-REC.
            03  TOUKEICSV-B-HOSPNUM    PIC 9(2).
            03  TOUKEICSV-B-TBL-KEY    PIC X(8).
      *     連番
            03  TOUKEICSV-B-RENNUM     PIC 9(4).
      *     グループ
            03  TOUKEICSV-B-TBL-GROUP  PIC X(14).
      *     処理内連番
            03  TOUKEICSV-B-SHORI-RENNUM
                                       PIC 9(4).
      *    シェルＩＤ
           03  TOUKEICSV-B-SHELLID     PIC X(20).
      *    診療年月
           03  TOUKEICSV-B-SRYYM       PIC X(6).
      *    診療年月日
           03  TOUKEICSV-B-SRYYMD      PIC X(8).
           03  TOUKEICSV-B-PTID        PIC 9(10).
           03  TOUKEICSV-B-SEQ-NO      PIC 9(8).
           03  TOUKEICSV-B-CSVDATA     PIC X(20000).
      *    作成日
           03  TOUKEICSV-B-CREYMD      PIC X(8).
      *    更新年月日
           03  TOUKEICSV-B-UPYMD       PIC  X(08).
      *    更新時間
           03  TOUKEICSV-B-UPHMS       PIC  X(06).
