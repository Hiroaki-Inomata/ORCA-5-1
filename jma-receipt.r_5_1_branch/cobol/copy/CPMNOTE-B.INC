      *01  MNOTE-B-REC.
            03  MNOTE-B-HOSPNUM    PIC 9(2).
            03  MNOTE-B-TBL-KEY    PIC X(8).
      *     連番
            03  MNOTE-B-RENNUM     PIC 9(4).
      *     グループ
            03  MNOTE-B-TBL-GROUP  PIC X(14).
      *     UUID
            03  MNOTE-B-TBL-UUID
                                       PIC X(36).
      *     処理内連番
            03  MNOTE-B-SHORI-RENNUM
                                       PIC 9(4).
      *    シェルＩＤ
           03  MNOTE-B-SHELLID     PIC X(20).
      *    診療年月
           03  MNOTE-B-SRYYM       PIC X(6).
      *    診療年月日
           03  MNOTE-B-SRYYMD      PIC X(8).
           03  MNOTE-B-SEQ-NO      PIC 9(5).
           03  MNOTE-B-CSVDATA     PIC X(20000).
      *    作成日
           03  MNOTE-B-CREYMD      PIC X(8).
      *    更新年月日
           03  MNOTE-B-UPYMD       PIC  X(08).
      *    更新時間
           03  MNOTE-B-UPHMS       PIC  X(06).
