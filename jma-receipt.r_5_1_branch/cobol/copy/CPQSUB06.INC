      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
       01  QSUB06-LNK.
           03  QSUB06-SHELLNAME                PIC X(32).
      *        0: FAULE, 1: TRUE
           03  QSUB06-REQUEST-G.
               05  QSUB06-REQUEST-PRINT        PIC X(01).
               05  QSUB06-REQUEST-CSV          PIC X(01).
           03  QSUB06-SELHMS                   PIC X(06).
           03  QSUB06-SHELLID                  PIC X(08).
           03  QSUB06-JOBID                    PIC 9(07).
           03  QSUB06-KARTE-UID                PIC X(36).
           03  QSUB06-ORCA-UID                 PIC X(36).
      *    1: ���饤�������¸
           03  QSUB06-FILESVKBN                PIC X(01).
      *    1: "EUC-JP", 2: "S-JIS", 3: "UTF-8"
           03  QSUB06-ENCODEKBN                PIC X(01).
           03  QSUB06-DIRNAME                  PIC X(100).
           03  QSUB06-FILENAME                 PIC X(100).
      *    ���Ϸ��
      *    1:  ����, 2: ����, 3:���Ź԰�
           03  QSUB06-OUTKBN                   PIC X(01).
           03  QSUB06-DATA-ID                  PIC X(37).
           03  QSUB06-PRINT-ID                 PIC X(37).
           03  QSUB06-TBL-KEY                  PIC X(08).
      *    �ãӣ֥ե�����̾
           03  QSUB06-CSVFILENAME              PIC X(100).
      *    �Уģƥե�����̾
           03  QSUB06-PDFFILENAME              PIC X(100).
      *    ɽ��
           03  QSUB06-HYODAI                   PIC X(100).
      *    ��
           03  QSUB06-JYOKEN1                  PIC X(100).
      *    ��
           03  QSUB06-JYOKEN2                  PIC X(100).
      *    ���ϥڡ�����
           03  QSUB06-STPAGE                   PIC 9(09).
      *    ��λ�ڡ�����
           03  QSUB06-EDPAGE                   PIC 9(09).
      *    ���Ϸ��
           03  QSUB06-STKNS                    PIC 9(09).
      *    ��λ���
           03  QSUB06-EDKNS                    PIC 9(09).
      *    �����
           03  QSUB06-KJYNYMD                  PIC 9(08).
      *    ����ǯ��
           03  QSUB06-SRYYM                    PIC X(06).
      *    ������ʬ
           03  QSUB06-NYUGAIKBN                PIC X(01).
      *    Ϣ�ȶ�ʬ
           03  QSUB06-LNKKBN                   PIC X(01).
      *    SQL(ORDER)
           03  QSUB06-SQL                      PIC X(500).
