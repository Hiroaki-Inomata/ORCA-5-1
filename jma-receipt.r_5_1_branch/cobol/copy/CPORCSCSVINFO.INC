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
      *************************************************************
      *    �ãӣ��ѣģ����極�֥ѥ�᥿    CPORCSCSVINFO.INC      *
      *************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSCSVINFOAREA.
      *    ������ʬ                              
           03  SCSVINFO-MODE           PIC X(03).
      *    ����������     
           03  SCSVINFO-RETURN         PIC S9(03).
      *    �ˣţ�
           03  SCSVINFO-TBL-KEY        PIC X(08).
      *    ������ɣ�
           03  SCSVINFO-SHELLID        PIC X(20).
      *    ������Ϣ��
           03  SCSVINFO-SHORI-RENNUM   PIC 9(03).
      *    Ϣ��
           03  SCSVINFO-RENNUM         PIC 9(03).
      *    �ǡ���Ϣ��
           03  SCSVINFO-SEQ-NO         PIC 9(06).
      *    ������ʬ
           03  SCSVINFO-NYUGAIKBN      PIC X(01).
      *    ����ǯ��
           03  SCSVINFO-SRYYM          PIC X(06).
      *    ����ǯ����
           03  SCSVINFO-SKYYMD         PIC X(08).
      *    ������ʧ����
           03  SCSVINFO-TEISYUTUSAKI   PIC X(01).
      *    �Уԣɣ�
           03  SCSVINFO-PTID           PIC 9(10).
      *    �ãӣ֥ǡ���
           03  SCSVINFO-CSVDATA        PIC X(2000).
