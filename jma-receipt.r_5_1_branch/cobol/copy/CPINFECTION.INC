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
      *****************************************************************
      *  �����ɥץ롼�վ���                          (CPINFECTION.INC)*
      *****************************************************************
      *  ��������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
      *01  INFECTION-REC.
           03  INFECT-KEY.
      *���ŵ��أɣ�
               05  INFECT-HOSPNUM      PIC 9(2).
      *�����
               05  INFECT-SENDYMD      PIC X(8).
      *���ԣɣ�
               05  INFECT-PTID         PIC 9(10).
      *�¸��ζ�ʬ
               05  INFECT-DISEASE      PIC 9(02).
      *������
               05  INFECT-SRYYMD       PIC X(08).
      *���ť����ɡʿ��Ź԰١�
           03  INFECT-SRYCD            PIC X(09).
      *���ť����ɡʰ����ʡ�
           03  INFECT-MEDSRYCD         PIC X(09).
      *��ǺƿǶ�ʬ
           03  INFECT-SYOSAI           PIC 9(01).
      *��ǻ�����
           03  INFECT-SYOSINYMD        PIC X(08).
      *��̾������
           03  INFECT-BYOMEICD         PIC X(07).
      *���ų�����
           03  INFECT-BYOSRYYMD        PIC X(08).
      *ž����ʬ
           03  INFECT-TENKIKBN         PIC X(01).
      *ž����
           03  INFECT-TENKIYMD         PIC X(08).
      *ü���ɣ�
           03  INFECT-TERMID           PIC X(16).
      *���ڥ졼���ɣ�
           03  INFECT-OPID             PIC X(16).
      *����ǯ����
           03  INFECT-CREYMD           PIC X(08).
      *����ǯ����
           03  INFECT-UPYMD            PIC X(08).
      *��������
           03  INFECT-UPHMS            PIC X(06).
