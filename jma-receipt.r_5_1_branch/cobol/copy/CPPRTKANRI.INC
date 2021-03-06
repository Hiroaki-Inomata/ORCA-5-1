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
      ************************************************************
      *    ��������                       (CPPRTKANRI.INC)       *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  PRTKANRI-REC.
      *
           03  PRTKANRI-KEY.
               05  PRTKANRI-HOSPNUM   PIC 9(2).
      *�ˣţ�
               05  PRTKANRI-TBL-KEY    PIC X(08).
      *Ϣ��
               05  PRTKANRI-RENNUM     PIC 9(04).
      *���롼��
               05  PRTKANRI-TBL-GROUP  PIC X(14).
      *UUID
               05  PRTKANRI-TBL-UUID   PIC X(36).
      *������Ϣ��
               05  PRTKANRI-SHORI-RENNUM
                                       PIC 9(04).
      *��̳������
               05  PRTKANRI-GYOUMU-CD  PIC 9(03).                        
      *������ɣ�
           03  PRTKANRI-SHELLID        PIC X(20).
      *����ǯ��
           03  PRTKANRI-SRYYM          PIC X(06).
      *����ǯ����
           03  PRTKANRI-SKYYMD         PIC X(08).
      *������Ϣ��
           03  PRTKANRI-PRIORITY       PIC 9(04).
      *��
           03  PRTKANRI-PAGE           PIC 9(05).
      *�����ե饰
           03  PRTKANRI-PRTFLG         PIC X(01).
      *Ģɼ�����ȥ�
           03  PRTKANRI-TITLE          PIC X(100).
      *�ץ��̾
           03  PRTKANRI-PRTNM          PIC X(16).
      *���Уɡݣɣ�
           03  PRTKANRI-API-ID         PIC X(20).
      *ü���ɣ�
           03  PRTKANRI-TERMID         PIC X(32).
      *���ڥ졼���ɣ�
           03  PRTKANRI-OPID           PIC X(16).
      *������
           03  PRTKANRI-CREYMD         PIC X(8).
      *LIMIT
           03  PRTKANRI-LIMIT          PIC 9(8).
      *OFFSET
           03  PRTKANRI-OFFSET         PIC 9(8).
