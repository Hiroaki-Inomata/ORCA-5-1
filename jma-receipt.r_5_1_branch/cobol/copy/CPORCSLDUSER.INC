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
      *    �桼�����������֡�COPY (CPORCSLDUSER.INC)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SLDUSER-AREA.
           03  SLDUSER-MY-LD               PIC 9(03).
           03  SLDUSER-OTHERS              PIC 9(05).
           03  SLDUSER-MAX                 PIC 9(05).
           03  SLDUSER-ID                  PIC X(64).
           03  SLDUSER-KBN                 PIC 9(02).
      *        KBN 1:������2:���̾���
      *            3:���ݥåץ��åס�4:���̥ݥåץ��åס�
      *            5:��������������6:���̥�����������
      *            7:��祢�ܡ��ȡ�8:���̥��ܡ���
           03  SLDUSER-MESSAGE             PIC X(256).
           03  SLDUSER-OCC                 OCCURS  100.
               05  SLDUSER-HOSPNUM         PIC 9(02).
               05  SLDUSER-TERMID          PIC X(64).
               05  SLDUSER-LD              PIC 9(03).
               05  SLDUSER-IN-PROCESS      PIC X(64).
               05  SLDUSER-WINDOW          PIC X(64).
               05  SLDUSER-USER            PIC X(64).
               05  SLDUSER-EVENT           PIC X(64).
               05  SLDUSER-ACCESS-TIME     PIC X(64).
               05  SLDUSER-PROCESS-TIME    PIC X(64).
               05  SLDUSER-TOTAL-PROC      PIC X(64).
               05  SLDUSER-AGENT           PIC X(64).
               05  SLDUSER-HOST            PIC X(64).
               05  SLDUSER-WIDGET          PIC X(64).
               05  SLDUSER-CREATE-TIME     PIC X(64).