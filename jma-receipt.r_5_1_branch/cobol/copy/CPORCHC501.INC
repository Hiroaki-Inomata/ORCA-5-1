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
      *----------------------------------------------------------*
      *    ��������䵽��ϥѥ�᥿         COPY (CPORCHC501.INC)
      *                                        (ORCHC501 �ǻ��ѡ�
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCHC501AREA.
      *    ������ʬ�ʣ���Ƭ���
           03  ORCHC501-SYORIKBN       PIC 9(01).
      *  Ģɼ������������
           03  ORCHC501-ORDER-KEY.
      *  �����ֹ�
               05  ORCHC501-PTNUM         PIC  X(20).
      *  ���Ų�
               05  ORCHC501-SRYKA         PIC  X(02).
      *  �ݸ��ȹ礻
               05  ORCHC501-HKNCOMBI      PIC  X(04).
      *  ��������
               05  ORCHC501-SRYYMD        PIC  X(08).
      *  �����ƥ�����
               05  ORCHC501-SYSYMD        PIC  X(08).
      *  ȯ����
               05  ORCHC501-PRINTYMD      PIC X(08).
      *  ȯ�Ի���
               05  ORCHC501-PRINTHMS      PIC X(06).
      *  �ɥ�������Ƭ��λ��Τߡ�
           03  ORCHC501-DRCD           PIC X(05).
      *
      *    ȯ�Զ�ʬ�����飴
           03  ORCHC501-CHKPRT         PIC 9(01)
                                       OCCURS   4.
      *
           03  ORCHC501-OUT-AREA.
      *        �꥿���󥳡��ɡʣ���������ʳ����۾��
               05  ORCHC501-RC             PIC 9(02).
      *