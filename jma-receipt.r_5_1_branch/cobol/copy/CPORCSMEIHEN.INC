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
      *    ���Ź԰�̾�κ��Խ��ѥ�᥿����COPY (CPORCSMEIHEN.INC)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSMEIHENAREA.
      *
      *����
           03  ORCSMEIHEN-IN-AREA.
      *������ʬ�ʣ������̡���������������
               05  ORCSMEIHEN-KBN                 PIC  X(01).
      *���ŵ����ֹ�
               05  ORCSMEIHEN-GRPHOSPNUM          PIC  9(02).
      *���ť�����
               05  ORCSMEIHEN-SRYCD               PIC  X(09).
      *̾��
               05  ORCSMEIHEN-TNS-NAME            PIC  X(100).
      *���̡��������
               05  ORCSMEIHEN-SURYO               PIC 9(05)V9(05).
      *����
           03  ORCSMEIHEN-OUT-AREA.
               05  ORCSMEIHEN-MEISYO           PIC X(80).
               05  ORCSMEIHEN-MEISYO2          PIC X(20).
      *
