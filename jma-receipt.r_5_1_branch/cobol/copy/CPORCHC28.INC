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
      *    ���԰�������           COPY (CPORCHC28.INC)
      *                                (ORCHC28 �ǻ��ѡ�
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCHC28AREA.
      *ɽ��
           03  ORCHC28-HYODAI              PIC X(100).
      *��
           03  ORCHC28-JYOKEN1             PIC X(100).
      *��
           03  ORCHC28-JYOKEN2             PIC X(100).
      *����ID�ΰ�
           03  ORCHC28-PTID-AREA.
      *����ID���
               05  ORCHC28-PTID-COUNT      PIC 9(05).
      *
               05  ORCHC28-PTID-OCC        OCCURS  200.
      *Ϣ��
                   07  ORCHC28-PTID-SEQ    PIC X(5).
      *����ID
                   07  ORCHC28-PTID        PIC 9(10).
      *
