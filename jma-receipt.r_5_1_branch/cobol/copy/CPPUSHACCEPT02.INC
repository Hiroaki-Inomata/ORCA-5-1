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
       01  PUSHAC02-REC.
           03  PUSHAC02-EVENT                  PIC X(50).
           03  PUSHAC02-BODY.
      *            �����⡼��
             05  PUSHAC02-PMODE                PIC X(10).
      *            �����ֹ�
             05  PUSHAC02-PTNUM                PIC X(20).
      *            ������
             05  PUSHAC02-UKEYMD               PIC X(10).
      *            ���ջ���
             05  PUSHAC02-UKETIME              PIC X(08).
      *            ����ID
             05  PUSHAC02-UKEID                PIC X(05).
      *            ���Ųʥ�����
             05  PUSHAC02-SRYKA                PIC X(02).
      *            �ɥ�����������
             05  PUSHAC02-DRCD                 PIC X(05).
      *            �ݸ��ȹ礻�ֹ�
             05  PUSHAC02-HKNCOMBI             PIC X(04).
